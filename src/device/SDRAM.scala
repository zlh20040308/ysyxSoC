package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val dq  = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramChisel extends RawModule {
  val io = IO(Flipped(new SDRAMIO))
  
  withClockAndReset(io.clk.asClock, false.B){
    val banks = 4
    val rows  = 8192
    val cols  = 512
    val depth = banks * rows * cols
    val rowAddress = io.a(12, 0)
    val colAddress = io.a(8, 0)
    val bankAddress = io.ba
    val (dqmLow, dqmHigh) = (io.dqm(0), io.dqm(1))  
    
    val dout = Wire(UInt(16.W))
    val out_en  = Wire(Bool())
    val din = TriStateInBuf(io.dq, dout, out_en)
    val (lowByte, highByte) = (din(7, 0), din(15, 8))  
    dout := 0.U
  
    val modeReg = Reg(UInt(13.W))
    val casLatency = modeReg(6, 4)
    val burstLength = Wire(UInt(3.W))
    burstLength := MuxLookup(modeReg(2, 0), "b000".U)(
      Seq(
        "b000".U -> 1.U,
        "b001".U -> 2.U,
        "b010".U -> 4.U,
        "b011".U -> 8.U
      )
    )
    val command = Cat(io.cs, io.ras, io.cas, io.we)
    val casCounter = Reg(UInt(13.W))
    
    // SDRAM 数据区
    val mem = SyncReadMem(depth, Vec(2, UInt(8.W)))
  
    // command
    // 由于 Mode 寄存器只需要实现 CAS Latency 和 Burst Length, 所以我不需要考虑 Burst Type, 默认 Type = Sequential
    val commandInhibit = BitPat("b1???")
    val noOperation = BitPat("b0111")
    val active = BitPat("b0011")
    val read = BitPat("b0101")
    val write = BitPat("b0100")
    val burstTerminate = BitPat("b0110")
    val loadModeRegister = BitPat("b0000")
  
    val s_idle :: s_CasLatency :: s_read :: s_write :: Nil = Enum(4)
    val state = RegInit(s_idle)
  
    when(command === loadModeRegister){
      modeReg := io.a
    }
  
    val bankAddressReg = Reg(UInt(bankAddress.getWidth.W))
    val colAddressReg = Reg(UInt(9.W))
    val activeBankRowReg = Reg(Vec(banks, UInt(rowAddress.getWidth.W)))
    val colAddressCnt = Reg(UInt(3.W))
    val casLatencyCnt = Reg(UInt(2.W))
    
    for (b <- 0 until banks) {
      when (command === active && bankAddress === b.asUInt) {
        activeBankRowReg(b) := rowAddress
      }
    }
  
    when(command === read || command === write) {
      colAddressReg := colAddress
      bankAddressReg := bankAddress
    }
  
    when(state === s_CasLatency || command === read) {
      casLatencyCnt := casLatencyCnt + 1.U
    }.otherwise{
      casLatencyCnt := 0.U
    }
    
    when ((state === s_CasLatency && casLatencyCnt + 1.U === casLatency) || state === s_read || state === s_write || (command === write)) {
      colAddressCnt := colAddressCnt + 1.U
    }.otherwise{
      colAddressCnt := 0.U
    }
    
    out_en := Mux(state === s_read, true.B, false.B)
    
    def flatAddr(bank: UInt, row: UInt, col: UInt): UInt = {
      bank * (rows * cols).U + row * cols.U + col
    }
    
    val addr = flatAddr(Mux(command === write, bankAddress, bankAddressReg), activeBankRowReg(bankAddressReg), Mux(command === write, colAddress, colAddressReg) + colAddressCnt)
    when (state === s_read || casLatencyCnt + 1.U === casLatency) {
      val rdata = mem.read(addr)
      val rdataUInt = Cat(rdata(1), rdata(0))
      dout := rdataUInt
    
      // printf(p"[SDRAM] READ  addr=0x${Hexadecimal(addr)} data=0x${Hexadecimal(rdataUInt)} state=${state}\n")
    }
    
    when (state === s_write || command === write) {
      mem.write(
        addr,
        VecInit(lowByte, highByte),
        VecInit(dqmLow === 0.U, dqmHigh === 0.U)
      )
    
      // printf(p"[SDRAM] WRITE addr=0x${Hexadecimal(addr)} data=0x${Hexadecimal(din)} state=${state}\n")
    }
  
    // FSM
    switch(state){
      is(s_idle){
        when(command === read){
          state := s_CasLatency
        }.elsewhen (command === write) {
          state := s_write
        }
      }
      is(s_CasLatency){
        when(casLatencyCnt + 1.U === casLatency){
          state := s_read
        }
      }
      is(s_read){
        when(colAddressCnt === burstLength){
          state := s_idle
        }
      }
      is(s_write){
        when(colAddressCnt + 1.U === burstLength){
          state := s_idle
        }
      }
    }
    
  }
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 4
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
        address       = address,
        executable    = true,
        supportsWrite = TransferSizes(1, beatBytes),
        supportsRead  = TransferSizes(1, beatBytes),
        interleavedId = Some(0))
    ),
    beatBytes  = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
