package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2 = new PS2IO
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

// 关于 FIFO 写满时，数据该如何处理，我的做法就比较简单，直接丢弃读到的数据
// 关于 FIFO “什么时候视作写满”， 我的想法就是按照普通 FIFO 来，其实这样设计不是很好，可能会导致无法接受一次完整的数据，但是 FIFO 开大一些应该就没啥问题
class ps2Chisel extends Module {
  val io = IO(new PS2CtrlIO)
  val s_idle :: s_return_data :: Nil = Enum(2)
  val fifo_size       = 10
  val data_len        = 10
  val state           = RegInit(s_idle)
  val fifo            = Reg(Vec(fifo_size, UInt(8.W)))
  val data_buf        = RegInit(0.U(data_len.W))
  val data_counter    = RegInit(0.U(4.W))
  val w_ptr           = RegInit(0.U(log2Ceil(fifo_size).W))
  val r_ptr           = RegInit(0.U(log2Ceil(fifo_size).W))
  val ps2_clk_sync    = RegInit(0.U(3.W))
  val sampling        = Wire(Bool())
  val ps2_vaild       = Wire(Bool())
  val can_write       = Wire(Bool())
  val can_read        = Wire(Bool())

  val return_data     = Wire(UInt(8.W))

  ps2_vaild    := io.in.psel && io.in.penable && ~io.in.pwrite
  ps2_clk_sync := Cat(ps2_clk_sync(1, 0), io.ps2.clk)
  sampling     := ps2_clk_sync(2) & ~ps2_clk_sync(1)
  data_buf     := Mux(sampling, Cat(io.ps2.data, data_buf(9, 1)), data_buf)
  data_counter := Mux(sampling, Mux(data_counter =/= data_len.U, data_counter + 1.U, 0.U), data_counter)
  can_write    := ((w_ptr + 1.U) % data_len.U) =/= r_ptr
  can_read     := w_ptr =/= r_ptr


  fifo(w_ptr)  := Mux(sampling && 
                      can_write &&
                      data_counter === data_len.U && 
                      data_buf(0) === 0.U && 
                      io.ps2.data === 1.U && 
                      data_buf(9, 1).xorR === 1.U, data_buf(8, 1), fifo(w_ptr))

  w_ptr        := Mux(sampling &&
                      can_write &&
                      data_counter === data_len.U && 
                      data_buf(0) === 0.U && 
                      io.ps2.data === 1.U && 
                      data_buf(9, 1).xorR === 1.U && 
                      (w_ptr + 1.U) % fifo_size.U =/= r_ptr, (w_ptr + 1.U) % fifo_size.U, w_ptr)

  r_ptr        := Mux(io.in.pready && can_read, (r_ptr + 1.U) % fifo_size.U, r_ptr)
  return_data  := Mux(can_read, fifo(r_ptr), 0.U)


  state := MuxLookup(state, s_idle)(
    List(
      s_idle        -> Mux(ps2_vaild, s_return_data, s_idle),
      s_return_data -> s_idle,
    )
  )

  io.in.pslverr := 0.U
  io.in.pready  := state === s_return_data
  io.in.prdata  := Cat(Fill(24, 0.U(1.W)), return_data)
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    // val mps2 = Module(new ps2_top_apb)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
