package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class gpioChisel extends Module {
  val io = IO(new GPIOCtrlIO)
  val s_idle :: s_read :: s_write :: Nil = Enum(3)


  val gpio_rvalid = Wire(Bool())
  val gpio_wvalid = Wire(Bool())
  val offset      = Wire(UInt(4.W))
  val state       = RegInit(s_idle)
  val datareg     = RegInit(0.U(64.W))
  val lut = VecInit(Seq(
    "b00000011".U(8.W),  // input 0 ->  output  b00000011
    "b10011111".U(8.W),  // input 1 ->  output  b10011111
    "b00100101".U(8.W),  // input 2 ->  output  b00100101
    "b00001101".U(8.W),  // input 3 ->  output  b00001101
    "b10011001".U(8.W),  // input 4 ->  output  b10011001
    "b01001001".U(8.W),  // input 5 ->  output  b01001001
    "b01000001".U(8.W),  // input 6 ->  output  b01000001
    "b00011111".U(8.W),  // input 7 ->  output  b00011111
    "b00000001".U(8.W),  // input 8 ->  output  b00000001
    "b00001001".U(8.W),  // input 9 ->  output  b00001001
    "b00010001".U(8.W),  // input 10 -> output  b00010001
    "b11000001".U(8.W),  // input 11 -> output  b11000001
    "b01100011".U(8.W),  // input 12 -> output  b01100011
    "b10000101".U(8.W),  // input 13 -> output  b10000101
    "b01100001".U(8.W),  // input 14 -> output  b01100001
    "b01110001".U(8.W)   // input 15 -> output  b01110001
  ))

  state := MuxLookup(state, s_idle)(
    List(
      s_idle     -> Mux(gpio_rvalid, s_read, Mux(gpio_wvalid, s_write, s_idle)),
      s_read     -> s_idle,
      s_write    -> s_idle
    )
  )

  offset        := io.in.paddr(3, 0) & 0xf.U
  gpio_rvalid   := io.in.psel && io.in.penable && ~io.in.pwrite
  gpio_wvalid   := io.in.psel && io.in.penable && io.in.pwrite
  io.in.pslverr := 0.U
  io.in.pready  := (state === s_read || state === s_write)
  io.in.prdata  := Mux(state === s_read, 
                       Mux(offset === 0.U, 
                           Cat(0.U(16.W), datareg(15, 0)), 
                           Mux(offset === 4.U, 
                               Cat(0.U(16.W), io.gpio.in), 
                               Mux(offset === 8.U, datareg(63, 32), 
                               0.U))), 
                       0.U)


  datareg         := Mux(state === s_write, 
                         Mux(offset === 0.U, 
                             Cat(datareg(63, 16), io.in.pwdata(15, 0)), 
                             Mux(offset === 8.U, 
                                 Cat(io.in.pwdata, datareg(31, 0)),
                                 datareg)), 
                         datareg)
  io.gpio.seg(0)  := lut(datareg(35, 32))
  io.gpio.seg(1)  := lut(datareg(39, 36))
  io.gpio.seg(2)  := lut(datareg(43, 40))
  io.gpio.seg(3)  := lut(datareg(47, 44))
  io.gpio.seg(4)  := lut(datareg(51, 48)) 
  io.gpio.seg(5)  := lut(datareg(55, 52)) 
  io.gpio.seg(6)  := lut(datareg(59, 56)) 
  io.gpio.seg(7)  := lut(datareg(63, 60)) 
  io.gpio.out     := datareg(15, 0)
}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val gpio_bundle = IO(new GPIOIO)

    // val mgpio = Module(new gpio_top_apb)
    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
