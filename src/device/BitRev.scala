package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io = IO(Flipped(new SPIIO(1)))  

  withClock(io.sck.asClock) {
    val s_idle :: s_transfer :: Nil = Enum(2)
    val state = RegEnable(s_idle, io.sck)
    val counter = RegEnable(0.U(8.W), io.sck)
    val dataReg = RegEnable(0.U(8.W), io.sck)
    counter := Mux(counter =/= 16.U , counter + 1.U, 0.U)
    state := MuxLookup(state, s_idle)(
      List(
        s_idle     -> Mux(~io.ss.asBool, s_transfer, s_idle),
        s_transfer -> Mux(counter === 16.U, s_idle, s_transfer)
      )
    )
    dataReg := Mux(~io.ss.asBool && counter < 8.U, (dataReg >> 1) | (io.mosi.asUInt << 7.U), dataReg)
    io.miso := Mux(state === s_idle, true.B, dataReg(15.U - counter))
  }
}
