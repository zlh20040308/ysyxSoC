package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val spi_bundle = IO(new SPIIO)

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    mspi.io.in <> in
    spi_bundle <> mspi.io.spi

    val s_idle :: s_xpi_mode :: s_normal_mode :: s_xpi_send_cmd_mode :: s_xpi_send_divide_num_mode :: s_xpi_send_ss_mode :: s_xpi_send_ctrl_mode :: s_xpi_send_start_mode:: s_xpi_check_mode :: s_xpi_return_data_mode :: s_xpi_receive_data_mode :: s_cancel :: s_error :: Nil = Enum(13)
    val is_flash = in.paddr === BitPat("b0011????????????????????????????")
    val rdata = Reg(UInt(32.W))
    val state = RegInit(s_idle)
    state := MuxLookup(state, s_idle)(
      List(
        s_idle                      -> Mux(in.psel, Mux(is_flash , s_xpi_mode, s_normal_mode), s_idle),
        s_xpi_mode                  -> Mux(in.penable, Mux(~in.pwrite , s_xpi_send_cmd_mode, s_error), s_xpi_mode),
        s_normal_mode               -> Mux(mspi.io.in.pready, s_idle, s_normal_mode),
        s_xpi_send_cmd_mode         -> Mux(mspi.io.in.pready, s_xpi_send_ss_mode, s_xpi_send_cmd_mode),
        s_xpi_send_ss_mode          -> Mux(mspi.io.in.pready, s_xpi_send_divide_num_mode, s_xpi_send_ss_mode),
        s_xpi_send_divide_num_mode  -> Mux(mspi.io.in.pready, s_xpi_send_ctrl_mode, s_xpi_send_divide_num_mode),
        s_xpi_send_ctrl_mode        -> Mux(mspi.io.in.pready, s_xpi_send_start_mode, s_xpi_send_ctrl_mode),
        s_xpi_send_start_mode       -> Mux(mspi.io.in.pready, s_xpi_check_mode, s_xpi_send_start_mode),
        s_xpi_check_mode            -> Mux(mspi.io.in.pready && (mspi.io.in.prdata & (1 << 8).U(32.W)) === 0.U, s_xpi_receive_data_mode, s_xpi_check_mode),

        s_xpi_receive_data_mode     -> Mux(mspi.io.in.pready, s_xpi_return_data_mode, s_xpi_receive_data_mode),
        s_xpi_return_data_mode      -> s_cancel,
        s_cancel                    -> Mux(mspi.io.in.pready, s_idle, s_cancel),
        s_error -> s_error
      )
    )

    mspi.io.in.psel := MuxLookup(state, s_idle)(
      List(
        s_idle                     -> in.psel,
        s_xpi_mode                 -> in.psel,
        s_normal_mode              -> in.psel,
        s_xpi_send_cmd_mode        -> in.psel,
        s_xpi_send_ss_mode         -> in.psel,
        s_xpi_send_divide_num_mode -> in.psel,
        s_xpi_send_ctrl_mode       -> in.psel,
        s_xpi_send_start_mode      -> in.psel,
        s_xpi_check_mode           -> in.psel,
        s_xpi_receive_data_mode    -> in.psel,
        s_xpi_return_data_mode     -> true.B,
        s_cancel                   -> true.B,
        s_error                    -> false.B
      )
    )
    mspi.io.in.penable := MuxLookup(state, s_idle)(
      List(
        s_idle                     -> false.B,
        s_xpi_mode                 -> false.B,
        s_normal_mode              -> in.pwrite,
        s_xpi_send_cmd_mode        -> true.B,
        s_xpi_send_ss_mode         -> true.B,
        s_xpi_send_divide_num_mode -> true.B,
        s_xpi_send_ctrl_mode       -> true.B,
        s_xpi_send_start_mode      -> true.B,
        s_xpi_check_mode           -> true.B,
        s_xpi_receive_data_mode    -> true.B,
        s_xpi_return_data_mode     -> false.B,
        s_cancel                   -> true.B,
        s_error                    -> false.B
      )
    )
    mspi.io.in.pwrite  := MuxLookup(state, s_idle)(
      List(
        s_idle                     -> false.B,
        s_xpi_mode                 -> false.B,
        s_normal_mode              -> in.pwrite,
        s_xpi_send_cmd_mode        -> true.B,
        s_xpi_send_ss_mode         -> true.B,
        s_xpi_send_divide_num_mode -> true.B,
        s_xpi_send_ctrl_mode       -> true.B,
        s_xpi_send_start_mode      -> true.B,
        s_xpi_check_mode           -> false.B,
        s_xpi_receive_data_mode    -> false.B,
        s_xpi_return_data_mode     -> false.B,
        s_cancel                   -> true.B,
        s_error                    -> false.B
      )
    )

    mspi.io.in.paddr := MuxLookup(state, s_idle)(
      List(
        s_idle                     -> 0x0.U(32.W),
        s_xpi_mode                 -> 0x0.U(32.W),
        s_normal_mode              -> in.paddr,
        s_xpi_send_cmd_mode        -> 0x10001004.U(32.W),
        s_xpi_send_ss_mode         -> 0x10001018.U(32.W),
        s_xpi_send_divide_num_mode -> 0x10001014.U(32.W),
        s_xpi_send_ctrl_mode       -> 0x10001010.U(32.W),
        s_xpi_send_start_mode      -> 0x10001010.U(32.W),
        s_xpi_check_mode           -> 0x10001010.U(32.W),
        s_xpi_receive_data_mode    -> 0x10001000.U(32.W),
        s_xpi_return_data_mode     -> 0x0.U(32.W),
        s_cancel                   -> 0x10001018.U(32.W),
        s_error                    -> 0x0.U(32.W)
      )
    )

    mspi.io.in.pprot := in.pprot

    mspi.io.in.pwdata := MuxLookup(state, s_idle)(
      List(
        s_idle                     -> 0x0.U(32.W),
        s_xpi_mode                 -> 0x0.U(32.W),
        s_normal_mode              -> 0x0.U(32.W),
        s_xpi_send_cmd_mode        -> Cat(0x03.U(8.W),in.paddr(23, 0)),
        s_xpi_send_ss_mode         -> 0x1.U(32.W),
        s_xpi_send_divide_num_mode -> 0x1.U(32.W),
        s_xpi_send_ctrl_mode       -> 0x440.U(32.W),
        s_xpi_send_start_mode      -> 0x540.U(32.W),
        s_xpi_check_mode           -> 0x0.U(32.W),
        s_xpi_receive_data_mode    -> 0x0.U(32.W),
        s_xpi_return_data_mode     -> 0x0.U(32.W),
        s_cancel                   -> 0x0.U(32.W),
        s_error                    -> 0x0.U(32.W)
      )
    )

    mspi.io.in.pstrb := MuxLookup(state, s_idle)(
      List(
        s_idle                     -> 0x0.U(4.W),
        s_xpi_mode                 -> 0x0.U(4.W),
        s_normal_mode              -> in.pstrb,
        s_xpi_send_cmd_mode        -> 0xf.U(4.W),
        s_xpi_send_ss_mode         -> 0xf.U(4.W),
        s_xpi_send_divide_num_mode -> 0xf.U(4.W),
        s_xpi_send_ctrl_mode       -> 0xf.U(4.W),
        s_xpi_send_start_mode      -> 0xf.U(4.W),
        s_xpi_check_mode           -> 0x0.U(4.W),
        s_xpi_receive_data_mode    -> 0x0.U(4.W),
        s_xpi_return_data_mode     -> 0x0.U(4.W),
        s_cancel                   -> 0xf.U(4.W),
        s_error                    -> 0x0.U(4.W)
      )
    )

    rdata      := Mux(state === s_xpi_receive_data_mode && mspi.io.in.pready, mspi.io.in.prdata, rdata)
    in.prdata  := Mux(state === s_normal_mode, mspi.io.in.prdata, Cat(rdata(7,0),rdata(15,8),rdata(23,16),rdata(31,24)))
    in.pready  := Mux(state === s_normal_mode, mspi.io.in.pready, Mux(state === s_xpi_return_data_mode, true.B, false.B))
    in.pslverr := mspi.io.in.pslverr
  }
}
