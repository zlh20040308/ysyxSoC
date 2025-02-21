package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)
  val screen_width  = 640
  val screen_height = 480
  val screen_size   = screen_height * screen_width
  val vmem          = Mem(screen_size, UInt(32.W))
  val s_idle :: s_write_data :: Nil = Enum(2)
  val state                         = RegInit(s_idle)
  val vga_vaild                     = Wire(Bool())

  val h_frontporch = 96;
  val h_active = 144;
  val h_backporch = 784;
  val h_total = 800;
  
  val v_frontporch = 2;
  val v_active = 35;
  val v_backporch = 515;
  val v_total = 525;

  //像素计数值
  val     x_cnt   = RegInit(1.U(10.W));
  val     y_cnt   = RegInit(1.U(10.W));
  val     h_valid = Wire(Bool());
  val     v_valid = Wire(Bool());
  val     h_addr = Wire(UInt(10.W));
  val     v_addr = Wire(UInt(10.W));
  val     vga_data = Wire(UInt(24.W));

  x_cnt := Mux(x_cnt === h_total.U, 1.U, x_cnt + 1.U)
  y_cnt := Mux(x_cnt === h_total.U, Mux(y_cnt === v_total.U, 1.U, y_cnt + 1.U), y_cnt)

  io.vga.hsync := (x_cnt > h_frontporch.U)
  io.vga.vsync := (y_cnt > v_frontporch.U)

  h_valid := (x_cnt > h_active.U) & (x_cnt <= h_backporch.U)
  v_valid := (y_cnt > v_active.U) & (y_cnt <= v_backporch.U)

  io.vga.valid := h_valid & v_valid

  h_addr := Mux(h_valid, (x_cnt - 145.U), 0.U);
  v_addr := Mux(v_valid, (y_cnt - 36.U) , 0.U);
  //设置输出的颜色值 
  io.vga.r := vga_data(23,16);
  io.vga.g := vga_data(15, 8);
  io.vga.b := vga_data(7,  0);
  vga_data := vmem(h_addr +  v_addr * 640.U)(23, 0)

  vga_vaild := io.in.psel && io.in.penable && io.in.pwrite

  state := MuxLookup(state, s_idle)(
    List(
      s_idle        -> Mux(vga_vaild, s_write_data, s_idle),
      s_write_data -> s_idle,
    )
  )

  when(state === s_write_data){
    vmem((io.in.paddr & 0xfffffc.U) >> 2) := io.in.pwdata
  }

  io.in.pready        := Mux(state === s_write_data, true.B, false.B)
  io.in.pslverr       := 0.U
  io.in.prdata        := 0.U
  
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    // val mvga = Module(new vga_top_apb)
    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
