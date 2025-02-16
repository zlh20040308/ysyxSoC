module psram(
  input sck,
  input ce_n,
  inout [3:0] dio
);

  wire reset = ce_n;
  wire [3:0] din, dout;
  wire [31:0] rev_wdata, rev_rdata;

  typedef enum [3:0] {
     cmd_t, 
     addr_t, 
     read_data_t,
     read_latency_t,
     return_data_t, 
     receive_wdata_t, 
     idle_t, 
     err_t 
  } state_t;

  reg [3:0]  state;
  reg [7:0]  counter;
  reg [7:0]  cmd;
  reg [23:0] addr;
  reg [31:0] rdata;
  reg [7:0] wdata;

  wire ren = state == read_data_t;
  wire wen = ((state == receive_wdata_t) && ((counter % 2) == 0) && counter != 0);

  always@(posedge sck or posedge reset) begin
    if (reset) state <= cmd_t;
    else begin
      case (state)
        cmd_t:  state <= (counter == 8'd7 ) ? addr_t : state;
        addr_t: state <= (cmd     != 8'heb && cmd     != 8'h38) ? err_t  :
                         (counter == 8'd5) ? (cmd     == 8'heb ? read_data_t : receive_wdata_t) : state;
        read_data_t:       state <= (counter == 8'd0) ? read_latency_t : state;
        read_latency_t:    state <= (counter == 8'd5) ? return_data_t : state;
        return_data_t:     state <= (counter == 8'd7) ? idle_t : state;
        receive_wdata_t:   state <=  state;
        idle_t:            state <=  state;
        default: begin
          state <= state;
          $fwrite(32'h80000002, "Assertion failed: Unsupported command `%xh`\n", cmd);
          $fatal;
        end
      endcase
    end
  end

  always@(posedge sck or posedge reset) begin
    if (reset) counter <= 8'd0;
    else begin
      case (state)
        cmd_t:                  counter <= (counter < 8'd7)  ? counter + 8'd1 : 8'd0;
        addr_t:                 counter <= (counter < 8'd5)  ? counter + 8'd1 : 8'd0;
        read_data_t:            counter <= (counter < 8'd0)  ? counter + 8'd1 : 8'd0;
        read_latency_t:         counter <= (counter < 8'd5)  ? counter + 8'd1 : 8'd0;
        return_data_t:          counter <= (counter < 8'd7)  ? counter + 8'd1 : 8'd0;
        receive_wdata_t:        counter <=  counter + 8'd1;
        default:                counter <=  counter + 8'd1;
      endcase
    end
  end

  always@(posedge sck or posedge reset) begin
    if (reset)               cmd <= 8'd0;
    else if (state == cmd_t) cmd <= { cmd[6:0], din[0] };
  end

  always@(posedge sck or posedge reset) begin
    if (reset)                         addr <= 24'd0;
    else if (state == addr_t)          addr <= { addr[19:0], din[3], din[2], din[1] , din[0]};
  end

  always@(posedge sck or posedge reset) begin
    if (reset)                         wdata <= 32'd0;
    else if (state == receive_wdata_t) wdata <= { wdata[3:0], din[3], din[2], din[1] , din[0]};
  end

  psram_cmd psram_cmd_i(
    .clock(sck),
    .ren(ren),
    .wen(wen),
    .cmd(cmd),
    .raddr({8'd0 ,addr}),
    .waddr({8'd0 ,addr} + (counter >> 1) - 1),
    .wdata(rev_wdata),
    .rdata(rdata)
  );

  assign rev_wdata = {wdata[7:0], wdata[15:8], wdata[23:16], wdata[31:24]};
  assign rev_rdata = {rdata[27:24], rdata[31:28], rdata[19:16], rdata[23:20], rdata[11:8], rdata[15:12], rdata[3:0], rdata[7:4]};

  assign dout[0] = rev_rdata[(counter<<2) + 0];
  assign dout[1] = rev_rdata[(counter<<2) + 1];
  assign dout[2] = rev_rdata[(counter<<2) + 2];
  assign dout[3] = rev_rdata[(counter<<2) + 3];

  assign dio[0] = (state == return_data_t) ? dout[0] : 1'bz;
  assign dio[1] = (state == return_data_t) ? dout[1] : 1'bz;
  assign dio[2] = (state == return_data_t) ? dout[2] : 1'bz;
  assign dio[3] = (state == return_data_t) ? dout[3] : 1'bz;
  assign din    = dio;

endmodule

import "DPI-C" function void psram_read(input int addr, output int data);
import "DPI-C" function void psram_write(input int addr, input byte data);

module psram_cmd(
  input             clock,
  input             ren,
  input             wen,
  input      [ 7:0] cmd,
  input      [31:0] raddr,
  input      [31:0] waddr,
  input      [ 7:0] wdata,
  output reg [31:0] rdata
);
  always@(posedge clock) begin
    if (ren && cmd == 8'heb) begin
      psram_read(raddr, rdata);
    end
  end
  always@(negedge clock) begin
    if (wen && cmd == 8'h38) begin
      psram_write(waddr, wdata);
    end
  end
endmodule
