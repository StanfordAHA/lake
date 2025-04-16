module glb_bank_sram_gen_13_emu #(parameter BRAM = 1) (
  input logic [12:0] A,
  input logic [63:0] BWEB,
  input logic CEB,
  input logic CLK,
  input logic [63:0] D,
  input logic REB,
  input logic RESET,
  input logic WEB,
  output logic [63:0] Q
);

logic [11:0] A_SRAM;
logic [11:0] A_SRAM_d;
logic [63:0] BWEB_d;
logic CEB_d;
logic [63:0] D_d;
logic Q_SEL;
logic [63:0] Q_SRAM2MUX [1:0];
logic [63:0] Q_w;
logic [1:0] REB_DEMUX;
logic [1:0] REB_DEMUX_d;
logic SRAM_SEL;
logic SRAM_SEL_d;
logic [1:0] WEB_DEMUX;
logic [1:0] WEB_DEMUX_d;
logic WEB_d;
logic [63:0] sram_array_0_q;
logic sram_array_0_ren;
logic sram_array_0_wen;
logic [63:0] sram_array_1_q;
logic sram_array_1_ren;
logic sram_array_1_wen;
logic [76:0] sram_signals_pipeline_in_;
logic [76:0] sram_signals_pipeline_out_;
logic [69:0] sram_signals_reset_high_pipeline_in_;
logic [69:0] sram_signals_reset_high_pipeline_out_;
assign SRAM_SEL = A[12];
assign A_SRAM = A[11:0];
assign sram_signals_reset_high_pipeline_in_ = {WEB, CEB, REB_DEMUX, WEB_DEMUX, BWEB};
assign {WEB_d, CEB_d, REB_DEMUX_d, WEB_DEMUX_d, BWEB_d} = sram_signals_reset_high_pipeline_out_;
assign sram_signals_pipeline_in_ = {A_SRAM, SRAM_SEL, D};
assign {A_SRAM_d, SRAM_SEL_d, D_d} = sram_signals_pipeline_out_;

always_ff @(posedge CLK, posedge RESET) begin
  if (RESET) begin
    Q_SEL <= 1'h0;
  end
  else if ((CEB_d == 1'h0) & (WEB_d == 1'h1)) begin
    Q_SEL <= SRAM_SEL_d;
  end
end
always_comb begin
  if (~WEB) begin
    WEB_DEMUX = ~(2'h1 << 2'(SRAM_SEL));
  end
  else WEB_DEMUX = 2'h3;
  if (~REB) begin
    REB_DEMUX = ~(2'h1 << 2'(SRAM_SEL));
  end
  else REB_DEMUX = 2'h3;
end
assign sram_array_0_wen = ~WEB_DEMUX_d[0];
assign sram_array_0_ren = ~REB_DEMUX_d[0];
assign Q_SRAM2MUX[0] = sram_array_0_q;
assign sram_array_1_wen = ~WEB_DEMUX_d[1];
assign sram_array_1_ren = ~REB_DEMUX_d[1];
assign Q_SRAM2MUX[1] = sram_array_1_q;
assign Q_w = Q_SRAM2MUX[Q_SEL];
pipeline_w_70_d_0_reset_high sram_signals_reset_high_pipeline (
  .clk(CLK),
  .clk_en(1'h1),
  .in_(sram_signals_reset_high_pipeline_in_),
  .reset(RESET),
  .out_(sram_signals_reset_high_pipeline_out_)
);

pipeline_w_77_d_0 sram_signals_pipeline (
  .clk(CLK),
  .clk_en(1'h1),
  .in_(sram_signals_pipeline_in_),
  .reset(RESET),
  .out_(sram_signals_pipeline_out_)
);

pipeline_w_64_d_0 sram_signals_output_pipeline (
  .clk(CLK),
  .clk_en(1'h1),
  .in_(Q_w),
  .reset(RESET),
  .out_(Q)
);

generate
  if (BRAM == 0) begin : TAPOUT_inst
    ip224uhdlp1p11rf_4096x64m4b2c1s1_t0r0p0d0a1m1h sram_array_0 (
      .adr(A_SRAM_d),
      .clk(CLK),
      .clkbyp(1'h0),
      .din(D_d),
      .fwen(RESET),
      .mc(3'h0),
      .mcen(1'h0),
      .ren(sram_array_0_ren),
      .wa(2'h0),
      .wbeb(BWEB_d),
      .wen(sram_array_0_wen),
      .wpulse(2'h0),
      .wpulseen(1'h0),
      .q(sram_array_0_q)
    );

    ip224uhdlp1p11rf_4096x64m4b2c1s1_t0r0p0d0a1m1h sram_array_1 (
      .adr(A_SRAM_d),
      .clk(CLK),
      .clkbyp(1'h0),
      .din(D_d),
      .fwen(RESET),
      .mc(3'h0),
      .mcen(1'h0),
      .ren(sram_array_1_ren),
      .wa(2'h0),
      .wbeb(BWEB_d),
      .wen(sram_array_1_wen),
      .wpulse(2'h0),
      .wpulseen(1'h0),
      .q(sram_array_1_q)
    );

  end else begin : BRAM_inst
    sram_as_bram sram_array_0 (
      .adr(A_SRAM_d),
      .clk(CLK),
      .din(D_d),
      .fwen(RESET),
      .ren(sram_array_0_ren),
      .wbeb({BWEB_d[56],BWEB_d[48],BWEB_d[40],BWEB_d[32],BWEB_d[24],BWEB_d[16],BWEB_d[8],BWEB_d[0]}),
      .wen(sram_array_0_wen),
      .q(sram_array_0_q)
    );

    sram_as_bram sram_array_1 (
      .adr(A_SRAM_d),
      .clk(CLK),
      .din(D_d),
      .fwen(RESET),
      .ren(sram_array_1_ren),
      .wbeb({BWEB_d[56],BWEB_d[48],BWEB_d[40],BWEB_d[32],BWEB_d[24],BWEB_d[16],BWEB_d[8],BWEB_d[0]}),
      .wen(sram_array_1_wen),
      .q(sram_array_1_q)
    );
  end
endgenerate

endmodule

module sram_as_bram (
  input logic [11:0] adr,
  input logic clk,
  input logic [63:0] din,
  input logic fwen,
  input logic ren,
  input logic [7:0] wbeb,
  input logic wen,
  output logic [63:0] q
);

logic [7:0] data_array0 [4095:0];
logic [7:0] data_array1 [4095:0];
logic [7:0] data_array2 [4095:0];
logic [7:0] data_array3 [4095:0];
logic [7:0] data_array4 [4095:0];
logic [7:0] data_array5 [4095:0];
logic [7:0] data_array6 [4095:0];
logic [7:0] data_array7 [4095:0];

always_ff @(posedge clk) begin
  if (wen == 1'h1) begin
    if (~wbeb[0]) begin
      data_array0[adr] <= din[7:0];
    end
    if (~wbeb[1]) begin
      data_array1[adr] <= din[15:8];
    end
    if (~wbeb[2]) begin
      data_array2[adr] <= din[23:16];
    end
    if (~wbeb[3]) begin
      data_array3[adr] <= din[31:24];
    end
    if (~wbeb[4]) begin
      data_array4[adr] <= din[39:32];
    end
    if (~wbeb[5]) begin
      data_array5[adr] <= din[47:40];
    end
    if (~wbeb[6]) begin
      data_array6[adr] <= din[55:48];
    end
    if (~wbeb[7]) begin
      data_array7[adr] <= din[63:56];
    end
  end
  else if (ren == 1'h1) begin
    q <= {data_array7[adr], data_array6[adr], data_array5[adr], data_array4[adr], data_array3[adr], data_array2[adr], data_array1[adr], data_array0[adr]};
  end
end
endmodule


