module LakeTop (
  input logic [3:0] agg_agg1_sram_edge_forloop_dimensionality,
  input logic [5:0] [15:0] agg_agg1_sram_edge_forloop_ranges,
  input logic [15:0] agg_agg1_sram_edge_read_addr_gen_starting_addr,
  input logic [5:0] [15:0] agg_agg1_sram_edge_read_addr_gen_strides,
  input logic [15:0] agg_agg1_sram_edge_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] agg_agg1_sram_edge_sched_gen_sched_addr_gen_strides,
  input logic [15:0] agg_agg1_sram_edge_write_addr_gen_starting_addr,
  input logic [5:0] [15:0] agg_agg1_sram_edge_write_addr_gen_strides,
  input logic clk,
  input logic clk_en,
  input logic [1:0] [15:0] data_in,
  input logic flush,
  input logic [3:0] input2agg1_forloop_dimensionality,
  input logic [5:0] [15:0] input2agg1_forloop_ranges,
  input logic [3:0] input2agg1_write_addr_gen_starting_addr,
  input logic [5:0] [3:0] input2agg1_write_addr_gen_strides,
  input logic [15:0] input2agg1_write_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] input2agg1_write_sched_gen_sched_addr_gen_strides,
  input logic [3:0] input2agg_forloop_dimensionality,
  input logic [5:0] [15:0] input2agg_forloop_ranges,
  input logic [3:0] input2agg_write_addr_gen_starting_addr,
  input logic [5:0] [3:0] input2agg_write_addr_gen_strides,
  input logic [15:0] input2agg_write_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] input2agg_write_sched_gen_sched_addr_gen_strides,
  input logic rst_n,
  input logic [3:0] sram_tb_tb1_edge_forloop_dimensionality,
  input logic [5:0] [15:0] sram_tb_tb1_edge_forloop_ranges,
  input logic [15:0] sram_tb_tb1_edge_read_addr_gen_starting_addr,
  input logic [5:0] [15:0] sram_tb_tb1_edge_read_addr_gen_strides,
  input logic [15:0] sram_tb_tb1_edge_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] sram_tb_tb1_edge_sched_gen_sched_addr_gen_strides,
  input logic [15:0] sram_tb_tb1_edge_write_addr_gen_starting_addr,
  input logic [5:0] [15:0] sram_tb_tb1_edge_write_addr_gen_strides,
  input logic [3:0] tb12output_forloop_dimensionality,
  input logic [5:0] [15:0] tb12output_forloop_ranges,
  input logic [3:0] tb12output_read_addr_gen_starting_addr,
  input logic [5:0] [3:0] tb12output_read_addr_gen_strides,
  input logic [15:0] tb12output_read_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] tb12output_read_sched_gen_sched_addr_gen_strides,
  input logic [3:0] tb2output_forloop_dimensionality,
  input logic [5:0] [15:0] tb2output_forloop_ranges,
  input logic [3:0] tb2output_read_addr_gen_starting_addr,
  input logic [5:0] [3:0] tb2output_read_addr_gen_strides,
  input logic [15:0] tb2output_read_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] tb2output_read_sched_gen_sched_addr_gen_strides,
  input logic tile_en,
  output logic [1:0] [15:0] data_out
);

logic agg1_clk;
logic [0:0][0:0][15:0] agg1_data_in;
logic [0:0][3:0][15:0] agg1_data_out;
logic [1:0] agg1_read_addr [0:0];
logic [1:0] agg1_write_addr [0:0];
logic agg_agg1_sram_edge_accessor_valid;
logic agg_agg1_sram_edge_forloop_clk;
logic [2:0] agg_agg1_sram_edge_forloop_mux_sel_out;
logic agg_agg1_sram_edge_forloop_restart;
logic agg_agg1_sram_edge_mux_sel;
logic [15:0] agg_agg1_sram_edge_read_addr_gen_addr_out;
logic agg_agg1_sram_edge_read_addr_gen_clk;
logic agg_agg1_sram_edge_sched_gen_clk;
logic [15:0] agg_agg1_sram_edge_write_addr_gen_addr_out;
logic agg_agg1_sram_edge_write_addr_gen_clk;
logic agg_clk;
logic [0:0][0:0][15:0] agg_data_in;
logic [0:0][3:0][15:0] agg_data_out;
logic [1:0] agg_read_addr [0:0];
logic [1:0] agg_write_addr [0:0];
logic [15:0] cycle_count;
logic gclk;
logic input2agg1_accessor_valid;
logic input2agg1_forloop_clk;
logic [2:0] input2agg1_forloop_mux_sel_out;
logic input2agg1_forloop_restart;
logic [3:0] input2agg1_write_addr_gen_addr_out;
logic input2agg1_write_addr_gen_clk;
logic input2agg1_write_sched_gen_clk;
logic input2agg_accessor_valid;
logic input2agg_forloop_clk;
logic [2:0] input2agg_forloop_mux_sel_out;
logic input2agg_forloop_restart;
logic [3:0] input2agg_write_addr_gen_addr_out;
logic input2agg_write_addr_gen_clk;
logic input2agg_write_sched_gen_clk;
logic low;
logic [3:0][15:0] mem_data_out_agg;
logic [3:0][15:0] mem_data_out_agg1;
logic [3:0][15:0] mem_data_out_sram;
logic [0:0][15:0] mem_data_out_tb;
logic [0:0][15:0] mem_data_out_tb1;
logic sram_clk;
logic [0:0][3:0][15:0] sram_data_in;
logic [0:0][3:0][15:0] sram_data_out;
logic [8:0] sram_read_write_addr [0:0];
logic sram_tb_tb1_edge_accessor_valid;
logic [15:0] sram_tb_tb1_edge_adjust_addr;
logic [0:0][2:0] sram_tb_tb1_edge_delayed_mux_sels;
logic sram_tb_tb1_edge_delayed_restarts;
logic sram_tb_tb1_edge_delayed_writes;
logic sram_tb_tb1_edge_forloop_clk;
logic [2:0] sram_tb_tb1_edge_forloop_mux_sel_out;
logic sram_tb_tb1_edge_forloop_restart;
logic sram_tb_tb1_edge_mux_sel_to;
logic [15:0] sram_tb_tb1_edge_read_addr_gen_addr_out;
logic sram_tb_tb1_edge_read_addr_gen_clk;
logic sram_tb_tb1_edge_sched_gen_clk;
logic [15:0] sram_tb_tb1_edge_write_addr_gen_addr_out;
logic sram_tb_tb1_edge_write_addr_gen_clk;
logic [2:0] sram_tb_tb1_edge_write_addr_gen_mux_sel;
logic sram_tb_tb1_edge_write_addr_gen_restart;
logic sram_tb_tb1_edge_write_addr_gen_step;
logic tb12output_accessor_valid;
logic tb12output_forloop_clk;
logic [2:0] tb12output_forloop_mux_sel_out;
logic tb12output_forloop_restart;
logic [3:0] tb12output_read_addr_gen_addr_out;
logic tb12output_read_addr_gen_clk;
logic tb12output_read_sched_gen_clk;
logic tb1_clk;
logic [2:0] tb1_read_addr [0:0];
logic tb1_write;
logic [2:0] tb1_write_addr [0:0];
logic tb2output_accessor_valid;
logic tb2output_forloop_clk;
logic [2:0] tb2output_forloop_mux_sel_out;
logic tb2output_forloop_restart;
logic [3:0] tb2output_read_addr_gen_addr_out;
logic tb2output_read_addr_gen_clk;
logic tb2output_read_sched_gen_clk;
logic tb_clk;
logic [2:0] tb_read_addr [0:0];
logic tb_write;
logic [2:0] tb_write_addr [0:0];
assign gclk = clk & tile_en;

always_ff @(posedge gclk, negedge rst_n) begin
  if (~rst_n) begin
    cycle_count <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      cycle_count <= 16'h0;
    end
    else cycle_count <= cycle_count + 16'h1;
  end
end
assign agg_clk = gclk;
assign mem_data_out_agg = agg_data_out;
assign agg1_clk = gclk;
assign mem_data_out_agg1 = agg1_data_out;
assign sram_clk = gclk;
assign mem_data_out_sram = sram_data_out;
assign tb_clk = gclk;
assign tb1_clk = gclk;
assign low = 1'h0;
assign agg_data_in[0] = data_in[0];
assign input2agg_forloop_clk = gclk;
assign input2agg_write_addr_gen_clk = gclk;
assign agg_write_addr[0] = input2agg_write_addr_gen_addr_out[1:0];
assign input2agg_write_sched_gen_clk = gclk;
assign agg1_data_in[0] = data_in[1];
assign input2agg1_forloop_clk = gclk;
assign input2agg1_write_addr_gen_clk = gclk;
assign agg1_write_addr[0] = input2agg1_write_addr_gen_addr_out[1:0];
assign input2agg1_write_sched_gen_clk = gclk;
assign data_out[0] = mem_data_out_tb[0];
assign tb2output_forloop_clk = gclk;
assign tb2output_read_addr_gen_clk = gclk;
assign tb_read_addr[0] = tb2output_read_addr_gen_addr_out[2:0];
assign tb2output_read_sched_gen_clk = gclk;
assign data_out[1] = mem_data_out_tb1[0];
assign tb12output_forloop_clk = gclk;
assign tb12output_read_addr_gen_clk = gclk;
assign tb1_read_addr[0] = tb12output_read_addr_gen_addr_out[2:0];
assign tb12output_read_sched_gen_clk = gclk;
assign agg_agg1_sram_edge_forloop_clk = gclk;
assign agg_agg1_sram_edge_read_addr_gen_clk = gclk;
assign agg_read_addr[0] = agg_agg1_sram_edge_read_addr_gen_addr_out[1:0];
assign agg1_read_addr[0] = agg_agg1_sram_edge_read_addr_gen_addr_out[1:0];
assign agg_agg1_sram_edge_mux_sel = agg_agg1_sram_edge_read_addr_gen_addr_out[3:2][0];
always_comb begin
  if (agg_agg1_sram_edge_mux_sel == 1'h0) begin
    sram_data_in = agg_data_out;
  end
  else sram_data_in = agg1_data_out;
end
assign agg_agg1_sram_edge_write_addr_gen_clk = gclk;
assign agg_agg1_sram_edge_sched_gen_clk = gclk;
assign sram_tb_tb1_edge_forloop_clk = gclk;
assign sram_tb_tb1_edge_read_addr_gen_clk = gclk;
assign sram_tb_tb1_edge_write_addr_gen_clk = gclk;
always_comb begin
  sram_tb_tb1_edge_adjust_addr = sram_tb_tb1_edge_write_addr_gen_addr_out;
end
assign tb_write_addr[0] = sram_tb_tb1_edge_adjust_addr[2:0];
always_comb begin
  sram_tb_tb1_edge_adjust_addr = sram_tb_tb1_edge_write_addr_gen_addr_out;
end
assign tb1_write_addr[0] = sram_tb_tb1_edge_adjust_addr[2:0];

always_ff @(posedge gclk, negedge rst_n) begin
  if (~rst_n) begin
    sram_tb_tb1_edge_delayed_writes <= 1'h0;
    sram_tb_tb1_edge_delayed_mux_sels <= 3'h0;
    sram_tb_tb1_edge_delayed_restarts <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      sram_tb_tb1_edge_delayed_writes <= 1'h0;
      sram_tb_tb1_edge_delayed_mux_sels <= 3'h0;
      sram_tb_tb1_edge_delayed_restarts <= 1'h0;
    end
    else begin
      sram_tb_tb1_edge_delayed_writes <= sram_tb_tb1_edge_accessor_valid;
      sram_tb_tb1_edge_delayed_mux_sels[0] <= sram_tb_tb1_edge_forloop_mux_sel_out;
      sram_tb_tb1_edge_delayed_restarts <= sram_tb_tb1_edge_forloop_restart;
    end
  end
end
assign sram_tb_tb1_edge_mux_sel_to = sram_tb_tb1_edge_write_addr_gen_addr_out[4:3][0];
always_comb begin
  if (sram_tb_tb1_edge_mux_sel_to == 1'h0) begin
    tb_write = sram_tb_tb1_edge_delayed_writes;
  end
  else tb_write = low;
  if (sram_tb_tb1_edge_mux_sel_to == 1'h1) begin
    tb1_write = sram_tb_tb1_edge_delayed_writes;
  end
  else tb1_write = low;
end
assign sram_tb_tb1_edge_write_addr_gen_step = sram_tb_tb1_edge_delayed_writes;
assign sram_tb_tb1_edge_write_addr_gen_mux_sel = sram_tb_tb1_edge_delayed_mux_sels[0];
assign sram_tb_tb1_edge_write_addr_gen_restart = sram_tb_tb1_edge_delayed_restarts;
assign sram_tb_tb1_edge_sched_gen_clk = gclk;
always_comb begin
  if (agg_agg1_sram_edge_accessor_valid == 1'h1) begin
    sram_read_write_addr[0] = agg_agg1_sram_edge_write_addr_gen_addr_out[8:0];
  end
  else sram_read_write_addr[0] = sram_tb_tb1_edge_read_addr_gen_addr_out[8:0];
end
lake_mem agg (
  .clk(agg_clk),
  .clk_en(clk_en),
  .data_in(agg_data_in),
  .flush(flush),
  .read_addr(agg_read_addr),
  .rst_n(rst_n),
  .write(input2agg_accessor_valid),
  .write_addr(agg_write_addr),
  .data_out(agg_data_out)
);

lake_mem agg1 (
  .clk(agg1_clk),
  .clk_en(clk_en),
  .data_in(agg1_data_in),
  .flush(flush),
  .read_addr(agg1_read_addr),
  .rst_n(rst_n),
  .write(input2agg1_accessor_valid),
  .write_addr(agg1_write_addr),
  .data_out(agg1_data_out)
);

lake_mem_unq0 sram (
  .clk(sram_clk),
  .clk_en(clk_en),
  .data_in(sram_data_in),
  .flush(flush),
  .read_write_addr(sram_read_write_addr),
  .rst_n(rst_n),
  .write(agg_agg1_sram_edge_accessor_valid),
  .data_out(sram_data_out)
);

lake_mem_unq1 tb (
  .clk(tb_clk),
  .clk_en(clk_en),
  .data_in(sram_data_out),
  .flush(flush),
  .read_addr(tb_read_addr),
  .rst_n(rst_n),
  .write(tb_write),
  .write_addr(tb_write_addr),
  .data_out(mem_data_out_tb)
);

lake_mem_unq1 tb1 (
  .clk(tb1_clk),
  .clk_en(clk_en),
  .data_in(sram_data_out),
  .flush(flush),
  .read_addr(tb1_read_addr),
  .rst_n(rst_n),
  .write(tb1_write),
  .write_addr(tb1_write_addr),
  .data_out(mem_data_out_tb1)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
input2agg_forloop (
  .clk(input2agg_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(input2agg_forloop_dimensionality),
  .flush(flush),
  .ranges(input2agg_forloop_ranges),
  .rst_n(rst_n),
  .step(input2agg_accessor_valid),
  .mux_sel_out(input2agg_forloop_mux_sel_out),
  .restart(input2agg_forloop_restart)
);

addr_gen_6_4 input2agg_write_addr_gen (
  .clk(input2agg_write_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(input2agg_forloop_mux_sel_out),
  .restart(input2agg_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(input2agg_write_addr_gen_starting_addr),
  .step(input2agg_accessor_valid),
  .strides(input2agg_write_addr_gen_strides),
  .addr_out(input2agg_write_addr_gen_addr_out)
);

sched_gen_6_16 input2agg_write_sched_gen (
  .clk(input2agg_write_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(input2agg_forloop_restart),
  .flush(flush),
  .mux_sel(input2agg_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(input2agg_write_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(input2agg_write_sched_gen_sched_addr_gen_strides),
  .valid_output(input2agg_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
input2agg1_forloop (
  .clk(input2agg1_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(input2agg1_forloop_dimensionality),
  .flush(flush),
  .ranges(input2agg1_forloop_ranges),
  .rst_n(rst_n),
  .step(input2agg1_accessor_valid),
  .mux_sel_out(input2agg1_forloop_mux_sel_out),
  .restart(input2agg1_forloop_restart)
);

addr_gen_6_4 input2agg1_write_addr_gen (
  .clk(input2agg1_write_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(input2agg1_forloop_mux_sel_out),
  .restart(input2agg1_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(input2agg1_write_addr_gen_starting_addr),
  .step(input2agg1_accessor_valid),
  .strides(input2agg1_write_addr_gen_strides),
  .addr_out(input2agg1_write_addr_gen_addr_out)
);

sched_gen_6_16 input2agg1_write_sched_gen (
  .clk(input2agg1_write_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(input2agg1_forloop_restart),
  .flush(flush),
  .mux_sel(input2agg1_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(input2agg1_write_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(input2agg1_write_sched_gen_sched_addr_gen_strides),
  .valid_output(input2agg1_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
tb2output_forloop (
  .clk(tb2output_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(tb2output_forloop_dimensionality),
  .flush(flush),
  .ranges(tb2output_forloop_ranges),
  .rst_n(rst_n),
  .step(tb2output_accessor_valid),
  .mux_sel_out(tb2output_forloop_mux_sel_out),
  .restart(tb2output_forloop_restart)
);

addr_gen_6_4 tb2output_read_addr_gen (
  .clk(tb2output_read_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(tb2output_forloop_mux_sel_out),
  .restart(tb2output_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(tb2output_read_addr_gen_starting_addr),
  .step(tb2output_accessor_valid),
  .strides(tb2output_read_addr_gen_strides),
  .addr_out(tb2output_read_addr_gen_addr_out)
);

sched_gen_6_16 tb2output_read_sched_gen (
  .clk(tb2output_read_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(tb2output_forloop_restart),
  .flush(flush),
  .mux_sel(tb2output_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(tb2output_read_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(tb2output_read_sched_gen_sched_addr_gen_strides),
  .valid_output(tb2output_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
tb12output_forloop (
  .clk(tb12output_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(tb12output_forloop_dimensionality),
  .flush(flush),
  .ranges(tb12output_forloop_ranges),
  .rst_n(rst_n),
  .step(tb12output_accessor_valid),
  .mux_sel_out(tb12output_forloop_mux_sel_out),
  .restart(tb12output_forloop_restart)
);

addr_gen_6_4 tb12output_read_addr_gen (
  .clk(tb12output_read_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(tb12output_forloop_mux_sel_out),
  .restart(tb12output_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(tb12output_read_addr_gen_starting_addr),
  .step(tb12output_accessor_valid),
  .strides(tb12output_read_addr_gen_strides),
  .addr_out(tb12output_read_addr_gen_addr_out)
);

sched_gen_6_16 tb12output_read_sched_gen (
  .clk(tb12output_read_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(tb12output_forloop_restart),
  .flush(flush),
  .mux_sel(tb12output_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(tb12output_read_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(tb12output_read_sched_gen_sched_addr_gen_strides),
  .valid_output(tb12output_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
agg_agg1_sram_edge_forloop (
  .clk(agg_agg1_sram_edge_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(agg_agg1_sram_edge_forloop_dimensionality),
  .flush(flush),
  .ranges(agg_agg1_sram_edge_forloop_ranges),
  .rst_n(rst_n),
  .step(agg_agg1_sram_edge_accessor_valid),
  .mux_sel_out(agg_agg1_sram_edge_forloop_mux_sel_out),
  .restart(agg_agg1_sram_edge_forloop_restart)
);

addr_gen_6_16 agg_agg1_sram_edge_read_addr_gen (
  .clk(agg_agg1_sram_edge_read_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(agg_agg1_sram_edge_forloop_mux_sel_out),
  .restart(agg_agg1_sram_edge_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(agg_agg1_sram_edge_read_addr_gen_starting_addr),
  .step(agg_agg1_sram_edge_accessor_valid),
  .strides(agg_agg1_sram_edge_read_addr_gen_strides),
  .addr_out(agg_agg1_sram_edge_read_addr_gen_addr_out)
);

addr_gen_6_16 agg_agg1_sram_edge_write_addr_gen (
  .clk(agg_agg1_sram_edge_write_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(agg_agg1_sram_edge_forloop_mux_sel_out),
  .restart(agg_agg1_sram_edge_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(agg_agg1_sram_edge_write_addr_gen_starting_addr),
  .step(agg_agg1_sram_edge_accessor_valid),
  .strides(agg_agg1_sram_edge_write_addr_gen_strides),
  .addr_out(agg_agg1_sram_edge_write_addr_gen_addr_out)
);

sched_gen_6_16 agg_agg1_sram_edge_sched_gen (
  .clk(agg_agg1_sram_edge_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(agg_agg1_sram_edge_forloop_restart),
  .flush(flush),
  .mux_sel(agg_agg1_sram_edge_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(agg_agg1_sram_edge_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(agg_agg1_sram_edge_sched_gen_sched_addr_gen_strides),
  .valid_output(agg_agg1_sram_edge_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
sram_tb_tb1_edge_forloop (
  .clk(sram_tb_tb1_edge_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(sram_tb_tb1_edge_forloop_dimensionality),
  .flush(flush),
  .ranges(sram_tb_tb1_edge_forloop_ranges),
  .rst_n(rst_n),
  .step(sram_tb_tb1_edge_accessor_valid),
  .mux_sel_out(sram_tb_tb1_edge_forloop_mux_sel_out),
  .restart(sram_tb_tb1_edge_forloop_restart)
);

addr_gen_6_16 sram_tb_tb1_edge_read_addr_gen (
  .clk(sram_tb_tb1_edge_read_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(sram_tb_tb1_edge_forloop_mux_sel_out),
  .restart(sram_tb_tb1_edge_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(sram_tb_tb1_edge_read_addr_gen_starting_addr),
  .step(sram_tb_tb1_edge_accessor_valid),
  .strides(sram_tb_tb1_edge_read_addr_gen_strides),
  .addr_out(sram_tb_tb1_edge_read_addr_gen_addr_out)
);

addr_gen_6_16 sram_tb_tb1_edge_write_addr_gen (
  .clk(sram_tb_tb1_edge_write_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(sram_tb_tb1_edge_write_addr_gen_mux_sel),
  .restart(sram_tb_tb1_edge_write_addr_gen_restart),
  .rst_n(rst_n),
  .starting_addr(sram_tb_tb1_edge_write_addr_gen_starting_addr),
  .step(sram_tb_tb1_edge_write_addr_gen_step),
  .strides(sram_tb_tb1_edge_write_addr_gen_strides),
  .addr_out(sram_tb_tb1_edge_write_addr_gen_addr_out)
);

sched_gen_6_16 sram_tb_tb1_edge_sched_gen (
  .clk(sram_tb_tb1_edge_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(sram_tb_tb1_edge_forloop_restart),
  .flush(flush),
  .mux_sel(sram_tb_tb1_edge_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(sram_tb_tb1_edge_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(sram_tb_tb1_edge_sched_gen_sched_addr_gen_strides),
  .valid_output(sram_tb_tb1_edge_accessor_valid)
);

endmodule   // LakeTop

module ReverseFlatten (
  input logic clk_en,
  input logic flush,
  input logic [63:0] input_array,
  output logic [3:0][0:0] [15:0] output_array
);

always_comb begin
  output_array[0] = input_array[15:0];
  output_array[1] = input_array[31:16];
  output_array[2] = input_array[47:32];
  output_array[3] = input_array[63:48];
end
endmodule   // ReverseFlatten

module SRAM_NAME_generator (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [8:0] mem_addr_in_bank,
  input logic mem_cen_in_bank,
  input logic [3:0] [15:0] mem_data_in_bank,
  input logic mem_wen_in_bank,
  input logic [1:0] rtsel,
  input logic [1:0] wtsel,
  output logic [0:0][3:0] [15:0] mem_data_out_bank
);

logic [8:0] mem_addr_to_sram;
logic mem_inst_0_pt_0_sram_cen;
logic [31:0] mem_inst_0_pt_0_sram_data_in;
logic [31:0] mem_inst_0_pt_0_sram_data_out;
logic mem_inst_0_pt_0_sram_wen;
logic mem_inst_0_pt_1_sram_cen;
logic [31:0] mem_inst_0_pt_1_sram_data_in;
logic [31:0] mem_inst_0_pt_1_sram_data_out;
logic mem_inst_0_pt_1_sram_wen;
logic [63:0] sram_mem_data_in_bank;
logic [63:0] sram_mem_data_out_bank;
assign mem_inst_0_pt_0_sram_cen = ~mem_cen_in_bank;
assign mem_inst_0_pt_0_sram_data_in = sram_mem_data_in_bank[31:0];
assign sram_mem_data_out_bank[31:0] = mem_inst_0_pt_0_sram_data_out;
assign mem_inst_0_pt_0_sram_wen = ~mem_wen_in_bank;
assign mem_inst_0_pt_1_sram_cen = ~mem_cen_in_bank;
assign mem_inst_0_pt_1_sram_data_in = sram_mem_data_in_bank[63:32];
assign sram_mem_data_out_bank[63:32] = mem_inst_0_pt_1_sram_data_out;
assign mem_inst_0_pt_1_sram_wen = ~mem_wen_in_bank;
always_comb begin
  mem_addr_to_sram = mem_addr_in_bank;
end
flattenND flatten_data_in_0 (
  .clk_en(clk_en),
  .flush(flush),
  .input_array(mem_data_in_bank),
  .output_array(sram_mem_data_in_bank)
);

SRAM_NAME mem_inst_0_pt_0 (
  .A(mem_addr_to_sram),
  .CEB(mem_inst_0_pt_0_sram_cen),
  .CLK(clk),
  .D(mem_inst_0_pt_0_sram_data_in),
  .RTSEL(rtsel),
  .WEB(mem_inst_0_pt_0_sram_wen),
  .WTSEL(wtsel),
  .Q(mem_inst_0_pt_0_sram_data_out)
);

SRAM_NAME mem_inst_0_pt_1 (
  .A(mem_addr_to_sram),
  .CEB(mem_inst_0_pt_1_sram_cen),
  .CLK(clk),
  .D(mem_inst_0_pt_1_sram_data_in),
  .RTSEL(rtsel),
  .WEB(mem_inst_0_pt_1_sram_wen),
  .WTSEL(wtsel),
  .Q(mem_inst_0_pt_1_sram_data_out)
);

ReverseFlatten flatten_data_out_0 (
  .clk_en(clk_en),
  .flush(flush),
  .input_array(sram_mem_data_out_bank),
  .output_array(mem_data_out_bank)
);

endmodule   // SRAM_NAME_generator

module addr_gen_6_16 (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [2:0] mux_sel,
  input logic restart,
  input logic rst_n,
  input logic [15:0] starting_addr,
  input logic step,
  input logic [5:0] [15:0] strides,
  output logic [15:0] addr_out
);

logic [15:0] calc_addr;
logic [15:0] current_addr;
logic [15:0] strt_addr;
assign strt_addr = starting_addr;
assign addr_out = calc_addr;
assign calc_addr = strt_addr + current_addr;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    current_addr <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      current_addr <= 16'h0;
    end
    else if (step) begin
      if (restart) begin
        current_addr <= 16'h0;
      end
      else current_addr <= current_addr + strides[mux_sel];
    end
  end
end
endmodule   // addr_gen_6_16

module addr_gen_6_4 (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [2:0] mux_sel,
  input logic restart,
  input logic rst_n,
  input logic [3:0] starting_addr,
  input logic step,
  input logic [5:0] [3:0] strides,
  output logic [3:0] addr_out
);

logic [3:0] calc_addr;
logic [3:0] current_addr;
logic [3:0] strt_addr;
assign strt_addr = starting_addr;
assign addr_out = calc_addr;
assign calc_addr = strt_addr + current_addr;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    current_addr <= 4'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      current_addr <= 4'h0;
    end
    else if (step) begin
      if (restart) begin
        current_addr <= 4'h0;
      end
      else current_addr <= current_addr + strides[mux_sel];
    end
  end
end
endmodule   // addr_gen_6_4

module flattenND (
  input logic clk_en,
  input logic flush,
  input logic [3:0][0:0] [15:0] input_array,
  output logic [63:0] output_array
);

always_comb begin
  output_array[15:0] = input_array[0];
  output_array[31:16] = input_array[1];
  output_array[47:32] = input_array[2];
  output_array[63:48] = input_array[3];
end
endmodule   // flattenND

module for_loop_6_16 #(
  parameter CONFIG_WIDTH = 5'h10,
  parameter ITERATOR_SUPPORT = 4'h6
)
(
  input logic clk,
  input logic clk_en,
  input logic [3:0] dimensionality,
  input logic flush,
  input logic [5:0] [15:0] ranges,
  input logic rst_n,
  input logic step,
  output logic [2:0] mux_sel_out,
  output logic restart
);

logic [5:0] clear;
logic [5:0][15:0] dim_counter;
logic done;
logic [5:0] inc;
logic [15:0] inced_cnt;
logic [5:0] max_value;
logic maxed_value;
logic [2:0] mux_sel;
assign mux_sel_out = mux_sel;
assign inced_cnt = dim_counter[mux_sel] + 16'h1;
assign maxed_value = (dim_counter[mux_sel] == ranges[mux_sel]) & inc[mux_sel];
always_comb begin
  mux_sel = 3'h0;
  done = 1'h0;
  if (~done) begin
    if ((~max_value[0]) & (dimensionality > 4'h0)) begin
      mux_sel = 3'h0;
      done = 1'h1;
    end
  end
  if (~done) begin
    if ((~max_value[1]) & (dimensionality > 4'h1)) begin
      mux_sel = 3'h1;
      done = 1'h1;
    end
  end
  if (~done) begin
    if ((~max_value[2]) & (dimensionality > 4'h2)) begin
      mux_sel = 3'h2;
      done = 1'h1;
    end
  end
  if (~done) begin
    if ((~max_value[3]) & (dimensionality > 4'h3)) begin
      mux_sel = 3'h3;
      done = 1'h1;
    end
  end
  if (~done) begin
    if ((~max_value[4]) & (dimensionality > 4'h4)) begin
      mux_sel = 3'h4;
      done = 1'h1;
    end
  end
  if (~done) begin
    if ((~max_value[5]) & (dimensionality > 4'h5)) begin
      mux_sel = 3'h5;
      done = 1'h1;
    end
  end
end
always_comb begin
  clear[0] = 1'h0;
  if (((mux_sel > 3'h0) & step) | (~done)) begin
    clear[0] = 1'h1;
  end
end
always_comb begin
  inc[0] = 1'h0;
  if ((5'h0 == 5'h0) & step & (dimensionality > 4'h0)) begin
    inc[0] = 1'h1;
  end
  else if ((mux_sel == 3'h0) & step & (dimensionality > 4'h0)) begin
    inc[0] = 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[0] <= 16'h0;
    end
    else if (clear[0]) begin
      dim_counter[0] <= 16'h0;
    end
    else if (inc[0]) begin
      dim_counter[0] <= inced_cnt;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      max_value[0] <= 1'h0;
    end
    else if (clear[0]) begin
      max_value[0] <= 1'h0;
    end
    else if (inc[0]) begin
      max_value[0] <= maxed_value;
    end
  end
end
always_comb begin
  clear[1] = 1'h0;
  if (((mux_sel > 3'h1) & step) | (~done)) begin
    clear[1] = 1'h1;
  end
end
always_comb begin
  inc[1] = 1'h0;
  if ((5'h1 == 5'h0) & step & (dimensionality > 4'h1)) begin
    inc[1] = 1'h1;
  end
  else if ((mux_sel == 3'h1) & step & (dimensionality > 4'h1)) begin
    inc[1] = 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[1] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[1] <= 16'h0;
    end
    else if (clear[1]) begin
      dim_counter[1] <= 16'h0;
    end
    else if (inc[1]) begin
      dim_counter[1] <= inced_cnt;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      max_value[1] <= 1'h0;
    end
    else if (clear[1]) begin
      max_value[1] <= 1'h0;
    end
    else if (inc[1]) begin
      max_value[1] <= maxed_value;
    end
  end
end
always_comb begin
  clear[2] = 1'h0;
  if (((mux_sel > 3'h2) & step) | (~done)) begin
    clear[2] = 1'h1;
  end
end
always_comb begin
  inc[2] = 1'h0;
  if ((5'h2 == 5'h0) & step & (dimensionality > 4'h2)) begin
    inc[2] = 1'h1;
  end
  else if ((mux_sel == 3'h2) & step & (dimensionality > 4'h2)) begin
    inc[2] = 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[2] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[2] <= 16'h0;
    end
    else if (clear[2]) begin
      dim_counter[2] <= 16'h0;
    end
    else if (inc[2]) begin
      dim_counter[2] <= inced_cnt;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[2] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      max_value[2] <= 1'h0;
    end
    else if (clear[2]) begin
      max_value[2] <= 1'h0;
    end
    else if (inc[2]) begin
      max_value[2] <= maxed_value;
    end
  end
end
always_comb begin
  clear[3] = 1'h0;
  if (((mux_sel > 3'h3) & step) | (~done)) begin
    clear[3] = 1'h1;
  end
end
always_comb begin
  inc[3] = 1'h0;
  if ((5'h3 == 5'h0) & step & (dimensionality > 4'h3)) begin
    inc[3] = 1'h1;
  end
  else if ((mux_sel == 3'h3) & step & (dimensionality > 4'h3)) begin
    inc[3] = 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[3] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[3] <= 16'h0;
    end
    else if (clear[3]) begin
      dim_counter[3] <= 16'h0;
    end
    else if (inc[3]) begin
      dim_counter[3] <= inced_cnt;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[3] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      max_value[3] <= 1'h0;
    end
    else if (clear[3]) begin
      max_value[3] <= 1'h0;
    end
    else if (inc[3]) begin
      max_value[3] <= maxed_value;
    end
  end
end
always_comb begin
  clear[4] = 1'h0;
  if (((mux_sel > 3'h4) & step) | (~done)) begin
    clear[4] = 1'h1;
  end
end
always_comb begin
  inc[4] = 1'h0;
  if ((5'h4 == 5'h0) & step & (dimensionality > 4'h4)) begin
    inc[4] = 1'h1;
  end
  else if ((mux_sel == 3'h4) & step & (dimensionality > 4'h4)) begin
    inc[4] = 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[4] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[4] <= 16'h0;
    end
    else if (clear[4]) begin
      dim_counter[4] <= 16'h0;
    end
    else if (inc[4]) begin
      dim_counter[4] <= inced_cnt;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[4] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      max_value[4] <= 1'h0;
    end
    else if (clear[4]) begin
      max_value[4] <= 1'h0;
    end
    else if (inc[4]) begin
      max_value[4] <= maxed_value;
    end
  end
end
always_comb begin
  clear[5] = 1'h0;
  if (((mux_sel > 3'h5) & step) | (~done)) begin
    clear[5] = 1'h1;
  end
end
always_comb begin
  inc[5] = 1'h0;
  if ((5'h5 == 5'h0) & step & (dimensionality > 4'h5)) begin
    inc[5] = 1'h1;
  end
  else if ((mux_sel == 3'h5) & step & (dimensionality > 4'h5)) begin
    inc[5] = 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[5] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[5] <= 16'h0;
    end
    else if (clear[5]) begin
      dim_counter[5] <= 16'h0;
    end
    else if (inc[5]) begin
      dim_counter[5] <= inced_cnt;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[5] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      max_value[5] <= 1'h0;
    end
    else if (clear[5]) begin
      max_value[5] <= 1'h0;
    end
    else if (inc[5]) begin
      max_value[5] <= maxed_value;
    end
  end
end
assign restart = ~done;
endmodule   // for_loop_6_16

module lake_mem (
  input logic clk,
  input logic clk_en,
  input logic [0:0][0:0] [15:0] data_in,
  input logic flush,
  input logic [1:0] read_addr [0:0],
  input logic rst_n,
  input logic write,
  input logic [1:0] write_addr [0:0],
  output logic [0:0][3:0] [15:0] data_out
);

logic [3:0][15:0] memory;

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (write) begin
      memory[write_addr[0] + 2'h0] <= data_in[0][0];
    end
  end
end
always_comb begin
  data_out[0][0] = memory[read_addr[0] + 2'h0];
  data_out[0][1] = memory[read_addr[0] + 2'h1];
  data_out[0][2] = memory[read_addr[0] + 2'h2];
  data_out[0][3] = memory[read_addr[0] + 2'h3];
end
endmodule   // lake_mem

module lake_mem_unq0 (
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_in,
  input logic flush,
  input logic [8:0] read_write_addr [0:0],
  input logic rst_n,
  input logic write,
  output logic [0:0][3:0] [15:0] data_out
);

SRAM_NAME_generator SRAM_sram (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mem_addr_in_bank(read_write_addr),
  .mem_cen_in_bank(1'h1),
  .mem_data_in_bank(data_in),
  .mem_wen_in_bank(write),
  .rtsel(2'h1),
  .wtsel(2'h0),
  .mem_data_out_bank(data_out)
);

endmodule   // lake_mem_unq0

module lake_mem_unq1 (
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_in,
  input logic flush,
  input logic [2:0] read_addr [0:0],
  input logic rst_n,
  input logic write,
  input logic [2:0] write_addr [0:0],
  output logic [0:0][0:0] [15:0] data_out
);

logic [7:0][15:0] memory;

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (write) begin
      memory[write_addr[0] + 3'h0] <= data_in[0][0];
      memory[write_addr[0] + 3'h1] <= data_in[0][1];
      memory[write_addr[0] + 3'h2] <= data_in[0][2];
      memory[write_addr[0] + 3'h3] <= data_in[0][3];
    end
  end
end
always_comb begin
  data_out[0][0] = memory[read_addr[0] + 3'h0];
end
endmodule   // lake_mem_unq1

module sched_gen_6_16 (
  input logic clk,
  input logic clk_en,
  input logic [15:0] cycle_count,
  input logic finished,
  input logic flush,
  input logic [2:0] mux_sel,
  input logic rst_n,
  input logic [15:0] sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] sched_addr_gen_strides,
  output logic valid_output
);

logic [15:0] addr_out;
logic valid_gate;
logic valid_gate_inv;
logic valid_out;
assign valid_gate = ~valid_gate_inv;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    valid_gate_inv <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      valid_gate_inv <= 1'h0;
    end
    else if (finished) begin
      valid_gate_inv <= 1'h1;
    end
  end
end
always_comb begin
  valid_out = (cycle_count == addr_out) & valid_gate;
end
always_comb begin
  valid_output = valid_out;
end
addr_gen_6_16 sched_addr_gen (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(mux_sel),
  .restart(1'h0),
  .rst_n(rst_n),
  .starting_addr(sched_addr_gen_starting_addr),
  .step(valid_out),
  .strides(sched_addr_gen_strides),
  .addr_out(addr_out)
);

endmodule   // sched_gen_6_16

