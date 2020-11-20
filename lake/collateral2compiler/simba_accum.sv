module LakeTop (
  input logic [3:0] accum2output_forloop_dimensionality,
  input logic [5:0] [15:0] accum2output_forloop_ranges,
  input logic [15:0] accum2output_read_addr_gen_starting_addr,
  input logic [5:0] [15:0] accum2output_read_addr_gen_strides,
  input logic [15:0] accum2output_read_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] accum2output_read_sched_gen_sched_addr_gen_strides,
  input logic clk,
  input logic clk_en,
  input logic [0:0] [191:0] data_in,
  input logic flush,
  input logic [3:0] input2accum_forloop_dimensionality,
  input logic [5:0] [15:0] input2accum_forloop_ranges,
  input logic [15:0] input2accum_write_addr_gen_starting_addr,
  input logic [5:0] [15:0] input2accum_write_addr_gen_strides,
  input logic [15:0] input2accum_write_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] input2accum_write_sched_gen_sched_addr_gen_strides,
  input logic rst_n,
  input logic tile_en,
  output logic [0:0] [191:0] data_out
);

logic accum2output_accessor_valid;
logic accum2output_forloop_clk;
logic [2:0] accum2output_forloop_mux_sel_out;
logic accum2output_forloop_restart;
logic [15:0] accum2output_read_addr_gen_addr_out;
logic accum2output_read_addr_gen_clk;
logic accum2output_read_sched_gen_clk;
logic accum_clk;
logic [0:0][0:0][191:0] accum_data_in;
logic [5:0] accum_read_write_addr [0:0];
logic [15:0] cycle_count;
logic gclk;
logic input2accum_accessor_valid;
logic input2accum_forloop_clk;
logic [2:0] input2accum_forloop_mux_sel_out;
logic input2accum_forloop_restart;
logic [15:0] input2accum_write_addr_gen_addr_out;
logic input2accum_write_addr_gen_clk;
logic input2accum_write_sched_gen_clk;
logic low;
logic [0:0][191:0] mem_data_out_accum;
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
assign accum_clk = gclk;
assign low = 1'h0;
assign accum_data_in[0] = data_in[0];
assign input2accum_forloop_clk = gclk;
assign input2accum_write_addr_gen_clk = gclk;
assign accum_read_write_addr[0] = input2accum_write_addr_gen_addr_out[5:0];
assign input2accum_write_sched_gen_clk = gclk;
assign data_out[0] = mem_data_out_accum[0];
assign accum2output_forloop_clk = gclk;
assign accum2output_read_addr_gen_clk = gclk;
assign accum_read_write_addr[0] = accum2output_read_addr_gen_addr_out[5:0];
assign accum2output_read_sched_gen_clk = gclk;
lake_mem accum (
  .clk(accum_clk),
  .clk_en(clk_en),
  .data_in(accum_data_in),
  .flush(flush),
  .read_write_addr(accum_read_write_addr),
  .rst_n(rst_n),
  .write(input2accum_accessor_valid),
  .data_out(mem_data_out_accum)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
input2accum_forloop (
  .clk(input2accum_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(input2accum_forloop_dimensionality),
  .flush(flush),
  .ranges(input2accum_forloop_ranges),
  .rst_n(rst_n),
  .step(input2accum_accessor_valid),
  .mux_sel_out(input2accum_forloop_mux_sel_out),
  .restart(input2accum_forloop_restart)
);

addr_gen_6_16 input2accum_write_addr_gen (
  .clk(input2accum_write_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(input2accum_forloop_mux_sel_out),
  .restart(input2accum_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(input2accum_write_addr_gen_starting_addr),
  .step(input2accum_accessor_valid),
  .strides(input2accum_write_addr_gen_strides),
  .addr_out(input2accum_write_addr_gen_addr_out)
);

sched_gen_6_16 input2accum_write_sched_gen (
  .clk(input2accum_write_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(input2accum_forloop_restart),
  .flush(flush),
  .mux_sel(input2accum_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(input2accum_write_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(input2accum_write_sched_gen_sched_addr_gen_strides),
  .valid_output(input2accum_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
accum2output_forloop (
  .clk(accum2output_forloop_clk),
  .clk_en(clk_en),
  .dimensionality(accum2output_forloop_dimensionality),
  .flush(flush),
  .ranges(accum2output_forloop_ranges),
  .rst_n(rst_n),
  .step(accum2output_accessor_valid),
  .mux_sel_out(accum2output_forloop_mux_sel_out),
  .restart(accum2output_forloop_restart)
);

addr_gen_6_16 accum2output_read_addr_gen (
  .clk(accum2output_read_addr_gen_clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(accum2output_forloop_mux_sel_out),
  .restart(accum2output_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(accum2output_read_addr_gen_starting_addr),
  .step(accum2output_accessor_valid),
  .strides(accum2output_read_addr_gen_strides),
  .addr_out(accum2output_read_addr_gen_addr_out)
);

sched_gen_6_16 accum2output_read_sched_gen (
  .clk(accum2output_read_sched_gen_clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .finished(accum2output_forloop_restart),
  .flush(flush),
  .mux_sel(accum2output_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(accum2output_read_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(accum2output_read_sched_gen_sched_addr_gen_strides),
  .valid_output(accum2output_accessor_valid)
);

endmodule   // LakeTop

module SRAM_NAME_generator (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [5:0] mem_addr_in_bank,
  input logic mem_cen_in_bank,
  input logic [191:0] mem_data_in_bank,
  input logic mem_wen_in_bank,
  input logic [1:0] rtsel,
  input logic [1:0] wtsel,
  output logic [191:0] mem_data_out_bank
);

logic [5:0] mem_addr_to_sram;
logic mem_inst_0_sram_cen;
logic mem_inst_0_sram_wen;
assign mem_inst_0_sram_cen = ~mem_cen_in_bank;
assign mem_inst_0_sram_wen = ~mem_wen_in_bank;
always_comb begin
  mem_addr_to_sram = mem_addr_in_bank;
end
SRAM_NAME mem_inst_0 (
  .A(mem_addr_to_sram),
  .CEB(mem_inst_0_sram_cen),
  .CLK(clk),
  .D(mem_data_in_bank),
  .RTSEL(rtsel),
  .WEB(mem_inst_0_sram_wen),
  .WTSEL(wtsel),
  .Q(mem_data_out_bank)
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
  input logic [0:0][0:0] [191:0] data_in,
  input logic flush,
  input logic [5:0] read_write_addr [0:0],
  input logic rst_n,
  input logic write,
  output logic [0:0][0:0] [191:0] data_out
);

SRAM_NAME_generator SRAM_accum (
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

endmodule   // lake_mem

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

