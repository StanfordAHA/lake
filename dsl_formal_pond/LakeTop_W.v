module LakeTop (
  input logic clk_mem,
  input logic [0:0] [15:0] data_in,
  input logic [3:0] input2pond_forloop_dimensionality,
  input logic [5:0] [15:0] input2pond_forloop_ranges,
  input logic [15:0] input2pond_write_addr_gen_starting_addr,
  input logic [5:0] [15:0] input2pond_write_addr_gen_strides,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] input2pond_write_sched_gen_sched_addr_gen_strides,
  input logic [3:0] pond2output_forloop_dimensionality,
  input logic [5:0] [15:0] pond2output_forloop_ranges,
  input logic [15:0] pond2output_read_addr_gen_starting_addr,
  input logic [5:0] [15:0] pond2output_read_addr_gen_strides,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] pond2output_read_sched_gen_sched_addr_gen_strides,
  input logic rst_n,
  input logic tile_en,
  output logic [0:0] [15:0] data_out
);

logic [15:0] cycle_count;
logic gclk;
logic input2pond_accessor_valid;
logic input2pond_forloop_clk;
logic [2:0] input2pond_forloop_mux_sel_out;
logic input2pond_forloop_restart;
logic [15:0] input2pond_write_addr_gen_addr_out;
logic input2pond_write_addr_gen_clk;
logic input2pond_write_sched_gen_clk;
logic low;
logic [0:0][15:0] mem_data_out_pond;
logic pond2output_accessor_valid;
logic pond2output_forloop_clk;
logic [2:0] pond2output_forloop_mux_sel_out;
logic pond2output_forloop_restart;
logic [15:0] pond2output_read_addr_gen_addr_out;
logic pond2output_read_addr_gen_clk;
logic pond2output_read_sched_gen_clk;
logic pond_clk;
logic [0:0][0:0][15:0] pond_data_in;
logic [4:0] pond_read_addr [0:0];
logic [4:0] pond_write_addr [0:0];
assign gclk = clk_mem & tile_en;

always_ff @(posedge gclk, negedge rst_n) begin
  if (~rst_n) begin
    cycle_count <= 16'h0;
  end
  else cycle_count <= cycle_count + 16'h1;
end
assign pond_clk = gclk;
assign low = 1'h0;
assign pond_data_in[0] = data_in[0];
assign input2pond_forloop_clk = gclk;
assign input2pond_write_addr_gen_clk = gclk;
assign pond_write_addr[0] = input2pond_write_addr_gen_addr_out[4:0];
assign input2pond_write_sched_gen_clk = gclk;
assign data_out[0] = mem_data_out_pond[0];
assign pond2output_forloop_clk = gclk;
assign pond2output_read_addr_gen_clk = gclk;
assign pond_read_addr[0] = pond2output_read_addr_gen_addr_out[4:0];
assign pond2output_read_sched_gen_clk = gclk;
lake_mem pond (
  .clk(pond_clk),
  .data_in(pond_data_in),
  .read_addr(pond_read_addr),
  .rst_n(rst_n),
  .write(input2pond_accessor_valid),
  .write_addr(pond_write_addr),
  .data_out(mem_data_out_pond)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
input2pond_forloop (
  .clk(input2pond_forloop_clk),
  .dimensionality(input2pond_forloop_dimensionality),
  .ranges(input2pond_forloop_ranges),
  .rst_n(rst_n),
  .step(input2pond_accessor_valid),
  .mux_sel_out(input2pond_forloop_mux_sel_out),
  .restart(input2pond_forloop_restart)
);

addr_gen_6_16 input2pond_write_addr_gen (
  .clk(input2pond_write_addr_gen_clk),
  .mux_sel(input2pond_forloop_mux_sel_out),
  .restart(input2pond_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(input2pond_write_addr_gen_starting_addr),
  .step(input2pond_accessor_valid),
  .strides(input2pond_write_addr_gen_strides),
  .addr_out(input2pond_write_addr_gen_addr_out)
);

sched_gen_6_16 input2pond_write_sched_gen (
  .clk(input2pond_write_sched_gen_clk),
  .cycle_count(cycle_count),
  .finished(input2pond_forloop_restart),
  .mux_sel(input2pond_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(input2pond_write_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(input2pond_write_sched_gen_sched_addr_gen_strides),
  .valid_output(input2pond_accessor_valid)
);

for_loop_6_16 #(
  .CONFIG_WIDTH(5'h10),
  .ITERATOR_SUPPORT(4'h6))
pond2output_forloop (
  .clk(pond2output_forloop_clk),
  .dimensionality(pond2output_forloop_dimensionality),
  .ranges(pond2output_forloop_ranges),
  .rst_n(rst_n),
  .step(pond2output_accessor_valid),
  .mux_sel_out(pond2output_forloop_mux_sel_out),
  .restart(pond2output_forloop_restart)
);

addr_gen_6_16 pond2output_read_addr_gen (
  .clk(pond2output_read_addr_gen_clk),
  .mux_sel(pond2output_forloop_mux_sel_out),
  .restart(pond2output_forloop_restart),
  .rst_n(rst_n),
  .starting_addr(pond2output_read_addr_gen_starting_addr),
  .step(pond2output_accessor_valid),
  .strides(pond2output_read_addr_gen_strides),
  .addr_out(pond2output_read_addr_gen_addr_out)
);

sched_gen_6_16 pond2output_read_sched_gen (
  .clk(pond2output_read_sched_gen_clk),
  .cycle_count(cycle_count),
  .finished(pond2output_forloop_restart),
  .mux_sel(pond2output_forloop_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(pond2output_read_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(pond2output_read_sched_gen_sched_addr_gen_strides),
  .valid_output(pond2output_accessor_valid)
);

endmodule   // LakeTop

module LakeTop_W (
  input logic clk_mem,
  input logic [0:0] [15:0] data_in,
  input logic [3:0] input2pond_forloop_dimensionality,
  input logic [15:0] input2pond_forloop_ranges_0,
  input logic [15:0] input2pond_forloop_ranges_1,
  input logic [15:0] input2pond_forloop_ranges_2,
  input logic [15:0] input2pond_forloop_ranges_3,
  input logic [15:0] input2pond_forloop_ranges_4,
  input logic [15:0] input2pond_forloop_ranges_5,
  input logic [15:0] input2pond_write_addr_gen_starting_addr,
  input logic [15:0] input2pond_write_addr_gen_strides_0,
  input logic [15:0] input2pond_write_addr_gen_strides_1,
  input logic [15:0] input2pond_write_addr_gen_strides_2,
  input logic [15:0] input2pond_write_addr_gen_strides_3,
  input logic [15:0] input2pond_write_addr_gen_strides_4,
  input logic [15:0] input2pond_write_addr_gen_strides_5,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_starting_addr,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_strides_0,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_strides_1,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_strides_2,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_strides_3,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_strides_4,
  input logic [15:0] input2pond_write_sched_gen_sched_addr_gen_strides_5,
  input logic [3:0] pond2output_forloop_dimensionality,
  input logic [15:0] pond2output_forloop_ranges_0,
  input logic [15:0] pond2output_forloop_ranges_1,
  input logic [15:0] pond2output_forloop_ranges_2,
  input logic [15:0] pond2output_forloop_ranges_3,
  input logic [15:0] pond2output_forloop_ranges_4,
  input logic [15:0] pond2output_forloop_ranges_5,
  input logic [15:0] pond2output_read_addr_gen_starting_addr,
  input logic [15:0] pond2output_read_addr_gen_strides_0,
  input logic [15:0] pond2output_read_addr_gen_strides_1,
  input logic [15:0] pond2output_read_addr_gen_strides_2,
  input logic [15:0] pond2output_read_addr_gen_strides_3,
  input logic [15:0] pond2output_read_addr_gen_strides_4,
  input logic [15:0] pond2output_read_addr_gen_strides_5,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_starting_addr,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_strides_0,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_strides_1,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_strides_2,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_strides_3,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_strides_4,
  input logic [15:0] pond2output_read_sched_gen_sched_addr_gen_strides_5,
  input logic rst_n,
  input logic tile_en,
  output logic [0:0] [15:0] data_out
);

logic [5:0][15:0] LakeTop_input2pond_forloop_ranges;
logic [5:0][15:0] LakeTop_input2pond_write_addr_gen_strides;
logic [5:0][15:0] LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_pond2output_forloop_ranges;
logic [5:0][15:0] LakeTop_pond2output_read_addr_gen_strides;
logic [5:0][15:0] LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides;
assign LakeTop_input2pond_forloop_ranges[0] = input2pond_forloop_ranges_0;
assign LakeTop_input2pond_forloop_ranges[1] = input2pond_forloop_ranges_1;
assign LakeTop_input2pond_forloop_ranges[2] = input2pond_forloop_ranges_2;
assign LakeTop_input2pond_forloop_ranges[3] = input2pond_forloop_ranges_3;
assign LakeTop_input2pond_forloop_ranges[4] = input2pond_forloop_ranges_4;
assign LakeTop_input2pond_forloop_ranges[5] = input2pond_forloop_ranges_5;
assign LakeTop_input2pond_write_addr_gen_strides[0] = input2pond_write_addr_gen_strides_0;
assign LakeTop_input2pond_write_addr_gen_strides[1] = input2pond_write_addr_gen_strides_1;
assign LakeTop_input2pond_write_addr_gen_strides[2] = input2pond_write_addr_gen_strides_2;
assign LakeTop_input2pond_write_addr_gen_strides[3] = input2pond_write_addr_gen_strides_3;
assign LakeTop_input2pond_write_addr_gen_strides[4] = input2pond_write_addr_gen_strides_4;
assign LakeTop_input2pond_write_addr_gen_strides[5] = input2pond_write_addr_gen_strides_5;
assign LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides[0] = input2pond_write_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides[1] = input2pond_write_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides[2] = input2pond_write_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides[3] = input2pond_write_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides[4] = input2pond_write_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides[5] = input2pond_write_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_pond2output_forloop_ranges[0] = pond2output_forloop_ranges_0;
assign LakeTop_pond2output_forloop_ranges[1] = pond2output_forloop_ranges_1;
assign LakeTop_pond2output_forloop_ranges[2] = pond2output_forloop_ranges_2;
assign LakeTop_pond2output_forloop_ranges[3] = pond2output_forloop_ranges_3;
assign LakeTop_pond2output_forloop_ranges[4] = pond2output_forloop_ranges_4;
assign LakeTop_pond2output_forloop_ranges[5] = pond2output_forloop_ranges_5;
assign LakeTop_pond2output_read_addr_gen_strides[0] = pond2output_read_addr_gen_strides_0;
assign LakeTop_pond2output_read_addr_gen_strides[1] = pond2output_read_addr_gen_strides_1;
assign LakeTop_pond2output_read_addr_gen_strides[2] = pond2output_read_addr_gen_strides_2;
assign LakeTop_pond2output_read_addr_gen_strides[3] = pond2output_read_addr_gen_strides_3;
assign LakeTop_pond2output_read_addr_gen_strides[4] = pond2output_read_addr_gen_strides_4;
assign LakeTop_pond2output_read_addr_gen_strides[5] = pond2output_read_addr_gen_strides_5;
assign LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides[0] = pond2output_read_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides[1] = pond2output_read_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides[2] = pond2output_read_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides[3] = pond2output_read_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides[4] = pond2output_read_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides[5] = pond2output_read_sched_gen_sched_addr_gen_strides_5;
LakeTop LakeTop (
  .clk_mem(clk_mem),
  .data_in(data_in),
  .input2pond_forloop_dimensionality(input2pond_forloop_dimensionality),
  .input2pond_forloop_ranges(LakeTop_input2pond_forloop_ranges),
  .input2pond_write_addr_gen_starting_addr(input2pond_write_addr_gen_starting_addr),
  .input2pond_write_addr_gen_strides(LakeTop_input2pond_write_addr_gen_strides),
  .input2pond_write_sched_gen_sched_addr_gen_starting_addr(input2pond_write_sched_gen_sched_addr_gen_starting_addr),
  .input2pond_write_sched_gen_sched_addr_gen_strides(LakeTop_input2pond_write_sched_gen_sched_addr_gen_strides),
  .pond2output_forloop_dimensionality(pond2output_forloop_dimensionality),
  .pond2output_forloop_ranges(LakeTop_pond2output_forloop_ranges),
  .pond2output_read_addr_gen_starting_addr(pond2output_read_addr_gen_starting_addr),
  .pond2output_read_addr_gen_strides(LakeTop_pond2output_read_addr_gen_strides),
  .pond2output_read_sched_gen_sched_addr_gen_starting_addr(pond2output_read_sched_gen_sched_addr_gen_starting_addr),
  .pond2output_read_sched_gen_sched_addr_gen_strides(LakeTop_pond2output_read_sched_gen_sched_addr_gen_strides),
  .rst_n(rst_n),
  .tile_en(tile_en),
  .data_out(data_out)
);

endmodule   // LakeTop_W

module addr_gen_6_16 (
  input logic clk,
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
  else if (step) begin
    if (restart) begin
      current_addr <= 16'h0;
    end
    else current_addr <= current_addr + strides[mux_sel];
  end
end
endmodule   // addr_gen_6_16

module for_loop_6_16 #(
  parameter CONFIG_WIDTH = 5'h10,
  parameter ITERATOR_SUPPORT = 4'h6
)
(
  input logic clk,
  input logic [3:0] dimensionality,
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
  else if (clear[0]) begin
    dim_counter[0] <= 16'h0;
  end
  else if (inc[0]) begin
    dim_counter[0] <= inced_cnt;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[0] <= 1'h0;
  end
  else if (clear[0]) begin
    max_value[0] <= 1'h0;
  end
  else if (inc[0]) begin
    max_value[0] <= maxed_value;
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
  else if (clear[1]) begin
    dim_counter[1] <= 16'h0;
  end
  else if (inc[1]) begin
    dim_counter[1] <= inced_cnt;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[1] <= 1'h0;
  end
  else if (clear[1]) begin
    max_value[1] <= 1'h0;
  end
  else if (inc[1]) begin
    max_value[1] <= maxed_value;
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
  else if (clear[2]) begin
    dim_counter[2] <= 16'h0;
  end
  else if (inc[2]) begin
    dim_counter[2] <= inced_cnt;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[2] <= 1'h0;
  end
  else if (clear[2]) begin
    max_value[2] <= 1'h0;
  end
  else if (inc[2]) begin
    max_value[2] <= maxed_value;
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
  else if (clear[3]) begin
    dim_counter[3] <= 16'h0;
  end
  else if (inc[3]) begin
    dim_counter[3] <= inced_cnt;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[3] <= 1'h0;
  end
  else if (clear[3]) begin
    max_value[3] <= 1'h0;
  end
  else if (inc[3]) begin
    max_value[3] <= maxed_value;
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
  else if (clear[4]) begin
    dim_counter[4] <= 16'h0;
  end
  else if (inc[4]) begin
    dim_counter[4] <= inced_cnt;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[4] <= 1'h0;
  end
  else if (clear[4]) begin
    max_value[4] <= 1'h0;
  end
  else if (inc[4]) begin
    max_value[4] <= maxed_value;
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
  else if (clear[5]) begin
    dim_counter[5] <= 16'h0;
  end
  else if (inc[5]) begin
    dim_counter[5] <= inced_cnt;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    max_value[5] <= 1'h0;
  end
  else if (clear[5]) begin
    max_value[5] <= 1'h0;
  end
  else if (inc[5]) begin
    max_value[5] <= maxed_value;
  end
end
assign restart = ~done;
endmodule   // for_loop_6_16

module lake_mem (
  input logic clk,
  input logic [0:0][0:0] [15:0] data_in,
  input logic [4:0] read_addr [0:0],
  input logic rst_n,
  input logic write,
  input logic [4:0] write_addr [0:0],
  output logic [0:0][0:0] [15:0] data_out
);

logic [31:0][15:0] memory;

always_ff @(posedge clk) begin
  if (write) begin
    memory[write_addr[0] + 5'h0] <= data_in[0][0];
  end
end
always_comb begin
  data_out[0][0] = memory[read_addr[0] + 5'h0];
end
endmodule   // lake_mem

module sched_gen_6_16 (
  input logic clk,
  input logic [15:0] cycle_count,
  input logic finished,
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
  else if (finished) begin
    valid_gate_inv <= 1'h1;
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
  .mux_sel(mux_sel),
  .restart(1'h0),
  .rst_n(rst_n),
  .starting_addr(sched_addr_gen_starting_addr),
  .step(valid_out),
  .strides(sched_addr_gen_strides),
  .addr_out(addr_out)
);

endmodule   // sched_gen_6_16


