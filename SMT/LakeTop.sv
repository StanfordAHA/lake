module LakeTop (
  input logic [0:0] [15:0] addr_in,
  input logic clk,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  output logic [0:0] [15:0] config_data_out,
  input logic config_en,
  input logic config_read,
  input logic config_write,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic [3:0] input_addr_ctrl_address_gen_0_dimensionality,
  input logic [1:0] [31:0] input_addr_ctrl_address_gen_0_ranges,
  input logic [31:0] input_addr_ctrl_address_gen_0_starting_addr,
  input logic [1:0] [31:0] input_addr_ctrl_address_gen_0_strides,
  input logic [3:0] output_addr_ctrl_address_gen_0_dimensionality,
  input logic [1:0] [31:0] output_addr_ctrl_address_gen_0_ranges,
  input logic [31:0] output_addr_ctrl_address_gen_0_starting_addr,
  input logic [1:0] [31:0] output_addr_ctrl_address_gen_0_strides,
  input logic ren,
  input logic ren_en,
  input logic rst_n,
  output logic valid_out,
  input logic wen,
  input logic wen_en
);

logic ack_reduced;
logic [0:0] ack_transpose;
logic [0:0][4:0] addr_out;
logic [0:0][0:0][4:0] addr_to_arb;
logic [0:0] arb_acks;
logic [0:0][0:0][0:0][15:0] arb_dat_out;
logic [0:0][0:0][15:0] arb_dat_out_f;
logic [0:0][0:0] arb_port_out;
logic [0:0] arb_port_out_f;
logic [0:0] arb_valid_out;
logic arb_valid_out_f;
logic [0:0][0:0][0:0][15:0] data_to_arb;
logic [0:0][0:0][15:0] demux_rds_data_out;
logic [4:0] mem_addr_cfg;
logic [0:0] mem_cen_datapath;
logic [0:0] mem_cen_in;
logic [0:0][15:0] mem_data_cfg;
logic [0:0][0:0][0:0][15:0] mem_data_dp;
logic [0:0][0:0][0:0][15:0] mem_data_in;
logic [0:0][0:0][15:0] mem_data_low_pt;
logic [0:0][0:0][0:0][15:0] mem_data_out;
logic mem_ren_cfg;
logic mem_wen_cfg;
logic [0:0] mem_wen_datapath;
logic [0:0] mem_wen_in;
logic oac_step;
logic oac_valid;
logic [0:0][0:0][4:0] rd_mem_addr_dp;
logic [0:0][0:0][4:0] rd_mem_addr_in;
logic [0:0] ren_out;
logic [0:0] ren_out_tpose;
logic [0:0][15:0] rf_0_data_in;
logic [0:0][15:0] rf_0_data_out;
logic [4:0] rf_0_rd_addr;
logic rf_0_wen;
logic [4:0] rf_0_wr_addr;
logic rw_arb_0_cen_mem;
logic [0:0][0:0][15:0] rw_arb_0_data_from_mem;
logic [0:0][0:0][15:0] rw_arb_0_data_to_mem;
logic rw_arb_0_out_ack;
logic [0:0][0:0][15:0] rw_arb_0_out_data;
logic [0:0] rw_arb_0_out_port;
logic rw_arb_0_out_valid;
logic [0:0][4:0] rw_arb_0_rd_addr_to_mem;
logic rw_arb_0_ren_in;
logic [0:0][4:0] rw_arb_0_w_addr;
logic [0:0][0:0][15:0] rw_arb_0_w_data;
logic rw_arb_0_wen_in;
logic rw_arb_0_wen_mem;
logic [0:0][4:0] rw_arb_0_wr_addr_to_mem;
logic [0:0] wen_to_arb;
logic [0:0][0:0][4:0] wr_mem_addr_dp;
logic [0:0][0:0][4:0] wr_mem_addr_in;
assign oac_valid = ren;
assign oac_step = ren;
assign ren_out_tpose = ren_out;
assign mem_data_low_pt[0] = mem_data_out[0][0];
assign rw_arb_0_wen_in = wen_to_arb;
assign rw_arb_0_w_data = data_to_arb[0];
assign rw_arb_0_w_addr = addr_to_arb[0];
assign rw_arb_0_data_from_mem = mem_data_out[0];
assign arb_dat_out[0] = rw_arb_0_out_data;
assign arb_port_out = rw_arb_0_out_port;
assign arb_valid_out = rw_arb_0_out_valid;
assign mem_cen_datapath = rw_arb_0_cen_mem;
assign mem_wen_datapath = rw_arb_0_wen_mem;
assign mem_data_dp[0] = rw_arb_0_data_to_mem;
assign arb_acks = rw_arb_0_out_ack;
assign wr_mem_addr_dp[0] = rw_arb_0_wr_addr_to_mem;
assign rd_mem_addr_dp[0] = rw_arb_0_rd_addr_to_mem;
assign rw_arb_0_ren_in = ren_out;
assign mem_wen_in = (|config_en) ? mem_wen_cfg: mem_wen_datapath;
assign mem_cen_in = (|config_en) ? mem_ren_cfg: mem_cen_datapath;
assign wr_mem_addr_in[0][0] = (|config_en) ? mem_addr_cfg: wr_mem_addr_dp[0][0];
assign rd_mem_addr_in[0][0] = (|config_en) ? mem_addr_cfg: rd_mem_addr_dp[0][0];
assign mem_data_in[0][0] = (|config_en) ? mem_data_cfg: mem_data_dp[0][0];
assign rf_0_wen = mem_wen_in;
assign rf_0_wr_addr = wr_mem_addr_in[0];
assign rf_0_rd_addr = rd_mem_addr_in[0];
assign rf_0_data_in = mem_data_in[0];
assign mem_data_out[0] = rf_0_data_out;
assign arb_dat_out_f[0] = arb_dat_out[0][0];
assign arb_port_out_f = arb_port_out;
assign arb_valid_out_f = arb_valid_out;
assign data_out[0] = demux_rds_data_out[0];
always_comb begin
  ack_transpose = arb_acks;
end
always_comb begin
  ack_reduced = |ack_transpose;
end
input_addr_ctrl input_addr_ctrl (
  .addr_out(addr_to_arb),
  .address_gen_0_dimensionality(input_addr_ctrl_address_gen_0_dimensionality),
  .address_gen_0_ranges(input_addr_ctrl_address_gen_0_ranges),
  .address_gen_0_starting_addr(input_addr_ctrl_address_gen_0_starting_addr),
  .address_gen_0_strides(input_addr_ctrl_address_gen_0_strides),
  .clk(clk),
  .data_in(data_in),
  .data_out(data_to_arb),
  .rst_n(rst_n),
  .valid_in(wen),
  .wen_en(wen_en),
  .wen_to_sram(wen_to_arb)
);

output_addr_ctrl output_addr_ctrl (
  .addr_out(addr_out),
  .address_gen_0_dimensionality(output_addr_ctrl_address_gen_0_dimensionality),
  .address_gen_0_ranges(output_addr_ctrl_address_gen_0_ranges),
  .address_gen_0_starting_addr(output_addr_ctrl_address_gen_0_starting_addr),
  .address_gen_0_strides(output_addr_ctrl_address_gen_0_strides),
  .clk(clk),
  .ren(ren_out),
  .rst_n(rst_n),
  .step_in(oac_step),
  .valid_in(oac_valid)
);

rw_arbiter rw_arb_0 (
  .cen_mem(rw_arb_0_cen_mem),
  .clk(clk),
  .data_from_mem(rw_arb_0_data_from_mem),
  .data_to_mem(rw_arb_0_data_to_mem),
  .out_ack(rw_arb_0_out_ack),
  .out_data(rw_arb_0_out_data),
  .out_port(rw_arb_0_out_port),
  .out_valid(rw_arb_0_out_valid),
  .rd_addr(addr_out),
  .rd_addr_to_mem(rw_arb_0_rd_addr_to_mem),
  .ren_en(ren_en),
  .ren_in(rw_arb_0_ren_in),
  .rst_n(rst_n),
  .w_addr(rw_arb_0_w_addr),
  .w_data(rw_arb_0_w_data),
  .wen_in(rw_arb_0_wen_in),
  .wen_mem(rw_arb_0_wen_mem),
  .wr_addr_to_mem(rw_arb_0_wr_addr_to_mem)
);

storage_config_seq config_seq (
  .addr_out(mem_addr_cfg),
  .clk(clk),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_en(config_en),
  .config_rd(config_read),
  .config_wr(config_write),
  .rd_data_out(config_data_out),
  .rd_data_stg(mem_data_low_pt),
  .ren_out(mem_ren_cfg),
  .rst_n(rst_n),
  .wen_out(mem_wen_cfg),
  .wr_data(mem_data_cfg)
);

register_file rf_0 (
  .clk(clk),
  .data_in(rf_0_data_in),
  .data_out(rf_0_data_out),
  .rd_addr(rf_0_rd_addr),
  .wen(rf_0_wen),
  .wr_addr(rf_0_wr_addr)
);

demux_reads demux_rds (
  .clk(clk),
  .data_in(arb_dat_out_f),
  .data_out(demux_rds_data_out),
  .port_in(arb_port_out_f),
  .rst_n(rst_n),
  .valid_in(arb_valid_out_f),
  .valid_out(valid_out)
);

endmodule   // LakeTop

module LakeTop_W (
  input logic [0:0] [15:0] addr_in,
  input logic clk,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  output logic [0:0] [15:0] config_data_out,
  input logic config_en,
  input logic config_read,
  input logic config_write,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic [3:0] input_addr_ctrl_address_gen_0_dimensionality,
  input logic [31:0] input_addr_ctrl_address_gen_0_ranges_0,
  input logic [31:0] input_addr_ctrl_address_gen_0_ranges_1,
  input logic [31:0] input_addr_ctrl_address_gen_0_starting_addr,
  input logic [31:0] input_addr_ctrl_address_gen_0_strides_0,
  input logic [31:0] input_addr_ctrl_address_gen_0_strides_1,
  input logic [3:0] output_addr_ctrl_address_gen_0_dimensionality,
  input logic [31:0] output_addr_ctrl_address_gen_0_ranges_0,
  input logic [31:0] output_addr_ctrl_address_gen_0_ranges_1,
  input logic [31:0] output_addr_ctrl_address_gen_0_starting_addr,
  input logic [31:0] output_addr_ctrl_address_gen_0_strides_0,
  input logic [31:0] output_addr_ctrl_address_gen_0_strides_1,
  input logic ren,
  input logic ren_en,
  input logic rst_n,
  output logic valid_out,
  input logic wen,
  input logic wen_en
);

logic [1:0][31:0] LakeTop_input_addr_ctrl_address_gen_0_ranges;
logic [1:0][31:0] LakeTop_input_addr_ctrl_address_gen_0_strides;
logic [1:0][31:0] LakeTop_output_addr_ctrl_address_gen_0_ranges;
logic [1:0][31:0] LakeTop_output_addr_ctrl_address_gen_0_strides;
assign LakeTop_input_addr_ctrl_address_gen_0_ranges[0] = input_addr_ctrl_address_gen_0_ranges_0;
assign LakeTop_input_addr_ctrl_address_gen_0_ranges[1] = input_addr_ctrl_address_gen_0_ranges_1;
assign LakeTop_input_addr_ctrl_address_gen_0_strides[0] = input_addr_ctrl_address_gen_0_strides_0;
assign LakeTop_input_addr_ctrl_address_gen_0_strides[1] = input_addr_ctrl_address_gen_0_strides_1;
assign LakeTop_output_addr_ctrl_address_gen_0_ranges[0] = output_addr_ctrl_address_gen_0_ranges_0;
assign LakeTop_output_addr_ctrl_address_gen_0_ranges[1] = output_addr_ctrl_address_gen_0_ranges_1;
assign LakeTop_output_addr_ctrl_address_gen_0_strides[0] = output_addr_ctrl_address_gen_0_strides_0;
assign LakeTop_output_addr_ctrl_address_gen_0_strides[1] = output_addr_ctrl_address_gen_0_strides_1;
LakeTop LakeTop (
  .addr_in(addr_in),
  .clk(clk),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_data_out(config_data_out),
  .config_en(config_en),
  .config_read(config_read),
  .config_write(config_write),
  .data_in(data_in),
  .data_out(data_out),
  .input_addr_ctrl_address_gen_0_dimensionality(input_addr_ctrl_address_gen_0_dimensionality),
  .input_addr_ctrl_address_gen_0_ranges(LakeTop_input_addr_ctrl_address_gen_0_ranges),
  .input_addr_ctrl_address_gen_0_starting_addr(input_addr_ctrl_address_gen_0_starting_addr),
  .input_addr_ctrl_address_gen_0_strides(LakeTop_input_addr_ctrl_address_gen_0_strides),
  .output_addr_ctrl_address_gen_0_dimensionality(output_addr_ctrl_address_gen_0_dimensionality),
  .output_addr_ctrl_address_gen_0_ranges(LakeTop_output_addr_ctrl_address_gen_0_ranges),
  .output_addr_ctrl_address_gen_0_starting_addr(output_addr_ctrl_address_gen_0_starting_addr),
  .output_addr_ctrl_address_gen_0_strides(LakeTop_output_addr_ctrl_address_gen_0_strides),
  .ren(ren),
  .ren_en(ren_en),
  .rst_n(rst_n),
  .valid_out(valid_out),
  .wen(wen),
  .wen_en(wen_en)
);

endmodule   // LakeTop_W

module addr_gen_2 (
  output logic [31:0] addr_out,
  input logic clk,
  input logic clk_en,
  input logic [3:0] dimensionality,
  input logic flush,
  input logic [1:0] [31:0] ranges,
  input logic rst_n,
  input logic [31:0] starting_addr,
  input logic step,
  input logic [1:0] [31:0] strides
);

logic [31:0] calc_addr;
logic [1:0][31:0] current_loc;
logic [1:0][31:0] dim_counter;
logic [31:0] strt_addr;
logic [1:0] update;
assign strt_addr = starting_addr;
assign addr_out = calc_addr;
assign update[0] = 1'h1;
assign update[1] = (dim_counter[0] == (ranges[0] - 32'h1)) & update[0];
always_comb begin
  calc_addr = ((4'h0 < dimensionality) ? current_loc[0]: 32'h0) + ((4'h1 < dimensionality) ?
      current_loc[1]: 32'h0) + strt_addr;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter <= 64'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[0] <= 32'h0;
      dim_counter[1] <= 32'h0;
    end
    else if (step) begin
      if (update[0] & (dimensionality > 4'h0)) begin
        if (dim_counter[0] == (ranges[0] - 32'h1)) begin
          dim_counter[0] <= 32'h0;
        end
        else dim_counter[0] <= dim_counter[0] + 32'h1;
      end
      if (update[1] & (dimensionality > 4'h1)) begin
        if (dim_counter[1] == (ranges[1] - 32'h1)) begin
          dim_counter[1] <= 32'h0;
        end
        else dim_counter[1] <= dim_counter[1] + 32'h1;
      end
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    current_loc <= 64'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      current_loc[0] <= 32'h0;
      current_loc[1] <= 32'h0;
    end
    else if (step) begin
      if (update[0] & (dimensionality > 4'h0)) begin
        if (dim_counter[0] == (ranges[0] - 32'h1)) begin
          current_loc[0] <= 32'h0;
        end
        else current_loc[0] <= current_loc[0] + strides[0];
      end
      if (update[1] & (dimensionality > 4'h1)) begin
        if (dim_counter[1] == (ranges[1] - 32'h1)) begin
          current_loc[1] <= 32'h0;
        end
        else current_loc[1] <= current_loc[1] + strides[1];
      end
    end
  end
end
endmodule   // addr_gen_2

module demux_reads (
  input logic clk,
  input logic [0:0][0:0] [15:0] data_in,
  output logic [0:0][0:0] [15:0] data_out,
  input logic [0:0] port_in,
  input logic rst_n,
  input logic valid_in,
  output logic valid_out
);

logic done;
always_comb begin
  valid_out = 1'h0;
  data_out[0] = 16'h0;
  done = 1'h0;
  if (~done) begin
    if (valid_in & port_in) begin
      valid_out = 1'h1;
      data_out[0] = data_in[0];
      done = 1'h1;
    end
  end
end
endmodule   // demux_reads

module input_addr_ctrl (
  output logic [0:0][0:0] [4:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [1:0] [31:0] address_gen_0_ranges,
  input logic [31:0] address_gen_0_starting_addr,
  input logic [1:0] [31:0] address_gen_0_strides,
  input logic clk,
  input logic [0:0][0:0] [15:0] data_in,
  output logic [0:0][0:0][0:0] [15:0] data_out,
  input logic rst_n,
  input logic valid_in,
  input logic wen_en,
  output logic [0:0] wen_to_sram
);

logic [31:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [0:0] done;
logic [0:0][0:0][4:0] local_addrs;
logic valid_gate;
logic [0:0][0:0] wen_full;
logic [0:0] wen_reduced;
assign valid_gate = valid_in & wen_en;
assign wen_reduced = |wen_full;
assign wen_full = valid_gate;
always_comb begin
  wen_to_sram = 1'h0;
  done = 1'h0;
  data_out[0][0] = 16'h0;
  addr_out[0][0] = 5'h0;
  if (~done) begin
    if (wen_reduced) begin
      done = 1'h1;
      wen_to_sram = 1'h1;
      data_out[0][0] = data_in[0];
      addr_out[0][0] = local_addrs[0][0][4:0];
    end
  end
end
assign address_gen_0_step = valid_gate;
always_comb begin
  local_addrs[0][0] = address_gen_0_addr_out[4:0];
end
addr_gen_2 address_gen_0 (
  .addr_out(address_gen_0_addr_out),
  .clk(clk),
  .clk_en(1'h1),
  .dimensionality(address_gen_0_dimensionality),
  .flush(1'h0),
  .ranges(address_gen_0_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_0_starting_addr),
  .step(address_gen_0_step),
  .strides(address_gen_0_strides)
);

endmodule   // input_addr_ctrl

module output_addr_ctrl (
  output logic [0:0] [4:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [1:0] [31:0] address_gen_0_ranges,
  input logic [31:0] address_gen_0_starting_addr,
  input logic [1:0] [31:0] address_gen_0_strides,
  input logic clk,
  output logic [0:0] ren,
  input logic rst_n,
  input logic step_in,
  input logic valid_in
);

logic [31:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [0:0][4:0] local_addrs;
assign ren = valid_in;
always_comb begin
  addr_out = 5'h0;
  addr_out[0] = local_addrs[0][4:0];
end
assign address_gen_0_step = step_in & valid_in;
assign local_addrs[0] = address_gen_0_addr_out[4:0];
addr_gen_2 address_gen_0 (
  .addr_out(address_gen_0_addr_out),
  .clk(clk),
  .clk_en(1'h1),
  .dimensionality(address_gen_0_dimensionality),
  .flush(1'h0),
  .ranges(address_gen_0_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_0_starting_addr),
  .step(address_gen_0_step),
  .strides(address_gen_0_strides)
);

endmodule   // output_addr_ctrl

module register_file (
  input logic clk,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic [4:0] rd_addr,
  input logic wen,
  input logic [4:0] wr_addr
);

logic [31:0][0:0][15:0] data_array;

always_ff @(posedge clk) begin
  if (wen) begin
    data_array[wr_addr] <= data_in;
  end
end
always_comb begin
  data_out = data_array[rd_addr];
end
endmodule   // register_file

module rw_arbiter (
  output logic cen_mem,
  input logic clk,
  input logic [0:0][0:0] [15:0] data_from_mem,
  output logic [0:0][0:0] [15:0] data_to_mem,
  output logic out_ack,
  output logic [0:0][0:0] [15:0] out_data,
  output logic [0:0] out_port,
  output logic out_valid,
  input logic [0:0] [4:0] rd_addr,
  output logic [0:0] [4:0] rd_addr_to_mem,
  input logic ren_en,
  input logic ren_in,
  input logic rst_n,
  input logic [0:0] [4:0] w_addr,
  input logic [0:0][0:0] [15:0] w_data,
  input logic wen_in,
  output logic wen_mem,
  output logic [0:0] [4:0] wr_addr_to_mem
);

logic done;
logic [0:0] next_rd_port;
logic next_rd_port_red;
logic [0:0][4:0] rd_addr_sel;
logic [0:0] rd_port;
logic rd_valid;
logic ren_int;
logic wen_int;
assign ren_int = ren_in & ren_en;
assign wen_int = wen_in;
always_comb begin
  next_rd_port = 1'h0;
  rd_addr_sel[0] = 5'h0;
  done = 1'h0;
  if (~done) begin
    if (ren_int) begin
      rd_addr_sel[0] = rd_addr[0];
      next_rd_port = 1'h1;
      done = 1'h1;
    end
  end
end
assign next_rd_port_red = |next_rd_port;
assign out_ack = next_rd_port_red;
always_comb begin
  data_to_mem[0] = w_data[0];
  wr_addr_to_mem[0] = w_addr[0];
  wen_mem = wen_int;
end
always_comb begin
  cen_mem = |next_rd_port;
  rd_addr_to_mem[0] = rd_addr_sel[0];
end
always_comb begin
  rd_valid = ((~wen_int) | 1'h1) & (|next_rd_port);
  rd_port = next_rd_port;
end
always_comb begin
  out_data = data_from_mem;
  out_port = rd_port;
  out_valid = rd_valid;
end
endmodule   // rw_arbiter

module storage_config_seq (
  output logic [4:0] addr_out,
  input logic clk,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  input logic config_en,
  input logic config_rd,
  input logic config_wr,
  output logic [0:0] [15:0] rd_data_out,
  input logic [0:0][0:0] [15:0] rd_data_stg,
  output logic ren_out,
  input logic rst_n,
  output logic wen_out,
  output logic [0:0] [15:0] wr_data
);

assign addr_out = config_addr_in[4:0];
assign wr_data[0] = config_data_in;
assign rd_data_out[0] = rd_data_stg[0];
assign wen_out = config_wr;
assign ren_out = config_rd;
endmodule   // storage_config_seq


