module LakeTop (
  input logic [0:0] [15:0] addr_in,
  input logic [10:0] agg_align_0_line_length,
  input logic [5:0] agg_in_0_in_period,
  input logic [63:0] [2:0] agg_in_0_in_sched,
  input logic [5:0] agg_in_0_out_period,
  input logic [63:0] [2:0] agg_in_0_out_sched,
  input logic clk,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  output logic [3:0] [15:0] config_data_out,
  input logic [3:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic [3:0] input_addr_ctrl_address_gen_0_dimensionality,
  input logic [5:0] [31:0] input_addr_ctrl_address_gen_0_ranges,
  input logic [31:0] input_addr_ctrl_address_gen_0_starting_addr,
  input logic [5:0] [31:0] input_addr_ctrl_address_gen_0_strides,
  input logic [3:0] output_addr_ctrl_address_gen_0_dimensionality,
  input logic [5:0] [31:0] output_addr_ctrl_address_gen_0_ranges,
  input logic [31:0] output_addr_ctrl_address_gen_0_starting_addr,
  input logic [5:0] [31:0] output_addr_ctrl_address_gen_0_strides,
  input logic output_en,
  input logic [5:0] pre_fetch_0_input_latency,
  input logic ren,
  input logic ren_en,
  input logic rst_n,
  input logic [0:0] sync_grp_sync_group,
  input logic [1:0] tba_0_tb_0_dimensionality,
  input logic [2047:0] tba_0_tb_0_indices,
  input logic [10:0] tba_0_tb_0_range_inner,
  input logic [10:0] tba_0_tb_0_range_outer,
  input logic [3:0] tba_0_tb_0_stride,
  input logic tba_0_tb_0_tb_height,
  output logic valid_out,
  input logic wen,
  input logic wen_en
);

logic [0:0][15:0] ab_to_mem_dat;
logic ab_to_mem_valid;
logic ack_reduced;
logic [0:0][1:0] ack_transpose;
logic [0:0][8:0] addr_out;
logic [1:0][0:0][8:0] addr_to_arb;
logic agg_align_0_align;
logic [15:0] agg_align_0_in_dat;
logic agg_align_0_in_valid;
logic [15:0] agg_align_0_out_dat;
logic agg_align_0_out_valid;
logic agg_in_0_align;
logic [15:0] agg_in_0_data_in;
logic [15:0] agg_in_0_data_out;
logic agg_in_0_valid_in;
logic agg_in_0_valid_out;
logic align_input;
logic [1:0] arb_acks;
logic [1:0][0:0][0:0][15:0] arb_dat_out;
logic [1:0][0:0][15:0] arb_dat_out_f;
logic [1:0][0:0] arb_port_out;
logic [1:0] arb_port_out_f;
logic [1:0] arb_valid_out;
logic [1:0] arb_valid_out_f;
logic [0:0][15:0] data_consume;
logic [1:0][0:0][0:0][15:0] data_to_arb;
logic [0:0][0:0][15:0] data_to_pref;
logic [0:0][0:0][15:0] data_to_sync;
logic [0:0][0:0][15:0] data_to_tba;
logic [8:0] mem_0_addr;
logic mem_0_cen;
logic [0:0][15:0] mem_0_data_in;
logic [0:0][15:0] mem_0_data_out;
logic mem_0_wen;
logic [8:0] mem_1_addr;
logic mem_1_cen;
logic [0:0][15:0] mem_1_data_in;
logic [0:0][15:0] mem_1_data_out;
logic mem_1_wen;
logic [8:0] mem_addr_cfg;
logic [1:0][0:0][8:0] mem_addr_dp;
logic [1:0][0:0][8:0] mem_addr_in;
logic [1:0] mem_cen_datapath;
logic [1:0] mem_cen_in;
logic [0:0][15:0] mem_data_cfg;
logic [1:0][0:0][0:0][15:0] mem_data_dp;
logic [1:0][0:0][0:0][15:0] mem_data_in;
logic [1:0][0:0][15:0] mem_data_low_pt;
logic [1:0][0:0][0:0][15:0] mem_data_out;
logic [1:0] mem_ren_cfg;
logic [1:0] mem_wen_cfg;
logic [1:0] mem_wen_datapath;
logic [1:0] mem_wen_in;
logic oac_step;
logic oac_valid;
logic [0:0][15:0] pre_fetch_0_data_in;
logic [0:0][15:0] pre_fetch_0_data_out;
logic pre_fetch_0_prefetch_step;
logic pre_fetch_0_tba_rdy_in;
logic pre_fetch_0_valid_out;
logic pre_fetch_0_valid_read;
logic prefetch_step;
logic rd_sync_gate;
logic ready_tba;
logic [1:0] ren_out;
logic ren_out_reduced;
logic [0:0][1:0] ren_out_tpose;
logic [0:0][8:0] rw_arb_0_addr_to_mem;
logic rw_arb_0_cen_mem;
logic [0:0][0:0][15:0] rw_arb_0_data_from_mem;
logic [0:0][0:0][15:0] rw_arb_0_data_to_mem;
logic rw_arb_0_out_ack;
logic [0:0][0:0][15:0] rw_arb_0_out_data;
logic [0:0] rw_arb_0_out_port;
logic rw_arb_0_out_valid;
logic rw_arb_0_ren_in;
logic [0:0][8:0] rw_arb_0_w_addr;
logic [0:0][0:0][15:0] rw_arb_0_w_data;
logic rw_arb_0_wen_in;
logic rw_arb_0_wen_mem;
logic [0:0][8:0] rw_arb_1_addr_to_mem;
logic rw_arb_1_cen_mem;
logic [0:0][0:0][15:0] rw_arb_1_data_from_mem;
logic [0:0][0:0][15:0] rw_arb_1_data_to_mem;
logic rw_arb_1_out_ack;
logic [0:0][0:0][15:0] rw_arb_1_out_data;
logic [0:0] rw_arb_1_out_port;
logic rw_arb_1_out_valid;
logic rw_arb_1_ren_in;
logic [0:0][8:0] rw_arb_1_w_addr;
logic [0:0][0:0][15:0] rw_arb_1_w_data;
logic rw_arb_1_wen_in;
logic rw_arb_1_wen_mem;
logic [15:0] tba_0_SRAM_to_tb_data;
logic tba_0_ack_in;
logic tba_0_tb_arbiter_rdy;
logic [15:0] tba_0_tb_to_interconnect_data;
logic tba_0_tb_to_interconnect_valid;
logic tba_0_valid_data;
logic valid_consume;
logic valid_to_pref;
logic valid_to_sync;
logic valid_to_tba;
logic [1:0] wen_to_arb;
assign agg_align_0_in_dat = data_in[0];
assign agg_align_0_in_valid = wen;
assign align_input = agg_align_0_align;
assign valid_consume = agg_align_0_out_valid;
assign data_consume[0] = agg_align_0_out_dat;
assign agg_in_0_data_in = data_consume[0];
assign agg_in_0_valid_in = valid_consume;
assign agg_in_0_align = align_input;
assign ab_to_mem_dat[0] = agg_in_0_data_out;
assign ab_to_mem_valid = agg_in_0_valid_out;
assign oac_valid = prefetch_step;
assign oac_step = ack_reduced;
assign ren_out_tpose[0][0] = ren_out[0];
assign ren_out_tpose[0][1] = ren_out[1];
assign mem_data_low_pt[0] = mem_data_out[0][0];
assign mem_data_low_pt[1] = mem_data_out[1][0];
assign rw_arb_0_wen_in = wen_to_arb[0];
assign rw_arb_0_w_data = data_to_arb[0];
assign rw_arb_0_w_addr = addr_to_arb[0];
assign rw_arb_0_data_from_mem = mem_data_out[0];
assign arb_dat_out[0] = rw_arb_0_out_data;
assign arb_port_out[0] = rw_arb_0_out_port;
assign arb_valid_out[0] = rw_arb_0_out_valid;
assign mem_cen_datapath[0] = rw_arb_0_cen_mem;
assign mem_wen_datapath[0] = rw_arb_0_wen_mem;
assign mem_data_dp[0] = rw_arb_0_data_to_mem;
assign arb_acks[0] = rw_arb_0_out_ack;
assign mem_addr_dp[0] = rw_arb_0_addr_to_mem;
assign rw_arb_0_ren_in = ren_out[0] & rd_sync_gate;
assign rw_arb_1_wen_in = wen_to_arb[1];
assign rw_arb_1_w_data = data_to_arb[1];
assign rw_arb_1_w_addr = addr_to_arb[1];
assign rw_arb_1_data_from_mem = mem_data_out[1];
assign arb_dat_out[1] = rw_arb_1_out_data;
assign arb_port_out[1] = rw_arb_1_out_port;
assign arb_valid_out[1] = rw_arb_1_out_valid;
assign mem_cen_datapath[1] = rw_arb_1_cen_mem;
assign mem_wen_datapath[1] = rw_arb_1_wen_mem;
assign mem_data_dp[1] = rw_arb_1_data_to_mem;
assign arb_acks[1] = rw_arb_1_out_ack;
assign mem_addr_dp[1] = rw_arb_1_addr_to_mem;
assign rw_arb_1_ren_in = ren_out[1] & rd_sync_gate;
assign mem_wen_in[0] = (|config_en) ? mem_wen_cfg[0]: mem_wen_datapath[0];
assign mem_cen_in[0] = (|config_en) ? mem_wen_cfg[0] | mem_ren_cfg[0]: mem_cen_datapath[0];
assign mem_addr_in[0][0] = (|config_en) ? mem_addr_cfg: mem_addr_dp[0][0];
assign mem_data_in[0][0] = (|config_en) ? mem_data_cfg: mem_data_dp[0][0];
assign mem_wen_in[1] = (|config_en) ? mem_wen_cfg[1]: mem_wen_datapath[1];
assign mem_cen_in[1] = (|config_en) ? mem_wen_cfg[1] | mem_ren_cfg[1]: mem_cen_datapath[1];
assign mem_addr_in[1][0] = (|config_en) ? mem_addr_cfg: mem_addr_dp[1][0];
assign mem_data_in[1][0] = (|config_en) ? mem_data_cfg: mem_data_dp[1][0];
assign mem_0_data_in = mem_data_in[0];
assign mem_0_addr = mem_addr_in[0];
assign mem_0_cen = mem_cen_in[0];
assign mem_0_wen = mem_wen_in[0];
assign mem_data_out[0] = mem_0_data_out;
assign mem_1_data_in = mem_data_in[1];
assign mem_1_addr = mem_addr_in[1];
assign mem_1_cen = mem_cen_in[1];
assign mem_1_wen = mem_wen_in[1];
assign mem_data_out[1] = mem_1_data_out;
assign arb_dat_out_f[0] = arb_dat_out[0][0];
assign arb_port_out_f[0] = arb_port_out[0];
assign arb_valid_out_f[0] = arb_valid_out[0];
assign arb_dat_out_f[1] = arb_dat_out[1][0];
assign arb_port_out_f[1] = arb_port_out[1];
assign arb_valid_out_f[1] = arb_valid_out[1];
assign ren_out_reduced = |ren_out_tpose[0];
assign pre_fetch_0_data_in = data_to_pref[0];
assign pre_fetch_0_valid_read = valid_to_pref;
assign pre_fetch_0_tba_rdy_in = ready_tba;
assign data_to_tba[0] = pre_fetch_0_data_out;
assign valid_to_tba = pre_fetch_0_valid_out;
assign prefetch_step = pre_fetch_0_prefetch_step;
assign tba_0_SRAM_to_tb_data = data_to_tba[0];
assign tba_0_valid_data = valid_to_tba;
assign tba_0_ack_in = valid_to_tba;
assign data_out[0] = tba_0_tb_to_interconnect_data;
assign valid_out = tba_0_tb_to_interconnect_valid;
assign ready_tba = tba_0_tb_arbiter_rdy;
always_comb begin
  ack_transpose[0][0] = arb_acks[0];
  ack_transpose[0][1] = arb_acks[1];
end
always_comb begin
  ack_reduced = |ack_transpose[0];
end
agg_aligner agg_align_0 (
  .align(agg_align_0_align),
  .clk(clk),
  .in_dat(agg_align_0_in_dat),
  .in_valid(agg_align_0_in_valid),
  .line_length(agg_align_0_line_length),
  .out_dat(agg_align_0_out_dat),
  .out_valid(agg_align_0_out_valid),
  .rst_n(rst_n)
);

aggregation_buffer agg_in_0 (
  .align(agg_in_0_align),
  .clk(clk),
  .data_in(agg_in_0_data_in),
  .data_out(agg_in_0_data_out),
  .in_period(agg_in_0_in_period),
  .in_sched(agg_in_0_in_sched),
  .out_period(agg_in_0_out_period),
  .out_sched(agg_in_0_out_sched),
  .rst_n(rst_n),
  .valid_in(agg_in_0_valid_in),
  .valid_out(agg_in_0_valid_out),
  .write_act(1'h1)
);

input_addr_ctrl input_addr_ctrl (
  .addr_out(addr_to_arb),
  .address_gen_0_dimensionality(input_addr_ctrl_address_gen_0_dimensionality),
  .address_gen_0_ranges(input_addr_ctrl_address_gen_0_ranges),
  .address_gen_0_starting_addr(input_addr_ctrl_address_gen_0_starting_addr),
  .address_gen_0_strides(input_addr_ctrl_address_gen_0_strides),
  .clk(clk),
  .data_in(ab_to_mem_dat),
  .data_out(data_to_arb),
  .rst_n(rst_n),
  .valid_in(ab_to_mem_valid),
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
  .addr_to_mem(rw_arb_0_addr_to_mem),
  .cen_mem(rw_arb_0_cen_mem),
  .clk(clk),
  .data_from_mem(rw_arb_0_data_from_mem),
  .data_to_mem(rw_arb_0_data_to_mem),
  .out_ack(rw_arb_0_out_ack),
  .out_data(rw_arb_0_out_data),
  .out_port(rw_arb_0_out_port),
  .out_valid(rw_arb_0_out_valid),
  .rd_addr(addr_out),
  .ren_en(ren_en),
  .ren_in(rw_arb_0_ren_in),
  .rst_n(rst_n),
  .w_addr(rw_arb_0_w_addr),
  .w_data(rw_arb_0_w_data),
  .wen_in(rw_arb_0_wen_in),
  .wen_mem(rw_arb_0_wen_mem)
);

rw_arbiter rw_arb_1 (
  .addr_to_mem(rw_arb_1_addr_to_mem),
  .cen_mem(rw_arb_1_cen_mem),
  .clk(clk),
  .data_from_mem(rw_arb_1_data_from_mem),
  .data_to_mem(rw_arb_1_data_to_mem),
  .out_ack(rw_arb_1_out_ack),
  .out_data(rw_arb_1_out_data),
  .out_port(rw_arb_1_out_port),
  .out_valid(rw_arb_1_out_valid),
  .rd_addr(addr_out),
  .ren_en(ren_en),
  .ren_in(rw_arb_1_ren_in),
  .rst_n(rst_n),
  .w_addr(rw_arb_1_w_addr),
  .w_data(rw_arb_1_w_data),
  .wen_in(rw_arb_1_wen_in),
  .wen_mem(rw_arb_1_wen_mem)
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

sram_stub mem_0 (
  .addr(mem_0_addr),
  .cen(mem_0_cen),
  .clk(clk),
  .data_in(mem_0_data_in),
  .data_out(mem_0_data_out),
  .wen(mem_0_wen)
);

sram_stub mem_1 (
  .addr(mem_1_addr),
  .cen(mem_1_cen),
  .clk(clk),
  .data_in(mem_1_data_in),
  .data_out(mem_1_data_out),
  .wen(mem_1_wen)
);

demux_reads demux_rds (
  .clk(clk),
  .data_in(arb_dat_out_f),
  .data_out(data_to_sync),
  .port_in(arb_port_out_f),
  .rst_n(rst_n),
  .valid_in(arb_valid_out_f),
  .valid_out(valid_to_sync)
);

sync_groups sync_grp (
  .ack_in(ack_reduced),
  .clk(clk),
  .data_in(data_to_sync),
  .data_out(data_to_pref),
  .rd_sync_gate(rd_sync_gate),
  .ren_in(ren_out_reduced),
  .rst_n(rst_n),
  .sync_group(sync_grp_sync_group),
  .valid_in(valid_to_sync),
  .valid_out(valid_to_pref)
);

prefetcher pre_fetch_0 (
  .clk(clk),
  .data_in(pre_fetch_0_data_in),
  .data_out(pre_fetch_0_data_out),
  .input_latency(pre_fetch_0_input_latency),
  .prefetch_step(pre_fetch_0_prefetch_step),
  .rst_n(rst_n),
  .tba_rdy_in(pre_fetch_0_tba_rdy_in),
  .valid_out(pre_fetch_0_valid_out),
  .valid_read(pre_fetch_0_valid_read)
);

transpose_buffer_aggregation tba_0 (
  .SRAM_to_tb_data(tba_0_SRAM_to_tb_data),
  .ack_in(tba_0_ack_in),
  .clk(clk),
  .rst_n(rst_n),
  .tb_0_dimensionality(tba_0_tb_0_dimensionality),
  .tb_0_indices(tba_0_tb_0_indices),
  .tb_0_range_inner(tba_0_tb_0_range_inner),
  .tb_0_range_outer(tba_0_tb_0_range_outer),
  .tb_0_stride(tba_0_tb_0_stride),
  .tb_0_tb_height(tba_0_tb_0_tb_height),
  .tb_arbiter_rdy(tba_0_tb_arbiter_rdy),
  .tb_index_for_data(1'h0),
  .tb_to_interconnect_data(tba_0_tb_to_interconnect_data),
  .tb_to_interconnect_valid(tba_0_tb_to_interconnect_valid),
  .tba_ren(output_en),
  .valid_data(tba_0_valid_data)
);

endmodule   // LakeTop

module addr_gen_6 (
  output logic [31:0] addr_out,
  input logic clk,
  input logic clk_en,
  input logic [3:0] dimensionality,
  input logic flush,
  input logic [5:0] [31:0] ranges,
  input logic rst_n,
  input logic [31:0] starting_addr,
  input logic step,
  input logic [5:0] [31:0] strides
);

logic [31:0] calc_addr;
logic [5:0][31:0] current_loc;
logic [5:0][31:0] dim_counter;
logic [31:0] strt_addr;
logic [5:0] update;
assign strt_addr = starting_addr;
assign addr_out = calc_addr;
assign update[0] = 1'h1;
assign update[1] = (dim_counter[0] == (ranges[0] - 32'h1)) & update[0];
assign update[2] = (dim_counter[1] == (ranges[1] - 32'h1)) & update[1];
assign update[3] = (dim_counter[2] == (ranges[2] - 32'h1)) & update[2];
assign update[4] = (dim_counter[3] == (ranges[3] - 32'h1)) & update[3];
assign update[5] = (dim_counter[4] == (ranges[4] - 32'h1)) & update[4];
always_comb begin
  calc_addr = ((4'h0 < dimensionality) ? current_loc[0]: 32'h0) + ((4'h1 < dimensionality) ?
      current_loc[1]: 32'h0) + ((4'h2 < dimensionality) ? current_loc[2]: 32'h0) +
      ((4'h3 < dimensionality) ? current_loc[3]: 32'h0) + ((4'h4 < dimensionality) ?
      current_loc[4]: 32'h0) + ((4'h5 < dimensionality) ? current_loc[5]: 32'h0) +
      strt_addr;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter <= 192'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[0] <= 32'h0;
      dim_counter[1] <= 32'h0;
      dim_counter[2] <= 32'h0;
      dim_counter[3] <= 32'h0;
      dim_counter[4] <= 32'h0;
      dim_counter[5] <= 32'h0;
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
      if (update[2] & (dimensionality > 4'h2)) begin
        if (dim_counter[2] == (ranges[2] - 32'h1)) begin
          dim_counter[2] <= 32'h0;
        end
        else dim_counter[2] <= dim_counter[2] + 32'h1;
      end
      if (update[3] & (dimensionality > 4'h3)) begin
        if (dim_counter[3] == (ranges[3] - 32'h1)) begin
          dim_counter[3] <= 32'h0;
        end
        else dim_counter[3] <= dim_counter[3] + 32'h1;
      end
      if (update[4] & (dimensionality > 4'h4)) begin
        if (dim_counter[4] == (ranges[4] - 32'h1)) begin
          dim_counter[4] <= 32'h0;
        end
        else dim_counter[4] <= dim_counter[4] + 32'h1;
      end
      if (update[5] & (dimensionality > 4'h5)) begin
        if (dim_counter[5] == (ranges[5] - 32'h1)) begin
          dim_counter[5] <= 32'h0;
        end
        else dim_counter[5] <= dim_counter[5] + 32'h1;
      end
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    current_loc <= 192'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      current_loc[0] <= 32'h0;
      current_loc[1] <= 32'h0;
      current_loc[2] <= 32'h0;
      current_loc[3] <= 32'h0;
      current_loc[4] <= 32'h0;
      current_loc[5] <= 32'h0;
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
      if (update[2] & (dimensionality > 4'h2)) begin
        if (dim_counter[2] == (ranges[2] - 32'h1)) begin
          current_loc[2] <= 32'h0;
        end
        else current_loc[2] <= current_loc[2] + strides[2];
      end
      if (update[3] & (dimensionality > 4'h3)) begin
        if (dim_counter[3] == (ranges[3] - 32'h1)) begin
          current_loc[3] <= 32'h0;
        end
        else current_loc[3] <= current_loc[3] + strides[3];
      end
      if (update[4] & (dimensionality > 4'h4)) begin
        if (dim_counter[4] == (ranges[4] - 32'h1)) begin
          current_loc[4] <= 32'h0;
        end
        else current_loc[4] <= current_loc[4] + strides[4];
      end
      if (update[5] & (dimensionality > 4'h5)) begin
        if (dim_counter[5] == (ranges[5] - 32'h1)) begin
          current_loc[5] <= 32'h0;
        end
        else current_loc[5] <= current_loc[5] + strides[5];
      end
    end
  end
end
endmodule   // addr_gen_6

module agg_aligner (
  output logic align,
  input logic clk,
  input logic [15:0] in_dat,
  input logic in_valid,
  input logic [10:0] line_length,
  output logic [15:0] out_dat,
  output logic out_valid,
  input logic rst_n
);

logic [10:0] cnt;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    cnt <= 11'h0;
  end
  else if (in_valid) begin
    if ((line_length - 11'h1) == cnt) begin
      cnt <= 11'h0;
    end
    else cnt <= cnt + 11'h1;
  end
end
always_comb begin
  align = in_valid & ((line_length - 11'h1) == cnt);
end
assign out_dat = in_dat;
assign out_valid = in_valid;
endmodule   // agg_aligner

module aggregation_buffer (
  input logic align,
  input logic clk,
  input logic [15:0] data_in,
  output logic [15:0] data_out,
  output logic [15:0] data_out_chop_0,
  input logic [5:0] in_period,
  input logic [63:0] [2:0] in_sched,
  input logic [5:0] out_period,
  input logic [63:0] [2:0] out_sched,
  input logic rst_n,
  input logic valid_in,
  output logic valid_out,
  input logic write_act
);

logic agg_0_next_full;
logic agg_0_valid_in;
logic agg_0_valid_out;
logic agg_1_next_full;
logic agg_1_valid_in;
logic agg_1_valid_out;
logic agg_2_next_full;
logic agg_2_valid_in;
logic agg_2_valid_out;
logic agg_3_next_full;
logic agg_3_valid_in;
logic agg_3_valid_out;
logic agg_4_next_full;
logic agg_4_valid_in;
logic agg_4_valid_out;
logic agg_5_next_full;
logic agg_5_valid_in;
logic agg_5_valid_out;
logic agg_6_next_full;
logic agg_6_valid_in;
logic agg_6_valid_out;
logic agg_7_next_full;
logic agg_7_valid_in;
logic agg_7_valid_out;
logic [7:0][15:0] aggs_out;
logic [15:0] aggs_sep_0;
logic [15:0] aggs_sep_1;
logic [15:0] aggs_sep_2;
logic [15:0] aggs_sep_3;
logic [15:0] aggs_sep_4;
logic [15:0] aggs_sep_5;
logic [15:0] aggs_sep_6;
logic [15:0] aggs_sep_7;
logic [5:0] in_sched_ptr;
logic [7:0] next_full;
logic [5:0] out_sched_ptr;
logic [7:0] valid_demux;
logic [7:0] valid_out_mux;
assign data_out_chop_0 = data_out[15:0];
assign agg_0_valid_in = valid_demux[0];
assign valid_out_mux[0] = agg_0_valid_out;
assign next_full[0] = agg_0_next_full;
assign aggs_out[0] = aggs_sep_0;
assign agg_1_valid_in = valid_demux[1];
assign valid_out_mux[1] = agg_1_valid_out;
assign next_full[1] = agg_1_next_full;
assign aggs_out[1] = aggs_sep_1;
assign agg_2_valid_in = valid_demux[2];
assign valid_out_mux[2] = agg_2_valid_out;
assign next_full[2] = agg_2_next_full;
assign aggs_out[2] = aggs_sep_2;
assign agg_3_valid_in = valid_demux[3];
assign valid_out_mux[3] = agg_3_valid_out;
assign next_full[3] = agg_3_next_full;
assign aggs_out[3] = aggs_sep_3;
assign agg_4_valid_in = valid_demux[4];
assign valid_out_mux[4] = agg_4_valid_out;
assign next_full[4] = agg_4_next_full;
assign aggs_out[4] = aggs_sep_4;
assign agg_5_valid_in = valid_demux[5];
assign valid_out_mux[5] = agg_5_valid_out;
assign next_full[5] = agg_5_next_full;
assign aggs_out[5] = aggs_sep_5;
assign agg_6_valid_in = valid_demux[6];
assign valid_out_mux[6] = agg_6_valid_out;
assign next_full[6] = agg_6_next_full;
assign aggs_out[6] = aggs_sep_6;
assign agg_7_valid_in = valid_demux[7];
assign valid_out_mux[7] = agg_7_valid_out;
assign next_full[7] = agg_7_next_full;
assign aggs_out[7] = aggs_sep_7;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    in_sched_ptr <= 6'h0;
  end
  else if (next_full[in_sched[in_sched_ptr]]) begin
    in_sched_ptr <= ((in_period - 6'h1) == in_sched_ptr) ? 6'h0: in_sched_ptr + 6'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    out_sched_ptr <= 6'h0;
  end
  else if (write_act) begin
    out_sched_ptr <= ((out_period - 6'h1) == out_sched_ptr) ? 6'h0: out_sched_ptr + 6'h1;
  end
end
always_comb begin
  valid_demux = 8'h0;
  valid_demux[in_sched[in_sched_ptr]] = valid_in;
end
always_comb begin
  valid_out = valid_out_mux[out_sched[out_sched_ptr]];
end
always_comb begin
  data_out = aggs_out[out_sched[out_sched_ptr]];
end
aggregator agg_0 (
  .agg_out(aggs_sep_0),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_0_next_full),
  .rst_n(rst_n),
  .valid_in(agg_0_valid_in),
  .valid_out(agg_0_valid_out)
);

aggregator agg_1 (
  .agg_out(aggs_sep_1),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_1_next_full),
  .rst_n(rst_n),
  .valid_in(agg_1_valid_in),
  .valid_out(agg_1_valid_out)
);

aggregator agg_2 (
  .agg_out(aggs_sep_2),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_2_next_full),
  .rst_n(rst_n),
  .valid_in(agg_2_valid_in),
  .valid_out(agg_2_valid_out)
);

aggregator agg_3 (
  .agg_out(aggs_sep_3),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_3_next_full),
  .rst_n(rst_n),
  .valid_in(agg_3_valid_in),
  .valid_out(agg_3_valid_out)
);

aggregator agg_4 (
  .agg_out(aggs_sep_4),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_4_next_full),
  .rst_n(rst_n),
  .valid_in(agg_4_valid_in),
  .valid_out(agg_4_valid_out)
);

aggregator agg_5 (
  .agg_out(aggs_sep_5),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_5_next_full),
  .rst_n(rst_n),
  .valid_in(agg_5_valid_in),
  .valid_out(agg_5_valid_out)
);

aggregator agg_6 (
  .agg_out(aggs_sep_6),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_6_next_full),
  .rst_n(rst_n),
  .valid_in(agg_6_valid_in),
  .valid_out(agg_6_valid_out)
);

aggregator agg_7 (
  .agg_out(aggs_sep_7),
  .clk(clk),
  .in_pixels(data_in),
  .next_full(agg_7_next_full),
  .rst_n(rst_n),
  .valid_in(agg_7_valid_in),
  .valid_out(agg_7_valid_out)
);

endmodule   // aggregation_buffer

module aggregator (
  output logic [0:0] [15:0] agg_out,
  input logic clk,
  input logic [15:0] in_pixels,
  output logic next_full,
  input logic rst_n,
  input logic valid_in,
  output logic valid_out
);

logic [0:0][15:0] shift_reg;
assign valid_out = 1'h1;
assign next_full = valid_in;
assign shift_reg[0] = in_pixels;
always_comb begin
  agg_out = shift_reg;
end
endmodule   // aggregator

module demux_reads (
  input logic clk,
  input logic [1:0][0:0] [15:0] data_in,
  output logic [0:0][0:0] [15:0] data_out,
  input logic [1:0] port_in,
  input logic rst_n,
  input logic [1:0] valid_in,
  output logic valid_out
);

logic done;
always_comb begin
  valid_out = 1'h0;
  data_out[0] = 16'h0;
  done = 1'h0;
  if (~done) begin
    if (valid_in[0] & port_in[0]) begin
      valid_out = 1'h1;
      data_out[0] = data_in[0];
      done = 1'h1;
    end
  end
  if (~done) begin
    if (valid_in[1] & port_in[1]) begin
      valid_out = 1'h1;
      data_out[0] = data_in[1];
      done = 1'h1;
    end
  end
end
endmodule   // demux_reads

module input_addr_ctrl (
  output logic [1:0][0:0] [8:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [5:0] [31:0] address_gen_0_ranges,
  input logic [31:0] address_gen_0_starting_addr,
  input logic [5:0] [31:0] address_gen_0_strides,
  input logic clk,
  input logic [0:0][0:0] [15:0] data_in,
  output logic [1:0][0:0][0:0] [15:0] data_out,
  input logic rst_n,
  input logic valid_in,
  input logic wen_en,
  output logic [1:0] wen_to_sram
);

logic [31:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [1:0] done;
logic [0:0][0:0][9:0] local_addrs;
logic valid_gate;
logic [0:0][0:0][1:0] wen_full;
logic [0:0][1:0] wen_reduced;
assign valid_gate = valid_in & wen_en;
assign wen_reduced[0][0] = |wen_full[0][0][0];
assign wen_reduced[0][1] = |wen_full[0][0][1];
always_comb begin
  wen_full[0][0][0] = 1'h0;
  if (valid_gate) begin
    if (local_addrs[0][0][9] == 1'h0) begin
      wen_full[0][0][0] = 1'h1;
    end
  end
  wen_full[0][0][1] = 1'h0;
  if (valid_gate) begin
    if (local_addrs[0][0][9] == 1'h1) begin
      wen_full[0][0][1] = 1'h1;
    end
  end
end
always_comb begin
  wen_to_sram[0] = 1'h0;
  done[0] = 1'h0;
  data_out[0][0] = 16'h0;
  addr_out[0][0] = 9'h0;
  if (~done[0]) begin
    if (wen_reduced[0][0]) begin
      done[0] = 1'h1;
      wen_to_sram[0] = 1'h1;
      data_out[0][0] = data_in[0];
      addr_out[0][0] = local_addrs[0][0][8:0];
    end
  end
  wen_to_sram[1] = 1'h0;
  done[1] = 1'h0;
  data_out[1][0] = 16'h0;
  addr_out[1][0] = 9'h0;
  if (~done[1]) begin
    if (wen_reduced[0][1]) begin
      done[1] = 1'h1;
      wen_to_sram[1] = 1'h1;
      data_out[1][0] = data_in[0];
      addr_out[1][0] = local_addrs[0][0][8:0];
    end
  end
end
assign address_gen_0_step = valid_gate;
always_comb begin
  local_addrs[0][0] = address_gen_0_addr_out[9:0];
end
addr_gen_6 address_gen_0 (
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
  output logic [0:0] [8:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [5:0] [31:0] address_gen_0_ranges,
  input logic [31:0] address_gen_0_starting_addr,
  input logic [5:0] [31:0] address_gen_0_strides,
  input logic clk,
  output logic [1:0] ren,
  input logic rst_n,
  input logic step_in,
  input logic valid_in
);

logic [31:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [0:0][9:0] local_addrs;
always_comb begin
  ren = 2'h0;
  if (valid_in) begin
    ren[local_addrs[0][9]] = 1'h1;
  end
end
always_comb begin
  addr_out = 9'h0;
  addr_out[0] = local_addrs[0][8:0];
end
assign address_gen_0_step = step_in & valid_in;
assign local_addrs[0] = address_gen_0_addr_out[9:0];
addr_gen_6 address_gen_0 (
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

module prefetcher (
  input logic clk,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic [5:0] input_latency,
  output logic prefetch_step,
  input logic rst_n,
  input logic tba_rdy_in,
  output logic valid_out,
  input logic valid_read
);

logic [5:0] cnt;
logic fifo_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    cnt <= 6'h0;
  end
  else if (valid_read & (~tba_rdy_in)) begin
    cnt <= cnt + 6'h1;
  end
  else if ((~valid_read) & tba_rdy_in & (~fifo_empty)) begin
    cnt <= cnt - 6'h1;
  end
end
always_comb begin
  prefetch_step = (cnt + input_latency) < 6'h3F;
end
reg_fifo fifo (
  .clk(clk),
  .clk_en(1'h1),
  .data_in(data_in),
  .data_out(data_out),
  .empty(fifo_empty),
  .pop(tba_rdy_in),
  .push(valid_read),
  .rst_n(rst_n),
  .valid(valid_out)
);

endmodule   // prefetcher

module reg_fifo (
  input logic clk,
  input logic clk_en,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  output logic empty,
  output logic full,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic valid
);

logic passthru;
logic [5:0] rd_ptr;
logic read;
logic [63:0][0:0][15:0] reg_array;
logic [5:0] wr_ptr;
logic write;
assign passthru = pop & push & empty;
assign empty = wr_ptr == rd_ptr;
assign full = (wr_ptr + 6'h1) == rd_ptr;
assign read = pop & (~passthru) & (~empty);
assign write = push & (~passthru) & (~full);

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_ptr <= 6'h0;
  end
  else if (read) begin
    rd_ptr <= rd_ptr + 6'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_ptr <= 6'h0;
  end
  else if (write) begin
    wr_ptr <= wr_ptr + 6'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    reg_array <= 1024'h0;
  end
  else if (write) begin
    reg_array[wr_ptr] <= data_in;
  end
end
always_comb begin
  if (passthru) begin
    data_out = data_in;
  end
  else data_out = reg_array[rd_ptr];
end
always_comb begin
  valid = pop & ((~empty) | passthru);
end
endmodule   // reg_fifo

module rw_arbiter (
  output logic [0:0] [8:0] addr_to_mem,
  output logic cen_mem,
  input logic clk,
  input logic [0:0][0:0] [15:0] data_from_mem,
  output logic [0:0][0:0] [15:0] data_to_mem,
  output logic out_ack,
  output logic [0:0][0:0] [15:0] out_data,
  output logic [0:0] out_port,
  output logic out_valid,
  input logic [0:0] [8:0] rd_addr,
  input logic ren_en,
  input logic ren_in,
  input logic rst_n,
  input logic [0:0] [8:0] w_addr,
  input logic [0:0][0:0] [15:0] w_data,
  input logic wen_in,
  output logic wen_mem
);

logic done;
logic [0:0] next_rd_port;
logic next_rd_port_red;
logic [0:0][8:0] rd_addr_sel;
logic [0:0] rd_port;
logic rd_valid;
logic ren_int;
logic wen_int;
assign ren_int = ren_in & ren_en;
assign wen_int = wen_in;
always_comb begin
  next_rd_port = 1'h0;
  rd_addr_sel[0] = 9'h0;
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
assign out_ack = next_rd_port_red & (~wen_int);
always_comb begin
  cen_mem = wen_int | (|next_rd_port);
  data_to_mem[0] = w_data[0];
  if (wen_int) begin
    addr_to_mem[0] = w_addr[0];
  end
  else addr_to_mem[0] = rd_addr_sel[0];
  wen_mem = wen_int;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_port <= 1'h0;
    rd_valid <= 1'h0;
  end
  else begin
    rd_valid <= ((~wen_int) | 1'h0) & (|next_rd_port);
    rd_port <= next_rd_port;
  end
end
always_comb begin
  out_data = data_from_mem;
  out_port = rd_port;
  out_valid = rd_valid;
end
endmodule   // rw_arbiter

module sram_stub (
  input logic [8:0] addr,
  input logic cen,
  input logic clk,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic wen
);

logic [511:0][0:0][15:0] data_array;

always_ff @(posedge clk) begin
  if (cen & wen) begin
    data_array[addr] <= data_in;
  end
end

always_ff @(posedge clk) begin
  if (cen & (~wen)) begin
    data_out <= data_array[addr];
  end
end
endmodule   // sram_stub

module storage_config_seq (
  output logic [8:0] addr_out,
  input logic clk,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  input logic [3:0] config_en,
  input logic config_rd,
  input logic config_wr,
  output logic [3:0] [15:0] rd_data_out,
  input logic [1:0][0:0] [15:0] rd_data_stg,
  output logic [1:0] ren_out,
  input logic rst_n,
  output logic [1:0] wen_out,
  output logic [0:0] [15:0] wr_data
);

logic [1:0] reduce_en;
logic set_to_addr;
assign reduce_en[0] = |{config_en[0], config_en[2]};
assign reduce_en[1] = |{config_en[1], config_en[3]};
always_comb begin
  set_to_addr = 1'h0;
  if (reduce_en[0]) begin
    set_to_addr = 1'h0;
  end
  if (reduce_en[1]) begin
    set_to_addr = 1'h1;
  end
end
assign addr_out = {set_to_addr, config_addr_in};
assign wr_data[0] = config_data_in;
assign rd_data_out[0] = rd_data_stg[0];
assign rd_data_out[1] = rd_data_stg[0];
assign rd_data_out[2] = rd_data_stg[1];
assign rd_data_out[3] = rd_data_stg[1];
assign wen_out[0] = config_wr & (|config_en[1:0]);
assign wen_out[1] = config_wr & (|config_en[3:2]);
assign ren_out[0] = config_rd & (|config_en[1:0]);
assign ren_out[1] = config_rd & (|config_en[3:2]);
endmodule   // storage_config_seq

module sync_groups (
  input logic ack_in,
  input logic clk,
  input logic [0:0][0:0] [15:0] data_in,
  output logic [0:0][0:0] [15:0] data_out,
  output logic rd_sync_gate,
  input logic ren_in,
  input logic rst_n,
  input logic [0:0] sync_group,
  input logic valid_in,
  output logic valid_out
);

logic [0:0][0:0][15:0] data_reg;
logic group_finished;
logic [0:0] grp_fin_large;
logic [0:0] local_gate_bus;
logic [0:0] local_gate_bus_tpose;
logic [0:0] local_gate_mask;
logic local_gate_reduced;
logic ren_int;
logic [0:0] sync_agg;
logic sync_valid;
logic valid_reg;
assign data_out = data_reg;
assign rd_sync_gate = local_gate_reduced;
assign ren_int = ren_in & local_gate_reduced;
always_comb begin
  if (sync_group == 1'h1) begin
    sync_agg = valid_reg;
  end
  else sync_agg = 1'h1;
end
always_comb begin
  sync_valid = &sync_agg;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    data_reg[0] <= 16'h0;
    valid_reg <= 1'h0;
  end
  else if (|(sync_valid & sync_group)) begin
    data_reg[0] <= data_in[0];
    valid_reg <= valid_in;
  end
  else if (~valid_reg) begin
    data_reg[0] <= data_in[0];
    valid_reg <= valid_in;
  end
end
always_comb begin
  valid_out = |(sync_valid & sync_group);
end
always_comb begin
  local_gate_reduced = &local_gate_bus_tpose;
end

always_ff @(posedge clk, negedge rst_n) begin
  if ((~rst_n) | group_finished) begin
    local_gate_bus <= ~1'h0;
  end
  else local_gate_bus <= local_gate_bus & local_gate_mask;
end
always_comb begin
  local_gate_bus_tpose = local_gate_bus;
end
always_comb begin
  group_finished = &grp_fin_large;
end
always_comb begin
  local_gate_mask = 1'h1;
  if (sync_group == 1'h1) begin
    local_gate_mask = ~(ren_int & ack_in);
  end
end
always_comb begin
  grp_fin_large = 1'h1;
  if (sync_group == 1'h1) begin
    grp_fin_large = (~local_gate_bus) | (~local_gate_mask);
  end
end
endmodule   // sync_groups

module transpose_buffer (
  input logic ack_in,
  input logic clk,
  output logic [0:0] [15:0] col_pixels,
  input logic [1:0] dimensionality,
  input logic [2047:0] indices,
  input logic [15:0] input_data,
  output logic output_valid,
  input logic [10:0] range_inner,
  input logic [10:0] range_outer,
  output logic rdy_to_arbiter,
  input logic ren,
  input logic rst_n,
  input logic [3:0] stride,
  input logic tb_height,
  input logic valid_data
);

logic [21:0] curr_out_start;
logic [10:0] index_inner;
logic [10:0] index_outer;
logic indices_index_inner;
logic input_buf_index;
logic input_index;
logic old_start_data;
logic out_buf_index;
logic [21:0] output_index_abs;
logic pause_output;
logic pause_tb;
logic prev_out_buf_index;
logic prev_output_valid;
logic row_index;
logic start_data;
logic [1:0][15:0] tb;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    index_outer <= 11'h0;
  end
  else if (dimensionality == 2'h0) begin
    index_outer <= 11'h0;
  end
  else if (dimensionality == 2'h1) begin
    if ((range_outer - 11'h1) == index_outer) begin
      if (~pause_output) begin
        index_outer <= 11'h0;
      end
      else index_outer <= index_outer;
    end
    else if (pause_tb) begin
      index_outer <= index_outer;
    end
    else if (~pause_output) begin
      index_outer <= index_outer + 11'h1;
    end
  end
  else if ((range_inner - 11'h1) == index_inner) begin
    if ((range_outer - 11'h1) == index_outer) begin
      if (~pause_output) begin
        index_outer <= 11'h0;
      end
      else index_outer <= index_outer;
    end
    else if (~pause_output) begin
      index_outer <= index_outer + 11'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    index_inner <= 11'h0;
  end
  else if (dimensionality <= 2'h1) begin
    index_inner <= 11'h0;
  end
  else if ((range_inner - 11'h1) == index_inner) begin
    if (~pause_output) begin
      index_inner <= 11'h0;
    end
    else index_inner <= index_inner;
  end
  else if (pause_tb) begin
    index_inner <= index_inner;
  end
  else if (~pause_output) begin
    index_inner <= index_inner + 11'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pause_tb <= 1'h1;
  end
  else if (dimensionality == 2'h0) begin
    pause_tb <= 1'h1;
  end
  else if (dimensionality == 2'h1) begin
    if ((range_outer - 11'h1) == index_outer) begin
      if (~pause_output) begin
        pause_tb <= ~valid_data;
      end
      else pause_tb <= 1'h0;
    end
    else if (pause_tb) begin
      pause_tb <= ~valid_data;
    end
    else if (~pause_output) begin
      pause_tb <= 1'h0;
    end
  end
  else if ((range_inner - 11'h1) == index_inner) begin
    if ((range_outer - 11'h1) == index_outer) begin
      if (~pause_output) begin
        pause_tb <= ~valid_data;
      end
      else pause_tb <= 1'h0;
    end
    else pause_tb <= 1'h0;
  end
  else if (pause_tb) begin
    pause_tb <= ~valid_data;
  end
  else if (~pause_output) begin
    pause_tb <= 1'h0;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    row_index <= 1'h0;
  end
  else if (dimensionality == 2'h0) begin
    row_index <= 1'h0;
  end
  else if ((valid_data & row_index) == (tb_height - 1'h1)) begin
    row_index <= 1'h0;
  end
  else if (valid_data) begin
    row_index <= row_index + 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    input_buf_index <= 1'h0;
  end
  else if (valid_data & ((tb_height - 1'h1) == row_index)) begin
    input_buf_index <= ~input_buf_index;
  end
end

always_ff @(posedge clk) begin
  if (valid_data) begin
    if (dimensionality == 2'h0) begin
      tb[input_index] <= 16'h0;
    end
    else tb[input_index] <= input_data;
  end
end

always_ff @(posedge clk) begin
  if (tb_height > 1'h0) begin
    if (dimensionality == 2'h0) begin
      col_pixels[0] <= 16'h0;
    end
    else if (out_buf_index) begin
      col_pixels[0] <= tb[0];
    end
    else col_pixels[0] <= tb[1];
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    prev_output_valid <= 1'h0;
  end
  else if (dimensionality == 2'h0) begin
    prev_output_valid <= 1'h0;
  end
  else if (pause_output) begin
    prev_output_valid <= 1'h0;
  end
  else prev_output_valid <= 1'h1;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    output_valid <= 1'h0;
  end
  else output_valid <= prev_output_valid;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    out_buf_index <= 1'h1;
  end
  else if ((curr_out_start + 22'h1) <= output_index_abs) begin
    out_buf_index <= ~out_buf_index;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rdy_to_arbiter <= 1'h1;
  end
  else if (dimensionality == 2'h0) begin
    rdy_to_arbiter <= 1'h0;
  end
  else if (start_data & (~old_start_data)) begin
    rdy_to_arbiter <= 1'h1;
  end
  else if (prev_out_buf_index != out_buf_index) begin
    rdy_to_arbiter <= 1'h1;
  end
  else if (tb_height != 1'h1) begin
    if ((tb_height - 1'h1) != row_index) begin
      rdy_to_arbiter <= 1'h1;
    end
  end
  else if (ack_in) begin
    rdy_to_arbiter <= 1'h0;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    start_data <= 1'h0;
  end
  else if (valid_data & (~start_data)) begin
    start_data <= 1'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    curr_out_start <= 22'h0;
  end
  else if (dimensionality == 2'h0) begin
    curr_out_start <= 22'h0;
  end
  else if ((curr_out_start + 22'h1) <= output_index_abs) begin
    curr_out_start <= curr_out_start + 22'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    prev_out_buf_index <= 1'h0;
  end
  else prev_out_buf_index <= out_buf_index;
end

always_ff @(posedge clk) begin
  old_start_data <= start_data;
end
always_comb begin
  if (pause_tb) begin
    pause_output = 1'h1;
  end
  else if (start_data & (~old_start_data)) begin
    pause_output = 1'h1;
  end
  else pause_output = ~ren;
end
always_comb begin
  if (dimensionality == 2'h0) begin
    input_index = 1'h0;
  end
  else if (input_buf_index) begin
    input_index = 1'h1 + 1'(row_index);
  end
  else input_index = 1'(row_index);
end
always_comb begin
  if (dimensionality == 2'h0) begin
    indices_index_inner = 1'h0;
    output_index_abs = 22'h0;
  end
  else if (dimensionality == 2'h1) begin
    indices_index_inner = 1'h0;
    output_index_abs = 22'(index_outer) * 22'(stride);
  end
  else begin
    indices_index_inner = indices[index_inner];
    output_index_abs = (22'(index_outer) * 22'(stride)) + 22'(indices_index_inner);
  end
end
endmodule   // transpose_buffer

module transpose_buffer_aggregation (
  input logic [15:0] SRAM_to_tb_data,
  input logic ack_in,
  input logic clk,
  input logic rst_n,
  input logic [1:0] tb_0_dimensionality,
  input logic [2047:0] tb_0_indices,
  input logic [10:0] tb_0_range_inner,
  input logic [10:0] tb_0_range_outer,
  input logic [3:0] tb_0_stride,
  input logic tb_0_tb_height,
  output logic tb_arbiter_rdy,
  input logic tb_index_for_data,
  output logic [15:0] tb_to_interconnect_data,
  output logic tb_to_interconnect_valid,
  input logic tba_ren,
  input logic valid_data
);

logic [15:0] output_inter;
logic [0:0][15:0] tb_0_col_pixels;
logic tb_0_output_valid;
logic tb_0_rdy_to_arbiter;
logic tb_0_valid_data;
logic tb_arbiter_rdy_all;
logic [0:0][0:0][15:0] tb_output_data_all;
logic tb_output_valid_all;
logic valid_data_all;
assign tb_0_valid_data = valid_data_all;
assign tb_output_data_all[0] = tb_0_col_pixels;
assign tb_output_valid_all = tb_0_output_valid;
assign tb_arbiter_rdy_all = tb_0_rdy_to_arbiter;
always_comb begin
  if (~valid_data) begin
    valid_data_all = 1'h0;
  end
  else if (tb_index_for_data == 1'h0) begin
    valid_data_all = 1'h1;
  end
  else valid_data_all = 1'h0;
end
always_comb begin
  output_inter = tb_output_data_all[0];
  tb_to_interconnect_data = output_inter;
  if (tb_output_valid_all > 1'h0) begin
    tb_to_interconnect_valid = 1'h1;
  end
  else tb_to_interconnect_valid = 1'h0;
end
always_comb begin
  if (tb_arbiter_rdy_all > 1'h0) begin
    tb_arbiter_rdy = 1'h1;
  end
  else tb_arbiter_rdy = 1'h0;
end
transpose_buffer tb_0 (
  .ack_in(ack_in),
  .clk(clk),
  .col_pixels(tb_0_col_pixels),
  .dimensionality(tb_0_dimensionality),
  .indices(tb_0_indices),
  .input_data(SRAM_to_tb_data),
  .output_valid(tb_0_output_valid),
  .range_inner(tb_0_range_inner),
  .range_outer(tb_0_range_outer),
  .rdy_to_arbiter(tb_0_rdy_to_arbiter),
  .ren(tba_ren),
  .rst_n(rst_n),
  .stride(tb_0_stride),
  .tb_height(tb_0_tb_height),
  .valid_data(tb_0_valid_data)
);

endmodule   // transpose_buffer_aggregation

