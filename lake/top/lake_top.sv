module LakeTop (
  input logic [1:0] [15:0] addr_in,
  input logic clk,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  output logic [0:0] [15:0] config_data_out,
  input logic config_en,
  input logic config_read,
  input logic config_write,
  input logic [1:0] [15:0] data_in,
  output logic [1:0] [15:0] data_out,
  input logic [3:0] input_addr_ctrl_address_gen_0_dimensionality,
  input logic [1:0] [31:0] input_addr_ctrl_address_gen_0_ranges,
  input logic [31:0] input_addr_ctrl_address_gen_0_starting_addr,
  input logic [1:0] [31:0] input_addr_ctrl_address_gen_0_strides,
  input logic [3:0] input_addr_ctrl_address_gen_1_dimensionality,
  input logic [1:0] [31:0] input_addr_ctrl_address_gen_1_ranges,
  input logic [31:0] input_addr_ctrl_address_gen_1_starting_addr,
  input logic [1:0] [31:0] input_addr_ctrl_address_gen_1_strides,
  input logic [3:0] output_addr_ctrl_address_gen_0_dimensionality,
  input logic [1:0] [31:0] output_addr_ctrl_address_gen_0_ranges,
  input logic [31:0] output_addr_ctrl_address_gen_0_starting_addr,
  input logic [1:0] [31:0] output_addr_ctrl_address_gen_0_strides,
  input logic [3:0] output_addr_ctrl_address_gen_1_dimensionality,
  input logic [1:0] [31:0] output_addr_ctrl_address_gen_1_ranges,
  input logic [31:0] output_addr_ctrl_address_gen_1_starting_addr,
  input logic [1:0] [31:0] output_addr_ctrl_address_gen_1_strides,
  input logic [2:0] pre_fetch_0_input_latency,
  input logic [2:0] pre_fetch_1_input_latency,
  input logic [1:0] ren,
  input logic [1:0] ren_en,
  input logic rst_n,
  input logic [1:0] [1:0] sync_grp_sync_group,
  output logic [1:0] valid_out,
  input logic [1:0] wen,
  input logic [1:0] wen_en
);

logic [1:0] ack_reduced;
logic [1:0] ack_transpose;
logic [1:0][4:0] addr_out;
logic [0:0][1:0][4:0] addr_to_arb;
logic [0:0][1:0] arb_acks;
logic [0:0][1:0][0:0][15:0] arb_dat_out;
logic [1:0][0:0][15:0] arb_dat_out_f;
logic [0:0][1:0][1:0] arb_port_out;
logic [1:0][1:0] arb_port_out_f;
logic [0:0][1:0] arb_valid_out;
logic [1:0] arb_valid_out_f;
logic [0:0][1:0][0:0][15:0] data_to_arb;
logic [1:0][0:0][15:0] data_to_pref;
logic [1:0][0:0][15:0] data_to_sync;
logic [4:0] mem_addr_cfg;
logic [0:0][1:0] mem_cen_datapath;
logic [0:0][1:0] mem_cen_in;
logic [0:0][15:0] mem_data_cfg;
logic [0:0][1:0][0:0][15:0] mem_data_dp;
logic [0:0][1:0][0:0][15:0] mem_data_in;
logic [0:0][0:0][15:0] mem_data_low_pt;
logic [0:0][1:0][0:0][15:0] mem_data_out;
logic mem_ren_cfg;
logic mem_wen_cfg;
logic [0:0][1:0] mem_wen_datapath;
logic [0:0][1:0] mem_wen_in;
logic [0:0][15:0] pre_fetch_0_data_in;
logic [0:0][15:0] pre_fetch_0_data_out;
logic pre_fetch_0_prefetch_step;
logic pre_fetch_0_tba_rdy_in;
logic pre_fetch_0_valid_out;
logic pre_fetch_0_valid_read;
logic [0:0][15:0] pre_fetch_1_data_in;
logic [0:0][15:0] pre_fetch_1_data_out;
logic pre_fetch_1_prefetch_step;
logic pre_fetch_1_tba_rdy_in;
logic pre_fetch_1_valid_out;
logic pre_fetch_1_valid_read;
logic [1:0] prefetch_step;
logic [0:0][1:0][4:0] rd_mem_addr_dp;
logic [0:0][1:0][4:0] rd_mem_addr_in;
logic [1:0] rd_sync_gate;
logic [0:0][1:0] ren_out;
logic [1:0] ren_out_reduced;
logic [1:0] ren_out_tpose;
logic [1:0][0:0][15:0] rf_0_data_in;
logic [1:0][0:0][15:0] rf_0_data_out;
logic [1:0][4:0] rf_0_rd_addr;
logic [1:0] rf_0_wen;
logic [1:0][4:0] rf_0_wr_addr;
logic [1:0] rw_arb_0_cen_mem;
logic [1:0][0:0][15:0] rw_arb_0_data_from_mem;
logic [1:0][0:0][15:0] rw_arb_0_data_to_mem;
logic [1:0] rw_arb_0_out_ack;
logic [1:0][0:0][15:0] rw_arb_0_out_data;
logic [1:0][1:0] rw_arb_0_out_port;
logic [1:0] rw_arb_0_out_valid;
logic [1:0][4:0] rw_arb_0_rd_addr_to_mem;
logic [1:0] rw_arb_0_ren_in;
logic [1:0][4:0] rw_arb_0_w_addr;
logic [1:0][0:0][15:0] rw_arb_0_w_data;
logic [1:0] rw_arb_0_wen_in;
logic [1:0] rw_arb_0_wen_mem;
logic [1:0][4:0] rw_arb_0_wr_addr_to_mem;
logic [1:0] valid_to_pref;
logic [1:0] valid_to_sync;
logic [0:0][1:0] wen_to_arb;
logic [0:0][1:0][4:0] wr_mem_addr_dp;
logic [0:0][1:0][4:0] wr_mem_addr_in;
assign ren_out_tpose[0] = ren_out[0][0];
assign ren_out_tpose[1] = ren_out[0][1];
assign mem_data_low_pt[0] = mem_data_out[0][0];
assign rw_arb_0_wen_in = wen_to_arb[0];
assign rw_arb_0_w_data = data_to_arb[0];
assign rw_arb_0_w_addr = addr_to_arb[0];
assign rw_arb_0_data_from_mem = mem_data_out[0];
assign rw_arb_0_ren_in = ren_out[0] & rd_sync_gate;
assign arb_dat_out[0] = rw_arb_0_out_data;
assign arb_port_out[0] = rw_arb_0_out_port;
assign arb_valid_out[0] = rw_arb_0_out_valid;
assign mem_cen_datapath[0] = rw_arb_0_cen_mem;
assign mem_wen_datapath[0] = rw_arb_0_wen_mem;
assign mem_data_dp[0] = rw_arb_0_data_to_mem;
assign arb_acks[0] = rw_arb_0_out_ack;
assign wr_mem_addr_dp[0] = rw_arb_0_wr_addr_to_mem;
assign rd_mem_addr_dp[0] = rw_arb_0_rd_addr_to_mem;
assign mem_wen_in[0][0] = (|config_en) ? mem_wen_cfg: mem_wen_datapath[0][0];
assign mem_cen_in[0][0] = (|config_en) ? mem_ren_cfg: mem_cen_datapath[0][0];
assign wr_mem_addr_in[0][0] = (|config_en) ? mem_addr_cfg: wr_mem_addr_dp[0][0];
assign rd_mem_addr_in[0][0] = (|config_en) ? mem_addr_cfg: rd_mem_addr_dp[0][0];
assign mem_data_in[0][0] = (|config_en) ? mem_data_cfg: mem_data_dp[0][0];
assign mem_wen_in[0][1] = mem_wen_datapath[0][1];
assign wr_mem_addr_in[0][1] = wr_mem_addr_dp[0][1];
assign mem_data_in[0][1] = mem_data_dp[0][1];
assign mem_cen_in[0][1] = mem_cen_datapath[0][1];
assign rd_mem_addr_in[0][1] = rd_mem_addr_dp[0][1];
assign rf_0_wen = mem_wen_in[0];
assign rf_0_wr_addr = wr_mem_addr_in[0];
assign rf_0_rd_addr = rd_mem_addr_in[0];
assign rf_0_data_in = mem_data_in[0];
assign mem_data_out[0] = rf_0_data_out;
assign arb_dat_out_f[0] = arb_dat_out[0][0];
assign arb_port_out_f[0] = arb_port_out[0][0];
assign arb_valid_out_f[0] = arb_valid_out[0][0];
assign arb_dat_out_f[1] = arb_dat_out[0][1];
assign arb_port_out_f[1] = arb_port_out[0][1];
assign arb_valid_out_f[1] = arb_valid_out[0][1];
assign ren_out_reduced[0] = |ren_out_tpose[0];
assign ren_out_reduced[1] = |ren_out_tpose[1];
assign pre_fetch_0_data_in = data_to_pref[0];
assign pre_fetch_0_valid_read = valid_to_pref[0];
assign pre_fetch_0_tba_rdy_in = ren[0];
assign valid_out[0] = pre_fetch_0_valid_out;
assign prefetch_step[0] = pre_fetch_0_prefetch_step;
assign data_out[0] = pre_fetch_0_data_out[0];
assign pre_fetch_1_data_in = data_to_pref[1];
assign pre_fetch_1_valid_read = valid_to_pref[1];
assign pre_fetch_1_tba_rdy_in = ren[1];
assign valid_out[1] = pre_fetch_1_valid_out;
assign prefetch_step[1] = pre_fetch_1_prefetch_step;
assign data_out[1] = pre_fetch_1_data_out[0];
always_comb begin
  ack_transpose[0] = arb_acks[0][0];
  ack_transpose[1] = arb_acks[0][1];
end
always_comb begin
  ack_reduced[0] = |ack_transpose[0];
  ack_reduced[1] = |ack_transpose[1];
end
input_addr_ctrl input_addr_ctrl (
  .addr_out(addr_to_arb),
  .address_gen_0_dimensionality(input_addr_ctrl_address_gen_0_dimensionality),
  .address_gen_0_ranges(input_addr_ctrl_address_gen_0_ranges),
  .address_gen_0_starting_addr(input_addr_ctrl_address_gen_0_starting_addr),
  .address_gen_0_strides(input_addr_ctrl_address_gen_0_strides),
  .address_gen_1_dimensionality(input_addr_ctrl_address_gen_1_dimensionality),
  .address_gen_1_ranges(input_addr_ctrl_address_gen_1_ranges),
  .address_gen_1_starting_addr(input_addr_ctrl_address_gen_1_starting_addr),
  .address_gen_1_strides(input_addr_ctrl_address_gen_1_strides),
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
  .address_gen_1_dimensionality(output_addr_ctrl_address_gen_1_dimensionality),
  .address_gen_1_ranges(output_addr_ctrl_address_gen_1_ranges),
  .address_gen_1_starting_addr(output_addr_ctrl_address_gen_1_starting_addr),
  .address_gen_1_strides(output_addr_ctrl_address_gen_1_strides),
  .clk(clk),
  .ren(ren_out),
  .rst_n(rst_n),
  .step_in(ack_reduced),
  .valid_in(prefetch_step)
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

prefetcher pre_fetch_1 (
  .clk(clk),
  .data_in(pre_fetch_1_data_in),
  .data_out(pre_fetch_1_data_out),
  .input_latency(pre_fetch_1_input_latency),
  .prefetch_step(pre_fetch_1_prefetch_step),
  .rst_n(rst_n),
  .tba_rdy_in(pre_fetch_1_tba_rdy_in),
  .valid_out(pre_fetch_1_valid_out),
  .valid_read(pre_fetch_1_valid_read)
);

endmodule   // LakeTop

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
  input logic [1:0][0:0] [15:0] data_in,
  output logic [1:0][0:0] [15:0] data_out,
  input logic [1:0] [1:0] port_in,
  input logic rst_n,
  input logic [1:0] valid_in,
  output logic [1:0] valid_out
);

logic [1:0] done;
always_comb begin
  valid_out[0] = 1'h0;
  data_out[0] = 16'h0;
  done[0] = 1'h0;
  if (~done[0]) begin
    if (valid_in[0] & port_in[0][0]) begin
      valid_out[0] = 1'h1;
      data_out[0] = data_in[0];
      done[0] = 1'h1;
    end
  end
  if (~done[0]) begin
    if (valid_in[1] & port_in[1][0]) begin
      valid_out[0] = 1'h1;
      data_out[0] = data_in[1];
      done[0] = 1'h1;
    end
  end
  valid_out[1] = 1'h0;
  data_out[1] = 16'h0;
  done[1] = 1'h0;
  if (~done[1]) begin
    if (valid_in[0] & port_in[0][1]) begin
      valid_out[1] = 1'h1;
      data_out[1] = data_in[0];
      done[1] = 1'h1;
    end
  end
  if (~done[1]) begin
    if (valid_in[1] & port_in[1][1]) begin
      valid_out[1] = 1'h1;
      data_out[1] = data_in[1];
      done[1] = 1'h1;
    end
  end
end
endmodule   // demux_reads

module input_addr_ctrl (
  output logic [0:0][1:0] [4:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [1:0] [31:0] address_gen_0_ranges,
  input logic [31:0] address_gen_0_starting_addr,
  input logic [1:0] [31:0] address_gen_0_strides,
  input logic [3:0] address_gen_1_dimensionality,
  input logic [1:0] [31:0] address_gen_1_ranges,
  input logic [31:0] address_gen_1_starting_addr,
  input logic [1:0] [31:0] address_gen_1_strides,
  input logic clk,
  input logic [1:0][0:0] [15:0] data_in,
  output logic [0:0][1:0][0:0] [15:0] data_out,
  input logic rst_n,
  input logic [1:0] valid_in,
  input logic [1:0] wen_en,
  output logic [0:0] [1:0] wen_to_sram
);

logic [31:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [31:0] address_gen_1_addr_out;
logic address_gen_1_step;
logic [0:0][1:0] done;
logic [0:0][0:0][7:0] idx_cnt;
logic [1:0][0:0][4:0] local_addrs;
logic [1:0] valid_gate;
logic [1:0][0:0] wen_full;
logic [1:0] wen_reduced;
assign valid_gate = valid_in & wen_en;
assign wen_reduced[0] = |wen_full[0];
assign wen_reduced[1] = |wen_full[1];
always_comb begin
  wen_full[0] = 1'h0;
  if (valid_gate[0]) begin
    wen_full[0] = 1'h1;
  end
  wen_full[1] = 1'h0;
  if (valid_gate[1]) begin
    wen_full[1] = 1'h1;
  end
end
always_comb begin
  wen_to_sram[0][0] = 1'h0;
  done[0][0] = 1'h0;
  data_out[0][0] = 16'h0;
  addr_out[0][0] = 5'h0;
  if (~done[0][0]) begin
    if (wen_reduced[0]) begin
      done[0][0] = 1'h1;
      wen_to_sram[0][0] = 1'h1;
      data_out[0][0] = data_in[0];
      addr_out[0][0] = local_addrs[0][0][4:0];
    end
  end
  if (~done[0][0]) begin
    if (wen_reduced[1]) begin
      done[0][0] = 1'h1;
      wen_to_sram[0][0] = 1'h1;
      data_out[0][0] = data_in[1];
      addr_out[0][0] = local_addrs[1][0][4:0];
    end
  end
end
always_comb begin
  wen_to_sram[0][1] = 1'h0;
  done[0][1] = 1'h0;
  data_out[0][1] = 16'h0;
  addr_out[0][1] = 5'h0;
  idx_cnt[0][0] = 8'h0;
  if (~done[0][1]) begin
    if (wen_reduced[0] & (idx_cnt[0][0] == 8'h1)) begin
      wen_to_sram[0][1] = 1'h1;
      done[0][1] = 1'h1;
      data_out[0][1] = data_in[0];
      addr_out[0][1] = local_addrs[0][0][4:0];
    end
    idx_cnt[0][0] = idx_cnt[0][0] + 8'h1;
  end
  if (~done[0][1]) begin
    if (wen_reduced[1] & (idx_cnt[0][0] == 8'h1)) begin
      wen_to_sram[0][1] = 1'h1;
      done[0][1] = 1'h1;
      data_out[0][1] = data_in[1];
      addr_out[0][1] = local_addrs[1][0][4:0];
    end
    idx_cnt[0][0] = idx_cnt[0][0] + 8'h1;
  end
end
assign address_gen_0_step = valid_gate[0];
assign address_gen_1_step = valid_gate[1];
always_comb begin
  local_addrs[0][0] = address_gen_0_addr_out[4:0];
  local_addrs[1][0] = address_gen_1_addr_out[4:0];
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

addr_gen_2 address_gen_1 (
  .addr_out(address_gen_1_addr_out),
  .clk(clk),
  .clk_en(1'h1),
  .dimensionality(address_gen_1_dimensionality),
  .flush(1'h0),
  .ranges(address_gen_1_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_1_starting_addr),
  .step(address_gen_1_step),
  .strides(address_gen_1_strides)
);

endmodule   // input_addr_ctrl

module output_addr_ctrl (
  output logic [1:0] [4:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [1:0] [31:0] address_gen_0_ranges,
  input logic [31:0] address_gen_0_starting_addr,
  input logic [1:0] [31:0] address_gen_0_strides,
  input logic [3:0] address_gen_1_dimensionality,
  input logic [1:0] [31:0] address_gen_1_ranges,
  input logic [31:0] address_gen_1_starting_addr,
  input logic [1:0] [31:0] address_gen_1_strides,
  input logic clk,
  output logic [0:0] [1:0] ren,
  input logic rst_n,
  input logic [1:0] step_in,
  input logic [1:0] valid_in
);

logic [31:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [31:0] address_gen_1_addr_out;
logic address_gen_1_step;
logic [1:0][4:0] local_addrs;
always_comb begin
  ren = 2'h0;
  if (valid_in[0]) begin
    ren[0][0] = 1'h1;
  end
  if (valid_in[1]) begin
    ren[0][1] = 1'h1;
  end
end
always_comb begin
  addr_out = 10'h0;
  addr_out[0] = local_addrs[0][4:0];
  addr_out[1] = local_addrs[1][4:0];
end
assign address_gen_0_step = step_in[0] & valid_in[0];
assign local_addrs[0] = address_gen_0_addr_out[4:0];
assign address_gen_1_step = step_in[1] & valid_in[1];
assign local_addrs[1] = address_gen_1_addr_out[4:0];
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

addr_gen_2 address_gen_1 (
  .addr_out(address_gen_1_addr_out),
  .clk(clk),
  .clk_en(1'h1),
  .dimensionality(address_gen_1_dimensionality),
  .flush(1'h0),
  .ranges(address_gen_1_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_1_starting_addr),
  .step(address_gen_1_step),
  .strides(address_gen_1_strides)
);

endmodule   // output_addr_ctrl

module prefetcher (
  input logic clk,
  input logic [0:0] [15:0] data_in,
  output logic [0:0] [15:0] data_out,
  input logic [2:0] input_latency,
  output logic prefetch_step,
  input logic rst_n,
  input logic tba_rdy_in,
  output logic valid_out,
  input logic valid_read
);

logic [2:0] cnt;
logic fifo_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    cnt <= 3'h0;
  end
  else if (valid_read & (~tba_rdy_in)) begin
    cnt <= cnt + 3'h1;
  end
  else if ((~valid_read) & tba_rdy_in & (~fifo_empty)) begin
    cnt <= cnt - 3'h1;
  end
end
always_comb begin
  prefetch_step = (cnt + input_latency) < 3'h7;
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
logic [2:0] rd_ptr;
logic read;
logic [7:0][0:0][15:0] reg_array;
logic [2:0] wr_ptr;
logic write;
assign passthru = pop & push & empty;
assign empty = wr_ptr == rd_ptr;
assign full = (wr_ptr + 3'h1) == rd_ptr;
assign read = pop & (~passthru) & (~empty);
assign write = push & (~passthru) & (~full);

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_ptr <= 3'h0;
  end
  else if (read) begin
    rd_ptr <= rd_ptr + 3'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_ptr <= 3'h0;
  end
  else if (write) begin
    wr_ptr <= wr_ptr + 3'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    reg_array <= 128'h0;
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

module register_file (
  input logic clk,
  input logic [1:0][0:0] [15:0] data_in,
  output logic [1:0][0:0] [15:0] data_out,
  input logic [1:0] [4:0] rd_addr,
  input logic [1:0] wen,
  input logic [1:0] [4:0] wr_addr
);

logic [31:0][0:0][15:0] data_array;

always_ff @(posedge clk) begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      if (wen[1'(i)]) begin
        data_array[wr_addr[1'(i)]] <= data_in[1'(i)];
      end
    end
end
always_comb begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      data_out[1'(i)] = data_array[rd_addr[1'(i)]];
    end
end
endmodule   // register_file

module rw_arbiter (
  output logic [1:0] cen_mem,
  input logic clk,
  input logic [1:0][0:0] [15:0] data_from_mem,
  output logic [1:0][0:0] [15:0] data_to_mem,
  output logic [1:0] out_ack,
  output logic [1:0][0:0] [15:0] out_data,
  output logic [1:0] [1:0] out_port,
  output logic [1:0] out_valid,
  input logic [1:0] [4:0] rd_addr,
  output logic [1:0] [4:0] rd_addr_to_mem,
  input logic [1:0] ren_en,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] [4:0] w_addr,
  input logic [1:0][0:0] [15:0] w_data,
  input logic [1:0] wen_in,
  output logic [1:0] wen_mem,
  output logic [1:0] [4:0] wr_addr_to_mem
);

logic [1:0] done;
logic [0:0][4:0] idx_cnt;
logic [1:0][1:0] next_rd_port;
logic [1:0] next_rd_port_red;
logic [1:0][4:0] rd_addr_sel;
logic [1:0][1:0] rd_port;
logic [1:0] rd_valid;
logic [1:0] ren_int;
logic [1:0] wen_int;
assign ren_int = ren_in & ren_en;
assign wen_int = wen_in;
always_comb begin
  next_rd_port[0] = 2'h0;
  rd_addr_sel[0] = 5'h0;
  done[0] = 1'h0;
  if (~done[0]) begin
    if (ren_int[0]) begin
      rd_addr_sel[0] = rd_addr[0];
      next_rd_port[0][0] = 1'h1;
      done[0] = 1'h1;
    end
  end
  if (~done[0]) begin
    if (ren_int[1]) begin
      rd_addr_sel[0] = rd_addr[1];
      next_rd_port[0][1] = 1'h1;
      done[0] = 1'h1;
    end
  end
end
always_comb begin
  next_rd_port[1] = 2'h0;
  idx_cnt[0] = 5'h0;
  rd_addr_sel[1] = 5'h0;
  done[1] = 1'h0;
  if (~done[1]) begin
    if (ren_int[0] & (idx_cnt[0] == 5'h1)) begin
      done[1] = 1'h1;
      rd_addr_sel[1] = rd_addr[0];
      next_rd_port[1][0] = 1'h1;
    end
    idx_cnt[0] = idx_cnt[0] + 5'h1;
  end
  if (~done[1]) begin
    if (ren_int[1] & (idx_cnt[0] == 5'h1)) begin
      done[1] = 1'h1;
      rd_addr_sel[1] = rd_addr[1];
      next_rd_port[1][1] = 1'h1;
    end
    idx_cnt[0] = idx_cnt[0] + 5'h1;
  end
end
assign next_rd_port_red[0] = |{next_rd_port[0][0], next_rd_port[1][0]};
assign next_rd_port_red[1] = |{next_rd_port[0][1], next_rd_port[1][1]};
assign out_ack = next_rd_port_red;
always_comb begin
  wen_mem[0] = wen_int[0];
  data_to_mem[0] = w_data[0];
  wr_addr_to_mem[0] = w_addr[0];
end
always_comb begin
  wen_mem[1] = wen_int[1];
  data_to_mem[1] = w_data[1];
  wr_addr_to_mem[1] = w_addr[1];
end
always_comb begin
  cen_mem[0] = |next_rd_port[0];
  rd_addr_to_mem[0] = rd_addr_sel[0];
end
always_comb begin
  cen_mem[1] = |next_rd_port[1];
  rd_addr_to_mem[1] = rd_addr_sel[1];
end
always_comb begin
  rd_valid[0] = ((~wen_int[0]) | 1'h1) & (|next_rd_port[0]);
  rd_port[0] = next_rd_port[0];
end
always_comb begin
  rd_valid[1] = ((~wen_int[1]) | 1'h1) & (|next_rd_port[1]);
  rd_port[1] = next_rd_port[1];
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

module sync_groups (
  input logic [1:0] ack_in,
  input logic clk,
  input logic [1:0][0:0] [15:0] data_in,
  output logic [1:0][0:0] [15:0] data_out,
  output logic [1:0] rd_sync_gate,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] [1:0] sync_group,
  input logic [1:0] valid_in,
  output logic [1:0] valid_out
);

logic [1:0][0:0][15:0] data_reg;
logic [1:0] group_finished;
logic [1:0][1:0] grp_fin_large;
logic [1:0][1:0] local_gate_bus;
logic [1:0][1:0] local_gate_bus_tpose;
logic [1:0][1:0] local_gate_mask;
logic [1:0] local_gate_reduced;
logic [1:0] ren_int;
logic [1:0][1:0] sync_agg;
logic [1:0] sync_valid;
logic [1:0] valid_reg;
assign data_out = data_reg;
assign rd_sync_gate = local_gate_reduced;
assign ren_int = ren_in & local_gate_reduced;
always_comb begin
  if (sync_group[0] == 2'h1) begin
    sync_agg[0][0] = valid_reg[0];
  end
  else sync_agg[0][0] = 1'h1;
  if (sync_group[1] == 2'h1) begin
    sync_agg[0][1] = valid_reg[1];
  end
  else sync_agg[0][1] = 1'h1;
  if (sync_group[0] == 2'h2) begin
    sync_agg[1][0] = valid_reg[0];
  end
  else sync_agg[1][0] = 1'h1;
  if (sync_group[1] == 2'h2) begin
    sync_agg[1][1] = valid_reg[1];
  end
  else sync_agg[1][1] = 1'h1;
end
always_comb begin
  sync_valid[0] = &sync_agg[0];
  sync_valid[1] = &sync_agg[1];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    data_reg[0] <= 16'h0;
    valid_reg[0] <= 1'h0;
  end
  else if (|(sync_valid & sync_group[0])) begin
    data_reg[0] <= data_in[0];
    valid_reg[0] <= valid_in[0];
  end
  else if (~valid_reg[0]) begin
    data_reg[0] <= data_in[0];
    valid_reg[0] <= valid_in[0];
  end
  if (~rst_n) begin
    data_reg[1] <= 16'h0;
    valid_reg[1] <= 1'h0;
  end
  else if (|(sync_valid & sync_group[1])) begin
    data_reg[1] <= data_in[1];
    valid_reg[1] <= valid_in[1];
  end
  else if (~valid_reg[1]) begin
    data_reg[1] <= data_in[1];
    valid_reg[1] <= valid_in[1];
  end
end
always_comb begin
  valid_out[0] = |(sync_valid & sync_group[0]);
  valid_out[1] = |(sync_valid & sync_group[1]);
end
always_comb begin
  local_gate_reduced[0] = &local_gate_bus_tpose[0];
  local_gate_reduced[1] = &local_gate_bus_tpose[1];
end

always_ff @(posedge clk, negedge rst_n) begin
  if ((~rst_n) | group_finished[0]) begin
    local_gate_bus[0] <= ~2'h0;
  end
  else local_gate_bus[0] <= local_gate_bus[0] & local_gate_mask[0];
  if ((~rst_n) | group_finished[1]) begin
    local_gate_bus[1] <= ~2'h0;
  end
  else local_gate_bus[1] <= local_gate_bus[1] & local_gate_mask[1];
end
always_comb begin
  local_gate_bus_tpose[0][0] = local_gate_bus[0][0];
  local_gate_bus_tpose[0][1] = local_gate_bus[1][0];
  local_gate_bus_tpose[1][0] = local_gate_bus[0][1];
  local_gate_bus_tpose[1][1] = local_gate_bus[1][1];
end
always_comb begin
  group_finished[0] = &grp_fin_large[0];
  group_finished[1] = &grp_fin_large[1];
end
always_comb begin
  local_gate_mask[0][0] = 1'h1;
  if (sync_group[0] == 2'h1) begin
    local_gate_mask[0][0] = ~(ren_int[0] & ack_in[0]);
  end
  local_gate_mask[0][1] = 1'h1;
  if (sync_group[1] == 2'h1) begin
    local_gate_mask[0][1] = ~(ren_int[1] & ack_in[1]);
  end
  local_gate_mask[1][0] = 1'h1;
  if (sync_group[0] == 2'h2) begin
    local_gate_mask[1][0] = ~(ren_int[0] & ack_in[0]);
  end
  local_gate_mask[1][1] = 1'h1;
  if (sync_group[1] == 2'h2) begin
    local_gate_mask[1][1] = ~(ren_int[1] & ack_in[1]);
  end
end
always_comb begin
  grp_fin_large[0][0] = 1'h1;
  if (sync_group[0] == 2'h1) begin
    grp_fin_large[0][0] = (~local_gate_bus[0][0]) | (~local_gate_mask[0][0]);
  end
  grp_fin_large[0][1] = 1'h1;
  if (sync_group[1] == 2'h1) begin
    grp_fin_large[0][1] = (~local_gate_bus[0][1]) | (~local_gate_mask[0][1]);
  end
  grp_fin_large[1][0] = 1'h1;
  if (sync_group[0] == 2'h2) begin
    grp_fin_large[1][0] = (~local_gate_bus[1][0]) | (~local_gate_mask[1][0]);
  end
  grp_fin_large[1][1] = 1'h1;
  if (sync_group[1] == 2'h2) begin
    grp_fin_large[1][1] = (~local_gate_bus[1][1]) | (~local_gate_mask[1][1]);
  end
end
endmodule   // sync_groups

