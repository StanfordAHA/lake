module Chain (
  input logic [1:0] [15:0] chain_data_in,
  output logic [1:0] [15:0] chain_data_out,
  input logic chain_idx_output,
  input logic [1:0] chain_valid_in,
  output logic [1:0] chain_valid_out,
  input logic clk_en,
  input logic [1:0] [15:0] curr_tile_data_out,
  input logic [1:0] curr_tile_valid_out,
  output logic [1:0] [15:0] data_out_tile,
  input logic enable_chain_output,
  input logic flush,
  output logic [1:0] valid_out_tile
);

logic [1:0][15:0] chain_data_out_inter;
logic [1:0] chain_valid_out_inter;
always_comb begin
  if (enable_chain_output) begin
    data_out_tile = chain_data_out_inter;
  end
  else data_out_tile = curr_tile_data_out;
end
always_comb begin
  if (enable_chain_output) begin
    if (~(chain_idx_output == 1'h0)) begin
      valid_out_tile[0] = 1'h0;
      valid_out_tile[1] = 1'h0;
    end
    else valid_out_tile = chain_valid_out_inter;
  end
  else valid_out_tile = curr_tile_valid_out;
end
always_comb begin
  chain_data_out = chain_data_out_inter;
end
always_comb begin
  if ((chain_idx_output == 1'h0) | (~enable_chain_output)) begin
    chain_valid_out[0] = 1'h0;
    chain_valid_out[1] = 1'h0;
  end
  else chain_valid_out = chain_valid_out_inter;
end
always_comb begin
  if (chain_valid_in[0] == 1'h0) begin
    chain_data_out_inter[0] = curr_tile_data_out[0];
    chain_valid_out_inter[0] = curr_tile_valid_out[0];
  end
  else begin
    chain_data_out_inter[0] = chain_data_in[0];
    chain_valid_out_inter[0] = chain_valid_in[0];
  end
  if (chain_valid_in[1] == 1'h0) begin
    chain_data_out_inter[1] = curr_tile_data_out[1];
    chain_valid_out_inter[1] = curr_tile_valid_out[1];
  end
  else begin
    chain_data_out_inter[1] = chain_data_in[1];
    chain_valid_out_inter[1] = chain_valid_in[1];
  end
end
endmodule   // Chain

module LakeTop (
  input logic [1:0] [15:0] addr_in,
  input logic [1:0] [15:0] chain_data_in,
  output logic [1:0] [15:0] chain_data_out,
  input logic chain_idx_input,
  input logic chain_idx_output,
  input logic [1:0] chain_valid_in,
  output logic [1:0] chain_valid_out,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  output logic [1:0] [31:0] config_data_out,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [1:0] [15:0] data_in,
  output logic [1:0] [15:0] data_out,
  output logic empty,
  input logic enable_chain_input,
  input logic enable_chain_output,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  output logic full,
  input logic [1:0] mode,
  input logic [1:0] ren_in,
  input logic rst_n,
  output logic sram_ready_out,
  input logic [6:0] strg_ub_agg_align_0_line_length,
  input logic [6:0] strg_ub_agg_align_1_line_length,
  input logic [3:0] strg_ub_agg_in_0_in_period,
  input logic [15:0] [1:0] strg_ub_agg_in_0_in_sched,
  input logic [3:0] strg_ub_agg_in_0_out_period,
  input logic [15:0] [1:0] strg_ub_agg_in_0_out_sched,
  input logic [3:0] strg_ub_agg_in_1_in_period,
  input logic [15:0] [1:0] strg_ub_agg_in_1_in_sched,
  input logic [3:0] strg_ub_agg_in_1_out_period,
  input logic [15:0] [1:0] strg_ub_agg_in_1_out_sched,
  input logic [1:0] strg_ub_app_ctrl_coarse_input_port,
  input logic [1:0] strg_ub_app_ctrl_coarse_output_port,
  input logic [1:0] strg_ub_app_ctrl_coarse_prefill,
  input logic [1:0] [15:0] strg_ub_app_ctrl_coarse_read_depth,
  input logic [1:0] [15:0] strg_ub_app_ctrl_coarse_write_depth_ss,
  input logic [1:0] [15:0] strg_ub_app_ctrl_coarse_write_depth_wo,
  input logic [1:0] strg_ub_app_ctrl_input_port,
  input logic [1:0] strg_ub_app_ctrl_output_port,
  input logic [1:0] strg_ub_app_ctrl_prefill,
  input logic [3:0] [15:0] strg_ub_app_ctrl_ranges,
  input logic [1:0] [15:0] strg_ub_app_ctrl_read_depth,
  input logic [3:0] [15:0] strg_ub_app_ctrl_threshold,
  input logic [1:0] [15:0] strg_ub_app_ctrl_write_depth_ss,
  input logic [1:0] [15:0] strg_ub_app_ctrl_write_depth_wo,
  input logic [3:0] strg_ub_input_addr_ctrl_address_gen_0_dimensionality,
  input logic [5:0] [15:0] strg_ub_input_addr_ctrl_address_gen_0_ranges,
  input logic [15:0] strg_ub_input_addr_ctrl_address_gen_0_starting_addr,
  input logic [5:0] [15:0] strg_ub_input_addr_ctrl_address_gen_0_strides,
  input logic [3:0] strg_ub_input_addr_ctrl_address_gen_1_dimensionality,
  input logic [5:0] [15:0] strg_ub_input_addr_ctrl_address_gen_1_ranges,
  input logic [15:0] strg_ub_input_addr_ctrl_address_gen_1_starting_addr,
  input logic [5:0] [15:0] strg_ub_input_addr_ctrl_address_gen_1_strides,
  input logic [3:0] strg_ub_output_addr_ctrl_address_gen_0_dimensionality,
  input logic [5:0] [15:0] strg_ub_output_addr_ctrl_address_gen_0_ranges,
  input logic [15:0] strg_ub_output_addr_ctrl_address_gen_0_starting_addr,
  input logic [5:0] [15:0] strg_ub_output_addr_ctrl_address_gen_0_strides,
  input logic [3:0] strg_ub_output_addr_ctrl_address_gen_1_dimensionality,
  input logic [5:0] [15:0] strg_ub_output_addr_ctrl_address_gen_1_ranges,
  input logic [15:0] strg_ub_output_addr_ctrl_address_gen_1_starting_addr,
  input logic [5:0] [15:0] strg_ub_output_addr_ctrl_address_gen_1_strides,
  input logic [3:0] strg_ub_pre_fetch_0_input_latency,
  input logic [3:0] strg_ub_pre_fetch_1_input_latency,
  input logic [1:0] [1:0] strg_ub_rate_matched,
  input logic [1:0] [1:0] strg_ub_sync_grp_sync_group,
  input logic [1:0] strg_ub_tba_0_tb_0_dimensionality,
  input logic [63:0] [2:0] strg_ub_tba_0_tb_0_indices,
  input logic [5:0] strg_ub_tba_0_tb_0_range_inner,
  input logic [9:0] strg_ub_tba_0_tb_0_range_outer,
  input logic [1:0] strg_ub_tba_0_tb_0_starting_addr,
  input logic [3:0] strg_ub_tba_0_tb_0_stride,
  input logic strg_ub_tba_0_tb_0_tb_height,
  input logic [1:0] strg_ub_tba_1_tb_0_dimensionality,
  input logic [63:0] [2:0] strg_ub_tba_1_tb_0_indices,
  input logic [5:0] strg_ub_tba_1_tb_0_range_inner,
  input logic [9:0] strg_ub_tba_1_tb_0_range_outer,
  input logic [1:0] strg_ub_tba_1_tb_0_starting_addr,
  input logic [3:0] strg_ub_tba_1_tb_0_stride,
  input logic strg_ub_tba_1_tb_0_tb_height,
  input logic tile_en,
  output logic [1:0] valid_out,
  input logic [1:0] wen_in
);

logic [2:0][0:0][9:0] all_addr_to_mem;
logic [2:0][15:0] all_data_out;
logic [2:0][0:0][3:0][15:0] all_data_to_mem;
logic [2:0][0:0] all_ren_to_mem;
logic [2:0] all_valid_out;
logic [2:0][0:0] all_wen_to_mem;
logic [15:0] config_data_in_shrt;
logic [1:0][15:0] config_data_out_shrt;
logic config_seq_clk_en;
logic [1:0][15:0] data_out_tile;
logic [0:0][9:0] fifo_addr_to_mem;
logic fifo_ctrl_clk;
logic [15:0] fifo_ctrl_data_in;
logic fifo_ctrl_pop;
logic fifo_ctrl_push;
logic [15:0] fifo_data_out;
logic [0:0][3:0][15:0] fifo_data_to_mem;
logic fifo_empty;
logic fifo_full;
logic fifo_ren_to_mem;
logic fifo_valid_out;
logic fifo_wen_to_mem;
logic gclk;
logic mem_0_clk;
logic mem_0_clk_en;
logic [9:0] mem_0_mem_addr_in_bank;
logic mem_0_mem_cen_in_bank;
logic [3:0][15:0] mem_0_mem_data_in_bank;
logic [0:0][3:0][15:0] mem_0_mem_data_out_bank;
logic mem_0_mem_wen_in_bank;
logic mem_0_valid_data;
logic [9:0] mem_addr_cfg;
logic [0:0][0:0][9:0] mem_addr_dp;
logic [0:0][0:0][9:0] mem_addr_in;
logic [0:0] mem_cen_dp;
logic [0:0] mem_cen_in;
logic [3:0][15:0] mem_data_cfg;
logic [0:0][0:0][3:0][15:0] mem_data_dp;
logic [0:0][0:0][3:0][15:0] mem_data_in;
logic [0:0][3:0][15:0] mem_data_low_pt;
logic [0:0][0:0][3:0][15:0] mem_data_out;
logic mem_ren_cfg;
logic [0:0] mem_valid_data;
logic mem_wen_cfg;
logic [0:0] mem_wen_dp;
logic [0:0] mem_wen_in;
logic [0:0][9:0] sram_addr_to_mem;
logic sram_ctrl_clk;
logic [15:0] sram_ctrl_data_in;
logic [15:0] sram_ctrl_rd_addr_in;
logic sram_ctrl_ren;
logic sram_ctrl_wen;
logic [15:0] sram_ctrl_wr_addr_in;
logic [15:0] sram_data_out;
logic [0:0][3:0][15:0] sram_data_to_mem;
logic sram_ren_to_mem;
logic sram_valid_out;
logic sram_wen_to_mem;
logic strg_ub_clk;
logic [0:0][0:0][9:0] ub_addr_to_mem;
logic [0:0] ub_cen_to_mem;
logic [1:0][15:0] ub_data_out;
logic [0:0][0:0][3:0][15:0] ub_data_to_mem;
logic [1:0] ub_valid_out;
logic [0:0] ub_wen_to_mem;
logic [1:0] valid_out_tile;
assign config_data_in_shrt = config_data_in[15:0];
assign config_data_out[0] = 32'(config_data_out_shrt[0]);
assign config_data_out[1] = 32'(config_data_out_shrt[1]);
assign gclk = clk & tile_en;
assign mem_data_low_pt[0] = mem_data_out[0][0];
assign config_seq_clk_en = clk_en | (|config_en);
assign mem_wen_in = (|config_en) ? mem_wen_cfg: mem_wen_dp;
assign mem_cen_in = (|config_en) ? mem_wen_cfg | mem_ren_cfg: mem_cen_dp;
assign mem_addr_in[0][0] = (|config_en) ? mem_addr_cfg: mem_addr_dp[0][0];
assign mem_data_in[0][0] = (|config_en) ? mem_data_cfg: mem_data_dp[0][0];
assign strg_ub_clk = gclk;
assign sram_ctrl_clk = gclk;
assign sram_ctrl_wen = wen_in[0];
assign sram_ctrl_ren = ren_in[0];
assign sram_ctrl_data_in = data_in[0];
assign sram_ctrl_wr_addr_in = addr_in[0];
assign sram_ctrl_rd_addr_in = addr_in[0];
assign fifo_ctrl_clk = gclk;
assign fifo_ctrl_data_in = data_in[0];
assign fifo_ctrl_push = wen_in[0];
assign fifo_ctrl_pop = ren_in[0];
assign empty = fifo_empty;
assign full = fifo_full;
assign all_data_to_mem[0] = ub_data_to_mem;
assign all_wen_to_mem[0] = ub_wen_to_mem;
assign all_ren_to_mem[0] = ub_cen_to_mem;
assign all_addr_to_mem[0] = ub_addr_to_mem;
assign all_data_to_mem[1] = fifo_data_to_mem;
assign all_wen_to_mem[1] = fifo_wen_to_mem;
assign all_ren_to_mem[1] = fifo_ren_to_mem;
assign all_addr_to_mem[1][0] = fifo_addr_to_mem[0];
assign all_data_to_mem[2] = sram_data_to_mem;
assign all_wen_to_mem[2] = sram_wen_to_mem;
assign all_ren_to_mem[2] = sram_ren_to_mem;
assign all_addr_to_mem[2][0] = sram_addr_to_mem[0];
assign mem_data_dp = all_data_to_mem[mode];
assign mem_cen_dp = all_ren_to_mem[mode] | all_wen_to_mem[mode];
assign mem_wen_dp = all_wen_to_mem[mode];
assign mem_addr_dp = all_addr_to_mem[mode];
assign mem_0_clk = gclk;
assign mem_0_clk_en = clk_en | (|config_en);
assign mem_0_mem_data_in_bank = mem_data_in[0];
assign mem_data_out[0] = mem_0_mem_data_out_bank;
assign mem_0_mem_addr_in_bank = mem_addr_in[0];
assign mem_0_mem_cen_in_bank = mem_cen_in;
assign mem_0_mem_wen_in_bank = mem_wen_in;
assign mem_valid_data = mem_0_valid_data;
assign all_data_out[0] = ub_data_out[0];
assign all_valid_out[0] = ub_valid_out[0];
assign all_data_out[1] = fifo_data_out;
assign all_valid_out[1] = fifo_valid_out;
assign all_data_out[2] = sram_data_out;
assign all_valid_out[2] = sram_valid_out;
assign data_out_tile[0] = all_data_out[mode];
assign valid_out_tile[0] = all_valid_out[mode];
assign data_out_tile[1] = ub_data_out[1];
assign valid_out_tile[1] = ub_valid_out[1];
storage_config_seq config_seq (
  .addr_out(mem_addr_cfg),
  .clk(gclk),
  .clk_en(config_seq_clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in_shrt),
  .config_en(config_en),
  .config_rd(config_read),
  .config_wr(config_write),
  .flush(flush),
  .rd_data_out(config_data_out_shrt),
  .rd_data_stg(mem_data_low_pt),
  .ren_out(mem_ren_cfg),
  .rst_n(rst_n),
  .wen_out(mem_wen_cfg),
  .wr_data(mem_data_cfg)
);

strg_ub strg_ub (
  .addr_out(ub_addr_to_mem),
  .agg_align_0_line_length(strg_ub_agg_align_0_line_length),
  .agg_align_1_line_length(strg_ub_agg_align_1_line_length),
  .agg_in_0_in_period(strg_ub_agg_in_0_in_period),
  .agg_in_0_in_sched(strg_ub_agg_in_0_in_sched),
  .agg_in_0_out_period(strg_ub_agg_in_0_out_period),
  .agg_in_0_out_sched(strg_ub_agg_in_0_out_sched),
  .agg_in_1_in_period(strg_ub_agg_in_1_in_period),
  .agg_in_1_in_sched(strg_ub_agg_in_1_in_sched),
  .agg_in_1_out_period(strg_ub_agg_in_1_out_period),
  .agg_in_1_out_sched(strg_ub_agg_in_1_out_sched),
  .app_ctrl_coarse_input_port(strg_ub_app_ctrl_coarse_input_port),
  .app_ctrl_coarse_output_port(strg_ub_app_ctrl_coarse_output_port),
  .app_ctrl_coarse_prefill(strg_ub_app_ctrl_coarse_prefill),
  .app_ctrl_coarse_read_depth(strg_ub_app_ctrl_coarse_read_depth),
  .app_ctrl_coarse_write_depth_ss(strg_ub_app_ctrl_coarse_write_depth_ss),
  .app_ctrl_coarse_write_depth_wo(strg_ub_app_ctrl_coarse_write_depth_wo),
  .app_ctrl_input_port(strg_ub_app_ctrl_input_port),
  .app_ctrl_output_port(strg_ub_app_ctrl_output_port),
  .app_ctrl_prefill(strg_ub_app_ctrl_prefill),
  .app_ctrl_ranges(strg_ub_app_ctrl_ranges),
  .app_ctrl_read_depth(strg_ub_app_ctrl_read_depth),
  .app_ctrl_threshold(strg_ub_app_ctrl_threshold),
  .app_ctrl_write_depth_ss(strg_ub_app_ctrl_write_depth_ss),
  .app_ctrl_write_depth_wo(strg_ub_app_ctrl_write_depth_wo),
  .cen_to_strg(ub_cen_to_mem),
  .chain_idx_output(chain_idx_output),
  .clk(strg_ub_clk),
  .clk_en(clk_en),
  .data_from_strg(mem_data_out),
  .data_in(data_in),
  .data_out(ub_data_out),
  .data_to_strg(ub_data_to_mem),
  .enable_chain_output(enable_chain_output),
  .flush(flush),
  .input_addr_ctrl_address_gen_0_dimensionality(strg_ub_input_addr_ctrl_address_gen_0_dimensionality),
  .input_addr_ctrl_address_gen_0_ranges(strg_ub_input_addr_ctrl_address_gen_0_ranges),
  .input_addr_ctrl_address_gen_0_starting_addr(strg_ub_input_addr_ctrl_address_gen_0_starting_addr),
  .input_addr_ctrl_address_gen_0_strides(strg_ub_input_addr_ctrl_address_gen_0_strides),
  .input_addr_ctrl_address_gen_1_dimensionality(strg_ub_input_addr_ctrl_address_gen_1_dimensionality),
  .input_addr_ctrl_address_gen_1_ranges(strg_ub_input_addr_ctrl_address_gen_1_ranges),
  .input_addr_ctrl_address_gen_1_starting_addr(strg_ub_input_addr_ctrl_address_gen_1_starting_addr),
  .input_addr_ctrl_address_gen_1_strides(strg_ub_input_addr_ctrl_address_gen_1_strides),
  .mem_valid_data(mem_valid_data),
  .output_addr_ctrl_address_gen_0_dimensionality(strg_ub_output_addr_ctrl_address_gen_0_dimensionality),
  .output_addr_ctrl_address_gen_0_ranges(strg_ub_output_addr_ctrl_address_gen_0_ranges),
  .output_addr_ctrl_address_gen_0_starting_addr(strg_ub_output_addr_ctrl_address_gen_0_starting_addr),
  .output_addr_ctrl_address_gen_0_strides(strg_ub_output_addr_ctrl_address_gen_0_strides),
  .output_addr_ctrl_address_gen_1_dimensionality(strg_ub_output_addr_ctrl_address_gen_1_dimensionality),
  .output_addr_ctrl_address_gen_1_ranges(strg_ub_output_addr_ctrl_address_gen_1_ranges),
  .output_addr_ctrl_address_gen_1_starting_addr(strg_ub_output_addr_ctrl_address_gen_1_starting_addr),
  .output_addr_ctrl_address_gen_1_strides(strg_ub_output_addr_ctrl_address_gen_1_strides),
  .pre_fetch_0_input_latency(strg_ub_pre_fetch_0_input_latency),
  .pre_fetch_1_input_latency(strg_ub_pre_fetch_1_input_latency),
  .rate_matched(strg_ub_rate_matched),
  .ren_in(ren_in),
  .rst_n(rst_n),
  .sync_grp_sync_group(strg_ub_sync_grp_sync_group),
  .tba_0_tb_0_dimensionality(strg_ub_tba_0_tb_0_dimensionality),
  .tba_0_tb_0_indices(strg_ub_tba_0_tb_0_indices),
  .tba_0_tb_0_range_inner(strg_ub_tba_0_tb_0_range_inner),
  .tba_0_tb_0_range_outer(strg_ub_tba_0_tb_0_range_outer),
  .tba_0_tb_0_starting_addr(strg_ub_tba_0_tb_0_starting_addr),
  .tba_0_tb_0_stride(strg_ub_tba_0_tb_0_stride),
  .tba_0_tb_0_tb_height(strg_ub_tba_0_tb_0_tb_height),
  .tba_1_tb_0_dimensionality(strg_ub_tba_1_tb_0_dimensionality),
  .tba_1_tb_0_indices(strg_ub_tba_1_tb_0_indices),
  .tba_1_tb_0_range_inner(strg_ub_tba_1_tb_0_range_inner),
  .tba_1_tb_0_range_outer(strg_ub_tba_1_tb_0_range_outer),
  .tba_1_tb_0_starting_addr(strg_ub_tba_1_tb_0_starting_addr),
  .tba_1_tb_0_stride(strg_ub_tba_1_tb_0_stride),
  .tba_1_tb_0_tb_height(strg_ub_tba_1_tb_0_tb_height),
  .valid_out(ub_valid_out),
  .wen_in(wen_in),
  .wen_to_strg(ub_wen_to_mem)
);

strg_ram sram_ctrl (
  .addr_out(sram_addr_to_mem),
  .clk(sram_ctrl_clk),
  .clk_en(clk_en),
  .data_from_strg(mem_data_out),
  .data_in(sram_ctrl_data_in),
  .data_out(sram_data_out),
  .data_to_strg(sram_data_to_mem),
  .flush(flush),
  .rd_addr_in(sram_ctrl_rd_addr_in),
  .ready(sram_ready_out),
  .ren(sram_ctrl_ren),
  .ren_to_strg(sram_ren_to_mem),
  .rst_n(rst_n),
  .valid_out(sram_valid_out),
  .wen(sram_ctrl_wen),
  .wen_to_strg(sram_wen_to_mem),
  .wr_addr_in(sram_ctrl_wr_addr_in)
);

strg_fifo fifo_ctrl (
  .addr_out(fifo_addr_to_mem),
  .clk(fifo_ctrl_clk),
  .clk_en(clk_en),
  .data_from_strg(mem_data_out),
  .data_in(fifo_ctrl_data_in),
  .data_out(fifo_data_out),
  .data_to_strg(fifo_data_to_mem),
  .empty(fifo_empty),
  .fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .full(fifo_full),
  .pop(fifo_ctrl_pop),
  .push(fifo_ctrl_push),
  .ren_to_strg(fifo_ren_to_mem),
  .rst_n(rst_n),
  .valid_out(fifo_valid_out),
  .wen_to_strg(fifo_wen_to_mem)
);

TS1N16FFCLLSBLVTC512X32M4S_generator mem_0 (
  .chain_idx_input(chain_idx_input),
  .chain_idx_output(chain_idx_output),
  .clk(mem_0_clk),
  .clk_en(mem_0_clk_en),
  .enable_chain_input(enable_chain_input),
  .enable_chain_output(enable_chain_output),
  .flush(flush),
  .mem_addr_in_bank(mem_0_mem_addr_in_bank),
  .mem_cen_in_bank(mem_0_mem_cen_in_bank),
  .mem_data_in_bank(mem_0_mem_data_in_bank),
  .mem_data_out_bank(mem_0_mem_data_out_bank),
  .mem_wen_in_bank(mem_0_mem_wen_in_bank),
  .rtsel(2'h0),
  .valid_data(mem_0_valid_data),
  .wtsel(2'h0)
);

Chain chain (
  .chain_data_in(chain_data_in),
  .chain_data_out(chain_data_out),
  .chain_idx_output(chain_idx_output),
  .chain_valid_in(chain_valid_in),
  .chain_valid_out(chain_valid_out),
  .clk_en(clk_en),
  .curr_tile_data_out(data_out_tile),
  .curr_tile_valid_out(valid_out_tile),
  .data_out_tile(data_out),
  .enable_chain_output(enable_chain_output),
  .flush(flush),
  .valid_out_tile(valid_out)
);

endmodule   // LakeTop

module TS1N16FFCLLSBLVTC512X32M4S_generator (
  input logic chain_idx_input,
  input logic chain_idx_output,
  input logic clk,
  input logic clk_en,
  input logic enable_chain_input,
  input logic enable_chain_output,
  input logic flush,
  input logic [9:0] mem_addr_in_bank,
  input logic mem_cen_in_bank,
  input logic [3:0] [15:0] mem_data_in_bank,
  output logic [0:0][3:0] [15:0] mem_data_out_bank,
  input logic mem_wen_in_bank,
  input logic [1:0] rtsel,
  output logic valid_data,
  input logic [1:0] wtsel
);

logic chain_idx_tile;
logic [8:0] mem_addr_to_sram;
logic mem_cen_in_bank_chain;
logic mem_wen_in_bank_chain;
always_comb begin
  chain_idx_tile = mem_addr_in_bank[9];
end
always_comb begin
  mem_addr_to_sram = mem_addr_in_bank[8:0];
end
always_comb begin
  if (~enable_chain_input) begin
    mem_wen_in_bank_chain = mem_wen_in_bank;
  end
  else if (mem_wen_in_bank) begin
    if (chain_idx_input == chain_idx_tile) begin
      mem_wen_in_bank_chain = mem_wen_in_bank;
    end
    else mem_wen_in_bank_chain = 1'h0;
  end
  else mem_wen_in_bank_chain = 1'h0;
end
always_comb begin
  if (mem_wen_in_bank) begin
    if (enable_chain_input) begin
      if (chain_idx_input == chain_idx_tile) begin
        mem_cen_in_bank_chain = mem_cen_in_bank;
      end
      else mem_cen_in_bank_chain = 1'h0;
    end
    else mem_cen_in_bank_chain = mem_cen_in_bank;
  end
  else if (enable_chain_output) begin
    if (chain_idx_output == chain_idx_tile) begin
      mem_cen_in_bank_chain = mem_cen_in_bank;
    end
    else mem_cen_in_bank_chain = 1'h0;
  end
  else mem_cen_in_bank_chain = mem_cen_in_bank;
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (~mem_wen_in_bank) begin
      if (enable_chain_output) begin
        if (chain_idx_output == chain_idx_tile) begin
          valid_data <= mem_cen_in_bank;
        end
        else valid_data <= 1'h0;
      end
      else valid_data <= mem_cen_in_bank;
    end
    else valid_data <= 1'h0;
  end
end
sram_stub mem_0 (
  .addr(mem_addr_to_sram),
  .cen(mem_cen_in_bank_chain),
  .clk(clk),
  .clk_en(clk_en),
  .data_in(mem_data_in_bank),
  .data_out(mem_data_out_bank),
  .flush(flush),
  .wen(mem_wen_in_bank_chain)
);

endmodule   // TS1N16FFCLLSBLVTC512X32M4S_generator

module addr_gen_6 (
  output logic [15:0] addr_out,
  input logic clk,
  input logic clk_en,
  input logic [3:0] dimensionality,
  input logic flush,
  input logic [5:0] [15:0] ranges,
  input logic rst_n,
  input logic [15:0] starting_addr,
  input logic step,
  input logic [5:0] [15:0] strides
);

logic [15:0] calc_addr;
logic [5:0][15:0] current_loc;
logic [5:0][15:0] dim_counter;
logic [15:0] strt_addr;
logic [5:0] update;
assign strt_addr = starting_addr;
assign addr_out = calc_addr;
assign update[0] = 1'h1;
assign update[1] = (dim_counter[0] == (ranges[0] - 16'h1)) & update[0];
assign update[2] = (dim_counter[1] == (ranges[1] - 16'h1)) & update[1];
assign update[3] = (dim_counter[2] == (ranges[2] - 16'h1)) & update[2];
assign update[4] = (dim_counter[3] == (ranges[3] - 16'h1)) & update[3];
assign update[5] = (dim_counter[4] == (ranges[4] - 16'h1)) & update[4];
always_comb begin
  calc_addr = ((4'h0 < dimensionality) ? current_loc[0]: 16'h0) + ((4'h1 < dimensionality) ?
      current_loc[1]: 16'h0) + ((4'h2 < dimensionality) ? current_loc[2]: 16'h0) +
      ((4'h3 < dimensionality) ? current_loc[3]: 16'h0) + ((4'h4 < dimensionality) ?
      current_loc[4]: 16'h0) + ((4'h5 < dimensionality) ? current_loc[5]: 16'h0) +
      strt_addr;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter <= 96'h0;
  end
  else if (clk_en) begin
    if (clk_en) begin
      if (flush) begin
        for (int unsigned i = 0; i < 6; i += 1) begin
            dim_counter[3'(i)] <= 16'h0;
          end
      end
      else if (step) begin
        for (int unsigned i = 0; i < 6; i += 1) begin
            if (update[3'(i)] & (4'(i) < dimensionality)) begin
              if (dim_counter[3'(i)] == (ranges[3'(i)] - 16'h1)) begin
                dim_counter[3'(i)] <= 16'h0;
              end
              else dim_counter[3'(i)] <= dim_counter[3'(i)] + 16'h1;
            end
          end
      end
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    current_loc <= 96'h0;
  end
  else if (clk_en) begin
    if (clk_en) begin
      if (flush) begin
        for (int unsigned i = 0; i < 6; i += 1) begin
            current_loc[3'(i)] <= 16'h0;
          end
      end
      else if (step) begin
        for (int unsigned i = 0; i < 6; i += 1) begin
            if (update[3'(i)] & (4'(i) < dimensionality)) begin
              if (dim_counter[3'(i)] == (ranges[3'(i)] - 16'h1)) begin
                current_loc[3'(i)] <= 16'h0;
              end
              else current_loc[3'(i)] <= current_loc[3'(i)] + strides[3'(i)];
            end
          end
      end
    end
  end
end
endmodule   // addr_gen_6

module agg_aligner (
  output logic align,
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [15:0] in_dat,
  input logic in_valid,
  input logic [6:0] line_length,
  output logic [15:0] out_dat,
  output logic out_valid,
  input logic rst_n
);

logic [6:0] cnt;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    cnt <= 7'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      cnt <= 7'h0;
    end
    else if (in_valid) begin
      if ((line_length - 7'h1) == cnt) begin
        cnt <= 7'h0;
      end
      else cnt <= cnt + 7'h1;
    end
  end
end
always_comb begin
  align = in_valid & ((line_length - 7'h1) == cnt);
end
assign out_dat = in_dat;
assign out_valid = in_valid;
endmodule   // agg_aligner

module aggregation_buffer (
  input logic align,
  input logic clk,
  input logic clk_en,
  input logic [15:0] data_in,
  output logic [63:0] data_out,
  input logic flush,
  input logic [3:0] in_period,
  input logic [15:0] [1:0] in_sched,
  input logic [3:0] out_period,
  input logic [15:0] [1:0] out_sched,
  input logic rst_n,
  input logic valid_in,
  output logic valid_out
);

logic agg_0_align;
logic agg_0_next_full;
logic agg_0_valid_in;
logic agg_0_valid_out;
logic agg_1_align;
logic agg_1_next_full;
logic agg_1_valid_in;
logic agg_1_valid_out;
logic agg_2_align;
logic agg_2_next_full;
logic agg_2_valid_in;
logic agg_2_valid_out;
logic agg_3_align;
logic agg_3_next_full;
logic agg_3_valid_in;
logic agg_3_valid_out;
logic [3:0][63:0] aggs_out;
logic [3:0][15:0] aggs_sep_0;
logic [3:0][15:0] aggs_sep_1;
logic [3:0][15:0] aggs_sep_2;
logic [3:0][15:0] aggs_sep_3;
logic [3:0] align_demux;
logic [3:0] in_sched_ptr;
logic [3:0] next_full;
logic [3:0] out_sched_ptr;
logic [3:0] valid_demux;
logic [3:0] valid_out_mux;
assign agg_0_valid_in = valid_demux[0];
assign valid_out_mux[0] = agg_0_valid_out;
assign next_full[0] = agg_0_next_full;
assign agg_0_align = align_demux[0];
assign aggs_out[0] = {aggs_sep_0[3], aggs_sep_0[2], aggs_sep_0[1], aggs_sep_0[0]};
assign agg_1_valid_in = valid_demux[1];
assign valid_out_mux[1] = agg_1_valid_out;
assign next_full[1] = agg_1_next_full;
assign agg_1_align = align_demux[1];
assign aggs_out[1] = {aggs_sep_1[3], aggs_sep_1[2], aggs_sep_1[1], aggs_sep_1[0]};
assign agg_2_valid_in = valid_demux[2];
assign valid_out_mux[2] = agg_2_valid_out;
assign next_full[2] = agg_2_next_full;
assign agg_2_align = align_demux[2];
assign aggs_out[2] = {aggs_sep_2[3], aggs_sep_2[2], aggs_sep_2[1], aggs_sep_2[0]};
assign agg_3_valid_in = valid_demux[3];
assign valid_out_mux[3] = agg_3_valid_out;
assign next_full[3] = agg_3_next_full;
assign agg_3_align = align_demux[3];
assign aggs_out[3] = {aggs_sep_3[3], aggs_sep_3[2], aggs_sep_3[1], aggs_sep_3[0]};

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    in_sched_ptr <= 4'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      in_sched_ptr <= 4'h0;
    end
    else if (next_full[in_sched[in_sched_ptr]]) begin
      in_sched_ptr <= ((in_period - 4'h1) == in_sched_ptr) ? 4'h0: in_sched_ptr + 4'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    out_sched_ptr <= 4'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      out_sched_ptr <= 4'h0;
    end
    else if (valid_out) begin
      out_sched_ptr <= ((out_period - 4'h1) == out_sched_ptr) ? 4'h0: out_sched_ptr + 4'h1;
    end
  end
end
always_comb begin
  valid_demux = 4'h0;
  valid_demux[in_sched[in_sched_ptr]] = valid_in;
end
always_comb begin
  align_demux = 4'h0;
  align_demux[in_sched[in_sched_ptr]] = align;
end
always_comb begin
  valid_out = valid_out_mux[out_sched[out_sched_ptr]];
end
always_comb begin
  data_out = aggs_out[out_sched[out_sched_ptr]];
end
aggregator agg_0 (
  .agg_out(aggs_sep_0),
  .align(agg_0_align),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .in_pixels(data_in),
  .next_full(agg_0_next_full),
  .rst_n(rst_n),
  .valid_in(agg_0_valid_in),
  .valid_out(agg_0_valid_out)
);

aggregator agg_1 (
  .agg_out(aggs_sep_1),
  .align(agg_1_align),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .in_pixels(data_in),
  .next_full(agg_1_next_full),
  .rst_n(rst_n),
  .valid_in(agg_1_valid_in),
  .valid_out(agg_1_valid_out)
);

aggregator agg_2 (
  .agg_out(aggs_sep_2),
  .align(agg_2_align),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .in_pixels(data_in),
  .next_full(agg_2_next_full),
  .rst_n(rst_n),
  .valid_in(agg_2_valid_in),
  .valid_out(agg_2_valid_out)
);

aggregator agg_3 (
  .agg_out(aggs_sep_3),
  .align(agg_3_align),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .in_pixels(data_in),
  .next_full(agg_3_next_full),
  .rst_n(rst_n),
  .valid_in(agg_3_valid_in),
  .valid_out(agg_3_valid_out)
);

endmodule   // aggregation_buffer

module aggregator (
  output logic [3:0] [15:0] agg_out,
  input logic align,
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [15:0] in_pixels,
  output logic next_full,
  input logic rst_n,
  input logic valid_in,
  output logic valid_out
);

logic [3:0][15:0] shift_reg;
logic [1:0] word_count;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    valid_out <= 1'h0;
    word_count <= 2'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      valid_out <= 1'h0;
      word_count <= 2'h0;
    end
    else if (valid_in) begin
      if ((2'h3 == word_count) | align) begin
        valid_out <= 1'h1;
        word_count <= 2'h0;
      end
      else begin
        valid_out <= 1'h0;
        word_count <= word_count + 2'h1;
      end
    end
    else valid_out <= 1'h0;
  end
end
always_comb begin
  next_full = (valid_in & (2'h3 == word_count)) | align;
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (valid_in) begin
      shift_reg[word_count] <= in_pixels;
    end
  end
end
always_comb begin
  agg_out = shift_reg;
end
endmodule   // aggregator

module app_ctrl (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [1:0] input_port,
  input logic [1:0] output_port,
  input logic [1:0] prefill,
  input logic [3:0] [15:0] ranges,
  input logic [1:0] [15:0] read_depth,
  input logic [1:0] ren_in,
  output logic [1:0] ren_out,
  input logic [1:0] ren_update,
  input logic rst_n,
  input logic [1:0] tb_valid,
  input logic [3:0] [15:0] threshold,
  output logic [1:0] valid_out_data,
  output logic [1:0] valid_out_stencil,
  input logic [1:0] wen_in,
  output logic [1:0] wen_out,
  input logic [1:0] [15:0] write_depth_ss,
  input logic [1:0] [15:0] write_depth_wo
);

logic [3:0][15:0] dim_counter;
logic [1:0][15:0] read_count;
logic [1:0] read_done;
logic [1:0] read_done_ff;
logic [1:0] read_on;
logic [3:0] update;
logic [1:0] wr_delay_state_n;
logic [1:0][15:0] write_count;
logic [1:0][15:0] write_depth;
logic [1:0] write_done;
logic [1:0] write_done_ff;
assign update[0] = 1'h1;
assign update[1] = (dim_counter[0] == (ranges[0] - 16'h1)) & update[0];
assign update[2] = (dim_counter[1] == (ranges[1] - 16'h1)) & update[1];
assign update[3] = (dim_counter[2] == (ranges[2] - 16'h1)) & update[2];

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    dim_counter[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      dim_counter[0] <= 16'h0;
    end
    else if (ren_in[0] & ren_update[0]) begin
      if (update[0]) begin
        if (dim_counter[0] == (ranges[0] - 16'h1)) begin
          dim_counter[0] <= 16'h0;
        end
        else dim_counter[0] <= dim_counter[0] + 16'h1;
      end
    end
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
    else if (ren_in[0] & ren_update[0]) begin
      if (update[1]) begin
        if (dim_counter[1] == (ranges[1] - 16'h1)) begin
          dim_counter[1] <= 16'h0;
        end
        else dim_counter[1] <= dim_counter[1] + 16'h1;
      end
    end
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
    else if (ren_in[0] & ren_update[0]) begin
      if (update[2]) begin
        if (dim_counter[2] == (ranges[2] - 16'h1)) begin
          dim_counter[2] <= 16'h0;
        end
        else dim_counter[2] <= dim_counter[2] + 16'h1;
      end
    end
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
    else if (ren_in[0] & ren_update[0]) begin
      if (update[3]) begin
        if (dim_counter[3] == (ranges[3] - 16'h1)) begin
          dim_counter[3] <= 16'h0;
        end
        else dim_counter[3] <= dim_counter[3] + 16'h1;
      end
    end
  end
end
assign valid_out_stencil[0] = &{dim_counter[0] >= threshold[0], dim_counter[1] >= threshold[1], dim_counter[2]
    >= threshold[2], dim_counter[3] >= threshold[3]};
assign valid_out_stencil[1] = &{dim_counter[0] >= threshold[0], dim_counter[1] >= threshold[1], dim_counter[2]
    >= threshold[2], dim_counter[3] >= threshold[3]};
assign valid_out_data = tb_valid & valid_out_stencil;
assign write_depth[0] = wr_delay_state_n[0] ? write_depth_ss[0]: write_depth_wo[0];
assign write_depth[1] = wr_delay_state_n[1] ? write_depth_ss[1]: write_depth_wo[1];
always_comb begin
  read_done[0] = (ren_update[0] & ren_in[0] & (read_count[0] == (read_depth[0] - 16'h1))) |
      read_done_ff[0] | ((~prefill[0]) & (~wr_delay_state_n[0]));
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_done_ff[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_done_ff[0] <= 1'h0;
    end
    else if (write_done[input_port[0]] & read_done[0]) begin
      read_done_ff[0] <= 1'h0;
    end
    else if (read_done[0]) begin
      read_done_ff[0] <= 1'h1;
    end
  end
end
always_comb begin
  read_done[1] = (ren_update[1] & ren_in[1] & (read_count[1] == (read_depth[1] - 16'h1))) |
      read_done_ff[1] | ((~prefill[1]) & (~wr_delay_state_n[1]));
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_done_ff[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_done_ff[1] <= 1'h0;
    end
    else if (write_done[input_port[1]] & read_done[1]) begin
      read_done_ff[1] <= 1'h0;
    end
    else if (read_done[1]) begin
      read_done_ff[1] <= 1'h1;
    end
  end
end
always_comb begin
  write_done[0] = (wen_in[0] & (write_count[0] == (write_depth[0] - 16'h1))) | write_done_ff[0];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_done_ff[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_done_ff[0] <= 1'h0;
    end
    else if (write_done[0] & read_done[output_port[0]]) begin
      write_done_ff[0] <= 1'h0;
    end
    else if (write_done[0]) begin
      write_done_ff[0] <= 1'h1;
    end
  end
end
always_comb begin
  write_done[1] = (wen_in[1] & (write_count[1] == (write_depth[1] - 16'h1))) | write_done_ff[1];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_done_ff[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_done_ff[1] <= 1'h0;
    end
    else if (write_done[1] & read_done[output_port[1]]) begin
      write_done_ff[1] <= 1'h0;
    end
    else if (write_done[1]) begin
      write_done_ff[1] <= 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_count[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_count[0] <= 16'h0;
    end
    else if (write_done[0] & read_done[output_port[0]]) begin
      write_count[0] <= 16'h0;
    end
    else if (wen_in[0] & (~write_done_ff[0])) begin
      write_count[0] <= write_count[0] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_count[1] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_count[1] <= 16'h0;
    end
    else if (write_done[1] & read_done[output_port[1]]) begin
      write_count[1] <= 16'h0;
    end
    else if (wen_in[1] & (~write_done_ff[1])) begin
      write_count[1] <= write_count[1] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_count[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_count[0] <= 16'h0;
    end
    else if (write_done[input_port[0]] & read_done[0]) begin
      read_count[0] <= 16'h0;
    end
    else if (ren_in[0] & ren_update[0]) begin
      read_count[0] <= read_count[0] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_count[1] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_count[1] <= 16'h0;
    end
    else if (write_done[input_port[1]] & read_done[1]) begin
      read_count[1] <= 16'h0;
    end
    else if (ren_in[1] & ren_update[1]) begin
      read_count[1] <= read_count[1] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_delay_state_n[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wr_delay_state_n[0] <= 1'h0;
    end
    else if (write_done[input_port[0]]) begin
      wr_delay_state_n[0] <= 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_delay_state_n[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wr_delay_state_n[1] <= 1'h0;
    end
    else if (write_done[input_port[1]]) begin
      wr_delay_state_n[1] <= 1'h1;
    end
  end
end
assign read_on[0] = |read_depth[0];
assign read_on[1] = |read_depth[1];
assign ren_out = (wr_delay_state_n | prefill) & (~read_done_ff) & ren_in & read_on;
assign wen_out = (~write_done_ff) & wen_in;
endmodule   // app_ctrl

module app_ctrl_unq0 (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [1:0] input_port,
  input logic [1:0] output_port,
  input logic [1:0] prefill,
  input logic [1:0] [15:0] read_depth,
  input logic [1:0] ren_in,
  output logic [1:0] ren_out,
  input logic [1:0] ren_update,
  input logic rst_n,
  input logic [1:0] tb_valid,
  output logic [1:0] valid_out_data,
  output logic [1:0] valid_out_stencil,
  input logic [1:0] wen_in,
  output logic [1:0] wen_out,
  input logic [1:0] [15:0] write_depth_ss,
  input logic [1:0] [15:0] write_depth_wo
);

logic [1:0][15:0] read_count;
logic [1:0] read_done;
logic [1:0] read_done_ff;
logic [1:0] read_on;
logic [1:0] wr_delay_state_n;
logic [1:0][15:0] write_count;
logic [1:0][15:0] write_depth;
logic [1:0] write_done;
logic [1:0] write_done_ff;
assign valid_out_stencil = tb_valid;
assign valid_out_data = tb_valid & valid_out_stencil;
assign write_depth[0] = wr_delay_state_n[0] ? write_depth_ss[0]: write_depth_wo[0];
assign write_depth[1] = wr_delay_state_n[1] ? write_depth_ss[1]: write_depth_wo[1];
always_comb begin
  read_done[0] = (ren_update[0] & ren_in[0] & (read_count[0] == (read_depth[0] - 16'h1))) |
      read_done_ff[0] | ((~prefill[0]) & (~wr_delay_state_n[0]));
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_done_ff[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_done_ff[0] <= 1'h0;
    end
    else if (write_done[input_port[0]] & read_done[0]) begin
      read_done_ff[0] <= 1'h0;
    end
    else if (read_done[0]) begin
      read_done_ff[0] <= 1'h1;
    end
  end
end
always_comb begin
  read_done[1] = (ren_update[1] & ren_in[1] & (read_count[1] == (read_depth[1] - 16'h1))) |
      read_done_ff[1] | ((~prefill[1]) & (~wr_delay_state_n[1]));
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_done_ff[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_done_ff[1] <= 1'h0;
    end
    else if (write_done[input_port[1]] & read_done[1]) begin
      read_done_ff[1] <= 1'h0;
    end
    else if (read_done[1]) begin
      read_done_ff[1] <= 1'h1;
    end
  end
end
always_comb begin
  write_done[0] = (wen_in[0] & (write_count[0] == (write_depth[0] - 16'h1))) | write_done_ff[0];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_done_ff[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_done_ff[0] <= 1'h0;
    end
    else if (write_done[0] & read_done[output_port[0]]) begin
      write_done_ff[0] <= 1'h0;
    end
    else if (write_done[0]) begin
      write_done_ff[0] <= 1'h1;
    end
  end
end
always_comb begin
  write_done[1] = (wen_in[1] & (write_count[1] == (write_depth[1] - 16'h1))) | write_done_ff[1];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_done_ff[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_done_ff[1] <= 1'h0;
    end
    else if (write_done[1] & read_done[output_port[1]]) begin
      write_done_ff[1] <= 1'h0;
    end
    else if (write_done[1]) begin
      write_done_ff[1] <= 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_count[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_count[0] <= 16'h0;
    end
    else if (write_done[0] & read_done[output_port[0]]) begin
      write_count[0] <= 16'h0;
    end
    else if (wen_in[0] & (~write_done_ff[0])) begin
      write_count[0] <= write_count[0] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_count[1] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_count[1] <= 16'h0;
    end
    else if (write_done[1] & read_done[output_port[1]]) begin
      write_count[1] <= 16'h0;
    end
    else if (wen_in[1] & (~write_done_ff[1])) begin
      write_count[1] <= write_count[1] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_count[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_count[0] <= 16'h0;
    end
    else if (write_done[input_port[0]] & read_done[0]) begin
      read_count[0] <= 16'h0;
    end
    else if (ren_in[0] & ren_update[0]) begin
      read_count[0] <= read_count[0] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_count[1] <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_count[1] <= 16'h0;
    end
    else if (write_done[input_port[1]] & read_done[1]) begin
      read_count[1] <= 16'h0;
    end
    else if (ren_in[1] & ren_update[1]) begin
      read_count[1] <= read_count[1] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_delay_state_n[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wr_delay_state_n[0] <= 1'h0;
    end
    else if (write_done[input_port[0]]) begin
      wr_delay_state_n[0] <= 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_delay_state_n[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wr_delay_state_n[1] <= 1'h0;
    end
    else if (write_done[input_port[1]]) begin
      wr_delay_state_n[1] <= 1'h1;
    end
  end
end
assign read_on[0] = |read_depth[0];
assign read_on[1] = |read_depth[1];
assign ren_out = (wr_delay_state_n | prefill) & (~read_done_ff) & ren_in & read_on;
assign wen_out = (~write_done_ff) & wen_in;
endmodule   // app_ctrl_unq0

module demux_reads (
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_in,
  output logic [1:0][3:0] [15:0] data_out,
  input logic flush,
  input logic mem_valid_data,
  output logic [1:0] mem_valid_data_out,
  input logic [0:0] [1:0] port_in,
  input logic rst_n,
  input logic valid_in,
  output logic [1:0] valid_out
);

logic [1:0] done;
always_comb begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      valid_out[1'(i)] = 1'h0;
      data_out[1'(i)] = 64'h0;
      done[1'(i)] = 1'h0;
      mem_valid_data_out[1'(i)] = 1'h0;
      if (~done[1'(i)]) begin
        if (valid_in & port_in[0][1'(i)]) begin
          valid_out[1'(i)] = 1'h1;
          data_out[1'(i)] = data_in[0];
          mem_valid_data_out[1'(i)] = mem_valid_data;
          done[1'(i)] = 1'h1;
        end
      end
    end
end
endmodule   // demux_reads

module input_addr_ctrl (
  output logic [0:0][0:0] [9:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [5:0] [15:0] address_gen_0_ranges,
  input logic [15:0] address_gen_0_starting_addr,
  input logic [5:0] [15:0] address_gen_0_strides,
  input logic [3:0] address_gen_1_dimensionality,
  input logic [5:0] [15:0] address_gen_1_ranges,
  input logic [15:0] address_gen_1_starting_addr,
  input logic [5:0] [15:0] address_gen_1_strides,
  input logic clk,
  input logic clk_en,
  input logic [1:0][3:0] [15:0] data_in,
  output logic [0:0][0:0][3:0] [15:0] data_out,
  input logic flush,
  output logic [1:0] port_out,
  input logic rst_n,
  input logic [1:0] valid_in,
  input logic [1:0] wen_en,
  output logic [0:0] wen_to_sram
);

logic [15:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [15:0] address_gen_1_addr_out;
logic address_gen_1_step;
logic counter;
logic [1:0][3:0][15:0] data_in_saved;
logic [0:0] done;
logic [1:0][0:0][9:0] local_addrs;
logic [1:0][0:0][9:0] local_addrs_saved;
logic [0:0][1:0] port_out_exp;
logic [1:0] wen_en_saved;
logic [1:0][0:0] wen_full;
logic [1:0] wen_reduced;
logic [1:0] wen_reduced_saved;
assign port_out[0] = |port_out_exp[0][0];
assign port_out[1] = |port_out_exp[0][1];
assign wen_reduced[0] = |wen_full[0];
assign wen_reduced[1] = |wen_full[1];
always_comb begin
  wen_full[0] = 1'h0;
  if (valid_in[0]) begin
    wen_full[0] = 1'h1;
  end
  wen_full[1] = 1'h0;
  if (valid_in[1]) begin
    wen_full[1] = 1'h1;
  end
end
always_comb begin
  wen_to_sram = 1'h0;
  done = 1'h0;
  port_out_exp[0] = 2'h0;
  data_out[0][0] = 64'h0;
  addr_out[0][0] = 10'h0;
  if (~done) begin
    if (wen_reduced[0]) begin
      done = 1'h1;
      wen_to_sram = wen_en[0];
      port_out_exp[0][0] = 1'h1;
      data_out[0][0] = data_in[0];
      addr_out[0][0] = local_addrs[0][0][9:0];
    end
    else if (wen_reduced_saved[counter + 1'h1]) begin
      done = 1'h1;
      wen_to_sram = wen_en_saved[counter + 1'h1];
      port_out_exp[0][counter + 1'h1] = 1'h1;
      data_out[0][0] = data_in_saved[counter + 1'h1];
      addr_out[0][0] = local_addrs_saved[counter + 1'h1][0][9:0];
    end
  end
end
assign address_gen_0_step = valid_in[0];
assign address_gen_1_step = valid_in[1];
always_comb begin
  local_addrs[0][0] = address_gen_0_addr_out[9:0];
  local_addrs[1][0] = address_gen_1_addr_out[9:0];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    counter <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      counter <= 1'h0;
    end
    else if (counter == 1'h0) begin
      counter <= 1'h0;
    end
    else if (|wen_en_saved) begin
      counter <= counter + 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wen_en_saved <= 2'h0;
    wen_reduced_saved <= 2'h0;
    data_in_saved <= 128'h0;
    local_addrs_saved <= 20'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wen_en_saved <= 2'h0;
      wen_reduced_saved <= 2'h0;
      data_in_saved <= 128'h0;
      local_addrs_saved <= 20'h0;
    end
    else if (|wen_en) begin
      wen_en_saved <= wen_en;
      wen_reduced_saved <= wen_reduced;
      data_in_saved <= data_in;
      local_addrs_saved <= local_addrs;
    end
    else if (counter == 1'h0) begin
      wen_en_saved <= 2'h0;
      wen_reduced_saved <= 2'h0;
      data_in_saved <= 128'h0;
      local_addrs_saved <= 20'h0;
    end
  end
end
addr_gen_6 address_gen_0 (
  .addr_out(address_gen_0_addr_out),
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(address_gen_0_dimensionality),
  .flush(flush),
  .ranges(address_gen_0_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_0_starting_addr),
  .step(address_gen_0_step),
  .strides(address_gen_0_strides)
);

addr_gen_6 address_gen_1 (
  .addr_out(address_gen_1_addr_out),
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(address_gen_1_dimensionality),
  .flush(flush),
  .ranges(address_gen_1_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_1_starting_addr),
  .step(address_gen_1_step),
  .strides(address_gen_1_strides)
);

endmodule   // input_addr_ctrl

module output_addr_ctrl (
  output logic [1:0] [9:0] addr_out,
  input logic [3:0] address_gen_0_dimensionality,
  input logic [5:0] [15:0] address_gen_0_ranges,
  input logic [15:0] address_gen_0_starting_addr,
  input logic [5:0] [15:0] address_gen_0_strides,
  input logic [3:0] address_gen_1_dimensionality,
  input logic [5:0] [15:0] address_gen_1_ranges,
  input logic [15:0] address_gen_1_starting_addr,
  input logic [5:0] [15:0] address_gen_1_strides,
  input logic clk,
  input logic clk_en,
  input logic flush,
  output logic [0:0] [1:0] ren,
  input logic rst_n,
  input logic [1:0] step_in,
  input logic [1:0] valid_in
);

logic [15:0] address_gen_0_addr_out;
logic address_gen_0_step;
logic [15:0] address_gen_1_addr_out;
logic address_gen_1_step;
logic [1:0][9:0] local_addrs;
always_comb begin
  ren = 2'h0;
  for (int unsigned i = 0; i < 2; i += 1) begin
      if (valid_in[1'(i)]) begin
        ren[0][1'(i)] = 1'h1;
      end
    end
end
always_comb begin
  addr_out = 20'h0;
  for (int unsigned i = 0; i < 2; i += 1) begin
      addr_out[1'(i)] = local_addrs[1'(i)][9:0];
    end
end
assign address_gen_0_step = step_in[0] & valid_in[0];
assign local_addrs[0] = address_gen_0_addr_out[9:0];
assign address_gen_1_step = step_in[1] & valid_in[1];
assign local_addrs[1] = address_gen_1_addr_out[9:0];
addr_gen_6 address_gen_0 (
  .addr_out(address_gen_0_addr_out),
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(address_gen_0_dimensionality),
  .flush(flush),
  .ranges(address_gen_0_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_0_starting_addr),
  .step(address_gen_0_step),
  .strides(address_gen_0_strides)
);

addr_gen_6 address_gen_1 (
  .addr_out(address_gen_1_addr_out),
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(address_gen_1_dimensionality),
  .flush(flush),
  .ranges(address_gen_1_ranges),
  .rst_n(rst_n),
  .starting_addr(address_gen_1_starting_addr),
  .step(address_gen_1_step),
  .strides(address_gen_1_strides)
);

endmodule   // output_addr_ctrl

module prefetcher (
  input logic clk,
  input logic clk_en,
  input logic [3:0] [15:0] data_in,
  output logic [3:0] [15:0] data_out,
  input logic flush,
  input logic [3:0] input_latency,
  input logic mem_valid_data,
  output logic mem_valid_data_out,
  output logic prefetch_step,
  input logic rst_n,
  input logic tba_rdy_in,
  output logic valid_out,
  input logic valid_read
);

logic [3:0] cnt;
logic fifo_empty;
logic fifo_full;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    cnt <= 4'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      cnt <= 4'h0;
    end
    else if (valid_read & (~tba_rdy_in) & (~fifo_full)) begin
      cnt <= cnt + 4'h1;
    end
    else if ((~valid_read) & tba_rdy_in & (~fifo_empty)) begin
      cnt <= cnt - 4'h1;
    end
  end
end
always_comb begin
  prefetch_step = (cnt + input_latency) < 4'h8;
end
reg_fifo_d8_w4 #(
  .data_width(16'h10)) fifo (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(data_in),
  .data_out(data_out),
  .empty(fifo_empty),
  .flush(flush),
  .full(fifo_full),
  .mem_valid_data(mem_valid_data),
  .mem_valid_data_out(mem_valid_data_out),
  .pop(tba_rdy_in),
  .push(valid_read),
  .rst_n(rst_n),
  .valid(valid_out)
);

endmodule   // prefetcher

module reg_fifo_d4_w1 #(parameter data_width = 16'h10)
(
  input logic clk,
  input logic clk_en,
  input logic [0:0] [data_width-1:0] data_in,
  output logic [0:0] [data_width-1:0] data_out,
  output logic empty,
  input logic flush,
  output logic full,
  input logic mem_valid_data,
  output logic mem_valid_data_out,
  input logic [2:0] num_load,
  input logic [3:0][0:0] [data_width-1:0] parallel_in,
  input logic parallel_load,
  output logic [3:0][0:0] [data_width-1:0] parallel_out,
  input logic parallel_read,
  input logic pop,
  input logic push,
  output logic [1:0] rd_ptr_out,
  input logic rst_n,
  output logic valid
);

logic [3:0] mvd_array;
logic [2:0] num_items;
logic passthru;
logic [1:0] rd_ptr;
logic read;
logic [3:0][0:0][data_width-1:0] reg_array;
logic [1:0] wr_ptr;
logic write;
assign rd_ptr_out = rd_ptr;
assign full = num_items == 3'h4;
assign empty = num_items == 3'h0;
assign read = pop & (~passthru) & (~empty);
assign passthru = pop & push & empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    num_items <= 3'h0;
  end
  else if (flush) begin
    num_items <= 3'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      if (num_load == 3'h0) begin
        num_items <= 3'(push);
      end
      else num_items <= num_load;
    end
    else if (parallel_read) begin
      if (push) begin
        num_items <= 3'h1;
      end
      else num_items <= 3'h0;
    end
    else if (write & (~read)) begin
      num_items <= num_items + 3'h1;
    end
    else if ((~write) & read) begin
      num_items <= num_items - 3'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    reg_array <= 64'h0;
    mvd_array <= 4'h0;
  end
  else if (flush) begin
    reg_array <= 64'h0;
    mvd_array <= 4'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      reg_array <= parallel_in;
      mvd_array <= 4'h0;
    end
    else if (write) begin
      if (parallel_read) begin
        reg_array[0] <= data_in;
        mvd_array[0] <= mem_valid_data;
      end
      else begin
        reg_array[wr_ptr] <= data_in;
        mvd_array[wr_ptr] <= mem_valid_data;
      end
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_ptr <= 2'h0;
  end
  else if (flush) begin
    wr_ptr <= 2'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      wr_ptr <= num_load[1:0];
    end
    else if (parallel_read) begin
      if (push) begin
        wr_ptr <= 2'h1;
      end
      else wr_ptr <= 2'h0;
    end
    else if (write) begin
      wr_ptr <= wr_ptr + 2'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_ptr <= 2'h0;
  end
  else if (flush) begin
    rd_ptr <= 2'h0;
  end
  else if (clk_en) begin
    if (parallel_load | parallel_read) begin
      rd_ptr <= 2'h0;
    end
    else if (read) begin
      rd_ptr <= rd_ptr + 2'h1;
    end
  end
end
assign parallel_out = reg_array;
assign write = push & (~passthru) & ((~full) | pop | parallel_read);
always_comb begin
  if (passthru) begin
    data_out = data_in;
    mem_valid_data_out = mem_valid_data;
  end
  else begin
    data_out = reg_array[rd_ptr];
    mem_valid_data_out = mvd_array[rd_ptr];
  end
end
always_comb begin
  valid = pop & ((~empty) | passthru);
end
endmodule   // reg_fifo_d4_w1

module reg_fifo_d4_w1_unq0 #(parameter data_width = 16'h10)
(
  input logic clk,
  input logic clk_en,
  input logic [0:0] [data_width-1:0] data_in,
  output logic [0:0] [data_width-1:0] data_out,
  output logic empty,
  input logic flush,
  output logic full,
  input logic mem_valid_data,
  output logic mem_valid_data_out,
  input logic [2:0] num_load,
  input logic [3:0][0:0] [data_width-1:0] parallel_in,
  input logic parallel_load,
  output logic [3:0][0:0] [data_width-1:0] parallel_out,
  input logic parallel_read,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic valid
);

logic [3:0] mvd_array;
logic [2:0] num_items;
logic passthru;
logic [1:0] rd_ptr;
logic read;
logic [3:0][0:0][data_width-1:0] reg_array;
logic [1:0] wr_ptr;
logic write;
assign full = num_items == 3'h4;
assign empty = num_items == 3'h0;
assign read = pop & (~passthru) & (~empty);
assign passthru = pop & push & empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    num_items <= 3'h0;
  end
  else if (flush) begin
    num_items <= 3'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      if (num_load == 3'h0) begin
        num_items <= 3'(push);
      end
      else num_items <= num_load;
    end
    else if (parallel_read) begin
      if (push) begin
        num_items <= 3'h1;
      end
      else num_items <= 3'h0;
    end
    else if (write & (~read)) begin
      num_items <= num_items + 3'h1;
    end
    else if ((~write) & read) begin
      num_items <= num_items - 3'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    reg_array <= 64'h0;
    mvd_array <= 4'h0;
  end
  else if (flush) begin
    reg_array <= 64'h0;
    mvd_array <= 4'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      reg_array <= parallel_in;
      mvd_array <= 4'h0;
    end
    else if (write) begin
      if (parallel_read) begin
        reg_array[0] <= data_in;
        mvd_array[0] <= mem_valid_data;
      end
      else begin
        reg_array[wr_ptr] <= data_in;
        mvd_array[wr_ptr] <= mem_valid_data;
      end
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_ptr <= 2'h0;
  end
  else if (flush) begin
    wr_ptr <= 2'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      wr_ptr <= num_load[1:0];
    end
    else if (parallel_read) begin
      if (push) begin
        wr_ptr <= 2'h1;
      end
      else wr_ptr <= 2'h0;
    end
    else if (write) begin
      wr_ptr <= wr_ptr + 2'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_ptr <= 2'h0;
  end
  else if (flush) begin
    rd_ptr <= 2'h0;
  end
  else if (clk_en) begin
    if (parallel_load | parallel_read) begin
      rd_ptr <= 2'h0;
    end
    else if (read) begin
      rd_ptr <= rd_ptr + 2'h1;
    end
  end
end
assign parallel_out = reg_array;
assign write = push & (~passthru) & ((~full) | pop | parallel_read);
always_comb begin
  if (passthru) begin
    data_out = data_in;
    mem_valid_data_out = mem_valid_data;
  end
  else begin
    data_out = reg_array[rd_ptr];
    mem_valid_data_out = mvd_array[rd_ptr];
  end
end
always_comb begin
  valid = pop & ((~empty) | passthru);
end
endmodule   // reg_fifo_d4_w1_unq0

module reg_fifo_d8_w4 #(parameter data_width = 16'h10)
(
  input logic clk,
  input logic clk_en,
  input logic [3:0] [data_width-1:0] data_in,
  output logic [3:0] [data_width-1:0] data_out,
  output logic empty,
  input logic flush,
  output logic full,
  input logic mem_valid_data,
  output logic mem_valid_data_out,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic valid
);

logic [7:0] mvd_array;
logic [3:0] num_items;
logic passthru;
logic [2:0] rd_ptr;
logic read;
logic [7:0][3:0][data_width-1:0] reg_array;
logic [2:0] wr_ptr;
logic write;
assign full = num_items == 4'h8;
assign empty = num_items == 4'h0;
assign read = pop & (~passthru) & (~empty);
assign passthru = pop & push & empty;
assign write = push & (~passthru) & ((~full) | pop);

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    num_items <= 4'h0;
  end
  else if (flush) begin
    num_items <= 4'h0;
  end
  else if (clk_en) begin
    if (write & (~read)) begin
      num_items <= num_items + 4'h1;
    end
    else if ((~write) & read) begin
      num_items <= num_items - 4'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    reg_array <= 512'h0;
    mvd_array <= 8'h0;
  end
  else if (flush) begin
    reg_array <= 512'h0;
    mvd_array <= 8'h0;
  end
  else if (clk_en) begin
    if (write) begin
      reg_array[wr_ptr] <= data_in;
      mvd_array[wr_ptr] <= mem_valid_data;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_ptr <= 3'h0;
  end
  else if (flush) begin
    wr_ptr <= 3'h0;
  end
  else if (clk_en) begin
    if (write) begin
      if (wr_ptr == 3'h7) begin
        wr_ptr <= 3'h0;
      end
      else wr_ptr <= wr_ptr + 3'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_ptr <= 3'h0;
  end
  else if (flush) begin
    rd_ptr <= 3'h0;
  end
  else if (clk_en) begin
    if (read) begin
      rd_ptr <= rd_ptr + 3'h1;
    end
  end
end
always_comb begin
  if (passthru) begin
    data_out = data_in;
    mem_valid_data_out = mem_valid_data;
  end
  else begin
    data_out = reg_array[rd_ptr];
    mem_valid_data_out = mvd_array[rd_ptr];
  end
end
always_comb begin
  valid = pop & ((~empty) | passthru);
end
endmodule   // reg_fifo_d8_w4

module rw_arbiter (
  output logic [0:0] [9:0] addr_to_mem,
  output logic cen_mem,
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_from_mem,
  output logic [0:0][3:0] [15:0] data_to_mem,
  input logic flush,
  input logic mem_valid_data,
  output logic [1:0] out_ack,
  output logic [0:0][3:0] [15:0] out_data,
  output logic out_mem_valid_data,
  output logic [0:0] [1:0] out_port,
  output logic out_valid,
  input logic [1:0] [9:0] rd_addr,
  input logic [1:0] ren_en,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [0:0] [9:0] w_addr,
  input logic [0:0][3:0] [15:0] w_data,
  input logic wen_in,
  output logic wen_mem
);

logic done;
logic [0:0][1:0] next_rd_port;
logic [1:0] next_rd_port_red;
logic [0:0][9:0] rd_addr_sel;
logic [0:0][1:0] rd_port;
logic rd_valid;
logic [1:0] ren_int;
logic wen_int;
assign ren_int = ren_in & ren_en;
assign wen_int = wen_in;
always_comb begin
  next_rd_port[0] = 2'h0;
  rd_addr_sel[0] = 10'h0;
  done = 1'h0;
  for (int unsigned i = 0; i < 2; i += 1) begin
      if (~done) begin
        if (ren_int[1'(i)]) begin
          rd_addr_sel[0] = rd_addr[1'(i)];
          next_rd_port[0][1'(i)] = 1'h1;
          done = 1'h1;
        end
      end
    end
end
assign next_rd_port_red[0] = |next_rd_port[0][0];
assign next_rd_port_red[1] = |next_rd_port[0][1];
assign out_ack = next_rd_port_red & {~wen_int, ~wen_int};
always_comb begin
  cen_mem = wen_int | (|next_rd_port[0]);
  data_to_mem[0] = w_data[0];
  if (wen_int) begin
    addr_to_mem[0] = w_addr[0];
  end
  else addr_to_mem[0] = rd_addr_sel[0];
  wen_mem = wen_int;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_port[0] <= 2'h0;
    rd_valid <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      rd_port[0] <= 2'h0;
      rd_valid <= 1'h0;
    end
    else begin
      rd_valid <= ((~wen_int) | 1'h0) & (|next_rd_port[0]);
      rd_port[0] <= next_rd_port[0];
    end
  end
end
always_comb begin
  out_data = data_from_mem;
  out_port = rd_port;
  out_valid = rd_valid;
  out_mem_valid_data = mem_valid_data;
end
endmodule   // rw_arbiter

module sram_stub (
  input logic [8:0] addr,
  input logic cen,
  input logic clk,
  input logic clk_en,
  input logic [3:0] [15:0] data_in,
  output logic [3:0] [15:0] data_out,
  input logic flush,
  input logic wen
);

logic [511:0][3:0][15:0] data_array;

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (cen & wen) begin
      data_array[addr] <= data_in;
    end
  end
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (cen & (~wen)) begin
      data_out <= data_array[addr];
    end
  end
end
endmodule   // sram_stub

module storage_config_seq (
  output logic [9:0] addr_out,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_rd,
  input logic config_wr,
  input logic flush,
  output logic [1:0] [15:0] rd_data_out,
  input logic [0:0][3:0] [15:0] rd_data_stg,
  output logic ren_out,
  input logic rst_n,
  output logic wen_out,
  output logic [3:0] [15:0] wr_data
);

logic [1:0] cnt;
logic [2:0][15:0] data_wr_reg;
logic [1:0] rd_cnt;
logic [1:0] reduce_en;
logic set_to_addr;
assign reduce_en[0] = |config_en[0];
assign reduce_en[1] = |config_en[1];
always_comb begin
  set_to_addr = 1'h0;
  for (int unsigned i = 0; i < 2; i += 1) begin
      if (reduce_en[1'(i)]) begin
        set_to_addr = 1'(i);
      end
    end
end
assign addr_out = {1'h0, set_to_addr, config_addr_in};

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    cnt <= 2'h0;
  end
  else if (flush) begin
    cnt <= 2'h0;
  end
  else if (clk_en) begin
    if ((config_wr | config_rd) & (|config_en)) begin
      cnt <= cnt + 2'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_cnt <= 2'h0;
  end
  else if (flush) begin
    rd_cnt <= 2'h0;
  end
  else if (clk_en) begin
    rd_cnt <= cnt;
  end
end
assign rd_data_out[0] = rd_data_stg[0][rd_cnt];
assign rd_data_out[1] = rd_data_stg[0][rd_cnt];

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    data_wr_reg <= 48'h0;
  end
  else if (flush) begin
    data_wr_reg <= 48'h0;
  end
  else if (clk_en) begin
    if (config_wr & (cnt < 2'h3)) begin
      data_wr_reg[cnt] <= config_data_in;
    end
  end
end
assign wr_data[0] = data_wr_reg[0];
assign wr_data[1] = data_wr_reg[1];
assign wr_data[2] = data_wr_reg[2];
assign wr_data[3] = config_data_in;
assign wen_out = config_wr & (cnt == 2'h3);
assign ren_out = config_rd;
endmodule   // storage_config_seq

module strg_fifo (
  output logic [0:0] [9:0] addr_out,
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_from_strg,
  input logic [15:0] data_in,
  output logic [15:0] data_out,
  output logic [0:0][3:0] [15:0] data_to_strg,
  output logic empty,
  input logic [15:0] fifo_depth,
  input logic flush,
  output logic full,
  input logic pop,
  input logic push,
  output logic ren_to_strg,
  input logic rst_n,
  output logic valid_out,
  output logic wen_to_strg
);

logic [15:0] back_data_in;
logic [15:0] back_data_out;
logic back_empty;
logic back_full;
logic [2:0] back_num_load;
logic [2:0] back_occ;
logic [3:0][0:0][15:0] back_par_in;
logic [3:0][0:0][15:0] back_parallel_out;
logic back_pl;
logic back_pop;
logic back_push;
logic [0:0][15:0] back_rf_data_in;
logic [0:0][15:0] back_rf_data_out;
logic back_rf_parallel_load;
logic back_valid;
logic curr_bank_rd;
logic curr_bank_wr;
logic [3:0][15:0] front_combined;
logic [15:0] front_data_out;
logic front_empty;
logic front_full;
logic [2:0] front_occ;
logic [3:0][0:0][15:0] front_par_out;
logic front_par_read;
logic front_pop;
logic front_push;
logic [1:0] front_rd_ptr;
logic [0:0][15:0] front_rf_data_in;
logic [0:0][15:0] front_rf_data_out;
logic front_valid;
logic fw_is_1;
logic mem_valid_data_out;
logic mem_valid_data_out1;
logic [15:0] num_items;
logic [15:0] num_words_mem;
logic prev_bank_rd;
logic queued_write;
logic [0:0][9:0] ren_addr;
logic ren_delay;
logic [0:0][9:0] wen_addr;
logic [0:0][3:0][15:0] write_queue;
assign curr_bank_wr = 1'h0;
assign curr_bank_rd = 1'h0;
assign front_push = push & ((~full) | pop);
assign front_rf_data_in[0] = data_in;
assign front_data_out = front_rf_data_out[0];
assign fw_is_1 = 1'h0;
assign back_pop = pop & ((~empty) | push);
assign back_rf_parallel_load = back_pl & (|back_num_load);
assign back_rf_data_in[0] = back_data_in;
assign back_data_out = back_rf_data_out[0];
always_comb begin
  wen_to_strg = ((~ren_to_strg) | 1'h0) & (queued_write | ((front_occ == 3'h4) & push &
      (~front_pop) & (curr_bank_wr == 1'h0)));
end
always_comb begin
  ren_to_strg = ((back_occ == 3'h1) | fw_is_1) & (curr_bank_rd == 1'h0) & (pop | ((back_occ ==
      3'h0) & (back_num_load == 3'h0))) & ((num_words_mem > 16'h1) | ((num_words_mem
      == 16'h1) & (~back_pl)));
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    ren_delay <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      ren_delay <= 1'h0;
    end
    else ren_delay <= |ren_to_strg;
  end
end
assign back_pl = ren_delay;
assign front_combined[0] = front_par_out[front_rd_ptr + 2'h0];
assign front_combined[1] = front_par_out[front_rd_ptr + 2'h1];
assign front_combined[2] = front_par_out[front_rd_ptr + 2'h2];
assign front_combined[3] = front_par_out[front_rd_ptr + 2'h3];
assign data_to_strg[0] = queued_write ? write_queue[0]: front_combined;
assign back_data_in = front_data_out;
assign back_push = front_valid;
always_comb begin
  front_pop = ((num_words_mem == 16'h0) | ((num_words_mem == 16'h1) & back_pl)) & ((~back_pl)
      | (back_pl & (back_num_load == 3'h0))) & ((~(back_occ == 3'h4)) | pop) &
      ((~(front_occ == 3'h0)) | push);
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    write_queue[0] <= 64'h0;
    queued_write <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      write_queue[0] <= 64'h0;
      queued_write <= 1'h0;
    end
    else if (front_par_read & (~wen_to_strg) & (curr_bank_wr == 1'h0)) begin
      write_queue[0] <= front_combined;
      queued_write <= 1'h1;
    end
    else if (wen_to_strg) begin
      queued_write <= 1'h0;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    num_words_mem <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      num_words_mem <= 16'h0;
    end
    else if ((~back_pl) & front_par_read) begin
      num_words_mem <= num_words_mem + 16'h1;
    end
    else if (back_pl & (~front_par_read)) begin
      num_words_mem <= num_words_mem - 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    front_occ <= 3'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      front_occ <= 3'h0;
    end
    else if (front_par_read) begin
      if (front_push) begin
        front_occ <= 3'h1;
      end
      else front_occ <= 3'h0;
    end
    else if (front_push & (~front_pop)) begin
      front_occ <= front_occ + 3'h1;
    end
    else if ((~front_push) & front_pop) begin
      front_occ <= front_occ - 3'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    back_occ <= 3'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      back_occ <= 3'h0;
    end
    else if (back_pl) begin
      if (back_num_load == 3'h0) begin
        back_occ <= 3'(back_push);
      end
      else back_occ <= back_num_load;
    end
    else if (back_push & (~back_pop)) begin
      back_occ <= back_occ + 3'h1;
    end
    else if ((~back_push) & back_pop) begin
      back_occ <= back_occ - 3'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    prev_bank_rd <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      prev_bank_rd <= 1'h0;
    end
    else prev_bank_rd <= curr_bank_rd;
  end
end
assign back_par_in[0] = (back_num_load == 3'h4) ? data_from_strg[prev_bank_rd][0]:
    data_from_strg[prev_bank_rd][1];
assign back_par_in[1] = (back_num_load == 3'h4) ? data_from_strg[prev_bank_rd][1]:
    data_from_strg[prev_bank_rd][2];
assign back_par_in[2] = (back_num_load == 3'h4) ? data_from_strg[prev_bank_rd][2]:
    data_from_strg[prev_bank_rd][3];
assign back_par_in[3] = (back_num_load == 3'h4) ? data_from_strg[prev_bank_rd][3]: 16'h0;
always_comb begin
  front_par_read = (front_occ == 3'h4) & push & (~front_pop);
end
always_comb begin
  if (back_pl) begin
    back_num_load = pop ? 3'h3: 3'h4;
  end
  else back_num_load = 3'h0;
end
assign data_out = back_pl ? data_from_strg[prev_bank_rd][0]: back_data_out;
assign valid_out = back_pl ? pop: back_valid;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wen_addr[0] <= 10'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wen_addr[0] <= 10'h0;
    end
    else if (wen_to_strg) begin
      wen_addr[0] <= wen_addr[0] + 10'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    ren_addr[0] <= 10'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      ren_addr[0] <= 10'h0;
    end
    else if (ren_to_strg) begin
      ren_addr[0] <= ren_addr[0] + 10'h1;
    end
  end
end
assign addr_out[0] = wen_to_strg ? wen_addr[0]: ren_addr[0];
always_comb begin
  num_items = 16'(num_words_mem * 16'h4) + 16'(front_occ) + 16'(back_occ);
end
assign empty = num_items == 16'h0;
assign full = fifo_depth == num_items;
reg_fifo_d4_w1 #(
  .data_width(16'h10)) front_rf (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(front_rf_data_in),
  .data_out(front_rf_data_out),
  .empty(front_empty),
  .flush(flush),
  .full(front_full),
  .mem_valid_data(1'h1),
  .mem_valid_data_out(mem_valid_data_out),
  .num_load(3'h0),
  .parallel_in(64'h0),
  .parallel_load(1'h0),
  .parallel_out(front_par_out),
  .parallel_read(front_par_read),
  .pop(front_pop),
  .push(front_push),
  .rd_ptr_out(front_rd_ptr),
  .rst_n(rst_n),
  .valid(front_valid)
);

reg_fifo_d4_w1_unq0 #(
  .data_width(16'h10)) back_rf (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(back_rf_data_in),
  .data_out(back_rf_data_out),
  .empty(back_empty),
  .flush(flush),
  .full(back_full),
  .mem_valid_data(1'h1),
  .mem_valid_data_out(mem_valid_data_out1),
  .num_load(back_num_load),
  .parallel_in(back_par_in),
  .parallel_out(back_parallel_out),
  .parallel_load(back_rf_parallel_load),
  .parallel_read(1'h0),
  .pop(back_pop),
  .push(back_push),
  .rst_n(rst_n),
  .valid(back_valid)
);

endmodule   // strg_fifo

module strg_ram (
  output logic [0:0] [9:0] addr_out,
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_from_strg,
  input logic [15:0] data_in,
  output logic [15:0] data_out,
  output logic [0:0][3:0] [15:0] data_to_strg,
  input logic flush,
  input logic [15:0] rd_addr_in,
  output logic ready,
  input logic ren,
  output logic ren_to_strg,
  input logic rst_n,
  output logic valid_out,
  input logic wen,
  output logic wen_to_strg,
  input logic [15:0] wr_addr_in
);

typedef enum logic[1:0] {
  IDLE = 2'h0,
  MODIFY = 2'h1,
  READ = 2'h2
} r_w_seq_state;
logic [15:0] addr_to_write;
logic [3:0][15:0] data_combined;
logic [15:0] data_to_write;
r_w_seq_state r_w_seq_current_state;
r_w_seq_state r_w_seq_next_state;
logic [15:0] rd_addr;
logic rd_bank;
logic rd_valid;
logic read_gate;
logic [15:0] wr_addr;
logic write_gate;
assign wr_addr = wr_addr_in;
assign rd_addr = wr_addr_in;
assign rd_bank = 1'h0;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_valid <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      rd_valid <= 1'h0;
    end
    else rd_valid <= ren & (~wen);
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    data_to_write <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      data_to_write <= 16'h0;
    end
    else data_to_write <= data_in;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    addr_to_write <= 16'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      addr_to_write <= 16'h0;
    end
    else addr_to_write <= wr_addr;
  end
end
assign data_to_strg[0] = data_combined;
assign ren_to_strg = (wen | ren) & read_gate;
assign wen_to_strg = write_gate;
always_comb begin
  addr_out[0] = rd_addr[11:2];
  if (wen & (~write_gate)) begin
    addr_out[0] = wr_addr[11:2];
  end
  else if (write_gate) begin
    addr_out[0] = addr_to_write[11:2];
  end
end
always_comb begin
  if (addr_to_write[1:0] == 2'h0) begin
    data_combined[0] = data_to_write;
  end
  else data_combined[0] = data_from_strg[rd_bank][0];
end
always_comb begin
  if (addr_to_write[1:0] == 2'h1) begin
    data_combined[1] = data_to_write;
  end
  else data_combined[1] = data_from_strg[rd_bank][1];
end
always_comb begin
  if (addr_to_write[1:0] == 2'h2) begin
    data_combined[2] = data_to_write;
  end
  else data_combined[2] = data_from_strg[rd_bank][2];
end
always_comb begin
  if (addr_to_write[1:0] == 2'h3) begin
    data_combined[3] = data_to_write;
  end
  else data_combined[3] = data_from_strg[rd_bank][3];
end

always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
    r_w_seq_current_state <= IDLE;
  end
  else r_w_seq_current_state <= r_w_seq_next_state;
end
always_comb begin
  unique case (r_w_seq_current_state)
    IDLE: if ((~wen) & (~ren)) begin
      r_w_seq_next_state = IDLE;
    end
    else if (wen) begin
      r_w_seq_next_state = MODIFY;
    end
    else if (ren & (~wen)) begin
      r_w_seq_next_state = READ;
    end
    else r_w_seq_next_state = IDLE;
    MODIFY: if (1'h1) begin
      r_w_seq_next_state = IDLE;
    end
    else r_w_seq_next_state = MODIFY;
    READ: if ((~wen) & (~ren)) begin
      r_w_seq_next_state = IDLE;
    end
    else if (wen) begin
      r_w_seq_next_state = MODIFY;
    end
    else if (ren & (~wen)) begin
      r_w_seq_next_state = READ;
    end
    else r_w_seq_next_state = READ;
    default: r_w_seq_next_state = r_w_seq_current_state;
  endcase
end
always_comb begin
  unique case (r_w_seq_current_state)
    IDLE: begin :r_w_seq_IDLE_Output
        data_out = 16'h0;
        read_gate = 1'h1;
        ready = 1'h1;
        valid_out = 1'h0;
        write_gate = 1'h0;
      end :r_w_seq_IDLE_Output
    MODIFY: begin :r_w_seq_MODIFY_Output
        data_out = 16'h0;
        read_gate = 1'h0;
        ready = 1'h0;
        valid_out = 1'h0;
        write_gate = 1'h1;
      end :r_w_seq_MODIFY_Output
    READ: begin :r_w_seq_READ_Output
        data_out = data_from_strg[rd_bank][addr_to_write[1:0]];
        read_gate = 1'h1;
        ready = 1'h1;
        valid_out = 1'h1;
        write_gate = 1'h0;
      end :r_w_seq_READ_Output
    default: begin
      data_out = 16'h0;
      read_gate = 1'h1;
      ready = 1'h1;
      valid_out = 1'h0;
      write_gate = 1'h0;
    end
  endcase
end
endmodule   // strg_ram

module strg_ub (
  output logic [0:0][0:0] [9:0] addr_out,
  input logic [6:0] agg_align_0_line_length,
  input logic [6:0] agg_align_1_line_length,
  input logic [3:0] agg_in_0_in_period,
  input logic [15:0] [1:0] agg_in_0_in_sched,
  input logic [3:0] agg_in_0_out_period,
  input logic [15:0] [1:0] agg_in_0_out_sched,
  input logic [3:0] agg_in_1_in_period,
  input logic [15:0] [1:0] agg_in_1_in_sched,
  input logic [3:0] agg_in_1_out_period,
  input logic [15:0] [1:0] agg_in_1_out_sched,
  input logic [1:0] app_ctrl_coarse_input_port,
  input logic [1:0] app_ctrl_coarse_output_port,
  input logic [1:0] app_ctrl_coarse_prefill,
  input logic [1:0] [15:0] app_ctrl_coarse_read_depth,
  input logic [1:0] [15:0] app_ctrl_coarse_write_depth_ss,
  input logic [1:0] [15:0] app_ctrl_coarse_write_depth_wo,
  input logic [1:0] app_ctrl_input_port,
  input logic [1:0] app_ctrl_output_port,
  input logic [1:0] app_ctrl_prefill,
  input logic [3:0] [15:0] app_ctrl_ranges,
  input logic [1:0] [15:0] app_ctrl_read_depth,
  input logic [3:0] [15:0] app_ctrl_threshold,
  input logic [1:0] [15:0] app_ctrl_write_depth_ss,
  input logic [1:0] [15:0] app_ctrl_write_depth_wo,
  output logic [0:0] cen_to_strg,
  input logic chain_idx_output,
  input logic clk,
  input logic clk_en,
  input logic [0:0][0:0][3:0] [15:0] data_from_strg,
  input logic [1:0] [15:0] data_in,
  output logic [1:0] [15:0] data_out,
  output logic [0:0][0:0][3:0] [15:0] data_to_strg,
  input logic enable_chain_output,
  input logic flush,
  input logic [3:0] input_addr_ctrl_address_gen_0_dimensionality,
  input logic [5:0] [15:0] input_addr_ctrl_address_gen_0_ranges,
  input logic [15:0] input_addr_ctrl_address_gen_0_starting_addr,
  input logic [5:0] [15:0] input_addr_ctrl_address_gen_0_strides,
  input logic [3:0] input_addr_ctrl_address_gen_1_dimensionality,
  input logic [5:0] [15:0] input_addr_ctrl_address_gen_1_ranges,
  input logic [15:0] input_addr_ctrl_address_gen_1_starting_addr,
  input logic [5:0] [15:0] input_addr_ctrl_address_gen_1_strides,
  input logic [0:0] mem_valid_data,
  input logic [3:0] output_addr_ctrl_address_gen_0_dimensionality,
  input logic [5:0] [15:0] output_addr_ctrl_address_gen_0_ranges,
  input logic [15:0] output_addr_ctrl_address_gen_0_starting_addr,
  input logic [5:0] [15:0] output_addr_ctrl_address_gen_0_strides,
  input logic [3:0] output_addr_ctrl_address_gen_1_dimensionality,
  input logic [5:0] [15:0] output_addr_ctrl_address_gen_1_ranges,
  input logic [15:0] output_addr_ctrl_address_gen_1_starting_addr,
  input logic [5:0] [15:0] output_addr_ctrl_address_gen_1_strides,
  input logic [3:0] pre_fetch_0_input_latency,
  input logic [3:0] pre_fetch_1_input_latency,
  input logic [1:0] [1:0] rate_matched,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] [1:0] sync_grp_sync_group,
  input logic [1:0] tba_0_tb_0_dimensionality,
  input logic [63:0] [2:0] tba_0_tb_0_indices,
  input logic [5:0] tba_0_tb_0_range_inner,
  input logic [9:0] tba_0_tb_0_range_outer,
  input logic [1:0] tba_0_tb_0_starting_addr,
  input logic [3:0] tba_0_tb_0_stride,
  input logic tba_0_tb_0_tb_height,
  input logic [1:0] tba_1_tb_0_dimensionality,
  input logic [63:0] [2:0] tba_1_tb_0_indices,
  input logic [5:0] tba_1_tb_0_range_inner,
  input logic [9:0] tba_1_tb_0_range_outer,
  input logic [1:0] tba_1_tb_0_starting_addr,
  input logic [3:0] tba_1_tb_0_stride,
  input logic tba_1_tb_0_tb_height,
  output logic [1:0] valid_out,
  input logic [1:0] wen_in,
  output logic [0:0] wen_to_strg
);

logic [1:0][63:0] ab_to_mem_dat;
logic [1:0] ab_to_mem_valid;
logic [1:0] ack_reduced;
logic [1:0] ack_transpose;
logic [0:0][0:0][9:0] addr_to_arb;
logic agg_align_0_align;
logic [15:0] agg_align_0_in_dat;
logic agg_align_0_in_valid;
logic [15:0] agg_align_0_out_dat;
logic agg_align_0_out_valid;
logic agg_align_1_align;
logic [15:0] agg_align_1_in_dat;
logic agg_align_1_in_valid;
logic [15:0] agg_align_1_out_dat;
logic agg_align_1_out_valid;
logic agg_in_0_align;
logic [15:0] agg_in_0_data_in;
logic [63:0] agg_in_0_data_out;
logic agg_in_0_valid_in;
logic agg_in_0_valid_out;
logic agg_in_1_align;
logic [15:0] agg_in_1_data_in;
logic [63:0] agg_in_1_data_out;
logic agg_in_1_valid_in;
logic agg_in_1_valid_out;
logic [1:0] align_input;
logic [0:0][1:0] arb_acks;
logic [0:0][0:0][3:0][15:0] arb_dat_out;
logic [0:0][3:0][15:0] arb_dat_out_f;
logic arb_mem_valid_data_f;
logic [1:0] arb_mem_valid_data_out;
logic [0:0][0:0][1:0] arb_port_out;
logic [0:0][1:0] arb_port_out_f;
logic [1:0] arb_ren_en;
logic [0:0] arb_valid_out;
logic arb_valid_out_f;
logic [1:0] arb_wen_en;
logic [1:0][15:0] data_consume;
logic [0:0][0:0][3:0][15:0] data_to_arb;
logic [1:0][3:0][15:0] data_to_pref;
logic [1:0][3:0][15:0] data_to_sync;
logic [1:0][3:0][15:0] data_to_tba;
logic [1:0] mem_valid_data_pref;
logic [1:0] mem_valid_data_sync;
logic [1:0][9:0] oac_addr_out;
logic [1:0] oac_step;
logic [1:0] oac_valid;
logic [0:0] out_mem_valid_data;
logic [1:0] port_wens;
logic [3:0][15:0] pre_fetch_0_data_in;
logic [3:0][15:0] pre_fetch_0_data_out;
logic pre_fetch_0_mem_valid_data;
logic pre_fetch_0_mem_valid_data_out;
logic pre_fetch_0_prefetch_step;
logic pre_fetch_0_tba_rdy_in;
logic pre_fetch_0_valid_out;
logic pre_fetch_0_valid_read;
logic [3:0][15:0] pre_fetch_1_data_in;
logic [3:0][15:0] pre_fetch_1_data_out;
logic pre_fetch_1_mem_valid_data;
logic pre_fetch_1_mem_valid_data_out;
logic pre_fetch_1_prefetch_step;
logic pre_fetch_1_tba_rdy_in;
logic pre_fetch_1_valid_out;
logic pre_fetch_1_valid_read;
logic [1:0] prefetch_step;
logic [1:0] rd_sync_gate;
logic [1:0] ready_tba;
logic [1:0] ren;
logic [1:0] ren_in_muxed;
logic [0:0][1:0] ren_out;
logic [1:0] ren_out_reduced;
logic [1:0] ren_out_tpose;
logic [0:0][9:0] rw_arb_0_addr_to_mem;
logic rw_arb_0_cen_mem;
logic [0:0][3:0][15:0] rw_arb_0_data_from_mem;
logic [0:0][3:0][15:0] rw_arb_0_data_to_mem;
logic rw_arb_0_mem_valid_data;
logic [1:0] rw_arb_0_out_ack;
logic [0:0][3:0][15:0] rw_arb_0_out_data;
logic rw_arb_0_out_mem_valid_data;
logic [0:0][1:0] rw_arb_0_out_port;
logic rw_arb_0_out_valid;
logic [1:0] rw_arb_0_ren_in;
logic [0:0][9:0] rw_arb_0_w_addr;
logic [0:0][3:0][15:0] rw_arb_0_w_data;
logic rw_arb_0_wen_in;
logic rw_arb_0_wen_mem;
logic [1:0][15:0] tb_data_out;
logic [1:0] tb_valid_out;
logic [3:0][15:0] tba_0_SRAM_to_tb_data;
logic tba_0_ack_in;
logic tba_0_mem_valid_data;
logic tba_0_tb_arbiter_rdy;
logic [15:0] tba_0_tb_to_interconnect_data;
logic tba_0_tb_to_interconnect_valid;
logic tba_0_tba_ren;
logic tba_0_valid_data;
logic [3:0][15:0] tba_1_SRAM_to_tb_data;
logic tba_1_ack_in;
logic tba_1_mem_valid_data;
logic tba_1_tb_arbiter_rdy;
logic [15:0] tba_1_tb_to_interconnect_data;
logic tba_1_tb_to_interconnect_valid;
logic tba_1_tba_ren;
logic tba_1_valid_data;
logic [1:0] valid_consume;
logic [1:0] valid_out_data_coarse;
logic [1:0] valid_out_data_stencil;
logic [1:0] valid_out_stencil;
logic [1:0] valid_to_pref;
logic [1:0] valid_to_sync;
logic [1:0] valid_to_tba;
logic [1:0] wen;
logic [0:0] wen_to_arb;
assign ren_in_muxed[0] = rate_matched[0][0] ? wen_in[rate_matched[0][1]]: ren_in[0];
assign ren_in_muxed[1] = rate_matched[1][0] ? wen_in[rate_matched[1][1]]: ren_in[1];
assign agg_align_0_in_dat = data_in[0];
assign agg_align_0_in_valid = wen[0];
assign align_input[0] = agg_align_0_align;
assign valid_consume[0] = agg_align_0_out_valid;
assign data_consume[0] = agg_align_0_out_dat;
assign agg_align_1_in_dat = data_in[1];
assign agg_align_1_in_valid = wen[1];
assign align_input[1] = agg_align_1_align;
assign valid_consume[1] = agg_align_1_out_valid;
assign data_consume[1] = agg_align_1_out_dat;
assign agg_in_0_data_in = data_consume[0];
assign agg_in_0_valid_in = valid_consume[0];
assign agg_in_0_align = align_input[0];
assign ab_to_mem_dat[0] = agg_in_0_data_out;
assign ab_to_mem_valid[0] = agg_in_0_valid_out;
assign agg_in_1_data_in = data_consume[1];
assign agg_in_1_valid_in = valid_consume[1];
assign agg_in_1_align = align_input[1];
assign ab_to_mem_dat[1] = agg_in_1_data_out;
assign ab_to_mem_valid[1] = agg_in_1_valid_out;
assign oac_valid = prefetch_step;
assign oac_step = ack_reduced;
assign ren_out_tpose[0] = ren_out[0][0];
assign ren_out_tpose[1] = ren_out[0][1];
assign rw_arb_0_wen_in = wen_to_arb;
assign rw_arb_0_w_data = data_to_arb[0];
assign rw_arb_0_w_addr = addr_to_arb[0];
assign rw_arb_0_data_from_mem = data_from_strg[0];
assign rw_arb_0_mem_valid_data = mem_valid_data;
assign out_mem_valid_data = rw_arb_0_out_mem_valid_data;
assign arb_dat_out[0] = rw_arb_0_out_data;
assign arb_port_out[0] = rw_arb_0_out_port;
assign arb_valid_out = rw_arb_0_out_valid;
assign cen_to_strg = rw_arb_0_cen_mem;
assign wen_to_strg = rw_arb_0_wen_mem;
assign data_to_strg[0] = rw_arb_0_data_to_mem;
assign arb_acks[0] = rw_arb_0_out_ack;
assign addr_out[0] = rw_arb_0_addr_to_mem;
assign rw_arb_0_ren_in = ren_out[0] & rd_sync_gate;
assign arb_dat_out_f[0] = arb_dat_out[0][0];
assign arb_port_out_f[0] = arb_port_out[0][0];
assign arb_valid_out_f = arb_valid_out;
assign arb_mem_valid_data_f = out_mem_valid_data;
assign ren_out_reduced[0] = |ren_out_tpose[0];
assign ren_out_reduced[1] = |ren_out_tpose[1];
assign pre_fetch_0_data_in = data_to_pref[0];
assign pre_fetch_0_mem_valid_data = mem_valid_data_sync[0];
assign mem_valid_data_pref[0] = pre_fetch_0_mem_valid_data_out;
assign pre_fetch_0_valid_read = valid_to_pref[0];
assign pre_fetch_0_tba_rdy_in = ready_tba[0];
assign data_to_tba[0] = pre_fetch_0_data_out;
assign valid_to_tba[0] = pre_fetch_0_valid_out;
assign prefetch_step[0] = pre_fetch_0_prefetch_step;
assign pre_fetch_1_data_in = data_to_pref[1];
assign pre_fetch_1_mem_valid_data = mem_valid_data_sync[1];
assign mem_valid_data_pref[1] = pre_fetch_1_mem_valid_data_out;
assign pre_fetch_1_valid_read = valid_to_pref[1];
assign pre_fetch_1_tba_rdy_in = ready_tba[1];
assign data_to_tba[1] = pre_fetch_1_data_out;
assign valid_to_tba[1] = pre_fetch_1_valid_out;
assign prefetch_step[1] = pre_fetch_1_prefetch_step;
assign tba_0_SRAM_to_tb_data = data_to_tba[0];
assign tba_0_valid_data = valid_to_tba[0];
assign tba_0_ack_in = valid_to_tba[0];
assign tba_0_mem_valid_data = mem_valid_data_pref[0];
assign tb_data_out[0] = tba_0_tb_to_interconnect_data;
assign tb_valid_out[0] = tba_0_tb_to_interconnect_valid;
assign ready_tba[0] = tba_0_tb_arbiter_rdy;
assign tba_0_tba_ren = ren[0];
assign tba_1_SRAM_to_tb_data = data_to_tba[1];
assign tba_1_valid_data = valid_to_tba[1];
assign tba_1_ack_in = valid_to_tba[1];
assign tba_1_mem_valid_data = mem_valid_data_pref[1];
assign tb_data_out[1] = tba_1_tb_to_interconnect_data;
assign tb_valid_out[1] = tba_1_tb_to_interconnect_valid;
assign ready_tba[1] = tba_1_tb_arbiter_rdy;
assign tba_1_tba_ren = ren[1];
assign data_out[0] = tb_data_out[0];
assign data_out[1] = tb_data_out[1];
always_comb begin
  ack_transpose[0] = arb_acks[0][0];
  ack_transpose[1] = arb_acks[0][1];
end
always_comb begin
  ack_reduced[0] = |ack_transpose[0];
  ack_reduced[1] = |ack_transpose[1];
end
app_ctrl app_ctrl (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .input_port(app_ctrl_input_port),
  .output_port(app_ctrl_output_port),
  .prefill(app_ctrl_prefill),
  .ranges(app_ctrl_ranges),
  .read_depth(app_ctrl_read_depth),
  .ren_in(ren_in_muxed),
  .ren_out(ren),
  .ren_update(tb_valid_out),
  .rst_n(rst_n),
  .tb_valid(tb_valid_out),
  .threshold(app_ctrl_threshold),
  .valid_out_data(valid_out),
  .valid_out_stencil(valid_out_stencil),
  .wen_in(wen_in),
  .wen_out(wen),
  .write_depth_ss(app_ctrl_write_depth_ss),
  .write_depth_wo(app_ctrl_write_depth_wo)
);

app_ctrl_unq0 app_ctrl_coarse (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .input_port(app_ctrl_coarse_input_port),
  .output_port(app_ctrl_coarse_output_port),
  .prefill(app_ctrl_coarse_prefill),
  .read_depth(app_ctrl_coarse_read_depth),
  .ren_in(ren_out_reduced),
  .ren_out(arb_ren_en),
  .ren_update(ack_reduced),
  .rst_n(rst_n),
  .tb_valid(2'h0),
  .valid_out_data(valid_out_data_coarse),
  .valid_out_stencil(valid_out_data_stencil),
  .wen_in(ab_to_mem_valid),
  .wen_out(arb_wen_en),
  .write_depth_ss(app_ctrl_coarse_write_depth_ss),
  .write_depth_wo(app_ctrl_coarse_write_depth_wo)
);

agg_aligner agg_align_0 (
  .align(agg_align_0_align),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .in_dat(agg_align_0_in_dat),
  .in_valid(agg_align_0_in_valid),
  .line_length(agg_align_0_line_length),
  .out_dat(agg_align_0_out_dat),
  .out_valid(agg_align_0_out_valid),
  .rst_n(rst_n)
);

agg_aligner agg_align_1 (
  .align(agg_align_1_align),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .in_dat(agg_align_1_in_dat),
  .in_valid(agg_align_1_in_valid),
  .line_length(agg_align_1_line_length),
  .out_dat(agg_align_1_out_dat),
  .out_valid(agg_align_1_out_valid),
  .rst_n(rst_n)
);

aggregation_buffer agg_in_0 (
  .align(agg_in_0_align),
  .clk(clk),
  .clk_en(clk_en),
  .data_in(agg_in_0_data_in),
  .data_out(agg_in_0_data_out),
  .flush(flush),
  .in_period(agg_in_0_in_period),
  .in_sched(agg_in_0_in_sched),
  .out_period(agg_in_0_out_period),
  .out_sched(agg_in_0_out_sched),
  .rst_n(rst_n),
  .valid_in(agg_in_0_valid_in),
  .valid_out(agg_in_0_valid_out)
);

aggregation_buffer agg_in_1 (
  .align(agg_in_1_align),
  .clk(clk),
  .clk_en(clk_en),
  .data_in(agg_in_1_data_in),
  .data_out(agg_in_1_data_out),
  .flush(flush),
  .in_period(agg_in_1_in_period),
  .in_sched(agg_in_1_in_sched),
  .out_period(agg_in_1_out_period),
  .out_sched(agg_in_1_out_sched),
  .rst_n(rst_n),
  .valid_in(agg_in_1_valid_in),
  .valid_out(agg_in_1_valid_out)
);

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
  .clk_en(clk_en),
  .data_in(ab_to_mem_dat),
  .data_out(data_to_arb),
  .flush(flush),
  .port_out(port_wens),
  .rst_n(rst_n),
  .valid_in(ab_to_mem_valid),
  .wen_en(arb_wen_en),
  .wen_to_sram(wen_to_arb)
);

output_addr_ctrl output_addr_ctrl (
  .addr_out(oac_addr_out),
  .address_gen_0_dimensionality(output_addr_ctrl_address_gen_0_dimensionality),
  .address_gen_0_ranges(output_addr_ctrl_address_gen_0_ranges),
  .address_gen_0_starting_addr(output_addr_ctrl_address_gen_0_starting_addr),
  .address_gen_0_strides(output_addr_ctrl_address_gen_0_strides),
  .address_gen_1_dimensionality(output_addr_ctrl_address_gen_1_dimensionality),
  .address_gen_1_ranges(output_addr_ctrl_address_gen_1_ranges),
  .address_gen_1_starting_addr(output_addr_ctrl_address_gen_1_starting_addr),
  .address_gen_1_strides(output_addr_ctrl_address_gen_1_strides),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .ren(ren_out),
  .rst_n(rst_n),
  .step_in(oac_step),
  .valid_in(oac_valid)
);

rw_arbiter rw_arb_0 (
  .addr_to_mem(rw_arb_0_addr_to_mem),
  .cen_mem(rw_arb_0_cen_mem),
  .clk(clk),
  .clk_en(clk_en),
  .data_from_mem(rw_arb_0_data_from_mem),
  .data_to_mem(rw_arb_0_data_to_mem),
  .flush(flush),
  .mem_valid_data(rw_arb_0_mem_valid_data),
  .out_ack(rw_arb_0_out_ack),
  .out_data(rw_arb_0_out_data),
  .out_mem_valid_data(rw_arb_0_out_mem_valid_data),
  .out_port(rw_arb_0_out_port),
  .out_valid(rw_arb_0_out_valid),
  .rd_addr(oac_addr_out),
  .ren_en(arb_ren_en),
  .ren_in(rw_arb_0_ren_in),
  .rst_n(rst_n),
  .w_addr(rw_arb_0_w_addr),
  .w_data(rw_arb_0_w_data),
  .wen_in(rw_arb_0_wen_in),
  .wen_mem(rw_arb_0_wen_mem)
);

demux_reads demux_rds (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(arb_dat_out_f),
  .data_out(data_to_sync),
  .flush(flush),
  .mem_valid_data(arb_mem_valid_data_f),
  .mem_valid_data_out(arb_mem_valid_data_out),
  .port_in(arb_port_out_f),
  .rst_n(rst_n),
  .valid_in(arb_valid_out_f),
  .valid_out(valid_to_sync)
);

sync_groups sync_grp (
  .ack_in(ack_reduced),
  .clk(clk),
  .clk_en(clk_en),
  .data_in(data_to_sync),
  .data_out(data_to_pref),
  .flush(flush),
  .mem_valid_data(arb_mem_valid_data_out),
  .mem_valid_data_out(mem_valid_data_sync),
  .rd_sync_gate(rd_sync_gate),
  .ren_in(ren_out_reduced),
  .rst_n(rst_n),
  .sync_group(sync_grp_sync_group),
  .valid_in(valid_to_sync),
  .valid_out(valid_to_pref)
);

prefetcher pre_fetch_0 (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(pre_fetch_0_data_in),
  .data_out(pre_fetch_0_data_out),
  .flush(flush),
  .input_latency(pre_fetch_0_input_latency),
  .mem_valid_data(pre_fetch_0_mem_valid_data),
  .mem_valid_data_out(pre_fetch_0_mem_valid_data_out),
  .prefetch_step(pre_fetch_0_prefetch_step),
  .rst_n(rst_n),
  .tba_rdy_in(pre_fetch_0_tba_rdy_in),
  .valid_out(pre_fetch_0_valid_out),
  .valid_read(pre_fetch_0_valid_read)
);

prefetcher pre_fetch_1 (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(pre_fetch_1_data_in),
  .data_out(pre_fetch_1_data_out),
  .flush(flush),
  .input_latency(pre_fetch_1_input_latency),
  .mem_valid_data(pre_fetch_1_mem_valid_data),
  .mem_valid_data_out(pre_fetch_1_mem_valid_data_out),
  .prefetch_step(pre_fetch_1_prefetch_step),
  .rst_n(rst_n),
  .tba_rdy_in(pre_fetch_1_tba_rdy_in),
  .valid_out(pre_fetch_1_valid_out),
  .valid_read(pre_fetch_1_valid_read)
);

transpose_buffer_aggregation tba_0 (
  .SRAM_to_tb_data(tba_0_SRAM_to_tb_data),
  .ack_in(tba_0_ack_in),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mem_valid_data(tba_0_mem_valid_data),
  .rst_n(rst_n),
  .tb_0_dimensionality(tba_0_tb_0_dimensionality),
  .tb_0_indices(tba_0_tb_0_indices),
  .tb_0_range_inner(tba_0_tb_0_range_inner),
  .tb_0_range_outer(tba_0_tb_0_range_outer),
  .tb_0_starting_addr(tba_0_tb_0_starting_addr),
  .tb_0_stride(tba_0_tb_0_stride),
  .tb_0_tb_height(tba_0_tb_0_tb_height),
  .tb_arbiter_rdy(tba_0_tb_arbiter_rdy),
  .tb_index_for_data(1'h0),
  .tb_to_interconnect_data(tba_0_tb_to_interconnect_data),
  .tb_to_interconnect_valid(tba_0_tb_to_interconnect_valid),
  .tba_ren(tba_0_tba_ren),
  .valid_data(tba_0_valid_data)
);

transpose_buffer_aggregation tba_1 (
  .SRAM_to_tb_data(tba_1_SRAM_to_tb_data),
  .ack_in(tba_1_ack_in),
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mem_valid_data(tba_1_mem_valid_data),
  .rst_n(rst_n),
  .tb_0_dimensionality(tba_1_tb_0_dimensionality),
  .tb_0_indices(tba_1_tb_0_indices),
  .tb_0_range_inner(tba_1_tb_0_range_inner),
  .tb_0_range_outer(tba_1_tb_0_range_outer),
  .tb_0_starting_addr(tba_1_tb_0_starting_addr),
  .tb_0_stride(tba_1_tb_0_stride),
  .tb_0_tb_height(tba_1_tb_0_tb_height),
  .tb_arbiter_rdy(tba_1_tb_arbiter_rdy),
  .tb_index_for_data(1'h0),
  .tb_to_interconnect_data(tba_1_tb_to_interconnect_data),
  .tb_to_interconnect_valid(tba_1_tb_to_interconnect_valid),
  .tba_ren(tba_1_tba_ren),
  .valid_data(tba_1_valid_data)
);

endmodule   // strg_ub

module sync_groups (
  input logic [1:0] ack_in,
  input logic clk,
  input logic clk_en,
  input logic [1:0][3:0] [15:0] data_in,
  output logic [1:0][3:0] [15:0] data_out,
  input logic flush,
  input logic [1:0] mem_valid_data,
  output logic [1:0] mem_valid_data_out,
  output logic [1:0] rd_sync_gate,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] [1:0] sync_group,
  input logic [1:0] valid_in,
  output logic [1:0] valid_out
);

logic [1:0][3:0][15:0] data_reg;
logic [1:0] group_finished;
logic [1:0][1:0] grp_fin_large;
logic [1:0][1:0] local_gate_bus;
logic [1:0][1:0] local_gate_bus_n;
logic [1:0][1:0] local_gate_bus_tpose;
logic [1:0][1:0] local_gate_mask;
logic [1:0] local_gate_reduced;
logic [1:0] ren_int;
logic [1:0][1:0] sync_agg;
logic [1:0] sync_valid;
logic [1:0] valid_reg;
assign local_gate_bus = ~local_gate_bus_n;
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
  for (int unsigned i = 0; i < 2; i += 1) begin
      sync_valid[1'(i)] = &sync_agg[1'(i)];
    end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    data_reg[0] <= 64'h0;
    valid_reg[0] <= 1'h0;
    mem_valid_data_out[0] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      data_reg[0] <= 64'h0;
      valid_reg[0] <= 1'h0;
      mem_valid_data_out[0] <= 1'h0;
    end
    else if (|(sync_valid & sync_group[0])) begin
      data_reg[0] <= data_in[0];
      mem_valid_data_out[0] <= mem_valid_data[0];
      valid_reg[0] <= valid_in[0];
    end
    else if (~valid_reg[0]) begin
      data_reg[0] <= data_in[0];
      mem_valid_data_out[0] <= mem_valid_data[0];
      valid_reg[0] <= valid_in[0];
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    data_reg[1] <= 64'h0;
    valid_reg[1] <= 1'h0;
    mem_valid_data_out[1] <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      data_reg[1] <= 64'h0;
      valid_reg[1] <= 1'h0;
      mem_valid_data_out[1] <= 1'h0;
    end
    else if (|(sync_valid & sync_group[1])) begin
      data_reg[1] <= data_in[1];
      mem_valid_data_out[1] <= mem_valid_data[1];
      valid_reg[1] <= valid_in[1];
    end
    else if (~valid_reg[1]) begin
      data_reg[1] <= data_in[1];
      mem_valid_data_out[1] <= mem_valid_data[1];
      valid_reg[1] <= valid_in[1];
    end
  end
end
always_comb begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      valid_out[1'(i)] = |(sync_valid & sync_group[1'(i)]);
    end
end
always_comb begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      local_gate_reduced[1'(i)] = &local_gate_bus_tpose[1'(i)];
    end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    local_gate_bus_n[0] <= 2'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      local_gate_bus_n[0] <= 2'h0;
    end
    else if (group_finished[0]) begin
      local_gate_bus_n[0] <= 2'h0;
    end
    else local_gate_bus_n[0] <= ~(local_gate_bus[0] & local_gate_mask[0]);
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    local_gate_bus_n[1] <= 2'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      local_gate_bus_n[1] <= 2'h0;
    end
    else if (group_finished[1]) begin
      local_gate_bus_n[1] <= 2'h0;
    end
    else local_gate_bus_n[1] <= ~(local_gate_bus[1] & local_gate_mask[1]);
  end
end
always_comb begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      for (int unsigned j = 0; j < 2; j += 1) begin
          local_gate_bus_tpose[1'(i)][1'(j)] = local_gate_bus[1'(j)][1'(i)];
        end
    end
end
always_comb begin
  for (int unsigned i = 0; i < 2; i += 1) begin
      group_finished[1'(i)] = &grp_fin_large[1'(i)];
    end
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

module transpose_buffer (
  input logic ack_in,
  input logic clk,
  input logic clk_en,
  output logic [0:0] [15:0] col_pixels,
  input logic [1:0] dimensionality,
  input logic flush,
  input logic [63:0] [2:0] indices,
  input logic [3:0] [15:0] input_data,
  input logic mem_valid_data,
  output logic output_valid,
  input logic [5:0] range_inner,
  input logic [9:0] range_outer,
  output logic rdy_to_arbiter,
  input logic ren,
  input logic rst_n,
  input logic [1:0] starting_addr,
  input logic [3:0] stride,
  input logic tb_height,
  input logic valid_data
);

logic [19:0] curr_out_start;
logic [5:0] index_inner;
logic [9:0] index_outer;
logic [2:0] indices_index_inner;
logic input_buf_index;
logic input_index;
logic mask_valid;
logic old_start_data;
logic on_next_line;
logic out_buf_index;
logic out_buf_indexinv;
logic [1:0] output_index;
logic [19:0] output_index_abs;
logic [19:0] output_index_long;
logic pause_output;
logic pause_tb;
logic pause_tbinv;
logic rdy_to_arbiterinv;
logic row_index;
logic start_data;
logic switch_next_line;
logic switch_out_buf;
logic [1:0][3:0][15:0] tb;
logic [1:0] tb_valid;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    index_outer <= 10'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      index_outer <= 10'h0;
    end
    else if (dimensionality == 2'h0) begin
      index_outer <= 10'h0;
    end
    else if ((dimensionality == 2'h1) | ((dimensionality == 2'h2) & ((range_inner - 6'h1) == index_inner))) begin
      if (~pause_output) begin
        if ((range_outer - 10'h1) == index_outer) begin
          index_outer <= 10'h0;
        end
        else index_outer <= index_outer + 10'h1;
      end
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    index_inner <= 6'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      index_inner <= 6'h0;
    end
    else if (dimensionality <= 2'h1) begin
      index_inner <= 6'h0;
    end
    else if (~pause_output) begin
      if ((range_inner - 6'h1) == index_inner) begin
        index_inner <= 6'h0;
      end
      else index_inner <= index_inner + 6'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pause_tbinv <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      pause_tbinv <= 1'h0;
    end
    else if (dimensionality == 2'h0) begin
      pause_tbinv <= 1'h0;
    end
    else if (((range_outer - 10'h1) == index_outer) & ((dimensionality == 2'h1) | ((dimensionality == 2'h2) & ((range_inner - 6'h1) == index_inner)))) begin
      if (~pause_output) begin
        if ((~rdy_to_arbiter) | valid_data) begin
          pause_tbinv <= 1'h1;
        end
        else pause_tbinv <= 1'h0;
      end
    end
    else if (pause_tb) begin
      pause_tbinv <= valid_data;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    row_index <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
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
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    input_buf_index <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      input_buf_index <= 1'h0;
    end
    else if (valid_data & ((tb_height - 1'h1) == row_index)) begin
      input_buf_index <= ~input_buf_index;
    end
  end
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (valid_data) begin
      if (dimensionality == 2'h0) begin
        tb[input_index] <= 64'h0;
        tb_valid[input_index] <= 1'h0;
      end
      else begin
        tb[input_index] <= input_data;
        tb_valid[input_index] <= mem_valid_data;
      end
    end
  end
end
always_comb begin
  col_pixels[0] = 16'h0;
  if (tb_height > 1'h0) begin
    if (dimensionality == 2'h0) begin
      col_pixels[0] = 16'h0;
    end
    else if (out_buf_index ^ switch_out_buf) begin
      col_pixels[0] = tb[0][output_index];
    end
    else col_pixels[0] = tb[1][output_index];
  end
end
always_comb begin
  if (dimensionality == 2'h0) begin
    output_valid = 1'h0;
  end
  else if (pause_output) begin
    output_valid = 1'h0;
  end
  else output_valid = mask_valid;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    out_buf_indexinv <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      out_buf_indexinv <= 1'h0;
    end
    else if (~start_data) begin
      out_buf_indexinv <= 1'h0;
    end
    else if (switch_out_buf) begin
      out_buf_indexinv <= ~out_buf_indexinv;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rdy_to_arbiterinv <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      rdy_to_arbiterinv <= 1'h0;
    end
    else if (dimensionality == 2'h0) begin
      rdy_to_arbiterinv <= 1'h1;
    end
    else if (start_data & (~old_start_data)) begin
      rdy_to_arbiterinv <= 1'h0;
    end
    else if (switch_out_buf) begin
      rdy_to_arbiterinv <= 1'h0;
    end
    else if (tb_height != 1'h1) begin
      if ((tb_height - 1'h1) != row_index) begin
        rdy_to_arbiterinv <= 1'h0;
      end
    end
    else if (ack_in) begin
      rdy_to_arbiterinv <= 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    start_data <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      start_data <= 1'h0;
    end
    else if (valid_data & (~start_data)) begin
      start_data <= 1'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    curr_out_start <= 20'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      curr_out_start <= 20'h0;
    end
    else if (dimensionality == 2'h0) begin
      curr_out_start <= 20'h0;
    end
    else if (switch_next_line) begin
      curr_out_start <= 20'h0;
    end
    else if ((curr_out_start + 20'h4) <= output_index_abs) begin
      curr_out_start <= curr_out_start + 20'h4;
    end
  end
end
always_comb begin
  output_index = output_index_long[1:0];
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    old_start_data <= start_data;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    on_next_line <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      on_next_line <= 1'h0;
    end
    else if (switch_next_line) begin
      on_next_line <= 1'h1;
    end
    else if ((range_outer - 10'h1) == index_outer) begin
      if ((dimensionality == 2'h1) | ((dimensionality == 2'h2) & ((range_inner - 6'h1) == index_inner))) begin
        on_next_line <= 1'h0;
      end
    end
  end
end
always_comb begin
  if (pause_tb) begin
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
    indices_index_inner = 3'h0;
    output_index_abs = 20'h0;
  end
  else if (dimensionality == 2'h1) begin
    indices_index_inner = 3'h0;
    output_index_abs = (20'(index_outer) * 20'(stride)) + 20'(starting_addr);
  end
  else begin
    indices_index_inner = indices[index_inner];
    output_index_abs = (20'(index_outer) * 20'(stride)) + 20'(indices_index_inner) + 20'(starting_addr);
  end
end
always_comb begin
  if (switch_next_line | ((curr_out_start + 20'h4) <= output_index_abs)) begin
    switch_out_buf = 1'h1;
  end
  else switch_out_buf = 1'h0;
end
always_comb begin
  if ((index_outer == 10'h0) & (~on_next_line) & ((dimensionality == 2'h1) | ((dimensionality == 2'h2) & (index_inner == 6'h0)))) begin
    switch_next_line = 1'h1;
  end
  else switch_next_line = 1'h0;
end
always_comb begin
  output_index_long = output_index_abs % 20'h4;
end
always_comb begin
  if (out_buf_index ^ switch_out_buf) begin
    mask_valid = tb_valid[0];
  end
  else mask_valid = tb_valid[1];
end
always_comb begin
  pause_tb = ~pause_tbinv;
  out_buf_index = ~out_buf_indexinv;
  rdy_to_arbiter = ~rdy_to_arbiterinv;
end
endmodule   // transpose_buffer

module transpose_buffer_aggregation (
  input logic [3:0] [15:0] SRAM_to_tb_data,
  input logic ack_in,
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic mem_valid_data,
  input logic rst_n,
  input logic [1:0] tb_0_dimensionality,
  input logic [63:0] [2:0] tb_0_indices,
  input logic [5:0] tb_0_range_inner,
  input logic [9:0] tb_0_range_outer,
  input logic [1:0] tb_0_starting_addr,
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
  .clk_en(clk_en),
  .col_pixels(tb_0_col_pixels),
  .dimensionality(tb_0_dimensionality),
  .flush(flush),
  .indices(tb_0_indices),
  .input_data(SRAM_to_tb_data),
  .mem_valid_data(mem_valid_data),
  .output_valid(tb_0_output_valid),
  .range_inner(tb_0_range_inner),
  .range_outer(tb_0_range_outer),
  .rdy_to_arbiter(tb_0_rdy_to_arbiter),
  .ren(tba_ren),
  .rst_n(rst_n),
  .starting_addr(tb_0_starting_addr),
  .stride(tb_0_stride),
  .tb_height(tb_0_tb_height),
  .valid_data(tb_0_valid_data)
);

endmodule   // transpose_buffer_aggregation

