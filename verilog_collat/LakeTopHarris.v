module cwlib_ub__confignull__input_num1__output_num2__width16 ub_lyy_stencil_op_hcompute_lyy_stencil_2_to_lyy_stencil_op_hcompute_lgyy_stencil_1_12 (
  input logic [15:0] addr_in_0,
  input logic [15:0] addr_in_1,
  input logic [15:0] chain_data_in_0,
  input logic [15:0] chain_data_in_1,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [15:0] data_in_0,
  input logic [15:0] data_in_1,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] wen_in,
  output logic [31:0] config_data_out_0,
  output logic [31:0] config_data_out_1,
  output logic [15:0] data_out_0,
  output logic [15:0] data_out_1,
  output logic empty,
  output logic full,
  output logic sram_ready_out
);

logic [1:0][15:0] LakeTop_addr_in;
logic [1:0][15:0] LakeTop_chain_data_in;
logic [1:0][31:0] LakeTop_config_data_out;
logic [1:0][15:0] LakeTop_data_in;
logic [1:0][15:0] LakeTop_data_out;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_read_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_out_sel_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_write_ranges;
logic [5:0][15:0] LakeTop_strg_ub_out_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_1_strides;
wire [15:0] strg_ub_agg_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_input_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_starting_addr = 131;
wire [3:0] strg_ub_loops_in2buf_autovec_read_0_dimensionality = 3;
wire [3:0] strg_ub_loops_in2buf_autovec_write_dimensionality = 3;
wire [15:0] strg_ub_output_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_out_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_starting_addr = 256;
wire [3:0] strg_ub_loops_buf2out_autovec_read_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_out_sel_dimensionality = 4;
wire [15:0] strg_ub_agg_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr = 128;
wire [3:0] strg_ub_loops_in2buf_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr = 258;
wire [3:0] strg_ub_loops_buf2out_read_0_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr = 258;
wire [3:0] strg_ub_loops_buf2out_read_1_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_1_dimensionality = 4;
wire [1:0] mode = 0;
wire [0:0] tile_en = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_0 = 2;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_1 = 14;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_1 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_2 = 60;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_2 = -1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_3 = -1;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_3 = -125;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3 = -3967;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_0 = 14;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_0 = 14;
wire [15:0] strg_ub_input_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_0 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_1 = 60;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_1 = 60;
wire [15:0] strg_ub_input_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_1 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_2 = -1;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_2 = -1;
wire [15:0] strg_ub_input_addr_gen_strides_2 = -930;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_2 = -3964;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_0 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_0 = 16;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_0 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_0 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_0 = 1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_0 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_1 = 14;
wire [15:0] strg_ub_output_addr_gen_strides_1 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_1 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_1 = 14;
wire [15:0] strg_ub_out_port_sel_addr_strides_1 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_1 = 14;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_2 = 58;
wire [15:0] strg_ub_output_addr_gen_strides_2 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_2 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_2 = 58;
wire [15:0] strg_ub_out_port_sel_addr_strides_2 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_2 = 58;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_2 = 58;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_3 = -1;
wire [15:0] strg_ub_output_addr_gen_strides_3 = -975;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_3 = -3838;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_3 = -1;
wire [15:0] strg_ub_out_port_sel_addr_strides_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_3 = -959;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_3 = -959;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_3 = -1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_2 = 58;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_3 = -3839;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3 = -3839;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_2 = 58;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_3 = -3839;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3 = -3839;
wire [15:0] strg_ub_agg_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_dimensionality = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_5 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_5 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_0 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_1 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_2 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_3 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_5 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_dimensionality = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_5 = 0;
assign LakeTop_addr_in[0] = addr_in_0;
assign LakeTop_addr_in[1] = addr_in_1;
assign LakeTop_chain_data_in[0] = chain_data_in_0;
assign LakeTop_chain_data_in[1] = chain_data_in_1;
assign config_data_out_0 = LakeTop_config_data_out[0];
assign config_data_out_1 = LakeTop_config_data_out[1];
assign LakeTop_data_in[0] = data_in_0;
assign LakeTop_data_in[1] = data_in_1;
assign data_out_0 = LakeTop_data_out[0];
assign data_out_1 = LakeTop_data_out[1];
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[0] = strg_ub_agg_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[1] = strg_ub_agg_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[2] = strg_ub_agg_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[3] = strg_ub_agg_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[4] = strg_ub_agg_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[5] = strg_ub_agg_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[0] = strg_ub_agg_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[1] = strg_ub_agg_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[2] = strg_ub_agg_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[3] = strg_ub_agg_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[4] = strg_ub_agg_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[5] = strg_ub_agg_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[0] = strg_ub_agg_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[1] = strg_ub_agg_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[2] = strg_ub_agg_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[3] = strg_ub_agg_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[4] = strg_ub_agg_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[5] = strg_ub_agg_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[0] = strg_ub_agg_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[1] = strg_ub_agg_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[2] = strg_ub_agg_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[3] = strg_ub_agg_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[4] = strg_ub_agg_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[5] = strg_ub_agg_write_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_input_addr_gen_strides[0] = strg_ub_input_addr_gen_strides_0;
assign LakeTop_strg_ub_input_addr_gen_strides[1] = strg_ub_input_addr_gen_strides_1;
assign LakeTop_strg_ub_input_addr_gen_strides[2] = strg_ub_input_addr_gen_strides_2;
assign LakeTop_strg_ub_input_addr_gen_strides[3] = strg_ub_input_addr_gen_strides_3;
assign LakeTop_strg_ub_input_addr_gen_strides[4] = strg_ub_input_addr_gen_strides_4;
assign LakeTop_strg_ub_input_addr_gen_strides[5] = strg_ub_input_addr_gen_strides_5;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[0] = strg_ub_input_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[1] = strg_ub_input_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[2] = strg_ub_input_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[3] = strg_ub_input_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[4] = strg_ub_input_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[5] = strg_ub_input_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[0] = strg_ub_loops_buf2out_autovec_read_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[1] = strg_ub_loops_buf2out_autovec_read_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[2] = strg_ub_loops_buf2out_autovec_read_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[3] = strg_ub_loops_buf2out_autovec_read_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[4] = strg_ub_loops_buf2out_autovec_read_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[5] = strg_ub_loops_buf2out_autovec_read_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[0] = strg_ub_loops_buf2out_autovec_write_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[1] = strg_ub_loops_buf2out_autovec_write_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[2] = strg_ub_loops_buf2out_autovec_write_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[3] = strg_ub_loops_buf2out_autovec_write_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[4] = strg_ub_loops_buf2out_autovec_write_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[5] = strg_ub_loops_buf2out_autovec_write_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[0] = strg_ub_loops_buf2out_autovec_write_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[1] = strg_ub_loops_buf2out_autovec_write_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[2] = strg_ub_loops_buf2out_autovec_write_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[3] = strg_ub_loops_buf2out_autovec_write_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[4] = strg_ub_loops_buf2out_autovec_write_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[5] = strg_ub_loops_buf2out_autovec_write_1_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[0] = strg_ub_loops_buf2out_out_sel_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[1] = strg_ub_loops_buf2out_out_sel_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[2] = strg_ub_loops_buf2out_out_sel_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[3] = strg_ub_loops_buf2out_out_sel_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[4] = strg_ub_loops_buf2out_out_sel_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[5] = strg_ub_loops_buf2out_out_sel_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[0] = strg_ub_loops_buf2out_read_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[1] = strg_ub_loops_buf2out_read_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[2] = strg_ub_loops_buf2out_read_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[3] = strg_ub_loops_buf2out_read_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[4] = strg_ub_loops_buf2out_read_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[5] = strg_ub_loops_buf2out_read_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[0] = strg_ub_loops_buf2out_read_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[1] = strg_ub_loops_buf2out_read_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[2] = strg_ub_loops_buf2out_read_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[3] = strg_ub_loops_buf2out_read_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[4] = strg_ub_loops_buf2out_read_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[5] = strg_ub_loops_buf2out_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[0] = strg_ub_loops_in2buf_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[1] = strg_ub_loops_in2buf_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[2] = strg_ub_loops_in2buf_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[3] = strg_ub_loops_in2buf_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[4] = strg_ub_loops_in2buf_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[5] = strg_ub_loops_in2buf_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[0] = strg_ub_loops_in2buf_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[1] = strg_ub_loops_in2buf_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[2] = strg_ub_loops_in2buf_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[3] = strg_ub_loops_in2buf_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[4] = strg_ub_loops_in2buf_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[5] = strg_ub_loops_in2buf_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[0] = strg_ub_loops_in2buf_autovec_read_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[1] = strg_ub_loops_in2buf_autovec_read_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[2] = strg_ub_loops_in2buf_autovec_read_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[3] = strg_ub_loops_in2buf_autovec_read_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[4] = strg_ub_loops_in2buf_autovec_read_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[5] = strg_ub_loops_in2buf_autovec_read_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[0] = strg_ub_loops_in2buf_autovec_read_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[1] = strg_ub_loops_in2buf_autovec_read_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[2] = strg_ub_loops_in2buf_autovec_read_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[3] = strg_ub_loops_in2buf_autovec_read_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[4] = strg_ub_loops_in2buf_autovec_read_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[5] = strg_ub_loops_in2buf_autovec_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[0] = strg_ub_loops_in2buf_autovec_write_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[1] = strg_ub_loops_in2buf_autovec_write_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[2] = strg_ub_loops_in2buf_autovec_write_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[3] = strg_ub_loops_in2buf_autovec_write_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[4] = strg_ub_loops_in2buf_autovec_write_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[5] = strg_ub_loops_in2buf_autovec_write_ranges_5;
assign LakeTop_strg_ub_out_port_sel_addr_strides[0] = strg_ub_out_port_sel_addr_strides_0;
assign LakeTop_strg_ub_out_port_sel_addr_strides[1] = strg_ub_out_port_sel_addr_strides_1;
assign LakeTop_strg_ub_out_port_sel_addr_strides[2] = strg_ub_out_port_sel_addr_strides_2;
assign LakeTop_strg_ub_out_port_sel_addr_strides[3] = strg_ub_out_port_sel_addr_strides_3;
assign LakeTop_strg_ub_out_port_sel_addr_strides[4] = strg_ub_out_port_sel_addr_strides_4;
assign LakeTop_strg_ub_out_port_sel_addr_strides[5] = strg_ub_out_port_sel_addr_strides_5;
assign LakeTop_strg_ub_output_addr_gen_strides[0] = strg_ub_output_addr_gen_strides_0;
assign LakeTop_strg_ub_output_addr_gen_strides[1] = strg_ub_output_addr_gen_strides_1;
assign LakeTop_strg_ub_output_addr_gen_strides[2] = strg_ub_output_addr_gen_strides_2;
assign LakeTop_strg_ub_output_addr_gen_strides[3] = strg_ub_output_addr_gen_strides_3;
assign LakeTop_strg_ub_output_addr_gen_strides[4] = strg_ub_output_addr_gen_strides_4;
assign LakeTop_strg_ub_output_addr_gen_strides[5] = strg_ub_output_addr_gen_strides_5;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[0] = strg_ub_output_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[1] = strg_ub_output_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[2] = strg_ub_output_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[3] = strg_ub_output_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[4] = strg_ub_output_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[5] = strg_ub_output_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_port_sel_addr_strides[0] = strg_ub_port_sel_addr_strides_0;
assign LakeTop_strg_ub_port_sel_addr_strides[1] = strg_ub_port_sel_addr_strides_1;
assign LakeTop_strg_ub_port_sel_addr_strides[2] = strg_ub_port_sel_addr_strides_2;
assign LakeTop_strg_ub_port_sel_addr_strides[3] = strg_ub_port_sel_addr_strides_3;
assign LakeTop_strg_ub_port_sel_addr_strides[4] = strg_ub_port_sel_addr_strides_4;
assign LakeTop_strg_ub_port_sel_addr_strides[5] = strg_ub_port_sel_addr_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[0] = strg_ub_tb_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[1] = strg_ub_tb_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[2] = strg_ub_tb_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[3] = strg_ub_tb_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[4] = strg_ub_tb_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[5] = strg_ub_tb_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[0] = strg_ub_tb_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[1] = strg_ub_tb_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[2] = strg_ub_tb_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[3] = strg_ub_tb_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[4] = strg_ub_tb_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[5] = strg_ub_tb_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[0] = strg_ub_tb_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[1] = strg_ub_tb_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[2] = strg_ub_tb_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[3] = strg_ub_tb_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[4] = strg_ub_tb_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[5] = strg_ub_tb_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[0] = strg_ub_tb_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[1] = strg_ub_tb_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[2] = strg_ub_tb_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[3] = strg_ub_tb_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[4] = strg_ub_tb_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[5] = strg_ub_tb_write_addr_gen_1_strides_5;
LakeTop LakeTop (
  .addr_in(LakeTop_addr_in),
  .chain_data_in(LakeTop_chain_data_in),
  .clk(clk),
  .clk_en(clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_en(config_en),
  .config_read(config_read),
  .config_write(config_write),
  .data_in(LakeTop_data_in),
  .fifo_ctrl_fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .mode(mode),
  .ren_in(ren_in),
  .rst_n(rst_n),
  .strg_ub_agg_read_addr_gen_0_starting_addr(strg_ub_agg_read_addr_gen_0_starting_addr),
  .strg_ub_agg_read_addr_gen_0_strides(LakeTop_strg_ub_agg_read_addr_gen_0_strides),
  .strg_ub_agg_read_addr_gen_1_starting_addr(strg_ub_agg_read_addr_gen_1_starting_addr),
  .strg_ub_agg_read_addr_gen_1_strides(LakeTop_strg_ub_agg_read_addr_gen_1_strides),
  .strg_ub_agg_write_addr_gen_0_starting_addr(strg_ub_agg_write_addr_gen_0_starting_addr),
  .strg_ub_agg_write_addr_gen_0_strides(LakeTop_strg_ub_agg_write_addr_gen_0_strides),
  .strg_ub_agg_write_addr_gen_1_starting_addr(strg_ub_agg_write_addr_gen_1_starting_addr),
  .strg_ub_agg_write_addr_gen_1_strides(LakeTop_strg_ub_agg_write_addr_gen_1_strides),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_input_addr_gen_starting_addr(strg_ub_input_addr_gen_starting_addr),
  .strg_ub_input_addr_gen_strides(LakeTop_strg_ub_input_addr_gen_strides),
  .strg_ub_input_sched_gen_sched_addr_gen_starting_addr(strg_ub_input_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_input_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides),
  .strg_ub_loops_buf2out_autovec_read_dimensionality(strg_ub_loops_buf2out_autovec_read_dimensionality),
  .strg_ub_loops_buf2out_autovec_read_ranges(LakeTop_strg_ub_loops_buf2out_autovec_read_ranges),
  .strg_ub_loops_buf2out_autovec_write_0_dimensionality(strg_ub_loops_buf2out_autovec_write_0_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_0_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges),
  .strg_ub_loops_buf2out_autovec_write_1_dimensionality(strg_ub_loops_buf2out_autovec_write_1_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_1_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges),
  .strg_ub_loops_buf2out_out_sel_dimensionality(strg_ub_loops_buf2out_out_sel_dimensionality),
  .strg_ub_loops_buf2out_out_sel_ranges(LakeTop_strg_ub_loops_buf2out_out_sel_ranges),
  .strg_ub_loops_buf2out_read_0_dimensionality(strg_ub_loops_buf2out_read_0_dimensionality),
  .strg_ub_loops_buf2out_read_0_ranges(LakeTop_strg_ub_loops_buf2out_read_0_ranges),
  .strg_ub_loops_buf2out_read_1_dimensionality(strg_ub_loops_buf2out_read_1_dimensionality),
  .strg_ub_loops_buf2out_read_1_ranges(LakeTop_strg_ub_loops_buf2out_read_1_ranges),
  .strg_ub_loops_in2buf_0_dimensionality(strg_ub_loops_in2buf_0_dimensionality),
  .strg_ub_loops_in2buf_0_ranges(LakeTop_strg_ub_loops_in2buf_0_ranges),
  .strg_ub_loops_in2buf_1_dimensionality(strg_ub_loops_in2buf_1_dimensionality),
  .strg_ub_loops_in2buf_1_ranges(LakeTop_strg_ub_loops_in2buf_1_ranges),
  .strg_ub_loops_in2buf_autovec_read_0_dimensionality(strg_ub_loops_in2buf_autovec_read_0_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_0_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges),
  .strg_ub_loops_in2buf_autovec_read_1_dimensionality(strg_ub_loops_in2buf_autovec_read_1_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_1_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges),
  .strg_ub_loops_in2buf_autovec_write_dimensionality(strg_ub_loops_in2buf_autovec_write_dimensionality),
  .strg_ub_loops_in2buf_autovec_write_ranges(LakeTop_strg_ub_loops_in2buf_autovec_write_ranges),
  .strg_ub_out_port_sel_addr_starting_addr(strg_ub_out_port_sel_addr_starting_addr),
  .strg_ub_out_port_sel_addr_strides(LakeTop_strg_ub_out_port_sel_addr_strides),
  .strg_ub_output_addr_gen_starting_addr(strg_ub_output_addr_gen_starting_addr),
  .strg_ub_output_addr_gen_strides(LakeTop_strg_ub_output_addr_gen_strides),
  .strg_ub_output_sched_gen_sched_addr_gen_starting_addr(strg_ub_output_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_output_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides),
  .strg_ub_port_sel_addr_starting_addr(strg_ub_port_sel_addr_starting_addr),
  .strg_ub_port_sel_addr_strides(LakeTop_strg_ub_port_sel_addr_strides),
  .strg_ub_tb_read_addr_gen_0_starting_addr(strg_ub_tb_read_addr_gen_0_starting_addr),
  .strg_ub_tb_read_addr_gen_0_strides(LakeTop_strg_ub_tb_read_addr_gen_0_strides),
  .strg_ub_tb_read_addr_gen_1_starting_addr(strg_ub_tb_read_addr_gen_1_starting_addr),
  .strg_ub_tb_read_addr_gen_1_strides(LakeTop_strg_ub_tb_read_addr_gen_1_strides),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_tb_write_addr_gen_0_starting_addr(strg_ub_tb_write_addr_gen_0_starting_addr),
  .strg_ub_tb_write_addr_gen_0_strides(LakeTop_strg_ub_tb_write_addr_gen_0_strides),
  .strg_ub_tb_write_addr_gen_1_starting_addr(strg_ub_tb_write_addr_gen_1_starting_addr),
  .strg_ub_tb_write_addr_gen_1_strides(LakeTop_strg_ub_tb_write_addr_gen_1_strides),
  .tile_en(tile_en),
  .wen_in(wen_in),
  .config_data_out(LakeTop_config_data_out),
  .data_out(LakeTop_data_out),
  .empty(empty),
  .full(full),
  .sram_ready_out(sram_ready_out)
);

endmodule   // LakeTop_W

module cwlib_ub__confignull__input_num1__output_num2__width16 ub_lxy_stencil_op_hcompute_lxy_stencil_4_to_lxy_stencil_op_hcompute_lgxy_stencil_1_22 (
  input logic [15:0] addr_in_0,
  input logic [15:0] addr_in_1,
  input logic [15:0] chain_data_in_0,
  input logic [15:0] chain_data_in_1,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [15:0] data_in_0,
  input logic [15:0] data_in_1,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] wen_in,
  output logic [31:0] config_data_out_0,
  output logic [31:0] config_data_out_1,
  output logic [15:0] data_out_0,
  output logic [15:0] data_out_1,
  output logic empty,
  output logic full,
  output logic sram_ready_out
);

logic [1:0][15:0] LakeTop_addr_in;
logic [1:0][15:0] LakeTop_chain_data_in;
logic [1:0][31:0] LakeTop_config_data_out;
logic [1:0][15:0] LakeTop_data_in;
logic [1:0][15:0] LakeTop_data_out;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_read_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_out_sel_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_write_ranges;
logic [5:0][15:0] LakeTop_strg_ub_out_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_1_strides;
wire [15:0] strg_ub_agg_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_input_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_starting_addr = 135;
wire [3:0] strg_ub_loops_in2buf_autovec_read_0_dimensionality = 3;
wire [3:0] strg_ub_loops_in2buf_autovec_write_dimensionality = 3;
wire [15:0] strg_ub_output_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_out_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_starting_addr = 258;
wire [3:0] strg_ub_loops_buf2out_autovec_read_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_out_sel_dimensionality = 4;
wire [15:0] strg_ub_agg_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr = 132;
wire [3:0] strg_ub_loops_in2buf_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr = 260;
wire [3:0] strg_ub_loops_buf2out_read_0_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr = 260;
wire [3:0] strg_ub_loops_buf2out_read_1_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_1_dimensionality = 4;
wire [1:0] mode = 0;
wire [0:0] tile_en = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_0 = 2;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_1 = 14;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_1 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_2 = 60;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_2 = -1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_3 = -1;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_3 = -125;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3 = -3967;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_0 = 14;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_0 = 14;
wire [15:0] strg_ub_input_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_0 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_1 = 60;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_1 = 60;
wire [15:0] strg_ub_input_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_1 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_2 = -1;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_2 = -1;
wire [15:0] strg_ub_input_addr_gen_strides_2 = -930;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_2 = -3964;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_0 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_0 = 16;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_0 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_0 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_0 = 1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_0 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_1 = 14;
wire [15:0] strg_ub_output_addr_gen_strides_1 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_1 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_1 = 14;
wire [15:0] strg_ub_out_port_sel_addr_strides_1 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_1 = 14;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_2 = 58;
wire [15:0] strg_ub_output_addr_gen_strides_2 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_2 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_2 = 58;
wire [15:0] strg_ub_out_port_sel_addr_strides_2 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_2 = 58;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_2 = 58;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_3 = -1;
wire [15:0] strg_ub_output_addr_gen_strides_3 = -975;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_3 = -3838;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_3 = -1;
wire [15:0] strg_ub_out_port_sel_addr_strides_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_3 = -959;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_3 = -959;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_3 = -1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_2 = 58;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_3 = -3839;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3 = -3839;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_2 = 58;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_3 = -3839;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3 = -3839;
wire [15:0] strg_ub_output_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_dimensionality = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_5 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_input_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_0 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_1 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_2 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_3 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_dimensionality = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_5 = 0;
assign LakeTop_addr_in[0] = addr_in_0;
assign LakeTop_addr_in[1] = addr_in_1;
assign LakeTop_chain_data_in[0] = chain_data_in_0;
assign LakeTop_chain_data_in[1] = chain_data_in_1;
assign config_data_out_0 = LakeTop_config_data_out[0];
assign config_data_out_1 = LakeTop_config_data_out[1];
assign LakeTop_data_in[0] = data_in_0;
assign LakeTop_data_in[1] = data_in_1;
assign data_out_0 = LakeTop_data_out[0];
assign data_out_1 = LakeTop_data_out[1];
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[0] = strg_ub_agg_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[1] = strg_ub_agg_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[2] = strg_ub_agg_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[3] = strg_ub_agg_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[4] = strg_ub_agg_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[5] = strg_ub_agg_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[0] = strg_ub_agg_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[1] = strg_ub_agg_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[2] = strg_ub_agg_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[3] = strg_ub_agg_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[4] = strg_ub_agg_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[5] = strg_ub_agg_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[0] = strg_ub_agg_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[1] = strg_ub_agg_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[2] = strg_ub_agg_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[3] = strg_ub_agg_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[4] = strg_ub_agg_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[5] = strg_ub_agg_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[0] = strg_ub_agg_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[1] = strg_ub_agg_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[2] = strg_ub_agg_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[3] = strg_ub_agg_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[4] = strg_ub_agg_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[5] = strg_ub_agg_write_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_input_addr_gen_strides[0] = strg_ub_input_addr_gen_strides_0;
assign LakeTop_strg_ub_input_addr_gen_strides[1] = strg_ub_input_addr_gen_strides_1;
assign LakeTop_strg_ub_input_addr_gen_strides[2] = strg_ub_input_addr_gen_strides_2;
assign LakeTop_strg_ub_input_addr_gen_strides[3] = strg_ub_input_addr_gen_strides_3;
assign LakeTop_strg_ub_input_addr_gen_strides[4] = strg_ub_input_addr_gen_strides_4;
assign LakeTop_strg_ub_input_addr_gen_strides[5] = strg_ub_input_addr_gen_strides_5;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[0] = strg_ub_input_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[1] = strg_ub_input_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[2] = strg_ub_input_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[3] = strg_ub_input_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[4] = strg_ub_input_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[5] = strg_ub_input_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[0] = strg_ub_loops_buf2out_autovec_read_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[1] = strg_ub_loops_buf2out_autovec_read_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[2] = strg_ub_loops_buf2out_autovec_read_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[3] = strg_ub_loops_buf2out_autovec_read_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[4] = strg_ub_loops_buf2out_autovec_read_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[5] = strg_ub_loops_buf2out_autovec_read_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[0] = strg_ub_loops_buf2out_autovec_write_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[1] = strg_ub_loops_buf2out_autovec_write_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[2] = strg_ub_loops_buf2out_autovec_write_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[3] = strg_ub_loops_buf2out_autovec_write_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[4] = strg_ub_loops_buf2out_autovec_write_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[5] = strg_ub_loops_buf2out_autovec_write_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[0] = strg_ub_loops_buf2out_autovec_write_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[1] = strg_ub_loops_buf2out_autovec_write_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[2] = strg_ub_loops_buf2out_autovec_write_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[3] = strg_ub_loops_buf2out_autovec_write_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[4] = strg_ub_loops_buf2out_autovec_write_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[5] = strg_ub_loops_buf2out_autovec_write_1_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[0] = strg_ub_loops_buf2out_out_sel_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[1] = strg_ub_loops_buf2out_out_sel_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[2] = strg_ub_loops_buf2out_out_sel_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[3] = strg_ub_loops_buf2out_out_sel_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[4] = strg_ub_loops_buf2out_out_sel_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[5] = strg_ub_loops_buf2out_out_sel_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[0] = strg_ub_loops_buf2out_read_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[1] = strg_ub_loops_buf2out_read_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[2] = strg_ub_loops_buf2out_read_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[3] = strg_ub_loops_buf2out_read_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[4] = strg_ub_loops_buf2out_read_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[5] = strg_ub_loops_buf2out_read_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[0] = strg_ub_loops_buf2out_read_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[1] = strg_ub_loops_buf2out_read_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[2] = strg_ub_loops_buf2out_read_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[3] = strg_ub_loops_buf2out_read_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[4] = strg_ub_loops_buf2out_read_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[5] = strg_ub_loops_buf2out_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[0] = strg_ub_loops_in2buf_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[1] = strg_ub_loops_in2buf_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[2] = strg_ub_loops_in2buf_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[3] = strg_ub_loops_in2buf_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[4] = strg_ub_loops_in2buf_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[5] = strg_ub_loops_in2buf_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[0] = strg_ub_loops_in2buf_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[1] = strg_ub_loops_in2buf_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[2] = strg_ub_loops_in2buf_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[3] = strg_ub_loops_in2buf_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[4] = strg_ub_loops_in2buf_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[5] = strg_ub_loops_in2buf_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[0] = strg_ub_loops_in2buf_autovec_read_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[1] = strg_ub_loops_in2buf_autovec_read_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[2] = strg_ub_loops_in2buf_autovec_read_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[3] = strg_ub_loops_in2buf_autovec_read_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[4] = strg_ub_loops_in2buf_autovec_read_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[5] = strg_ub_loops_in2buf_autovec_read_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[0] = strg_ub_loops_in2buf_autovec_read_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[1] = strg_ub_loops_in2buf_autovec_read_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[2] = strg_ub_loops_in2buf_autovec_read_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[3] = strg_ub_loops_in2buf_autovec_read_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[4] = strg_ub_loops_in2buf_autovec_read_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[5] = strg_ub_loops_in2buf_autovec_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[0] = strg_ub_loops_in2buf_autovec_write_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[1] = strg_ub_loops_in2buf_autovec_write_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[2] = strg_ub_loops_in2buf_autovec_write_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[3] = strg_ub_loops_in2buf_autovec_write_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[4] = strg_ub_loops_in2buf_autovec_write_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[5] = strg_ub_loops_in2buf_autovec_write_ranges_5;
assign LakeTop_strg_ub_out_port_sel_addr_strides[0] = strg_ub_out_port_sel_addr_strides_0;
assign LakeTop_strg_ub_out_port_sel_addr_strides[1] = strg_ub_out_port_sel_addr_strides_1;
assign LakeTop_strg_ub_out_port_sel_addr_strides[2] = strg_ub_out_port_sel_addr_strides_2;
assign LakeTop_strg_ub_out_port_sel_addr_strides[3] = strg_ub_out_port_sel_addr_strides_3;
assign LakeTop_strg_ub_out_port_sel_addr_strides[4] = strg_ub_out_port_sel_addr_strides_4;
assign LakeTop_strg_ub_out_port_sel_addr_strides[5] = strg_ub_out_port_sel_addr_strides_5;
assign LakeTop_strg_ub_output_addr_gen_strides[0] = strg_ub_output_addr_gen_strides_0;
assign LakeTop_strg_ub_output_addr_gen_strides[1] = strg_ub_output_addr_gen_strides_1;
assign LakeTop_strg_ub_output_addr_gen_strides[2] = strg_ub_output_addr_gen_strides_2;
assign LakeTop_strg_ub_output_addr_gen_strides[3] = strg_ub_output_addr_gen_strides_3;
assign LakeTop_strg_ub_output_addr_gen_strides[4] = strg_ub_output_addr_gen_strides_4;
assign LakeTop_strg_ub_output_addr_gen_strides[5] = strg_ub_output_addr_gen_strides_5;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[0] = strg_ub_output_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[1] = strg_ub_output_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[2] = strg_ub_output_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[3] = strg_ub_output_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[4] = strg_ub_output_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[5] = strg_ub_output_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_port_sel_addr_strides[0] = strg_ub_port_sel_addr_strides_0;
assign LakeTop_strg_ub_port_sel_addr_strides[1] = strg_ub_port_sel_addr_strides_1;
assign LakeTop_strg_ub_port_sel_addr_strides[2] = strg_ub_port_sel_addr_strides_2;
assign LakeTop_strg_ub_port_sel_addr_strides[3] = strg_ub_port_sel_addr_strides_3;
assign LakeTop_strg_ub_port_sel_addr_strides[4] = strg_ub_port_sel_addr_strides_4;
assign LakeTop_strg_ub_port_sel_addr_strides[5] = strg_ub_port_sel_addr_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[0] = strg_ub_tb_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[1] = strg_ub_tb_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[2] = strg_ub_tb_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[3] = strg_ub_tb_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[4] = strg_ub_tb_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[5] = strg_ub_tb_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[0] = strg_ub_tb_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[1] = strg_ub_tb_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[2] = strg_ub_tb_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[3] = strg_ub_tb_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[4] = strg_ub_tb_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[5] = strg_ub_tb_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[0] = strg_ub_tb_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[1] = strg_ub_tb_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[2] = strg_ub_tb_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[3] = strg_ub_tb_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[4] = strg_ub_tb_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[5] = strg_ub_tb_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[0] = strg_ub_tb_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[1] = strg_ub_tb_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[2] = strg_ub_tb_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[3] = strg_ub_tb_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[4] = strg_ub_tb_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[5] = strg_ub_tb_write_addr_gen_1_strides_5;
LakeTop LakeTop (
  .addr_in(LakeTop_addr_in),
  .chain_data_in(LakeTop_chain_data_in),
  .clk(clk),
  .clk_en(clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_en(config_en),
  .config_read(config_read),
  .config_write(config_write),
  .data_in(LakeTop_data_in),
  .fifo_ctrl_fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .mode(mode),
  .ren_in(ren_in),
  .rst_n(rst_n),
  .strg_ub_agg_read_addr_gen_0_starting_addr(strg_ub_agg_read_addr_gen_0_starting_addr),
  .strg_ub_agg_read_addr_gen_0_strides(LakeTop_strg_ub_agg_read_addr_gen_0_strides),
  .strg_ub_agg_read_addr_gen_1_starting_addr(strg_ub_agg_read_addr_gen_1_starting_addr),
  .strg_ub_agg_read_addr_gen_1_strides(LakeTop_strg_ub_agg_read_addr_gen_1_strides),
  .strg_ub_agg_write_addr_gen_0_starting_addr(strg_ub_agg_write_addr_gen_0_starting_addr),
  .strg_ub_agg_write_addr_gen_0_strides(LakeTop_strg_ub_agg_write_addr_gen_0_strides),
  .strg_ub_agg_write_addr_gen_1_starting_addr(strg_ub_agg_write_addr_gen_1_starting_addr),
  .strg_ub_agg_write_addr_gen_1_strides(LakeTop_strg_ub_agg_write_addr_gen_1_strides),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_input_addr_gen_starting_addr(strg_ub_input_addr_gen_starting_addr),
  .strg_ub_input_addr_gen_strides(LakeTop_strg_ub_input_addr_gen_strides),
  .strg_ub_input_sched_gen_sched_addr_gen_starting_addr(strg_ub_input_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_input_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides),
  .strg_ub_loops_buf2out_autovec_read_dimensionality(strg_ub_loops_buf2out_autovec_read_dimensionality),
  .strg_ub_loops_buf2out_autovec_read_ranges(LakeTop_strg_ub_loops_buf2out_autovec_read_ranges),
  .strg_ub_loops_buf2out_autovec_write_0_dimensionality(strg_ub_loops_buf2out_autovec_write_0_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_0_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges),
  .strg_ub_loops_buf2out_autovec_write_1_dimensionality(strg_ub_loops_buf2out_autovec_write_1_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_1_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges),
  .strg_ub_loops_buf2out_out_sel_dimensionality(strg_ub_loops_buf2out_out_sel_dimensionality),
  .strg_ub_loops_buf2out_out_sel_ranges(LakeTop_strg_ub_loops_buf2out_out_sel_ranges),
  .strg_ub_loops_buf2out_read_0_dimensionality(strg_ub_loops_buf2out_read_0_dimensionality),
  .strg_ub_loops_buf2out_read_0_ranges(LakeTop_strg_ub_loops_buf2out_read_0_ranges),
  .strg_ub_loops_buf2out_read_1_dimensionality(strg_ub_loops_buf2out_read_1_dimensionality),
  .strg_ub_loops_buf2out_read_1_ranges(LakeTop_strg_ub_loops_buf2out_read_1_ranges),
  .strg_ub_loops_in2buf_0_dimensionality(strg_ub_loops_in2buf_0_dimensionality),
  .strg_ub_loops_in2buf_0_ranges(LakeTop_strg_ub_loops_in2buf_0_ranges),
  .strg_ub_loops_in2buf_1_dimensionality(strg_ub_loops_in2buf_1_dimensionality),
  .strg_ub_loops_in2buf_1_ranges(LakeTop_strg_ub_loops_in2buf_1_ranges),
  .strg_ub_loops_in2buf_autovec_read_0_dimensionality(strg_ub_loops_in2buf_autovec_read_0_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_0_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges),
  .strg_ub_loops_in2buf_autovec_read_1_dimensionality(strg_ub_loops_in2buf_autovec_read_1_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_1_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges),
  .strg_ub_loops_in2buf_autovec_write_dimensionality(strg_ub_loops_in2buf_autovec_write_dimensionality),
  .strg_ub_loops_in2buf_autovec_write_ranges(LakeTop_strg_ub_loops_in2buf_autovec_write_ranges),
  .strg_ub_out_port_sel_addr_starting_addr(strg_ub_out_port_sel_addr_starting_addr),
  .strg_ub_out_port_sel_addr_strides(LakeTop_strg_ub_out_port_sel_addr_strides),
  .strg_ub_output_addr_gen_starting_addr(strg_ub_output_addr_gen_starting_addr),
  .strg_ub_output_addr_gen_strides(LakeTop_strg_ub_output_addr_gen_strides),
  .strg_ub_output_sched_gen_sched_addr_gen_starting_addr(strg_ub_output_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_output_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides),
  .strg_ub_port_sel_addr_starting_addr(strg_ub_port_sel_addr_starting_addr),
  .strg_ub_port_sel_addr_strides(LakeTop_strg_ub_port_sel_addr_strides),
  .strg_ub_tb_read_addr_gen_0_starting_addr(strg_ub_tb_read_addr_gen_0_starting_addr),
  .strg_ub_tb_read_addr_gen_0_strides(LakeTop_strg_ub_tb_read_addr_gen_0_strides),
  .strg_ub_tb_read_addr_gen_1_starting_addr(strg_ub_tb_read_addr_gen_1_starting_addr),
  .strg_ub_tb_read_addr_gen_1_strides(LakeTop_strg_ub_tb_read_addr_gen_1_strides),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_tb_write_addr_gen_0_starting_addr(strg_ub_tb_write_addr_gen_0_starting_addr),
  .strg_ub_tb_write_addr_gen_0_strides(LakeTop_strg_ub_tb_write_addr_gen_0_strides),
  .strg_ub_tb_write_addr_gen_1_starting_addr(strg_ub_tb_write_addr_gen_1_starting_addr),
  .strg_ub_tb_write_addr_gen_1_strides(LakeTop_strg_ub_tb_write_addr_gen_1_strides),
  .tile_en(tile_en),
  .wen_in(wen_in),
  .config_data_out(LakeTop_config_data_out),
  .data_out(LakeTop_data_out),
  .empty(empty),
  .full(full),
  .sram_ready_out(sram_ready_out)
);

endmodule   // LakeTop_W

module cwlib_ub__confignull__input_num1__output_num2__width16 ub_lxx_stencil_op_hcompute_lxx_stencil_7_to_lxx_stencil_op_hcompute_lgxx_stencil_1_32 (
  input logic [15:0] addr_in_0,
  input logic [15:0] addr_in_1,
  input logic [15:0] chain_data_in_0,
  input logic [15:0] chain_data_in_1,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [15:0] data_in_0,
  input logic [15:0] data_in_1,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] wen_in,
  output logic [31:0] config_data_out_0,
  output logic [31:0] config_data_out_1,
  output logic [15:0] data_out_0,
  output logic [15:0] data_out_1,
  output logic empty,
  output logic full,
  output logic sram_ready_out
);

logic [1:0][15:0] LakeTop_addr_in;
logic [1:0][15:0] LakeTop_chain_data_in;
logic [1:0][31:0] LakeTop_config_data_out;
logic [1:0][15:0] LakeTop_data_in;
logic [1:0][15:0] LakeTop_data_out;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_read_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_out_sel_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_write_ranges;
logic [5:0][15:0] LakeTop_strg_ub_out_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_1_strides;
wire [15:0] strg_ub_agg_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_input_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_starting_addr = 135;
wire [3:0] strg_ub_loops_in2buf_autovec_read_0_dimensionality = 3;
wire [3:0] strg_ub_loops_in2buf_autovec_write_dimensionality = 3;
wire [15:0] strg_ub_output_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_out_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_starting_addr = 258;
wire [3:0] strg_ub_loops_buf2out_autovec_read_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_out_sel_dimensionality = 4;
wire [15:0] strg_ub_agg_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr = 132;
wire [3:0] strg_ub_loops_in2buf_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr = 260;
wire [3:0] strg_ub_loops_buf2out_read_0_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr = 260;
wire [3:0] strg_ub_loops_buf2out_read_1_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_1_dimensionality = 4;
wire [1:0] mode = 0;
wire [0:0] tile_en = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_0 = 2;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_1 = 14;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_1 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_2 = 60;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_2 = -1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_3 = -1;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_3 = -125;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3 = -3967;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_0 = 14;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_0 = 14;
wire [15:0] strg_ub_input_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_0 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_1 = 60;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_1 = 60;
wire [15:0] strg_ub_input_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_1 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_2 = -1;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_2 = -1;
wire [15:0] strg_ub_input_addr_gen_strides_2 = -930;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_2 = -3964;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_0 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_0 = 16;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_0 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_0 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_0 = 1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_0 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_1 = 14;
wire [15:0] strg_ub_output_addr_gen_strides_1 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_1 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_1 = 14;
wire [15:0] strg_ub_out_port_sel_addr_strides_1 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_1 = 14;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_2 = 58;
wire [15:0] strg_ub_output_addr_gen_strides_2 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_2 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_2 = 58;
wire [15:0] strg_ub_out_port_sel_addr_strides_2 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_2 = 58;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_2 = 58;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_3 = -1;
wire [15:0] strg_ub_output_addr_gen_strides_3 = -975;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_3 = -3838;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_3 = -1;
wire [15:0] strg_ub_out_port_sel_addr_strides_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_3 = -959;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_3 = -959;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_3 = -1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_2 = 58;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_3 = -3839;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3 = -3839;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_2 = 58;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_3 = -3839;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3 = -3839;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_5 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_dimensionality = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_5 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_dimensionality = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_5 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_0 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_1 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_2 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_3 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_5 = 0;
assign LakeTop_addr_in[0] = addr_in_0;
assign LakeTop_addr_in[1] = addr_in_1;
assign LakeTop_chain_data_in[0] = chain_data_in_0;
assign LakeTop_chain_data_in[1] = chain_data_in_1;
assign config_data_out_0 = LakeTop_config_data_out[0];
assign config_data_out_1 = LakeTop_config_data_out[1];
assign LakeTop_data_in[0] = data_in_0;
assign LakeTop_data_in[1] = data_in_1;
assign data_out_0 = LakeTop_data_out[0];
assign data_out_1 = LakeTop_data_out[1];
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[0] = strg_ub_agg_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[1] = strg_ub_agg_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[2] = strg_ub_agg_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[3] = strg_ub_agg_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[4] = strg_ub_agg_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[5] = strg_ub_agg_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[0] = strg_ub_agg_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[1] = strg_ub_agg_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[2] = strg_ub_agg_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[3] = strg_ub_agg_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[4] = strg_ub_agg_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[5] = strg_ub_agg_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[0] = strg_ub_agg_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[1] = strg_ub_agg_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[2] = strg_ub_agg_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[3] = strg_ub_agg_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[4] = strg_ub_agg_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[5] = strg_ub_agg_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[0] = strg_ub_agg_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[1] = strg_ub_agg_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[2] = strg_ub_agg_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[3] = strg_ub_agg_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[4] = strg_ub_agg_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[5] = strg_ub_agg_write_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_input_addr_gen_strides[0] = strg_ub_input_addr_gen_strides_0;
assign LakeTop_strg_ub_input_addr_gen_strides[1] = strg_ub_input_addr_gen_strides_1;
assign LakeTop_strg_ub_input_addr_gen_strides[2] = strg_ub_input_addr_gen_strides_2;
assign LakeTop_strg_ub_input_addr_gen_strides[3] = strg_ub_input_addr_gen_strides_3;
assign LakeTop_strg_ub_input_addr_gen_strides[4] = strg_ub_input_addr_gen_strides_4;
assign LakeTop_strg_ub_input_addr_gen_strides[5] = strg_ub_input_addr_gen_strides_5;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[0] = strg_ub_input_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[1] = strg_ub_input_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[2] = strg_ub_input_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[3] = strg_ub_input_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[4] = strg_ub_input_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[5] = strg_ub_input_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[0] = strg_ub_loops_buf2out_autovec_read_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[1] = strg_ub_loops_buf2out_autovec_read_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[2] = strg_ub_loops_buf2out_autovec_read_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[3] = strg_ub_loops_buf2out_autovec_read_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[4] = strg_ub_loops_buf2out_autovec_read_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[5] = strg_ub_loops_buf2out_autovec_read_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[0] = strg_ub_loops_buf2out_autovec_write_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[1] = strg_ub_loops_buf2out_autovec_write_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[2] = strg_ub_loops_buf2out_autovec_write_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[3] = strg_ub_loops_buf2out_autovec_write_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[4] = strg_ub_loops_buf2out_autovec_write_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[5] = strg_ub_loops_buf2out_autovec_write_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[0] = strg_ub_loops_buf2out_autovec_write_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[1] = strg_ub_loops_buf2out_autovec_write_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[2] = strg_ub_loops_buf2out_autovec_write_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[3] = strg_ub_loops_buf2out_autovec_write_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[4] = strg_ub_loops_buf2out_autovec_write_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[5] = strg_ub_loops_buf2out_autovec_write_1_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[0] = strg_ub_loops_buf2out_out_sel_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[1] = strg_ub_loops_buf2out_out_sel_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[2] = strg_ub_loops_buf2out_out_sel_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[3] = strg_ub_loops_buf2out_out_sel_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[4] = strg_ub_loops_buf2out_out_sel_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[5] = strg_ub_loops_buf2out_out_sel_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[0] = strg_ub_loops_buf2out_read_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[1] = strg_ub_loops_buf2out_read_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[2] = strg_ub_loops_buf2out_read_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[3] = strg_ub_loops_buf2out_read_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[4] = strg_ub_loops_buf2out_read_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[5] = strg_ub_loops_buf2out_read_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[0] = strg_ub_loops_buf2out_read_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[1] = strg_ub_loops_buf2out_read_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[2] = strg_ub_loops_buf2out_read_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[3] = strg_ub_loops_buf2out_read_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[4] = strg_ub_loops_buf2out_read_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[5] = strg_ub_loops_buf2out_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[0] = strg_ub_loops_in2buf_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[1] = strg_ub_loops_in2buf_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[2] = strg_ub_loops_in2buf_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[3] = strg_ub_loops_in2buf_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[4] = strg_ub_loops_in2buf_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[5] = strg_ub_loops_in2buf_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[0] = strg_ub_loops_in2buf_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[1] = strg_ub_loops_in2buf_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[2] = strg_ub_loops_in2buf_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[3] = strg_ub_loops_in2buf_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[4] = strg_ub_loops_in2buf_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[5] = strg_ub_loops_in2buf_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[0] = strg_ub_loops_in2buf_autovec_read_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[1] = strg_ub_loops_in2buf_autovec_read_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[2] = strg_ub_loops_in2buf_autovec_read_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[3] = strg_ub_loops_in2buf_autovec_read_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[4] = strg_ub_loops_in2buf_autovec_read_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[5] = strg_ub_loops_in2buf_autovec_read_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[0] = strg_ub_loops_in2buf_autovec_read_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[1] = strg_ub_loops_in2buf_autovec_read_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[2] = strg_ub_loops_in2buf_autovec_read_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[3] = strg_ub_loops_in2buf_autovec_read_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[4] = strg_ub_loops_in2buf_autovec_read_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[5] = strg_ub_loops_in2buf_autovec_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[0] = strg_ub_loops_in2buf_autovec_write_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[1] = strg_ub_loops_in2buf_autovec_write_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[2] = strg_ub_loops_in2buf_autovec_write_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[3] = strg_ub_loops_in2buf_autovec_write_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[4] = strg_ub_loops_in2buf_autovec_write_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[5] = strg_ub_loops_in2buf_autovec_write_ranges_5;
assign LakeTop_strg_ub_out_port_sel_addr_strides[0] = strg_ub_out_port_sel_addr_strides_0;
assign LakeTop_strg_ub_out_port_sel_addr_strides[1] = strg_ub_out_port_sel_addr_strides_1;
assign LakeTop_strg_ub_out_port_sel_addr_strides[2] = strg_ub_out_port_sel_addr_strides_2;
assign LakeTop_strg_ub_out_port_sel_addr_strides[3] = strg_ub_out_port_sel_addr_strides_3;
assign LakeTop_strg_ub_out_port_sel_addr_strides[4] = strg_ub_out_port_sel_addr_strides_4;
assign LakeTop_strg_ub_out_port_sel_addr_strides[5] = strg_ub_out_port_sel_addr_strides_5;
assign LakeTop_strg_ub_output_addr_gen_strides[0] = strg_ub_output_addr_gen_strides_0;
assign LakeTop_strg_ub_output_addr_gen_strides[1] = strg_ub_output_addr_gen_strides_1;
assign LakeTop_strg_ub_output_addr_gen_strides[2] = strg_ub_output_addr_gen_strides_2;
assign LakeTop_strg_ub_output_addr_gen_strides[3] = strg_ub_output_addr_gen_strides_3;
assign LakeTop_strg_ub_output_addr_gen_strides[4] = strg_ub_output_addr_gen_strides_4;
assign LakeTop_strg_ub_output_addr_gen_strides[5] = strg_ub_output_addr_gen_strides_5;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[0] = strg_ub_output_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[1] = strg_ub_output_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[2] = strg_ub_output_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[3] = strg_ub_output_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[4] = strg_ub_output_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[5] = strg_ub_output_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_port_sel_addr_strides[0] = strg_ub_port_sel_addr_strides_0;
assign LakeTop_strg_ub_port_sel_addr_strides[1] = strg_ub_port_sel_addr_strides_1;
assign LakeTop_strg_ub_port_sel_addr_strides[2] = strg_ub_port_sel_addr_strides_2;
assign LakeTop_strg_ub_port_sel_addr_strides[3] = strg_ub_port_sel_addr_strides_3;
assign LakeTop_strg_ub_port_sel_addr_strides[4] = strg_ub_port_sel_addr_strides_4;
assign LakeTop_strg_ub_port_sel_addr_strides[5] = strg_ub_port_sel_addr_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[0] = strg_ub_tb_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[1] = strg_ub_tb_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[2] = strg_ub_tb_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[3] = strg_ub_tb_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[4] = strg_ub_tb_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[5] = strg_ub_tb_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[0] = strg_ub_tb_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[1] = strg_ub_tb_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[2] = strg_ub_tb_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[3] = strg_ub_tb_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[4] = strg_ub_tb_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[5] = strg_ub_tb_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[0] = strg_ub_tb_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[1] = strg_ub_tb_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[2] = strg_ub_tb_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[3] = strg_ub_tb_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[4] = strg_ub_tb_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[5] = strg_ub_tb_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[0] = strg_ub_tb_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[1] = strg_ub_tb_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[2] = strg_ub_tb_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[3] = strg_ub_tb_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[4] = strg_ub_tb_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[5] = strg_ub_tb_write_addr_gen_1_strides_5;
LakeTop LakeTop (
  .addr_in(LakeTop_addr_in),
  .chain_data_in(LakeTop_chain_data_in),
  .clk(clk),
  .clk_en(clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_en(config_en),
  .config_read(config_read),
  .config_write(config_write),
  .data_in(LakeTop_data_in),
  .fifo_ctrl_fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .mode(mode),
  .ren_in(ren_in),
  .rst_n(rst_n),
  .strg_ub_agg_read_addr_gen_0_starting_addr(strg_ub_agg_read_addr_gen_0_starting_addr),
  .strg_ub_agg_read_addr_gen_0_strides(LakeTop_strg_ub_agg_read_addr_gen_0_strides),
  .strg_ub_agg_read_addr_gen_1_starting_addr(strg_ub_agg_read_addr_gen_1_starting_addr),
  .strg_ub_agg_read_addr_gen_1_strides(LakeTop_strg_ub_agg_read_addr_gen_1_strides),
  .strg_ub_agg_write_addr_gen_0_starting_addr(strg_ub_agg_write_addr_gen_0_starting_addr),
  .strg_ub_agg_write_addr_gen_0_strides(LakeTop_strg_ub_agg_write_addr_gen_0_strides),
  .strg_ub_agg_write_addr_gen_1_starting_addr(strg_ub_agg_write_addr_gen_1_starting_addr),
  .strg_ub_agg_write_addr_gen_1_strides(LakeTop_strg_ub_agg_write_addr_gen_1_strides),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_input_addr_gen_starting_addr(strg_ub_input_addr_gen_starting_addr),
  .strg_ub_input_addr_gen_strides(LakeTop_strg_ub_input_addr_gen_strides),
  .strg_ub_input_sched_gen_sched_addr_gen_starting_addr(strg_ub_input_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_input_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides),
  .strg_ub_loops_buf2out_autovec_read_dimensionality(strg_ub_loops_buf2out_autovec_read_dimensionality),
  .strg_ub_loops_buf2out_autovec_read_ranges(LakeTop_strg_ub_loops_buf2out_autovec_read_ranges),
  .strg_ub_loops_buf2out_autovec_write_0_dimensionality(strg_ub_loops_buf2out_autovec_write_0_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_0_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges),
  .strg_ub_loops_buf2out_autovec_write_1_dimensionality(strg_ub_loops_buf2out_autovec_write_1_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_1_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges),
  .strg_ub_loops_buf2out_out_sel_dimensionality(strg_ub_loops_buf2out_out_sel_dimensionality),
  .strg_ub_loops_buf2out_out_sel_ranges(LakeTop_strg_ub_loops_buf2out_out_sel_ranges),
  .strg_ub_loops_buf2out_read_0_dimensionality(strg_ub_loops_buf2out_read_0_dimensionality),
  .strg_ub_loops_buf2out_read_0_ranges(LakeTop_strg_ub_loops_buf2out_read_0_ranges),
  .strg_ub_loops_buf2out_read_1_dimensionality(strg_ub_loops_buf2out_read_1_dimensionality),
  .strg_ub_loops_buf2out_read_1_ranges(LakeTop_strg_ub_loops_buf2out_read_1_ranges),
  .strg_ub_loops_in2buf_0_dimensionality(strg_ub_loops_in2buf_0_dimensionality),
  .strg_ub_loops_in2buf_0_ranges(LakeTop_strg_ub_loops_in2buf_0_ranges),
  .strg_ub_loops_in2buf_1_dimensionality(strg_ub_loops_in2buf_1_dimensionality),
  .strg_ub_loops_in2buf_1_ranges(LakeTop_strg_ub_loops_in2buf_1_ranges),
  .strg_ub_loops_in2buf_autovec_read_0_dimensionality(strg_ub_loops_in2buf_autovec_read_0_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_0_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges),
  .strg_ub_loops_in2buf_autovec_read_1_dimensionality(strg_ub_loops_in2buf_autovec_read_1_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_1_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges),
  .strg_ub_loops_in2buf_autovec_write_dimensionality(strg_ub_loops_in2buf_autovec_write_dimensionality),
  .strg_ub_loops_in2buf_autovec_write_ranges(LakeTop_strg_ub_loops_in2buf_autovec_write_ranges),
  .strg_ub_out_port_sel_addr_starting_addr(strg_ub_out_port_sel_addr_starting_addr),
  .strg_ub_out_port_sel_addr_strides(LakeTop_strg_ub_out_port_sel_addr_strides),
  .strg_ub_output_addr_gen_starting_addr(strg_ub_output_addr_gen_starting_addr),
  .strg_ub_output_addr_gen_strides(LakeTop_strg_ub_output_addr_gen_strides),
  .strg_ub_output_sched_gen_sched_addr_gen_starting_addr(strg_ub_output_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_output_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides),
  .strg_ub_port_sel_addr_starting_addr(strg_ub_port_sel_addr_starting_addr),
  .strg_ub_port_sel_addr_strides(LakeTop_strg_ub_port_sel_addr_strides),
  .strg_ub_tb_read_addr_gen_0_starting_addr(strg_ub_tb_read_addr_gen_0_starting_addr),
  .strg_ub_tb_read_addr_gen_0_strides(LakeTop_strg_ub_tb_read_addr_gen_0_strides),
  .strg_ub_tb_read_addr_gen_1_starting_addr(strg_ub_tb_read_addr_gen_1_starting_addr),
  .strg_ub_tb_read_addr_gen_1_strides(LakeTop_strg_ub_tb_read_addr_gen_1_strides),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_tb_write_addr_gen_0_starting_addr(strg_ub_tb_write_addr_gen_0_starting_addr),
  .strg_ub_tb_write_addr_gen_0_strides(LakeTop_strg_ub_tb_write_addr_gen_0_strides),
  .strg_ub_tb_write_addr_gen_1_starting_addr(strg_ub_tb_write_addr_gen_1_starting_addr),
  .strg_ub_tb_write_addr_gen_1_strides(LakeTop_strg_ub_tb_write_addr_gen_1_strides),
  .tile_en(tile_en),
  .wen_in(wen_in),
  .config_data_out(LakeTop_config_data_out),
  .data_out(LakeTop_data_out),
  .empty(empty),
  .full(full),
  .sram_ready_out(sram_ready_out)
);

endmodule   // LakeTop_W

module cwlib_ub__confignull__input_num1__output_num2__width16 ub_cim_stencil_op_hcompute_cim_stencil_55_to_cim_stencil_op_hcompute_cim_output_stencil_63 (
  input logic [15:0] addr_in_0,
  input logic [15:0] addr_in_1,
  input logic [15:0] chain_data_in_0,
  input logic [15:0] chain_data_in_1,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [15:0] data_in_0,
  input logic [15:0] data_in_1,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] wen_in,
  output logic [31:0] config_data_out_0,
  output logic [31:0] config_data_out_1,
  output logic [15:0] data_out_0,
  output logic [15:0] data_out_1,
  output logic empty,
  output logic full,
  output logic sram_ready_out
);

logic [1:0][15:0] LakeTop_addr_in;
logic [1:0][15:0] LakeTop_chain_data_in;
logic [1:0][31:0] LakeTop_config_data_out;
logic [1:0][15:0] LakeTop_data_in;
logic [1:0][15:0] LakeTop_data_out;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_read_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_out_sel_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_write_ranges;
logic [5:0][15:0] LakeTop_strg_ub_out_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_1_strides;
wire [15:0] strg_ub_agg_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_input_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_starting_addr = 265;
wire [3:0] strg_ub_loops_in2buf_autovec_read_0_dimensionality = 3;
wire [3:0] strg_ub_loops_in2buf_autovec_write_dimensionality = 3;
wire [15:0] strg_ub_output_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_out_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_starting_addr = 388;
wire [3:0] strg_ub_loops_buf2out_autovec_read_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_out_sel_dimensionality = 4;
wire [15:0] strg_ub_agg_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr = 262;
wire [3:0] strg_ub_loops_in2buf_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr = 390;
wire [3:0] strg_ub_loops_buf2out_read_0_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr = 390;
wire [3:0] strg_ub_loops_buf2out_read_1_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_1_dimensionality = 4;
wire [1:0] mode = 0;
wire [0:0] tile_en = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_0 = 2;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_1 = 13;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_1 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_2 = 58;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_2 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2 = 5;
wire [15:0] strg_ub_loops_in2buf_0_ranges_3 = -1;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_3 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3 = -3835;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_0 = 13;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_0 = 13;
wire [15:0] strg_ub_input_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_0 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_1 = 58;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_1 = 58;
wire [15:0] strg_ub_input_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_1 = 8;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_2 = -1;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_2 = -1;
wire [15:0] strg_ub_input_addr_gen_strides_2 = -899;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_2 = -3832;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_0 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_0 = 15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_0 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_0 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_0 = 1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_0 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_1 = 13;
wire [15:0] strg_ub_output_addr_gen_strides_1 = -14;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_1 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_1 = 13;
wire [15:0] strg_ub_out_port_sel_addr_strides_1 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_1 = 13;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_1 = 13;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_2 = 56;
wire [15:0] strg_ub_output_addr_gen_strides_2 = -14;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_2 = 6;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_2 = 56;
wire [15:0] strg_ub_out_port_sel_addr_strides_2 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_2 = 56;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_2 = 56;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_3 = -1;
wire [15:0] strg_ub_output_addr_gen_strides_3 = -884;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_3 = -3706;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_3 = -1;
wire [15:0] strg_ub_out_port_sel_addr_strides_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_3 = -869;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_3 = -869;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_3 = -1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_1 = 13;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_2 = 56;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2 = 5;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_3 = -3479;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3 = -3707;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_1 = 13;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_2 = 56;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2 = 5;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_3 = -3479;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3 = -3707;
wire [15:0] strg_ub_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_dimensionality = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_dimensionality = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_5 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_5 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_0 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_1 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_2 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_3 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_5 = 0;
assign LakeTop_addr_in[0] = addr_in_0;
assign LakeTop_addr_in[1] = addr_in_1;
assign LakeTop_chain_data_in[0] = chain_data_in_0;
assign LakeTop_chain_data_in[1] = chain_data_in_1;
assign config_data_out_0 = LakeTop_config_data_out[0];
assign config_data_out_1 = LakeTop_config_data_out[1];
assign LakeTop_data_in[0] = data_in_0;
assign LakeTop_data_in[1] = data_in_1;
assign data_out_0 = LakeTop_data_out[0];
assign data_out_1 = LakeTop_data_out[1];
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[0] = strg_ub_agg_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[1] = strg_ub_agg_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[2] = strg_ub_agg_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[3] = strg_ub_agg_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[4] = strg_ub_agg_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[5] = strg_ub_agg_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[0] = strg_ub_agg_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[1] = strg_ub_agg_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[2] = strg_ub_agg_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[3] = strg_ub_agg_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[4] = strg_ub_agg_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[5] = strg_ub_agg_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[0] = strg_ub_agg_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[1] = strg_ub_agg_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[2] = strg_ub_agg_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[3] = strg_ub_agg_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[4] = strg_ub_agg_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[5] = strg_ub_agg_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[0] = strg_ub_agg_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[1] = strg_ub_agg_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[2] = strg_ub_agg_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[3] = strg_ub_agg_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[4] = strg_ub_agg_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[5] = strg_ub_agg_write_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_input_addr_gen_strides[0] = strg_ub_input_addr_gen_strides_0;
assign LakeTop_strg_ub_input_addr_gen_strides[1] = strg_ub_input_addr_gen_strides_1;
assign LakeTop_strg_ub_input_addr_gen_strides[2] = strg_ub_input_addr_gen_strides_2;
assign LakeTop_strg_ub_input_addr_gen_strides[3] = strg_ub_input_addr_gen_strides_3;
assign LakeTop_strg_ub_input_addr_gen_strides[4] = strg_ub_input_addr_gen_strides_4;
assign LakeTop_strg_ub_input_addr_gen_strides[5] = strg_ub_input_addr_gen_strides_5;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[0] = strg_ub_input_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[1] = strg_ub_input_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[2] = strg_ub_input_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[3] = strg_ub_input_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[4] = strg_ub_input_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[5] = strg_ub_input_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[0] = strg_ub_loops_buf2out_autovec_read_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[1] = strg_ub_loops_buf2out_autovec_read_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[2] = strg_ub_loops_buf2out_autovec_read_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[3] = strg_ub_loops_buf2out_autovec_read_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[4] = strg_ub_loops_buf2out_autovec_read_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[5] = strg_ub_loops_buf2out_autovec_read_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[0] = strg_ub_loops_buf2out_autovec_write_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[1] = strg_ub_loops_buf2out_autovec_write_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[2] = strg_ub_loops_buf2out_autovec_write_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[3] = strg_ub_loops_buf2out_autovec_write_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[4] = strg_ub_loops_buf2out_autovec_write_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[5] = strg_ub_loops_buf2out_autovec_write_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[0] = strg_ub_loops_buf2out_autovec_write_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[1] = strg_ub_loops_buf2out_autovec_write_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[2] = strg_ub_loops_buf2out_autovec_write_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[3] = strg_ub_loops_buf2out_autovec_write_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[4] = strg_ub_loops_buf2out_autovec_write_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[5] = strg_ub_loops_buf2out_autovec_write_1_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[0] = strg_ub_loops_buf2out_out_sel_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[1] = strg_ub_loops_buf2out_out_sel_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[2] = strg_ub_loops_buf2out_out_sel_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[3] = strg_ub_loops_buf2out_out_sel_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[4] = strg_ub_loops_buf2out_out_sel_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[5] = strg_ub_loops_buf2out_out_sel_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[0] = strg_ub_loops_buf2out_read_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[1] = strg_ub_loops_buf2out_read_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[2] = strg_ub_loops_buf2out_read_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[3] = strg_ub_loops_buf2out_read_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[4] = strg_ub_loops_buf2out_read_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[5] = strg_ub_loops_buf2out_read_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[0] = strg_ub_loops_buf2out_read_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[1] = strg_ub_loops_buf2out_read_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[2] = strg_ub_loops_buf2out_read_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[3] = strg_ub_loops_buf2out_read_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[4] = strg_ub_loops_buf2out_read_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[5] = strg_ub_loops_buf2out_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[0] = strg_ub_loops_in2buf_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[1] = strg_ub_loops_in2buf_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[2] = strg_ub_loops_in2buf_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[3] = strg_ub_loops_in2buf_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[4] = strg_ub_loops_in2buf_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[5] = strg_ub_loops_in2buf_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[0] = strg_ub_loops_in2buf_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[1] = strg_ub_loops_in2buf_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[2] = strg_ub_loops_in2buf_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[3] = strg_ub_loops_in2buf_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[4] = strg_ub_loops_in2buf_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[5] = strg_ub_loops_in2buf_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[0] = strg_ub_loops_in2buf_autovec_read_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[1] = strg_ub_loops_in2buf_autovec_read_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[2] = strg_ub_loops_in2buf_autovec_read_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[3] = strg_ub_loops_in2buf_autovec_read_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[4] = strg_ub_loops_in2buf_autovec_read_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[5] = strg_ub_loops_in2buf_autovec_read_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[0] = strg_ub_loops_in2buf_autovec_read_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[1] = strg_ub_loops_in2buf_autovec_read_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[2] = strg_ub_loops_in2buf_autovec_read_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[3] = strg_ub_loops_in2buf_autovec_read_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[4] = strg_ub_loops_in2buf_autovec_read_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[5] = strg_ub_loops_in2buf_autovec_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[0] = strg_ub_loops_in2buf_autovec_write_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[1] = strg_ub_loops_in2buf_autovec_write_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[2] = strg_ub_loops_in2buf_autovec_write_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[3] = strg_ub_loops_in2buf_autovec_write_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[4] = strg_ub_loops_in2buf_autovec_write_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[5] = strg_ub_loops_in2buf_autovec_write_ranges_5;
assign LakeTop_strg_ub_out_port_sel_addr_strides[0] = strg_ub_out_port_sel_addr_strides_0;
assign LakeTop_strg_ub_out_port_sel_addr_strides[1] = strg_ub_out_port_sel_addr_strides_1;
assign LakeTop_strg_ub_out_port_sel_addr_strides[2] = strg_ub_out_port_sel_addr_strides_2;
assign LakeTop_strg_ub_out_port_sel_addr_strides[3] = strg_ub_out_port_sel_addr_strides_3;
assign LakeTop_strg_ub_out_port_sel_addr_strides[4] = strg_ub_out_port_sel_addr_strides_4;
assign LakeTop_strg_ub_out_port_sel_addr_strides[5] = strg_ub_out_port_sel_addr_strides_5;
assign LakeTop_strg_ub_output_addr_gen_strides[0] = strg_ub_output_addr_gen_strides_0;
assign LakeTop_strg_ub_output_addr_gen_strides[1] = strg_ub_output_addr_gen_strides_1;
assign LakeTop_strg_ub_output_addr_gen_strides[2] = strg_ub_output_addr_gen_strides_2;
assign LakeTop_strg_ub_output_addr_gen_strides[3] = strg_ub_output_addr_gen_strides_3;
assign LakeTop_strg_ub_output_addr_gen_strides[4] = strg_ub_output_addr_gen_strides_4;
assign LakeTop_strg_ub_output_addr_gen_strides[5] = strg_ub_output_addr_gen_strides_5;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[0] = strg_ub_output_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[1] = strg_ub_output_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[2] = strg_ub_output_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[3] = strg_ub_output_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[4] = strg_ub_output_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[5] = strg_ub_output_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_port_sel_addr_strides[0] = strg_ub_port_sel_addr_strides_0;
assign LakeTop_strg_ub_port_sel_addr_strides[1] = strg_ub_port_sel_addr_strides_1;
assign LakeTop_strg_ub_port_sel_addr_strides[2] = strg_ub_port_sel_addr_strides_2;
assign LakeTop_strg_ub_port_sel_addr_strides[3] = strg_ub_port_sel_addr_strides_3;
assign LakeTop_strg_ub_port_sel_addr_strides[4] = strg_ub_port_sel_addr_strides_4;
assign LakeTop_strg_ub_port_sel_addr_strides[5] = strg_ub_port_sel_addr_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[0] = strg_ub_tb_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[1] = strg_ub_tb_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[2] = strg_ub_tb_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[3] = strg_ub_tb_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[4] = strg_ub_tb_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[5] = strg_ub_tb_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[0] = strg_ub_tb_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[1] = strg_ub_tb_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[2] = strg_ub_tb_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[3] = strg_ub_tb_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[4] = strg_ub_tb_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[5] = strg_ub_tb_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[0] = strg_ub_tb_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[1] = strg_ub_tb_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[2] = strg_ub_tb_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[3] = strg_ub_tb_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[4] = strg_ub_tb_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[5] = strg_ub_tb_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[0] = strg_ub_tb_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[1] = strg_ub_tb_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[2] = strg_ub_tb_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[3] = strg_ub_tb_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[4] = strg_ub_tb_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[5] = strg_ub_tb_write_addr_gen_1_strides_5;
LakeTop LakeTop (
  .addr_in(LakeTop_addr_in),
  .chain_data_in(LakeTop_chain_data_in),
  .clk(clk),
  .clk_en(clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_en(config_en),
  .config_read(config_read),
  .config_write(config_write),
  .data_in(LakeTop_data_in),
  .fifo_ctrl_fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .mode(mode),
  .ren_in(ren_in),
  .rst_n(rst_n),
  .strg_ub_agg_read_addr_gen_0_starting_addr(strg_ub_agg_read_addr_gen_0_starting_addr),
  .strg_ub_agg_read_addr_gen_0_strides(LakeTop_strg_ub_agg_read_addr_gen_0_strides),
  .strg_ub_agg_read_addr_gen_1_starting_addr(strg_ub_agg_read_addr_gen_1_starting_addr),
  .strg_ub_agg_read_addr_gen_1_strides(LakeTop_strg_ub_agg_read_addr_gen_1_strides),
  .strg_ub_agg_write_addr_gen_0_starting_addr(strg_ub_agg_write_addr_gen_0_starting_addr),
  .strg_ub_agg_write_addr_gen_0_strides(LakeTop_strg_ub_agg_write_addr_gen_0_strides),
  .strg_ub_agg_write_addr_gen_1_starting_addr(strg_ub_agg_write_addr_gen_1_starting_addr),
  .strg_ub_agg_write_addr_gen_1_strides(LakeTop_strg_ub_agg_write_addr_gen_1_strides),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_input_addr_gen_starting_addr(strg_ub_input_addr_gen_starting_addr),
  .strg_ub_input_addr_gen_strides(LakeTop_strg_ub_input_addr_gen_strides),
  .strg_ub_input_sched_gen_sched_addr_gen_starting_addr(strg_ub_input_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_input_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides),
  .strg_ub_loops_buf2out_autovec_read_dimensionality(strg_ub_loops_buf2out_autovec_read_dimensionality),
  .strg_ub_loops_buf2out_autovec_read_ranges(LakeTop_strg_ub_loops_buf2out_autovec_read_ranges),
  .strg_ub_loops_buf2out_autovec_write_0_dimensionality(strg_ub_loops_buf2out_autovec_write_0_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_0_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges),
  .strg_ub_loops_buf2out_autovec_write_1_dimensionality(strg_ub_loops_buf2out_autovec_write_1_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_1_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges),
  .strg_ub_loops_buf2out_out_sel_dimensionality(strg_ub_loops_buf2out_out_sel_dimensionality),
  .strg_ub_loops_buf2out_out_sel_ranges(LakeTop_strg_ub_loops_buf2out_out_sel_ranges),
  .strg_ub_loops_buf2out_read_0_dimensionality(strg_ub_loops_buf2out_read_0_dimensionality),
  .strg_ub_loops_buf2out_read_0_ranges(LakeTop_strg_ub_loops_buf2out_read_0_ranges),
  .strg_ub_loops_buf2out_read_1_dimensionality(strg_ub_loops_buf2out_read_1_dimensionality),
  .strg_ub_loops_buf2out_read_1_ranges(LakeTop_strg_ub_loops_buf2out_read_1_ranges),
  .strg_ub_loops_in2buf_0_dimensionality(strg_ub_loops_in2buf_0_dimensionality),
  .strg_ub_loops_in2buf_0_ranges(LakeTop_strg_ub_loops_in2buf_0_ranges),
  .strg_ub_loops_in2buf_1_dimensionality(strg_ub_loops_in2buf_1_dimensionality),
  .strg_ub_loops_in2buf_1_ranges(LakeTop_strg_ub_loops_in2buf_1_ranges),
  .strg_ub_loops_in2buf_autovec_read_0_dimensionality(strg_ub_loops_in2buf_autovec_read_0_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_0_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges),
  .strg_ub_loops_in2buf_autovec_read_1_dimensionality(strg_ub_loops_in2buf_autovec_read_1_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_1_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges),
  .strg_ub_loops_in2buf_autovec_write_dimensionality(strg_ub_loops_in2buf_autovec_write_dimensionality),
  .strg_ub_loops_in2buf_autovec_write_ranges(LakeTop_strg_ub_loops_in2buf_autovec_write_ranges),
  .strg_ub_out_port_sel_addr_starting_addr(strg_ub_out_port_sel_addr_starting_addr),
  .strg_ub_out_port_sel_addr_strides(LakeTop_strg_ub_out_port_sel_addr_strides),
  .strg_ub_output_addr_gen_starting_addr(strg_ub_output_addr_gen_starting_addr),
  .strg_ub_output_addr_gen_strides(LakeTop_strg_ub_output_addr_gen_strides),
  .strg_ub_output_sched_gen_sched_addr_gen_starting_addr(strg_ub_output_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_output_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides),
  .strg_ub_port_sel_addr_starting_addr(strg_ub_port_sel_addr_starting_addr),
  .strg_ub_port_sel_addr_strides(LakeTop_strg_ub_port_sel_addr_strides),
  .strg_ub_tb_read_addr_gen_0_starting_addr(strg_ub_tb_read_addr_gen_0_starting_addr),
  .strg_ub_tb_read_addr_gen_0_strides(LakeTop_strg_ub_tb_read_addr_gen_0_strides),
  .strg_ub_tb_read_addr_gen_1_starting_addr(strg_ub_tb_read_addr_gen_1_starting_addr),
  .strg_ub_tb_read_addr_gen_1_strides(LakeTop_strg_ub_tb_read_addr_gen_1_strides),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_tb_write_addr_gen_0_starting_addr(strg_ub_tb_write_addr_gen_0_starting_addr),
  .strg_ub_tb_write_addr_gen_0_strides(LakeTop_strg_ub_tb_write_addr_gen_0_strides),
  .strg_ub_tb_write_addr_gen_1_starting_addr(strg_ub_tb_write_addr_gen_1_starting_addr),
  .strg_ub_tb_write_addr_gen_1_strides(LakeTop_strg_ub_tb_write_addr_gen_1_strides),
  .tile_en(tile_en),
  .wen_in(wen_in),
  .config_data_out(LakeTop_config_data_out),
  .data_out(LakeTop_data_out),
  .empty(empty),
  .full(full),
  .sram_ready_out(sram_ready_out)
);

endmodule   // LakeTop_W

module Chain (
  input logic [1:0] accessor_output,
  input logic [1:0] [15:0] chain_data_in,
  input logic clk_en,
  input logic [1:0] [15:0] curr_tile_data_out,
  input logic flush,
  output logic [1:0] [15:0] data_out_tile
);

always_comb begin
  if (accessor_output[0]) begin
    data_out_tile[0] = curr_tile_data_out[0];
  end
  else data_out_tile[0] = chain_data_in[0];
  if (accessor_output[1]) begin
    data_out_tile[1] = curr_tile_data_out[1];
  end
  else data_out_tile[1] = chain_data_in[1];
end
endmodule   // Chain

module LakeTop (
  input logic [1:0] [15:0] addr_in,
  input logic [1:0] [15:0] chain_data_in,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [1:0] [15:0] data_in,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  input logic [1:0] mode,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [15:0] strg_ub_agg_read_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] strg_ub_agg_read_addr_gen_0_strides,
  input logic [15:0] strg_ub_agg_read_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] strg_ub_agg_read_addr_gen_1_strides,
  input logic [15:0] strg_ub_agg_write_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] strg_ub_agg_write_addr_gen_0_strides,
  input logic [15:0] strg_ub_agg_write_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] strg_ub_agg_write_addr_gen_1_strides,
  input logic [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides,
  input logic [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides,
  input logic [15:0] strg_ub_input_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_input_addr_gen_strides,
  input logic [15:0] strg_ub_input_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides,
  input logic [3:0] strg_ub_loops_buf2out_autovec_read_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_buf2out_autovec_read_ranges,
  input logic [3:0] strg_ub_loops_buf2out_autovec_write_0_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges,
  input logic [3:0] strg_ub_loops_buf2out_autovec_write_1_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges,
  input logic [3:0] strg_ub_loops_buf2out_out_sel_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_buf2out_out_sel_ranges,
  input logic [3:0] strg_ub_loops_buf2out_read_0_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_buf2out_read_0_ranges,
  input logic [3:0] strg_ub_loops_buf2out_read_1_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_buf2out_read_1_ranges,
  input logic [3:0] strg_ub_loops_in2buf_0_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_in2buf_0_ranges,
  input logic [3:0] strg_ub_loops_in2buf_1_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_in2buf_1_ranges,
  input logic [3:0] strg_ub_loops_in2buf_autovec_read_0_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges,
  input logic [3:0] strg_ub_loops_in2buf_autovec_read_1_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges,
  input logic [3:0] strg_ub_loops_in2buf_autovec_write_dimensionality,
  input logic [5:0] [15:0] strg_ub_loops_in2buf_autovec_write_ranges,
  input logic [15:0] strg_ub_out_port_sel_addr_starting_addr,
  input logic [5:0] [15:0] strg_ub_out_port_sel_addr_strides,
  input logic [15:0] strg_ub_output_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_output_addr_gen_strides,
  input logic [15:0] strg_ub_output_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides,
  input logic [15:0] strg_ub_port_sel_addr_starting_addr,
  input logic [5:0] [15:0] strg_ub_port_sel_addr_strides,
  input logic [15:0] strg_ub_tb_read_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] strg_ub_tb_read_addr_gen_0_strides,
  input logic [15:0] strg_ub_tb_read_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] strg_ub_tb_read_addr_gen_1_strides,
  input logic [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides,
  input logic [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides,
  input logic [15:0] strg_ub_tb_write_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] strg_ub_tb_write_addr_gen_0_strides,
  input logic [15:0] strg_ub_tb_write_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] strg_ub_tb_write_addr_gen_1_strides,
  input logic tile_en,
  input logic [1:0] wen_in,
  output logic [1:0] [31:0] config_data_out,
  output logic [1:0] [15:0] data_out,
  output logic empty,
  output logic full,
  output logic sram_ready_out
);

logic [1:0] accessor_output;
logic [2:0][0:0][8:0] all_addr_to_mem;
logic [2:0][15:0] all_data_out;
logic [2:0][0:0][3:0][15:0] all_data_to_mem;
logic [2:0][0:0] all_ren_to_mem;
logic [2:0] all_valid_out;
logic [2:0][0:0] all_wen_to_mem;
logic cfg_seq_clk;
logic [15:0] config_data_in_shrt;
logic [1:0][15:0] config_data_out_shrt;
logic config_seq_clk;
logic config_seq_clk_en;
logic [1:0][15:0] data_out_tile;
logic [0:0][8:0] fifo_addr_to_mem;
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
logic [8:0] mem_0_mem_addr_in_bank;
logic mem_0_mem_cen_in_bank;
logic [3:0][15:0] mem_0_mem_data_in_bank;
logic [0:0][3:0][15:0] mem_0_mem_data_out_bank;
logic mem_0_mem_wen_in_bank;
logic [8:0] mem_addr_cfg;
logic [0:0][0:0][8:0] mem_addr_dp;
logic [0:0][0:0][8:0] mem_addr_in;
logic [0:0] mem_cen_dp;
logic [0:0] mem_cen_in;
logic [3:0][15:0] mem_data_cfg;
logic [0:0][0:0][3:0][15:0] mem_data_dp;
logic [0:0][0:0][3:0][15:0] mem_data_in;
logic [0:0][3:0][15:0] mem_data_low_pt;
logic [0:0][0:0][3:0][15:0] mem_data_out;
logic mem_ren_cfg;
logic mem_wen_cfg;
logic [0:0] mem_wen_dp;
logic [0:0] mem_wen_in;
logic [0:0][8:0] sram_addr_to_mem;
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
logic [0:0][0:0][8:0] ub_addr_to_mem;
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
assign cfg_seq_clk = gclk;
assign config_seq_clk = cfg_seq_clk;
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
  .clk(config_seq_clk),
  .clk_en(config_seq_clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in_shrt),
  .config_en(config_en),
  .config_rd(config_read),
  .config_wr(config_write),
  .flush(flush),
  .rd_data_stg(mem_data_low_pt),
  .rst_n(rst_n),
  .addr_out(mem_addr_cfg),
  .rd_data_out(config_data_out_shrt),
  .ren_out(mem_ren_cfg),
  .wen_out(mem_wen_cfg),
  .wr_data(mem_data_cfg)
);

strg_ub_vec strg_ub (
  .agg_read_addr_gen_0_starting_addr(strg_ub_agg_read_addr_gen_0_starting_addr),
  .agg_read_addr_gen_0_strides(strg_ub_agg_read_addr_gen_0_strides),
  .agg_read_addr_gen_1_starting_addr(strg_ub_agg_read_addr_gen_1_starting_addr),
  .agg_read_addr_gen_1_strides(strg_ub_agg_read_addr_gen_1_strides),
  .agg_write_addr_gen_0_starting_addr(strg_ub_agg_write_addr_gen_0_starting_addr),
  .agg_write_addr_gen_0_strides(strg_ub_agg_write_addr_gen_0_strides),
  .agg_write_addr_gen_1_starting_addr(strg_ub_agg_write_addr_gen_1_starting_addr),
  .agg_write_addr_gen_1_strides(strg_ub_agg_write_addr_gen_1_strides),
  .agg_write_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .agg_write_sched_gen_0_sched_addr_gen_strides(strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides),
  .agg_write_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .agg_write_sched_gen_1_sched_addr_gen_strides(strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides),
  .clk(strg_ub_clk),
  .clk_en(clk_en),
  .data_from_strg(mem_data_out),
  .data_in(data_in),
  .flush(flush),
  .input_addr_gen_starting_addr(strg_ub_input_addr_gen_starting_addr),
  .input_addr_gen_strides(strg_ub_input_addr_gen_strides),
  .input_sched_gen_sched_addr_gen_starting_addr(strg_ub_input_sched_gen_sched_addr_gen_starting_addr),
  .input_sched_gen_sched_addr_gen_strides(strg_ub_input_sched_gen_sched_addr_gen_strides),
  .loops_buf2out_autovec_read_dimensionality(strg_ub_loops_buf2out_autovec_read_dimensionality),
  .loops_buf2out_autovec_read_ranges(strg_ub_loops_buf2out_autovec_read_ranges),
  .loops_buf2out_autovec_write_0_dimensionality(strg_ub_loops_buf2out_autovec_write_0_dimensionality),
  .loops_buf2out_autovec_write_0_ranges(strg_ub_loops_buf2out_autovec_write_0_ranges),
  .loops_buf2out_autovec_write_1_dimensionality(strg_ub_loops_buf2out_autovec_write_1_dimensionality),
  .loops_buf2out_autovec_write_1_ranges(strg_ub_loops_buf2out_autovec_write_1_ranges),
  .loops_buf2out_out_sel_dimensionality(strg_ub_loops_buf2out_out_sel_dimensionality),
  .loops_buf2out_out_sel_ranges(strg_ub_loops_buf2out_out_sel_ranges),
  .loops_buf2out_read_0_dimensionality(strg_ub_loops_buf2out_read_0_dimensionality),
  .loops_buf2out_read_0_ranges(strg_ub_loops_buf2out_read_0_ranges),
  .loops_buf2out_read_1_dimensionality(strg_ub_loops_buf2out_read_1_dimensionality),
  .loops_buf2out_read_1_ranges(strg_ub_loops_buf2out_read_1_ranges),
  .loops_in2buf_0_dimensionality(strg_ub_loops_in2buf_0_dimensionality),
  .loops_in2buf_0_ranges(strg_ub_loops_in2buf_0_ranges),
  .loops_in2buf_1_dimensionality(strg_ub_loops_in2buf_1_dimensionality),
  .loops_in2buf_1_ranges(strg_ub_loops_in2buf_1_ranges),
  .loops_in2buf_autovec_read_0_dimensionality(strg_ub_loops_in2buf_autovec_read_0_dimensionality),
  .loops_in2buf_autovec_read_0_ranges(strg_ub_loops_in2buf_autovec_read_0_ranges),
  .loops_in2buf_autovec_read_1_dimensionality(strg_ub_loops_in2buf_autovec_read_1_dimensionality),
  .loops_in2buf_autovec_read_1_ranges(strg_ub_loops_in2buf_autovec_read_1_ranges),
  .loops_in2buf_autovec_write_dimensionality(strg_ub_loops_in2buf_autovec_write_dimensionality),
  .loops_in2buf_autovec_write_ranges(strg_ub_loops_in2buf_autovec_write_ranges),
  .out_port_sel_addr_starting_addr(strg_ub_out_port_sel_addr_starting_addr),
  .out_port_sel_addr_strides(strg_ub_out_port_sel_addr_strides),
  .output_addr_gen_starting_addr(strg_ub_output_addr_gen_starting_addr),
  .output_addr_gen_strides(strg_ub_output_addr_gen_strides),
  .output_sched_gen_sched_addr_gen_starting_addr(strg_ub_output_sched_gen_sched_addr_gen_starting_addr),
  .output_sched_gen_sched_addr_gen_strides(strg_ub_output_sched_gen_sched_addr_gen_strides),
  .port_sel_addr_starting_addr(strg_ub_port_sel_addr_starting_addr),
  .port_sel_addr_strides(strg_ub_port_sel_addr_strides),
  .rst_n(rst_n),
  .tb_read_addr_gen_0_starting_addr(strg_ub_tb_read_addr_gen_0_starting_addr),
  .tb_read_addr_gen_0_strides(strg_ub_tb_read_addr_gen_0_strides),
  .tb_read_addr_gen_1_starting_addr(strg_ub_tb_read_addr_gen_1_starting_addr),
  .tb_read_addr_gen_1_strides(strg_ub_tb_read_addr_gen_1_strides),
  .tb_read_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .tb_read_sched_gen_0_sched_addr_gen_strides(strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides),
  .tb_read_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .tb_read_sched_gen_1_sched_addr_gen_strides(strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides),
  .tb_write_addr_gen_0_starting_addr(strg_ub_tb_write_addr_gen_0_starting_addr),
  .tb_write_addr_gen_0_strides(strg_ub_tb_write_addr_gen_0_strides),
  .tb_write_addr_gen_1_starting_addr(strg_ub_tb_write_addr_gen_1_starting_addr),
  .tb_write_addr_gen_1_strides(strg_ub_tb_write_addr_gen_1_strides),
  .accessor_output(accessor_output),
  .addr_out(ub_addr_to_mem),
  .cen_to_strg(ub_cen_to_mem),
  .data_out(ub_data_out),
  .data_to_strg(ub_data_to_mem),
  .wen_to_strg(ub_wen_to_mem)
);

strg_ram sram_ctrl (
  .clk(sram_ctrl_clk),
  .clk_en(clk_en),
  .data_from_strg(mem_data_out),
  .data_in(sram_ctrl_data_in),
  .flush(flush),
  .rd_addr_in(sram_ctrl_rd_addr_in),
  .ren(sram_ctrl_ren),
  .rst_n(rst_n),
  .wen(sram_ctrl_wen),
  .wr_addr_in(sram_ctrl_wr_addr_in),
  .addr_out(sram_addr_to_mem),
  .data_out(sram_data_out),
  .data_to_strg(sram_data_to_mem),
  .ready(sram_ready_out),
  .ren_to_strg(sram_ren_to_mem),
  .valid_out(sram_valid_out),
  .wen_to_strg(sram_wen_to_mem)
);

strg_fifo fifo_ctrl (
  .clk(fifo_ctrl_clk),
  .clk_en(clk_en),
  .data_from_strg(mem_data_out),
  .data_in(fifo_ctrl_data_in),
  .fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .pop(fifo_ctrl_pop),
  .push(fifo_ctrl_push),
  .rst_n(rst_n),
  .addr_out(fifo_addr_to_mem),
  .data_out(fifo_data_out),
  .data_to_strg(fifo_data_to_mem),
  .empty(fifo_empty),
  .full(fifo_full),
  .ren_to_strg(fifo_ren_to_mem),
  .valid_out(fifo_valid_out),
  .wen_to_strg(fifo_wen_to_mem)
);

tsmc_name_generator mem_0 (
  .clk(mem_0_clk),
  .clk_en(mem_0_clk_en),
  .flush(flush),
  .mem_addr_in_bank(mem_0_mem_addr_in_bank),
  .mem_cen_in_bank(mem_0_mem_cen_in_bank),
  .mem_data_in_bank(mem_0_mem_data_in_bank),
  .mem_wen_in_bank(mem_0_mem_wen_in_bank),
  .rtsel(2'h0),
  .wtsel(2'h0),
  .mem_data_out_bank(mem_0_mem_data_out_bank)
);

Chain chain (
  .accessor_output(accessor_output),
  .chain_data_in(chain_data_in),
  .clk_en(clk_en),
  .curr_tile_data_out(data_out_tile),
  .flush(flush),
  .data_out_tile(data_out)
);

endmodule   // LakeTop

module cwlib_ub__confignull__input_num1__output_num2__width16 ub_padded16_global_wrapper_stencil_op_hcompute_padded16_global_wrapper_stencil_0_to_padded16_global_wrapper_stencil_op_hcompute_grad_x_stencil_49 (
  input logic [15:0] addr_in_0,
  input logic [15:0] addr_in_1,
  input logic [15:0] chain_data_in_0,
  input logic [15:0] chain_data_in_1,
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [31:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_read,
  input logic config_write,
  input logic [15:0] data_in_0,
  input logic [15:0] data_in_1,
  input logic [15:0] fifo_ctrl_fifo_depth,
  input logic flush,
  input logic [1:0] ren_in,
  input logic rst_n,
  input logic [1:0] wen_in,
  output logic [31:0] config_data_out_0,
  output logic [31:0] config_data_out_1,
  output logic [15:0] data_out_0,
  output logic [15:0] data_out_1,
  output logic empty,
  output logic full,
  output logic sram_ready_out
);

logic [1:0][15:0] LakeTop_addr_in;
logic [1:0][15:0] LakeTop_chain_data_in;
logic [1:0][31:0] LakeTop_config_data_out;
logic [1:0][15:0] LakeTop_data_in;
logic [1:0][15:0] LakeTop_data_out;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_read_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_out_sel_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_buf2out_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges;
logic [5:0][15:0] LakeTop_strg_ub_loops_in2buf_autovec_write_ranges;
logic [5:0][15:0] LakeTop_strg_ub_out_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_port_sel_addr_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_addr_gen_1_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_0_strides;
logic [5:0][15:0] LakeTop_strg_ub_tb_write_addr_gen_1_strides;
wire [15:0] strg_ub_agg_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_input_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_starting_addr = 3;
wire [3:0] strg_ub_loops_in2buf_autovec_read_0_dimensionality = 3;
wire [3:0] strg_ub_loops_in2buf_autovec_write_dimensionality = 3;
wire [15:0] strg_ub_output_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_out_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_starting_addr = 128;
wire [3:0] strg_ub_loops_buf2out_autovec_read_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_out_sel_dimensionality = 4;
wire [15:0] strg_ub_agg_write_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr = 0;
wire [3:0] strg_ub_loops_in2buf_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_0_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr = 130;
wire [3:0] strg_ub_loops_buf2out_read_0_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_0_dimensionality = 4;
wire [15:0] strg_ub_tb_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr = 130;
wire [3:0] strg_ub_loops_buf2out_read_1_dimensionality = 4;
wire [3:0] strg_ub_loops_buf2out_autovec_write_1_dimensionality = 4;
wire [1:0] mode = 0;
wire [0:0] tile_en = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_0 = 2;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_1 = 14;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_1 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_2 = 62;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_2 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_in2buf_0_ranges_3 = -1;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_3 = -3;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3 = -4095;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_0 = 14;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_0 = 14;
wire [15:0] strg_ub_input_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_0 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_1 = 62;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_1 = 62;
wire [15:0] strg_ub_input_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_1 = 4;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_2 = -1;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_2 = -1;
wire [15:0] strg_ub_input_addr_gen_strides_2 = -1023;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_2 = -4092;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_0 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_0 = 16;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_0 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_0 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_0 = 1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_0 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_1 = 14;
wire [15:0] strg_ub_output_addr_gen_strides_1 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_1 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_1 = 14;
wire [15:0] strg_ub_out_port_sel_addr_strides_1 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_1 = 14;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_2 = 60;
wire [15:0] strg_ub_output_addr_gen_strides_2 = -15;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_2 = 2;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_2 = 60;
wire [15:0] strg_ub_out_port_sel_addr_strides_2 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_2 = 60;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_2 = 60;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_3 = -1;
wire [15:0] strg_ub_output_addr_gen_strides_3 = -1007;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_3 = -3966;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_3 = -1;
wire [15:0] strg_ub_out_port_sel_addr_strides_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_3 = -991;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_3 = -991;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_3 = -1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_2 = 60;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_3 = -3967;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3 = -3967;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_0 = 2;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_0 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_1 = 14;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_1 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_2 = 60;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_2 = 1;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2 = 1;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_3 = -1;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_3 = -3967;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3 = -3967;
wire [15:0] strg_ub_out_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_out_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_out_sel_ranges_5 = 0;
wire [15:0] strg_ub_port_sel_addr_starting_addr = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_dimensionality = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_1_ranges_5 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_dimensionality = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_1_ranges_5 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_read_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_starting_addr = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_tb_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_0_ranges_5 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_write_ranges_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_read_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_0_ranges_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_0 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_1 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_2 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_3 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_1_strides_5 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_0 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_1 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_2 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_3 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_in2buf_autovec_read_1_ranges_5 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_sched_gen_sched_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_3 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_input_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_tb_read_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_4 = 0;
wire [15:0] strg_ub_agg_write_addr_gen_0_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_0_ranges_5 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_4 = 0;
wire [15:0] strg_ub_output_addr_gen_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_read_0_ranges_5 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_0 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_1 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_2 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_3 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_4 = 0;
wire [15:0] strg_ub_port_sel_addr_strides_5 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_4 = 0;
wire [15:0] strg_ub_loops_buf2out_autovec_write_1_ranges_5 = 0;
assign LakeTop_addr_in[0] = addr_in_0;
assign LakeTop_addr_in[1] = addr_in_1;
assign LakeTop_chain_data_in[0] = chain_data_in_0;
assign LakeTop_chain_data_in[1] = chain_data_in_1;
assign config_data_out_0 = LakeTop_config_data_out[0];
assign config_data_out_1 = LakeTop_config_data_out[1];
assign LakeTop_data_in[0] = data_in_0;
assign LakeTop_data_in[1] = data_in_1;
assign data_out_0 = LakeTop_data_out[0];
assign data_out_1 = LakeTop_data_out[1];
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[0] = strg_ub_agg_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[1] = strg_ub_agg_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[2] = strg_ub_agg_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[3] = strg_ub_agg_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[4] = strg_ub_agg_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_0_strides[5] = strg_ub_agg_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[0] = strg_ub_agg_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[1] = strg_ub_agg_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[2] = strg_ub_agg_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[3] = strg_ub_agg_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[4] = strg_ub_agg_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_read_addr_gen_1_strides[5] = strg_ub_agg_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[0] = strg_ub_agg_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[1] = strg_ub_agg_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[2] = strg_ub_agg_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[3] = strg_ub_agg_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[4] = strg_ub_agg_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_0_strides[5] = strg_ub_agg_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[0] = strg_ub_agg_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[1] = strg_ub_agg_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[2] = strg_ub_agg_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[3] = strg_ub_agg_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[4] = strg_ub_agg_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_agg_write_addr_gen_1_strides[5] = strg_ub_agg_write_addr_gen_1_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_input_addr_gen_strides[0] = strg_ub_input_addr_gen_strides_0;
assign LakeTop_strg_ub_input_addr_gen_strides[1] = strg_ub_input_addr_gen_strides_1;
assign LakeTop_strg_ub_input_addr_gen_strides[2] = strg_ub_input_addr_gen_strides_2;
assign LakeTop_strg_ub_input_addr_gen_strides[3] = strg_ub_input_addr_gen_strides_3;
assign LakeTop_strg_ub_input_addr_gen_strides[4] = strg_ub_input_addr_gen_strides_4;
assign LakeTop_strg_ub_input_addr_gen_strides[5] = strg_ub_input_addr_gen_strides_5;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[0] = strg_ub_input_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[1] = strg_ub_input_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[2] = strg_ub_input_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[3] = strg_ub_input_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[4] = strg_ub_input_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides[5] = strg_ub_input_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[0] = strg_ub_loops_buf2out_autovec_read_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[1] = strg_ub_loops_buf2out_autovec_read_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[2] = strg_ub_loops_buf2out_autovec_read_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[3] = strg_ub_loops_buf2out_autovec_read_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[4] = strg_ub_loops_buf2out_autovec_read_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_read_ranges[5] = strg_ub_loops_buf2out_autovec_read_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[0] = strg_ub_loops_buf2out_autovec_write_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[1] = strg_ub_loops_buf2out_autovec_write_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[2] = strg_ub_loops_buf2out_autovec_write_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[3] = strg_ub_loops_buf2out_autovec_write_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[4] = strg_ub_loops_buf2out_autovec_write_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges[5] = strg_ub_loops_buf2out_autovec_write_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[0] = strg_ub_loops_buf2out_autovec_write_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[1] = strg_ub_loops_buf2out_autovec_write_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[2] = strg_ub_loops_buf2out_autovec_write_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[3] = strg_ub_loops_buf2out_autovec_write_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[4] = strg_ub_loops_buf2out_autovec_write_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges[5] = strg_ub_loops_buf2out_autovec_write_1_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[0] = strg_ub_loops_buf2out_out_sel_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[1] = strg_ub_loops_buf2out_out_sel_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[2] = strg_ub_loops_buf2out_out_sel_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[3] = strg_ub_loops_buf2out_out_sel_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[4] = strg_ub_loops_buf2out_out_sel_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_out_sel_ranges[5] = strg_ub_loops_buf2out_out_sel_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[0] = strg_ub_loops_buf2out_read_0_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[1] = strg_ub_loops_buf2out_read_0_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[2] = strg_ub_loops_buf2out_read_0_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[3] = strg_ub_loops_buf2out_read_0_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[4] = strg_ub_loops_buf2out_read_0_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_0_ranges[5] = strg_ub_loops_buf2out_read_0_ranges_5;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[0] = strg_ub_loops_buf2out_read_1_ranges_0;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[1] = strg_ub_loops_buf2out_read_1_ranges_1;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[2] = strg_ub_loops_buf2out_read_1_ranges_2;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[3] = strg_ub_loops_buf2out_read_1_ranges_3;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[4] = strg_ub_loops_buf2out_read_1_ranges_4;
assign LakeTop_strg_ub_loops_buf2out_read_1_ranges[5] = strg_ub_loops_buf2out_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[0] = strg_ub_loops_in2buf_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[1] = strg_ub_loops_in2buf_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[2] = strg_ub_loops_in2buf_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[3] = strg_ub_loops_in2buf_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[4] = strg_ub_loops_in2buf_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_0_ranges[5] = strg_ub_loops_in2buf_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[0] = strg_ub_loops_in2buf_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[1] = strg_ub_loops_in2buf_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[2] = strg_ub_loops_in2buf_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[3] = strg_ub_loops_in2buf_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[4] = strg_ub_loops_in2buf_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_1_ranges[5] = strg_ub_loops_in2buf_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[0] = strg_ub_loops_in2buf_autovec_read_0_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[1] = strg_ub_loops_in2buf_autovec_read_0_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[2] = strg_ub_loops_in2buf_autovec_read_0_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[3] = strg_ub_loops_in2buf_autovec_read_0_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[4] = strg_ub_loops_in2buf_autovec_read_0_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges[5] = strg_ub_loops_in2buf_autovec_read_0_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[0] = strg_ub_loops_in2buf_autovec_read_1_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[1] = strg_ub_loops_in2buf_autovec_read_1_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[2] = strg_ub_loops_in2buf_autovec_read_1_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[3] = strg_ub_loops_in2buf_autovec_read_1_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[4] = strg_ub_loops_in2buf_autovec_read_1_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges[5] = strg_ub_loops_in2buf_autovec_read_1_ranges_5;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[0] = strg_ub_loops_in2buf_autovec_write_ranges_0;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[1] = strg_ub_loops_in2buf_autovec_write_ranges_1;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[2] = strg_ub_loops_in2buf_autovec_write_ranges_2;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[3] = strg_ub_loops_in2buf_autovec_write_ranges_3;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[4] = strg_ub_loops_in2buf_autovec_write_ranges_4;
assign LakeTop_strg_ub_loops_in2buf_autovec_write_ranges[5] = strg_ub_loops_in2buf_autovec_write_ranges_5;
assign LakeTop_strg_ub_out_port_sel_addr_strides[0] = strg_ub_out_port_sel_addr_strides_0;
assign LakeTop_strg_ub_out_port_sel_addr_strides[1] = strg_ub_out_port_sel_addr_strides_1;
assign LakeTop_strg_ub_out_port_sel_addr_strides[2] = strg_ub_out_port_sel_addr_strides_2;
assign LakeTop_strg_ub_out_port_sel_addr_strides[3] = strg_ub_out_port_sel_addr_strides_3;
assign LakeTop_strg_ub_out_port_sel_addr_strides[4] = strg_ub_out_port_sel_addr_strides_4;
assign LakeTop_strg_ub_out_port_sel_addr_strides[5] = strg_ub_out_port_sel_addr_strides_5;
assign LakeTop_strg_ub_output_addr_gen_strides[0] = strg_ub_output_addr_gen_strides_0;
assign LakeTop_strg_ub_output_addr_gen_strides[1] = strg_ub_output_addr_gen_strides_1;
assign LakeTop_strg_ub_output_addr_gen_strides[2] = strg_ub_output_addr_gen_strides_2;
assign LakeTop_strg_ub_output_addr_gen_strides[3] = strg_ub_output_addr_gen_strides_3;
assign LakeTop_strg_ub_output_addr_gen_strides[4] = strg_ub_output_addr_gen_strides_4;
assign LakeTop_strg_ub_output_addr_gen_strides[5] = strg_ub_output_addr_gen_strides_5;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[0] = strg_ub_output_sched_gen_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[1] = strg_ub_output_sched_gen_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[2] = strg_ub_output_sched_gen_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[3] = strg_ub_output_sched_gen_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[4] = strg_ub_output_sched_gen_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides[5] = strg_ub_output_sched_gen_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_port_sel_addr_strides[0] = strg_ub_port_sel_addr_strides_0;
assign LakeTop_strg_ub_port_sel_addr_strides[1] = strg_ub_port_sel_addr_strides_1;
assign LakeTop_strg_ub_port_sel_addr_strides[2] = strg_ub_port_sel_addr_strides_2;
assign LakeTop_strg_ub_port_sel_addr_strides[3] = strg_ub_port_sel_addr_strides_3;
assign LakeTop_strg_ub_port_sel_addr_strides[4] = strg_ub_port_sel_addr_strides_4;
assign LakeTop_strg_ub_port_sel_addr_strides[5] = strg_ub_port_sel_addr_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[0] = strg_ub_tb_read_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[1] = strg_ub_tb_read_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[2] = strg_ub_tb_read_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[3] = strg_ub_tb_read_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[4] = strg_ub_tb_read_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_0_strides[5] = strg_ub_tb_read_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[0] = strg_ub_tb_read_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[1] = strg_ub_tb_read_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[2] = strg_ub_tb_read_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[3] = strg_ub_tb_read_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[4] = strg_ub_tb_read_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_read_addr_gen_1_strides[5] = strg_ub_tb_read_addr_gen_1_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[0] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[1] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[2] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[3] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[4] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4;
assign LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides[5] = strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[0] = strg_ub_tb_write_addr_gen_0_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[1] = strg_ub_tb_write_addr_gen_0_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[2] = strg_ub_tb_write_addr_gen_0_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[3] = strg_ub_tb_write_addr_gen_0_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[4] = strg_ub_tb_write_addr_gen_0_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_0_strides[5] = strg_ub_tb_write_addr_gen_0_strides_5;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[0] = strg_ub_tb_write_addr_gen_1_strides_0;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[1] = strg_ub_tb_write_addr_gen_1_strides_1;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[2] = strg_ub_tb_write_addr_gen_1_strides_2;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[3] = strg_ub_tb_write_addr_gen_1_strides_3;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[4] = strg_ub_tb_write_addr_gen_1_strides_4;
assign LakeTop_strg_ub_tb_write_addr_gen_1_strides[5] = strg_ub_tb_write_addr_gen_1_strides_5;
LakeTop LakeTop (
  .addr_in(LakeTop_addr_in),
  .chain_data_in(LakeTop_chain_data_in),
  .clk(clk),
  .clk_en(clk_en),
  .config_addr_in(config_addr_in),
  .config_data_in(config_data_in),
  .config_en(config_en),
  .config_read(config_read),
  .config_write(config_write),
  .data_in(LakeTop_data_in),
  .fifo_ctrl_fifo_depth(fifo_ctrl_fifo_depth),
  .flush(flush),
  .mode(mode),
  .ren_in(ren_in),
  .rst_n(rst_n),
  .strg_ub_agg_read_addr_gen_0_starting_addr(strg_ub_agg_read_addr_gen_0_starting_addr),
  .strg_ub_agg_read_addr_gen_0_strides(LakeTop_strg_ub_agg_read_addr_gen_0_strides),
  .strg_ub_agg_read_addr_gen_1_starting_addr(strg_ub_agg_read_addr_gen_1_starting_addr),
  .strg_ub_agg_read_addr_gen_1_strides(LakeTop_strg_ub_agg_read_addr_gen_1_strides),
  .strg_ub_agg_write_addr_gen_0_starting_addr(strg_ub_agg_write_addr_gen_0_starting_addr),
  .strg_ub_agg_write_addr_gen_0_strides(LakeTop_strg_ub_agg_write_addr_gen_0_strides),
  .strg_ub_agg_write_addr_gen_1_starting_addr(strg_ub_agg_write_addr_gen_1_starting_addr),
  .strg_ub_agg_write_addr_gen_1_strides(LakeTop_strg_ub_agg_write_addr_gen_1_strides),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_input_addr_gen_starting_addr(strg_ub_input_addr_gen_starting_addr),
  .strg_ub_input_addr_gen_strides(LakeTop_strg_ub_input_addr_gen_strides),
  .strg_ub_input_sched_gen_sched_addr_gen_starting_addr(strg_ub_input_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_input_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_input_sched_gen_sched_addr_gen_strides),
  .strg_ub_loops_buf2out_autovec_read_dimensionality(strg_ub_loops_buf2out_autovec_read_dimensionality),
  .strg_ub_loops_buf2out_autovec_read_ranges(LakeTop_strg_ub_loops_buf2out_autovec_read_ranges),
  .strg_ub_loops_buf2out_autovec_write_0_dimensionality(strg_ub_loops_buf2out_autovec_write_0_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_0_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_0_ranges),
  .strg_ub_loops_buf2out_autovec_write_1_dimensionality(strg_ub_loops_buf2out_autovec_write_1_dimensionality),
  .strg_ub_loops_buf2out_autovec_write_1_ranges(LakeTop_strg_ub_loops_buf2out_autovec_write_1_ranges),
  .strg_ub_loops_buf2out_out_sel_dimensionality(strg_ub_loops_buf2out_out_sel_dimensionality),
  .strg_ub_loops_buf2out_out_sel_ranges(LakeTop_strg_ub_loops_buf2out_out_sel_ranges),
  .strg_ub_loops_buf2out_read_0_dimensionality(strg_ub_loops_buf2out_read_0_dimensionality),
  .strg_ub_loops_buf2out_read_0_ranges(LakeTop_strg_ub_loops_buf2out_read_0_ranges),
  .strg_ub_loops_buf2out_read_1_dimensionality(strg_ub_loops_buf2out_read_1_dimensionality),
  .strg_ub_loops_buf2out_read_1_ranges(LakeTop_strg_ub_loops_buf2out_read_1_ranges),
  .strg_ub_loops_in2buf_0_dimensionality(strg_ub_loops_in2buf_0_dimensionality),
  .strg_ub_loops_in2buf_0_ranges(LakeTop_strg_ub_loops_in2buf_0_ranges),
  .strg_ub_loops_in2buf_1_dimensionality(strg_ub_loops_in2buf_1_dimensionality),
  .strg_ub_loops_in2buf_1_ranges(LakeTop_strg_ub_loops_in2buf_1_ranges),
  .strg_ub_loops_in2buf_autovec_read_0_dimensionality(strg_ub_loops_in2buf_autovec_read_0_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_0_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_0_ranges),
  .strg_ub_loops_in2buf_autovec_read_1_dimensionality(strg_ub_loops_in2buf_autovec_read_1_dimensionality),
  .strg_ub_loops_in2buf_autovec_read_1_ranges(LakeTop_strg_ub_loops_in2buf_autovec_read_1_ranges),
  .strg_ub_loops_in2buf_autovec_write_dimensionality(strg_ub_loops_in2buf_autovec_write_dimensionality),
  .strg_ub_loops_in2buf_autovec_write_ranges(LakeTop_strg_ub_loops_in2buf_autovec_write_ranges),
  .strg_ub_out_port_sel_addr_starting_addr(strg_ub_out_port_sel_addr_starting_addr),
  .strg_ub_out_port_sel_addr_strides(LakeTop_strg_ub_out_port_sel_addr_strides),
  .strg_ub_output_addr_gen_starting_addr(strg_ub_output_addr_gen_starting_addr),
  .strg_ub_output_addr_gen_strides(LakeTop_strg_ub_output_addr_gen_strides),
  .strg_ub_output_sched_gen_sched_addr_gen_starting_addr(strg_ub_output_sched_gen_sched_addr_gen_starting_addr),
  .strg_ub_output_sched_gen_sched_addr_gen_strides(LakeTop_strg_ub_output_sched_gen_sched_addr_gen_strides),
  .strg_ub_port_sel_addr_starting_addr(strg_ub_port_sel_addr_starting_addr),
  .strg_ub_port_sel_addr_strides(LakeTop_strg_ub_port_sel_addr_strides),
  .strg_ub_tb_read_addr_gen_0_starting_addr(strg_ub_tb_read_addr_gen_0_starting_addr),
  .strg_ub_tb_read_addr_gen_0_strides(LakeTop_strg_ub_tb_read_addr_gen_0_strides),
  .strg_ub_tb_read_addr_gen_1_starting_addr(strg_ub_tb_read_addr_gen_1_starting_addr),
  .strg_ub_tb_read_addr_gen_1_strides(LakeTop_strg_ub_tb_read_addr_gen_1_strides),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr(strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides(LakeTop_strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides),
  .strg_ub_tb_write_addr_gen_0_starting_addr(strg_ub_tb_write_addr_gen_0_starting_addr),
  .strg_ub_tb_write_addr_gen_0_strides(LakeTop_strg_ub_tb_write_addr_gen_0_strides),
  .strg_ub_tb_write_addr_gen_1_starting_addr(strg_ub_tb_write_addr_gen_1_starting_addr),
  .strg_ub_tb_write_addr_gen_1_strides(LakeTop_strg_ub_tb_write_addr_gen_1_strides),
  .tile_en(tile_en),
  .wen_in(wen_in),
  .config_data_out(LakeTop_config_data_out),
  .data_out(LakeTop_data_out),
  .empty(empty),
  .full(full),
  .sram_ready_out(sram_ready_out)
);

endmodule   // LakeTop_W

module addr_gen_6 (
  input logic clk,
  input logic clk_en,
  input logic flush,
  input logic [2:0] mux_sel,
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
      current_addr <= current_addr + strides[mux_sel];
    end
  end
end
endmodule   // addr_gen_6

module for_loop_6 (
  input logic clk,
  input logic clk_en,
  input logic [3:0] dimensionality,
  input logic flush,
  input logic [5:0] [15:0] ranges,
  input logic rst_n,
  input logic step,
  output logic [2:0] mux_sel_out
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
    if (~max_value[0]) begin
      mux_sel = 3'h0;
      done = 1'h1;
    end
  end
  if (~done) begin
    if (~max_value[1]) begin
      mux_sel = 3'h1;
      done = 1'h1;
    end
  end
  if (~done) begin
    if (~max_value[2]) begin
      mux_sel = 3'h2;
      done = 1'h1;
    end
  end
  if (~done) begin
    if (~max_value[3]) begin
      mux_sel = 3'h3;
      done = 1'h1;
    end
  end
  if (~done) begin
    if (~max_value[4]) begin
      mux_sel = 3'h4;
      done = 1'h1;
    end
  end
  if (~done) begin
    if (~max_value[5]) begin
      mux_sel = 3'h5;
      done = 1'h1;
    end
  end
end
always_comb begin
  clear[0] = 1'h0;
  if ((mux_sel > 3'h0) & step) begin
    clear[0] = 1'h1;
  end
end
always_comb begin
  inc[0] = 1'h0;
  if ((5'h0 == 5'h0) & step) begin
    inc[0] = 1'h1;
  end
  else if ((mux_sel == 3'h0) & step) begin
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
  if ((mux_sel > 3'h1) & step) begin
    clear[1] = 1'h1;
  end
end
always_comb begin
  inc[1] = 1'h0;
  if ((5'h1 == 5'h0) & step) begin
    inc[1] = 1'h1;
  end
  else if ((mux_sel == 3'h1) & step) begin
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
  if ((mux_sel > 3'h2) & step) begin
    clear[2] = 1'h1;
  end
end
always_comb begin
  inc[2] = 1'h0;
  if ((5'h2 == 5'h0) & step) begin
    inc[2] = 1'h1;
  end
  else if ((mux_sel == 3'h2) & step) begin
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
  if ((mux_sel > 3'h3) & step) begin
    clear[3] = 1'h1;
  end
end
always_comb begin
  inc[3] = 1'h0;
  if ((5'h3 == 5'h0) & step) begin
    inc[3] = 1'h1;
  end
  else if ((mux_sel == 3'h3) & step) begin
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
  if ((mux_sel > 3'h4) & step) begin
    clear[4] = 1'h1;
  end
end
always_comb begin
  inc[4] = 1'h0;
  if ((5'h4 == 5'h0) & step) begin
    inc[4] = 1'h1;
  end
  else if ((mux_sel == 3'h4) & step) begin
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
  if ((mux_sel > 3'h5) & step) begin
    clear[5] = 1'h1;
  end
end
always_comb begin
  inc[5] = 1'h0;
  if ((5'h5 == 5'h0) & step) begin
    inc[5] = 1'h1;
  end
  else if ((mux_sel == 3'h5) & step) begin
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
endmodule   // for_loop_6

module reg_fifo_d4_w1 #(
  parameter data_width = 16'h10
)
(
  input logic clk,
  input logic clk_en,
  input logic [0:0] [data_width-1:0] data_in,
  input logic flush,
  input logic [2:0] num_load,
  input logic [3:0][0:0] [data_width-1:0] parallel_in,
  input logic parallel_load,
  input logic parallel_read,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic [0:0] [data_width-1:0] data_out,
  output logic empty,
  output logic full,
  output logic [3:0][0:0] [data_width-1:0] parallel_out,
  output logic [1:0] rd_ptr_out,
  output logic valid
);

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
  end
  else if (flush) begin
    reg_array <= 64'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      reg_array <= parallel_in;
    end
    else if (write) begin
      if (parallel_read) begin
        reg_array[0] <= data_in;
      end
      else reg_array[wr_ptr] <= data_in;
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
  end
  else data_out = reg_array[rd_ptr];
end
always_comb begin
  valid = pop & ((~empty) | passthru);
end
endmodule   // reg_fifo_d4_w1

module reg_fifo_d4_w1_unq0 #(
  parameter data_width = 16'h10
)
(
  input logic clk,
  input logic clk_en,
  input logic [0:0] [data_width-1:0] data_in,
  input logic flush,
  input logic [2:0] num_load,
  input logic [3:0][0:0] [data_width-1:0] parallel_in,
  input logic parallel_load,
  input logic parallel_read,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic [0:0] [data_width-1:0] data_out,
  output logic empty,
  output logic full,
  output logic [3:0][0:0] [data_width-1:0] parallel_out,
  output logic valid
);

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
  end
  else if (flush) begin
    reg_array <= 64'h0;
  end
  else if (clk_en) begin
    if (parallel_load) begin
      reg_array <= parallel_in;
    end
    else if (write) begin
      if (parallel_read) begin
        reg_array[0] <= data_in;
      end
      else reg_array[wr_ptr] <= data_in;
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
  end
  else data_out = reg_array[rd_ptr];
end
always_comb begin
  valid = pop & ((~empty) | passthru);
end
endmodule   // reg_fifo_d4_w1_unq0

module sched_gen_6 (
  input logic clk,
  input logic clk_en,
  input logic [15:0] cycle_count,
  input logic flush,
  input logic [2:0] mux_sel,
  input logic rst_n,
  input logic [15:0] sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] sched_addr_gen_strides,
  output logic valid_output
);

logic [15:0] addr_out;
logic valid_out;
always_comb begin
  if (cycle_count[15:0] == addr_out[15:0]) begin
    valid_out = 1'h1;
  end
  else valid_out = 1'h0;
end
always_comb begin
  valid_output = valid_out;
end
addr_gen_6 sched_addr_gen (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(mux_sel),
  .rst_n(rst_n),
  .starting_addr(sched_addr_gen_starting_addr),
  .step(valid_out),
  .strides(sched_addr_gen_strides),
  .addr_out(addr_out)
);

endmodule   // sched_gen_6

module sram_stub (
  input logic [8:0] addr,
  input logic cen,
  input logic clk,
  input logic clk_en,
  input logic [3:0] [15:0] data_in,
  input logic flush,
  input logic wen,
  output logic [3:0] [15:0] data_out
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
  input logic clk,
  input logic clk_en,
  input logic [7:0] config_addr_in,
  input logic [15:0] config_data_in,
  input logic [1:0] config_en,
  input logic config_rd,
  input logic config_wr,
  input logic flush,
  input logic [0:0][3:0] [15:0] rd_data_stg,
  input logic rst_n,
  output logic [8:0] addr_out,
  output logic [1:0] [15:0] rd_data_out,
  output logic ren_out,
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
assign addr_out = {set_to_addr, config_addr_in};

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
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_from_strg,
  input logic [15:0] data_in,
  input logic [15:0] fifo_depth,
  input logic flush,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic [0:0] [8:0] addr_out,
  output logic [15:0] data_out,
  output logic [0:0][3:0] [15:0] data_to_strg,
  output logic empty,
  output logic full,
  output logic ren_to_strg,
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
logic [15:0] num_items;
logic [15:0] num_words_mem;
logic prev_bank_rd;
logic queued_write;
logic [0:0][8:0] ren_addr;
logic ren_delay;
logic [0:0][8:0] wen_addr;
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
    wen_addr[0] <= 9'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      wen_addr[0] <= 9'h0;
    end
    else if (wen_to_strg) begin
      wen_addr[0] <= wen_addr[0] + 9'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    ren_addr[0] <= 9'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      ren_addr[0] <= 9'h0;
    end
    else if (ren_to_strg) begin
      ren_addr[0] <= ren_addr[0] + 9'h1;
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
  .flush(flush),
  .num_load(3'h0),
  .parallel_in(64'h0),
  .parallel_load(1'h0),
  .parallel_read(front_par_read),
  .pop(front_pop),
  .push(front_push),
  .rst_n(rst_n),
  .data_out(front_rf_data_out),
  .empty(front_empty),
  .full(front_full),
  .parallel_out(front_par_out),
  .rd_ptr_out(front_rd_ptr),
  .valid(front_valid)
);

reg_fifo_d4_w1_unq0 #(
  .data_width(16'h10)) back_rf (
  .clk(clk),
  .clk_en(clk_en),
  .data_in(back_rf_data_in),
  .flush(flush),
  .num_load(back_num_load),
  .parallel_in(back_par_in),
  .parallel_load(back_rf_parallel_load),
  .parallel_read(1'h0),
  .pop(back_pop),
  .push(back_push),
  .rst_n(rst_n),
  .data_out(back_rf_data_out),
  .empty(back_empty),
  .full(back_full),
  .valid(back_valid)
);

endmodule   // strg_fifo

module strg_ram (
  input logic clk,
  input logic clk_en,
  input logic [0:0][3:0] [15:0] data_from_strg,
  input logic [15:0] data_in,
  input logic flush,
  input logic [15:0] rd_addr_in,
  input logic ren,
  input logic rst_n,
  input logic wen,
  input logic [15:0] wr_addr_in,
  output logic [0:0] [8:0] addr_out,
  output logic [15:0] data_out,
  output logic [0:0][3:0] [15:0] data_to_strg,
  output logic ready,
  output logic ren_to_strg,
  output logic valid_out,
  output logic wen_to_strg
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
  addr_out[0] = rd_addr[10:2];
  if (wen & (~write_gate)) begin
    addr_out[0] = wr_addr[10:2];
  end
  else if (write_gate) begin
    addr_out[0] = addr_to_write[10:2];
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
  r_w_seq_next_state = r_w_seq_current_state;
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
    MODIFY: if (1'h1) begin
      r_w_seq_next_state = IDLE;
    end
    READ: if ((~wen) & (~ren)) begin
      r_w_seq_next_state = IDLE;
    end
    else if (wen) begin
      r_w_seq_next_state = MODIFY;
    end
    else if (ren & (~wen)) begin
      r_w_seq_next_state = READ;
    end
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
  endcase
end
endmodule   // strg_ram

module strg_ub_vec (
  input logic [15:0] agg_read_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] agg_read_addr_gen_0_strides,
  input logic [15:0] agg_read_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] agg_read_addr_gen_1_strides,
  input logic [15:0] agg_write_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] agg_write_addr_gen_0_strides,
  input logic [15:0] agg_write_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] agg_write_addr_gen_1_strides,
  input logic [15:0] agg_write_sched_gen_0_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] agg_write_sched_gen_0_sched_addr_gen_strides,
  input logic [15:0] agg_write_sched_gen_1_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] agg_write_sched_gen_1_sched_addr_gen_strides,
  input logic clk,
  input logic clk_en,
  input logic [3:0] [15:0] data_from_strg,
  input logic [1:0] [15:0] data_in,
  input logic flush,
  input logic [15:0] input_addr_gen_starting_addr,
  input logic [5:0] [15:0] input_addr_gen_strides,
  input logic [15:0] input_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] input_sched_gen_sched_addr_gen_strides,
  input logic [3:0] loops_buf2out_autovec_read_dimensionality,
  input logic [5:0] [15:0] loops_buf2out_autovec_read_ranges,
  input logic [3:0] loops_buf2out_autovec_write_0_dimensionality,
  input logic [5:0] [15:0] loops_buf2out_autovec_write_0_ranges,
  input logic [3:0] loops_buf2out_autovec_write_1_dimensionality,
  input logic [5:0] [15:0] loops_buf2out_autovec_write_1_ranges,
  input logic [3:0] loops_buf2out_out_sel_dimensionality,
  input logic [5:0] [15:0] loops_buf2out_out_sel_ranges,
  input logic [3:0] loops_buf2out_read_0_dimensionality,
  input logic [5:0] [15:0] loops_buf2out_read_0_ranges,
  input logic [3:0] loops_buf2out_read_1_dimensionality,
  input logic [5:0] [15:0] loops_buf2out_read_1_ranges,
  input logic [3:0] loops_in2buf_0_dimensionality,
  input logic [5:0] [15:0] loops_in2buf_0_ranges,
  input logic [3:0] loops_in2buf_1_dimensionality,
  input logic [5:0] [15:0] loops_in2buf_1_ranges,
  input logic [3:0] loops_in2buf_autovec_read_0_dimensionality,
  input logic [5:0] [15:0] loops_in2buf_autovec_read_0_ranges,
  input logic [3:0] loops_in2buf_autovec_read_1_dimensionality,
  input logic [5:0] [15:0] loops_in2buf_autovec_read_1_ranges,
  input logic [3:0] loops_in2buf_autovec_write_dimensionality,
  input logic [5:0] [15:0] loops_in2buf_autovec_write_ranges,
  input logic [15:0] out_port_sel_addr_starting_addr,
  input logic [5:0] [15:0] out_port_sel_addr_strides,
  input logic [15:0] output_addr_gen_starting_addr,
  input logic [5:0] [15:0] output_addr_gen_strides,
  input logic [15:0] output_sched_gen_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] output_sched_gen_sched_addr_gen_strides,
  input logic [15:0] port_sel_addr_starting_addr,
  input logic [5:0] [15:0] port_sel_addr_strides,
  input logic rst_n,
  input logic [15:0] tb_read_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] tb_read_addr_gen_0_strides,
  input logic [15:0] tb_read_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] tb_read_addr_gen_1_strides,
  input logic [15:0] tb_read_sched_gen_0_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] tb_read_sched_gen_0_sched_addr_gen_strides,
  input logic [15:0] tb_read_sched_gen_1_sched_addr_gen_starting_addr,
  input logic [5:0] [15:0] tb_read_sched_gen_1_sched_addr_gen_strides,
  input logic [15:0] tb_write_addr_gen_0_starting_addr,
  input logic [5:0] [15:0] tb_write_addr_gen_0_strides,
  input logic [15:0] tb_write_addr_gen_1_starting_addr,
  input logic [5:0] [15:0] tb_write_addr_gen_1_strides,
  output logic [1:0] accessor_output,
  output logic [8:0] addr_out,
  output logic cen_to_strg,
  output logic [1:0] [15:0] data_out,
  output logic [3:0] [15:0] data_to_strg,
  output logic wen_to_strg
);

logic [8:0] addr;
logic [1:0][3:0][3:0][15:0] agg;
logic [1:0][1:0] agg_read_addr;
logic [15:0] agg_read_addr_gen_0_addr_out;
logic agg_read_addr_gen_0_step;
logic [15:0] agg_read_addr_gen_1_addr_out;
logic agg_read_addr_gen_1_step;
logic [1:0][7:0] agg_read_addr_gen_out;
logic [1:0] agg_write;
logic [1:0][3:0] agg_write_addr;
logic [15:0] agg_write_addr_gen_0_addr_out;
logic agg_write_addr_gen_0_step;
logic [15:0] agg_write_addr_gen_1_addr_out;
logic agg_write_addr_gen_1_step;
logic agg_write_sched_gen_0_valid_output;
logic agg_write_sched_gen_1_valid_output;
logic [15:0] cycle_count;
logic input_port_sel_addr;
logic [2:0] loops_buf2out_autovec_read_mux_sel_out;
logic [2:0] loops_buf2out_autovec_write_0_mux_sel_out;
logic [2:0] loops_buf2out_autovec_write_1_mux_sel_out;
logic [2:0] loops_buf2out_out_sel_mux_sel_out;
logic [2:0] loops_buf2out_read_0_mux_sel_out;
logic loops_buf2out_read_0_step;
logic [2:0] loops_buf2out_read_1_mux_sel_out;
logic loops_buf2out_read_1_step;
logic [2:0] loops_in2buf_0_mux_sel_out;
logic loops_in2buf_0_step;
logic [2:0] loops_in2buf_1_mux_sel_out;
logic loops_in2buf_1_step;
logic [2:0] loops_in2buf_autovec_read_0_mux_sel_out;
logic loops_in2buf_autovec_read_0_step;
logic [2:0] loops_in2buf_autovec_read_1_mux_sel_out;
logic loops_in2buf_autovec_read_1_step;
logic [2:0] loops_in2buf_autovec_write_mux_sel_out;
logic [15:0] out_port_sel_addr_addr_out;
logic output_port_sel_addr;
logic [15:0] port_sel_addr_addr_out;
logic read;
logic [15:0] read_addr;
logic read_d1;
logic [3:0][15:0] sram_read_data;
logic [3:0][15:0] sram_write_data;
logic [1:0][1:0][3:0][15:0] tb;
logic [1:0] tb_read;
logic [1:0][15:0] tb_read_addr;
logic [15:0] tb_read_addr_gen_0_addr_out;
logic tb_read_addr_gen_0_step;
logic [15:0] tb_read_addr_gen_1_addr_out;
logic tb_read_addr_gen_1_step;
logic tb_read_sched_gen_0_valid_output;
logic tb_read_sched_gen_1_valid_output;
logic [1:0][15:0] tb_write_addr;
logic [15:0] tb_write_addr_gen_0_addr_out;
logic [15:0] tb_write_addr_gen_1_addr_out;
logic write;
logic [15:0] write_addr;

always_ff @(posedge clk, negedge rst_n) begin
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

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    read_d1 <= 1'h0;
  end
  else if (clk_en) begin
    if (flush) begin
      read_d1 <= 1'h0;
    end
    else read_d1 <= read;
  end
end
assign addr_out = addr;
assign data_to_strg = sram_write_data;
assign sram_read_data = data_from_strg;
assign wen_to_strg = write;
assign cen_to_strg = write | read;
assign loops_in2buf_0_step = agg_write[0];
assign agg_write_addr_gen_0_step = agg_write[0];
assign agg_write_addr[0] = agg_write_addr_gen_0_addr_out[3:0];
assign agg_write[0] = agg_write_sched_gen_0_valid_output;
assign loops_in2buf_autovec_read_0_step = write & (1'h0 == input_port_sel_addr);
assign agg_read_addr_gen_0_step = write & (1'h0 == input_port_sel_addr);
assign agg_read_addr_gen_out[0] = agg_read_addr_gen_0_addr_out[7:0];
assign agg_read_addr[0] = agg_read_addr_gen_out[0][1:0];
assign loops_in2buf_1_step = agg_write[1];
assign agg_write_addr_gen_1_step = agg_write[1];
assign agg_write_addr[1] = agg_write_addr_gen_1_addr_out[3:0];
assign agg_write[1] = agg_write_sched_gen_1_valid_output;
assign loops_in2buf_autovec_read_1_step = write & (1'h1 == input_port_sel_addr);
assign agg_read_addr_gen_1_step = write & (1'h1 == input_port_sel_addr);
assign agg_read_addr_gen_out[1] = agg_read_addr_gen_1_addr_out[7:0];
assign agg_read_addr[1] = agg_read_addr_gen_out[1][1:0];
assign input_port_sel_addr = port_sel_addr_addr_out[0];
assign accessor_output = tb_read;
assign tb_write_addr[0] = tb_write_addr_gen_0_addr_out;
assign loops_buf2out_read_0_step = tb_read[0];
assign tb_read_addr_gen_0_step = tb_read[0];
assign tb_read_addr[0] = tb_read_addr_gen_0_addr_out;
assign tb_read[0] = tb_read_sched_gen_0_valid_output;
assign tb_write_addr[1] = tb_write_addr_gen_1_addr_out;
assign loops_buf2out_read_1_step = tb_read[1];
assign tb_read_addr_gen_1_step = tb_read[1];
assign tb_read_addr[1] = tb_read_addr_gen_1_addr_out;
assign tb_read[1] = tb_read_sched_gen_1_valid_output;
assign output_port_sel_addr = out_port_sel_addr_addr_out[0];
always_comb begin
  if (write) begin
    addr = write_addr[8:0];
  end
  else addr = read_addr[8:0];
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (agg_write[0]) begin
      agg[0][agg_write_addr[0][3:2]][agg_write_addr[0][1:0]] <= data_in[0];
    end
  end
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (agg_write[1]) begin
      agg[1][agg_write_addr[1][3:2]][agg_write_addr[1][1:0]] <= data_in[1];
    end
  end
end
always_comb begin
  sram_write_data[0] = agg[input_port_sel_addr][agg_read_addr[input_port_sel_addr]][0];
  sram_write_data[1] = agg[input_port_sel_addr][agg_read_addr[input_port_sel_addr]][1];
  sram_write_data[2] = agg[input_port_sel_addr][agg_read_addr[input_port_sel_addr]][2];
  sram_write_data[3] = agg[input_port_sel_addr][agg_read_addr[input_port_sel_addr]][3];
end

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (read_d1) begin
      tb[output_port_sel_addr][tb_write_addr[output_port_sel_addr][0]] <= sram_read_data;
    end
  end
end
always_comb begin
  data_out[0] = tb[0][tb_read_addr[0][2]][tb_read_addr[0][1:0]];
end
always_comb begin
  data_out[1] = tb[1][tb_read_addr[1][2]][tb_read_addr[1][1:0]];
end
for_loop_6 loops_in2buf_0 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_in2buf_0_dimensionality),
  .flush(flush),
  .ranges(loops_in2buf_0_ranges),
  .rst_n(rst_n),
  .step(loops_in2buf_0_step),
  .mux_sel_out(loops_in2buf_0_mux_sel_out)
);

addr_gen_6 agg_write_addr_gen_0 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_in2buf_0_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(agg_write_addr_gen_0_starting_addr),
  .step(agg_write_addr_gen_0_step),
  .strides(agg_write_addr_gen_0_strides),
  .addr_out(agg_write_addr_gen_0_addr_out)
);

sched_gen_6 agg_write_sched_gen_0 (
  .clk(clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .flush(flush),
  .mux_sel(loops_in2buf_0_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(agg_write_sched_gen_0_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(agg_write_sched_gen_0_sched_addr_gen_strides),
  .valid_output(agg_write_sched_gen_0_valid_output)
);

for_loop_6 loops_in2buf_autovec_read_0 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_in2buf_autovec_read_0_dimensionality),
  .flush(flush),
  .ranges(loops_in2buf_autovec_read_0_ranges),
  .rst_n(rst_n),
  .step(loops_in2buf_autovec_read_0_step),
  .mux_sel_out(loops_in2buf_autovec_read_0_mux_sel_out)
);

addr_gen_6 agg_read_addr_gen_0 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_in2buf_autovec_read_0_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(agg_read_addr_gen_0_starting_addr),
  .step(agg_read_addr_gen_0_step),
  .strides(agg_read_addr_gen_0_strides),
  .addr_out(agg_read_addr_gen_0_addr_out)
);

for_loop_6 loops_in2buf_1 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_in2buf_1_dimensionality),
  .flush(flush),
  .ranges(loops_in2buf_1_ranges),
  .rst_n(rst_n),
  .step(loops_in2buf_1_step),
  .mux_sel_out(loops_in2buf_1_mux_sel_out)
);

addr_gen_6 agg_write_addr_gen_1 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_in2buf_1_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(agg_write_addr_gen_1_starting_addr),
  .step(agg_write_addr_gen_1_step),
  .strides(agg_write_addr_gen_1_strides),
  .addr_out(agg_write_addr_gen_1_addr_out)
);

sched_gen_6 agg_write_sched_gen_1 (
  .clk(clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .flush(flush),
  .mux_sel(loops_in2buf_1_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(agg_write_sched_gen_1_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(agg_write_sched_gen_1_sched_addr_gen_strides),
  .valid_output(agg_write_sched_gen_1_valid_output)
);

for_loop_6 loops_in2buf_autovec_read_1 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_in2buf_autovec_read_1_dimensionality),
  .flush(flush),
  .ranges(loops_in2buf_autovec_read_1_ranges),
  .rst_n(rst_n),
  .step(loops_in2buf_autovec_read_1_step),
  .mux_sel_out(loops_in2buf_autovec_read_1_mux_sel_out)
);

addr_gen_6 agg_read_addr_gen_1 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_in2buf_autovec_read_1_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(agg_read_addr_gen_1_starting_addr),
  .step(agg_read_addr_gen_1_step),
  .strides(agg_read_addr_gen_1_strides),
  .addr_out(agg_read_addr_gen_1_addr_out)
);

for_loop_6 loops_in2buf_autovec_write (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_in2buf_autovec_write_dimensionality),
  .flush(flush),
  .ranges(loops_in2buf_autovec_write_ranges),
  .rst_n(rst_n),
  .step(write),
  .mux_sel_out(loops_in2buf_autovec_write_mux_sel_out)
);

addr_gen_6 port_sel_addr (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_in2buf_autovec_write_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(port_sel_addr_starting_addr),
  .step(write),
  .strides(port_sel_addr_strides),
  .addr_out(port_sel_addr_addr_out)
);

addr_gen_6 input_addr_gen (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_in2buf_autovec_write_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(input_addr_gen_starting_addr),
  .step(write),
  .strides(input_addr_gen_strides),
  .addr_out(write_addr)
);

sched_gen_6 input_sched_gen (
  .clk(clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .flush(flush),
  .mux_sel(loops_in2buf_autovec_write_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(input_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(input_sched_gen_sched_addr_gen_strides),
  .valid_output(write)
);

for_loop_6 loops_buf2out_autovec_read (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_buf2out_autovec_read_dimensionality),
  .flush(flush),
  .ranges(loops_buf2out_autovec_read_ranges),
  .rst_n(rst_n),
  .step(read),
  .mux_sel_out(loops_buf2out_autovec_read_mux_sel_out)
);

addr_gen_6 output_addr_gen (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_buf2out_autovec_read_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(output_addr_gen_starting_addr),
  .step(read),
  .strides(output_addr_gen_strides),
  .addr_out(read_addr)
);

sched_gen_6 output_sched_gen (
  .clk(clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .flush(flush),
  .mux_sel(loops_buf2out_autovec_read_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(output_sched_gen_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(output_sched_gen_sched_addr_gen_strides),
  .valid_output(read)
);

for_loop_6 loops_buf2out_autovec_write_0 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_buf2out_autovec_write_0_dimensionality),
  .flush(flush),
  .ranges(loops_buf2out_autovec_write_0_ranges),
  .rst_n(rst_n),
  .step(read_d1),
  .mux_sel_out(loops_buf2out_autovec_write_0_mux_sel_out)
);

addr_gen_6 tb_write_addr_gen_0 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_buf2out_autovec_write_0_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(tb_write_addr_gen_0_starting_addr),
  .step(read_d1),
  .strides(tb_write_addr_gen_0_strides),
  .addr_out(tb_write_addr_gen_0_addr_out)
);

for_loop_6 loops_buf2out_read_0 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_buf2out_read_0_dimensionality),
  .flush(flush),
  .ranges(loops_buf2out_read_0_ranges),
  .rst_n(rst_n),
  .step(loops_buf2out_read_0_step),
  .mux_sel_out(loops_buf2out_read_0_mux_sel_out)
);

addr_gen_6 tb_read_addr_gen_0 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_buf2out_read_0_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(tb_read_addr_gen_0_starting_addr),
  .step(tb_read_addr_gen_0_step),
  .strides(tb_read_addr_gen_0_strides),
  .addr_out(tb_read_addr_gen_0_addr_out)
);

sched_gen_6 tb_read_sched_gen_0 (
  .clk(clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .flush(flush),
  .mux_sel(loops_buf2out_read_0_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(tb_read_sched_gen_0_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(tb_read_sched_gen_0_sched_addr_gen_strides),
  .valid_output(tb_read_sched_gen_0_valid_output)
);

for_loop_6 loops_buf2out_autovec_write_1 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_buf2out_autovec_write_1_dimensionality),
  .flush(flush),
  .ranges(loops_buf2out_autovec_write_1_ranges),
  .rst_n(rst_n),
  .step(read_d1),
  .mux_sel_out(loops_buf2out_autovec_write_1_mux_sel_out)
);

addr_gen_6 tb_write_addr_gen_1 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_buf2out_autovec_write_1_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(tb_write_addr_gen_1_starting_addr),
  .step(read_d1),
  .strides(tb_write_addr_gen_1_strides),
  .addr_out(tb_write_addr_gen_1_addr_out)
);

for_loop_6 loops_buf2out_read_1 (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_buf2out_read_1_dimensionality),
  .flush(flush),
  .ranges(loops_buf2out_read_1_ranges),
  .rst_n(rst_n),
  .step(loops_buf2out_read_1_step),
  .mux_sel_out(loops_buf2out_read_1_mux_sel_out)
);

addr_gen_6 tb_read_addr_gen_1 (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_buf2out_read_1_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(tb_read_addr_gen_1_starting_addr),
  .step(tb_read_addr_gen_1_step),
  .strides(tb_read_addr_gen_1_strides),
  .addr_out(tb_read_addr_gen_1_addr_out)
);

sched_gen_6 tb_read_sched_gen_1 (
  .clk(clk),
  .clk_en(clk_en),
  .cycle_count(cycle_count),
  .flush(flush),
  .mux_sel(loops_buf2out_read_1_mux_sel_out),
  .rst_n(rst_n),
  .sched_addr_gen_starting_addr(tb_read_sched_gen_1_sched_addr_gen_starting_addr),
  .sched_addr_gen_strides(tb_read_sched_gen_1_sched_addr_gen_strides),
  .valid_output(tb_read_sched_gen_1_valid_output)
);

for_loop_6 loops_buf2out_out_sel (
  .clk(clk),
  .clk_en(clk_en),
  .dimensionality(loops_buf2out_out_sel_dimensionality),
  .flush(flush),
  .ranges(loops_buf2out_out_sel_ranges),
  .rst_n(rst_n),
  .step(read_d1),
  .mux_sel_out(loops_buf2out_out_sel_mux_sel_out)
);

addr_gen_6 out_port_sel_addr (
  .clk(clk),
  .clk_en(clk_en),
  .flush(flush),
  .mux_sel(loops_buf2out_out_sel_mux_sel_out),
  .rst_n(rst_n),
  .starting_addr(out_port_sel_addr_starting_addr),
  .step(read_d1),
  .strides(out_port_sel_addr_strides),
  .addr_out(out_port_sel_addr_addr_out)
);

endmodule   // strg_ub_vec

everseFlatten (
  input logic [63:0] input_array,
  output logic [0:0][3:0] [15:0] output_array
);

always_comb begin
  output_array[0][0] = input_array[15:0];
  output_array[0][1] = input_array[31:16];
  output_array[0][2] = input_array[47:32];
  output_array[0][3] = input_array[63:48];
end
endmodule   // ReverseFlatten

module tsmc_name_generator (
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
logic mem_cen_in_bank_chain;
logic mem_inst_0_sram_cen;
logic mem_inst_0_sram_wen;
logic mem_wen_in_bank_chain;
logic [63:0] sram_mem_data_in_bank;
logic [63:0] sram_mem_data_out_bank;
assign mem_inst_0_sram_cen = ~mem_cen_in_bank_chain;
assign mem_inst_0_sram_wen = ~mem_wen_in_bank_chain;
always_comb begin
  mem_addr_to_sram = mem_addr_in_bank;
end
always_comb begin
  mem_wen_in_bank_chain = mem_wen_in_bank;
end
always_comb begin
  mem_cen_in_bank_chain = mem_cen_in_bank;
end

flattenND flatten_data_in_0 (
  .input_array(mem_data_in_bank),
  .output_array(sram_mem_data_in_bank)
);

TSMC mem_inst_0 (
  .A(mem_addr_to_sram),
  .CEB(mem_inst_0_sram_cen),
  .CLK(clk),
  .D(sram_mem_data_in_bank),
  .RTSEL(2'h1),
  .WEB(mem_inst_0_sram_wen),
  .WTSEL(2'h0),
  .Q(sram_mem_data_out_bank)
);

ReverseFlatten flatten_data_out_0 (
  .input_array(sram_mem_data_out_bank),
  .output_array(mem_data_out_bank)
);

endmodule   // TSMC_generator

module flattenND (
  input logic [0:0][3:0] [15:0] input_array,
  output logic [63:0] output_array
);

always_comb begin
  output_array[15:0] = input_array[0][0];
  output_array[31:16] = input_array[0][1];
  output_array[47:32] = input_array[0][2];
  output_array[63:48] = input_array[0][3];
end
endmodule   // flattenND
