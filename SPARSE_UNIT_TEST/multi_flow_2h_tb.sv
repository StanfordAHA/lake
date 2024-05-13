`timescale 1ns/1ns
`ifndef TX_NUM_0
`define TX_NUM_0 1
`endif
`ifndef SEG_MODE
`define SEG_MODE 1
`endif


module multi_flow_2h_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg vector_reduce_mode; 
    wire [63:0] cycle_count;
    reg seg_mode;

    wire [16:0] stream_in_0;
    wire stream_in_0_valid;
    wire stream_in_0_ready;
    wire [16:0] stream_in_1;
    wire stream_in_1_valid;
    wire stream_in_1_ready;
    wire [16:0] stream_in_2;
    wire stream_in_2_valid;
    wire stream_in_2_ready;
    wire [16:0] stream_in_3;
    wire stream_in_3_valid;
    wire stream_in_3_ready;
    wire [16:0] stream_in_4;
    wire stream_in_4_valid;
    wire stream_in_4_ready;
    wire [16:0] stream_in_5;
    wire stream_in_5_valid;
    wire stream_in_5_ready;
    wire [16:0] stream_in_6;
    wire stream_in_6_valid;
    wire stream_in_6_ready;
    wire [16:0] stream_in_7;
    wire stream_in_7_valid;
    wire stream_in_7_ready;
    wire [16:0] stream_in_8;
    wire stream_in_8_valid;
    wire stream_in_8_ready;
    wire [16:0] stream_out;
    wire stream_out_valid;
    wire stream_out_ready;

    reg [15:0] stream_id_0;
    reg [15:0] stream_id_1;
    reg [15:0] stream_id_2;
    reg [15:0] stream_id_3;
    reg [15:0] stream_id_4;

    // wire for dut input & output
    wire [16:0] blk_in_0;
    wire blk_in_0_valid;
    wire blk_in_0_ready;
    wire [16:0] blk_in_1;
    wire blk_in_1_valid;
    wire blk_in_1_ready;
    wire [16:0] blk_in_2;
    wire blk_in_2_valid;
    wire blk_in_2_ready;
    wire [16:0] blk_in_3;
    wire blk_in_3_valid;
    wire blk_in_3_ready;
    wire [16:0] blk_in_4;
    wire blk_in_4_valid;
    wire blk_in_4_ready;

    // wire for mem
    wire [63:0] memory_0_data_in_p0;
    wire [63:0] memory_0_data_out_p0;
    wire [8:0] memory_addr_to_mem_p0;
    wire memory_0_read_enable_p0;
    wire memory_0_write_enable_p0;
    wire [63:0] memory_0_data_in_p1;
    wire [63:0] memory_0_data_out_p1;
    wire [8:0] memory_addr_to_mem_p1;
    wire memory_0_read_enable_p1;
    wire memory_0_write_enable_p1;
    wire [63:0] memory_0_data_in_p2;
    wire [63:0] memory_0_data_out_p2;
    wire [8:0] memory_addr_to_mem_p2;
    wire memory_0_read_enable_p2;
    wire memory_0_write_enable_p2;
    wire [63:0] memory_0_data_in_p3;
    wire [63:0] memory_0_data_out_p3;
    wire [8:0] memory_addr_to_mem_p3;
    wire memory_0_read_enable_p3;
    wire memory_0_write_enable_p3;
    wire [63:0] memory_0_data_in_p4;
    wire [63:0] memory_0_data_out_p4;
    wire [8:0] memory_addr_to_mem_p4;
    wire memory_0_read_enable_p4;
    wire memory_0_write_enable_p4;

    // dummy connection
    wire [16:0] ws_addr;
    wire ws_addr_valid;
    wire ws_addr_ready;

    wire [16:0] ws_blk;
    wire ws_blk_valid;
    wire ws_blk_ready;
    wire ws_blk_ready_0, ws_blk_ready_1, ws_blk_ready_2, ws_blk_ready_3, ws_blk_ready_4;

    assign ws_blk_ready = ws_blk_ready_0 & ws_blk_ready_1 & ws_blk_ready_2 & ws_blk_ready_3 & ws_blk_ready_4;

    wire [16:0] rs_blk;
    wire rs_blk_valid;
    wire rs_blk_ready;

    assign {ws_addr, ws_addr_valid, coord_in_0, coord_in_0_valid} = 0;
    assign {coord_out_ready, pos_out_0_ready} = 0;
    assign {pos_in_0, pos_in_0_valid} = 0;

    logic [1:0] [31:0] config_out;
    reg [1:0] num_requests_0, num_requests_1;

    wire [1:0] done;
    parameter NUM_CYCLES = 40000;

    integer clk_count;
    integer start_write;
    integer write_eos;
    integer write_count;
    logic start_read;
    logic read_input_in;
    integer read_count;
    integer DONE_TOKEN = 17'h10100;

    sram_sp memory_0 (
        .clk(clk),
        .clk_en(clk_en),
        .data_in_p0(memory_0_data_in_p0),
        .flush(flush),
        .read_addr_p0(memory_addr_to_mem_p0),
        .read_enable_p0(memory_0_read_enable_p0),
        .write_addr_p0(memory_addr_to_mem_p0),
        .write_enable_p0(memory_0_write_enable_p0),
        .data_out_p0(memory_0_data_out_p0)
    );

    sram_sp memory_1 (
        .clk(clk),
        .clk_en(clk_en),
        .data_in_p0(memory_0_data_in_p1),
        .flush(flush),
        .read_addr_p0(memory_addr_to_mem_p1),
        .read_enable_p0(memory_0_read_enable_p1),
        .write_addr_p0(memory_addr_to_mem_p1),
        .write_enable_p0(memory_0_write_enable_p1),
        .data_out_p0(memory_0_data_out_p1)
    );

    sram_sp memory_2 (
        .clk(clk),
        .clk_en(clk_en),
        .data_in_p0(memory_0_data_in_p2),
        .flush(flush),
        .read_addr_p0(memory_addr_to_mem_p2),
        .read_enable_p0(memory_0_read_enable_p2),
        .write_addr_p0(memory_addr_to_mem_p2),
        .write_enable_p0(memory_0_write_enable_p2),
        .data_out_p0(memory_0_data_out_p2)
    );

    sram_sp memory_3 (
        .clk(clk),
        .clk_en(clk_en),
        .data_in_p0(memory_0_data_in_p3),
        .flush(flush),
        .read_addr_p0(memory_addr_to_mem_p3),
        .read_enable_p0(memory_0_read_enable_p3),
        .write_addr_p0(memory_addr_to_mem_p3),
        .write_enable_p0(memory_0_write_enable_p3),
        .data_out_p0(memory_0_data_out_p3)
    );

    sram_sp memory_4 (
        .clk(clk),
        .clk_en(clk_en),
        .data_in_p0(memory_0_data_in_p4),
        .flush(flush),
        .read_addr_p0(memory_addr_to_mem_p4),
        .read_enable_p0(memory_0_read_enable_p4),
        .write_addr_p0(memory_addr_to_mem_p4),
        .write_enable_p0(memory_0_write_enable_p4),
        .data_out_p0(memory_0_data_out_p4)
    );

    fiber_access_16 FA_0
    (
    .buffet_buffet_capacity_log({4'b1000, 4'b1000}),
    .data_from_mem(memory_0_data_out_p0),
    .buffet_tile_en(tile_en),
    .clk(clk),
    .clk_en(clk_en),
    .flush(flush),
    .read_scanner_block_mode(1'b1),
    .read_scanner_block_rd_out_ready(stream_in_0_ready),
    .read_scanner_coord_out_ready(coord_out_ready),
    .read_scanner_dense(1'b0),
    .read_scanner_dim_size(16'b0),
    .read_scanner_do_repeat(1'b0),
    .read_scanner_glb_addr_base(16'd0),
    .read_scanner_glb_addr_stride(16'd50),
    .read_scanner_inner_dim_offset(16'b0),
    .read_scanner_lookup(~seg_mode),
    .read_scanner_pos_out_ready(pos_out_0_ready),
    .read_scanner_repeat_factor(16'b0),
    .read_scanner_repeat_outer_inner_n(1'b0),
    .read_scanner_root(1'b0),
    // .read_scanner_spacc_mode(1'b0),
    // .read_scanner_stop_lvl(16'b0),
    .read_scanner_tile_en(tile_en),
    .read_scanner_us_pos_in(pos_in_0),
    // .read_scanner_us_pos_in_valid(pos_in_0_valid & start_read == 1),
    .read_scanner_us_pos_in_valid(pos_in_0_valid),
    .rst_n(rst_n),
    .tile_en(tile_en),
    .write_scanner_addr_in(ws_addr),
    .write_scanner_addr_in_valid(ws_addr_valid),
    .write_scanner_block_mode(1'b1),
    .write_scanner_block_wr_in(ws_blk),
    .write_scanner_block_wr_in_valid(ws_blk_valid & ws_blk_ready),
    .write_scanner_compressed(1'b1),
    .write_scanner_data_in(coord_in_0),
    .write_scanner_data_in_valid(coord_in_0_valid),
    .write_scanner_init_blank(1'b0),
    .write_scanner_lowest_level(~seg_mode),
    .write_scanner_stream_id(stream_id_0),
    // .write_scanner_spacc_mode(1'b0),
    // .write_scanner_stop_lvl(16'b0),
    .write_scanner_tile_en(tile_en),
    .addr_to_mem(memory_addr_to_mem_p0),
    .data_to_mem(memory_0_data_in_p0),
    .read_scanner_block_rd_out(stream_in_0),
    .read_scanner_block_rd_out_valid(stream_in_0_valid),
    .read_scanner_coord_out(coord_out_0),
    .read_scanner_coord_out_valid(coord_out_valid_0),
    .read_scanner_pos_out(pos_out_0),
    .read_scanner_pos_out_valid(pos_out_0_valid_0),
    .read_scanner_us_pos_in_ready(pos_in_0_ready_0),
    .ren_to_mem(memory_0_read_enable_p0),
    .wen_to_mem(memory_0_write_enable_p0),
    .write_scanner_addr_in_ready(ws_addr_ready_0),
    .write_scanner_block_wr_in_ready(ws_blk_ready_0),
    .write_scanner_data_in_ready(coord_in_0_ready_0),
    .vector_reduce_mode(vector_reduce_mode)
    );

    fiber_access_16 FA_1
    (
    .buffet_buffet_capacity_log({4'b1000, 4'b1000}),
    .data_from_mem(memory_0_data_out_p1),
    .buffet_tile_en(tile_en),
    .clk(clk),
    .clk_en(clk_en),
    .flush(flush),
    .read_scanner_block_mode(1'b1),
    .read_scanner_block_rd_out_ready(stream_in_1_ready),
    .read_scanner_coord_out_ready(coord_out_ready),
    .read_scanner_dense(1'b0),
    .read_scanner_dim_size(16'b0),
    .read_scanner_do_repeat(1'b0),
    .read_scanner_glb_addr_base(16'd500),
    .read_scanner_glb_addr_stride(16'd50),
    .read_scanner_inner_dim_offset(16'b0),
    .read_scanner_lookup(~seg_mode),
    .read_scanner_pos_out_ready(pos_out_0_ready),
    .read_scanner_repeat_factor(16'b0),
    .read_scanner_repeat_outer_inner_n(1'b0),
    .read_scanner_root(1'b0),
    // .read_scanner_spacc_mode(1'b0),
    // .read_scanner_stop_lvl(16'b0),
    .read_scanner_tile_en(tile_en),
    .read_scanner_us_pos_in(pos_in_0),
    // .read_scanner_us_pos_in_valid(pos_in_0_valid & start_read == 1),
    .read_scanner_us_pos_in_valid(pos_in_0_valid),
    .rst_n(rst_n),
    .tile_en(tile_en),
    .write_scanner_addr_in(ws_addr),
    .write_scanner_addr_in_valid(ws_addr_valid),
    .write_scanner_block_mode(1'b1),
    .write_scanner_block_wr_in(ws_blk),
    .write_scanner_block_wr_in_valid(ws_blk_valid & ws_blk_ready),
    .write_scanner_compressed(1'b1),
    .write_scanner_data_in(coord_in_0),
    .write_scanner_data_in_valid(coord_in_0_valid),
    .write_scanner_init_blank(1'b0),
    .write_scanner_lowest_level(~seg_mode),
    .write_scanner_stream_id(stream_id_1),
    // .write_scanner_spacc_mode(1'b0),
    // .write_scanner_stop_lvl(16'b0),
    .write_scanner_tile_en(tile_en),
    .addr_to_mem(memory_addr_to_mem_p1),
    .data_to_mem(memory_0_data_in_p1),
    .read_scanner_block_rd_out(stream_in_1),
    .read_scanner_block_rd_out_valid(stream_in_1_valid),
    .read_scanner_coord_out(coord_out_1),
    .read_scanner_coord_out_valid(coord_out_valid_1),
    .read_scanner_pos_out(pos_out_1),
    .read_scanner_pos_out_valid(pos_out_0_valid_1),
    .read_scanner_us_pos_in_ready(pos_in_0_ready_1),
    .ren_to_mem(memory_0_read_enable_p1),
    .wen_to_mem(memory_0_write_enable_p1),
    .write_scanner_addr_in_ready(ws_addr_ready_1),
    .write_scanner_block_wr_in_ready(ws_blk_ready_1),
    .write_scanner_data_in_ready(coord_in_0_ready_1),
    .vector_reduce_mode(vector_reduce_mode)
    );

    fiber_access_16 FA_2
    (
    .buffet_buffet_capacity_log({4'b1000, 4'b1000}),
    .data_from_mem(memory_0_data_out_p2),
    .buffet_tile_en(tile_en),
    .clk(clk),
    .clk_en(clk_en),
    .flush(flush),
    .read_scanner_block_mode(1'b1),
    .read_scanner_block_rd_out_ready(stream_in_2_ready),
    .read_scanner_coord_out_ready(coord_out_ready),
    .read_scanner_dense(1'b0),
    .read_scanner_dim_size(16'b0),
    .read_scanner_do_repeat(1'b0),
    .read_scanner_glb_addr_base(16'd1000),
    .read_scanner_glb_addr_stride(16'd50),
    .read_scanner_inner_dim_offset(16'b0),
    .read_scanner_lookup(~seg_mode),
    .read_scanner_pos_out_ready(pos_out_0_ready),
    .read_scanner_repeat_factor(16'b0),
    .read_scanner_repeat_outer_inner_n(1'b0),
    .read_scanner_root(1'b0),
    // .read_scanner_spacc_mode(1'b0),
    // .read_scanner_stop_lvl(16'b0),
    .read_scanner_tile_en(tile_en),
    .read_scanner_us_pos_in(pos_in_0),
    // .read_scanner_us_pos_in_valid(pos_in_0_valid & start_read == 1),
    .read_scanner_us_pos_in_valid(pos_in_0_valid),
    .rst_n(rst_n),
    .tile_en(tile_en),
    .write_scanner_addr_in(ws_addr),
    .write_scanner_addr_in_valid(ws_addr_valid),
    .write_scanner_block_mode(1'b1),
    .write_scanner_block_wr_in(ws_blk),
    .write_scanner_block_wr_in_valid(ws_blk_valid & ws_blk_ready),
    .write_scanner_compressed(1'b1),
    .write_scanner_data_in(coord_in_0),
    .write_scanner_data_in_valid(coord_in_0_valid),
    .write_scanner_init_blank(1'b0),
    .write_scanner_lowest_level(~seg_mode),
    .write_scanner_stream_id(stream_id_2),
    // .write_scanner_spacc_mode(1'b0),
    // .write_scanner_stop_lvl(16'b0),
    .write_scanner_tile_en(tile_en),
    .addr_to_mem(memory_addr_to_mem_p2),
    .data_to_mem(memory_0_data_in_p2),
    .read_scanner_block_rd_out(stream_in_2),
    .read_scanner_block_rd_out_valid(stream_in_2_valid),
    .read_scanner_coord_out(coord_out_2),
    .read_scanner_coord_out_valid(coord_out_valid_2),
    .read_scanner_pos_out(pos_out_2),
    .read_scanner_pos_out_valid(pos_out_0_valid_2),
    .read_scanner_us_pos_in_ready(pos_in_0_ready_2),
    .ren_to_mem(memory_0_read_enable_p2),
    .wen_to_mem(memory_0_write_enable_p2),
    .write_scanner_addr_in_ready(ws_addr_ready_2),
    .write_scanner_block_wr_in_ready(ws_blk_ready_2),
    .write_scanner_data_in_ready(coord_in_0_ready_2),
    .vector_reduce_mode(vector_reduce_mode)
    );

    fiber_access_16 FA_3
    (
    .buffet_buffet_capacity_log({4'b1000, 4'b1000}),
    .data_from_mem(memory_0_data_out_p3),
    .buffet_tile_en(tile_en),
    .clk(clk),
    .clk_en(clk_en),
    .flush(flush),
    .read_scanner_block_mode(1'b1),
    .read_scanner_block_rd_out_ready(stream_in_3_ready),
    .read_scanner_coord_out_ready(coord_out_ready),
    .read_scanner_dense(1'b0),
    .read_scanner_dim_size(16'b0),
    .read_scanner_do_repeat(1'b0),
    .read_scanner_glb_addr_base(16'd1500),
    .read_scanner_glb_addr_stride(16'd50),
    .read_scanner_inner_dim_offset(16'b0),
    .read_scanner_lookup(~seg_mode),
    .read_scanner_pos_out_ready(pos_out_0_ready),
    .read_scanner_repeat_factor(16'b0),
    .read_scanner_repeat_outer_inner_n(1'b0),
    .read_scanner_root(1'b0),
    // .read_scanner_spacc_mode(1'b0),
    // .read_scanner_stop_lvl(16'b0),
    .read_scanner_tile_en(tile_en),
    .read_scanner_us_pos_in(pos_in_0),
    // .read_scanner_us_pos_in_valid(pos_in_0_valid & start_read == 1),
    .read_scanner_us_pos_in_valid(pos_in_0_valid),
    .rst_n(rst_n),
    .tile_en(tile_en),
    .write_scanner_addr_in(ws_addr),
    .write_scanner_addr_in_valid(ws_addr_valid),
    .write_scanner_block_mode(1'b1),
    .write_scanner_block_wr_in(ws_blk),
    .write_scanner_block_wr_in_valid(ws_blk_valid & ws_blk_ready),
    .write_scanner_compressed(1'b1),
    .write_scanner_data_in(coord_in_0),
    .write_scanner_data_in_valid(coord_in_0_valid),
    .write_scanner_init_blank(1'b0),
    .write_scanner_lowest_level(~seg_mode),
    .write_scanner_stream_id(stream_id_3),
    // .write_scanner_spacc_mode(1'b0),
    // .write_scanner_stop_lvl(16'b0),
    .write_scanner_tile_en(tile_en),
    .addr_to_mem(memory_addr_to_mem_p3),
    .data_to_mem(memory_0_data_in_p3),
    .read_scanner_block_rd_out(stream_in_3),
    .read_scanner_block_rd_out_valid(stream_in_3_valid),
    .read_scanner_coord_out(coord_out_3),
    .read_scanner_coord_out_valid(coord_out_valid_3),
    .read_scanner_pos_out(pos_out_3),
    .read_scanner_pos_out_valid(pos_out_0_valid_3),
    .read_scanner_us_pos_in_ready(pos_in_0_ready_3),
    .ren_to_mem(memory_0_read_enable_p3),
    .wen_to_mem(memory_0_write_enable_p3),
    .write_scanner_addr_in_ready(ws_addr_ready_3),
    .write_scanner_block_wr_in_ready(ws_blk_ready_3),
    .write_scanner_data_in_ready(coord_in_0_ready_3),
    .vector_reduce_mode(vector_reduce_mode)
    );

    fiber_access_16 FA_4
    (
    .buffet_buffet_capacity_log({4'b1000, 4'b1000}),
    .data_from_mem(memory_0_data_out_p4),
    .buffet_tile_en(tile_en),
    .clk(clk),
    .clk_en(clk_en),
    .flush(flush),
    .read_scanner_block_mode(1'b1),
    .read_scanner_block_rd_out_ready(stream_in_4_ready),
    .read_scanner_coord_out_ready(coord_out_ready),
    .read_scanner_dense(1'b0),
    .read_scanner_dim_size(16'b0),
    .read_scanner_do_repeat(1'b0),
    .read_scanner_glb_addr_base(16'd200),
    .read_scanner_glb_addr_stride(16'd50),
    .read_scanner_inner_dim_offset(16'b0),
    .read_scanner_lookup(~seg_mode),
    .read_scanner_pos_out_ready(pos_out_0_ready),
    .read_scanner_repeat_factor(16'b0),
    .read_scanner_repeat_outer_inner_n(1'b0),
    .read_scanner_root(1'b0),
    // .read_scanner_spacc_mode(1'b0),
    // .read_scanner_stop_lvl(16'b0),
    .read_scanner_tile_en(tile_en),
    .read_scanner_us_pos_in(pos_in_0),
    // .read_scanner_us_pos_in_valid(pos_in_0_valid & start_read == 1),
    .read_scanner_us_pos_in_valid(pos_in_0_valid),
    .rst_n(rst_n),
    .tile_en(tile_en),
    .write_scanner_addr_in(ws_addr),
    .write_scanner_addr_in_valid(ws_addr_valid),
    .write_scanner_block_mode(1'b1),
    .write_scanner_block_wr_in(ws_blk),
    .write_scanner_block_wr_in_valid(ws_blk_valid & ws_blk_ready),
    .write_scanner_compressed(1'b1),
    .write_scanner_data_in(coord_in_0),
    .write_scanner_data_in_valid(coord_in_0_valid),
    .write_scanner_init_blank(1'b0),
    .write_scanner_lowest_level(~seg_mode),
    .write_scanner_stream_id(stream_id_4),
    // .write_scanner_spacc_mode(1'b0),
    // .write_scanner_stop_lvl(16'b0),
    .write_scanner_tile_en(tile_en),
    .addr_to_mem(memory_addr_to_mem_p4),
    .data_to_mem(memory_0_data_in_p4),
    .read_scanner_block_rd_out(stream_in_4),
    .read_scanner_block_rd_out_valid(stream_in_4_valid),
    .read_scanner_coord_out(coord_out_4),
    .read_scanner_coord_out_valid(coord_out_valid_4),
    .read_scanner_pos_out(pos_out_4),
    .read_scanner_pos_out_valid(pos_out_0_valid_4),
    .read_scanner_us_pos_in_ready(pos_in_0_ready_4),
    .ren_to_mem(memory_0_read_enable_p4),
    .wen_to_mem(memory_0_write_enable_p4),
    .write_scanner_addr_in_ready(ws_addr_ready_4),
    .write_scanner_block_wr_in_ready(ws_blk_ready_4),
    .write_scanner_data_in_ready(coord_in_0_ready_4),
    .vector_reduce_mode(vector_reduce_mode)
    );

    stream_arbiter #(
    ) sa_0 (
        .clk(clk),
        .clk_en(clk_en),
        .flush(flush),
        .num_requests(num_requests_0),
        .rst_n(rst_n),
        .stream_in_0(stream_in_0),
        .stream_in_0_valid(stream_in_0_valid),
        .stream_in_1(stream_in_1),
        .stream_in_1_valid(stream_in_1_valid),
        .stream_in_2(stream_in_2),
        .stream_in_2_valid(stream_in_2_valid),
        .stream_in_3(stream_in_7),
        .stream_in_3_valid(stream_in_7_valid),
        .stream_out_ready(stream_out_ready),
        .tile_en(tile_en),
        .seg_mode(seg_mode),
        .stream_in_0_ready(stream_in_0_ready),
        .stream_in_1_ready(stream_in_1_ready),
        .stream_in_2_ready(stream_in_2_ready),
        .stream_in_3_ready(stream_in_7_ready),
        .stream_out(stream_out),
        .stream_out_valid(stream_out_valid)
    );

    stream_arbiter #(
    ) sa_1 (
        .clk(clk),
        .clk_en(clk_en),
        .flush(flush),
        .num_requests(num_requests_1),
        .rst_n(rst_n),
        .stream_in_0(stream_in_3),
        .stream_in_0_valid(stream_in_3_valid),
        .stream_in_1(stream_in_4),
        .stream_in_1_valid(stream_in_4_valid),
        .stream_in_2(stream_in_5),
        .stream_in_2_valid(stream_in_5_valid),
        .stream_in_3(stream_in_6),
        .stream_in_3_valid(stream_in_6_valid),
        .stream_out_ready(stream_in_7_ready),
        .tile_en(tile_en),
        .seg_mode(seg_mode),
        .stream_in_0_ready(stream_in_3_ready),
        .stream_in_1_ready(stream_in_4_ready),
        .stream_in_2_ready(stream_in_5_ready),
        .stream_in_3_ready(stream_in_6_ready),
        .stream_out(stream_in_7),
        .stream_out_valid(stream_in_7_valid)
    );

    glb_stream_write #(
        .FILE_NAME("stream_in_0.txt"),
        .TX_NUM(`TX_NUM_0),
        .RAN_SHITF(0)
    ) stream_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(ws_blk),
        .ready(ws_blk_ready),
        .valid(ws_blk_valid),
        .done(done[0]),
        .flush(flush),
        .seg_mode(seg_mode)
    );

    glb_stream_read #(
        .FILE_NAME("stream_out.txt"),
        .TX_NUM(`TX_NUM_0),
        .RAN_SHITF(0)
    ) stream_out_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(stream_out),
        .ready(stream_out_ready),
        .valid(stream_out_valid),
        .done(done[1]),
        .flush(flush),
        .seg_mode(seg_mode)
    );

    // simulated clk signal, 10ns period
    initial begin
        clk_count = 0;
        start_write = 0;
        write_eos = 0;
        write_count = 0;
        start_read = 0;
        read_input_in = 0;
        read_count = 0;
        seg_mode = `SEG_MODE;
        stream_id_0 = 2; // a magic number
        stream_id_1 = 3; // a magic number
        stream_id_2 = 4; // a magic number
        stream_id_3 = 5; // a magic number
        stream_id_4 = 6; // a magic number

        num_requests_0 = 3;
        num_requests_1 = 1;

        clk = 0;
        clk_en = 1;
        rst_n = 0;
        tile_en = 1;
        vector_reduce_mode = 0;
        flush = 0;

        #5 clk = 1;
        flush = 1;
        rst_n = 1;
        #5 clk = 0;
        flush = 0;

        for(integer i = 0; i < NUM_CYCLES * 2; i = i + 1) begin
            #5 clk = ~clk;
            
            if (clk && rs_blk_valid) begin
                read_input_in = 1;
            end

            // FSM
            if (clk && ws_blk_valid && start_write == 0) begin
                start_write = 1;
            end

            // DATA
            if (clk && start_write == 1 && ~done[0]) begin
                write_count += 1;
            end

            if (clk && start_read == 0 && read_input_in) begin
                start_read = 1;
            end

            if (clk && start_read == 1 && ~done[1]) begin
                read_count += 1;
            end

        end
        $display("write cycle count: %0d", write_count);
        $display("read cycle count: %0d", read_count);
        $finish;
    
    end

endmodule
