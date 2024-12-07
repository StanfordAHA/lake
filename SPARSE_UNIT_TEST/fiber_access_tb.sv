`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module fiber_access_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg vector_reduce_mode; 
    wire [63:0] cycle_count ;

    // wire for dut input & output
    wire [16:0] coord_in_0;
    wire coord_in_0_valid;
    wire coord_in_0_ready;
    wire [16:0] pos_in_0;
    wire pos_in_0_valid;
    wire pos_in_0_ready;
    wire [16:0] coord_out;
    wire coord_out_valid;
    wire coord_out_ready;
    wire [16:0] pos_out_0;
    wire pos_out_0_valid;
    wire pos_out_0_ready;

    // wire for mem
    wire [63:0] memory_0_data_in_p0;
    wire [63:0] memory_0_data_out_p0;
    wire [8:0] memory_addr_to_mem_p0;
    wire memory_0_read_enable_p0;
    wire memory_0_write_enable_p0;

    // dummy connection
    wire [16:0] ws_addr;
    wire ws_addr_valid;
    wire ws_addr_ready;

    wire [16:0] ws_blk;
    wire ws_blk_valid;
    wire ws_blk_ready;

    wire [16:0] rs_blk;
    wire rs_blk_valid;
    wire rs_blk_ready;

    assign {ws_addr, ws_addr_valid, ws_blk, ws_blk_valid} = 35'b0;
    assign {rs_blk, rs_blk_valid} = 17'b0;

    logic [1:0] [31:0] config_out;

    wire [3:0] done;
    parameter NUM_CYCLES = 40000;

    integer clk_count;
    integer start_write;
    integer write_eos;
    integer write_count;
    logic start_read;
    logic read_input_in;
    integer read_count;
    integer wait_gap = 0; // should pass with arb gap
    integer DONE_TOKEN = 17'h10100;

    fiber_access_16 dut 
    (
    .buffet_buffet_capacity_log({4'b1000, 4'b1000}),
    .data_from_mem(memory_0_data_out_p0),
    .buffet_tile_en(tile_en),
    .clk(clk),
    .clk_en(clk_en),
    .flush(flush),
    .read_scanner_block_mode(1'b0),
    .read_scanner_block_rd_out_ready(rs_blk_ready),
    .read_scanner_coord_out_ready(coord_out_ready),
    .read_scanner_dense(1'b0),
    .read_scanner_do_repeat(1'b0),
    .read_scanner_inner_dim_offset(16'b0),
    .read_scanner_lookup(1'b0),
    .read_scanner_enable_locator_filter(1'b0),
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
    .write_scanner_block_mode(1'b0),
    .write_scanner_block_wr_in(ws_blk),
    .write_scanner_block_wr_in_valid(ws_blk_valid),
    .write_scanner_compressed(1'b1),
    .write_scanner_data_in(coord_in_0),
    .write_scanner_data_in_valid(coord_in_0_valid),
    .write_scanner_init_blank(1'b0),
    .write_scanner_lowest_level(1'b0),
    // .write_scanner_spacc_mode(1'b0),
    // .write_scanner_stop_lvl(16'b0),
    .write_scanner_tile_en(tile_en),
    .addr_to_mem(memory_addr_to_mem_p0),
    .data_to_mem(memory_0_data_in_p0),
    .read_scanner_block_rd_out(rs_blk),
    .read_scanner_block_rd_out_valid(rs_blk_valid),
    .read_scanner_coord_out(coord_out),
    .read_scanner_coord_out_valid(coord_out_valid),
    .read_scanner_pos_out(pos_out_0),
    .read_scanner_pos_out_valid(pos_out_0_valid),
    .read_scanner_us_pos_in_ready(pos_in_0_ready),
    .ren_to_mem(memory_0_read_enable_p0),
    .wen_to_mem(memory_0_write_enable_p0),
    .write_scanner_addr_in_ready(ws_addr_ready),
    .write_scanner_block_wr_in_ready(ws_blk_ready),
    .write_scanner_data_in_ready(coord_in_0_ready),
    .vector_reduce_mode(vector_reduce_mode)
    );

    tile_write #(
        .FILE_NAME("coord_in_0.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(0)
    ) coord_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_in_0),
        .ready(coord_in_0_ready),
        .valid(coord_in_0_valid),
        .done(done[0]),
        .flush(flush)
    );

    tile_write #(
        .FILE_NAME("pos_in_0.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
    ) pos_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(pos_in_0),
        // .ready(pos_in_0_ready & start_read == 1),
        .ready(pos_in_0_ready),
        .valid(pos_in_0_valid),
        .done(done[1]),
        .flush(flush)
    );

    tile_read #(
        .FILE_NAME("coord_out.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(2)
    ) coord_out_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_out),
        .ready(coord_out_ready),
        .valid(coord_out_valid),
        .done(done[2]),
        .flush(flush)
    );

    tile_read #(
        .FILE_NAME("pos_out_0.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(3)
    ) pos_out_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(pos_out_0),
        .ready(pos_out_0_ready),
        .valid(pos_out_0_valid),
        .done(done[3]),
        .flush(flush)
    );

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

    // simulated clk signal, 10ns period
    initial begin
        clk_count = 0;
        start_write = 0;
        write_eos = 0;
        write_count = 0;
        start_read = 0;
        read_input_in = 0;
        read_count = 0;

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
            
            if (clk && pos_in_0_valid) begin
                read_input_in = 1;
            end

            // FSM
            if (clk && coord_in_0_valid && start_write == 0) begin
                start_write = 1;
            end
            if (clk && start_write == 1 && coord_in_0 == DONE_TOKEN && coord_in_0_ready && coord_in_0_valid) begin
                write_eos = 1;
            end
            if (clk && start_write == 1 && write_eos && |{coord_in_0 != DONE_TOKEN, coord_in_0_ready, coord_in_0_valid}) begin
                write_eos = 0;
                start_write = 2;
            end

            // DATA
            if (clk && start_write == 1) begin
                write_count += 1;
            end

            if (clk && start_write == 2 && wait_gap > 0) begin
                wait_gap -= 1;
            end

            if (clk && start_write == 2 && wait_gap == 0 && start_read == 0 && read_input_in) begin
                start_read = 1;
            end

            if (clk && start_read == 1 && ~(done[2] & done[3])) begin
                read_count += 1;
            end

        end
        $display("write cycle count: %0d", write_count);
        $display("read cycle count: %0d", read_count);
        $finish;
    
    end

endmodule
