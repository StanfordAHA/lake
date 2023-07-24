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
    parameter NUM_CYCLES = 1000;

    // config
    wire [31:0] conf_0;
    wire [31:0] conf_1;
    wire [31:0] conf_2;
    wire [7:0] conf_3; 
    assign {conf_3, conf_2, conf_1, conf_0} = {4'b1000, 4'b1000, 1'b1, 1'b0, 1'b0, 16'b0, 1'b0, 16'b0, 1'b0, 16'b0, 1'b0, 1'b0, 1'b0, 16'b0, 1'b1, 1'b1, 1'b0, 1'b1, 1'b0, 1'b0, 1'b0, 16'b0, 1'b1};
    /*
    buffet_capacity_log_0 = 4'b1000;
    buffet_capacity_log_1 = 4'b1000;
    buffet_tile_en = 1'b1;
    read_scanner_block_mode = 1'b0;
    read_scanner_block_dense = 1'b0;
    read_scanner_dim_size = 16'b0;
    read_scanner_do_repeat = 1'b0;
    read_scanner_innder_dim_offset = 16'b0;
    read_scanner_lookup = 1'b0;
    read_scanner_repeat_factor = 16'b0;
    read_scanner_repeat_outer_inner_n = 1'b0;
    read_scanner_root = 1'b0;
    read_scanner_spacc_mode = 1'b0;
    read_scanner_stop_lvl = 16'b0;
    read_scanner_tile_en = 1'b1;
    tile_en = 1'b1;
    write_scanner_block_mode = 1'b0;
    write_scanner_compressed = 1'b1;
    write_scanner_init_blank = 1'b0;
    write_scanner_lowest_level = 1'b0;
    write_scanner_spacc_mode = 1'b0;
    write_scanner_stop_lvl = 16'b0;
    write_scanner_tile_en = 1'b1;
    */

    wire [7:0] conf_addr_in;
    wire [31:0] conf_data_in;
    wire [3:0] conf_other;
    assign {conf_addr_in, conf_data_in, conf_other} = {8'b0, 32'b0, 4'b0};

    integer clk_count;
    integer start_write;
    integer write_eos;
    integer write_count;
    logic start_read;
    integer read_count;
    integer wait_gap = 10;
    integer DONE_TOKEN = 17'h10100;

    CoreCombiner_width_4_Smp #(

    ) dut (
    .CONFIG_SPACE_0(conf_0),
    .CONFIG_SPACE_1(conf_1),
    .CONFIG_SPACE_2(conf_2),
    .CONFIG_SPACE_3(conf_3),
    .MEM_input_width_17_num_0(pos_in_0),
    .MEM_input_width_17_num_0_valid(pos_in_0_valid & start_read == 1),
    .MEM_input_width_17_num_1(ws_addr),
    .MEM_input_width_17_num_1_valid(ws_addr_valid),
    .MEM_input_width_17_num_2(ws_blk),
    .MEM_input_width_17_num_2_valid(ws_blk_valid),
    .MEM_input_width_17_num_3(coord_in_0),
    .MEM_input_width_17_num_3_valid(coord_in_0_valid),
    .MEM_output_width_17_num_0_ready(rs_blk_ready),
    .MEM_output_width_17_num_1_ready(coord_out_ready),
    .MEM_output_width_17_num_2_ready(pos_out_0_ready),
    .clk(clk),
    .clk_en(clk_en),
    .config_addr_in(conf_addr_in),
    .config_data_in(conf_data_in),
    .config_en(conf_other[3:2]),
    .config_read(conf_other[1]),
    .config_write(conf_other[0]),
    .flush(flush),
    .rst_n(rst_n),
    .tile_en(tile_en),
    .MEM_input_width_17_num_0_ready(pos_in_0_ready),
    .MEM_input_width_17_num_1_ready(ws_addr_ready),
    .MEM_input_width_17_num_2_ready(ws_blk_ready),
    .MEM_input_width_17_num_3_ready(coord_in_0_ready),
    .MEM_output_width_17_num_0(rs_blk),
    .MEM_output_width_17_num_0_valid(rs_blk_valid),
    .MEM_output_width_17_num_1(coord_out),
    .MEM_output_width_17_num_1_valid(coord_out_valid),
    .MEM_output_width_17_num_2(pos_out_0),
    .MEM_output_width_17_num_2_valid(pos_out_0_valid),
    .config_data_out(config_out)
    );

    glb_write #(
        .FILE_NAME("coord_in_0.txt")
    ) coord_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_in_0),
        .ready(coord_in_0_ready),
        .valid(coord_in_0_valid),
        .done(done[0]),
        .flush(flush)
    );

    glb_write #(
        .FILE_NAME("pos_in_0.txt")
    ) pos_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(pos_in_0),
        .ready(pos_in_0_ready & start_read == 1),
        .valid(pos_in_0_valid),
        .done(done[1]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("coord_out.txt"),
        .TX_NUM(`TX_NUM_GLB)
    ) coord_out_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_out),
        .ready(coord_out_ready),
        .valid(coord_out_valid),
        .done(done[2]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("pos_out_0.txt"),
        .TX_NUM(`TX_NUM_GLB)
    ) pos_out_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(pos_out_0),
        .ready(pos_out_0_ready),
        .valid(pos_out_0_valid),
        .done(done[3]),
        .flush(flush)
    );

    // simulated clk signal, 10ns period
    initial begin
        clk_count = 0;
        start_write = 0;
        write_eos = 0;
        write_count = 0;
        start_read = 0;
        read_count = 0;

        clk = 0;
        clk_en = 1;
        rst_n = 0;
        tile_en = 1;
        flush = 0;

        #5 clk = 1;
        flush = 1;
        rst_n = 1;
        #5 clk = 0;
        flush = 0;

        for(integer i = 0; i < NUM_CYCLES * 2; i = i + 1) begin
            #5 clk = ~clk;

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

            if (clk && start_write == 2 && wait_gap == 0 && start_read == 0 && pos_in_0_valid) begin
                start_read = 1;
            end

            if (clk && start_read == 1 && ~done[2]) begin
                read_count += 1;
            end

        end
        $display("write cycle count: %0d", write_count);
        $display("read cycle count: %0d", read_count);
        $finish;
    
    end

endmodule
