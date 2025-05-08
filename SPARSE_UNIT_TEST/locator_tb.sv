`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif
`ifndef L_LEVEL
`define L_LEVEL 1
`endif
`ifndef DIM
`define DIM 10
`endif

module locator_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg [4:0] locate_dim_size;
    reg [15:0] locate_lvl;
    wire [63:0] cycle_count ;

    // wire for dut input & output
    wire [16:0] coord_in_0;
    wire coord_in_0_valid;
    wire coord_in_0_ready;
    wire [16:0] coord_in_1;
    wire coord_in_1_valid;
    wire coord_in_1_ready;
    wire [16:0] addr_out;
    wire addr_out_valid;
    wire addr_out_ready;

    wire [6:0] done;
    parameter NUM_CYCLES = 4000;

    locator #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .coord_in_0(coord_in_0),
        .coord_in_0_valid(coord_in_0_valid),
        .coord_in_0_ready(coord_in_0_ready),
        .coord_in_1(coord_in_1),
        .coord_in_1_valid(coord_in_1_valid),
        .coord_in_1_ready(coord_in_1_ready),
        .tile_en(tile_en),
        .addr_out(addr_out),
        .addr_out_valid(addr_out_valid),
        .addr_out_ready(addr_out_ready),
        .rst_n(rst_n),
        .flush(flush),
        .locate_dim_size(locate_dim_size),
        .locate_lvl(locate_lvl)
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
        .FILE_NAME("coord_in_1.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
    ) coord_in_1_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_in_1),
        .ready(coord_in_1_ready),
        .valid(coord_in_1_valid),
        .done(done[1]),
        .flush(flush)
    );

    tile_read #(
        .FILE_NAME("pos_out_0.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
    ) pos_out_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(addr_out),
        .ready(addr_out_ready),
        .valid(addr_out_valid),
        .done(done[5]),
        .flush(flush)
    );

    integer start_record;
    integer clk_count;
    integer DONE_TOKEN = 17'h10100;

    // simulated clk signal, 10ns period
    initial begin
        start_record = 0;
        clk_count = 0;

        clk = 0;
        locate_dim_size = `DIM;
        locate_lvl = `L_LEVEL;
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
            if (~start_record && clk && (coord_in_0_valid | coord_in_1_valid)) begin
                start_record = 1;
            end
            if (clk && start_record && ~done[5]) begin
                clk_count += 1;
            end

        end
        $display("cycle count: %0d", clk_count);
        $finish;
    
    end

endmodule
