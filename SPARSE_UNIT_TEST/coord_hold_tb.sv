`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module coord_hold_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg [31:0] config_config_addr ;
    reg [31:0] config_config_data ;
    reg [0:0] config_read ;
    reg [0:0] config_write ;
    reg [0:0] joiner_op;
    wire [63:0] cycle_count ;

    // wire for dut input & output
    wire [16:0] coord_in_0;
    wire coord_in_0_valid;
    wire coord_in_0_ready;
    wire [16:0] coord_in_1;
    wire coord_in_1_valid;
    wire coord_in_1_ready;

    wire [16:0] pos_out_0;
    wire pos_out_0_valid;
    wire pos_out_0_ready;
    wire [16:0] pos_out_1;
    wire pos_out_1_valid;
    wire pos_out_1_ready;

    wire [3:0] done;
    parameter NUM_CYCLES = 1000;

    crdhold #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .cmrg_coord_in_0(coord_in_1),
        .cmrg_coord_in_0_valid(coord_in_1_valid),
        .cmrg_coord_in_1(coord_in_0),
        .cmrg_coord_in_1_valid(coord_in_0_valid),
        .cmrg_coord_out_0_ready(pos_out_0_ready),
        .cmrg_coord_out_1_ready(pos_out_1_ready),
        .cmrg_enable(1'b1), //not used
        .cmrg_stop_lvl(16'b0),
        .flush(flush),
        .rst_n(rst_n),
        .tile_en(tile_en),
        .cmrg_coord_in_0_ready(coord_in_1_ready),
        .cmrg_coord_in_1_ready(coord_in_0_ready),
        .cmrg_coord_out_0(pos_out_0),
        .cmrg_coord_out_0_valid(pos_out_0_valid),
        .cmrg_coord_out_1(pos_out_1),
        .cmrg_coord_out_1_valid(pos_out_1_valid)
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
        .FILE_NAME("coord_in_1.txt")
    ) coord_in_1_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_in_1),
        .ready(coord_in_1_ready),
        .valid(coord_in_1_valid),
        .done(done[1]),
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
        .done(done[2]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("pos_out_1.txt"),
        .TX_NUM(`TX_NUM_GLB)
    ) pos_out_1_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(pos_out_1),
        .ready(pos_out_1_ready),
        .valid(pos_out_1_valid),
        .done(done[3]),
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
        joiner_op = 1'b0;
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
            if (clk && start_record && ~(done[2] & done[3])) begin
                clk_count += 1;
            end

        end
        $display("cycle count: %0d", clk_count);
        $finish;
    
    end

endmodule
