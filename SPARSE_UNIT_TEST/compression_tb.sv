`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module compression_tb;

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
    wire [16:0] val_in;
    wire val_in_valid;
    wire val_in_ready;
    wire [16:0] coord_in;
    wire coord_in_valid;
    wire coord_in_ready;

    wire [16:0] val_out;
    wire val_out_valid;
    wire val_out_ready;
    wire [16:0] coord_out;
    wire coord_out_valid;
    wire coord_out_ready;

    wire [3:0] done;
    parameter NUM_CYCLES = 4000;

    crddrop #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .cmrg_coord_in_0(val_in),
        .cmrg_coord_in_0_valid(val_in_valid),
        .cmrg_coord_in_1(coord_in),
        .cmrg_coord_in_1_valid(coord_in_valid),
        .cmrg_coord_out_0_ready(val_out_ready),
        .cmrg_coord_out_1_ready(coord_out_ready),
        .cmrg_enable(1'b1), //not used
        .cmrg_stop_lvl(16'b0),
        .flush(flush),
        .rst_n(rst_n),
        .tile_en(tile_en),
        .cmrg_coord_in_0_ready(val_in_ready),
        .cmrg_coord_in_1_ready(coord_in_ready),
        .cmrg_coord_out_0(val_out),
        .cmrg_coord_out_0_valid(val_out_valid),
        .cmrg_coord_out_1(coord_out),
        .cmrg_coord_out_1_valid(coord_out_valid),
        .cmrg_mode(1'b0) // force the mode to perform drop
    );

    glb_write #(
        .FILE_NAME("val_in.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(0)
    ) coord_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(val_in),
        .ready(val_in_ready),
        .valid(val_in_valid),
        .done(done[0]),
        .flush(flush)
    );

    glb_write #(
        .FILE_NAME("coord_in.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
    ) coord_in_1_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_in),
        .ready(coord_in_ready),
        .valid(coord_in_valid),
        .done(done[1]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("val_out.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(2)
    ) pos_out_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(val_out),
        .ready(val_out_ready),
        .valid(val_out_valid),
        .done(done[2]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("coord_out.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(3)
    ) pos_out_1_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(coord_out),
        .ready(coord_out_ready),
        .valid(coord_out_valid),
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
            if (~start_record && clk && (val_in_valid | coord_in_valid)) begin
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
