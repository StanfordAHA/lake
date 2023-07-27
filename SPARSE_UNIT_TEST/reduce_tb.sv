`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module reduce_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg [15:0] lvl;
    wire [63:0] cycle_count ;

    // wire for dut input & output
    wire [16:0] val_in;
    wire val_in_valid;
    wire val_in_ready;

    wire [16:0] val_out;
    wire val_out_valid;
    wire val_out_ready;

    wire [1:0] done;
    parameter NUM_CYCLES = 1000;

    reg_cr #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .data_in(val_in),
        .data_in_valid(val_in_valid),
        .data_out_ready(val_out_ready),
        .default_value(16'b0),
        .flush(flush),
        .rst_n(rst_n),
        .stop_lvl(16'b0),
        .tile_en(tile_en),
        .data_in_ready(val_in_ready),
        .data_out(val_out),
        .data_out_valid(val_out_valid)
    );

    glb_write #(
        .FILE_NAME("coord_in_0.txt")
    ) val_in_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(val_in),
        .ready(val_in_ready),
        .valid(val_in_valid),
        .done(done[0]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("coord_out.txt"),
        .TX_NUM(`TX_NUM_GLB)
    ) val_out_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(val_out),
        .ready(val_out_ready),
        .valid(val_out_valid),
        .done(done[1]),
        .flush(flush)
    );


    integer start_record;
    integer clk_count;
    integer DONE_TOKEN = 17'h10100;

    // simulated clk signal, 10ns period
    initial begin
        start_record = 0;
        clk_count = 0;
        lvl = 0;

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
            if (~start_record && clk && val_in_valid) begin
                start_record = 1;
            end
            if (clk && start_record && ~done[1]) begin
                clk_count += 1;
            end

        end
        $display("cycle count: %0d", clk_count);
        $finish;
    
    end

endmodule
