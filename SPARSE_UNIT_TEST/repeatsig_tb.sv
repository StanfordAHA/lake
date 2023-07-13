`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module repeatsig_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg [15:0] lvl;
    wire [63:0] cycle_count ;

    // wire for dut input & output
    wire [16:0] base_data_in;
    wire base_data_in_valid;
    wire base_data_in_ready;

    wire [16:0] repsig_data_out;
    wire repsig_data_out_valid;
    wire repsig_data_out_ready;

    wire [1:0] done;
    parameter NUM_CYCLES = 2048;

    RepeatSignalGenerator #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .base_data_in(base_data_in),
        .base_data_in_valid(base_data_in_valid),
        .base_data_in_ready(base_data_in_ready),
        .tile_en(tile_en),
        .repsig_data_out(repsig_data_out),
        .repsig_data_out_valid(repsig_data_out_valid),
        .repsig_data_out_ready(repsig_data_out_ready),
        .rst_n(rst_n),
        .flush(flush),
        .tile_en(tile_en),
        .stop_lvl(lvl)
    );

    glb_write #(
        .FILE_NAME("coord_in_0.txt")
    ) base_data_in_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(base_data_in),
        .ready(base_data_in_ready),
        .valid(base_data_in_valid),
        .done(done[0]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("coord_out.txt"),
        .TX_NUM(`TX_NUM_GLB)
    ) repsig_data_out_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(repsig_data_out),
        .ready(repsig_data_out_ready),
        .valid(repsig_data_out_valid),
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
            if (~start_record && clk && base_data_in_valid) begin
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
