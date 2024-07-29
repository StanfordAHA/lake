`timescale 1ns/1ns
`ifndef TX_NUM_0
`define TX_NUM_0 1
`endif
`ifndef SEG_MODE
`define SEG_MODE 1
`endif

module pass_through_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg seg_mode;

    wire [63:0] cycle_count;

    // wire for dut input & output
    wire [16:0] stream_in_0;
    wire stream_in_0_valid;
    wire stream_in_0_ready;

    wire [16:0] stream_out;
    wire stream_out_valid;
    wire stream_out_ready;

    wire [4:0] done;
    parameter NUM_CYCLES = 4000;

    pass_through #(
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .flush(flush),
        .rst_n(rst_n),
        .stream_in(stream_in_0),
        .stream_in_valid(stream_in_0_valid),
        .stream_out_ready(stream_out_ready),
        .tile_en(tile_en),
        .stream_in_ready(stream_in_0_ready),
        .stream_out(stream_out),
        .stream_out_valid(stream_out_valid)
    );

    glb_stream_write #(
        .FILE_NAME("stream_in_0.txt"),
        .TX_NUM(`TX_NUM_0),
        .RAN_SHITF(0)
    ) stream_in_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(stream_in_0),
        .ready(stream_in_0_ready),
        .valid(stream_in_0_valid),
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
        .done(done[4]),
        .flush(flush),
        .seg_mode(seg_mode)
    );


    integer start_record;
    integer clk_count;
    integer DONE_TOKEN = 17'h10100;

    // simulated clk signal, 10ns period
    initial begin
        start_record = 0;
        clk_count = 0;

        clk = 0;
        clk_en = 1;
        rst_n = 0;
        tile_en = 1;
        flush = 0;
        seg_mode = `SEG_MODE;

        #5 clk = 1;
        flush = 1;
        rst_n = 1;
        #5 clk = 0;
        flush = 0;

        for(integer i = 0; i < NUM_CYCLES * 2; i = i + 1) begin
            #5 clk = ~clk;
            if (~start_record && clk && stream_in_0_valid) begin
                start_record = 1;
            end
            if (clk && start_record && ~done[4]) begin
                clk_count += 1;
            end

        end
        $display("cycle count: %0d", clk_count);
        $finish;
    
    end

endmodule
