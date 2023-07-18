`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module repeat_tb;

    reg clk;
    reg clk_en;
    reg rst_n;
    reg stall;
    reg flush;
    reg tile_en;
    reg root;
    reg spacc_mode;
    reg [15:0] stop_lvl;
    wire [63:0] cycle_count ;

    // wire for dut input & output
    wire [16:0] proc_data_in;
    wire proc_data_in_valid;
    wire proc_data_in_ready;
    wire [16:0] repsig_data_in;
    wire repsig_data_in_valid;
    wire repsig_data_in_ready;
    wire [16:0] ref_data_out;
    wire ref_data_out_valid;
    wire ref_data_out_ready;

    wire [2:0] done;
    parameter NUM_CYCLES = 4000;

    Repeat #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .proc_data_in(proc_data_in),
        .proc_data_in_valid(proc_data_in_valid),
        .proc_data_in_ready(proc_data_in_ready),
        .repsig_data_in(repsig_data_in),
        .repsig_data_in_valid(repsig_data_in_valid),
        .repsig_data_in_ready(repsig_data_in_ready),
        .tile_en(tile_en),
        .ref_data_out(ref_data_out),
        .ref_data_out_valid(ref_data_out_valid),
        .ref_data_out_ready(ref_data_out_ready),
        .rst_n(rst_n),
        .flush(flush),
        .tile_en(tile_en),
        .root(root),
        .spacc_mode(spacc_mode),
        .stop_lvl(stop_lvl)
    );


    glb_write #(
        .FILE_NAME("pos_in_0.txt")
    ) proc_data_in_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(proc_data_in),
        .ready(proc_data_in_ready),
        .valid(proc_data_in_valid),
        .done(done[0]),
        .flush(flush)
    );

    glb_write #(
        .FILE_NAME("pos_in_1.txt")
    ) repsig_data_in_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(repsig_data_in),
        .ready(repsig_data_in_ready),
        .valid(repsig_data_in_valid),
        .done(done[1]),
        .flush(flush)
    );


    glb_read #(
        .FILE_NAME("pos_out_0.txt"),
        .TX_NUM(`TX_NUM_GLB)
    ) ref_data_out_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(ref_data_out),
        .ready(ref_data_out_ready),
        .valid(ref_data_out_valid),
        .done(done[2]),
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
        stop_lvl = 0;
        spacc_mode = 0;
        root = 0;
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
            if (~start_record && clk && (proc_data_in_valid | repsig_data_in_valid)) begin
                start_record = 1;
            end
            if (clk && start_record && ~done[2]) begin
                clk_count += 1;
            end

        end
        $display("cycle count: %0d", clk_count);
        $finish;
    
    end

endmodule
