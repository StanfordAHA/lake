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
    parameter NUM_CYCLES = 4000;

    // instruction 
    reg [83:0] inst;
    reg [83:0] onyxpeintf_inst;
    reg [2:0] sparse_num_inputs;
    reg [2:0] num_inputs;

    reduce_pe_cluster #(
        
    ) dut (
        .clk(clk),
        .clk_en(clk_en),
        .pe_bit0(1'b0),
        .pe_bit1(1'b0),
        .pe_bit2(1'b0),
        // Configure pe to dense mode
        .pe_dense_mode(1'b1),
        // Configure pe to use internal connection with reduce
        .pe_in_external(1'b0),
        .reduce_data_in(val_in),
        .reduce_data_in_valid(val_in_valid),
        .reduce_data_out_ready(val_out_ready),
        .reduce_default_value(16'b0),
        .flush(flush),
        .rst_n(rst_n),
        .reduce_stop_lvl(16'b0),
        .tile_en(tile_en),
        .reduce_tile_en(tile_en),
        .reduce_data_in_ready(val_in_ready),
        .reduce_data_out(val_out),
        .reduce_data_out_valid(val_out_valid),
        .pe_onyxpeintf_inst(inst)
    );

    glb_write #(
        .FILE_NAME("coord_in_0.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(0)
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
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
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

        onyxpeintf_inst = $value$plusargs("inst=%h", inst);
        sparse_num_inputs = $value$plusargs("num_inputs=%h", num_inputs);

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
