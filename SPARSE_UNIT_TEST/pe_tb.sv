`timescale 1ns/1ns
`ifndef TX_NUM_GLB
`define TX_NUM_GLB 1
`endif

module pe_tb;

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
    reg [0:0] dense_mode;
    wire [63:0] cycle_count ;

    // wire for dut input & output
    reg bit0;
    reg bit1;
    reg bit2;
    wire [16:0] data0;
    wire data0_valid;
    wire data0_ready;
    wire [16:0] data1;
    wire data1_valid;
    wire data1_ready;
    wire [16:0] data2;
    wire data2_valid;
    wire data2_ready;
    wire [16:0] res;
    // TODO: What do do about res_p?
    wire res_p;
    wire res_valid;
    wire res_ready;
    wire res_p_valid;
    wire res_p_ready;

    // instruction 
    reg [83:0] inst;
    reg [83:0] onyxpeintf_inst;
    reg [2:0] sparse_num_inputs;
    reg [2:0] num_inputs;

    wire [3:0] done;
    parameter NUM_CYCLES = 4000;

    PE_onyx #(
        
    ) dut (
        .bit0(bit0),
        .bit1(bit1),
        .bit2(bit2),
        .clk(clk),
        .clk_en(clk_en),
        .data0(data0),
        .data0_valid(data0_valid),
        .data0_ready(data0_ready),
        .data1(data1),
        .data1_valid(data1_valid),
        .data1_ready(data1_ready),
        .data2(data2),
        .data2_valid(data2_valid),
        .data2_ready(data2_ready),
        .dense_mode(dense_mode),
        .sparse_num_inputs(num_inputs),
        .tile_en(tile_en),
        .res(res),
        .res_p(res_p),
        .res_valid(res_valid),
        .res_ready(res_ready),
        .rst_n(rst_n),
        .flush(flush),
        .tile_en(tile_en),
        .onyxpeintf_inst(inst)
    );

    glb_write #(
        .FILE_NAME("in_data0.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(0)
    ) data0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(data0),
        .ready(data0_ready),
        .valid(data0_valid),
        .done(done[0]),
        .flush(flush)
    );

    glb_write #(
        .FILE_NAME("in_data1.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
    ) data1_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(data1),
        .ready(data1_ready),
        .valid(data1_valid),
        .done(done[1]),
        .flush(flush)
    );

    glb_write #(
        .FILE_NAME("in_data2.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(1)
    ) data2_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(data2),
        .ready(data2_ready),
        .valid(data2_valid),
        .done(done[2]),
        .flush(flush)
    );

    glb_read #(
        .FILE_NAME("out_data.txt"),
        .FILE_NAME2("out_data_p.txt"),
        .TX_NUM(`TX_NUM_GLB),
        .RAN_SHITF(0)
    ) res_0_inst (
        .clk(clk),
        .rst_n(rst_n),
        .data(res),
        .ready(res_ready),
        .valid(res_valid),
        .done(done[3]),
        .flush(flush)
    );

    assign res_p_ready = res_ready;



    integer start_record;
    integer clk_count;
    integer DONE_TOKEN = 17'h10100;

    // simulated clk signal, 10ns period
    initial begin
        start_record = 0;
        clk_count = 0;


        //onyxpeintf_inst = 'h4_8000_0400_0100_0040_0000;
        onyxpeintf_inst = $value$plusargs("inst=%h", inst);
        sparse_num_inputs = $value$plusargs("num_inputs=%h", num_inputs);;
        #1;
        $display("inst %h", inst);
        $display("num inputs%d", num_inputs);

        clk = 0;
        dense_mode = 1'b0;
        
        clk_en = 1;
        rst_n = 0;
        tile_en = 1;
        flush = 0;

        #5 clk = 1;
        #5 clk = 0;
        #5 clk = 1;
        #5 clk = 0;
        
        #5 clk = 1;
        rst_n = 1;
        #5 clk = 0;
        
        #5 clk = 1;
        #5 clk = 0;
        #5 clk = 1;
        #5 clk = 0;

        flush = 1;
        #5 clk = 1;
        #5 clk = 0;
        #5 clk = 1;
        #5 clk = 0;
        flush = 0;

        #5 clk = 1;
        #5 clk = 0;
        #5 clk = 1;
        #5 clk = 0;

        bit0 = 0;
        bit1 = 0;
        bit2 = 0;

       


        for(integer i = 0; i < NUM_CYCLES * 2; i = i + 1) begin
            #5 clk = ~clk;
            if (~start_record && clk && (data0_valid | data1_valid | data2_valid)) begin
                start_record = 1;
            end
            if (clk && start_record && ~done[3]) begin
                clk_count += 1;
            end

        end
        $display("cycle count: %0d", clk_count);
        $finish;
    
    end

endmodule
