`timescale 1ns/1ns
module lake_static_tb;

    parameter DATA_WIDTH = 16;

    reg clk;
    reg rst_n;
    reg stall;
    reg flush;
    // reg [31:0] config_config_addr ;
    // reg [31:0] config_config_data ;
    // reg [0:0] config_read ;
    // reg [0:0] config_write ;
    // wire done;
    wire [63:0] cycle_count ;

    // parameter BITSTREAM_MAX_SIZE = 4096 - 1;
    // integer BITSTREAM_MAX_SIZE;
    parameter BITSTREAM_MAX_SIZE = 4096;
    parameter NUM_CYCLES = 1000;

    // logic [63:0] bitstream [0:BITSTREAM_MAX_SIZE - 1];
    logic [549:0] bitstream [0:0];
    // logic [31:0] bitstream_addr [0:BITSTREAM_MAX_SIZE - 1];
    // logic [31:0] bitstream_data [0:BITSTREAM_MAX_SIZE - 1];

    logic [DATA_WIDTH - 1:0] port_0_data;
    logic [DATA_WIDTH - 1:0] port_1_data;

    logic [DATA_WIDTH - 1:0] port_0_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_1_mem [0:NUM_CYCLES - 1] ;
    // SparseTBBuilder #(
        
    // ) dut (
    //     .clk(clk),
    //     .rst_n(rst_n),
    //     .stall(stall),
    //     .flush(flush),
    //     .config_config_addr(config_config_addr),
    //     .config_config_data(config_config_data),
    //     .config_read(config_read),
    //     .config_write(config_write),
    //     .done(done),
    //     .cycle_count(cycle_count)
    // );

    lakespec #(

    ) dut (
        // inputs
        .clk(clk),
        .rst_n(rst_n),
        .flush(flush),
        // config
        .config_memory_size_550(bitstream[0]),
        // input ports
        .port_0(port_0_data),
        // output ports
        .port_1(port_1_data)
    // input logic clk,
    // input logic [549:0] config_memory_size_550,
    // input logic flush,
    // input logic [15:0] port_0,
    // input logic rst_n,
    // output logic [15:0] port_1
    );


    integer THIS_CYC_COUNT;
    // integer BITSTREAM_CURR_SIZE;
    // integer BITSTREAM_CURR_SIZE_CNT;



    string TEST_DIRECTORY; // = "/home/max/Documents/lake/number_in_hex.txt";
    string BITSTREAM_LOCATION; // = "/home/max/Documents/lake/number_in_hex.txt";
    string OUTPUT_LOCATION;

    initial begin

        TEST_DIRECTORY = "./";

        if ($value$plusargs("TEST_DIRECTORY=%s", TEST_DIRECTORY)) begin
            $display("TEST_DIRECTORY set to %s", TEST_DIRECTORY);
        end else begin
            $display("TEST_DIRECTORY not set, using default value %s", TEST_DIRECTORY);
        end

        // $value$plusargs("TEST_DIRECTORY=%s", TEST_DIRECTORY);

        // string file_str;
        // file_str = $sformatf("/home/max/Documents/SPARSE/garnet/generic_memory_%d.txt", FILE_NO);
        // $value$plusargs("BITSTREAM_LOCATION=%s", BITSTREAM_LOCATION);
        // $value$plusargs("BITSTREAM_LOCATION=%s", BITSTREAM_LOCATION);
        BITSTREAM_LOCATION = $sformatf("%s/inputs/bitstream.bs", TEST_DIRECTORY);

        $display("BITSTREAM IS AT : %s", BITSTREAM_LOCATION);
        // Load the bitstream as an int into the bitstream memory
        $readmemh(BITSTREAM_LOCATION, bitstream);
        // $readmemh(BITSTREAM_LOCATION, bitstream);
        // for (integer i_ = 0; i_ < BITSTREAM_MAX_SIZE; i_ = i_ + 1) begin
            // bitstream_addr[i_] = bitstream[i_][63: 32];
            // bitstream_data[i_] = bitstream[i_][31: 0];
        // end
    end

    initial begin

        // $value$plusargs("BITSTREAM_SIZE=%d", BITSTREAM_CURR_SIZE);
        // BITSTREAM_CURR_SIZE_CNT = 0;
        // $display("BITSTREAM SIZE IS: %d", BITSTREAM_CURR_SIZE);

        THIS_CYC_COUNT = 0;

        clk <= 1'b0;
        clk <= 1'b0;
        rst_n <= 1'b1;
        stall <= 1'b0;
        flush <= 1'b0;
        // config_config_addr <= 32'd0;
        // config_config_data <= 32'd0;
        // config_read <= 1'd0;
        // config_write <= 1'd0;
        stall <= 1'b1;
        rst_n <= 1'b1;
        #1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        rst_n <= 1'b0;
        #5 clk ^= 1;
        #5 clk ^= 1;
        stall <= 1'b1;
        #1;

        // for (BITSTREAM_CURR_SIZE_CNT = 0; BITSTREAM_CURR_SIZE_CNT < BITSTREAM_CURR_SIZE; BITSTREAM_CURR_SIZE_CNT = BITSTREAM_CURR_SIZE_CNT + 1) begin
        //     clk <= 1'b0;
        //     rst_n <= 1'b0;
        //     config_config_addr <= bitstream_addr[BITSTREAM_CURR_SIZE_CNT];
        //     config_config_data <= bitstream_data[BITSTREAM_CURR_SIZE_CNT];
        //     config_read <= 1'd0;
        //     config_write <= 1'd1;
        //     #5 clk ^= 1;
        //     #5 clk ^= 1;
        //     config_write <= 1'd0;
        //     #1;
        // end

        // foreach (bitstream_addr[i]) begin
        //     clk <= 1'b0;
        //     rst_n <= 1'b0;
        //     config_config_addr <= bitstream_addr[i];
        //     config_config_data <= bitstream_data[i];
        //     config_read <= 1'd0;
        //     config_write <= 1'd1;
        //     #5 clk ^= 1;
        //     #5 clk ^= 1;
        //     config_write <= 1'd0;
        //     #1;
        // end

        clk <= 1'b0;
        rst_n <= 1'b1;
        // config_read <= 1'd0;
        // config_write <= 1'd0;
        #5 clk ^= 1;
        #5 clk ^= 1;
        flush <= 1'b1;
        #1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        stall <= 1'b0;
        #1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        #5 clk ^= 1;
        flush <= 1'b0;
        #1;
        #5 clk ^= 1;
        #5 clk ^= 1;

        while (THIS_CYC_COUNT < NUM_CYCLES) begin
            // if (dut.done) begin
            //     $write("Test is done...\n");
            //     $write("Cycle Count...%d\n", cycle_count);
            //     $finish;
            // end
            // Input 2*i
            port_0_data = THIS_CYC_COUNT * 2;
            #5 clk ^= 1;
            #5 clk ^= 1;
            THIS_CYC_COUNT = THIS_CYC_COUNT + 1;
            port_1_mem[THIS_CYC_COUNT] = port_1_data;
        end


        OUTPUT_LOCATION = $sformatf("%s/outputs/port_1_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_1_mem);
        // $writememh("/home/max/Documents/lake/port_1_output.txt", port_1_mem);

        #20 $finish;
    end

endmodule
