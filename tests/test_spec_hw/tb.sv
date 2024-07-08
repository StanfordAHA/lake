`timescale 1ns/1ns


module lake_static_tb;

    parameter DATA_WIDTH = 16;

`ifdef CONFIG_MEMORY_SIZE
    parameter CONFIG_MEMORY_SIZE = `CONFIG_MEMORY_SIZE;
`else
    parameter CONFIG_MEMORY_SIZE = 512;
`endif

`ifdef NUMBER_PORTS
    parameter NUMBER_PORTS = `NUMBER_PORTS;
`else
    parameter NUMBER_PORTS = 2;
`endif

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
    // logic [549:0] bitstream [0:0];
    logic [CONFIG_MEMORY_SIZE - 1:0] bitstream [0:0];
    // logic [31:0] bitstream_addr [0:BITSTREAM_MAX_SIZE - 1];
    // logic [31:0] bitstream_data [0:BITSTREAM_MAX_SIZE - 1];

    logic [DATA_WIDTH - 1:0] port_w0_data;
    logic [DATA_WIDTH - 1:0] port_w1_data;
    logic [DATA_WIDTH - 1:0] port_w2_data;
    logic [DATA_WIDTH - 1:0] port_w3_data;
    logic [DATA_WIDTH - 1:0] port_r0_data;
    logic [DATA_WIDTH - 1:0] port_r1_data;
    logic [DATA_WIDTH - 1:0] port_r2_data;
    logic [DATA_WIDTH - 1:0] port_r3_data;
    logic [DATA_WIDTH - 1:0] port_w0_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w1_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w2_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w3_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r0_mem [0:NUM_CYCLES - 1] ;
    logic [DATA_WIDTH - 1:0] port_r1_mem [0:NUM_CYCLES - 1] ;
    logic [DATA_WIDTH - 1:0] port_r2_mem [0:NUM_CYCLES - 1] ;
    logic [DATA_WIDTH - 1:0] port_r3_mem [0:NUM_CYCLES - 1] ;

    if (NUMBER_PORTS == 2) begin

        lakespec dut (
            // inputs
            .clk(clk),
            .rst_n(rst_n),
            .flush(flush),
            // config
            .config_memory(bitstream[0]),
            // input ports
            .port_0(port_w0_data),
            // output ports
            .port_1(port_r0_data)
        );

        port_r1_data = 0;
        port_r2_data = 0;
        port_r3_data = 0;

    end
    else if (NUMBER_PORTS == 4) begin

        lakespec dut (
            // inputs
            .clk(clk),
            .rst_n(rst_n),
            .flush(flush),
            // config
            .config_memory(bitstream[0]),
            // input ports
            .port_0(port_w0_data),
            .port_1(port_w1_data),
            // output ports
            .port_2(port_r0_data),
            .port_3(port_r1_data)
        );

        port_r2_data = 0;
        port_r3_data = 0;

    end
    else if (NUMBER_PORTS == 8) begin

        lakespec dut (
            // inputs
            .clk(clk),
            .rst_n(rst_n),
            .flush(flush),
            // config
            .config_memory(bitstream[0]),
            // input ports
            .port_0(port_w0_data),
            .port_1(port_w1_data),
            .port_2(port_w2_data),
            .port_3(port_w3_data),
            // output ports
            .port_4(port_r0_data),
            .port_5(port_r1_data),
            .port_6(port_r2_data),
            .port_7(port_r3_data)
        );

    end


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

        BITSTREAM_LOCATION = $sformatf("%s/inputs/bitstream.bs", TEST_DIRECTORY);

        $display("BITSTREAM IS AT : %s", BITSTREAM_LOCATION);
        // Load the bitstream as an int into the bitstream memory
        $readmemh(BITSTREAM_LOCATION, bitstream);

    end

    initial begin

        THIS_CYC_COUNT = 0;

        clk <= 1'b0;
        clk <= 1'b0;
        rst_n <= 1'b1;
        stall <= 1'b0;
        flush <= 1'b0;
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

        clk <= 1'b0;
        rst_n <= 1'b1;
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
            port_w0_data = THIS_CYC_COUNT * 2;
            port_w1_data = THIS_CYC_COUNT * 2;
            port_w2_data = THIS_CYC_COUNT * 2;
            port_w3_data = THIS_CYC_COUNT * 2;
            #5 clk ^= 1;
            #5 clk ^= 1;
            THIS_CYC_COUNT = THIS_CYC_COUNT + 1;
            port_r0_mem[THIS_CYC_COUNT] = port_r0_data;
            port_r1_mem[THIS_CYC_COUNT] = port_r1_data;
            port_r2_mem[THIS_CYC_COUNT] = port_r2_data;
            port_r3_mem[THIS_CYC_COUNT] = port_r3_data;
        end

        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r0_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r0_mem);
        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r1_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r1_mem);
        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r2_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r2_mem);
        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r3_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r3_mem);

        #20 $finish;
    end

endmodule
