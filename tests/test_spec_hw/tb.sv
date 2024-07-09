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
    logic                    port_w0_valid;
    logic                    port_w0_ready;
    integer w0_tracker;

    logic [DATA_WIDTH - 1:0] port_w1_data;
    logic                    port_w1_valid;
    logic                    port_w1_ready;
    integer w1_tracker;

    logic [DATA_WIDTH - 1:0] port_w2_data;
    logic                    port_w2_valid;
    logic                    port_w2_ready;
    integer w2_tracker;

    logic [DATA_WIDTH - 1:0] port_w3_data;
    logic                    port_w3_valid;
    logic                    port_w3_ready;
    integer w3_tracker;

    logic [DATA_WIDTH - 1:0] port_r0_data;
    logic                    port_r0_valid;
    logic                    port_r0_ready;
    integer r0_tracker;

    logic [DATA_WIDTH - 1:0] port_r1_data;
    logic                    port_r1_valid;
    logic                    port_r1_ready;
    integer r1_tracker;

    logic [DATA_WIDTH - 1:0] port_r2_data;
    logic                    port_r2_valid;
    logic                    port_r2_ready;
    integer r2_tracker;

    logic [DATA_WIDTH - 1:0] port_r3_data;
    logic                    port_r3_valid;
    logic                    port_r3_ready;
    integer r3_tracker;

    logic [DATA_WIDTH - 1:0] port_w0_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w1_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w2_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w3_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r0_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r1_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r2_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r3_mem [0:NUM_CYCLES - 1];

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
            .port_0_valid(port_w0_valid),
            .port_0_ready(port_w0_ready),
            // output ports
            .port_1(port_r0_data),
            .port_1_valid(port_r0_valid),
            .port_1_ready(port_r0_ready)
        );

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
            .port_0_valid(port_w0_valid),
            .port_0_ready(port_w0_ready),

            .port_1(port_w1_data),
            .port_1_valid(port_w1_valid),
            .port_1_ready(port_w1_ready),
            // output ports
            .port_2(port_r0_data),
            .port_2_valid(port_r0_valid),
            .port_2_ready(port_r0_ready),

            .port_3(port_r1_data),
            .port_3_valid(port_r1_valid),
            .port_3_ready(port_r1_ready)
        );

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
            .port_0_valid(port_w0_valid),
            .port_0_ready(port_w0_ready),

            .port_1(port_w1_data),
            .port_1_valid(port_w1_valid),
            .port_1_ready(port_w1_ready),

            .port_2(port_w2_data),
            .port_2_valid(port_w2_valid),
            .port_2_ready(port_w2_ready),

            .port_3(port_w3_data),
            .port_3_valid(port_w3_valid),
            .port_3_ready(port_w3_ready),
            // output ports
            .port_4(port_r0_data),
            .port_4_valid(port_r0_valid),
            .port_4_ready(port_r0_ready),

            .port_5(port_r1_data),
            .port_5_valid(port_r1_valid),
            .port_5_ready(port_r1_ready),

            .port_6(port_r2_data),
            .port_6_valid(port_r2_valid),
            .port_6_ready(port_r2_ready),

            .port_7(port_r3_data),
            .port_7_valid(port_r3_valid),
            .port_7_ready(port_r3_ready)
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

        w0_tracker = 0;
        w1_tracker = 0;
        w2_tracker = 0;
        w3_tracker = 0;
        r0_tracker = 0;
        r1_tracker = 0;
        r2_tracker = 0;
        r3_tracker = 0;

        port_w0_valid = 1'b1;
        port_w1_valid = 1'b1;
        port_w2_valid = 1'b1;
        port_w3_valid = 1'b1;
        // port_r0_ready = 1'b1;
        port_r0_ready = 1'b0;
        port_r1_ready = 1'b1;
        port_r2_ready = 1'b1;
        port_r3_ready = 1'b1;

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
            // For LI, delay the first read to verify that the
            // writes don't proceed too much
            if (THIS_CYC_COUNT > 64) begin
                port_r0_ready = 1'b1;
            end

            // Input 2*i
            port_w0_data = w0_tracker * 2;
            port_w1_data = w1_tracker * 2;
            port_w2_data = w2_tracker * 2;
            port_w3_data = w3_tracker * 2;
            #5 clk ^= 1;
            #5 clk ^= 1;
            THIS_CYC_COUNT = THIS_CYC_COUNT + 1;
            // Only increase rX/w/Y_tracker if r/v verified
            if (port_w0_valid & port_w0_ready) begin
                w0_tracker = w0_tracker + 1;
            end
            if (port_w1_valid & port_w1_ready) begin
                w1_tracker = w1_tracker + 1;
            end
            if (port_w2_valid & port_w2_ready) begin
                w2_tracker = w2_tracker + 1;
            end
            if (port_w3_valid & port_w3_ready) begin
                w3_tracker = w3_tracker + 1;
            end
            if (port_r0_valid & port_r0_ready) begin
                port_r0_mem[r0_tracker] = port_r0_data;
                r0_tracker = r0_tracker + 1;
            end
            if (port_r1_valid & port_r1_ready) begin
                port_r1_mem[r1_tracker] = port_r1_data;
                r1_tracker = r1_tracker + 1;
            end
            if (port_r2_valid & port_r2_ready) begin
                port_r2_mem[r2_tracker] = port_r2_data;
                r2_tracker = r2_tracker + 1;
            end
            if (port_r3_valid & port_r3_ready) begin
                port_r3_mem[r3_tracker] = port_r3_data;
                r3_tracker = r3_tracker + 1;
            end

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
