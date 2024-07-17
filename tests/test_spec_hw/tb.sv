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

    parameter BITSTREAM_MAX_SIZE = 4096;
    parameter NUM_CYCLES = 200;

    integer static_value = 1;
    logic clk;
    logic rst_n;
    logic stall;
    logic flush;
    logic [CONFIG_MEMORY_SIZE - 1:0] bitstream [0:0];

    logic [DATA_WIDTH - 1:0] port_w0_data;
    logic                    port_w0_valid;
    logic                    port_w0_ready;
    integer w0_tracker;
    integer w0_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_w1_data;
    logic                    port_w1_valid;
    logic                    port_w1_ready;
    integer w1_tracker;
    integer w1_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_w2_data;
    logic                    port_w2_valid;
    logic                    port_w2_ready;
    integer w2_tracker;
    integer w2_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_w3_data;
    logic                    port_w3_valid;
    logic                    port_w3_ready;
    integer w3_tracker;
    integer w3_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_r0_data;
    logic                    port_r0_valid;
    logic                    port_r0_ready;
    integer r0_tracker;
    integer r0_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_r1_data;
    logic                    port_r1_valid;
    logic                    port_r1_ready;
    integer r1_tracker;
    integer r1_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_r2_data;
    logic                    port_r2_valid;
    logic                    port_r2_ready;
    integer r2_tracker;
    integer r2_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_r3_data;
    logic                    port_r3_valid;
    logic                    port_r3_ready;
    integer r3_tracker;
    integer r3_num_data = 0;

    logic [DATA_WIDTH - 1:0] port_w0_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w1_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w2_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_w3_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r0_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r1_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r2_mem [0:NUM_CYCLES - 1];
    logic [DATA_WIDTH - 1:0] port_r3_mem [0:NUM_CYCLES - 1];

    if (NUMBER_PORTS == 2) begin : two_port_dut

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
    else if (NUMBER_PORTS == 4) begin : four_port_dut

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
    else if (NUMBER_PORTS == 8) begin : eight_port_dut

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

    /*
        Load all the different values from various files...
    */
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

        if ($value$plusargs("w0_num_data=%d", w0_num_data)) begin
            $display("w0_num_data set to %d", w0_num_data);
        end else begin
            $display("w0_num_data not set, using default value %d", w0_num_data);
        end
        if ($value$plusargs("w1_num_data=%d", w1_num_data)) begin
            $display("w1_num_data set to %d", w1_num_data);
        end else begin
            $display("w1_num_data not set, using default value %d", w1_num_data);
        end
        if ($value$plusargs("w2_num_data=%d", w2_num_data)) begin
            $display("w2_num_data set to %d", w2_num_data);
        end else begin
            $display("w2_num_data not set, using default value %d", w2_num_data);
        end
        if ($value$plusargs("w3_num_data=%d", w3_num_data)) begin
            $display("w3_num_data set to %d", w3_num_data);
        end else begin
            $display("w3_num_data not set, using default value %d", w3_num_data);
        end

        // Do the same for the read data
        if ($value$plusargs("r0_num_data=%d", r0_num_data)) begin
            $display("r0_num_data set to %d", r0_num_data);
        end else begin
            $display("r0_num_data not set, using default value %d", r0_num_data);
        end
        if ($value$plusargs("r1_num_data=%d", r1_num_data)) begin
            $display("r1_num_data set to %d", r1_num_data);
        end else begin
            $display("r1_num_data not set, using default value %d", r1_num_data);
        end
        if ($value$plusargs("r2_num_data=%d", r2_num_data)) begin
            $display("r2_num_data set to %d", r2_num_data);
        end else begin
            $display("r2_num_data not set, using default value %d", r2_num_data);
        end
        if ($value$plusargs("r3_num_data=%d", r3_num_data)) begin
            $display("r3_num_data set to %d", r3_num_data);
        end else begin
            $display("r3_num_data not set, using default value %d", r3_num_data);
        end

        if ($value$plusargs("static=%d", static_value)) begin
            $display("static set to %d", static_value);
        end else begin
            $display("static not set, using default value %d", static_value);
        end

    end

    /*
        Clock forever
    */

    initial begin
        clk = 0;
        forever begin : clock_4_ever
            #5 clk = ~clk;
        end
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

        port_w0_valid <= 1'b0;
        port_w1_valid <= 1'b0;
        port_w2_valid <= 1'b0;
        port_w3_valid <= 1'b0;
        port_r0_ready <= 1'b0;
        port_r1_ready <= 1'b0;
        port_r2_ready <= 1'b0;
        port_r3_ready <= 1'b0;

        rst_n <= 1'b1;
        //  Stall is clock enable
        stall <= 1'b1;

        #5;

        @(posedge clk);

        rst_n <= 1'b0;

        @(posedge clk);
        stall <= 1'b1;

        // Deassert async reset
        @(negedge clk);
        rst_n <= 1'b1;

        @(posedge clk);
        flush <= 1'b1;
        // Hold flush high for a cycle
        @(posedge clk);
        stall <= 1'b0;

        // Set all the valids high here if there will be
        // data on them
        if (w0_num_data > 0) begin
            port_w0_valid <= 1'b1;
        end
        if (w1_num_data > 0) begin
            port_w1_valid <= 1'b1;
        end
        if (w2_num_data > 0) begin
            port_w2_valid <= 1'b1;
        end
        if (w3_num_data > 0) begin
            port_w3_valid <= 1'b1;
        end

        // This is 0 for now as a testing condition...
        if (r0_num_data > 0) begin
            if (static_value == 1) begin
                port_r0_ready <= 1'b1;
            end
            else begin
                port_r0_ready <= 1'b0;
            end
        end
        if (r1_num_data > 0) begin
            port_r1_ready <= 1'b1;
        end
        if (r2_num_data > 0) begin
            port_r2_ready <= 1'b1;
        end
        if (r3_num_data > 0) begin
            port_r3_ready <= 1'b1;
        end

        @(posedge clk);
        flush <= 1'b0;

        while (THIS_CYC_COUNT < NUM_CYCLES) begin

            // Input 2*i
            port_w0_data <= w0_tracker * 2;
            port_w1_data <= w1_tracker * 2;
            port_w2_data <= w2_tracker * 2;
            port_w3_data <= w3_tracker * 2;

            // For LI, delay the first read to verify that the
            // writes don't proceed too much
            if (THIS_CYC_COUNT > 64) begin
                port_r0_ready <= 1'b1;
            end

            // Kill the input valids once their transactions are finished...
            if (w0_tracker >= w0_num_data) begin
                port_w0_valid <= 1'b0;
            end
            if (w1_tracker >= w1_num_data) begin
                port_w1_valid <= 1'b0;
            end
            if (w2_tracker >= w2_num_data) begin
                port_w2_valid <= 1'b0;
            end
            if (w3_tracker >= w3_num_data) begin
                port_w3_valid <= 1'b0;
            end

            // Kill the output readys once the data is done...
            // And check that we don't get any valids after!!!
            // Only for r/v
            if (r0_tracker >= r0_num_data && static_value == 0) begin
                port_r0_ready <= 1'b0;
                if (port_r0_valid == 1'b1) begin
                    $display("Still seeing data on port r0");
                    $display("FAIL");
                    $finish;
                end
            end
            if (r1_tracker >= r1_num_data && static_value == 0) begin
                port_r1_ready <= 1'b0;
                if (port_r1_valid == 1'b1) begin
                    $display("Still seeing data on port r1");
                    $display("FAIL");
                    $finish;
                end
            end
            if (r2_tracker >= r2_num_data && static_value == 0) begin
                port_r2_ready <= 1'b0;
                if (port_r2_valid == 1'b1) begin
                    $display("Still seeing data on port r2");
                    $display("FAIL");
                    $finish;
                end
            end
            if (r3_tracker >= r3_num_data && static_value == 0) begin
                port_r3_ready <= 1'b0;
                if (port_r3_valid == 1'b1) begin
                    $display("Still seeing data on port r3");
                    $display("FAIL");
                    $finish;
                end
            end

            @(posedge clk);
            THIS_CYC_COUNT = THIS_CYC_COUNT + 1;

            // Only increase rX/w/Y_tracker if r/v verified
            if (port_w0_valid && port_w0_ready && ((w0_tracker < w0_num_data) || (static_value == 1))) begin
                w0_tracker <= w0_tracker + 1;
            end
            if (port_w1_valid & port_w1_ready && ((w1_tracker < w1_num_data) || (static_value == 1))) begin
                w1_tracker <= w1_tracker + 1;
            end
            if (port_w2_valid & port_w2_ready && ((w2_tracker < w2_num_data) || (static_value == 1))) begin
                w2_tracker <= w2_tracker + 1;
            end
            if (port_w3_valid & port_w3_ready && ((w3_tracker < w3_num_data) || (static_value == 1))) begin
                w3_tracker <= w3_tracker + 1;
            end
            if (port_r0_valid & port_r0_ready && ((r0_tracker < r0_num_data) || (static_value == 1))) begin
                port_r0_mem[r0_tracker] = port_r0_data;
                r0_tracker <= r0_tracker + 1;
            end
            if (port_r1_valid & port_r1_ready && ((r1_tracker < r1_num_data) || (static_value == 1))) begin
                port_r1_mem[r1_tracker] = port_r1_data;
                r1_tracker <= r1_tracker + 1;
            end
            if (port_r2_valid & port_r2_ready && ((r2_tracker < r2_num_data) || (static_value == 1))) begin
                port_r2_mem[r2_tracker] = port_r2_data;
                r2_tracker <= r2_tracker + 1;
            end
            if (port_r3_valid & port_r3_ready && ((r3_tracker < r3_num_data) || (static_value == 1))) begin
                port_r3_mem[r3_tracker] = port_r3_data;
                r3_tracker <= r3_tracker + 1;
            end

        end

        // Check that all inputs and outputs are at their maximum for validity (in ready/valid)
        if ((r0_tracker != r0_num_data) && (static_value == 0)) begin
            $display("Not enough data on port r0");
            $display("Expected %d, but only received %d", r0_num_data, r0_tracker);
            $display("FAIL");
            $finish;
        end
        if ((r1_tracker != r1_num_data) && (static_value == 0)) begin
            $display("Not enough data on port r1");
            $display("Expected %d, but only received %d", r1_num_data, r1_tracker);
            $display("FAIL");
            $finish;
        end
        if ((r2_tracker != r2_num_data) && (static_value == 0)) begin
            $display("Not enough data on port r2");
            $display("Expected %d, but only received %d", r2_num_data, r2_tracker);
            $display("FAIL");
            $finish;
        end
        if ((r3_tracker != r3_num_data) && (static_value == 0)) begin
            $display("Not enough data on port r3");
            $display("Expected %d, but only received %d", r3_num_data, r3_tracker);
            $display("FAIL");
            $finish;
        end

        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r0_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r0_mem);
        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r1_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r1_mem);
        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r2_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r2_mem);
        OUTPUT_LOCATION = $sformatf("%s/outputs/port_r3_output.txt", TEST_DIRECTORY);
        $writememh(OUTPUT_LOCATION, port_r3_mem);

        $display("PASS");
        #20 $finish;
    end

endmodule
