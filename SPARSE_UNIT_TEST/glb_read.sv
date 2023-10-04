module glb_read #(
    parameter NUM_BLOCKS = 1,
    parameter FILE_NAME = "dst.txt",
    parameter FILE_NAME2 = "dst2.txt",
    parameter LOCATION = "X00_Y00",
    parameter TX_NUM = 1,
    parameter RAN_SHITF = 0
)
(
    input logic clk,
    input logic rst_n,
    input logic [16:0] data,
    input logic data_p,
    output logic ready,
    input logic valid,
    output logic done,
    input logic flush
);

logic [16:0] local_mem_0 [0:2047];
logic [1:0] local_mem_1 [0:2047];
integer num_rx;
integer size_0;

string F1_PARGS;
string F2_PARGS;
string NUM_BLOCKS_PARGS;
integer NUM_BLOCKS_USE;
string F1_USE;
string F2_USE;
string ENABLED_PARGS;
integer ENABLED;
integer DONE_TOKEN;
integer done_count;
integer delay_count;
integer ADD_DELAY;
integer mask;

initial begin

    ENABLED = 1;
    // ENABLED_PARGS = $sformatf("%s_ENABLED=%%d", LOCATION);
    // $value$plusargs(ENABLED_PARGS, ENABLED);

    num_rx = 0;
    ready = 0;
    size_0 = 0;
    done = 0;
    DONE_TOKEN = 17'h10100;
    done_count = TX_NUM;
    ADD_DELAY = 0;
    mask = 32'd3  << RAN_SHITF;

    if (ENABLED == 1) begin

        $display("%s is enabled...", LOCATION);

        F1_PARGS = $sformatf("%s_F1=%%s", LOCATION);
        F2_PARGS = $sformatf("%s_F2=%%s", LOCATION);
        NUM_BLOCKS_PARGS = $sformatf("%s_NUM_BLOCKS=%%d", LOCATION);

        $display("Location: %s", LOCATION);
        F1_USE = FILE_NAME;
        F2_USE = FILE_NAME2;
        $display("F1_USE before: %s", F1_USE);
        $value$plusargs(F1_PARGS, F1_USE);
        $display("F1_USE after: %s", F1_USE);

        @(posedge flush);
        @(negedge flush);

        @(posedge clk);
        @(posedge clk);
        @(posedge clk);

        while(done_count > 0) begin
            @(posedge clk);
            #1;
            // ready = $urandom();

            ready = 0;
            delay_count = $urandom & mask;
            while (delay_count > 0 & ADD_DELAY) begin
                @(posedge clk);
                #1;
                delay_count--;
            end

            ready = 1;
            if(ready == 1 && valid == 1) begin
                local_mem_0[num_rx] = data;
                local_mem_1[num_rx] = data_p;
                if (data == DONE_TOKEN) begin
                    done_count--;
                end
                num_rx = num_rx + 1;
            end
        end
        @(posedge clk);
        #1;
        assert(valid == 0) else $error("Valid signal fails to end");
        ready = 0;
        $writememh(F1_USE, local_mem_0);
        $writememh(F2_USE, local_mem_1);

    end

    @(posedge clk);
    ready = 0;
    done = 1;

end

endmodule
