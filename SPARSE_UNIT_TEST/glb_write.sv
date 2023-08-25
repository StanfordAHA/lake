module glb_write #(
    parameter TX_SIZE = 2048,
    parameter FILE_NAME = "src.txt",
    parameter LOCATION = "X00_Y00",
    parameter TX_NUM = 1,
    parameter RAN_SHITF = 0
)
(
    input logic clk,
    input logic rst_n,
    output logic [16:0] data,
    input logic ready,
    output logic valid,
    output logic done,
    input logic flush
);

logic [16:0] local_mem [0:2047];
integer num_tx;

string TX_SIZE_PARGS;
string FILE_NAME_PARGS;
integer TX_SIZE_USE;
string FILE_NAME_USE;
string ENABLED_PARGS;
integer ENABLED;
integer DELAY;
integer ADD_DELAY;
integer done_count;
integer DONE_TOKEN;
integer mask;

initial begin

    num_tx = 0;
    valid = 0;
    done = 0;
    data = 0;
    valid = 0;
    ENABLED = 1;
    ADD_DELAY = 0;
    done_count = TX_NUM;
    DONE_TOKEN = 17'h10100;
    mask = 32'hF  << RAN_SHITF;

    // ENABLED_PARGS = $sformatf("%s_ENABLED=%%d", LOCATION);
    // $value$plusargs(ENABLED_PARGS, ENABLED);

    if (ENABLED == 1) begin

        $display("%s is enabled...", LOCATION);

        FILE_NAME_PARGS = $sformatf("%s_FILE_NAME=%%s", LOCATION);
        FILE_NAME_USE = FILE_NAME;
        $value$plusargs(FILE_NAME_PARGS, FILE_NAME_USE);
        // string file_str;
        // file_str = $sformatf("/home/max/Documents/SPARSE/garnet/generic_memory_%d.txt", FILE_NO);
        $readmemh(FILE_NAME_USE, local_mem);

        TX_SIZE_PARGS = $sformatf("%s_TX_SIZE=%%d", LOCATION);
        TX_SIZE_USE = TX_SIZE;
        $value$plusargs(TX_SIZE_PARGS, TX_SIZE_USE);

        num_tx = 0;
        valid = 0;
        done = 0;
        data = 0;

        @(posedge flush);
        @(negedge flush);

        @(posedge clk);
        @(posedge clk);
        @(posedge clk);

        // Make as many transfers from the memory as needed.
        while(num_tx < TX_SIZE_USE && done_count > 0) begin
            @(posedge clk);
            #1;

            DELAY = $urandom & mask;
            if(ready == 1 && DELAY < 4 && ADD_DELAY) begin// 25% chance of delay
                valid = 0;
                @(posedge clk);
                #1;
            end

            data = local_mem[num_tx];
            valid = 1;
            if(ready == 1 && valid == 1) begin
                if (data == DONE_TOKEN) begin
                    done_count--;
                    // valid = 0;
                end
                num_tx = num_tx + 1;
            end
        end
    end

    @(posedge clk);
    done = 1;
    valid = 0;

end

endmodule