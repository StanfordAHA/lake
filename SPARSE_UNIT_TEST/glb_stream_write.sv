module glb_stream_write #(
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
    input logic flush,
    input logic seg_mode
);

logic [16:0] local_mem [0:4095];
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
integer length_count;
integer mask;
integer set_inst;
integer stream_count;

initial begin

    num_tx = 0;
    valid = 0;
    done = 0;
    data = 0;
    valid = 0;
    ENABLED = 1;
    ADD_DELAY = 0;
    done_count = TX_NUM;
    $display("%d", done_count);
    length_count = 0;
    set_inst = 0;
    mask = 32'd3  << RAN_SHITF;

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
            #1; //TODO: debug the issue with the 1 unit time delay with line 90

            valid = 0;
            DELAY = $urandom & mask;
            while (DELAY > 0 & ADD_DELAY) begin
                @(posedge clk);
                // #1;
                DELAY--;
            end

            data = local_mem[num_tx];
            valid = 1;
            #1;
            if(ready == 1 && valid == 1) begin
                num_tx = num_tx + 1;

                if (set_inst == 0) begin
                    set_inst = 1;
                    length_count = local_mem[num_tx] + 1;
                    stream_count = seg_mode ? 2 : 1;
                end
                else begin
                    length_count = length_count - 1;
                    if (length_count == 0) begin
                        stream_count = stream_count - 1;
                        if (stream_count == 0) begin
                            done_count = done_count - 1;
                            set_inst = 0;
                        end
                        else begin
                            length_count = local_mem[num_tx] + 1; // potential segfault
                        end
                    end
                end
            end
        end
    end

    @(posedge clk);
    done = 1;
    valid = 0;

end

endmodule
