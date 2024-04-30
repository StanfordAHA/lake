module glb_stream_read #(
    parameter NUM_BLOCKS = 1,
    parameter FILE_NAME = "dst.txt",
    parameter LOCATION = "X00_Y00",
    parameter TX_NUM = 1,
    parameter RAN_SHITF = 0
)
(
    input logic clk,
    input logic rst_n,
    input logic [16:0] data,
    output logic ready,
    input logic valid,
    output logic done,
    input logic flush,
    input logic seg_mode
);

logic [16:0] local_mem_0 [0:2047];
integer num_rx;
integer size_0;

string F1_PARGS;
string NUM_BLOCKS_PARGS;
integer NUM_BLOCKS_USE;
string F1_USE;
string ENABLED_PARGS;
integer ENABLED;
integer done_count;
integer length_count;
integer delay_count;
integer ADD_DELAY;
integer mask;
integer stream_count;
integer set_inst;

initial begin

    ENABLED = 1;
    // ENABLED_PARGS = $sformatf("%s_ENABLED=%%d", LOCATION);
    // $value$plusargs(ENABLED_PARGS, ENABLED);

    num_rx = 0;
    ready = 0;
    size_0 = 0;
    done = 0;
    done_count = TX_NUM + 1; // extra 1 for the initialization
    length_count = 0;
    ADD_DELAY = 0;
    set_inst = 0;
    mask = 32'd3  << RAN_SHITF;

    if (ENABLED == 1) begin
        
        $display("%s is enabled...", LOCATION);

        F1_PARGS = $sformatf("%s_F1=%%s", LOCATION);
        NUM_BLOCKS_PARGS = $sformatf("%s_NUM_BLOCKS=%%d", LOCATION);

        $display("Location: %s", LOCATION);
        F1_USE = FILE_NAME;
        $display("F1_USE before: %s", F1_USE);
        $value$plusargs(F1_PARGS, F1_USE);
        $display("F1_USE after: %s", F1_USE);

        @(posedge flush);
        @(negedge flush);

        @(posedge clk);
        @(posedge clk);
        @(posedge clk);

        while(done_count != 1 | length_count != 0) begin
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
                num_rx = num_rx + 1;
                if (set_inst == 0) begin
                    set_inst = 1;
                    stream_count = seg_mode ? 2 : 1;
                end
                else begin
                    if (stream_count != 0 & length_count == 0) begin
                        length_count = data + 1;
                        stream_count = stream_count - 1;
                        if (length_count == 1) begin
                            done_count = done_count - 1;
                            set_inst = 0;
                        end
                    end
                    else if (stream_count == 0 & length_count == 1) begin
                        done_count = done_count - 1;
                        set_inst = 0;
                    end

                    length_count = length_count - 1;
                end
            end
        end
        @(posedge clk);
        #1;
        assert(valid == 0) else $error("Valid signal fails to end");
        ready = 0;
        $writememh(F1_USE, local_mem_0);

    end

    @(posedge clk);
    ready = 0;
    done = 1;

end

endmodule
