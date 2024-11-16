`timescale 1ns/1ns



module mu2f_io_core_tb;

    reg clk;
    reg clk_en;
    reg io2f_17_T0_ready;
    reg io2f_17_T1_ready;
    reg io2f_17_T2_ready;
    reg io2f_17_T3_ready;
    reg io2f_17_T4_ready;
    reg [15:0] mu2io_16_0;
    reg mu2io_16_0_valid;
    reg [15:0] mu2io_16_1;
    reg mu2io_16_1_valid;
    reg [2:0] ready_select_0;
    reg [2:0] ready_select_1;
    reg rst_n;
    reg tile_en;
    reg [1:0] track_select_T0;
    reg [1:0] track_select_T1;
    reg [1:0] track_select_T2;
    reg [1:0] track_select_T3;
    reg [1:0] track_select_T4;

    // wires for dut outputs 
    wire [16:0] io2f_17_T0;
    wire io2f_17_T0_valid;
    wire [16:0] io2f_17_T1;
    wire io2f_17_T1_valid;
    wire [16:0] io2f_17_T2;
    wire io2f_17_T2_valid;
    wire [16:0] io2f_17_T3;
    wire io2f_17_T3_valid;
    wire [16:0] io2f_17_T4;
    wire io2f_17_T4_valid;
    wire mu2io_16_0_ready;
    wire mu2io_16_1_ready;

    mu2f_io_core dut (
        .clk(clk),
        .clk_en(clk_en),
        .io2f_17_T0_ready(io2f_17_T0_ready),
        .io2f_17_T1_ready(io2f_17_T1_ready),
        .io2f_17_T2_ready(io2f_17_T2_ready),
        .io2f_17_T3_ready(io2f_17_T3_ready),
        .io2f_17_T4_ready(io2f_17_T4_ready),
        .mu2io_16_0(mu2io_16_0),
        .mu2io_16_0_valid(mu2io_16_0_valid),
        .mu2io_16_1(mu2io_16_1),
        .mu2io_16_1_valid(mu2io_16_1_valid),
        .ready_select_0(ready_select_0),
        .ready_select_1(ready_select_1),
        .rst_n(rst_n),
        .tile_en(tile_en),
        .track_select_T0(track_select_T0),
        .track_select_T1(track_select_T1),
        .track_select_T2(track_select_T2),
        .track_select_T3(track_select_T3),
        .track_select_T4(track_select_T4),
        .io2f_17_T0(io2f_17_T0),
        .io2f_17_T0_valid(io2f_17_T0_valid),
        .io2f_17_T1(io2f_17_T1),
        .io2f_17_T1_valid(io2f_17_T1_valid),
        .io2f_17_T2(io2f_17_T2),
        .io2f_17_T2_valid(io2f_17_T2_valid),
        .io2f_17_T3(io2f_17_T3),
        .io2f_17_T3_valid(io2f_17_T3_valid),
        .io2f_17_T4(io2f_17_T4),
        .io2f_17_T4_valid(io2f_17_T4_valid),
        .mu2io_16_0_ready(mu2io_16_0_ready),
        .mu2io_16_1_ready(mu2io_16_1_ready)

    );

    initial begin
        clk = 0; // Initialize clock
        forever begin
            # 5 clk = ~clk; 
        end
    end

    initial begin
        mu2io_16_0 = 0;
        mu2io_16_1 = 6;

        // toggle mu2io signals 
        forever begin
            # 10
            if (mu2io_16_0 < 4) begin
                mu2io_16_0 += 1;
            end else begin
                mu2io_16_0 = 0;
            end

            if (mu2io_16_1 < 10) begin
                mu2io_16_1 += 1;
            end else begin
                mu2io_16_1 = 6;
            end

        end
    end


    initial begin
       
        clk_en = 1;
        tile_en = 0;
        rst_n = 0;
        io2f_17_T0_ready = 1;
        io2f_17_T1_ready = 1;
        io2f_17_T2_ready = 1;
        io2f_17_T3_ready = 1;
        io2f_17_T4_ready = 1;

        mu2io_16_0_valid = 1;
        mu2io_16_1_valid = 1;

        track_select_T0 = 2'b00;
        track_select_T1 = 2'b00;
        track_select_T2 = 2'b00;
        track_select_T3 = 2'b00;
        track_select_T4 = 2'b00;

        ready_select_0 = 3'b000;
        ready_select_1 = 3'b000;

        // Release reset
        #10 rst_n = 1;

        // Apply input stimuli 

        // First test with tile_en OFF (mu inactive)
        #15 track_select_T0 = 2'b01; track_select_T1 = 2'b10;
        ready_select_0 = 3'b001; ready_select_1 = 3'b010; 


        // Now turn tile_en ON (mu active); testing track selects
        #10 tile_en = 1;

        #10 track_select_T1 = 2'b00; track_select_T2 = 2'b10;
        #10 track_select_T2 = 2'b00; track_select_T3 = 2'b10;
        #10 track_select_T3 = 2'b00; track_select_T4 = 2'b10;
        #10 track_select_T4 = 2'b00;

        #10 track_select_T0 = 2'b10; track_select_T1 = 2'b01;
        #10 track_select_T1 = 2'b00; track_select_T2 = 2'b01;
        #10 track_select_T2 = 2'b00; track_select_T3 = 2'b01;
        #10 track_select_T3 = 2'b00; track_select_T4 = 2'b01;
        #10 track_select_T4 = 2'b00; track_select_T0 = 2'b00; 


        // Finish simulation 
        #300 $finish;

    end

endmodule