`define CLK_PERIOD 1.1
`define CLK_PERIOD 1.1
`define ASSIGNMENT_DELAY 0.22000000000000003
`define CONFIG_TIME 4096
`define RUN_TIME 4096

module TB;

    reg [0:0] clk;
    reg [15:0] in_input_read_0;
    reg [0:0] reset;
    reg [15:0] out_output_write_0;
    reg [14:0] addr;
    reg [15:0] test_vectors [12287:0];

    conv33_naive_compute DUT (
        .clk(clk),
        .in_input_read_0(in_input_read_0),
        .reset(reset),
        .out_output_write_0(out_output_write_0)
    );

    always #(`CLK_PERIOD/2) clk =~clk;

    initial begin
        reset = 1'b0;
        #`CLK_PERIOD
        #`CLK_PERIOD
        reset = 1'b1;
    end

    always @ (posedge clk) begin
        addr <= #`ASSIGNMENT_DELAY (addr + 1);
        in_input_read_0[15:0] <= test_vectors[addr];
    end

    initial begin
      $readmemh("test_vectors.txt", test_vectors);
      clk <= 0;
      addr <= 0;
    end
  
    initial begin
      $vcdplusfile("dump.vpd");
      $vcdplusmemon();
      $vcdpluson(0, TB);
      $set_toggle_region(TB);
      #(`CONFIG_TIME);
      $toggle_start();
      #(`RUN_TIME);
      $toggle_stop();
      $toggle_report("outputs/run.saif", 1e-9, TB);
      $finish(2);
    end

endmodule
