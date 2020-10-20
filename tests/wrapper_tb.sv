`define CLK_PERIOD 1.1
`define CLK_PERIOD 1.1
`define ASSIGNMENT_DELAY 0.22000000000000003
`define CONFIG_TIME 4096
`define RUN_TIME 406900

module TB;

    reg [0:0] clk;
    reg [15:0] data_in_0;
    reg [0:0] rst_n;
    reg [15:0] data_out_0;
    reg [15:0] data_out_1;
    reg [0:0] empty;
    reg [0:0] full;
    reg [1:0] read_out;
    reg [1:0] valid_out;
    reg [31:0] config0;
    reg [31:0] config1;
    reg [2:0] test;
    // reg [0:0] stencil_valid;

    LakeWrapper DUT (
        .addr_in_0(0),
        .addr_in_1(0),
        .chain_data_in_0(0),
        .chain_data_in_1(0),
        .clk(clk),
        .clk_en(1),
        .config_addr_in(0),
        .config_data_in(0),
        .config_en(0),
        .config_read(0),
        .config_write(0),
        .data_in_0(data_in_0),
        .data_in_1(0),
        .fifo_ctrl_fifo_depth(0),
        .flush(0),
        .ren_in(0),
        .rst_n(rst_n),
        .wen_in(0),
        .config_data_out_0(config0),
        .config_data_out_1(config1),
        .data_out_0(data_out_0),
        .data_out_1(data_out_1),
        .empty(empty),
        .full(full),
        .sram_ready_out(read_out),
        // .stencil_valid(stencil_valid),
        .valid_out(valid_out)
    );

    always #(`CLK_PERIOD/2) clk =~clk;

    always @ (posedge clk) begin
        if (test < 2) begin
            test += 1;
            rst_n = 0;
            data_in_0 = 0;
        end else begin
            test = 2;
            rst_n = 1;
            data_in_0 = data_in_0 + 1;
        end
    end

    initial begin
      clk <= 0;
      data_in_0 = 0;
      test = 0;
      rst_n <= 0;
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
