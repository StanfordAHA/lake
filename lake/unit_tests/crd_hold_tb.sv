module crd_hold_tb;

  reg clk, clk_en, cmrg_coord_in_0_valid, cmrg_coord_in_1_valid, cmrg_coord_out_0_ready, cmrg_coord_out_1_ready, cmrg_enable, rst_n, tile_en;
  reg [16:0] cmrg_coord_in_0;
  reg [16:0] cmrg_coord_in_1;
  reg [15:0] cmrg_stop_lvl;

  reg cmrg_coord_in_0_ready, cmrg_coord_in_1_ready, cmrg_coord_out_0_valid, cmrg_coord_out_1_valid;
  reg [16:0] cmrg_coord_out_0;
  reg [16:0] cmrg_coord_out_1;

  crdhold hold (.clk(clk), .clk_en(clk_en), .cmrg_coord_in_0(cmrg_coord_in_0), .cmrg_coord_in_0_valid(cmrg_coord_in_0_valid), .cmrg_coord_in_1(cmrg_coord_in_1), .cmrg_coord_in_1_valid(cmrg_coord_in_1_valid), .cmrg_coord_out_0_ready(cmrg_coord_out_0_ready), .cmrg_coord_out_1_ready(cmrg_coord_out_1_ready), .cmrg_enable(cmrg_enable), .cmrg_stop_lvl(cmrg_stop_lvl), .rst_n(rst_n), .tile_en(tile_en),  .cmrg_coord_in_0_ready(cmrg_coord_in_0_ready), .cmrg_coord_in_1_ready(cmrg_coord_in_1_ready), .cmrg_coord_out_0(cmrg_coord_out_0), .cmrg_coord_out_0_valid(cmrg_coord_out_0_valid), .cmrg_coord_out_1(cmrg_coord_out_1), .cmrg_coord_out_1_valid(cmrg_coord_out_1_valid));

  int time_out = 1000;
  int time_cycles = 0;

  string line;
  int fd;

  int in_addr;
  int gld_addr;
  int addr;
  int out_addr;

  reg [16:0] cmrg_coord_in_0_arr[16:0];
  reg [16:0] cmrg_coord_in_1_arr[16:0];
  reg [16:0] cmrg_coord_out_0_arr[16:0];
  reg [16:0] cmrg_coord_out_1_arr_gld[16:0];

  /*initial
  begin
	  $dumpfile("waveforms.vcd");
	  $dumpvars(0,top);
  end*/

  initial begin
	//$dumpfile("test.vcd");
	$dumpvars;
  	  fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_icrd", "r");
	in_addr = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  cmrg_coord_in_0_arr[in_addr] = line.atoi();
	  in_addr += 1;
	end

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_ocrd", "r");
	in_addr = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  cmrg_coord_in_1_arr[in_addr] = line.atoi();
	  in_addr += 1;
	end

	in_addr -= 1;	
       	
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_gold", "r");
  	gld_addr = 0;
	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  cmrg_coord_out_1_arr_gld[gld_addr] = line.atoi();
	  gld_addr += 1;
	end
	gld_addr = gld_addr - 1;
	addr = 0;

  end
  
  always @(posedge clk)
  begin
	time_cycles += 1;
	if (cmrg_coord_in_1_valid)
	begin
		cmrg_coord_in_0 = cmrg_coord_in_0_arr[addr];
		cmrg_coord_in_1 = cmrg_coord_in_1_arr[addr];
		//cmrg_coord_out_1 = cmrg_coord_out_1_arr[addr];
		//cmrg_coord_in_0 = cmrg_coord_in_0_arr[addr];
		addr = addr + 1;
	end
	if (time_cycles > time_out)
	begin
		$display("Error: TIME OUT");
		$finish;
	end
  end
  
  reg gold_crd1_val;
  reg gold_crd0_val;

  //reg[16:0] out_coord_out[16:0];
  always @(posedge clk)
  begin
	$display("	Cycle, 	Data Valid, gold_value,   out_value,	  out_addr, output size");
	$display("%d,    %10d, %10d, %10d,     %10d", time_cycles, cmrg_coord_out_1_valid, cmrg_coord_out_1_arr_gld[out_addr], cmrg_coord_out_1, out_addr, gld_addr);
	
  	
	if (cmrg_coord_out_1_valid)
	begin
		gold_crd1_val = cmrg_coord_out_1_arr_gld[out_addr];
		gold_crd0_val = cmrg_coord_in_0_arr[out_addr];

		if (out_addr <= gld_addr)
		begin
		       //gold_out_d[out_addr] = data_out;
		       if ((cmrg_coord_out_1_arr_gld[out_addr] != cmrg_coord_out_1) && (gold_crd0_val != cmrg_coord_out_0))
		       begin
			       $display("Error");
			       $finish;
		       end
		       out_addr += 1;
	       end
	end
       if (out_addr >= gld_addr)
       begin
	       //$dumpfile("temp.vcs");
	       $display("Simulation Done");
	       $finish;
       end
	
  end
  always
    begin
	clk = 1'b1;
  	#20;
	clk = 1'b0;
	#20;
    end
  //reg clk, clk_en, cmrg_coord_in_0_valid, cmrg_coord_in_1_valid, cmrg_coord_out_0_ready, cmrg_coord_out_1_ready, cmrg_enable, rst_n, tile_en;
  //reg [16:0] cmrg_coord_in_0;
  //reg [16:0] cmrg_coord_in_1;
  //reg [16:0] cmrg_stop_lvl;

  initial
  begin
	  addr = 0;
	  rst_n = 1'b0;
	  #40
	  rst_n = 1'b1;
	  out_addr = 0;
	  clk_en = 1'b1;
	  tile_en = 1'b1;
	  cmrg_stop_lvl = 0;
	  #40
	  cmrg_coord_in_0_valid = 1'b1;
	  cmrg_coord_in_1_valid = 1'b1;
	  cmrg_coord_out_0_ready = 1'b1;
	  cmrg_coord_out_1_ready = 1'b1;
	  cmrg_enable = 1'b1;
  end
endmodule
