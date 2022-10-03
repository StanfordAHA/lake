module repsiggen_tb;

  reg base_data_in_valid, clk, clk_en, passthru_data_out_ready, repsig_data_out_ready, rst_n, tile_en;
  reg [16:0] base_data_in;
  reg [15:0] stop_lvl;

  reg base_data_in_ready, passthru_data_out_valid, repsig_data_out_valid;
  reg [16:0] passthru_data_out;
  reg [16:0] repsig_data_out;

  RepeatSignalGenerator rep (.base_data_in(base_data_in), .base_data_in_valid(base_data_in_valid), .clk(clk), .clk_en(clk_en), .passthru_data_out_ready(passthru_data_out_ready), .repsig_data_out_ready(repsig_data_out_ready), .rst_n(rst_n), .stop_lvl(stop_lvl), .tile_en(tile_en), .base_data_in_ready(base_data_in_ready), .passthru_data_out(passthru_data_out), .passthru_data_out_valid(passthru_data_out_valid), .repsig_data_out(repsig_data_out), .repsig_data_out_valid(repsig_data_out_valid));

  /*input logic base_data_in_valid,
  input logic clk,
  input logic clk_en,
  input logic passthru_data_out_ready,
  input logic repsig_data_out_ready,
  input logic rst_n,
  input logic [15:0] stop_lvl,
  input logic tile_en,
  output logic base_data_in_ready,
  output logic [16:0] passthru_data_out,
  output logic passthru_data_out_valid,
  output logic [16:0] repsig_data_out,
  output logic repsig_data_out_valid
  */
  int time_out = 1000;
  int time_cycles = 0;

  string line;
  int fd;

  int in_addr;
  int gld_addr;
  int addr;
  int out_addr;

  reg [16:0] base_data_in_arr;
  reg [16:0] passthru_data_out_arr;
  reg [16:0] repsig_data_out_arr;


  initial begin
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_in_stream", "r");
	in_addr = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  base_data_in_arr[in_addr] = line.atoi();
	  in_addr += 1;
	end
	in_addr -= 1;

	gld_addr = 0;
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_gold", "r");
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  repsig_data_out_arr[gld_addr] = line.atoi();
	  gld_addr += 1;
	end
	gld_addr -= 1;
	addr = 0;
  end
  
  always @(posedge clk)
  begin
	time_cycles += 1;
	if (base_data_in_valid)
	begin
		base_data_in = base_data_in_arr[addr];
		addr = addr + 1;
	end
	if (time_cycles > time_out)
	begin
		$display("Error: TIME OUT");
		$finish;
	end
  end

  always @(posedge clk)
  begin
	$display("Cycle, Data Valid, gold_value, actual_value, out_addr, gold_addr, in_addr");
	$display("%10d, %10d, %10d, %10d, %10d, %10d %10d", time_cycles, repsig_data_out_valid, repsig_data_out_arr[out_addr], repsig_data_out, out_addr, gld_addr, in_addr);
	
 	if (repsig_data_out_valid)
	begin
		if (out_addr < gld_addr)
		begin
		       //gold_out_d[out_addr] = data_out;
		       if (repsig_data_out_arr[out_addr] != repsig_data_out)
		       begin
			       $display("Error");
			       //$finish;
		       end
		       out_addr += 1;
	       end
	end
       if (out_addr >= gld_addr)
       begin
	       $display("Simulation Done");
	       $finish;
       end	
  end
  
  //reg base_data_in_valid, clk, clk_en, passthru_data_out_ready, repsig_data_out_ready, rst_n, tile_en;
  //reg [16:0] base_data_in;

  always
    begin
	clk = 1'b1;
  	#20;
	clk = 1'b0;
	#20;
    end
  initial
  begin
	  rst_n = 1'b0;
	  clk_en = 1'b0;
	  #40;
	  rst_n = 1'b1;
	  clk_en = 1'b1;
	  tile_en = 1'b1;
	  #20;
	  out_addr = 0;
	  passthru_data_out_ready = 1'b1;
	  base_data_in_valid = 1'b1;
	  repsig_data_out_ready = 1'b1;
	  tile_en = 1'b1;
	  stop_lvl = 0;
  end
endmodule
