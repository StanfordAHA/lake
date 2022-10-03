module repeat_tb;
  reg clk, clk_en, proc_data_in_valid, ref_data_out_ready, repsig_data_in_valid, root, rst_n, spacc_mode, tile_en;
  reg [16:0] proc_data_in;
  reg [16:0] repsig_data_in;
  reg [15:0] stop_lvl;

  reg proc_data_in_ready, data_out_valid, repsig_data_in_ready;
  reg [16:0] data_out;

  Repeat rep (.clk(clk), .clk_en(clk_en), .proc_data_in(proc_data_in), .proc_data_in_valid(proc_data_in_valid), .ref_data_out_ready(ref_data_out_ready), .repsig_data_in(repsig_data_in), .repsig_data_in_valid(repsig_data_in_valid), .root(root), .rst_n(rst_n), .spacc_mode(spacc_mode), .stop_lvl(stop_lvl), .tile_en(tile_en), .proc_data_in_ready(proc_data_in_ready), .ref_data_out(data_out), .ref_data_out_valid(data_out_valid), .repsig_data_in_ready(repsig_data_in_ready));

  int time_out = 1000;
  int time_cycles = 0;

  string line;
  int fd;

  int in_addr;
  int gld_addr;
  int addr;
  int out_addr;

  reg [16:0] proc_data_in_arr[16:0];
  reg [16:0] repsig_data_in_arr[16:0];
  reg [16:0] gold_ref_data_out[16:0];

  initial begin
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_in_ref", "r");
	in_addr = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  proc_data_in_arr[in_addr] = line.atoi();
	  in_addr += 1;
	end

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_in_repeat", "r");
	in_addr = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  repsig_data_in_arr[in_addr] = line.atoi();
	  in_addr += 1;
	end

	in_addr -= 1;	
       	
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_gold", "r");
  	gld_addr = 0;
	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  gold_ref_data_out[gld_addr] = line.atoi();
	  gld_addr += 1;
	end
	gld_addr = gld_addr - 1;
	addr = 0;

  end
  
  always @(posedge clk)
  begin
	time_cycles += 1;
	if (proc_data_in_valid)
	begin
		proc_data_in = proc_data_in_arr[addr];
		repsig_data_in = repsig_data_in_arr[addr];
		addr = addr + 1;
	end
	if (time_cycles > time_out)
	begin
		$display("Error: TIME OUT");
		$finish;
	end
  end

  //reg[16:0] out_coord_out[16:0];
  always @(posedge clk)
  begin
	$display("Cycle, Data Valid, gold_value, actual_value, out_addr, gold_addr, in_addr");
	$display("%10d, %10d, %10d, %10d, %10d, %10d %10d", time_cycles, data_out_valid, gold_ref_data_out[out_addr], data_out, out_addr, gld_addr, in_addr);
	
  	if (data_out_valid)
	begin
		if (out_addr < gld_addr)
		begin
		       //gold_out_d[out_addr] = data_out;
		       if (gold_ref_data_out[out_addr] != data_out)
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
  always
    begin
	clk = 1'b1;
  	#20;
	clk = 1'b0;
	#20;
    end

  //reg clk, clk_en, data_in_valid, ref_data_out_ready, repsig_data_in_valid, root, rst_n, spacc_mode, tile_en;
  //reg [16:0] proc_data_in;
  //reg [16:0] repsig_data_in;
  //reg [15:0] stop_lvl;

  //reg proc_data_in_ready, data_out_valid, repsig_data_in_ready;
  //reg [16:0] data_out;

	  /*rst_n = 1'b0;
	  clk_en = 1'b0;
	  #60;
	  rst_n = 1'b1;
	  #20;
	  clk_en = 1'b1;
	  addr = 0;
	  out_addr = 0;
	  #20;

	  tile_en = 1'b1;
	  #20;
	  coord_in_1_valid = 1'b1;
	  coord_in_0_valid = 1'b1;
  	  joiner_op = 1'h0;
	  pos_in_0_valid = 1'b1;
	  pos_in_1_valid = 1'b1;

	  coord_out_ready = 1'b1;
	  pos_out_0_ready = 1'b1;
	  pos_out_1_ready = 1'b1;
 

  reg clk, clk_en, proc_data_in_valid, ref_data_out_ready, repsig_data_in_valid, root, rst_n, spacc_mode, tile_en;
  reg [16:0] proc_data_in;
  reg [16:0] repsig_data_in;
  reg [15:0] stop_lvl;
*/


  initial
  begin
	  addr = 0;
	  rst_n = 1'b0;
	  #40
	  rst_n = 1'b1;
	  out_addr = 0;
	  clk_en = 1'b1;
	  tile_en = 1'b1;
	  root = 1'b1;
       	  spacc_mode = 1'b1;
	  stop_lvl = 0;
	  #40
	  proc_data_in_valid = 1'b1;
	  repsig_data_in_valid = 1'b1; 
	  ref_data_out_ready = 1'b1;
  end
endmodule
