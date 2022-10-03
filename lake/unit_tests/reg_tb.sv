module reg_tb;

  reg clk, clk_en, data_in_valid, data_out_ready, rst_n, tile_en, data_in_ready, data_out_valid;
  reg [16:0] data_in;
  reg [15:0] default_value;
  reg [15:0] stop_lvl;

  reg [16:0] data_out;

  reg_cr mod (.clk(clk), .clk_en(clk_en), .data_in(data_in), .data_in_valid(data_in_valid), .data_out_ready(data_out_ready), .default_value(default_value), .rst_n(rst_n), .stop_lvl(stop_lvl), .tile_en(tile_en), .data_in_ready(data_in_ready), .data_out(data_out), .data_out_valid(data_out_valid));
  int time_out = 1000;
  int time_cycles = 0;

  string line;
  int fd;

  int in_addr;
  int gld_addr;
  int addr;
  int out_addr;

  reg [16:0] data[16:0];
  reg [16:0] gold_out_d[16:0];

  initial begin
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_val", "r");
	in_addr = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  data[in_addr] = line.atoi();
	  in_addr += 1;
	end
	in_addr -= 1;	
       	
	gld_addr = 0;
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold", "r");
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  gold_out_d[gld_addr] = line.atoi();
	  gld_addr += 1;
	end
	gld_addr -= 1;
	addr = 0;
  end
  
  always @(posedge clk)
  begin
	time_cycles += 1;
	if (data_in_valid)
	begin
		data_in = data[addr];
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
	if (data_out_valid)
	begin
		if (out_addr < gld_addr)
		begin
		       //gold_out_d[out_addr] = data_out;
		       if (gold_out_d[out_addr] != data_out)
		       begin
			       $display("Error");
			       $finish;
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
  initial
  begin
	  rst_n = 1'b0;
	  clk_en = 1'b0;
	  #40;
	  rst_n = 1'b1;
	  clk_en = 1'b1;
	  tile_en = 1'b1;
	  #20;
	  addr = 0;
	  out_addr = 0;
	  data_out_ready = 1'b1;
	  #20;
	  data_in_valid = 1'b1;
	  default_value = 0;
  end
endmodule
