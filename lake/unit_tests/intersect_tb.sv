module intersect_unit_tb;
  
  reg clk, clk_en, coord_in_0_valid , coord_in_1_valid, coord_out_ready, joiner_op, pos_in_0_valid, pos_in_1_valid, pos_out_0_ready, pos_out_1_ready, rst_n, tile_en;
  reg [32:0] coord_in_0;
  reg [32:0] coord_in_1;
  reg [32:0] pos_in_0;
  reg [32:0] pos_in_1;

  reg coord_in_0_ready, coord_in_1_ready, coord_out_valid, pos_in_0_ready, pos_in_1_ready, pos_out_0_valid, pos_out_1_valid;
  reg [32:0] coord_out;
  reg [32:0] pos_out_0;
  reg [32:0] pos_out_1;
 
  reg [32:0] gold_coord_out;
  reg [32:0] gold_pos_out_0;
  reg [32:0] gold_pos_out_1;

  reg [32:0] addr;
  reg [32:0] out_addr;


  reg [32:0] coord0[16:0];
  reg [32:0] coord1[16:0];
  reg [32:0] pos0[16:0];
  reg [32:0] pos1[16:0];


  reg [32:0] gold_coord[16:0];
  reg [32:0] gold_pos0[16:0];
  reg [32:0] gold_pos1[16:0];

  int time_out = 1000;
  int time_cycles = 0;

  intersect_unit mod (.clk(clk), .clk_en(clk_en), .coord_in_0(coord_in_0), .coord_in_0_valid(coord_in_0_valid), .coord_in_1(coord_in_1), .coord_in_1_valid(coord_in_1_valid), .coord_out_ready(coord_out_ready), .joiner_op(joiner_op), .pos_in_0(pos_in_0), .pos_in_0_valid(pos_in_0_valid), .pos_in_1(pos_in_1), .pos_in_1_valid(pos_in_1_valid), .pos_out_0_ready(pos_out_0_ready), .pos_out_1_ready(pos_out_1_ready), .rst_n(rst_n), .tile_en(tile_en), .coord_in_0_ready(coord_in_0_ready), .coord_in_1_ready(coord_in_1_ready), .coord_out(coord_out), .coord_out_valid(coord_out_valid), .pos_in_0_ready(pos_in_0_ready), .pos_in_1_ready(pos_in_1_ready), .pos_out_0(pos_out_0), .pos_out_0_valid(pos_out_0_valid), .pos_out_1(pos_out_1), .pos_out_1_valid(pos_out_1_valid) );
  
  string line;
  int fd;
  int a2;
  initial begin
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_crd1", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  coord0[a2] = line.atoi();
	  a2 += 1;
	end
	a2 = 0;

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_crd2", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  coord1[a2] = line.atoi();
	  a2 += 1;
	end
	a2 = 0;

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_ref1", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  pos0[a2] = line.atoi();
	  a2 += 1;
	end
	a2 = 0;

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_ref2", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  pos1[a2] = line.atoi();
	  a2 += 1;
	end
	a2 = 0;
	
	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_crd", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  gold_coord[a2] = line.atoi();
	  a2 += 1;
	end
	a2 = 0;

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_ref1", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  //$display("Line read : %d , %d , %d", line.atoi(), a2, coord0[1]);
	  gold_pos0[a2] = line.atoi();
	  a2 += 1;
	end
	a2=0;

	fd = $fopen("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_ref2", "r");
	a2 = 0;
  	while (!$feof(fd)) begin
	  $fgets(line , fd);
	  gold_pos1[a2] = line.atoi();
	  a2 += 1;
	end

  end

  /*initial
  begin
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_crd2", coord1);
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_crd1", coord0);
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_ref1", pos0);
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_ref2", pos1);

	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_crd", gold_coord); 
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_ref1", gold_pos0);
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_ref2", gold_pos1);
  end
*/
  always @(posedge clk)
  begin
	time_cycles += 1;
	if (coord_in_0_valid)
	begin

		coord_in_0 = coord0[addr];
		coord_in_1 = coord1[addr];
		pos_in_1 = pos1[addr];
		pos_in_0 = pos0[addr];

		gold_coord_out = gold_coord[addr];
		gold_pos_out_0 = gold_pos0[addr];
		gold_pos_out_1 = gold_pos1[addr];
		addr = addr + 1;
		/*if (addr >= a2)
		begin
			coord_in_1_valid = 1'b0;
    			coord_in_0_valid = 1'b0;
			pos_in_0_valid = 1'b0;
			pos_in_1_valid = 1'b0;
		end*/
	end
	if (time_cycles > time_out)
	begin
		$display("Simulation Done");
		$finish;
	end
  end


  reg[16:0] out_coord_out[16:0];
  reg[16:0] out_pos_out_0[16:0];
  reg[16:0] out_pos_out_1[16:0];

  always @(posedge clk)
  begin
	 
	/* begin
		$display("out_cd, out_ref1, out_ref2, gold_crd, gold_ref1, gold_ref2");
	  	$display(" %10b,   %10b,   %10b,	%10b,	%10b,	%10b", coord_out, pos_out_0, pos_out_1, gold_coord_out, gold_pos_out_0, gold_pos_out_1);
	 end
	*/
	if (coord_out_valid)
	begin
		if (out_addr < addr)
		begin
		 	out_coord_out[out_addr] = coord_out;
		       out_pos_out_0[out_addr] = pos_out_0;
		       out_pos_out_1[out_addr] = pos_out_1;
	
	  		//$display("CHECK::: %10b, %10b, %10b, %10b,   %10b, %10b", coord_out, gold_coord_out[out_addr], pos_out_0, gold_pos0[out_addr], pos_out_1, gold_pos1[out_addr]);
		       if ((out_coord_out[out_addr] != gold_coord_out[out_addr]) && (out_pos_out_0[out_addr] != gold_pos0[out_addr]) && (out_pos_out_1[out_addr] != gold_pos1[out_addr]))
		       begin
			       $display("Error");
			       $finish;
		       end
		       //$display("Output address val %d", out_addr);
		       out_addr += 1;		       
	       end

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
  end
endmodule
