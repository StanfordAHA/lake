module intersect_unit_tb;
  
  reg clk, clk_en, coord_in_0_valid , coord_in_1_valid, coord_out_ready, joiner_op, pos_in_0_valid, pos_in_1_valid, pos_out_0_ready, pos_out_1_ready, rst_n, tile_en;
  reg [16:0] coord_in_0;
  reg [16:0] coord_in_1;
  reg [16:0] pos_in_0;
  reg [16:0] pos_in_1;

  reg coord_in_0_ready, coord_in_1_ready, coord_out_valid, pos_in_0_ready, pos_in_1_ready, pos_out_0_valid, pos_out_1_valid;
  reg [16:0] coord_out;
  reg [16:0] pos_out_0;
  reg [16:0] pos_out_1;
 
  reg [16:0] gold_coord_out;
  reg [16:0] gold_pos_out_0;
  reg [16:0] gold_pos_out_1;

  reg [16:0] addr;


  reg [16:0] coord0[16:0];
  reg [16:0] coord1[16:0];
  reg [16:0] pos0[16:0];
  reg [16:0] pos1[16:0];


  reg [16:0] gold_coord[16:0];
  reg [16:0] gold_pos0[16:0];
  reg [16:0] gold_pos1[16:0];

  intersect_unit mod (.clk(clk), .clk_en(clk_en), .coord_in_0(coord_in_0), .coord_in_0_valid(coord_in_0_valid), .coord_in_1(coord_in_1), .coord_in_1_valid(coord_in_1_valid), .coord_out_ready(coord_out_ready), .joiner_op(joiner_op), .pos_in_0(pos_in_0), .pos_in_0_valid(pos_in_0_valid), .pos_in_1(pos_in_1), .pos_in_1_valid(pos_in_1_valid), .pos_out_0_ready(pos_out_0_ready), .pos_out_1_ready(pos_out_1_ready), .rst_n(rst_n), .tile_en(tile_en), .coord_in_0_ready(coord_in_0_ready), .coord_in_1_ready(coord_in_1_ready), .coord_out(coord_out), .coord_out_valid(coord_out_valid), .pos_in_0_ready(pos_in_0_ready), .pos_in_1_ready(pos_in_1_ready), .pos_out_0(pos_out_0), .pos_out_0_valid(pos_out_0_valid), .pos_out_1(pos_out_1), .pos_out_1_valid(pos_out_1_valid) );

  initial
  begin
	  $readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_crd2", coord1);
		$readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_crd1", coord0);
		$readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_ref1", pos0);
		$readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_ref2", pos1);


		$readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_crd", gold_coord); 
		$readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_ref1", gold_pos0);
		$readmemh("/aha/lake/lake/unit_tests_gen/temp_rit/test_1_gold_ref2", gold_pos1);
	end	


  always @(posedge clk)
  begin
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
		
		$display("time, clk, out_cd, out_ref1, out_ref2, gold_crd, gold_ref1, gold_ref2");
	  	$display("%10b, %10b, %10b,       %10b,	%10b,	%10b,	%10b,		%10b", $time, clk, coord_out, pos_out_0, pos_out_1, gold_coord_out, gold_pos_out_0, gold_pos_out_1);
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
	  #20;

	  coord_in_0_valid = 1'b1;
	  coord_in_1_valid = 1'b1;
  	  joiner_op = 1'b0;
	  pos_in_0_valid = 1'b1;
	  pos_in_1_valid = 1'b1;

	  coord_out_ready = 1'b1;
	  pos_out_0_ready = 1'b1;
	  pos_out_1_ready = 1'b1;
  end
endmodule
