module ReverseFlatten (
  input logic [63:0] input_array,
  output logic [3:0][0:0] [15:0] output_array
);

always_comb begin
  output_array[0] = input_array[15:0];
  output_array[1] = input_array[31:16];
  output_array[2] = input_array[47:32];
  output_array[3] = input_array[63:48];
end
endmodule   // ReverseFlatten

module TSMC_generator (
  input logic clk,
  input logic clk_en,
  input logic [7:0] mem_addr_in_bank,
  input logic mem_cen_in_bank,
  input logic [3:0] [15:0] mem_data_in_bank,
  input logic mem_wen_in_bank,
  input logic [1:0] rtsel,
  input logic [1:0] wtsel,
  output logic [0:0][3:0] [15:0] mem_data_out_bank,
);

logic [7:0] mem_addr_to_sram;
logic mem_cen_in_bank_chain;
logic mem_inst_0_sram_cen;
logic mem_inst_0_sram_wen;
logic mem_wen_in_bank_chain;
logic [63:0] sram_mem_data_in_bank;
logic [63:0] sram_mem_data_out_bank;
assign mem_inst_0_sram_cen = ~mem_cen_in_bank_chain;
assign mem_inst_0_sram_wen = ~mem_wen_in_bank_chain;
always_comb begin
  chain_idx_tile = 1'h0;
end
always_comb begin
  mem_addr_to_sram = mem_addr_in_bank;
end
always_comb begin
  mem_wen_in_bank_chain = mem_wen_in_bank;
end
always_comb begin
  mem_cen_in_bank_chain = mem_cen_in_bank;
end
flattenND flatten_data_in_0 (
  .input_array(mem_data_in_bank),
  .output_array(sram_mem_data_in_bank)
);

TSMC mem_inst_0 (
  .A(mem_addr_to_sram),
  .CEB(mem_inst_0_sram_cen),
  .CLK(clk),
  .D(sram_mem_data_in_bank),
  .RTSEL(2'h0),
  .WEB(mem_inst_0_sram_wen),
  .WTSEL(2'h0),
  .Q(sram_mem_data_out_bank)
);

ReverseFlatten flatten_data_out_0 (
  .input_array(sram_mem_data_out_bank),
  .output_array(mem_data_out_bank)
);

endmodule   // TSMC_generator

module flattenND (
  input logic [3:0][0:0] [15:0] input_array,
  output logic [63:0] output_array
);

always_comb begin
  output_array[15:0] = input_array[0];
  output_array[31:16] = input_array[1];
  output_array[47:32] = input_array[2];
  output_array[63:48] = input_array[3];
end
endmodule   // flattenND

