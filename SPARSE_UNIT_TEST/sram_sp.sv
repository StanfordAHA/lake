module sram_sp (
  input logic clk,
  input logic clk_en,
  input logic [63:0] data_in_p0,
  input logic flush,
  input logic [8:0] read_addr_p0,
  input logic read_enable_p0,
  input logic [8:0] write_addr_p0,
  input logic write_enable_p0,
  output logic [63:0] data_out_p0
);

logic [63:0] data_array [511:0];

always_ff @(posedge clk) begin
  if (clk_en) begin
    if (write_enable_p0 == 1'h1) begin
      data_array[write_addr_p0] <= data_in_p0;
    end
    else if (read_enable_p0) begin
      data_out_p0 <= data_array[read_addr_p0];
    end
  end
end
endmodule   // sram_sp