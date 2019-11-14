module transpose_buffer (
  input logic clk,
  input logic [3:0] mem_data,
  input logic rst_n,
  input logic [3:0] valid_input
);

logic [1:0] col_index;
logic [3:0] num_valid;
logic [1:0] row_index;
logic switch_buf;
logic [3:0] valid_data;

always_ff @(posedge clk, negedge rst_n) begin
  if (rst_n == 1'h0) begin
    col_index <= 2'h0;
    row_index <= 2'h0;
    switch_buf <= 1'h0;
  end
  else if (col_index == 2'h3) begin
    col_index <= 2'h0;
    row_index <= 2'h0;
    switch_buf <= ~switch_buf;
  end
  else begin
    col_index <= col_index + 2'h1;
    row_index <= row_index + 2'h1;
    switch_buf <= switch_buf;
  end
end
always_comb begin
  valid_data[0] = 1'h0;
  valid_data[1] = 1'h0;
  valid_data[2] = 1'h0;
  valid_data[3] = 1'h0;
  if (valid_input[0] == 1'h1) begin
    valid_data[0] = 1'h1;
  end
end
assign num_valid = 4'(valid_input[0]) + 4'(valid_input[1]) + 4'(valid_input[2]) +
    4'(valid_input[3]);
endmodule   // transpose_buffer

