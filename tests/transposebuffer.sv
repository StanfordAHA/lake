module transpose_buffer (
  input logic clk,
  output logic [3:0] col_pixels,
  input logic [3:0] mem_data,
  input logic rst_n,
  input logic [3:0] valid_input
);

logic [1:0] col_index;
logic [1:0] row_index;
logic switch_buf;
logic [7:0][3:0] tb;
logic [15:0] valid_data;

always_ff @(posedge clk) begin
  tb[(3'h4 * 3'(switch_buf)) + 3'(row_index)][0] <= valid_data[0];
  tb[(3'h4 * 3'(switch_buf)) + 3'(row_index)][1] <= valid_data[1];
  tb[(3'h4 * 3'(switch_buf)) + 3'(row_index)][2] <= valid_data[2];
  tb[(3'h4 * 3'(switch_buf)) + 3'(row_index)][3] <= valid_data[3];
end

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
  if (switch_buf == 1'h0) begin
    col_pixels[0] = tb[4][col_index];
    col_pixels[1] = tb[5][col_index];
    col_pixels[2] = tb[6][col_index];
    col_pixels[3] = tb[7][col_index];
  end
  else begin
    col_pixels[0] = tb[0][col_index];
    col_pixels[1] = tb[1][col_index];
    col_pixels[2] = tb[2][col_index];
    col_pixels[3] = tb[3][col_index];
  end
end
always_comb begin
  valid_data[0] = 1'h0;
  valid_data[1] = 1'h0;
  valid_data[2] = 1'h0;
  valid_data[3] = 1'h0;
  if (valid_input[0] == 1'h1) begin
    valid_data[0] = mem_data[0];
  end
  if (valid_input[1] == 1'h1) begin
    valid_data[(4'(valid_input[0]) + 4'(valid_input[1])) - 4'h1] = mem_data[1];
  end
  if (valid_input[2] == 1'h1) begin
    valid_data[(4'(valid_input[0]) + 4'(valid_input[1]) + 4'(valid_input[2])) - 4'h1] = mem_data[2];
  end
  if (valid_input[3] == 1'h1) begin
    valid_data[(4'(valid_input[0]) + 4'(valid_input[1]) + 4'(valid_input[2]) + 4'(valid_input[3])) - 4'h1] = mem_data[3];
  end
end
endmodule   // transpose_buffer

