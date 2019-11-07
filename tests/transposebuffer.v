module transpose_buffer (
  input logic clk,
  output logic [1:0] col_pixels,
  input logic [3:0] mem_data,
  output logic read_valid,
  input logic rst_n,
  output logic stencil_valid,
  input logic [3:0] valid_input
);

logic [1:0] col_index;
logic [3:0][1:0] indices;
logic [1:0] num_valid;
logic row_index;
logic switch_buf;
logic [3:0][3:0] tb;

always_ff @(posedge clk) begin
  num_valid <= 2'h3;
  indices[0] <= 2'h0;
  indices[1] <= 2'h1;
  indices[2] <= 2'h2;
  indices[3] <= 2'h3;
  if (valid_input[0] == 1'h0) begin
    indices[0] <= indices[1];
    indices[1] <= indices[2];
    indices[2] <= indices[3];
    num_valid <= num_valid - 2'h1;
  end
  if (valid_input[1] == 1'h0) begin
    indices[1] <= indices[2];
    indices[2] <= indices[3];
    num_valid <= num_valid - 2'h1;
  end
  if (valid_input[2] == 1'h0) begin
    indices[2] <= indices[3];
    num_valid <= num_valid - 2'h1;
  end
  if (valid_input[3] == 1'h0) begin
    num_valid <= num_valid - 2'h1;
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (rst_n == 1'h0) begin
    col_index <= 2'h0;
    row_index <= 1'h0;
    switch_buf <= 1'h0;
  end
  else if (col_index == 2'h3) begin
    col_index <= 2'h0;
    row_index <= 1'h0;
    switch_buf <= ~switch_buf;
  end
  else begin
    col_index <= col_index + 2'h1;
    row_index <= row_index + 1'h1;
    switch_buf <= switch_buf;
  end
end

always_ff @(posedge clk) begin
  if (switch_buf == 1'h0) begin
    col_pixels[0] <= tb[2][col_index];
    col_pixels[1] <= tb[3][col_index];
  end
  else begin
    col_pixels[0] <= tb[0][col_index];
    col_pixels[1] <= tb[1][col_index];
  end
end
always_comb begin
  read_valid = 1'h1;
  stencil_valid = 1'h1;
end
endmodule   // transpose_buffer

