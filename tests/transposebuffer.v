module transpose_buffer (
  input logic clk,
  input logic [3:0] mem_data,
  input logic rst_n,
  output logic [1:0] testing,
  input logic [3:0] valid_input
);

logic [1:0] col_index;
logic [3:0][2:0] indices;
logic [1:0] row_index;
logic switch_buf;

always_ff @(posedge clk) begin
  indices[0] <= 3'h4;
  indices[1] <= 3'h4;
  indices[2] <= 3'h4;
  indices[3] <= 3'h4;
  indices[0] <= 3'h0;
  indices[1] <= 3'h1;
  indices[2] <= 3'h2;
  indices[3] <= 3'h3;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (rst_n == 1'h0) begin
    col_index <= 2'h0;
    row_index <= 2'h0;
    switch_buf <= 1'h0;
    testing <= col_index;
  end
  else if (col_index == 2'h3) begin
    col_index <= 2'h0;
    row_index <= 2'h0;
    switch_buf <= ~switch_buf;
    testing <= col_index;
  end
  else begin
    col_index <= col_index + 2'h1;
    row_index <= row_index + 2'h1;
    switch_buf <= switch_buf;
    testing <= col_index;
  end
end
endmodule   // transpose_buffer

