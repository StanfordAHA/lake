module transpose_buffer (
  input logic clk,
  output logic [2:0] col_pixels,
  input logic [3:0] mem_data,
  output logic output_valid,
  input logic rst_n,
  input logic [3:0] valid_input
);

logic [1:0] col_index;
logic max_dim;
logic pause_input;
logic pause_output;
logic [1:0] row_index;
logic switch_buf;
logic [5:0][3:0] tb;
logic [15:0] valid_data;

always_ff @(posedge clk) begin
  if (pause_input == 1'h0) begin
    tb[(3'h3 * 3'(switch_buf)) + 3'(row_index)][0] <= valid_data[0];
    tb[(3'h3 * 3'(switch_buf)) + 3'(row_index)][1] <= valid_data[1];
    tb[(3'h3 * 3'(switch_buf)) + 3'(row_index)][2] <= valid_data[2];
    tb[(3'h3 * 3'(switch_buf)) + 3'(row_index)][3] <= valid_data[3];
  end
  else tb <= tb;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (rst_n == 1'h0) begin
    col_index <= 2'h0;
    row_index <= 2'h0;
    switch_buf <= 1'h0;
    pause_input <= 1'h0;
    pause_output <= 1'h0;
  end
  else if ((max_dim == 1'h0) & (row_index == 2'h2)) begin
    if (col_index == 2'h3) begin
      col_index <= 2'h0;
      row_index <= 2'h0;
      switch_buf <= ~switch_buf;
      pause_input <= 1'h0;
      pause_output <= pause_output;
    end
    else begin
      col_index <= col_index + 2'h1;
      row_index <= row_index;
      switch_buf <= switch_buf;
      pause_input <= 1'h1;
      pause_output <= pause_output;
    end
  end
  else if ((max_dim == 1'h1) & (col_index == 2'h3)) begin
    if (row_index == 2'h2) begin
      col_index <= 2'h0;
      row_index <= 2'h0;
      switch_buf <= ~switch_buf;
      pause_input <= pause_input;
      pause_output <= 1'h0;
    end
    else begin
      col_index <= col_index;
      row_index <= row_index + 2'h1;
      switch_buf <= switch_buf;
      pause_input <= pause_input;
      pause_output <= 1'h1;
    end
  end
  else begin
    col_index <= col_index + 2'h1;
    row_index <= row_index + 2'h1;
    switch_buf <= switch_buf;
    pause_input <= 1'h0;
    pause_output <= 1'h0;
  end
end
always_comb begin
  if (pause_output == 1'h0) begin
    if (switch_buf == 1'h0) begin
      col_pixels[0] = tb[3][col_index];
      col_pixels[1] = tb[4][col_index];
      col_pixels[2] = tb[5][col_index];
    end
    else begin
      col_pixels[0] = tb[0][col_index];
      col_pixels[1] = tb[1][col_index];
      col_pixels[2] = tb[2][col_index];
    end
  end
  else col_pixels = col_pixels;
end
always_comb begin
  if ((rst_n == 1'h0) | (pause_output == 1'h1)) begin
    output_valid = 1'h0;
  end
  else output_valid = 1'h1;
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
assign max_dim = 1'h0;
endmodule   // transpose_buffer

