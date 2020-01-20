module transpose_buffer (
  output logic buf_index,
  input logic clk,
  output logic [2:0] col_pixels,
  input logic [2:0] img_height,
  output logic [2:0] index_inner,
  output logic [2:0] index_outer,
  input logic [4:0] [2:0] indices,
  output logic [2:0] indices_index_inner,
  input logic [3:0] input_data,
  output logic [2:0] input_index,
  output logic [1:0] output_index,
  output logic [5:0] output_index_inter,
  output logic [5:0] output_index_inter_tb,
  output logic output_valid,
  input logic [2:0] range_inner,
  input logic [2:0] range_outer,
  output logic [1:0] row_index,
  input logic rst_n,
  input logic [2:0] stride,
  output logic switch_buf,
  output logic [4:0] tb0_end,
  output logic [4:0] tb0_start,
  output logic [4:0] tb1_end,
  output logic [4:0] tb1_start,
  output logic [4:0] tb_distance,
  input logic tb_start_index
);

logic [5:0][3:0] tb;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    index_outer <= 3'h0;
    index_inner <= 3'h0;
  end
  else if (index_inner == (range_inner - 3'h1)) begin
    index_inner <= 3'h0;
    if (index_outer == (range_outer - 3'h1)) begin
      index_outer <= 3'h0;
    end
    else index_outer <= index_outer + 3'h1;
  end
  else index_inner <= index_inner + 3'h1;
end
always_comb begin
  input_index = (3'h3 * 3'(buf_index)) + 3'(row_index);
end

always_ff @(posedge clk) begin
  if ((row_index == 2'h2) & switch_buf) begin
    tb <= tb;
  end
  else begin
    tb[input_index][0] <= input_data[0];
    tb[input_index][1] <= input_data[1];
    tb[input_index][2] <= input_data[2];
    tb[input_index][3] <= input_data[3];
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    row_index <= 2'h0;
  end
  else if (row_index == 2'h2) begin
    if (~switch_buf) begin
      row_index <= row_index;
    end
    else row_index <= 2'h0;
  end
  else row_index <= row_index + 2'h1;
end
always_comb begin
  indices_index_inner = indices[index_inner];
  output_index_inter_tb = (6'(index_outer) * 6'(stride)) + 6'(indices_index_inner);
  output_index_inter = output_index_inter_tb % 6'h4;
  output_index = output_index_inter[1:0];
end

always_ff @(posedge clk) begin
  if (output_valid) begin
    if (~buf_index) begin
      col_pixels[0] <= tb[0][output_index];
    end
    else col_pixels[0] <= tb[3][output_index];
    if (~buf_index) begin
      col_pixels[1] <= tb[1][output_index];
    end
    else col_pixels[1] <= tb[4][output_index];
    if (~buf_index) begin
      col_pixels[2] <= tb[2][output_index];
    end
    else col_pixels[2] <= tb[5][output_index];
  end
  else col_pixels <= col_pixels;
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    output_valid <= 1'h0;
    buf_index <= 1'h0;
  end
  else if ((6'(tb0_start) <= 6'(output_index_inter_tb)) & (6'(output_index_inter_tb) <= 6'(tb0_end))) begin
    output_valid <= 1'h1;
    buf_index <= 1'h0;
  end
  else if ((6'(tb1_start) <= 6'(output_index_inter_tb)) & (6'(output_index_inter_tb) <= 6'(tb1_end))) begin
    output_valid <= 1'h1;
    buf_index <= 1'h1;
  end
  else begin
    output_valid <= 1'h0;
    buf_index <= buf_index;
  end
end
always_comb begin
  if ((6'(tb0_start) <= 6'(output_index_inter_tb)) & (6'(output_index_inter_tb) <= 6'(tb0_end))) begin
    switch_buf = buf_index;
  end
  else if ((6'(tb1_start) <= 6'(output_index_inter_tb)) & (6'(output_index_inter_tb) <= 6'(tb1_end))) begin
    switch_buf = ~buf_index;
  end
  else switch_buf = 1'h0;
  tb_distance = 5'h4;
  tb0_start = 5'(tb_start_index);
  tb0_end = (tb0_start + 5'h4) - 5'h1;
  tb1_start = tb0_start + tb_distance;
  tb1_end = (tb1_start + 5'h4) - 5'h1;
end
endmodule   // transpose_buffer

