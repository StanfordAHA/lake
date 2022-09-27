module intersect_unit (
  input logic clk,
  input logic clk_en,
  input logic [16:0] coord_in_0,
  input logic coord_in_0_valid,
  input logic [16:0] coord_in_1,
  input logic coord_in_1_valid,
  input logic coord_out_ready,
  input logic joiner_op,
  input logic [16:0] pos_in_0,
  input logic pos_in_0_valid,
  input logic [16:0] pos_in_1,
  input logic pos_in_1_valid,
  input logic pos_out_0_ready,
  input logic pos_out_1_ready,
  input logic rst_n,
  input logic tile_en,
  output logic coord_in_0_ready,
  output logic coord_in_1_ready,
  output logic [16:0] coord_out,
  output logic coord_out_valid,
  output logic pos_in_0_ready,
  output logic pos_in_1_ready,
  output logic [16:0] pos_out_0,
  output logic pos_out_0_valid,
  output logic [16:0] pos_out_1,
  output logic pos_out_1_valid
);

typedef enum logic[2:0] {
  ALIGN = 3'h0,
  DONE = 3'h1,
  DRAIN = 3'h2,
  IDLE = 3'h3,
  ITER = 3'h4,
  UNION = 3'h5
} intersect_seq_state;
logic all_valid;
logic all_valid_join;
logic any_eos;
logic [1:0] clr_eos_sticky;
logic [16:0] coord_fifo_in_packed;
logic [16:0] coord_fifo_out_packed;
logic coord_in_0_fifo_eos_in;
logic [16:0] coord_in_0_fifo_in;
logic coord_in_0_fifo_valid_in;
logic coord_in_1_fifo_eos_in;
logic [16:0] coord_in_1_fifo_in;
logic coord_in_1_fifo_valid_in;
logic coord_in_fifo_0_empty;
logic coord_in_fifo_0_full;
logic coord_in_fifo_1_empty;
logic coord_in_fifo_1_full;
logic [15:0] coord_to_fifo;
logic coord_to_fifo_eos;
logic coordinate_fifo_empty;
logic coordinate_fifo_full;
logic [1:0] eos_in_sticky;
logic eos_sticky_0_sticky;
logic eos_sticky_0_was_high;
logic eos_sticky_1_sticky;
logic eos_sticky_1_was_high;
logic [2:0] fifo_full;
logic fifo_push;
logic gclk;
logic [1:0] inc_pos_cnt;
intersect_seq_state intersect_seq_current_state;
intersect_seq_state intersect_seq_next_state;
logic [15:0] maybe;
logic pos0_fifo_empty;
logic pos0_fifo_full;
logic [16:0] pos0_fifo_in_packed;
logic [16:0] pos0_fifo_out_packed;
logic pos1_fifo_empty;
logic pos1_fifo_full;
logic [16:0] pos1_fifo_in_packed;
logic [16:0] pos1_fifo_out_packed;
logic [1:0][15:0] pos_cnt;
logic pos_in_0_fifo_eos_in;
logic [16:0] pos_in_0_fifo_in;
logic pos_in_0_fifo_valid_in;
logic pos_in_1_fifo_eos_in;
logic [16:0] pos_in_1_fifo_in;
logic pos_in_1_fifo_valid_in;
logic pos_in_fifo_0_empty;
logic pos_in_fifo_0_full;
logic pos_in_fifo_1_empty;
logic pos_in_fifo_1_full;
logic [1:0][15:0] pos_to_fifo;
logic [1:0] pos_to_fifo_eos;
logic [1:0] rst_pos_cnt;
assign gclk = clk & tile_en;
assign coord_in_0_fifo_eos_in = coord_in_0_fifo_in[16];
assign coord_in_0_ready = ~coord_in_fifo_0_full;
assign coord_in_0_fifo_valid_in = ~coord_in_fifo_0_empty;
assign pos_in_0_fifo_eos_in = pos_in_0_fifo_in[16];
assign pos_in_0_ready = ~pos_in_fifo_0_full;
assign pos_in_0_fifo_valid_in = ~pos_in_fifo_0_empty;
assign coord_in_1_fifo_eos_in = coord_in_1_fifo_in[16];
assign coord_in_1_ready = ~coord_in_fifo_1_full;
assign coord_in_1_fifo_valid_in = ~coord_in_fifo_1_empty;
assign pos_in_1_fifo_eos_in = pos_in_1_fifo_in[16];
assign pos_in_1_ready = ~pos_in_fifo_1_full;
assign pos_in_1_fifo_valid_in = ~pos_in_fifo_1_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    eos_sticky_0_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_eos_sticky[0]) begin
      eos_sticky_0_was_high <= 1'h0;
    end
    else if (coord_in_0_fifo_eos_in & coord_in_0_fifo_valid_in & pos_in_0_fifo_eos_in & pos_in_0_fifo_valid_in) begin
      eos_sticky_0_was_high <= 1'h1;
    end
  end
end
assign eos_sticky_0_sticky = (coord_in_0_fifo_eos_in & coord_in_0_fifo_valid_in & pos_in_0_fifo_eos_in &
    pos_in_0_fifo_valid_in) | eos_sticky_0_was_high;
assign eos_in_sticky[0] = eos_sticky_0_sticky;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    eos_sticky_1_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_eos_sticky[1]) begin
      eos_sticky_1_was_high <= 1'h0;
    end
    else if (coord_in_1_fifo_eos_in & coord_in_1_fifo_valid_in & pos_in_1_fifo_eos_in & pos_in_1_fifo_valid_in) begin
      eos_sticky_1_was_high <= 1'h1;
    end
  end
end
assign eos_sticky_1_sticky = (coord_in_1_fifo_eos_in & coord_in_1_fifo_valid_in & pos_in_1_fifo_eos_in &
    pos_in_1_fifo_valid_in) | eos_sticky_1_was_high;
assign eos_in_sticky[1] = eos_sticky_1_sticky;
assign all_valid = (&{coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in,
    pos_in_1_fifo_valid_in}) & (~any_eos);
assign all_valid_join = &{coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in,
    pos_in_1_fifo_valid_in};
assign any_eos = |({coord_in_0_fifo_eos_in, coord_in_1_fifo_eos_in, pos_in_0_fifo_eos_in,
    pos_in_1_fifo_eos_in} & {coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in,
    pos_in_0_fifo_valid_in, pos_in_1_fifo_valid_in});
assign maybe = {6'h0, 2'h2, 8'h0};

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pos_cnt[0] <= 16'h0;
  end
  else if (clk_en) begin
    if (rst_pos_cnt[0]) begin
      pos_cnt[0] <= 16'h0;
    end
    else if (inc_pos_cnt[0]) begin
      pos_cnt[0] <= pos_cnt[0] + 16'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pos_cnt[1] <= 16'h0;
  end
  else if (clk_en) begin
    if (rst_pos_cnt[1]) begin
      pos_cnt[1] <= 16'h0;
    end
    else if (inc_pos_cnt[1]) begin
      pos_cnt[1] <= pos_cnt[1] + 16'h1;
    end
  end
end
assign coord_fifo_in_packed[16] = coord_to_fifo_eos;
assign coord_fifo_in_packed[15:0] = coord_to_fifo;
assign coord_out[16] = coord_fifo_out_packed[16];
assign coord_out[15:0] = coord_fifo_out_packed[15:0];
assign pos0_fifo_in_packed[16] = pos_to_fifo_eos[0];
assign pos0_fifo_in_packed[15:0] = pos_to_fifo[0];
assign pos_out_0[16] = pos0_fifo_out_packed[16];
assign pos_out_0[15:0] = pos0_fifo_out_packed[15:0];
assign pos1_fifo_in_packed[16] = pos_to_fifo_eos[1];
assign pos1_fifo_in_packed[15:0] = pos_to_fifo[1];
assign pos_out_1[16] = pos1_fifo_out_packed[16];
assign pos_out_1[15:0] = pos1_fifo_out_packed[15:0];
assign fifo_full[0] = coordinate_fifo_full;
assign fifo_full[1] = pos0_fifo_full;
assign fifo_full[2] = pos1_fifo_full;
assign coord_out_valid = ~coordinate_fifo_empty;
assign pos_out_0_valid = ~pos0_fifo_empty;
assign pos_out_1_valid = ~pos1_fifo_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
    intersect_seq_current_state <= IDLE;
  end
  else if (clk_en) begin
    intersect_seq_current_state <= intersect_seq_next_state;
  end
end
always_comb begin
  intersect_seq_next_state = intersect_seq_current_state;
  unique case (intersect_seq_current_state)
    ALIGN: begin
        if (&eos_in_sticky) begin
          intersect_seq_next_state = DRAIN;
        end
        else intersect_seq_next_state = ALIGN;
      end
    DONE: intersect_seq_next_state = IDLE;
    DRAIN: begin
        if ((~(&({coord_in_0_fifo_eos_in, coord_in_1_fifo_eos_in, pos_in_0_fifo_eos_in, pos_in_1_fifo_eos_in} & {coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in, pos_in_1_fifo_valid_in}))) & (&{coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in, pos_in_1_fifo_valid_in})) begin
          intersect_seq_next_state = DONE;
        end
        else intersect_seq_next_state = DRAIN;
      end
    IDLE: begin
        if (all_valid_join & (joiner_op == 1'h1) & tile_en) begin
          intersect_seq_next_state = UNION;
        end
        else if (any_eos & (joiner_op == 1'h0) & tile_en) begin
          intersect_seq_next_state = ALIGN;
        end
        else if (all_valid & (joiner_op == 1'h0) & tile_en) begin
          intersect_seq_next_state = ITER;
        end
        else intersect_seq_next_state = IDLE;
      end
    ITER: begin
        if (any_eos) begin
          intersect_seq_next_state = ALIGN;
        end
        else intersect_seq_next_state = ITER;
      end
    UNION: begin
        if (&eos_in_sticky) begin
          intersect_seq_next_state = DRAIN;
        end
        else intersect_seq_next_state = UNION;
      end
    default: intersect_seq_next_state = intersect_seq_current_state;
  endcase
end
always_comb begin
  unique case (intersect_seq_current_state)
    ALIGN: begin :intersect_seq_ALIGN_Output
        inc_pos_cnt[0] = (~eos_in_sticky[0]) & coord_in_0_fifo_valid_in & pos_in_0_fifo_valid_in;
        inc_pos_cnt[1] = (~eos_in_sticky[1]) & coord_in_1_fifo_valid_in & pos_in_1_fifo_valid_in;
        rst_pos_cnt[0] = 1'h0;
        rst_pos_cnt[1] = 1'h0;
        fifo_push = 1'h0;
        clr_eos_sticky[0] = 1'h0;
        clr_eos_sticky[1] = 1'h0;
        coord_to_fifo = 16'h0;
        pos_to_fifo[0] = 16'h0;
        pos_to_fifo[1] = 16'h0;
        coord_to_fifo_eos = 1'h0;
        pos_to_fifo_eos[0] = 1'h0;
        pos_to_fifo_eos[1] = 1'h0;
      end :intersect_seq_ALIGN_Output
    DONE: begin :intersect_seq_DONE_Output
        inc_pos_cnt[0] = 1'h0;
        inc_pos_cnt[1] = 1'h0;
        rst_pos_cnt[0] = 1'h1;
        rst_pos_cnt[1] = 1'h1;
        fifo_push = 1'h0;
        clr_eos_sticky[0] = 1'h1;
        clr_eos_sticky[1] = 1'h1;
        coord_to_fifo = 16'h0;
        pos_to_fifo[0] = 16'h0;
        pos_to_fifo[1] = 16'h0;
        coord_to_fifo_eos = 1'h0;
        pos_to_fifo_eos[0] = 1'h0;
        pos_to_fifo_eos[1] = 1'h0;
      end :intersect_seq_DONE_Output
    DRAIN: begin :intersect_seq_DRAIN_Output
        inc_pos_cnt[0] = (~(|fifo_full)) & (&({coord_in_0_fifo_eos_in, coord_in_1_fifo_eos_in,
            pos_in_0_fifo_eos_in, pos_in_1_fifo_eos_in} & {coord_in_0_fifo_valid_in,
            coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in, pos_in_1_fifo_valid_in})) &
            (&{coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in,
            pos_in_1_fifo_valid_in});
        inc_pos_cnt[1] = (~(|fifo_full)) & (&({coord_in_0_fifo_eos_in, coord_in_1_fifo_eos_in,
            pos_in_0_fifo_eos_in, pos_in_1_fifo_eos_in} & {coord_in_0_fifo_valid_in,
            coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in, pos_in_1_fifo_valid_in})) &
            (&{coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in,
            pos_in_1_fifo_valid_in});
        rst_pos_cnt[0] = 1'h0;
        rst_pos_cnt[1] = 1'h0;
        fifo_push = (~(|fifo_full)) & (&({coord_in_0_fifo_eos_in, coord_in_1_fifo_eos_in,
            pos_in_0_fifo_eos_in, pos_in_1_fifo_eos_in} & {coord_in_0_fifo_valid_in,
            coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in, pos_in_1_fifo_valid_in})) &
            (&{coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in,
            pos_in_1_fifo_valid_in});
        clr_eos_sticky[0] = 1'h0;
        clr_eos_sticky[1] = 1'h0;
        coord_to_fifo = coord_in_0_fifo_in[15:0];
        pos_to_fifo[0] = coord_in_0_fifo_in[15:0];
        pos_to_fifo[1] = coord_in_0_fifo_in[15:0];
        coord_to_fifo_eos = any_eos;
        pos_to_fifo_eos[0] = any_eos;
        pos_to_fifo_eos[1] = any_eos;
      end :intersect_seq_DRAIN_Output
    IDLE: begin :intersect_seq_IDLE_Output
        inc_pos_cnt[0] = 1'h0;
        inc_pos_cnt[1] = 1'h0;
        rst_pos_cnt[0] = 1'h0;
        rst_pos_cnt[1] = 1'h0;
        fifo_push = 1'h0;
        clr_eos_sticky[0] = 1'h0;
        clr_eos_sticky[1] = 1'h0;
        coord_to_fifo = 16'h0;
        pos_to_fifo[0] = 16'h0;
        pos_to_fifo[1] = 16'h0;
        coord_to_fifo_eos = 1'h0;
        pos_to_fifo_eos[0] = 1'h0;
        pos_to_fifo_eos[1] = 1'h0;
      end :intersect_seq_IDLE_Output
    ITER: begin :intersect_seq_ITER_Output
        inc_pos_cnt[0] = all_valid & (coord_in_0_fifo_in <= coord_in_1_fifo_in) & (~(|fifo_full));
        inc_pos_cnt[1] = all_valid & (coord_in_0_fifo_in >= coord_in_1_fifo_in) & (~(|fifo_full));
        rst_pos_cnt[0] = any_eos & (~(|fifo_full));
        rst_pos_cnt[1] = any_eos & (~(|fifo_full));
        fifo_push = all_valid & (coord_in_0_fifo_in == coord_in_1_fifo_in) & (~(|fifo_full)) &
            (~any_eos);
        clr_eos_sticky[0] = 1'h0;
        clr_eos_sticky[1] = 1'h0;
        coord_to_fifo = coord_in_0_fifo_in[15:0];
        pos_to_fifo[0] = pos_in_0_fifo_in[15:0];
        pos_to_fifo[1] = pos_in_1_fifo_in[15:0];
        coord_to_fifo_eos = 1'h0;
        pos_to_fifo_eos[0] = 1'h0;
        pos_to_fifo_eos[1] = 1'h0;
      end :intersect_seq_ITER_Output
    UNION: begin :intersect_seq_UNION_Output
        inc_pos_cnt[0] = all_valid_join & ((coord_in_0_fifo_in <= coord_in_1_fifo_in) |
            coord_in_1_fifo_eos_in) & (~(|fifo_full)) & (~coord_in_0_fifo_eos_in);
        inc_pos_cnt[1] = all_valid_join & ((coord_in_0_fifo_in >= coord_in_1_fifo_in) |
            coord_in_0_fifo_eos_in) & (~(|fifo_full)) & (~coord_in_1_fifo_eos_in);
        rst_pos_cnt[0] = any_eos & (~(|fifo_full));
        rst_pos_cnt[1] = any_eos & (~(|fifo_full));
        fifo_push = all_valid_join & (~(|fifo_full)) & (~(&({coord_in_0_fifo_eos_in,
            coord_in_1_fifo_eos_in, pos_in_0_fifo_eos_in, pos_in_1_fifo_eos_in} &
            {coord_in_0_fifo_valid_in, coord_in_1_fifo_valid_in, pos_in_0_fifo_valid_in,
            pos_in_1_fifo_valid_in})));
        clr_eos_sticky[0] = 1'h0;
        clr_eos_sticky[1] = 1'h0;
        coord_to_fifo = coord_in_0_fifo_eos_in ? coord_in_1_fifo_in[15:0]: coord_in_1_fifo_eos_in ?
            coord_in_0_fifo_in[15:0]: (coord_in_0_fifo_in <= coord_in_1_fifo_in) ?
            coord_in_0_fifo_in[15:0]: coord_in_1_fifo_in[15:0];
        pos_to_fifo[0] = coord_in_0_fifo_eos_in ? maybe: coord_in_1_fifo_eos_in ? pos_in_0_fifo_in[15:0]:
            (coord_in_0_fifo_in <= coord_in_1_fifo_in) ? pos_in_0_fifo_in[15:0]: maybe;
        pos_to_fifo[1] = coord_in_1_fifo_eos_in ? maybe: coord_in_0_fifo_eos_in ? pos_in_1_fifo_in[15:0]:
            (coord_in_1_fifo_in <= coord_in_0_fifo_in) ? pos_in_1_fifo_in[15:0]: maybe;
        coord_to_fifo_eos = 1'h0;
        pos_to_fifo_eos[0] = (pos_in_0_fifo_eos_in & (~coord_in_0_fifo_eos_in)) | (coord_in_0_fifo_eos_in ?
            1'h1: coord_in_1_fifo_eos_in ? 1'h0: (coord_in_0_fifo_in <= coord_in_1_fifo_in)
            ? 1'h0: 1'h1);
        pos_to_fifo_eos[1] = (pos_in_1_fifo_eos_in & (~coord_in_1_fifo_eos_in)) | (coord_in_1_fifo_eos_in ?
            1'h1: coord_in_0_fifo_eos_in ? 1'h0: (coord_in_1_fifo_in <= coord_in_0_fifo_in)
            ? 1'h0: 1'h1);
      end :intersect_seq_UNION_Output
    default: begin :intersect_seq_default_Output
        inc_pos_cnt[0] = 1'h0;
        inc_pos_cnt[1] = 1'h0;
        rst_pos_cnt[0] = 1'h0;
        rst_pos_cnt[1] = 1'h0;
        fifo_push = 1'h0;
        clr_eos_sticky[0] = 1'h0;
        clr_eos_sticky[1] = 1'h0;
        coord_to_fifo = 16'h0;
        pos_to_fifo[0] = 16'h0;
        pos_to_fifo[1] = 16'h0;
        coord_to_fifo_eos = 1'h0;
        pos_to_fifo_eos[0] = 1'h0;
        pos_to_fifo_eos[1] = 1'h0;
      end :intersect_seq_default_Output
  endcase
end
reg_fifo_depth_8_w_17_afd_2 coord_in_fifo_0 (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(coord_in_0),
  .pop(inc_pos_cnt[0]),
  .push(coord_in_0_valid),
  .rst_n(rst_n),
  .data_out(coord_in_0_fifo_in),
  .empty(coord_in_fifo_0_empty),
  .full(coord_in_fifo_0_full)
);

reg_fifo_depth_8_w_17_afd_2 pos_in_fifo_0 (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(pos_in_0),
  .pop(inc_pos_cnt[0]),
  .push(pos_in_0_valid),
  .rst_n(rst_n),
  .data_out(pos_in_0_fifo_in),
  .empty(pos_in_fifo_0_empty),
  .full(pos_in_fifo_0_full)
);

reg_fifo_depth_8_w_17_afd_2 coord_in_fifo_1 (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(coord_in_1),
  .pop(inc_pos_cnt[1]),
  .push(coord_in_1_valid),
  .rst_n(rst_n),
  .data_out(coord_in_1_fifo_in),
  .empty(coord_in_fifo_1_empty),
  .full(coord_in_fifo_1_full)
);

reg_fifo_depth_8_w_17_afd_2 pos_in_fifo_1 (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(pos_in_1),
  .pop(inc_pos_cnt[1]),
  .push(pos_in_1_valid),
  .rst_n(rst_n),
  .data_out(pos_in_1_fifo_in),
  .empty(pos_in_fifo_1_empty),
  .full(pos_in_fifo_1_full)
);

reg_fifo_depth_8_w_17_afd_2 coordinate_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(coord_fifo_in_packed),
  .pop(coord_out_ready),
  .push(fifo_push),
  .rst_n(rst_n),
  .data_out(coord_fifo_out_packed),
  .empty(coordinate_fifo_empty),
  .full(coordinate_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 pos0_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(pos0_fifo_in_packed),
  .pop(pos_out_0_ready),
  .push(fifo_push),
  .rst_n(rst_n),
  .data_out(pos0_fifo_out_packed),
  .empty(pos0_fifo_empty),
  .full(pos0_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 pos1_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(pos1_fifo_in_packed),
  .pop(pos_out_1_ready),
  .push(fifo_push),
  .rst_n(rst_n),
  .data_out(pos1_fifo_out_packed),
  .empty(pos1_fifo_empty),
  .full(pos1_fifo_full)
);

endmodule   // intersect_unit

module reg_fifo_depth_8_w_17_afd_2 (
  input logic clk,
  input logic clk_en,
  input logic [0:0] [16:0] data_in,
  input logic pop,
  input logic push,
  input logic rst_n,
  output logic almost_full,
  output logic [0:0] [16:0] data_out,
  output logic empty,
  output logic full,
  output logic valid
);

logic [3:0] num_items;
logic passthru;
logic [2:0] rd_ptr;
logic read;
logic [7:0][0:0][16:0] reg_array;
logic [2:0] wr_ptr;
logic write;
assign full = num_items == 4'h8;
assign almost_full = num_items >= 4'h6;
assign empty = num_items == 4'h0;
assign read = pop & (~passthru) & (~empty);
assign passthru = 1'h0;
assign write = push & (~passthru) & (~full);

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    num_items <= 4'h0;
  end
  else if (clk_en) begin
    if (write & (~read)) begin
      num_items <= num_items + 4'h1;
    end
    else if ((~write) & read) begin
      num_items <= num_items - 4'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    reg_array <= 136'h0;
  end
  else if (clk_en) begin
    if (write) begin
      reg_array[wr_ptr] <= data_in;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    wr_ptr <= 3'h0;
  end
  else if (clk_en) begin
    if (write) begin
      if (wr_ptr == 3'h7) begin
        wr_ptr <= 3'h0;
      end
      else wr_ptr <= wr_ptr + 3'h1;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    rd_ptr <= 3'h0;
  end
  else if (clk_en) begin
    if (read) begin
      rd_ptr <= rd_ptr + 3'h1;
    end
  end
end
always_comb begin
  if (passthru) begin
    data_out = data_in;
  end
  else data_out = reg_array[rd_ptr];
end
always_comb begin
  valid = (~empty) | passthru;
end
endmodule   // reg_fifo_depth_8_w_17_afd_2

