module reg_cr (
  input logic clk,
  input logic clk_en,
  input logic [16:0] data_in,
  input logic data_in_valid,
  input logic data_out_ready,
  input logic [15:0] default_value,
  input logic rst_n,
  input logic [15:0] stop_lvl,
  input logic tile_en,
  output logic data_in_ready,
  output logic [16:0] data_out,
  output logic data_out_valid
);

typedef enum logic[2:0] {
  ACCUM = 3'h0,
  DONE = 3'h1,
  OUTPUT = 3'h2,
  START = 3'h3,
  STOP_PASS = 3'h4
} accum_seq_state;
logic [15:0] accum_reg;
accum_seq_state accum_seq_current_state;
accum_seq_state accum_seq_next_state;
logic clr_once_popped;
logic [15:0] data_to_fifo;
logic gclk;
logic [16:0] infifo_in_packed;
logic [15:0] infifo_out_data;
logic infifo_out_eos;
logic [16:0] infifo_out_packed;
logic infifo_out_valid;
logic infifo_pop;
logic infifo_push;
logic input_fifo_empty;
logic input_fifo_full;
logic outfifo_full;
logic outfifo_in_eos;
logic [16:0] outfifo_in_packed;
logic [16:0] outfifo_out_packed;
logic outfifo_pop;
logic outfifo_push;
logic output_fifo_empty;
logic reg_accum;
logic reg_clr;
logic set_once_popped;
logic set_once_popped_sticky;
logic set_once_popped_was_high;
assign gclk = clk & tile_en;
assign data_in_ready = ~input_fifo_full;
assign infifo_in_packed[16:0] = data_in;
assign infifo_out_eos = infifo_out_packed[16];
assign infifo_out_data = infifo_out_packed[15:0];
assign infifo_push = data_in_valid;
assign infifo_out_valid = ~input_fifo_empty;
assign outfifo_in_packed[16] = outfifo_in_eos;
assign outfifo_in_packed[15:0] = data_to_fifo;
assign data_out = outfifo_out_packed[16:0];

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    set_once_popped_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_once_popped) begin
      set_once_popped_was_high <= 1'h0;
    end
    else if (set_once_popped) begin
      set_once_popped_was_high <= 1'h1;
    end
  end
end
assign set_once_popped_sticky = set_once_popped_was_high;
assign data_out_valid = ~output_fifo_empty;
assign outfifo_pop = data_out_ready;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    accum_reg <= 16'h0;
  end
  else if (clk_en) begin
    if (reg_clr) begin
      accum_reg <= default_value;
    end
    else if (reg_accum) begin
      accum_reg <= accum_reg + infifo_out_data;
    end
  end
end

always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
    accum_seq_current_state <= START;
  end
  else if (clk_en) begin
    accum_seq_current_state <= accum_seq_next_state;
  end
end
always_comb begin
  accum_seq_next_state = accum_seq_current_state;
  unique case (accum_seq_current_state)
    ACCUM: begin
        if (infifo_out_valid & infifo_out_eos) begin
          accum_seq_next_state = OUTPUT;
        end
        else accum_seq_next_state = ACCUM;
      end
    DONE: begin
        if (~outfifo_full) begin
          accum_seq_next_state = START;
        end
        else accum_seq_next_state = DONE;
      end
    OUTPUT: begin
        if (~outfifo_full) begin
          accum_seq_next_state = STOP_PASS;
        end
        else accum_seq_next_state = OUTPUT;
      end
    START: begin
        if (infifo_out_valid & (~infifo_out_eos)) begin
          accum_seq_next_state = ACCUM;
        end
        else if (infifo_out_valid & infifo_out_eos & (infifo_out_data[9:8] == 2'h1)) begin
          accum_seq_next_state = DONE;
        end
        else if (infifo_out_valid & infifo_out_eos & (infifo_out_data[9:8] == 2'h0)) begin
          accum_seq_next_state = OUTPUT;
        end
        else accum_seq_next_state = START;
      end
    STOP_PASS: begin
        if (~outfifo_full) begin
          accum_seq_next_state = START;
        end
        else accum_seq_next_state = STOP_PASS;
      end
    default: accum_seq_next_state = accum_seq_current_state;
  endcase
end
always_comb begin
  unique case (accum_seq_current_state)
    ACCUM: begin :accum_seq_ACCUM_Output
        infifo_pop = infifo_out_valid & (~infifo_out_eos);
        outfifo_push = 1'h0;
        reg_clr = 1'h0;
        reg_accum = infifo_out_valid & (~infifo_out_eos);
        data_to_fifo = 16'h0;
        outfifo_in_eos = 1'h0;
        set_once_popped = 1'h0;
        clr_once_popped = 1'h0;
      end :accum_seq_ACCUM_Output
    DONE: begin :accum_seq_DONE_Output
        infifo_pop = ~outfifo_full;
        outfifo_push = ~outfifo_full;
        reg_clr = 1'h1;
        reg_accum = 1'h0;
        data_to_fifo = infifo_out_data;
        outfifo_in_eos = infifo_out_eos;
        set_once_popped = 1'h0;
        clr_once_popped = 1'h1;
      end :accum_seq_DONE_Output
    OUTPUT: begin :accum_seq_OUTPUT_Output
        infifo_pop = 1'h0;
        outfifo_push = ~outfifo_full;
        reg_clr = 1'h0;
        reg_accum = 1'h0;
        data_to_fifo = accum_reg;
        outfifo_in_eos = 1'h0;
        set_once_popped = 1'h0;
        clr_once_popped = 1'h0;
      end :accum_seq_OUTPUT_Output
    START: begin :accum_seq_START_Output
        infifo_pop = 1'h0;
        outfifo_push = 1'h0;
        reg_clr = 1'h0;
        reg_accum = 1'h0;
        data_to_fifo = 16'h0;
        outfifo_in_eos = 1'h0;
        set_once_popped = 1'h0;
        clr_once_popped = 1'h0;
      end :accum_seq_START_Output
    STOP_PASS: begin :accum_seq_STOP_PASS_Output
        infifo_pop = (~outfifo_full) & infifo_out_valid & infifo_out_eos & (infifo_out_data[9:8] ==
            2'h0);
        outfifo_push = (~outfifo_full) & infifo_out_valid & infifo_out_eos & (infifo_out_data[9:8] ==
            2'h0) & (infifo_out_data[7:0] > 8'h0);
        reg_clr = 1'h1;
        reg_accum = 1'h0;
        data_to_fifo = infifo_out_data - 16'h1;
        outfifo_in_eos = 1'h1;
        set_once_popped = 1'h0;
        clr_once_popped = 1'h1;
      end :accum_seq_STOP_PASS_Output
    default: begin :accum_seq_default_Output
        infifo_pop = 1'h0;
        outfifo_push = 1'h0;
        reg_clr = 1'h0;
        reg_accum = 1'h0;
        data_to_fifo = 16'h0;
        outfifo_in_eos = 1'h0;
        set_once_popped = 1'h0;
        clr_once_popped = 1'h0;
      end :accum_seq_default_Output
  endcase
end
reg_fifo_depth_8_w_17_afd_2 input_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(infifo_in_packed),
  .pop(infifo_pop),
  .push(infifo_push),
  .rst_n(rst_n),
  .data_out(infifo_out_packed),
  .empty(input_fifo_empty),
  .full(input_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 output_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(outfifo_in_packed),
  .pop(outfifo_pop),
  .push(outfifo_push),
  .rst_n(rst_n),
  .data_out(outfifo_out_packed),
  .empty(output_fifo_empty),
  .full(outfifo_full)
);

endmodule   // reg_cr

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

