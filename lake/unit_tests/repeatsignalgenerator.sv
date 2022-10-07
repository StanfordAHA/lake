module RepeatSignalGenerator (
  input logic [16:0] base_data_in,
  input logic base_data_in_valid,
  input logic clk,
  input logic clk_en,
  input logic passthru_data_out_ready,
  input logic repsig_data_out_ready,
  input logic rst_n,
  input logic [15:0] stop_lvl,
  input logic tile_en,
  output logic base_data_in_ready,
  output logic [16:0] passthru_data_out,
  output logic passthru_data_out_valid,
  output logic [16:0] repsig_data_out,
  output logic repsig_data_out_valid
);

typedef enum logic[1:0] {
  DONE = 2'h0,
  PASS_REPEAT = 2'h1,
  PASS_STOP = 2'h2,
  START = 2'h3
} rsg_fsm_state;
logic already_pushed_repsig_eos_sticky;
logic already_pushed_repsig_eos_was_high;
logic [15:0] base_fifo_out_data;
logic base_fifo_out_eos;
logic base_fifo_pop;
logic base_fifo_valid;
logic [0:0][16:0] base_in_fifo_data_out;
logic base_in_fifo_empty;
logic base_in_fifo_full;
logic clr_already_pushed_repsig_eos;
logic gclk;
logic passthru_fifo_full;
logic [15:0] passthru_fifo_in_data;
logic passthru_fifo_in_eos;
logic passthru_fifo_push;
logic [0:0][16:0] passthru_out_fifo_data_in;
logic passthru_out_fifo_empty;
logic repsig_fifo_full;
logic [15:0] repsig_fifo_in_data;
logic repsig_fifo_in_eos;
logic repsig_fifo_push;
logic [0:0][16:0] repsig_out_fifo_data_in;
logic repsig_out_fifo_empty;
rsg_fsm_state rsg_fsm_current_state;
rsg_fsm_state rsg_fsm_next_state;
logic seen_root_eos_sticky;
logic seen_root_eos_was_high;
assign gclk = clk & tile_en;
assign {base_fifo_out_eos, base_fifo_out_data} = base_in_fifo_data_out;
assign base_data_in_ready = ~base_in_fifo_full;
assign base_fifo_valid = ~base_in_fifo_empty;
assign repsig_out_fifo_data_in = {repsig_fifo_in_eos, repsig_fifo_in_data};
assign repsig_data_out_valid = ~repsig_out_fifo_empty;
assign passthru_out_fifo_data_in = {passthru_fifo_in_eos, passthru_fifo_in_data};
assign passthru_data_out_valid = ~passthru_out_fifo_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    seen_root_eos_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (1'h0) begin
      seen_root_eos_was_high <= 1'h0;
    end
    else if ((base_fifo_out_data[9:8] == 2'h1) & base_fifo_out_eos & base_fifo_valid) begin
      seen_root_eos_was_high <= 1'h1;
    end
  end
end
assign seen_root_eos_sticky = ((base_fifo_out_data[9:8] == 2'h1) & base_fifo_out_eos & base_fifo_valid) |
    seen_root_eos_was_high;
assign passthru_fifo_in_data = base_fifo_out_data;
assign passthru_fifo_in_eos = base_fifo_out_eos;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    already_pushed_repsig_eos_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_already_pushed_repsig_eos) begin
      already_pushed_repsig_eos_was_high <= 1'h0;
    end
    else if (repsig_fifo_push & (~repsig_fifo_full)) begin
      already_pushed_repsig_eos_was_high <= 1'h1;
    end
  end
end
assign already_pushed_repsig_eos_sticky = already_pushed_repsig_eos_was_high;

always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
    rsg_fsm_current_state <= START;
  end
  else if (clk_en) begin
    rsg_fsm_current_state <= rsg_fsm_next_state;
  end
end
always_comb begin
  rsg_fsm_next_state = rsg_fsm_current_state;
  unique case (rsg_fsm_current_state)
    DONE: rsg_fsm_next_state = START;
    PASS_REPEAT: begin
        if (base_fifo_out_eos & base_fifo_valid) begin
          rsg_fsm_next_state = PASS_STOP;
        end
        else rsg_fsm_next_state = PASS_REPEAT;
      end
    PASS_STOP: begin
        if (base_fifo_valid & base_fifo_out_eos & (base_fifo_out_data[9:8] == 2'h1) & (~repsig_fifo_full)) begin
          rsg_fsm_next_state = DONE;
        end
        else if (base_fifo_valid & (~base_fifo_out_eos)) begin
          rsg_fsm_next_state = PASS_REPEAT;
        end
        else rsg_fsm_next_state = PASS_STOP;
      end
    START: begin
        if (tile_en) begin
          rsg_fsm_next_state = PASS_REPEAT;
        end
        else rsg_fsm_next_state = START;
      end
    default: begin end
  endcase
end
always_comb begin
  unique case (rsg_fsm_current_state)
    DONE: begin :rsg_fsm_DONE_Output
        repsig_fifo_in_data = 16'h0;
        repsig_fifo_in_eos = 1'h0;
        repsig_fifo_push = 1'h0;
        base_fifo_pop = 1'h0;
        clr_already_pushed_repsig_eos = 1'h0;
        passthru_fifo_push = 1'h0;
      end :rsg_fsm_DONE_Output
    PASS_REPEAT: begin :rsg_fsm_PASS_REPEAT_Output
        repsig_fifo_in_data = 16'h1;
        repsig_fifo_in_eos = 1'h0;
        repsig_fifo_push = (~base_fifo_out_eos) & base_fifo_valid;
        clr_already_pushed_repsig_eos = 1'h1;
        passthru_fifo_push = (~base_fifo_out_eos) & base_fifo_valid;
        base_fifo_pop = (~base_fifo_out_eos) & base_fifo_valid & (~repsig_fifo_full) &
            (~passthru_fifo_full);
      end :rsg_fsm_PASS_REPEAT_Output
    PASS_STOP: begin :rsg_fsm_PASS_STOP_Output
        repsig_fifo_in_data = (base_fifo_out_data[9:8] == 2'h1) ? base_fifo_out_data: base_fifo_out_data;
        repsig_fifo_in_eos = 1'h1;
        repsig_fifo_push = base_fifo_out_eos & base_fifo_valid;
        clr_already_pushed_repsig_eos = 1'h0;
        passthru_fifo_push = base_fifo_out_eos & base_fifo_valid;
        base_fifo_pop = base_fifo_out_eos & base_fifo_valid & (~repsig_fifo_full) &
            (~passthru_fifo_full);
      end :rsg_fsm_PASS_STOP_Output
    START: begin :rsg_fsm_START_Output
        repsig_fifo_in_data = 16'h0;
        repsig_fifo_in_eos = 1'h0;
        repsig_fifo_push = 1'h0;
        base_fifo_pop = 1'h0;
        clr_already_pushed_repsig_eos = 1'h0;
        passthru_fifo_push = 1'h0;
      end :rsg_fsm_START_Output
    default: begin end
  endcase
end
reg_fifo_depth_8_w_17_afd_2 base_in_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(base_data_in),
  .pop(base_fifo_pop),
  .push(base_data_in_valid),
  .rst_n(rst_n),
  .data_out(base_in_fifo_data_out),
  .empty(base_in_fifo_empty),
  .full(base_in_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 repsig_out_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(repsig_out_fifo_data_in),
  .pop(repsig_data_out_ready),
  .push(repsig_fifo_push),
  .rst_n(rst_n),
  .data_out(repsig_data_out),
  .empty(repsig_out_fifo_empty),
  .full(repsig_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 passthru_out_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(passthru_out_fifo_data_in),
  .pop(passthru_data_out_ready),
  .push(passthru_fifo_push),
  .rst_n(rst_n),
  .data_out(passthru_data_out),
  .empty(passthru_out_fifo_empty),
  .full(passthru_fifo_full)
);

endmodule   // RepeatSignalGenerator

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

