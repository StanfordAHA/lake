module Repeat (
  input logic clk,
  input logic clk_en,
  input logic [16:0] proc_data_in,
  input logic proc_data_in_valid,
  input logic ref_data_out_ready,
  input logic [16:0] repsig_data_in,
  input logic repsig_data_in_valid,
  input logic root,
  input logic rst_n,
  input logic spacc_mode,
  input logic [15:0] stop_lvl,
  input logic tile_en,
  output logic proc_data_in_ready,
  output logic [16:0] ref_data_out,
  output logic ref_data_out_valid,
  output logic repsig_data_in_ready
);

typedef enum logic[2:0] {
  DONE = 3'h0,
  INJECT0 = 3'h1,
  INJECT1 = 3'h2,
  PASS_REPEAT = 3'h3,
  PASS_STOP = 3'h4,
  START = 3'h5
} repeat_fsm_state;
logic clr_last_pushed_data;
logic gclk;
logic proc_done;
logic proc_fifo_full;
logic [15:0] proc_fifo_inject_data;
logic proc_fifo_inject_eos;
logic proc_fifo_inject_push;
logic [15:0] proc_fifo_out_data;
logic proc_fifo_out_eos;
logic proc_fifo_pop;
logic proc_fifo_push;
logic proc_fifo_valid;
logic [0:0][16:0] proc_in_fifo_data_in;
logic [0:0][16:0] proc_in_fifo_data_out;
logic proc_in_fifo_empty;
logic proc_in_fifo_full;
logic proc_stop;
logic ref_fifo_full;
logic [15:0] ref_fifo_in_data;
logic ref_fifo_in_eos;
logic ref_fifo_push;
logic ref_maybe;
logic [0:0][16:0] ref_out_fifo_data_in;
logic ref_out_fifo_empty;
repeat_fsm_state repeat_fsm_current_state;
repeat_fsm_state repeat_fsm_next_state;
logic repsig_done;
logic [15:0] repsig_fifo_out_data;
logic repsig_fifo_out_eos;
logic repsig_fifo_pop;
logic repsig_fifo_valid;
logic [0:0][16:0] repsig_in_fifo_data_out;
logic repsig_in_fifo_empty;
logic repsig_in_fifo_full;
logic repsig_stop;
logic seen_root_eos_sticky;
logic seen_root_eos_was_high;
logic set_last_pushed_data;
logic set_last_pushed_data_sticky;
logic set_last_pushed_data_was_high;
assign gclk = clk & tile_en;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    set_last_pushed_data_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_last_pushed_data) begin
      set_last_pushed_data_was_high <= 1'h0;
    end
    else if (set_last_pushed_data) begin
      set_last_pushed_data_was_high <= 1'h1;
    end
  end
end
assign set_last_pushed_data_sticky = set_last_pushed_data_was_high;
assign {repsig_fifo_out_eos, repsig_fifo_out_data} = repsig_in_fifo_data_out;
assign repsig_data_in_ready = ~repsig_in_fifo_full;
assign repsig_fifo_valid = ~repsig_in_fifo_empty;
assign proc_fifo_push = root ? proc_fifo_inject_push: proc_data_in_valid;
assign proc_in_fifo_data_in = root ? {proc_fifo_inject_eos, proc_fifo_inject_data}: proc_data_in;
assign {proc_fifo_out_eos, proc_fifo_out_data} = proc_in_fifo_data_out;
assign proc_data_in_ready = ~proc_in_fifo_full;
assign proc_fifo_full = proc_in_fifo_full;
assign proc_fifo_valid = ~proc_in_fifo_empty;
assign ref_out_fifo_data_in = {ref_fifo_in_eos, ref_fifo_in_data};
assign ref_data_out_valid = ~ref_out_fifo_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    seen_root_eos_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (1'h0) begin
      seen_root_eos_was_high <= 1'h0;
    end
    else if ((proc_fifo_out_data == 16'h0) & proc_fifo_out_eos & proc_fifo_valid) begin
      seen_root_eos_was_high <= 1'h1;
    end
  end
end
assign seen_root_eos_sticky = ((proc_fifo_out_data == 16'h0) & proc_fifo_out_eos & proc_fifo_valid) |
    seen_root_eos_was_high;
assign proc_stop = (proc_fifo_out_data[9:8] == 2'h0) & proc_fifo_out_eos & proc_fifo_valid;
assign proc_done = (proc_fifo_out_data[9:8] == 2'h1) & proc_fifo_out_eos & proc_fifo_valid;
assign repsig_stop = (repsig_fifo_out_data[9:8] == 2'h0) & repsig_fifo_out_eos & repsig_fifo_valid;
assign repsig_done = (repsig_fifo_out_data[9:8] == 2'h1) & repsig_fifo_out_eos & repsig_fifo_valid;
assign ref_maybe = proc_fifo_valid & proc_fifo_out_eos & (proc_fifo_out_data[9:8] == 2'h2);

always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
    repeat_fsm_current_state <= START;
  end
  else if (clk_en) begin
    repeat_fsm_current_state <= repeat_fsm_next_state;
  end
end
always_comb begin
  repeat_fsm_next_state = repeat_fsm_current_state;
  unique case (repeat_fsm_current_state)
    DONE: begin
        if ((~ref_fifo_full) & proc_done & repsig_done) begin
          repeat_fsm_next_state = START;
        end
      end
    INJECT0: begin
        if (~proc_fifo_full) begin
          repeat_fsm_next_state = INJECT1;
        end
        else repeat_fsm_next_state = INJECT0;
      end
    INJECT1: begin
        if (~proc_fifo_full) begin
          repeat_fsm_next_state = PASS_REPEAT;
        end
        else repeat_fsm_next_state = INJECT1;
      end
    PASS_REPEAT: begin
        if (proc_done) begin
          repeat_fsm_next_state = DONE;
        end
        else if (repsig_fifo_out_eos & repsig_fifo_valid & (repsig_fifo_out_data[9:8] == 2'h0)) begin
          repeat_fsm_next_state = PASS_STOP;
        end
        else repeat_fsm_next_state = PASS_REPEAT;
      end
    PASS_STOP: begin
        if (proc_fifo_valid & (~proc_stop) & (~ref_fifo_full)) begin
          repeat_fsm_next_state = PASS_REPEAT;
        end
        else repeat_fsm_next_state = PASS_STOP;
      end
    START: begin
        if (root & tile_en) begin
          repeat_fsm_next_state = INJECT0;
        end
        else if ((~root) & tile_en) begin
          repeat_fsm_next_state = PASS_REPEAT;
        end
        else repeat_fsm_next_state = START;
      end
    default: repeat_fsm_next_state = repeat_fsm_current_state;
  endcase
end
always_comb begin
  unique case (repeat_fsm_current_state)
    DONE: begin :repeat_fsm_DONE_Output
        ref_fifo_in_data = proc_fifo_out_data;
        ref_fifo_in_eos = 1'h1;
        ref_fifo_push = (~ref_fifo_full) & proc_done & repsig_done;
        proc_fifo_pop = (~ref_fifo_full) & proc_done & repsig_done;
        repsig_fifo_pop = (~ref_fifo_full) & proc_done & repsig_done;
        proc_fifo_inject_push = 1'h0;
        proc_fifo_inject_data = 16'h0;
        proc_fifo_inject_eos = 1'h0;
      end :repeat_fsm_DONE_Output
    INJECT0: begin :repeat_fsm_INJECT0_Output
        ref_fifo_in_data = 16'h0;
        ref_fifo_in_eos = 1'h0;
        ref_fifo_push = 1'h0;
        proc_fifo_pop = 1'h0;
        repsig_fifo_pop = 1'h0;
        proc_fifo_inject_push = 1'h1;
        proc_fifo_inject_data = 16'h0;
        proc_fifo_inject_eos = 1'h0;
      end :repeat_fsm_INJECT0_Output
    INJECT1: begin :repeat_fsm_INJECT1_Output
        ref_fifo_in_data = 16'h0;
        ref_fifo_in_eos = 1'h0;
        ref_fifo_push = 1'h0;
        proc_fifo_pop = 1'h0;
        repsig_fifo_pop = 1'h0;
        proc_fifo_inject_push = 1'h1;
        proc_fifo_inject_data = 16'h100;
        proc_fifo_inject_eos = 1'h1;
      end :repeat_fsm_INJECT1_Output
    PASS_REPEAT: begin :repeat_fsm_PASS_REPEAT_Output
        ref_fifo_in_data = proc_fifo_out_data;
        ref_fifo_in_eos = ref_maybe;
        ref_fifo_push = repsig_fifo_valid & proc_fifo_valid & (~repsig_fifo_out_eos) & (~proc_done) &
            (~ref_fifo_full);
        proc_fifo_pop = ((repsig_fifo_valid & repsig_fifo_out_eos & (~spacc_mode)) | (spacc_mode &
            repsig_done)) & (~proc_done);
        repsig_fifo_pop = (~ref_fifo_full) & repsig_fifo_valid & (~repsig_fifo_out_eos) & proc_fifo_valid
            & (~proc_done);
        proc_fifo_inject_push = 1'h0;
        proc_fifo_inject_data = 16'h0;
        proc_fifo_inject_eos = 1'h0;
      end :repeat_fsm_PASS_REPEAT_Output
    PASS_STOP: begin :repeat_fsm_PASS_STOP_Output
        ref_fifo_in_data = repsig_fifo_out_data;
        ref_fifo_in_eos = 1'h1;
        ref_fifo_push = repsig_stop & proc_fifo_valid & (~ref_fifo_full);
        proc_fifo_pop = repsig_fifo_valid & proc_stop & (~ref_fifo_full);
        repsig_fifo_pop = repsig_stop & proc_fifo_valid & (~ref_fifo_full);
        proc_fifo_inject_push = 1'h0;
        proc_fifo_inject_data = 16'h0;
        proc_fifo_inject_eos = 1'h0;
      end :repeat_fsm_PASS_STOP_Output
    START: begin :repeat_fsm_START_Output
        ref_fifo_in_data = 16'h0;
        ref_fifo_in_eos = 1'h0;
        ref_fifo_push = 1'h0;
        proc_fifo_pop = 1'h0;
        repsig_fifo_pop = 1'h0;
        proc_fifo_inject_push = 1'h0;
        proc_fifo_inject_data = 16'h0;
        proc_fifo_inject_eos = 1'h0;
      end :repeat_fsm_START_Output
    default: begin :repeat_fsm_default_Output
        ref_fifo_in_data = 16'h0;
        ref_fifo_in_eos = 1'h0;
        ref_fifo_push = 1'h0;
        proc_fifo_pop = 1'h0;
        repsig_fifo_pop = 1'h0;
        proc_fifo_inject_push = 1'h0;
        proc_fifo_inject_data = 16'h0;
        proc_fifo_inject_eos = 1'h0;
      end :repeat_fsm_default_Output
  endcase
end
reg_fifo_depth_8_w_17_afd_2 repsig_in_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(repsig_data_in),
  .pop(repsig_fifo_pop),
  .push(repsig_data_in_valid),
  .rst_n(rst_n),
  .data_out(repsig_in_fifo_data_out),
  .empty(repsig_in_fifo_empty),
  .full(repsig_in_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 proc_in_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(proc_in_fifo_data_in),
  .pop(proc_fifo_pop),
  .push(proc_fifo_push),
  .rst_n(rst_n),
  .data_out(proc_in_fifo_data_out),
  .empty(proc_in_fifo_empty),
  .full(proc_in_fifo_full)
);

reg_fifo_depth_8_w_17_afd_2 ref_out_fifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(ref_out_fifo_data_in),
  .pop(ref_data_out_ready),
  .push(ref_fifo_push),
  .rst_n(rst_n),
  .data_out(ref_data_out),
  .empty(ref_out_fifo_empty),
  .full(ref_fifo_full)
);

endmodule   // Repeat

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

