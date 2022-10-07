module crddrop (
  input logic clk,
  input logic clk_en,
  input logic [16:0] cmrg_coord_in_0,
  input logic cmrg_coord_in_0_valid,
  input logic [16:0] cmrg_coord_in_1,
  input logic cmrg_coord_in_1_valid,
  input logic cmrg_coord_out_0_ready,
  input logic cmrg_coord_out_1_ready,
  input logic cmrg_enable,
  input logic [15:0] cmrg_stop_lvl,
  input logic rst_n,
  input logic tile_en,
  output logic cmrg_coord_in_0_ready,
  output logic cmrg_coord_in_1_ready,
  output logic [16:0] cmrg_coord_out_0,
  output logic cmrg_coord_out_0_valid,
  output logic [16:0] cmrg_coord_out_1,
  output logic cmrg_coord_out_1_valid
);

typedef enum logic {
  PROCESS = 1'h0,
  START = 1'h1
} proc_seq_state;
logic base_data_seen;
logic base_done;
logic base_done_seen;
logic base_eos_seen;
logic base_infifo_empty;
logic base_infifo_full;
logic [15:0] base_infifo_in_data;
logic base_infifo_in_eos;
logic [16:0] base_infifo_in_packed;
logic base_infifo_in_valid;
logic [16:0] base_infifo_out_packed;
logic base_outfifo_empty;
logic base_outfifo_full;
logic [16:0] base_outfifo_in_packed;
logic [16:0] base_outfifo_out_packed;
logic both_done;
logic clr_pushed_data_lower;
logic clr_pushed_proc;
logic clr_pushed_stop_lvl;
logic cmrg_coord_in_0_eos;
logic cmrg_coord_in_1_eos;
logic [1:0] cmrg_fifo_pop;
logic [1:0] cmrg_fifo_push;
logic gclk;
logic proc_data_seen;
logic proc_done;
logic proc_infifo_empty;
logic proc_infifo_full;
logic [15:0] proc_infifo_in_data;
logic proc_infifo_in_eos;
logic [16:0] proc_infifo_in_packed;
logic proc_infifo_in_valid;
logic [16:0] proc_infifo_out_packed;
logic proc_outfifo_empty;
logic proc_outfifo_full;
logic [16:0] proc_outfifo_in_packed;
logic [16:0] proc_outfifo_out_packed;
proc_seq_state proc_seq_current_state;
proc_seq_state proc_seq_next_state;
logic pushed_data_sticky_sticky;
logic pushed_data_sticky_was_high;
logic pushed_proc_sticky;
logic pushed_proc_was_high;
logic pushed_stop_lvl_sticky;
logic pushed_stop_lvl_was_high;
logic pushing_done;
logic set_pushed_data_lower;
assign gclk = clk & tile_en;
assign cmrg_coord_in_0_eos = cmrg_coord_in_0[16];
assign cmrg_coord_in_1_eos = cmrg_coord_in_1[16];
assign base_infifo_in_packed[16] = cmrg_coord_in_0_eos;
assign base_infifo_in_packed[15:0] = cmrg_coord_in_0[15:0];
assign base_infifo_in_eos = base_infifo_out_packed[16];
assign base_infifo_in_data = base_infifo_out_packed[15:0];
assign base_infifo_in_valid = ~base_infifo_empty;
assign cmrg_coord_in_0_ready = ~base_infifo_full;
assign proc_infifo_in_packed[16] = cmrg_coord_in_1_eos;
assign proc_infifo_in_packed[15:0] = cmrg_coord_in_1[15:0];
assign proc_infifo_in_eos = proc_infifo_out_packed[16];
assign proc_infifo_in_data = proc_infifo_out_packed[15:0];
assign proc_infifo_in_valid = ~proc_infifo_empty;
assign cmrg_coord_in_1_ready = ~proc_infifo_full;
assign base_data_seen = base_infifo_in_valid & (~base_infifo_in_eos);
assign proc_data_seen = proc_infifo_in_valid & (~proc_infifo_in_eos);

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pushed_data_sticky_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_pushed_data_lower) begin
      pushed_data_sticky_was_high <= 1'h0;
    end
    else if (set_pushed_data_lower) begin
      pushed_data_sticky_was_high <= 1'h1;
    end
  end
end
assign pushed_data_sticky_sticky = pushed_data_sticky_was_high;
assign base_eos_seen = base_infifo_in_valid & base_infifo_in_eos & (base_infifo_in_data[9:8] == 2'h0);
assign base_done_seen = base_infifo_in_valid & base_infifo_in_eos & (base_infifo_in_data[9:8] == 2'h1);
assign base_done = base_infifo_in_valid & base_infifo_in_eos & (base_infifo_in_data[9:8] == 2'h1);
assign proc_done = proc_infifo_in_valid & proc_infifo_in_eos & (proc_infifo_in_data[9:8] == 2'h1);

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pushed_proc_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_pushed_proc) begin
      pushed_proc_was_high <= 1'h0;
    end
    else if (cmrg_fifo_push[1]) begin
      pushed_proc_was_high <= 1'h1;
    end
  end
end
assign pushed_proc_sticky = pushed_proc_was_high;

always_ff @(posedge clk, negedge rst_n) begin
  if (~rst_n) begin
    pushed_stop_lvl_was_high <= 1'h0;
  end
  else if (clk_en) begin
    if (clr_pushed_stop_lvl) begin
      pushed_stop_lvl_was_high <= 1'h0;
    end
    else if (cmrg_fifo_push[0] & base_infifo_in_valid & base_infifo_in_eos) begin
      pushed_stop_lvl_was_high <= 1'h1;
    end
  end
end
assign pushed_stop_lvl_sticky = pushed_stop_lvl_was_high;
assign both_done = base_infifo_in_valid & base_infifo_in_eos & proc_infifo_in_valid &
    proc_infifo_in_eos & (base_infifo_in_data[9:8] == 2'h1) &
    (proc_infifo_in_data[9:8] == 2'h1);
assign pushing_done = base_infifo_in_valid & base_infifo_in_eos & proc_infifo_in_valid &
    proc_infifo_in_eos & (base_infifo_in_data[9:8] == 2'h1) &
    (proc_infifo_in_data[9:8] == 2'h1) & (~base_outfifo_full) & (~proc_outfifo_full);
assign base_outfifo_in_packed[16] = base_infifo_in_eos;
assign base_outfifo_in_packed[15:0] = base_infifo_in_data;
assign cmrg_coord_out_0[16] = base_outfifo_out_packed[16];
assign cmrg_coord_out_0[15:0] = base_outfifo_out_packed[15:0];
assign cmrg_coord_out_0_valid = ~base_outfifo_empty;
assign proc_outfifo_in_packed[16] = proc_infifo_in_eos;
assign proc_outfifo_in_packed[15:0] = proc_infifo_in_data;
assign cmrg_coord_out_1[16] = proc_outfifo_out_packed[16];
assign cmrg_coord_out_1[15:0] = proc_outfifo_out_packed[15:0];
assign cmrg_coord_out_1_valid = ~proc_outfifo_empty;

always_ff @(posedge clk, negedge rst_n) begin
  if (!rst_n) begin
    proc_seq_current_state <= START;
  end
  else if (clk_en) begin
    proc_seq_current_state <= proc_seq_next_state;
  end
end
always_comb begin
  proc_seq_next_state = proc_seq_current_state;
  unique case (proc_seq_current_state)
    PROCESS: proc_seq_next_state = PROCESS;
    START: begin
        if (tile_en) begin
          proc_seq_next_state = PROCESS;
        end
        else proc_seq_next_state = START;
      end
    default: begin end
  endcase
end
always_comb begin
  unique case (proc_seq_current_state)
    PROCESS: begin :proc_seq_PROCESS_Output
        cmrg_fifo_pop[0] = base_done ? proc_done & (~base_outfifo_full) & (~proc_outfifo_full):
            (base_infifo_in_valid & (~base_infifo_in_eos)) ? ~base_outfifo_full:
            (base_infifo_in_valid & base_infifo_in_eos) ? (proc_done | (proc_infifo_in_valid
            & (~proc_infifo_in_eos))) & (~base_outfifo_full) & (~proc_outfifo_full): 1'h0;
        cmrg_fifo_pop[1] = proc_done ? base_done & (~base_outfifo_full) & (~proc_outfifo_full):
            (base_infifo_in_valid & base_infifo_in_eos & proc_infifo_in_valid &
            (~proc_infifo_in_eos)) ? (~base_outfifo_full) & ((~proc_outfifo_full) |
            (~pushed_data_sticky_sticky)): (proc_infifo_in_valid & proc_infifo_in_eos) ?
            ~proc_outfifo_full: 1'h0;
        cmrg_fifo_push[0] = base_done ? proc_done: (base_infifo_in_valid & (~base_infifo_in_eos)) ?
            ~base_outfifo_full: (base_infifo_in_valid & base_infifo_in_eos) ? (proc_done |
            (proc_infifo_in_valid & (~proc_infifo_in_eos) & pushed_data_sticky_sticky)) &
            (~base_outfifo_full) & (~proc_outfifo_full): 1'h0;
        cmrg_fifo_push[1] = proc_done ? base_done: (base_infifo_in_valid & base_infifo_in_eos &
            proc_infifo_in_valid & (~proc_infifo_in_eos)) ? (~base_outfifo_full) &
            (~proc_outfifo_full) & pushed_data_sticky_sticky: (proc_infifo_in_valid &
            proc_infifo_in_eos) ? ~proc_outfifo_full: 1'h0;
        clr_pushed_proc = 1'h0;
        clr_pushed_stop_lvl = 1'h0;
        set_pushed_data_lower = base_infifo_in_valid & (~base_infifo_in_eos) & (~base_outfifo_full);
        clr_pushed_data_lower = base_done | (base_infifo_in_valid & base_infifo_in_eos & proc_infifo_in_valid &
            (~proc_infifo_in_eos) & (~base_outfifo_full) & (~proc_outfifo_full));
      end :proc_seq_PROCESS_Output
    START: begin :proc_seq_START_Output
        cmrg_fifo_pop[0] = 1'h0;
        cmrg_fifo_pop[1] = 1'h0;
        cmrg_fifo_push[0] = 1'h0;
        cmrg_fifo_push[1] = 1'h0;
        clr_pushed_proc = 1'h1;
        clr_pushed_stop_lvl = 1'h1;
        set_pushed_data_lower = 1'h0;
        clr_pushed_data_lower = 1'h1;
      end :proc_seq_START_Output
    default: begin end
  endcase
end
reg_fifo_depth_8_w_17_afd_2 base_infifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(base_infifo_in_packed),
  .pop(cmrg_fifo_pop[0]),
  .push(cmrg_coord_in_0_valid),
  .rst_n(rst_n),
  .data_out(base_infifo_out_packed),
  .empty(base_infifo_empty),
  .full(base_infifo_full)
);

reg_fifo_depth_8_w_17_afd_2 proc_infifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(proc_infifo_in_packed),
  .pop(cmrg_fifo_pop[1]),
  .push(cmrg_coord_in_1_valid),
  .rst_n(rst_n),
  .data_out(proc_infifo_out_packed),
  .empty(proc_infifo_empty),
  .full(proc_infifo_full)
);

reg_fifo_depth_8_w_17_afd_2 base_outfifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(base_outfifo_in_packed),
  .pop(cmrg_coord_out_0_ready),
  .push(cmrg_fifo_push[0]),
  .rst_n(rst_n),
  .data_out(base_outfifo_out_packed),
  .empty(base_outfifo_empty),
  .full(base_outfifo_full)
);

reg_fifo_depth_8_w_17_afd_2 proc_outfifo (
  .clk(gclk),
  .clk_en(clk_en),
  .data_in(proc_outfifo_in_packed),
  .pop(cmrg_coord_out_1_ready),
  .push(cmrg_fifo_push[1]),
  .rst_n(rst_n),
  .data_out(proc_outfifo_out_packed),
  .empty(proc_outfifo_empty),
  .full(proc_outfifo_full)
);

endmodule   // crddrop

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

