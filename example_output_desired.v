module LakeTop (
  input logic [1:0] i_data_in,
  output logic [1:0] o_data_out
);

//==========================
//Declarations
logic   u_passthru_0_in;
logic   u_passthru_0_out;
logic   u_passthru_1_in;
logic   u_passthru_1_out;

//=====================================
//Main Body
assign o_data_out[0] = u_passthru_0_out;
assign o_data_out[1] = u_passthru_1_out;
assign u_passthru_0_in = i_data_in[0];
assign u_passthru_1_in = i_data_in[1];

PassThrough u_passthru_0 (
  .in(u_passthru_0_in),
  .out(u_passthru_0_out)
);

PassThrough u_passthru_1 (
  .in(u_passthru_1_in),
  .out(u_passthru_1_out)
);

endmodule   // LakeTop

