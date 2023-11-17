module behavioral_pe_add (
  input logic [16:0] a_in,
  input logic [16:0] b_in,
  output logic [16:0] out
);

// Behavioral PE add: won't add EOS tokens
assign out = a_in[16] ? a_in : (a_in + b_in); 

endmodule