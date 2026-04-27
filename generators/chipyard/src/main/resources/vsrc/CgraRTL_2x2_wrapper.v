//------------------------------------------------------------------------
// CgraRTL_2x2_wrapper.v
//------------------------------------------------------------------------
// Thin wrapper around PyMTL3-generated CgraRTL_2x2 to flatten
// SystemVerilog struct types and unpacked arrays into simple logic ports
// for Chisel BlackBox compatibility.
//
// Bit widths (from PyMTL3 generation):
//   IntraCgraPkt: 182 bits
//   InterCgraPkt: 185 bits
//   DataType:      35 bits (32 payload + 1 predicate + 1 bypass + 1 delay)
//------------------------------------------------------------------------

module CgraRTL_2x2_wrapper (
  input  logic        clk,
  input  logic        reset,

  // CPU control interface (val-rdy, IntraCgraPkt = 182 bits)
  input  logic        recv_from_cpu_pkt_val,
  input  logic [181:0] recv_from_cpu_pkt_msg,
  output logic        recv_from_cpu_pkt_rdy,

  output logic        send_to_cpu_pkt_val,
  output logic [181:0] send_to_cpu_pkt_msg,
  input  logic        send_to_cpu_pkt_rdy,

  // Inter-CGRA NoC interface (InterCgraPkt = 185 bits)
  input  logic        recv_from_inter_cgra_noc_val,
  input  logic [184:0] recv_from_inter_cgra_noc_msg,
  output logic        recv_from_inter_cgra_noc_rdy,

  output logic        send_to_inter_cgra_noc_val,
  output logic [184:0] send_to_inter_cgra_noc_msg,
  input  logic        send_to_inter_cgra_noc_rdy,

  // Boundary data ports — south [0] and [1] (DataType = 35 bits each)
  input  logic        recv_data_on_boundary_south_0_val,
  input  logic [34:0] recv_data_on_boundary_south_0_msg,
  output logic        recv_data_on_boundary_south_0_rdy,
  input  logic        recv_data_on_boundary_south_1_val,
  input  logic [34:0] recv_data_on_boundary_south_1_msg,
  output logic        recv_data_on_boundary_south_1_rdy,

  output logic        send_data_on_boundary_south_0_val,
  output logic [34:0] send_data_on_boundary_south_0_msg,
  input  logic        send_data_on_boundary_south_0_rdy,
  output logic        send_data_on_boundary_south_1_val,
  output logic [34:0] send_data_on_boundary_south_1_msg,
  input  logic        send_data_on_boundary_south_1_rdy,

  // Boundary data ports — north [0] and [1]
  input  logic        recv_data_on_boundary_north_0_val,
  input  logic [34:0] recv_data_on_boundary_north_0_msg,
  output logic        recv_data_on_boundary_north_0_rdy,
  input  logic        recv_data_on_boundary_north_1_val,
  input  logic [34:0] recv_data_on_boundary_north_1_msg,
  output logic        recv_data_on_boundary_north_1_rdy,

  output logic        send_data_on_boundary_north_0_val,
  output logic [34:0] send_data_on_boundary_north_0_msg,
  input  logic        send_data_on_boundary_north_0_rdy,
  output logic        send_data_on_boundary_north_1_val,
  output logic [34:0] send_data_on_boundary_north_1_msg,
  input  logic        send_data_on_boundary_north_1_rdy,

  // Boundary data ports — east [0] and [1]
  input  logic        recv_data_on_boundary_east_0_val,
  input  logic [34:0] recv_data_on_boundary_east_0_msg,
  output logic        recv_data_on_boundary_east_0_rdy,
  input  logic        recv_data_on_boundary_east_1_val,
  input  logic [34:0] recv_data_on_boundary_east_1_msg,
  output logic        recv_data_on_boundary_east_1_rdy,

  output logic        send_data_on_boundary_east_0_val,
  output logic [34:0] send_data_on_boundary_east_0_msg,
  input  logic        send_data_on_boundary_east_0_rdy,
  output logic        send_data_on_boundary_east_1_val,
  output logic [34:0] send_data_on_boundary_east_1_msg,
  input  logic        send_data_on_boundary_east_1_rdy,

  // Boundary data ports — west [0] and [1]
  input  logic        recv_data_on_boundary_west_0_val,
  input  logic [34:0] recv_data_on_boundary_west_0_msg,
  output logic        recv_data_on_boundary_west_0_rdy,
  input  logic        recv_data_on_boundary_west_1_val,
  input  logic [34:0] recv_data_on_boundary_west_1_msg,
  output logic        recv_data_on_boundary_west_1_rdy,

  output logic        send_data_on_boundary_west_0_val,
  output logic [34:0] send_data_on_boundary_west_0_msg,
  input  logic        send_data_on_boundary_west_0_rdy,
  output logic        send_data_on_boundary_west_1_val,
  output logic [34:0] send_data_on_boundary_west_1_msg,
  input  logic        send_data_on_boundary_west_1_rdy,

  // Configuration
  input  logic [1:0]  cgra_id,
  input  logic [6:0]  address_lower,
  input  logic [6:0]  address_upper
);

  // ---- Wire declarations for struct/array conversion ----

  // CPU pkt (IntraCgraPkt struct type)
  IntraCgraPacket_4_4x1_4_8_2_CgraPayload__f7422fcfeaac5767 w_recv_from_cpu_pkt_msg;
  IntraCgraPacket_4_4x1_4_8_2_CgraPayload__f7422fcfeaac5767 w_send_to_cpu_pkt_msg;

  // Inter-CGRA NoC pkt (InterCgraPkt struct type)
  InterCgraPacket_4_4x1_4_8_4_CgraPayload__7a9c50769f063131 w_recv_from_inter_cgra_noc_msg;
  InterCgraPacket_4_4x1_4_8_4_CgraPayload__7a9c50769f063131 w_send_to_inter_cgra_noc_msg;

  // Boundary data (unpacked arrays)
  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_recv_south_msg [0:1];
  logic [0:0] w_recv_south_rdy [0:1];
  logic [0:0] w_recv_south_val [0:1];
  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_send_south_msg [0:1];
  logic [0:0] w_send_south_rdy [0:1];
  logic [0:0] w_send_south_val [0:1];

  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_recv_north_msg [0:1];
  logic [0:0] w_recv_north_rdy [0:1];
  logic [0:0] w_recv_north_val [0:1];
  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_send_north_msg [0:1];
  logic [0:0] w_send_north_rdy [0:1];
  logic [0:0] w_send_north_val [0:1];

  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_recv_east_msg [0:1];
  logic [0:0] w_recv_east_rdy [0:1];
  logic [0:0] w_recv_east_val [0:1];
  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_send_east_msg [0:1];
  logic [0:0] w_send_east_rdy [0:1];
  logic [0:0] w_send_east_val [0:1];

  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_recv_west_msg [0:1];
  logic [0:0] w_recv_west_rdy [0:1];
  logic [0:0] w_recv_west_val [0:1];
  CgraData_32_1_1_1__payload_32__predicate_1__bypass_1__delay_1 w_send_west_msg [0:1];
  logic [0:0] w_send_west_rdy [0:1];
  logic [0:0] w_send_west_val [0:1];

  // ---- Struct <-> flat bits conversion ----

  // CPU pkt: cast flat bits to/from struct
  assign w_recv_from_cpu_pkt_msg = recv_from_cpu_pkt_msg;
  assign send_to_cpu_pkt_msg = w_send_to_cpu_pkt_msg;

  // Inter-CGRA NoC pkt
  assign w_recv_from_inter_cgra_noc_msg = recv_from_inter_cgra_noc_msg;
  assign send_to_inter_cgra_noc_msg = w_send_to_inter_cgra_noc_msg;

  // ---- Boundary data: flat <-> unpacked array + struct ----

  // South
  assign w_recv_south_val[0] = recv_data_on_boundary_south_0_val;
  assign w_recv_south_msg[0] = recv_data_on_boundary_south_0_msg;
  assign recv_data_on_boundary_south_0_rdy = w_recv_south_rdy[0];
  assign w_recv_south_val[1] = recv_data_on_boundary_south_1_val;
  assign w_recv_south_msg[1] = recv_data_on_boundary_south_1_msg;
  assign recv_data_on_boundary_south_1_rdy = w_recv_south_rdy[1];

  assign send_data_on_boundary_south_0_val = w_send_south_val[0];
  assign send_data_on_boundary_south_0_msg = w_send_south_msg[0];
  assign w_send_south_rdy[0] = send_data_on_boundary_south_0_rdy;
  assign send_data_on_boundary_south_1_val = w_send_south_val[1];
  assign send_data_on_boundary_south_1_msg = w_send_south_msg[1];
  assign w_send_south_rdy[1] = send_data_on_boundary_south_1_rdy;

  // North
  assign w_recv_north_val[0] = recv_data_on_boundary_north_0_val;
  assign w_recv_north_msg[0] = recv_data_on_boundary_north_0_msg;
  assign recv_data_on_boundary_north_0_rdy = w_recv_north_rdy[0];
  assign w_recv_north_val[1] = recv_data_on_boundary_north_1_val;
  assign w_recv_north_msg[1] = recv_data_on_boundary_north_1_msg;
  assign recv_data_on_boundary_north_1_rdy = w_recv_north_rdy[1];

  assign send_data_on_boundary_north_0_val = w_send_north_val[0];
  assign send_data_on_boundary_north_0_msg = w_send_north_msg[0];
  assign w_send_north_rdy[0] = send_data_on_boundary_north_0_rdy;
  assign send_data_on_boundary_north_1_val = w_send_north_val[1];
  assign send_data_on_boundary_north_1_msg = w_send_north_msg[1];
  assign w_send_north_rdy[1] = send_data_on_boundary_north_1_rdy;

  // East
  assign w_recv_east_val[0] = recv_data_on_boundary_east_0_val;
  assign w_recv_east_msg[0] = recv_data_on_boundary_east_0_msg;
  assign recv_data_on_boundary_east_0_rdy = w_recv_east_rdy[0];
  assign w_recv_east_val[1] = recv_data_on_boundary_east_1_val;
  assign w_recv_east_msg[1] = recv_data_on_boundary_east_1_msg;
  assign recv_data_on_boundary_east_1_rdy = w_recv_east_rdy[1];

  assign send_data_on_boundary_east_0_val = w_send_east_val[0];
  assign send_data_on_boundary_east_0_msg = w_send_east_msg[0];
  assign w_send_east_rdy[0] = send_data_on_boundary_east_0_rdy;
  assign send_data_on_boundary_east_1_val = w_send_east_val[1];
  assign send_data_on_boundary_east_1_msg = w_send_east_msg[1];
  assign w_send_east_rdy[1] = send_data_on_boundary_east_1_rdy;

  // West
  assign w_recv_west_val[0] = recv_data_on_boundary_west_0_val;
  assign w_recv_west_msg[0] = recv_data_on_boundary_west_0_msg;
  assign recv_data_on_boundary_west_0_rdy = w_recv_west_rdy[0];
  assign w_recv_west_val[1] = recv_data_on_boundary_west_1_val;
  assign w_recv_west_msg[1] = recv_data_on_boundary_west_1_msg;
  assign recv_data_on_boundary_west_1_rdy = w_recv_west_rdy[1];

  assign send_data_on_boundary_west_0_val = w_send_west_val[0];
  assign send_data_on_boundary_west_0_msg = w_send_west_msg[0];
  assign w_send_west_rdy[0] = send_data_on_boundary_west_0_rdy;
  assign send_data_on_boundary_west_1_val = w_send_west_val[1];
  assign send_data_on_boundary_west_1_msg = w_send_west_msg[1];
  assign w_send_west_rdy[1] = send_data_on_boundary_west_1_rdy;

  // ---- Instantiate the PyMTL3-generated CGRA ----
  CgraRTL_2x2 cgra_inst (
    .clk                                ( clk ),
    .reset                              ( reset ),

    // CPU control interface
    .recv_from_cpu_pkt__val             ( recv_from_cpu_pkt_val ),
    .recv_from_cpu_pkt__msg             ( w_recv_from_cpu_pkt_msg ),
    .recv_from_cpu_pkt__rdy             ( recv_from_cpu_pkt_rdy ),
    .send_to_cpu_pkt__val               ( send_to_cpu_pkt_val ),
    .send_to_cpu_pkt__msg               ( w_send_to_cpu_pkt_msg ),
    .send_to_cpu_pkt__rdy               ( send_to_cpu_pkt_rdy ),

    // Inter-CGRA NoC
    .recv_from_inter_cgra_noc__val      ( recv_from_inter_cgra_noc_val ),
    .recv_from_inter_cgra_noc__msg      ( w_recv_from_inter_cgra_noc_msg ),
    .recv_from_inter_cgra_noc__rdy      ( recv_from_inter_cgra_noc_rdy ),
    .send_to_inter_cgra_noc__val        ( send_to_inter_cgra_noc_val ),
    .send_to_inter_cgra_noc__msg        ( w_send_to_inter_cgra_noc_msg ),
    .send_to_inter_cgra_noc__rdy        ( send_to_inter_cgra_noc_rdy ),

    // Boundary — South
    .recv_data_on_boundary_south__val   ( w_recv_south_val ),
    .recv_data_on_boundary_south__msg   ( w_recv_south_msg ),
    .recv_data_on_boundary_south__rdy   ( w_recv_south_rdy ),
    .send_data_on_boundary_south__val   ( w_send_south_val ),
    .send_data_on_boundary_south__msg   ( w_send_south_msg ),
    .send_data_on_boundary_south__rdy   ( w_send_south_rdy ),

    // Boundary — North
    .recv_data_on_boundary_north__val   ( w_recv_north_val ),
    .recv_data_on_boundary_north__msg   ( w_recv_north_msg ),
    .recv_data_on_boundary_north__rdy   ( w_recv_north_rdy ),
    .send_data_on_boundary_north__val   ( w_send_north_val ),
    .send_data_on_boundary_north__msg   ( w_send_north_msg ),
    .send_data_on_boundary_north__rdy   ( w_send_north_rdy ),

    // Boundary — East
    .recv_data_on_boundary_east__val    ( w_recv_east_val ),
    .recv_data_on_boundary_east__msg    ( w_recv_east_msg ),
    .recv_data_on_boundary_east__rdy    ( w_recv_east_rdy ),
    .send_data_on_boundary_east__val    ( w_send_east_val ),
    .send_data_on_boundary_east__msg    ( w_send_east_msg ),
    .send_data_on_boundary_east__rdy    ( w_send_east_rdy ),

    // Boundary — West
    .recv_data_on_boundary_west__val    ( w_recv_west_val ),
    .recv_data_on_boundary_west__msg    ( w_recv_west_msg ),
    .recv_data_on_boundary_west__rdy    ( w_recv_west_rdy ),
    .send_data_on_boundary_west__val    ( w_send_west_val ),
    .send_data_on_boundary_west__msg    ( w_send_west_msg ),
    .send_data_on_boundary_west__rdy    ( w_send_west_rdy ),

    // Configuration
    .cgra_id                            ( cgra_id ),
    .address_lower                      ( address_lower ),
    .address_upper                      ( address_upper )
  );

endmodule
