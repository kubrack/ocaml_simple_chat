(* Protocol
   1 byte  version
   1 byte  type of msg (M - msg, P - part of msg, A - ack)
   2 bytes seq number of msg (in chat)
   2 bytes data length
   0 .. (2**16 - proto_offset_data - proto_offset_data) data
*)
let proto_type_ack = 'A'
let proto_type_msg = 'M'
let proto_ver = '!'
let proto_data_offset = 6

type proto_type_t = Ack of char | Msg of char

let buffer_len = 64  (* FIXME * 1024 *)

let char_list_of_buf buf len =
  Bytes.sub_string buf 0 len |> String.to_seq |> List.of_seq

let lsb16_of_int i = char_of_int @@ (i mod 256)
let msb16_of_int i = char_of_int @@ (i / 256 mod 256)
let int_of_i16be msb lsb = (int_of_char msb * 256) + int_of_char lsb

let compose t id msg =
  let length = Bytes.length msg in
  let header =
    String.make 1 proto_ver ^ String.make 1 t
    ^ String.make 1 (msb16_of_int id)
    ^ String.make 1 (lsb16_of_int id)
    ^ String.make 1 (msb16_of_int length)
    ^ String.make 1 (lsb16_of_int length)
  in
  Bytes.cat (Bytes.of_string header) msg

let net_msg_fsm reader ack_handler msg_handler =
  let buf = Bytes.create buffer_len in
  let ()  = Bytes.fill buf 0 64 '-' in (* FIXME *)
  let debug_fn m = Core.eprintf "== fsm: %s buf=[%s]\n%!" m (Bytes.to_string buf) in
  let rec s_sync () =
    debug_fn __FUNCTION__;
    match reader buf 0 1 with
    | 0 -> None
    | _ -> (
        match Bytes.get buf 0 with v when v = proto_ver -> s_start () | _ -> s_sync ())
  and s_start () =
    debug_fn __FUNCTION__;
    (* TODO if rcvd < proto_data_offset ... *)
    let rcvd = reader buf 1 (proto_data_offset - 1) in
    match rcvd with
    | 0 -> s_sync ()
    | _ -> (
        let ch_list = char_list_of_buf buf proto_data_offset in
        match ch_list with
        | [ _; a; seq_msb; seq_lsb; '\000'; '\000' ] when a = proto_type_ack ->
            s_ack (int_of_i16be seq_msb seq_lsb)
        | [ _; m; seq_msb; seq_lsb; len_msb; len_lsb ] when m = proto_type_msg->
            s_msg_start
              (int_of_i16be seq_msb seq_lsb)
              (int_of_i16be len_msb len_lsb)
        | _ -> s_sync ())
  and s_ack seq =
    debug_fn __FUNCTION__;
    ack_handler seq;
    (* RTT *)
    Some buf
    (*TODO trim buf*)
  and s_msg_start seq len =
    debug_fn __FUNCTION__;
    (*match rcvd with
      | len -> s_msg_done()
      | _ -> s_msg_continue*)
    let rcvd = reader buf proto_data_offset len in
    (* TODO receive rest *)
    match rcvd with
    | 0 -> s_sync ()
    | _ ->
        msg_handler buf seq len;
        Some buf
    (*TODO trim buf
      Bytes.sub buf 0 (proto_data_offset + len) in *)
  in
  let enterpoint () =
    debug_fn __FUNCTION__;
    (*match buf_pointer with
      | 0 -> s_start()
      | x when 0 < x && x < proto_data_offset -> s_header_continue buf_pointer
      | _ -> s_msg_continue buf_pointer in *)
    s_sync ()
  in
  enterpoint
