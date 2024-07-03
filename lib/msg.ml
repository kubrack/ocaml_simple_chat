(* Protocol
   +0  1 byte  version
   +1  1 byte  type of msg (M - msg, P - part of msg, A - ack)
   +2  8 bytes id of msg, 0 means part of msg
   +10 2 bytes data length
   +12 0 .. (2**16 - 12) data
*)
let proto_type_ack = 'A'
let proto_type_msg = 'M'
let proto_ver = '!'
let proto_ver_offset = 0
let proto_type_offset = 1
let proto_time_offset = 2
let proto_length_offset = 10
let proto_data_offset = 12

let buffer_len = 64 (* FIXME * 1024 *)

let now_us () = 
  Time_now.nanoseconds_since_unix_epoch() 
  |> Base.Int63.to_int64 
  |> (fun ns -> Int64.div ns 1000L)

let rtt id = Int64.sub (now_us ()) id
let rtt_s id = (rtt id |> Int64.to_int |> float_of_int) /. 1000000.

let compose mtype id msg_buf =
  let length = Bytes.length msg_buf in
  let header = Bytes.create proto_data_offset in
  let () = Bytes.set header proto_ver_offset proto_ver in
  let () = Bytes.set header proto_type_offset mtype in
  let () = Bytes.set_int64_be header proto_time_offset id in
  let () = Bytes.set_uint16_be header proto_length_offset length in
  Bytes.cat header msg_buf

let extract_header buf = 
  let mtype = Bytes.get buf proto_type_offset in
  let time = Bytes.get_int64_be buf proto_time_offset in
  let length = Bytes.get_uint16_be buf proto_length_offset in
  (mtype, time, length)

let compose_msg buf = compose proto_type_msg (now_us ()) buf
let compose_ack id = compose proto_type_ack id (Bytes.create 0)
let msg_id msg = match extract_header msg with (_mtype, time, _length) -> time

let net_msg_fsm reader ack_handler msg_handler =
  let buf = Bytes.create buffer_len in
  let ()  = Bytes.fill buf 0 64 '_' in (* FIXME *)
  let _debug_fn m = Core.eprintf "== fsm: %s buf=[%s]\n%!" m (Bytes.to_string buf) in
  let debug_fn _ = () in
  let rec s_sync () =
    debug_fn __FUNCTION__;
    match reader buf 0 1 with
    | 0 -> None
    | _ -> (
        match Bytes.get buf 0 with 
        | v when v = proto_ver -> s_start () 
        | _ -> s_sync ())
  and s_start () =
    debug_fn __FUNCTION__;
    let data_len = (proto_data_offset - 1) in
    match reader buf 1 data_len with
    | h_len when h_len = (proto_data_offset -1 ) -> (
      match extract_header buf with
      | mtype, time, 0 when mtype = proto_type_ack -> s_ack time
      | mtype, time, len when mtype = proto_type_msg -> s_msg time len
      | _ -> s_sync ())
    | _ -> s_sync ()
  and s_ack id =
    debug_fn __FUNCTION__;
    ack_handler id;
    Some buf
  and s_msg id len =
    debug_fn (__FUNCTION__ ^ (Core.sprintf " id [%Ld] len [%d]" id len));
    let max_data_chunk = buffer_len - proto_data_offset in
    let to_get = if len > max_data_chunk then max_data_chunk else len in
    let recvd = reader buf proto_data_offset to_get in
    let msg = Bytes.sub buf proto_data_offset recvd in
    match len > recvd with
    | true -> let () = msg_handler msg 0L in s_msg id (len - recvd)
    | _ -> let () = msg_handler msg id in Some msg
  in
  s_sync 
