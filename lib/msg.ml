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
let buffer_len = 64 * 1024
let max_data_chunk = buffer_len - proto_data_offset

let now_us () =
  Time_now.nanoseconds_since_unix_epoch () |> Base.Int63.to_int64 |> fun ns ->
  Int64.div ns 1000L

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
let msg_id msg = match extract_header msg with _mtype, time, _length -> time

let net_msg_fsm reader ack_handler msg_handler on_read_fail =
  let buf = Bytes.create buffer_len in
  let p_s_sync () = reader buf 0 1 in
  let rec cb_s_sync rcvd = 
    match rcvd with
    | 0 -> Lwt.try_bind p_s_sync cb_s_sync on_read_fail
    | _ -> (
        match Bytes.get buf 0 with
        | v when v = proto_ver -> Lwt.try_bind p_s_start cb_s_start on_read_fail
        | _ -> Lwt.try_bind p_s_sync cb_s_sync on_read_fail)
  and p_s_start () = reader buf 1 (proto_data_offset - 1)
  and cb_s_start rcvd =
    match rcvd with
    | header_len when header_len = proto_data_offset - 1 -> (
        match extract_header buf with
        | mtype, id, 0 when mtype = proto_type_ack -> (
          ack_handler id;
          Lwt.try_bind p_s_sync cb_s_sync on_read_fail)
        | mtype, id, len when mtype = proto_type_msg -> (
          Lwt.try_bind
            (fun () -> p_s_msg len) 
            (fun rcvd -> cb_s_msg id len rcvd) 
            on_read_fail)
        | _ -> Lwt.try_bind p_s_sync cb_s_sync on_read_fail)
    | _ -> Lwt.try_bind p_s_sync cb_s_sync on_read_fail
  and p_s_msg len_left = 
    let len_to_get = if len_left > max_data_chunk then max_data_chunk else len_left in
    reader buf proto_data_offset len_to_get 
  and cb_s_msg id len rcvd =
    let len_left = len - rcvd in
    let msg = Bytes.sub buf proto_data_offset rcvd in
    match len_left > 0 with
    | true ->
        let () = msg_handler msg 0L in
        Lwt.try_bind 
          (fun () -> p_s_msg len_left) 
          (fun rcvd -> cb_s_msg id len_left rcvd) 
          on_read_fail
    | _ ->
        let () = msg_handler msg id in
        Lwt.try_bind p_s_sync cb_s_sync on_read_fail
  in Lwt.try_bind p_s_sync cb_s_sync on_read_fail
