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

let seq = ref 0
let seq_ts = Array.make (64 * 1024) 0 (* 2 bytes in header ring *) 

let now_us () = 
  Time_now.nanoseconds_since_unix_epoch() 
  |> Base.Int63.to_int64 
  |> (fun i -> Int64.div i 1000L) 
  |> Int64.to_int

let next_seq () = 
  let () = incr seq in 
  let seq = !seq in
  let () = seq_ts.(seq) <- now_us () in
  seq

let rtt seq = now_us () - seq_ts.(seq)
let rtt_s seq = (rtt seq |> float_of_int) /. 1000000.

let char_list_of_buf buf len =
  Bytes.sub_string buf 0 len |> String.to_seq |> List.of_seq

let lsb16_of_int i = char_of_int @@ (i mod 256)
let msb16_of_int i = char_of_int @@ (i / 256 mod 256)
let int_of_i16be msb lsb = (int_of_char msb * 256) + int_of_char lsb

let compose t seq msg_buf =
  let length = Bytes.length msg_buf in
  let header =
    String.make 1 proto_ver ^ String.make 1 t
    ^ String.make 1 (msb16_of_int seq)
    ^ String.make 1 (lsb16_of_int seq)
    ^ String.make 1 (msb16_of_int length)
    ^ String.make 1 (lsb16_of_int length)
  in
  Bytes.cat (Bytes.of_string header) msg_buf

let compose_msg seq buf = compose proto_type_msg seq buf
let compose_ack seq     = compose proto_type_ack seq (Bytes.create 0)

let net_msg_fsm reader ack_handler msg_handler =
  let buf = Bytes.create buffer_len in
  let ()  = Bytes.fill buf 0 64 '_' in (* FIXME *)
  (* let debug_fn m = Core.eprintf "== fsm: %s buf=[%s]\n%!" m (Bytes.to_string buf) in *)
  let debug_fn _ = () in
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
            s_msg
              (int_of_i16be seq_msb seq_lsb)
              (int_of_i16be len_msb len_lsb)
        | _ -> s_sync ())
  and s_ack seq =
    debug_fn __FUNCTION__;
    ack_handler seq;
    Some buf
  and s_msg seq len =
    debug_fn (__FUNCTION__ ^ (Core.sprintf " seq [%d] len [%d]" seq len));
    let rcvd = reader buf proto_data_offset len in
    match rcvd with
    | 0 -> s_sync ()
    | _ ->
        let msg = Bytes.sub buf proto_data_offset len in 
        let () = msg_handler msg seq in Some msg
  in
  s_sync 
