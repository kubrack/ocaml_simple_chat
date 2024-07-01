open Chat.Sock

(*
let seq = ref 0
let next_seq () = incr seq; !seq
*)

let buffer_len = 64 * 1024
let buffer = Bytes.create buffer_len

let net_reader buf pos_to len =
  let r () = nblk_call Unix.read (rsocket ()) buf pos_to len in
  let on_ignore _ =
    rm_sock ();
    0
  in
  safe_call r on_ignore

let net_writer buf pos_to len =
  let w () = nblk_call Unix.write (rsocket ()) buf pos_to len in
  let on_ignore _ =
    rm_sock ();
    0
  in
  safe_call w on_ignore

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

(* Main loop: infinite mutual recursion: read_remote aka RR -> WL -> RL -> WR -> RR -> ... *)
let rec read_remote buf =
  Unix.sleepf 0.1;
  let rcvd = net_reader buf 0 buffer_len in
  write_local buf rcvd

and write_local buf len =
  let _sent = nblk_call Unix.write lsocket_w buf 0 len in
  read_local buf

and read_local buf =
  let rcvd = nblk_call Unix.read lsocket_r buf 0 buffer_len in
  write_remote buf rcvd

and write_remote buf len =
  let _sent = net_writer buf 0 len in
  read_remote buf

let () = write_remote buffer 0
