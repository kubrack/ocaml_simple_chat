open Chat.Sock

(*
let seq = ref 0
let next_seq () = incr seq; !seq
*)
let net_socket = (rsocket())
let cli_socket = (lsocket())

let buffer_len = 64 * 1024
let buffer = Bytes.create buffer_len

let net_reader buf pos_to len =
    nblk_call Unix.read net_socket buf pos_to len

let data_handler fn buf _len _is_rcvd = fn buf

(* Main loop: infinite mutual recursion: read_remote aka RR -> WL -> RL -> WR -> RR -> ... *)
let rec read_remote buf = 
    let rcvd = net_reader buf 0 buffer_len in 
    data_handler write_local buf rcvd true
and write_local buf = 
    let sent = nblk_call Unix.write cli_socket buf 0 buffer_len in 
    data_handler read_local buf sent false
and read_local buf = 
    let rcvd = nblk_call Unix.read cli_socket buf 0 buffer_len in 
    data_handler write_remote buf rcvd true
and write_remote buf = 
    let sent = nblk_call Unix.write net_socket buf 0 buffer_len in 
    data_handler read_remote buf sent false

let () = write_remote buffer
