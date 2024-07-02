


let send_buf buf =
  let rcdv = Unix.write (Chat.Sock.socket ()) buf 0 (Bytes.length buf) in 
  let () = Core.eprintf "send_buf [%d] []" rcdv in
  ()

(*
let () =
  Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ -> Chat.Sock.rm_sock ()))
*)

let ack_handler seq = 
  Core.printf "ACK for msg seq [%d] RTT [%f] ms\n%!" seq (Chat.Msg.rtt_s seq)

let msg_handler buf seq = 
  let _ = Chat.Msg.compose_ack seq |> send_buf in
  Core.printf "MSG %s" (Bytes.to_string buf)

let remote_to_local () = 
  let reader buf pos_to len = Unix.read (Chat.Sock.socket ()) buf pos_to len in
  let fsm = Chat.Msg.net_msg_fsm reader ack_handler msg_handler in
  Lwt.bind (fsm() |> Lwt.return) (fun _ -> Lwt.return ())

(*
let print_buf buf from len = 
  Bytes.sub buf from len |> Bytes.to_string |> Lwt_io.print  

let recv_buf buf pos_to len = 
  let fd = Lwt_unix.of_unix_file_descr (Chat.Sock.socket ()) in
  Lwt_unix.read fd buf pos_to len

let remote_to_local () = 
  let buf = Bytes.create Chat.Msg.buffer_len in
  let promise = recv_buf buf 0 Chat.Msg.buffer_len in
  Lwt.bind promise (fun recvd -> print_buf buf 0 recvd)
*)


let local_to_remote () =
  let promise = Lwt_io.(read_line stdin) in
  Lwt.bind promise (fun line ->
    let () = Core.eprintf "------------- %s\n " line in
    let buf = Bytes.of_string (String.concat "" [ line; "\n" ]) in
    let seq = Chat.Msg.next_seq () in
    Chat.Msg.compose_msg seq buf |> send_buf |> Lwt.return)

let rec io_loop () =
  let () = Core.eprintf ".%!" in
  let _results = Lwt_main.run (Lwt.nchoose [ local_to_remote (); remote_to_local () ]) in
  io_loop ()

let () = io_loop ()
