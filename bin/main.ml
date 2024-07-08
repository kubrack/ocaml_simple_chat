open Printf

let send_buf buf =
  let _rcdv = Unix.write (Chat.Sock.socket ()) buf 0 (Bytes.length buf) in ()

let ack_handler id = 
  eprintf "ACK [%Ld] RTT = %f s\n%!" id (Chat.Msg.rtt_s id)

let msg_handler buf id = 
  printf "%s%!" (Bytes.to_string buf);
  match id with
  | 0L -> ()
  | _ -> (
    Chat.Msg.compose_ack id |> send_buf;
    eprintf "GOT [%Ld] \n%!" id)

let read_net () = 
  let reader buf pos_to len = 
    Lwt_unix.read (Lwt_unix.of_unix_file_descr (Chat.Sock.socket ())) buf pos_to len in
  let on_read_fail ex = 
    match ex with
    | Unix.Unix_error (unix_err, _syscall, _where) -> (
      let () = eprintf "%s\n%!" (Unix.error_message unix_err) in
      let () = Chat.Sock.rm_sock () in
      Lwt.return 0)
    | _ -> raise ex in
  Chat.Msg.net_msg_fsm reader ack_handler msg_handler on_read_fail

let p_stdin () = Lwt_io.read_line (Lwt_io.of_unix_fd ~mode:Lwt_io.Input Unix.stdin)
let rec cb_stdin line =
  let buf = Bytes.of_string (String.concat "" [ line; "\n" ]) in
  let msg = Chat.Msg.compose_msg buf in
  let msg_id = Chat.Msg.msg_id msg in
  let _rcvd = send_buf msg in
  eprintf "PUT [%Ld]\n%!" msg_id;
  Lwt.bind (p_stdin ()) cb_stdin

let rec ev_loop () =
  let _ = Lwt_main.run (read_net ()) in
  ev_loop ()

let _ = Lwt.bind (p_stdin ()) cb_stdin
let () = ev_loop ()
