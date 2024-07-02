(*
let seq = ref 0
let next_seq () = incr seq; !seq
*)
let () =
  Sys.set_signal Sys.sigpipe (Sys.Signal_handle (fun _ -> Chat.Sock.rm_sock ()))

let chat_send buf =
  let call () = Unix.write (Chat.Sock.socket ()) buf 0 (Bytes.length buf) in
  let on_fail _ = Chat.Sock.rm_sock (); 0 in
  Chat.Sock.safe_call call on_fail


let remote_to_local () =
  let buf = Bytes.create Chat.Msg.buffer_len in
  let fd = Lwt_unix.of_unix_file_descr (Chat.Sock.socket ()) in
  let promise = Lwt_unix.read fd buf 0 Chat.Msg.buffer_len in
  Lwt.bind promise (fun recvd ->
      Unix.write Unix.stdout buf 0 recvd |> Lwt.return)

let local_to_remote () =
  let promise = Lwt_io.(read_line stdin) in
  Lwt.bind promise (fun line ->
      let buf = Bytes.of_string (String.concat "" [ line; "\n" ]) in
      (* Chat.Msg.compose Chat.Msg.proto_type_msg 1 buf *)
      chat_send buf |> Lwt.return) 

let rec io_loop () =
  (*let () = Core.eprintf ".%!" in *)
  let _results = Lwt_main.run (Lwt.nchoose [ remote_to_local (); local_to_remote () ]) in
  io_loop ()

let () = io_loop ()
