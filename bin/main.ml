
let send_buf buf =
  let _rcdv = Unix.write (Chat.Sock.socket ()) buf 0 (Bytes.length buf) in ()

let ack_handler id = 
  Core.eprintf "ACK [%Ld] RTT = %f s\n%!" id (Chat.Msg.rtt_s id)

let msg_handler buf id = 
  Core.printf "%s%!" (Bytes.to_string buf);
  match id with
  | 0L -> ()
  | _ -> (
    Chat.Msg.compose_ack id |> send_buf;
    Core.eprintf "GOT [%Ld] \n%!" id)

let rec remote_to_local () = 
  let reader buf pos_to len = Unix.read (Chat.Sock.socket ()) buf pos_to len in
  let fsm = Chat.Msg.net_msg_fsm reader ack_handler msg_handler in
  match fsm() with 
  | None -> (Chat.Sock.rm_sock (); remote_to_local ())
  | _ -> remote_to_local ()

let rec local_to_remote () =
  let line = read_line () in
  let buf = Bytes.of_string (String.concat "" [ line; "\n" ]) in
  let msg = Chat.Msg.compose_msg buf in
  let msg_id = Chat.Msg.msg_id msg in
  let _rcvd = send_buf msg in
  Core.eprintf "PUT [%Ld]\n%!" msg_id;
  local_to_remote ()

let rec spawn () =
  let _ = Chat.Sock.new_socket () in
  match Unix.fork () with 
  | 0 -> remote_to_local () 
  | pid1 -> begin
    match Unix.fork () with 
    | 0 -> local_to_remote () 
    | pid2 -> begin 
      match Unix.wait () with
      | (pid, _status) when pid = pid1 -> Unix.kill pid2 Sys.sigkill
      | (pid, _status) when pid = pid2 -> Unix.kill pid1 Sys.sigkill
      | _ -> ();
    end
  end;
  let _ = Unix.wait () in spawn ()

let () = spawn ()
