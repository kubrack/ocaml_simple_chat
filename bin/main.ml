
let send_buf buf =
  let _rcdv = Unix.write (Chat.Sock.socket ()) buf 0 (Bytes.length buf) in ()

let ack_handler seq = 
  Core.printf "ACK for msg seq [%d] RTT [%f] ms\n%!" seq (Chat.Msg.rtt_s seq)

let msg_handler buf seq = 
  let _ = Chat.Msg.compose_ack seq |> send_buf in
  Core.printf "MSG [%d]: %s%!" seq (Bytes.to_string buf)

let rec remote_to_local () = 
  let reader buf pos_to len = Unix.read (Chat.Sock.socket ()) buf pos_to len in
  let fsm = Chat.Msg.net_msg_fsm reader ack_handler msg_handler in
  let _ = fsm() in
  remote_to_local ()

let rec local_to_remote () =
  let line = read_line () in 
  let buf = Bytes.of_string (String.concat "" [ line; "\n" ]) in
  let seq = Chat.Msg.next_seq () in
  let _rcvd = Chat.Msg.compose_msg seq buf |> send_buf in
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
