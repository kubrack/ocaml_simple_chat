open Unix

let retry_in = 1
let server_listen_on = "0.0.0.0"
let host = Getopt.host ()
let port = Getopt.port ()
let sock = ref None
let rm_sock () = sock := None

let new_client_sock host port =
  let ip_addr = (gethostbyname host).h_addr_list.(0) in
  let sock_addr = ADDR_INET (ip_addr, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  let () = connect sock sock_addr in
  sock

let mk_new_server_sock port =
  let ip_addr = (gethostbyname server_listen_on).h_addr_list.(0) in
  let sock_addr = ADDR_INET (ip_addr, port) in
  let listen_sock = socket PF_INET SOCK_STREAM 0 in
  let () = bind listen_sock sock_addr in
  let () = listen listen_sock 1 in
  Core.eprintf "Listening at %s:%d\n%!" (string_of_inet_addr ip_addr) port;
  let acpt () =
    let accepted, addr = accept listen_sock in
    let msg =
      match addr with
      | ADDR_INET (ip, p) -> string_of_inet_addr ip ^ ":" ^ string_of_int p
      | _ -> "???"
    in
    let () = Core.eprintf "Accepted: %s\n%!" msg in
    accepted
  in
  acpt

let new_server_sock = lazy (mk_new_server_sock port)

let is_ex_non_fatal err =
  match err with
  | EADDRINUSE | EAGAIN | EALREADY | ECONNABORTED | ECONNREFUSED | ECONNRESET
  | EHOSTUNREACH | EINPROGRESS | EINTR | EINVAL | ENETDOWN | ENETUNREACH
  | ENOTCONN | EPIPE | ETIMEDOUT | EIO | EWOULDBLOCK ->
      true
  | _ -> false

let safe_call action on_call_fails =
  try action ()
  with Unix_error (unix_err, _syscall, _where) as ex ->
    if is_ex_non_fatal unix_err then on_call_fails unix_err else raise ex

let rec socket_create () =
  let socket =
    if Getopt.is_run_as_server () then (Lazy.force new_server_sock) ()
    else new_client_sock host port
  in
  Core.eprintf "New socket created \n%!";
  sock := Some socket;
  socket

and socket_re_create unix_err =
  Core.eprintf "%s, retrying in %d second(s)...\n%!"
    (Core_unix.Error.message unix_err)
    retry_in;
  Unix.sleep retry_in;
  new_socket ()

and new_socket () =
  safe_call socket_create socket_re_create

let socket () = match !sock with
  | Some s -> s
  | _ -> new_socket ()

