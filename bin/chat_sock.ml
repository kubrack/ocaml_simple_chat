open Unix

let retry_in = 1
let server_listen_on = "localhost"
let host = Chat_getopt.host()
let port = Chat_getopt.port()
let sock = ref (Core_unix.File_descr.of_int 0)

let new_client_sock host port = 
    let ip_addr = (gethostbyname host).h_addr_list.(0) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    connect sock (ADDR_INET(ip_addr, port));
    sock

let new_server_sock port = 
    let ip_addr = (gethostbyname server_listen_on).h_addr_list.(0) in
    let sock_addr = ADDR_INET (ip_addr, port) in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock sock_addr;
    listen sock 1;
    sock

let is_ex_non_fatal err = match err with
    | EADDRINUSE
    | EAGAIN
    | EALREADY
    | ECONNREFUSED
    | EHOSTUNREACH
    | EINPROGRESS
    | ENETDOWN
    | ENETUNREACH
    | ETIMEDOUT
    | ECONNRESET
    | EIO
    | EWOULDBLOCK -> true
    | _ -> false

(* TODO SIGPIPE at closed sock ---> set SIG handler *)
(* TODO (Unix.Unix_error "Socket is not connected" write "") in server mode ---> accept(2) *)
(* TODO Sys_blocked_io - A special case of Sys_error raised when no I/O is possible on a non-blocking I/O channel. 
   on set_nonblock Unix.stdin ---> FD dup?  *)
let rec new_socket () = 
    Core.eprintf "Creating new socket...\n";
    try
        let socket = if Chat_getopt.is_run_as_server () 
                                 then new_server_sock port 
                                 else new_client_sock host port in
        set_nonblock socket;
        sock := socket;
        socket         
    with 
        Unix_error (unix_err, _syscall, _where) as ex -> 
            Core.eprintf "%s, " (Core_unix.Error.message unix_err);
            if is_ex_non_fatal unix_err
            then begin 
                Core.eprintf "Retrying in %d second(s)...\n%!" retry_in;
                Unix.sleep retry_in;
                new_socket()
            end
            else raise ex

let is_sock_ok socket = match Core_unix.File_descr.to_int(socket) with 0 -> false | _ -> true

let rsocket () = if is_sock_ok !sock then !sock else new_socket ()
let lsocket () = Unix.stdin
(*let lsocket () = let () = set_nonblock Unix.stdin in Unix.stdin  TODO Lazy *)

