open Unix

let retry_in = 1
let server_listen_on = "localhost"
let host = Getopt.host()
let port = Getopt.port()
let sock = ref (Core_unix.File_descr.of_int 0)

let rm_sock() = sock := (Core_unix.File_descr.of_int 0)

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
    let acpt() = 
        let (accepted, addr) = accept listen_sock in
        let msg = match addr with 
        | ADDR_INET(ip, p) -> (string_of_inet_addr ip) ^":"^ (string_of_int p)
        | _ -> "???" in 
        let () = Core.eprintf "Accepted: %s\n%!" msg in
        accepted in
    acpt
let new_server_sock = lazy (mk_new_server_sock port)

let is_ex_non_fatal err = match err with
    | EADDRINUSE
    | EAGAIN
    | EALREADY
    | ECONNABORTED
    | ECONNREFUSED
    | ECONNRESET
    | EHOSTUNREACH
    | EINPROGRESS
    | EINTR
    | EINVAL
    | ENETDOWN
    | ENETUNREACH
    | ENOTCONN
    | EPIPE
    | ETIMEDOUT
    | EIO
    | EWOULDBLOCK -> true
    | _ -> false

let safe_call action on_ignore = 
    try action()
    with Unix_error (unix_err, _syscall, _where) as ex -> 
        if is_ex_non_fatal unix_err then on_ignore unix_err else raise ex 

let rec socket_create() = 
    let socket = if Getopt.is_run_as_server () 
        then ((Lazy.force new_server_sock)()) else new_client_sock host port in
    set_nonblock socket;
    sock := socket;
    socket         
and socket_re_create unix_err = 
    Core.eprintf "%s, retrying in %d second(s)...\n%!" (Core_unix.Error.message unix_err) retry_in;
    Unix.sleep retry_in;
    new_socket()
and new_socket() = 
    Core.eprintf "Creating new socket...\n";
    safe_call socket_create socket_re_create

let is_sock_ok socket = match Core_unix.File_descr.to_int(socket) with 0 -> false | _ -> true

let rsocket() = if is_sock_ok !sock then !sock else new_socket ()

let lsocket_w = stdout
let lsocket_r = dup stdin
let () = set_nonblock lsocket_r

let nblk_call fn fd buf pos len =
    try fn fd buf pos len
    with Unix.Unix_error (unix_err, _syscall, _where) as ex ->
        match unix_err with 
        | EAGAIN | EWOULDBLOCK -> 0
        | _ -> raise ex 

