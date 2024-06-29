open Unix

let retry_in = 1 (* TODO whap errs *)
let server_listen_on = "localhost"
let host = Chat_getopt.host()
let port = Chat_getopt.port()
let sock = ref None

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

let new_socket () = 
    let socket = 
        if Chat_getopt.is_run_as_server () 
        then new_server_sock port else new_client_sock host port in
    sock := (Some socket);
    (Some socket)

let is_sock_ok socket = match socket with Some _ -> true | None -> false

let socket () = if is_sock_ok !sock then !sock else new_socket ()

