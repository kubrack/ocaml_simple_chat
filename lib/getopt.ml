open Core

type opt_t = { host: string; port: int };;

let usage_msg = "Run as server: chat [-p port] \nRun as client: chat [-p port] host \nDefault port is 6666"
let port = ref 6666
let host = ref ""
let speclist = [("-p", Arg.Set_int port, "port number to listen or connect to")]

let parse_opts () =
    let () = Arg.parse speclist (fun line -> host := line) usage_msg in
    {port = !port; host = !host}

let opts = Lazy.from_fun parse_opts

let host () = (Lazy.force opts).host
let port () = (Lazy.force opts).port

let is_run_as_server () = String.is_empty (host()) 

