open Core

type opt_t = { host : string; port : int; retry_in : int }

let usage_msg =
  "Run as server: chat [-i retry_in] [-p port] \n\
   Run as client: chat [-i retry_in] [-p port] host \n\
   Default port is 6666"

let port = ref 6666
let host = ref ""
let retry_in = ref 1

let speclist =
  [ ("-p", Arg.Set_int port, "port number to listen or connect to");
    ("-i", Arg.Set_int retry_in, "reconnect interval")]

let parse_opts () =
  let () = Arg.parse speclist (fun line -> host := line) usage_msg in
  { port = !port; host = !host; retry_in = !retry_in }

let opts = Lazy.from_fun parse_opts
let host () = (Lazy.force opts).host
let port () = (Lazy.force opts).port
let retry_in () = (Lazy.force opts).retry_in
let is_run_as_server () = String.is_empty (host ())
