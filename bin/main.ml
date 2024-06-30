module Sock = Chat_sock

let buffer_len = 64 * 1024
let buffer = Bytes.create buffer_len

let iters = ref 0
let iii () = iters := if !iters > 10000 then begin Core.eprintf ".%!"; 0 end else !iters + 1

let nblk fn fd buf pos len =
    try fn fd buf pos len
    with Unix.Unix_error (unix_err, _syscall, _where) as ex ->
        match unix_err with 
        | EAGAIN | EWOULDBLOCK -> 0
        | _ -> raise ex 

let rec read_remote buf = 
    let _rcvd = nblk Unix.read  (Sock.rsocket()) buf 0 buffer_len in write_local buf
and write_local buf = 
    let _sent = nblk Unix.write (Sock.lsocket()) buf 0 buffer_len in read_local buf
and read_local buf = 
    let _rcvd = nblk Unix.read  (Sock.lsocket()) buf 0 buffer_len in 
    let () = iii()                                                in write_remote buf
and write_remote buf = 
    let _sent = nblk Unix.write (Sock.rsocket()) buf 0 buffer_len in read_remote buf

let () = write_remote buffer
