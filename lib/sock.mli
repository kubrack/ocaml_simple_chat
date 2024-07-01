val rm_sock : unit -> unit
val safe_call : (unit -> 'a) -> (Unix.error -> 'a) -> 'a
val rsocket : unit -> Core_unix.File_descr.t
val lsocket_w : Unix.file_descr
val lsocket_r : Unix.file_descr
val nblk_call : ('a -> 'b -> 'c -> 'd -> int) -> 'a -> 'b -> 'c -> 'd -> int

