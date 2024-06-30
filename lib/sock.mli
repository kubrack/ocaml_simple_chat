val lsocket : unit -> Unix.file_descr
val rsocket : unit -> Unix.file_descr
val nblk_call : ('a -> 'b -> 'c -> 'd -> int) -> 'a -> 'b -> 'c -> 'd -> int
