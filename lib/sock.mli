val rm_sock : unit -> unit
val safe_call : (unit -> 'a) -> (Unix.error -> 'a) -> 'a
val nblk_call : ('a -> 'b -> 'c -> 'd -> int) -> 'a -> 'b -> 'c -> 'd -> int
val socket : unit -> Core_unix.File_descr.t

