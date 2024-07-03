val rm_sock : unit -> unit
val new_socket : unit -> Unix.file_descr
val safe_call : (unit -> 'a) -> (Unix.error -> 'a) -> 'a
val socket : unit -> Core_unix.File_descr.t

