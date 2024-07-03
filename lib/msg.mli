val rtt_s : int64 -> float
val compose_msg : bytes -> bytes
val compose_ack : int64 -> bytes
val msg_id : bytes -> int64
val net_msg_fsm :
  (bytes -> int -> int -> int) ->
  (int64 -> unit) -> (bytes -> int64 -> unit) -> unit -> bytes option
