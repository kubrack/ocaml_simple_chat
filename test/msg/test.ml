open OUnit2
open Chat.Msg

let msg_ok_full = Bytes.of_string "!M\000\010\000\0200123456789abcdefghij"
let msg_ok_p1 = Bytes.of_string "!M\000\010\000\0200123456789"
let _msg_ok_p2 = Bytes.of_string "abcdefghi"
let _msg_ok_p3 = Bytes.of_string "j"
let _msg_fail_short_p1 = msg_ok_p1
let _msg_fail_short_p2 = msg_ok_full
let _ack_ok_full = Bytes.of_string "!A\000\010\000\000"
let _ack_ok_p1 = Bytes.of_string "!A\000"
let _ack_ok_p2 = Bytes.of_string "\010\000\000"
let _unk_ok_p1 = Bytes.of_string "!"
let _unk_ok_p2 = Bytes.of_string "A\000\010\000\000"

let mk_reader source =
  let from = ref 0 in
  let reader buf pos_to len =
    let left = Bytes.length source - !from in
    let rcvd = if left > len then len else left in
    Bytes.blit source !from buf pos_to rcvd;
    from := !from + rcvd;
    rcvd
  in
  reader

let test_mk_reader _ =
  let b_source = Bytes.of_string "0123456789ab" in
  let b_work = Bytes.of_string "--------" in
  let reader = mk_reader b_source in
  let recvd = reader b_work 0 3 in
  assert_equal 3 recvd ~printer:string_of_int;
  assert_equal (Bytes.of_string "012-----") b_work ~printer:Bytes.to_string;
  let recvd = reader b_work 2 6 in
  assert_equal 6 recvd ~printer:string_of_int;
  assert_equal (Bytes.of_string "01345678") b_work ~printer:Bytes.to_string;
  let recvd = reader b_work 2 10 in
  assert_equal 3 recvd ~printer:string_of_int;
  assert_equal (Bytes.of_string "019ab678") b_work ~printer:Bytes.to_string

let test_be _ =
  let i = 70000 (* hex 01 11 70 *)
  and msb = char_of_int 0x11
  and lsb = char_of_int 0x70 in
  assert_equal msb (msb16_of_int i);
  assert_equal lsb (lsb16_of_int i);
  assert_equal (i mod (256 * 256)) (int_of_i16be msb lsb)

let test_compose_msg _ =
  let msg = Bytes.of_string "0123456789ab"
  and seq = 16395
  and res = Bytes.of_string "!M\064\011\000\0120123456789ab" in
  assert_equal res (compose_msg seq msg) ~printer:Bytes.to_string

let ack_handler seq = Core.eprintf "== ack_handler seq %d\n" seq
let msg_handler _buf seq = Core.eprintf "== msg_handler seq %d\n" seq

let test_net_fsm_full_msg _ =
  let msg = Bytes.of_string "!M\000\010\000\0200123456789abcdefghij" in
  let reader = mk_reader msg in
  let fsm = net_msg_fsm reader ack_handler msg_handler in
  match fsm() with
  | Some res -> assert_equal msg res ~printer:Bytes.to_string
  | _ -> assert_failure "not Bytes(res)"

let test_net_fsm_splited_msg _ = ()

let suite =
  "chat_msg tests"
  >::: [
         "mk_reader tool test" >:: test_mk_reader;
         "Big endian 16 bit <-> int" >:: test_be;
         "Msg compose" >:: test_compose_msg;
         "fsm full msg" >:: test_net_fsm_full_msg;
         "fsm splited msg" >:: test_net_fsm_splited_msg;
       ]

let _ = run_test_tt_main suite
