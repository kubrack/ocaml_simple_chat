open OUnit2
open Chat.Msg

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

let ack_handler _seq = ()
let msg_handler _buf _seq = ()

let test_net_fsm_full_msg _ =
  let msg = Bytes.of_string "!M        \000\0200123456789abcdefghij" in
  let expected = Bytes.of_string "0123456789abcdefghij" in
  let reader = mk_reader msg in
  let fsm = net_msg_fsm reader ack_handler msg_handler in
  match fsm() with
  | Some res -> assert_equal expected res ~printer:Bytes.to_string
  | _ -> assert_failure "not Bytes(res)"

let suite =
  "chat_msg tests"
  >::: [
         "mk_reader tool test" >:: test_mk_reader;
         "fsm full msg" >:: test_net_fsm_full_msg;
       ]

let _ = run_test_tt_main suite
