open Common

(* this is a very small set of tests for the channel interface,
   intended to ensure that EOF conditions on the underlying flow are
   handled properly *)
module Channel = Channel.Make(Fflow)

let err_read = function
  | Ok (`Data ch) -> fail "character %c was returned from Channel.read_char on an empty flow" ch
  | Ok `Eof -> Lwt.return ()
  | Error (`Msg m) -> fail "unexpected error: %s" m

let err_no_exception () = fail "no exception"
let err_wrong_exception e = fail "wrong exception: %s" (Printexc.to_string e)

let test_read_char_eof () =
  let f = Fflow.make () in
  let c = Channel.create f in
  Channel.read_char c >>= err_read

let test_read_line () =
  let input = "I am the very model of a modern major general" in
  let f = Fflow.make ~input:(Fflow.input_string input) () in
  let c = Channel.create f in
  Channel.read_line c >|= function
  | Ok (`Data buf) -> assert_string "read line" input (Cstruct.copyv buf)
  | Ok `Eof -> fail "eof"
  | Error (`Msg m) -> fail "error: %s" m

let test_read_exactly () =
  let input = "I am the very model of a modern major general" in
  let f = Fflow.make ~input:(Fflow.input_string input) () in
  let c = Channel.create f in
  Channel.read_exactly ~len:4 c >|= function
  | Ok (`Data bufs) -> assert_int "wrong length" 4 (Cstruct.(len (concat bufs)))
  | Ok `Eof -> fail "eof"
  | Error (`Msg m) -> fail "error: %s" m

let test_read_until_eof_then_write () =
  let str = "I am the very model of a modern major general" in
  let closed = ref false in
  let output _buf _off len =
    if !closed
    then OUnit.assert_failure "attempted to write after the flow was closed"
    else Lwt.return len in
  let close () =
    closed := true;
    Lwt.return_unit in
  let input = Fflow.input_string str in
  let f = Fflow.make ~close ~input ~output () in
  let c = Channel.create f in
  (* Should read to EOF: *)
  Channel.read_line c >>= fun _ ->
  Channel.write_line c "Even though I've read to EOF, I should be able to write";
  Channel.flush c >|= function
  | Ok () -> ()
  | Error `Closed -> fail "error: closed"
  | Error (`Msg m) -> fail "error: %s" m

let suite = [
  "read_char + EOF" , `Quick, test_read_char_eof;
  "read_line"       , `Quick, test_read_line;
  "read_exactly"    , `Quick, test_read_exactly;
  "write after read EOF", `Quick, test_read_until_eof_then_write;
]
