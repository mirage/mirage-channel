open Lwt.Infix

module F = Mirage_flow_lwt.F

let fail fmt = Fmt.kstrf Alcotest.fail fmt

(* this is a very small set of tests for the channel interface,
   intended to ensure that EOF conditions on the underlying flow are
   handled properly *)
module Channel = Mirage_channel_lwt.Make(F)

let check_eof = function
| Ok (`Data ch) ->
    fail "character %c was returned from Channel.read_char on an empty flow" ch
| Ok `Eof -> Lwt.return ()
| Error e -> fail "unexpected error: %a" Channel.pp_error e

let err_no_exception () = fail "no exception"
let err_wrong_exception e = fail "wrong exception: %s" (Printexc.to_string e)

let test_read_char_eof () =
  let f = F.make () in
  let c = Channel.create f in
  Channel.read_char c >>=
  check_eof

let test_read_line () =
  let input = "I am the very model of a modern major general" in
  let f = F.make ~input:(F.input_string input) () in
  let c = Channel.create f in
  Channel.read_line c >|= function
  | Ok (`Data buf) -> Alcotest.(check string) "read line" input (Cstruct.copyv buf)
  | Ok `Eof -> fail "eof"
  | Error e -> fail "error: %a" Channel.pp_error e

(* The line is longer than the limit *)
let test_read_line_len () =
  let input = "I am the very model of a modern major general" in
  let f = F.make ~input:(F.input_string input) () in
  let c = Channel.create f in
  Channel.read_line ~len:5 c >|= function
  | Ok (`Data _) -> fail "read a line which was too big"
  | Ok `Eof -> fail "eof"
  | Error `Line_too_long -> ()
  | Error e -> fail "error: %a" Channel.pp_error e

(* The line is shorter than the limit and bounded by \r\n *)
let test_read_line_len2 () =
  let input = "I\r\n am the very model of a modern major general" in
  let f = F.make ~input:(F.input_string input) () in
  let c = Channel.create f in
  Channel.read_line ~len:5 c >|= function
  | Ok (`Data buf) -> Alcotest.(check string) "read line" "I" (Cstruct.copyv buf)
  | Ok `Eof -> fail "eof"
  | Error e -> fail "error: %a" Channel.pp_error e

(* The line is shorter than the limit and bounded by EOF *)
let test_read_line_len3 () =
  let input = "I am the very model of a modern major general" in
  let f = F.make ~input:(F.input_string input) () in
  let c = Channel.create f in
  Channel.read_line ~len:50 c >|= function
  | Ok (`Data buf) -> Alcotest.(check string) "read line" input (Cstruct.copyv buf)
  | Ok `Eof -> fail "eof"
  | Error e -> fail "error: %a" Channel.pp_error e

(* The line is longer than the limit but I can retry *)
let test_read_line_len4 () =
  let input = "I am the very model\n of a modern major general" in
  let f = F.make ~input:(F.input_string input) () in
  let c = Channel.create f in
  begin
    Channel.read_line ~len:5 c >|= function
    | Ok (`Data _) -> fail "read a line which was too big"
    | Ok `Eof -> fail "eof"
    | Error `Line_too_long -> ()
    | Error e -> fail "error: %a" Channel.pp_error e
  end
  >>= fun () ->
  begin
    Channel.read_line ~len:50 c >|= function
    | Ok (`Data buf) -> Alcotest.(check string) "read line" "I am the very model" (Cstruct.copyv buf)
    | Ok `Eof -> fail "eof"
    | Error e -> fail "error: %a" Channel.pp_error e
  end

let test_read_exactly () =
  let input = "I am the very model of a modern major general" in
  let f = F.make ~input:(F.input_string input) () in
  let c = Channel.create f in
  Channel.read_exactly ~len:4 c >|= function
  | Ok (`Data bufs) ->
      Alcotest.(check int) "wrong length" 4 (Cstruct.(len (concat bufs)))
  | Ok `Eof -> fail "eof"
  | Error e -> fail "error: %a" Channel.pp_error e

let test_read_until_eof_then_write () =
  let str = "I am the very model of a modern major general" in
  let closed = ref false in
  let output _buf _off len =
    if !closed
    then Alcotest.fail "attempted to write after the flow was closed"
    else Lwt.return len in
  let close () =
    closed := true;
    Lwt.return_unit in
  let input = F.input_string str in
  let f = F.make ~close ~input ~output () in
  let c = Channel.create f in
  (* Should read to EOF: *)
  Channel.read_line c >>= fun _ ->
  Channel.write_line c "Even though I've read to EOF, I should be able to write";
  Channel.flush c >|= function
  | Ok () -> ()
  | Error `Closed -> fail "error: closed"
  | Error e       -> fail "error: %a" Channel.pp_write_error e

let suite = [
  "read_char + EOF" , `Quick, test_read_char_eof;
  "read_line"       , `Quick, test_read_line;
  "read_line_len"   , `Quick, test_read_line_len;
  "read_line_len2"  , `Quick, test_read_line_len2;
  "read_line_len3"  , `Quick, test_read_line_len3;
  "read_line_len4"  , `Quick, test_read_line_len4;
]
