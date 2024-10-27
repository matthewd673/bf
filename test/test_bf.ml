open OUnit2
open Bf
open Instr

let test_parse_chars _ =
  let instrs = Parser.parse_chars ['+'; '-'; '<'; '>'; '['; ']'; ','; '.'] in
  assert_equal [Inc 1; Dec 1; Left 1; Right 1; Loop []; Input; Output] instrs
;;

let test_parse_runs _ =
  let instrs = Parser.parse_chars ['+'; '+'; '-'; '-'; '<'; '<'; '>'; '>'] in
  assert_equal [Inc 2; Dec 2; Left 2; Right 2] instrs
;;

let test_parse_interrupted_run _ =
  let instrs = Parser.parse_chars ['+'; '+'; '+'; '-'; '+'; '+'] in
  assert_equal [Inc 3; Dec 1; Inc 2] instrs
;;

let test_parse_run_with_comment _ =
  let instrs = Parser.parse_chars ['+'; '+'; 'h'; 'i'; '+'] in
  assert_equal [Inc 3] instrs
;;

let test_parse_run_with_loop _ =
  let instrs = Parser.parse_chars ['+'; '+'; '['; '+'; '+'; ']'; '+'] in
  assert_equal [Inc 2; Loop [Inc 2]; Inc 1] instrs
;;

let test_parse_loop_body _ =
  let instrs = Parser.parse_chars ['+'; '['; '-'; '['; '+'; '+'; ']'; ']'] in
  assert_equal [Inc 1; Loop [Dec 1; Loop [Inc 2]]] instrs
;;

let test_parse_mismatched_loop_open _ =
  assert_raises (Failure "Unmatched '['") (fun () -> Parser.parse_chars ['['])
;;

let test_parse_mismatched_loop_close _ =
  assert_raises (Failure "Unmatched ']'") (fun () -> Parser.parse_chars [']'])
;;

let suite =
  "Parser tests" >::: [
    "test_parse_chars" >:: test_parse_chars;
    "test_parse_runs" >:: test_parse_runs;
    "test_parse_interrupted_run" >:: test_parse_interrupted_run;
    "test_parse_run_with_comment" >:: test_parse_run_with_comment;
    "test_parse_run_with_loop" >:: test_parse_run_with_loop;
    "test_parse_loop_body" >:: test_parse_loop_body;
    "test_parse_mismatched_loop_open" >:: test_parse_mismatched_loop_open;
    "test_parse_mismatched_loop_close" >:: test_parse_mismatched_loop_close;
  ]
;;

let () = run_test_tt_main suite
