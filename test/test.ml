open OUnit2;;

let suite =
"suite">:::
 [ TestParser.tests;
 (* TestCodegen.tests;
  
  TestSemantic.tests;
  TestUrl.tests*)]
;;

let () =
  run_test_tt_main suite
;;