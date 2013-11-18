open OUnit2;;
open Swdogen.Parser;;

(* Name the test cases and group them together *)
module SP = Swdogen.Parser;;

let tests =
"test_parser">:::
 ["test1">::
 (fun test_ctxt -> 
 	assert_equal 1 1)]
;;