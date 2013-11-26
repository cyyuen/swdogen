(*
 *
 * The MIT License (MIT)
 * 
 * Copyright (c) <2013> <colsy2@gmail.com>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 *)

open OUnit2;;
open Swdogen.Ast;;

(* Name the test cases and group them together *)
module SP = Swdogen.Parser;;

let emptyFile1 = ("emptyFile1", "")
and emptyFile2 = ("emptyFile2", "/** */")
and emptyFile3 = ("emptyFile3", "/**")
and expectedAst_emptyFile = EmptyFile
;;

let createParserTest expected content =
  (fun txt -> 
    let (fname, out) = bracket_tmpfile txt in
    let () = output_string out content in
    let () = close_out out in
      assert_equal expected (SP.parse fname))

let test_emptyFiles =
  let tests = 
    List.map 
      (fun (tst, content) -> 
        ("test_" ^ tst) >:: (createParserTest expectedAst_emptyFile content)
      )
      [emptyFile1; emptyFile2; emptyFile3]
  in
    "test_emptyFiles" >::: tests
;;

let tests =
"test_parser">:::
 [test_emptyFiles]
;;