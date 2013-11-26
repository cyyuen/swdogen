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

let parse filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  let pos = lexbuf.Lexing.lex_start_p in
  let _ = lexbuf.Lexing.lex_start_p <- { pos with Lexing.pos_fname = filename }
  and _ = lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_fname = filename } in
  let ast = Swgparser.single_swg_source_file Swglexer.token lexbuf in
  let () = close_in in_channel in
    ast

let parseFiles files =
  let asts = List.map parse files in
  let rec awc asts = function
    | [] -> asts
    | Ast.EmptyFile :: rl -> awc asts rl
    | Ast.SWGSourceFile(_) as ast:: rl -> awc (ast :: asts) rl
  in awc [] asts
;;