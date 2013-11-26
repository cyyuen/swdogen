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

let config_file = "_swdogen"

type t = Config_t.t = {
  (* version *)
  apiVersion : string;
  (* output *)
  outputDir : string;
  compact : bool;
  (* project *)
  discoverPaths : string list;
  excludedPaths : string list;
}

exception Configuration_error of string

let raise_config_err msg =
  let error_msg = "config error: " ^ msg in
    raise (Configuration_error error_msg)
;;

let apiVersion config = config.apiVersion 

let swaggerVersion config = "1.2"

let outputDir config = config.outputDir

let compact config = config.compact

let discoverPaths config = config.discoverPaths

let excludedPaths config = config.excludedPaths

let init () = 
  try
    let in_channel = open_in config_file in
    let lex_state = Yojson.Safe.init_lexer ()
    and lexbuf = Lexing.from_channel in_channel in
    let config = Config_j.read_t lex_state lexbuf in
    let () = close_in in_channel in
      config
  with
  | Sys_error(_) ->
    let error_msg = Printf.sprintf ("config file %s not found.") config_file in
      raise_config_err error_msg
  | Ag_oj_run.Error(msg) -> raise_config_err msg