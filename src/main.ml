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

open Swdogen

let main () =
  let start_time = Sys.time () in
  (* scan the current dir for config file, .swg.conf *)
  let config = Config.init () in
  (* Get all files under the project root except the excluded ones *)
  let files = 
    Discover.discover (Config.discoverPaths config) (Config.ignores config) 
  in
  (* Parse files into Ast *)
  let asts = Parser.parseFiles files in
  (* Analysis all Asts *)
  let env = Semantic.analysis asts in
  (* Codegen *)
  let (resourceDesc, resourceList) = 
    Codegen.gen (Config.apiVersion config) (Config.swaggerVersion config) env 
  in
  (* Deploy *)
  let () = 
    Deployer.deploy 
      (Deployer.init (Config.outputDir config) (Config.compact config)) 
      resourceDesc 
      resourceList 
  in
  let end_time = Sys.time () in
    Printf.printf ("used %f seconds. \n") (end_time -. start_time)
;;

main ()
