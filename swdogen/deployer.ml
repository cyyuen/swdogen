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

exception Deployment_error of string

type t = {
  outdir : string;
  format : string -> string
}

let init outdir format =
  let format = 
    (if format then
      (fun ?std:bool x -> x)
    else 
      Yojson.Safe.prettify) 
  in
  let t = { outdir = outdir; 
            format = format 
  } in
  let createAPIDir () = Unix.mkdir t.outdir 0o755 in
  let () = try
            if Sys.is_directory t.outdir then
              ()
            else
              createAPIDir ()
          with
          | _ -> createAPIDir ()
  in t
;;

let trimResourcePath path =
  if path.[0] = '/' then
    String.sub path 1 ((String.length path) - 1) 
  else
    path
;;

let deployResource t resourcePath content =
  let resource = Filename.concat t.outdir (trimResourcePath resourcePath) in
  let out = open_out resource in
  let () = output_string out (t.format content) in
  let () = flush out in
    close_out out
;;

let deployResourceDesc t = deployResource t "api-docs"

let deploy t resourceDesc resourceList =
  let () = deployResourceDesc t resourceDesc in
  let deployResource_t = deployResource t in
    List.iter (fun (pth, content) -> deployResource_t pth content) resourceList