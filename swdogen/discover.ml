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

let isExcluded excluded path = List.mem path excluded

let rec discoverFile excluded file =
  if Sys.is_directory file then
    (if isExcluded excluded file then
      []
    else
      let dir = file in
      let files = Array.fold_left (fun fl file -> 
                                    (Filename.concat dir file) :: fl )
                                  []
                                  (Sys.readdir dir) in
      let filesList = List.map (discoverFile excluded) files in
        List.concat filesList
    )
  else
    [file]
;;

let discover config =
  let candidates = (Config.discoverPaths config)
  and discoverFile = discoverFile (Config.excludedPaths config) in
  let filesList = List.map discoverFile candidates in
    List.concat filesList
;;