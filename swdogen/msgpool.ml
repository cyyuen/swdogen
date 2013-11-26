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

module MsgSet = Set.Make(struct 
 type t = string * int * int * string

 let compare (fname,lnum,cnum,content) (fname', lnum', cnum', content') =
    match (String.compare fname fname') with
        | 0 -> (match Pervasives.compare lnum lnum' with
                    | 0 -> (match Pervasives.compare cnum cnum' with
                                | 0 -> String.compare content content'
                                | result -> result)
                    | result -> result)
        | result -> result
end
)

type t = {
  error_set : MsgSet.t;
  warning_set : MsgSet.t; 
}

let empty = {
  error_set = MsgSet.empty;
  warning_set = MsgSet.empty;
}

let add_warning pool fname lnum cnum msg = 
{ pool with
  warning_set = MsgSet.add (fname, lnum, cnum, msg) pool.warning_set
}

let add_error pool fname lnum cnum msg =
{ pool with
  error_set = MsgSet.add (fname, lnum, cnum, msg) pool.error_set
}
  
let contains_error pool = not (MsgSet.is_empty pool.error_set)

let print_msg msgtyp fname lnum cnum msg =
  Printf.printf ("File %s, line %d, char %d:\n  [%s] %s\n\n") fname lnum cnum msgtyp msg

let print_warning (fname, lnum, cnum, msg) =
  print_msg "Warning" fname lnum cnum msg

let print_error (fname, lnum, cnum, msg) = 
  print_msg "Error" fname lnum cnum msg

let print_warnings pool =
  MsgSet.iter print_warning pool.warning_set

let print_errors pool =
  MsgSet.iter print_error pool.error_set

let print_all pool =
  let () = print_warnings pool in
    print_errors pool

let append pl pl' = 
  {
    error_set = MsgSet.union pl.error_set pl'.error_set;
    warning_set = MsgSet.union pl.warning_set pl'.warning_set;
  }

let concat poolList =
  List.fold_left append empty poolList