type error = Error of (string * int * int * string)

type warning = Warning of (string * int * int * string)

type t = {
  error_list : error list;
  warning_list : warning list; 
}

let empty = {
  error_list = [];
  warning_list = [];
}

let add_warning pool fname lnum cnum msg =
  { pool with warning_list = Warning (fname, lnum, cnum, msg) :: pool.warning_list }

let add_error pool fname lnum cnum msg =
  { pool with error_list = Error (fname, lnum, cnum, msg) :: pool.error_list }

let contains_error pool = 
  0 = (List.length pool.error_list) 

let print_msg msgtyp fname lnum cnum msg =
  Printf.printf ("File %s, line %d, char %d:\n  [%s] %s") fname lnum cnum msgtyp msg

let print_warning (Warning (fname, lnum, cnum, msg)) =
  print_msg "Warning" fname lnum cnum msg

let print_error (Error (fname, lnum, cnum, msg)) = 
  print_msg "Error" fname lnum cnum msg

let print_warnings pool =
  List.iter print_warning pool.warning_list

let print_errors pool =
  List.iter print_error pool.error_list

let print_all pool =
  let () = print_warnings pool in
    print_errors pool

let concat poolList =
  List.fold_left 
    (fun pool pl -> 
      { error_list = pool.error_list @ pl.error_list; 
        warning_list = pool.warning_list @ pool.warning_list
      })
    empty poolList

let append pl pl' = 
  {
    error_list = pl.error_list @ pl'.error_list;
    warning_list = pl.warning_list @ pl'.warning_list;
  } 