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