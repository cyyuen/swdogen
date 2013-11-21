type err_list = ErrList of string list

let create () = ErrList []

let err_sprintf fname lnum cnum =
    Printf.sprintf ("File: %s, line: %d, charater: %d: \n  Error: %s\n") 
                   fname 
                   lnum 
                   cnum 
;;

let add (ErrList (err_list)) err = ErrList (err :: err_list)

let print_all (ErrList (err_list)) = 
  let lt = List.rev err_list in
    List.iter print_string lt
;; 

let is_empty = (function
  | ErrList [] -> true
  | _ -> false)

let concat errListList =
	let errStrListList = 
		List.map (fun (ErrList (err_list)) -> err_list) errListList
	in
		ErrList (List.concat errStrListList) 