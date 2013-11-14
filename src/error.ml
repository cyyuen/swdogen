type err_list = ErrList of string list

let create () = ErrList []

let err_sprintf fname lnum cnum =
    Printf.sprintf ("[Error] file: %s, line: %d, char: %d: \n \t %s") 
                   fname 
                   lnum 
                   cnum 
;;

let report (ErrList (err_list)) err = ErrList (err :: err_list)

let print_all (ErrList (err_list)) = 
	let lt = List.rev err_list in
		List.iter print_string lt
;; 
