type err_list

val create : unit -> err_list 

val err_sprintf : string (* file name *) 
				  -> int (* line number *)
				  -> int (* colum number *)
				  -> string (* error description *)
				  -> string

val add : err_list -> string -> err_list

val is_empty : err_list -> bool

val print_all : err_list -> unit

val concat : err_list list -> err_list