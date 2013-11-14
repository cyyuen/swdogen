type err_list

val create : unit -> err_list 

val err_sprintf : string (* file name *) 
				  -> int (* line number *)
				  -> int (* colum number *)
				  -> string (* error description *)
				  -> string

val report : err_list -> string -> err_list

val print_all : err_list -> unit