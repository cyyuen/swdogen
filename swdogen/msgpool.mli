type t

val empty : t

val add_warning : t -> string -> int -> int -> string -> t

val add_error : t -> string -> int -> int -> string -> t

val contains_error : t -> bool

val print_all : t -> unit

val print_warnings : t -> unit

val print_errors : t -> unit

val concat : t list -> t

val append : t -> t -> t