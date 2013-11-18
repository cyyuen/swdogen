type t

val init : Config.t -> t

val deployResource : t -> string -> string -> unit

val deployResourceDesc : t -> string -> unit 

val deploy : t -> string -> (string * string) list -> unit

exception Deployment_error of string