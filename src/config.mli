type t

val init : unit -> t

val apiVersion : t -> string

val swaggerVersion : t -> string

val outputDir : t -> string

val compact : t -> bool

val discoverPaths : t -> string list

val excludedPaths : t -> string list

exception Configuration_error of string