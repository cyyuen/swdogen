type t

type api

type operation

(* Getters *)

val resourcePath : t -> Ast.url

val basePath : t -> Ast.url option

val resourceDesc : t -> Ast.desc

val globalAuth : t -> Ast.authorization option

val globalProduces : t -> Ast.mime list

val globalConsumes : t -> Ast.mime list

val apis : t -> api list

val path : api -> Ast.url

val operations : api -> operation list

val nickname : operation -> Ast.identifier

val returnType : operation -> Ast.swgtype

val notes : operation -> Ast.desc option

val summary : operation -> Ast.desc option

val httpMethod : operation -> Ast.httpMethod

val parameters : operation -> Ast.paramDef list

val responses : operation -> Ast.response list

val localAuth : operation -> Ast.authorization option

val localProduces : operation -> Ast.mime list

val localConsumes : operation -> Ast.mime list

(* Inquier *)

val method_is_set : operation -> bool

val notes_is_set : operation -> bool

val summary_is_set : operation -> bool

val returnType_is_set : operation -> bool

val parameter_is_defined : operation -> string -> bool

val parameter_is_required : operation -> string -> bool 

(* Translate *)

val of_resource : Ast.resourceDef -> (t * Msgpool.t)

val merge_resource : t -> Ast.resourceDef -> (t * Msgpool.t) 

val merge_som : t -> t -> (t * Msgpool.t)