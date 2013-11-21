type t

type api

type operation

val resourcePath : t -> string

val basePath : t -> string

val resourceDesc : t -> string

val globalAuth : t -> Ast.authorization option

val globalProduces : t -> string list

val globalConsumes : t -> string list

val apis : t -> api list

val path : api -> string

val operations : api -> operation list

val nickname : operation -> string

val returnType : operation -> Ast.swgtype

val notes : operation -> string

val summary : operation -> string

val httpMethod : operation -> Ast.httpMethod

val parameters : operation -> Ast.paramDef list

val responses : operation -> Ast.response list

val localAuth : operation -> Ast.authorization option

val localProduces : operation -> string list

val localConsumes : operation -> string list

val of_resource : Ast.resourceDef -> t

val merge_resource : t -> Ast.resourceDef -> t 

val merge_som : t -> t -> t