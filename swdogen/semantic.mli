(**
 Semantic Module.
 *)

(**
 * Env
 *)
type env 

(**
 Fetch model by model id
 *)
val fetchModelById : env -> string -> Ast.modelType

(**
 Fetch model definition by model id
 *)
val fetchModelDefById : env -> string -> Ast.modelDef

(**
 *)
val getSoms : env -> Som.t list

(**
 Analysis the Ast
 *)
val analysis : Ast.sourceFile list -> env

exception Semantic_error