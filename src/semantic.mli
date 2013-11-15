type env 

val fetchModelById : env -> string -> Ast.modelType

val fetchModelDefById : env -> string -> Ast.modelDef

(**
    foldResource f init computes (f kN dN ... (f k1 d1 init)...), 
    where k1 ... kN are the keys of all bindings in tbl, and d1 ... dN 
    are the associated values
 *)
val foldResource : (string -> Ast.resourceDef -> 'a -> 'a ) 
                   -> env -> 'a -> 'a

val analysis : Ast.sourceFile list -> env

exception Semantic_error