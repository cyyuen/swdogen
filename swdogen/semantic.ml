open Ast

exception Semantic_error

(*********************
    Env
 *********************)
type env = {
  (* path -> resource *)
  somBinding : (string, Som.t) Hashtbl.t;
  (* id   -> model *)
  modelRefBinding    : (string, Ast.modelRef) Hashtbl.t;
  (* id   -> model *)
  modelDefBinding : (string, Ast.modelDef) Hashtbl.t;
}
;;

let fetchModelDefById env modelId = Hashtbl.find env.modelDefBinding modelId

let fetchModelById env modelId =
  try
    let modelDef = fetchModelDefById env modelId in
      (ModelDef (modelDef))
  with
  | Not_found ->
    let modelRef = Hashtbl.find env.modelRefBinding modelId in
      (ModelRef (modelRef))
;;

let getSoms env = Hashtbl.fold (fun _ elt lst -> elt :: lst) env.somBinding [] 

let addSom env path som =
  if Hashtbl.mem env.somBinding path then
    let som' = Hashtbl.find env.somBinding path in
    let () = Hashtbl.replace env.somBinding path (Som.merge_som som som') in env
  else
    let () = Hashtbl.add env.somBinding path som in env

let addResource env path resource =
  if Hashtbl.mem env.somBinding path then
    let som = Hashtbl.find env.somBinding path in
    let () = Hashtbl.replace env.somBinding path (Som.merge_resource som resource) in env
  else
    let () = Hashtbl.add env.somBinding path (Som.of_resource resource) in env 
;;

let addModelDef env modelId model =
  if Hashtbl.mem env.modelDefBinding modelId then env
  else
    let () = Hashtbl.add env.modelDefBinding modelId model in env
;;

let addModelRef env modelId model =
  if Hashtbl.mem env.modelRefBinding modelId then env
  else
    let () = Hashtbl.add env.modelRefBinding modelId model in env
;;

let addModel env modelId = function
  | ModelRef (m) -> addModelRef env modelId m
  | ModelDef (m) -> addModelDef env modelId m
;;

let createEnv () = {
  somBinding    = Hashtbl.create 100;
  modelDefBinding    = Hashtbl.create 100;
  modelRefBinding    = Hashtbl.create 100;
}
;;

(*********************
  Error Handling
 *********************)
type msg = Error of (Ast.tokenData * string)
     | Warning of (Ast.tokenData * string)
;;

let err_create_and_add (TokenData(fname, lnum, cnum)) msg errList =
  let err = Error.err_sprintf fname lnum cnum msg in
    Error.add errList err 
;;

type checker = Error.err_list -> Error.err_list

let applyCheck (errList: Error.err_list)
               (checkerList: checker list) =
    List.fold_left (fun errList checker -> checker errList) errList checkerList
;;

(*********************
   Properties Table
**********************)

module PropTbl : sig 
  type t

  val init : int -> int -> t

  val set_singleton_prop : string -> t -> Ast.tokenData ->unit 

  val set_multiple_prop : string -> string -> t ->  Ast.tokenData -> unit

  val singleton_is_set : string -> t -> bool

  val multiple_is_set : string -> t  -> bool

  val singleton_redefined_msg : string -> Ast.tokenData -> string

  val multiple_redefined_msg : string -> string -> Ast.tokenData -> string

  exception Property_redifined of Ast.tokenData

end = struct
  type t = {
    singleton : (string, Ast.tokenData) Hashtbl.t;
    multiple: (string, (string * Ast.tokenData)) Hashtbl.t;
  }

  exception Property_redifined of Ast.tokenData

  let init singleton_size multiple_size = {
    singleton = Hashtbl.create singleton_size;
    multiple = Hashtbl.create multiple_size;
  }

  let set_singleton_prop propName t tok = 
    if Hashtbl.mem t.singleton propName then
      let propTok = Hashtbl.find t.singleton propName in
        raise (Property_redifined (propTok))
    else
      Hashtbl.add t.singleton propName tok

  let set_multiple_prop propName id t tok =
    if Hashtbl.mem t.multiple propName then
      let propToks = Hashtbl.find_all t.multiple propName in
      let () = List.iter (fun (defined_id, tok) -> 
                            if id = defined_id then raise (Property_redifined (tok))
                            else () 
                          ) propToks
      in
        Hashtbl.add t.multiple propName (id, tok)
    else
      Hashtbl.add t.multiple propName (id, tok)

  let singleton_is_set propName t = Hashtbl.mem t.singleton propName

  let multiple_is_set propName t = Hashtbl.mem t.multiple propName 

  let singleton_redefined_msg propName (Ast.TokenData(fname, lnum, cnum)) = 
    Printf.sprintf ("%s is redefined. former definition at %s:%d:%d") 
                   propName fname lnum cnum

  let multiple_redefined_msg propName id (Ast.TokenData(fname, lnum, cnum)) = 
    Printf.sprintf ("%s(%s) is redefined. former definition at %s:%d:%d") 
                   propName id fname lnum cnum

end

(** 
 * helper function. 
 *  setter: should be one of the PropTbl.set_* function
 *  msgGener: should be one of the PropTbl.*_refined_msg function
 *)
let propertySetAndCheck propTbl pos
                         (setter: PropTbl.t -> Ast.tokenData -> unit) 
                         (msgGener: Ast.tokenData -> string)
                         errList =
try
  let () = setter propTbl pos in
    errList
with
| PropTbl.Property_redifined(definedPos) -> 
  let errMsg = msgGener definedPos in
    err_create_and_add pos errMsg errList
;;

(** 
 * helper function. 
 *  set_checker: should be one of the PropTbl.*_is_set function
 *)
let operationCheckSet propTbl pos propName
                      (set_checker: PropTbl.t -> bool)
                      errList =
  if set_checker propTbl then errList
  else
    let errMsg = Printf.sprintf ("%s is not defined") propName in
      err_create_and_add pos errMsg errList
;;

(*********************
    translate
 *********************)
let rec translateProperty (env, errList) 
                     (PropertyDef (pos, VarDef(_, Identifier(_, id), t, _), _)) =
  translateSWGTyp (env, errList) t
  
and translateArgument (env, errList) (VarDef (pos, Identifier(_, id), t, _)) =
  translateSWGTyp (env, errList) t

and translateModelDef (env, errList) ((_, _, props) as modelDef) =
  let model = ModelDef modelDef in
  let id = swgtype_toString (ModelType (model)) in
  let env' = addModel env id model in
    List.fold_left translateProperty (env', errList) props

and translateModelRef (env, errList) ((_, _, args) as modelRef) =
  let model = ModelRef modelRef in
  let id = swgtype_toString (ModelType (model)) in
  let env' = addModel env id model in
    List.fold_left translateArgument (env', errList) args
  
and translateModel (env, errList) = function
  | ModelDef(m) -> translateModelDef (env, errList) m
  | ModelRef(m) -> translateModelRef (env, errList) m

and translateSWGTyp (env, errList) = function
  | ModelType (m) -> translateModel (env, errList) m
  | ArrayType(SWGSet  (pos, ArrayType(_)))
  | ArrayType(SWGArray(pos, ArrayType(_))) ->
    let errMsg = "nested array/set is not supported." in
    let errList' = err_create_and_add pos errMsg errList in
      (env, errList')
  | ArrayType(SWGSet   (pos, ModelType(m)))
  | ArrayType(SWGArray (pos, ModelType(m))) ->
    translateModel (env, errList) m
  | _ -> (env, errList)
;;

let checkForPrimitive errList pos = function
  | PrimitiveType(_) 
  | CompoundType(_) ->errList
  | _ -> err_create_and_add pos "primitive type required." errList
;;

let translateParam (env, errList) (pos, varDef, paramType, _) =
  let VarDef (tok', Identifier(_, id), swgtype, required) = varDef in
    (match paramType with
         | PATH(pos) | QUERY(pos) | HEADER(pos) ->
            (env, checkForPrimitive errList pos swgtype)
         | _ ->
           translateSWGTyp (env, errList) swgtype)
;;

let translateResponse (env, errList) (pos, StatusCode(_, code), modelRef, _) =
  (match modelRef with
       | Some modelRef -> translateModelRef (env, errList) modelRef
       | None -> (env, errList))
;;

let translateReturn (env, errList) (pos, swgtyp) =
  translateSWGTyp (env, errList) swgtyp
;;

let translateOperationProperty (env, errList) = function
  | ParamDef (param) -> translateParam (env, errList) param
  | Return (return) -> translateReturn (env, errList) return 
  | ResponseMsg (response) -> translateResponse (env, errList) response
  | _ -> (env, errList)
;;

let translateOperation (env, errList) (OperationDef (pos, apiName, properties)) =
  List.fold_left translateOperationProperty (env, errList) properties
;;

(**
 * val translateApi : (env * errList) -> Ast.apiDef -> (env * errList)
 *)
let translateApi (env, errList) (APIDef(pos, URL(_, url), operations)) =
  List.fold_left translateOperation (env, errList) operations
;;

(**
 * val translateResourceDef : Ast.resourceDef -> (env, errList)
 *)
let translateResourceDef (ResourceDef(_, URL(_, path), _, _, apis) as resourceDef) =
  let env = createEnv ()
  and errList = Error.create () in
  let env = addResource env path resourceDef in
  let (env', errList') = 
    List.fold_left translateApi (env, errList) apis
  in
    (env', errList')
;;

(**
 * val translateSWGDocs : Ast.swgDoc -> (env, errList) list
 *)
let translateSwgDocs = function
  | ResourceDefs (rds) -> List.map translateResourceDef rds
  | ModelDefs (mds) -> 
    let env = createEnv ()
    and errList = Error.create () in
    let translateModelDef_init = translateModelDef (env, errList) in
      List.map translateModelDef_init mds
;;

(**
 * val translateFile : Ast.sourceFile -> (env, errList) list
 *)
let translateFile = function
  | EmptyFile -> []
  | SWGSourceFile (swgDocs) -> List.concat (List.map translateSwgDocs swgDocs)
;;

let concatEnv env env' =
  let { 
    somBinding = somBinding;
    modelRefBinding = modelRefBinding;
    modelDefBinding = modelDefBinding
  } = env in
  let env_r = 
    Hashtbl.fold (fun path som env -> addSom env path som)
                 somBinding env'
  in
  let env_md =
    Hashtbl.fold (fun id model env -> addModelRef env id model) 
                 modelRefBinding env_r
  in
    Hashtbl.fold (fun id model env -> addModelDef env id model)
                modelDefBinding env_md
;;

(**
 * Translate all Ast.swgDoc into Som and Env first
 *)
let analysis fileLists = 
  let (envList, errListList) = 
    List.split (List.concat (List.map translateFile fileLists)) 
  in
  let errList = Error.concat errListList in
    if Error.is_empty errList then
      List.fold_left concatEnv (createEnv ()) envList
    else
      let () = Error.print_all errList in
        raise Semantic_error
  