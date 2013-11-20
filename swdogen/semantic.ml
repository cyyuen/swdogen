open Ast
open Parmap

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
    Analysis
 *********************)
let rec analysisProperty (env, errList, propTbl) 
                     (PropertyDef (pos, VarDef(_, Identifier(_, id), t, _), _)) =
  let setter = PropTbl.set_multiple_prop "property" id
  and msgGener = PropTbl.multiple_redefined_msg "property" id in
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
  let (env', errList'') = analysisSWGTyp (env, errList') t in
    (env', errList'', propTbl)

and analysisArgument (env, errList, propTbl)
                     (VarDef (pos, Identifier(_, id), t, _)) =
  let setter = PropTbl.set_multiple_prop "property" id
  and msgGener = PropTbl.multiple_redefined_msg "property" id in
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
  let (env', errList'') = analysisSWGTyp (env, errList') t in
    (env', errList'', propTbl)

and analysisModelDef (env, errList) ((_, _, props) as modelDef) =
  let model = ModelDef modelDef in
  let id = swgtype_toString (ModelType (model)) in
  let env' = addModel env id model
  and propTbl = PropTbl.init 0 1 in
  let (env'', errList', _) =
    List.fold_left analysisProperty (env', errList, propTbl) props
  in
    (env'', errList')

and analysisModelRef (env, errList) ((_, _, args) as modelRef) =
  let model = ModelRef modelRef in
  let id = swgtype_toString (ModelType (model)) in
  let env' = addModel env id model
  and propTbl = PropTbl.init 0 1 in
  let (env'', errList', _) =
    List.fold_left analysisArgument (env', errList, propTbl) args
  in
    (env'', errList')

and analysisModel (env, errList) = function
  | ModelDef(m) -> analysisModelDef (env, errList) m
  | ModelRef(m) -> analysisModelRef (env, errList) m

and analysisSWGTyp (env, errList) = function
  | ModelType (m) -> analysisModel (env, errList) m
  | ArrayType(SWGSet  (pos, ArrayType(_)))
  | ArrayType(SWGArray(pos, ArrayType(_))) ->
    let errMsg = "nested array/set is not supported." in
    let errList' = err_create_and_add pos errMsg errList in
      (env, errList')
  | ArrayType(SWGSet   (pos, ModelType(m)))
  | ArrayType(SWGArray (pos, ModelType(m))) ->
    analysisModel (env, errList) m
  | _ -> (env, errList)
;;

let checkForPrimitive errList pos = function
  | PrimitiveType(_) 
  | CompoundType(_) ->errList
  | _ -> err_create_and_add pos "primitive type required." errList
;;

let checkForURLParameter errList pos paramsTbl param =
  if Hashtbl.mem paramsTbl param then
    let defined = Hashtbl.find paramsTbl param in
    if defined then
      let errMsg = Printf.sprintf ("parameter %s redefined.") param in
        err_create_and_add pos errMsg errList
    else
      let () = Hashtbl.replace paramsTbl param true in
        errList  
  else
    let errMsg = Printf.sprintf ("the parameter %s not matched to path.") param
    in
      err_create_and_add pos errMsg errList
;;

let analysisParam paramsTbl (env, errList, propTbl) param =
  let (pos, varDef, paramType, _) = param in
  let VarDef (tok', Identifier(_, id), swgtype, required) = varDef in
  let setter = PropTbl.set_multiple_prop "parameter" id
  and msgGener = PropTbl.multiple_redefined_msg "parameter" id in
  let propertyChecker = propertySetAndCheck propTbl pos setter msgGener in
  let primitiveChecker = 
    (fun errList -> checkForPrimitive errList pos swgtype)
  in 
  let (env', errList', checkerList) =
    (match paramType with
         | PATH(pos) ->
           let urlparamChecker =
             (fun errList -> checkForURLParameter errList pos paramsTbl id)
           in
             (env, errList, [primitiveChecker; urlparamChecker])
         | QUERY(pos) | HEADER(pos) ->
           (env, errList, [primitiveChecker])
         | _ ->
           let (env', errList') = analysisSWGTyp (env, errList) swgtype in
             (env', errList', []))
  in
  let errList'' = applyCheck errList' (propertyChecker :: checkerList) in
    (env', errList'', propTbl)
;;


let analysisResponse (env, errList, propTbl) 
                     (pos, StatusCode(_, code), modelRef, _) =
  let code_s = string_of_int code in
  let setter = PropTbl.set_multiple_prop "response" code_s
  and msgGener = PropTbl.multiple_redefined_msg "response" code_s in
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
  let (env', errList'') = 
    match modelRef with
    | Some modelRef -> analysisModelRef (env, errList') modelRef
    | None -> (env, errList')
  in
    (env', errList'', propTbl)
;;

let analysisMimes (env, errList, propTbl) mimeDef =
  let (pos, setter, msgGener) = (
    match mimeDef with
        | Produces(pos, MIME(_, mime)) ->
          (pos,
           PropTbl.set_multiple_prop "produces" mime,
           PropTbl.multiple_redefined_msg "produces" mime) 
        | Consumes(pos, MIME(_, mime)) ->
          (pos,
           PropTbl.set_multiple_prop "consumes" mime,
           PropTbl.multiple_redefined_msg "consumes" mime)
  ) in
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
    (env, errList', propTbl)
;;

let analysisReturn (env, errList, propTbl) (pos, swgtyp) =
  let setter = PropTbl.set_singleton_prop "return"
  and msgGener = PropTbl.singleton_redefined_msg "return" in
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
  let (env', errList'') = analysisSWGTyp (env, errList') swgtyp in
    (env', errList'', propTbl)
;;

let analysisSummary (env, errList, propTbl) (pos, _) =
  let setter = PropTbl.set_singleton_prop "summary"
  and msgGener = PropTbl.singleton_redefined_msg "summary" in 
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
    (env, errList', propTbl)
;;

let analysisNotes (env, errList, propTbl) (pos, _) =
  let setter = PropTbl.set_singleton_prop "notes"
  and msgGener = PropTbl.singleton_redefined_msg "notes" in 
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
    (env, errList', propTbl)
;;

let analysisMethod (env, errList, propTbl) (pos, _) = 
  let setter = PropTbl.set_singleton_prop "method"
  and msgGener = PropTbl.singleton_redefined_msg "method" in 
  let errList' = propertySetAndCheck propTbl pos setter msgGener errList in
    (env, errList', propTbl)
;;

let analysisOperationProperty paramsTbl (env, errList, propTbl) = function
  | ParamDef (param) -> analysisParam paramsTbl (env, errList, propTbl) param
  | Return (return) -> analysisReturn (env, errList, propTbl) return 
  | ResponseMsg (response) -> analysisResponse (env, errList, propTbl) response
  | Summary (summary) -> analysisSummary (env, errList, propTbl) summary
  | Notes (notes) -> analysisNotes (env, errList, propTbl) notes
  | Method (methd) -> analysisMethod (env, errList, propTbl) methd
  | LocalMIME (mime) -> analysisMimes (env, errList, propTbl) mime
;;

let analysisOperation paramsTbl (env, errList) operation =
  let (OperationDef (pos, apiName, properties)) = operation in
  let analysisFun = analysisOperationProperty paramsTbl
  and propTbl = PropTbl.init 4 3 in
  let (env', errList', propTbl') =
    List.fold_left analysisFun (env, errList, propTbl) properties
  in
  let operationCheckSet = operationCheckSet propTbl pos in
  let methodChecker = 
    operationCheckSet "HTTP method" (PropTbl.singleton_is_set "method")
  and summaryChecker =
    operationCheckSet "summary" (PropTbl.singleton_is_set "summary")
  and notesChecker = 
    operationCheckSet "notes" (PropTbl.singleton_is_set "notes")
  and returnChecker = 
    operationCheckSet "return" (PropTbl.singleton_is_set "return")
  in
  let checkerList = 
    [methodChecker; summaryChecker; notesChecker; returnChecker] 
  in
    (env', (applyCheck errList' checkerList))
;;

let analysisApi (env, errList) api =
  let (APIDef(pos, URL(_, url), operations)) = api in
  let urlparams = Url.url_params url in
  let createParamTbl tbl param = (
    if Hashtbl.mem tbl param then tbl
    else let () = Hashtbl.add tbl param false in tbl
  ) in
  let paramsTbl = List.fold_left createParamTbl
                  (Hashtbl.create 10)
                  urlparams
  in
  let analysisFun = analysisOperation paramsTbl in
  let (env, errList) = List.fold_left analysisFun (env, errList) operations in 
  let allParamDefined = Hashtbl.fold (fun k v d -> v && d)
                                     paramsTbl
                                     true
  in 
    if allParamDefined then (env, errList)
    else 
     let errMsg = Printf.sprintf ("api %s have undefined url parameter") url in
     let errList' = err_create_and_add pos errMsg errList in
       (env, errList')
;;

(**
 * val analysisResourceDef : Ast.resourceDef -> (env, errList)
 *)
let analysisResourceDef (ResourceDef(_, URL(_, path), _, _, apis) as resourceDef) =
  let env = createEnv ()
  and errList = Error.create () in
  let env = addResource env path resourceDef in
  let (env', errList') = 
    List.fold_left analysisApi (env, errList) apis
  in
    (env', errList')
;;

(**
 * val analysisSWGDocs : Ast.swgDoc -> (env, errList) list
 *)
let analysisSwgDocs = function
  | ResourceDefs (rds) -> parmap analysisResourceDef (L rds)
  | ModelDefs (mds) -> 
    let env = createEnv ()
    and errList = Error.create () in
    let analysisModelDef_init = analysisModelDef (env, errList) in
      parmap analysisModelDef_init (L mds)
;;

(**
 * val analysisFile : Ast.sourceFile -> (env, errList) list
 *)
let analysisFile = function
  | EmptyFile -> []
  | SWGSourceFile (swgDocs) -> List.concat (parmap analysisSwgDocs (L swgDocs))
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
 * 
 *)
let analysis fileLists = 
  let (envList, errListList) = 
    List.split (List.concat (parmap analysisFile (L fileLists))) 
  in
  let errList = Error.concat errListList in
    if Error.is_empty errList then
      parfold concatEnv (L envList) (createEnv ()) concatEnv
    else
      let () = Error.print_all errList in
        raise Semantic_error
  