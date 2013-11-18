open Ast

exception Semantic_error

(*********************
    Env
 *********************)
type env = {
  (* path -> resource *)
  resourceBinding : (string, Ast.resourceDef) Hashtbl.t;
  (* id   -> model *)
  modelRefBinding    : (string, Ast.modelRef) Hashtbl.t;
  (* id   -> model *)
  modelDefBinding : (string, Ast.modelDef) Hashtbl.t;
  (* *)
  resourceAPIBinding : (string, (string, Ast.apiDef) Hashtbl.t) Hashtbl.t;
}
;;

let rec bindAPIList apiBinding = function
  | [] -> apiBinding
  | APIDef (_, URL(_, url_s), operations) as api :: tl ->
    try
      let APIDef(pos, url, ops) = Hashtbl.find apiBinding url_s in

      let api = APIDef(pos, url, ops @ operations) in
      let () = Hashtbl.replace apiBinding url_s api in
        bindAPIList apiBinding tl 
    with
    | Not_found -> 
      let () = print_string url_s in
      let () = Hashtbl.add apiBinding url_s api in
        bindAPIList apiBinding tl
;;

let mergeAPIList apiBinding apiList =
  let binding = bindAPIList apiBinding apiList in
    Hashtbl.fold (fun k api cl -> api :: cl) binding []
;;

let mergeResource env 
                  (ResourceDef(pos, (URL(_, p) as path), desc, rsrcProps, apiList)) 
                  (ResourceDef(_, URL(_, p'), _, rsrcProps', apiList')) =
  let apiBinding = Hashtbl.find env.resourceAPIBinding p
  and ResourceProps (basePath, mimeList) = rsrcProps
  and ResourceProps (_, mimeList') = rsrcProps' in
  let mergedApiList = mergeAPIList apiBinding apiList'
  and mergedRscProp = ResourceProps (basePath, mimeList @ mimeList') in
    ResourceDef(pos, path, desc, mergedRscProp, mergedApiList)
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

let foldResource f env init = Hashtbl.fold f env.resourceBinding init

let addAPIBinding env path url api =
  try
    let binding = Hashtbl.find env.resourceAPIBinding path in
      try
        let _ = Hashtbl.find binding url in
          env
      with
      | Not_found ->
        let () = Hashtbl.add binding url api in
        let () = Hashtbl.replace env.resourceAPIBinding path binding in
          env
  with
  | Not_found ->
    let binding = Hashtbl.create 100 in
    let () = Hashtbl.add binding url api in
    let () = Hashtbl.add env.resourceAPIBinding path binding in
      env
;;

let addResource env path resource =
  try
    let resrc = Hashtbl.find env.resourceBinding path in
    let resrc = mergeResource env resrc resource in
    let () = Hashtbl.replace env.resourceBinding path resrc in
      env
  with
  | Not_found ->
    let () = Hashtbl.add env.resourceBinding path resource in
      env
;;

let addModelDef env modelId model =
  try
    let _ = Hashtbl.find env.modelDefBinding modelId in
      env        
  with
  | Not_found -> 
    let () = Hashtbl.add env.modelDefBinding modelId model in
      env
;;

let addModelRef env modelId model =
  try
    let _ = Hashtbl.find env.modelRefBinding modelId in
      env        
  with
  | Not_found -> 
    let () = Hashtbl.add env.modelRefBinding modelId model in
      env
;;

let addModel env modelId = function
  | ModelRef (m) -> addModelRef env modelId m
  | ModelDef (m) -> addModelDef env modelId m
;;

let createEnv () = {
  resourceBinding    = Hashtbl.create 100;
  modelDefBinding    = Hashtbl.create 100;
  modelRefBinding    = Hashtbl.create 100;
  resourceAPIBinding = Hashtbl.create 100
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
    try
      let propTok = Hashtbl.find t.singleton propName in
        raise (Property_redifined (propTok))
    with
    | Not_found -> Hashtbl.add t.singleton propName tok

  let set_multiple_prop propName id t tok =
    try
      let propToks = Hashtbl.find_all t.multiple propName in
      let () = List.iter (fun (defined_id, tok) -> 
                            if id = defined_id then raise (Property_redifined (tok))
                            else () 
                          ) propToks
      in
        Hashtbl.add t.multiple propName (id, tok)
    with
    | Not_found -> Hashtbl.add t.multiple propName (id, tok)

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
  try
    let defined = Hashtbl.find paramsTbl param in
    if defined then
      let errMsg = Printf.sprintf ("parameter %s redefined.") param in
        err_create_and_add pos errMsg errList
    else
      let () = Hashtbl.replace paramsTbl param true in
        errList  
  with
  | Not_found ->
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

let analysisApi path (env, errList) api =
  let (APIDef(pos, URL(_, url), operations)) = api in
  let env = addAPIBinding env path url api 
  and urlparams = Url.url_params url in
  let createParamTbl tbl param =
    (try
      let _ = Hashtbl.find tbl param in tbl
     with
     | Not_found -> 
       let () = Hashtbl.add tbl param false in tbl)
  in
  let paramsTbl = List.fold_left  createParamTbl
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

let analysisResourceDef (env, errList) resource =
  let (ResourceDef(pos, rpath, desc, rsrcProps, apis)) = resource in
  let URL(_, path) = rpath in
  let apiCombine apis = (
    let apiTbl = Hashtbl.create 10 in
    let apiAdd (APIDef (p, url, operations) as api) = (
      let URL(_, path) = url in
      try
        let APIDef (_, _, opers) = Hashtbl.find apiTbl path in
          Hashtbl.replace apiTbl path (APIDef (p, url, operations @ opers))
       with
       | Not_found -> Hashtbl.add apiTbl path api
    ) in
    let () = List.iter apiAdd apis in
      Hashtbl.fold (fun p api apis -> api :: apis) apiTbl []    
  ) in
  let mergedApis = apiCombine apis in
  let env = 
    addResource env path (ResourceDef(pos, rpath, desc, rsrcProps, mergedApis)) 
  in
  let (env', errList') = 
    List.fold_left (analysisApi path) (env, errList) apis
  in
    (env', errList')
;;

let rec analysisResourceList (env, errList) = function
  | [] -> (env, errList)
  | (SWGSourceFile ([])) :: rest ->
    analysisResourceList (env, errList) rest 
  | (SWGSourceFile (ResourceDefs(resourceDefs) :: srcRest)) :: rest ->
    let (env, errList) = 
      List.fold_left analysisResourceDef (env, errList) resourceDefs
    in
      analysisResourceList (env, errList) ((SWGSourceFile (srcRest)) :: rest)
  | (SWGSourceFile (ModelDefs(modelDefs) :: mdlRest)) :: rest ->
    let (env', errList) = 
      List.fold_left analysisModelDef (env, errList) modelDefs
    in
      analysisResourceList (env, errList) ((SWGSourceFile (mdlRest)) :: rest)
;;

let analysis resourceList = 
  let env = createEnv ()
  and errList = Error.create () in
  let (env, errList) = analysisResourceList (env, errList) resourceList in
    if Error.is_empty errList then
      env
    else
      let () = Error.print_all errList in 
        raise Semantic_error
  