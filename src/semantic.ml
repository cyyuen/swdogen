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
    | _ -> 
      let () = Hashtbl.add apiBinding url_s api in
        bindAPIList apiBinding tl
;;

let mergeAPIList apiBinding apiList =
  let binding = bindAPIList apiBinding apiList in
    Hashtbl.fold (fun k api cl -> api :: cl) binding []
;;

let mergeResource env 
                  (ResourceDef(pos, (URL(_, p) as path), desc, basepath, apiList)) 
                  (ResourceDef(_, URL(_, p'), _, _, apiList')) =
  let apiBinding = Hashtbl.find env.resourceAPIBinding p in
  let apiList = mergeAPIList apiBinding apiList' in
    ResourceDef(pos, path, desc, basepath, apiList)
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

let err_sprintf (TokenData(fname, lnum, cnum)) = 
  Error.err_sprintf fname lnum cnum
;;  

(*********************
    Analysis
 *********************)
let analysisModel (env, errList) model =
  let id = swgtype_toString (ModelType (model)) in
  let env = addModel env id model in
    (env, errList)
;;

let analysisModelDef (env, errList) modelDef =
  let model = ModelDef (modelDef) in
    analysisModel (env, errList) model
;;

let analysisModelRef (env, errList) modelRef =
  let model = ModelRef (modelRef) in
    analysisModel (env, errList) model
;;

let analysisSWGTyp (env, errList) = function
  | ModelType (m) -> analysisModel (env, errList) m
  | ArrayType(SWGSet  (pos, ArrayType(_)))
  | ArrayType(SWGArray(pos, ArrayType(_))) ->
    let err_msg = Printf.sprintf ("nested array/set is not supported.") in
    let err = err_sprintf pos err_msg in
      (env, Error.report errList err)
  | ArrayType(SWGSet   (pos, ModelType(m)))
  | ArrayType(SWGArray (pos, ModelType(m))) ->
    analysisModel (env, errList) m
  | _ -> (env, errList)
;;

let checkForPrimitive errList pos = function
  | PrimitiveType(_) -> errList
  | _ -> 
    let err = err_sprintf pos "primitive type required." in
      Error.report errList err
;;

let checkForURLParameter errList pos paramsTbl param =
  try
    let defined = Hashtbl.find paramsTbl param in
    if defined then
      let err_msg = Printf.sprintf ("parameter %s redefined.") param in
      let err = err_sprintf pos err_msg in
        Error.report errList err
    else
      let () = Hashtbl.replace paramsTbl param true in
        errList  
  with
  | Not_found ->
    let err_msg = Printf.sprintf ("the parameter %s not matched to path.") 
                   param 
    in
    let err = err_sprintf pos err_msg in 
      Error.report errList err
;;

let analysisParam paramsTbl (env, errList) param =
  let (tok, varDef, paramType, _) = param in
  let VarDef (tok', Identifier(_, id), swgtype, required) = varDef in
    match paramType with
        | PATH(pos) -> 
          let errList = checkForPrimitive errList pos swgtype in
          let errList = checkForURLParameter errList pos paramsTbl id in
            (env, errList)
        | QUERY(pos) | HEADER(pos) ->
          let errList = checkForPrimitive errList pos swgtype in
            (env, errList)
        | _ -> analysisSWGTyp (env, errList) swgtype
;;

let analysisOperationProperty paramsTbl (env, errList) = function
  | ParamDef(param) -> 
    analysisParam paramsTbl (env, errList) param
  | Return (_, swgtyp) ->
    analysisSWGTyp (env, errList) swgtyp
  | ResponseMsg (_, _, Some m, _) ->
    analysisModelRef (env, errList) m
  | _ -> (env, errList)
;;

let analysisOperation paramsTbl (env, errList) operation =
  let (OperationDef (tok, apiName, properties)) = operation in
  let analysisFun = analysisOperationProperty paramsTbl in
  let (env, errList) =
    List.fold_left analysisFun (env, errList) properties
  in
    (env, errList)
;;

let analysisApi (env, errList) api =
  let (APIDef(pos, URL(_, url), operations)) = api in
  let urlparams = Url.url_params url in
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
     ( let err_msg = Printf.sprintf ("api %s have undefined url parameter") url in
       let err = err_sprintf pos err_msg in
       let errList = Error.report errList err in
         (env, errList))
;;

let analysisResourceDef (env, errList) resource =
  let (ResourceDef(_, URL(_, path), _, _, apis)) = resource in
  let env = addResource env path resource in
  let (env, errList) = 
    List.fold_left analysisApi (env, errList) apis
  in
    (env, errList)
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
    env
  