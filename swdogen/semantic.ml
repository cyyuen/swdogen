(*
 *
 * The MIT License (MIT)
 * 
 * Copyright (c) <2013> <colsy2@gmail.com>
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 *)

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
    let () = print_endline "addSom" in
    let () = print_endline path in
    let predefinedSom = Hashtbl.find env.somBinding path in
    let (som', msgpool) = Som.merge_som som predefinedSom in
    let () = Hashtbl.replace env.somBinding path som' in 
      env, msgpool
  else
    let () = Hashtbl.add env.somBinding path som in 
      env, Msgpool.empty

let addResource env path resource =
  if Hashtbl.mem env.somBinding path then
    let () = print_endline "addResource" in
    let () = print_endline path in
    let som = Hashtbl.find env.somBinding path in
    let (som', msgpool) = Som.merge_resource som resource in
    let () = Hashtbl.replace env.somBinding path som' in 
      env, msgpool
  else
    let (som, msgpool) = Som.of_resource resource in
    let () = Hashtbl.add env.somBinding path som in 
      env, msgpool 
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

let createEmptyEnv () = {
  somBinding       = Hashtbl.create 100;
  modelDefBinding  = Hashtbl.create 100;
  modelRefBinding  = Hashtbl.create 100;
}
;;

(*********************
  Msgpool Handling
 *********************)
let addError (TokenData(fname, lnum, cnum)) msg msgpool =
  Msgpool.add_error msgpool fname lnum cnum msg

let addWarning (TokenData(fname, lnum, cnum)) msg msgpool =
  Msgpool.add_warning msgpool fname lnum cnum msg

(*********************
    translate
 *********************)
let rec translateProperty (env, msgpool) 
                          (PropertyDef (pos, VarDef(_, Identifier(_, id), t, _), _)) =
  translateSWGTyp (env, msgpool) t
  
and translateArgument (env, msgpool) (VarDef (pos, Identifier(_, id), t, _)) =
  translateSWGTyp (env, msgpool) t

and translateModelDef (env, msgpool) ((_, _, props) as modelDef) =
  let model = ModelDef modelDef in
  let id = swgtype_toString (ModelType (model)) in
  let env' = addModel env id model in
    List.fold_left translateProperty (env', msgpool) props

and translateModelRef (env, msgpool) ((_, _, args) as modelRef) =
  let model = ModelRef modelRef in
  let id = swgtype_toString (ModelType (model)) in
  let env' = addModel env id model in
    List.fold_left translateArgument (env', msgpool) args
  
and translateModel (env, msgpool) = function
  | ModelDef(m) -> translateModelDef (env, msgpool) m
  | ModelRef(m) -> translateModelRef (env, msgpool) m

and translateSWGTyp (env, msgpool) = function
  | ModelType (m) -> translateModel (env, msgpool) m
  | ArrayType(SWGSet  (pos, ArrayType(_)))
  | ArrayType(SWGArray(pos, ArrayType(_))) ->
      (env, addError pos "nested array/set is not supported." msgpool)
  | ArrayType(SWGSet   (pos, ModelType(m)))
  | ArrayType(SWGArray (pos, ModelType(m))) ->
    translateModel (env, msgpool) m
  | _ -> (env, msgpool)
;;

let checkForPrimitive msgpool pos = function
  | PrimitiveType(_) 
  | CompoundType(_) ->msgpool
  | _ -> addError pos "primitive type required." msgpool
;;

let translateParam (env, msgpool) (pos, varDef, paramType, _) =
  let VarDef (tok', Identifier(_, id), swgtype, required) = varDef in
    (match paramType with
         | PATH(pos) | QUERY(pos) | HEADER(pos) ->
            (env, checkForPrimitive msgpool pos swgtype)
         | _ ->
           translateSWGTyp (env, msgpool) swgtype)
;;

let translateResponse (env, msgpool) (pos, StatusCode(_, code), modelRef, _) =
  (match modelRef with
       | Some modelRef -> translateModelRef (env, msgpool) modelRef
       | None -> (env, msgpool))
;;

let translateReturn (env, msgpool) (pos, swgtyp) =
  translateSWGTyp (env, msgpool) swgtyp
;;

let translateOperationProperty (env, msgpool) = function
  | ParamDef (param) -> translateParam (env, msgpool) param
  | Return (return) -> translateReturn (env, msgpool) return 
  | ResponseMsg (response) -> translateResponse (env, msgpool) response
  | _ -> (env, msgpool)
;;

let translateOperation (env, msgpool) (OperationDef (pos, apiName, properties)) =
  List.fold_left translateOperationProperty (env, msgpool) properties
;;

(**
 * val translateApi : (env * msgpool) -> Ast.apiDef -> (env * msgpool)
 *)
let translateApi (env, msgpool) (APIDef(pos, URL(_, url), operations)) =
  List.fold_left translateOperation (env, msgpool) operations
;;

(**
 * val translateResourceDef : Ast.resourceDef -> (env, msgpool)
 *)
let translateResourceDef (ResourceDef(_, URL(_, path), _, _, apis) as resourceDef) =
  let (env, msgpool) = addResource (createEmptyEnv ()) path resourceDef in
    List.fold_left translateApi (env, msgpool) apis
;;

(**
 * val translateSWGDocs : Ast.swgDoc -> (env, msgpool) list
 *)
let translateSwgDocs = function
  | ResourceDefs (rds) ->
    List.map translateResourceDef rds
  | ModelDefs (mds) -> 
    let translateModelDef_init = 
      translateModelDef ((createEmptyEnv ()), Msgpool.empty) 
    in
      List.map translateModelDef_init mds
;;

(**
 * val translateFile : Ast.sourceFile -> (env, msgpool) list
 *)
let translateFile = function
  | EmptyFile -> []
  | SWGSourceFile (swgDocs) ->
     List.concat (List.map translateSwgDocs swgDocs)
;;

let concatEnv (env', msgpool) env =
  let { 
    somBinding = somBinding;
    modelRefBinding = modelRefBinding;
    modelDefBinding = modelDefBinding
  } = env in
  let (env_r, msgpool') = 
    Hashtbl.fold (fun path som (env, msgpool) -> 
                    let (env', msgpool') = addSom env path som in
                      env', Msgpool.append msgpool msgpool'
                 )
                 somBinding (env', msgpool)
  in
  let env_md =
    Hashtbl.fold (fun id model env -> addModelRef env id model)
                 modelRefBinding env_r
  in
    Hashtbl.fold (fun id model env -> addModelDef env id model)
                 modelDefBinding env_md
    , msgpool'
;;

let addParameterSetRequired pos param =
  addError pos (Printf.sprintf ("parameter %s should set to be required.") param) 

let addParameterRequired pos param =
  addError pos (Printf.sprintf ("parameter %s is required.") param) 

let addPropNotSetWarning pos prop =
  addWarning pos (Printf.sprintf ("%s is not set.") prop)

let addPropNotSetWarningWithDefault pos prop =
  addWarning pos (Printf.sprintf ("%s is not set. default value would be used.") prop)

let paramIsRequired pos operation param =
  if Som.parameter_is_defined operation param then 
    if Som.parameter_is_required operation param then
      Msgpool.empty
    else
      addParameterSetRequired pos param Msgpool.empty
  else 
    addParameterRequired pos param Msgpool.empty

let analysisOperation path operation =
  let extract_params url = 
    let param_regexp = Str.regexp "{\\([A-Za-z][a-zA-Z0-9-]*\\)}" in
    let rec extract url start params =
      (try
        let idx = Str.search_forward param_regexp url start in
        let param = Str.matched_group 1 url in
        let start = idx + String.length param in
          extract url start (param :: params)
      with
      | Not_found -> params)
    in
      extract url 0 []
  in
  let urlparams = extract_params path 
  and Identifier(pos, _) = Som.nickname operation in
  let urlparamAnalyzers = 
    List.map (fun param -> 
                fun () -> 
                  paramIsRequired pos operation param) 
             urlparams
  and setCheker = (fun checker msgpoolFunc prop ->
                    (fun () -> 
                      if checker operation then Msgpool.empty
                      else msgpoolFunc pos prop Msgpool.empty)
                  )
  in
  let methodSetChecker = setCheker Som.method_is_set addPropNotSetWarningWithDefault "@method"
  and notesSetChecker = setCheker Som.notes_is_set addPropNotSetWarning "@notes"
  and summarySetChecker = setCheker Som.summary_is_set addPropNotSetWarning "@summary"
  and returnTypeSetChecker = setCheker Som.returnType_is_set addPropNotSetWarningWithDefault "@return"
  in
  let authChecker = (fun () -> Msgpool.empty) 
  (*  
    TODO: support authorization

      match Som.localAuth operation with
        | Some (AuthApiKey(_, _, Identifier(_, param))) -> 
            fun () ->
              paramIsRequired pos operation param
        | None -> fun () -> Msgpool.empty
  *)
  in
    Msgpool.concat 
      (List.map 
        (fun analyzer -> analyzer ())
        (authChecker :: methodSetChecker :: notesSetChecker :: 
         summarySetChecker :: returnTypeSetChecker :: urlparamAnalyzers))

let analysisApi api =
  let URL(_, path) = Som.path api in
    Msgpool.concat (List.map (analysisOperation path) (Som.operations api))

let analysisSom som =
  Msgpool.concat (List.map analysisApi (Som.apis som))

let analysisEnv env =
  Hashtbl.fold (fun path som msgpool ->
                  Msgpool.append (analysisSom som) msgpool
               )
               env.somBinding Msgpool.empty

(**
 * Translate all Ast.swgDoc into Som and Env first
 *)
let analysis fileLists = 
  let (envList, msgpoollst) = 
    List.split (List.concat (List.map translateFile fileLists)) 
  in
  let (env, msgpool) =  
    List.fold_left concatEnv ((createEmptyEnv ()), Msgpool.concat msgpoollst) envList
  in
  let msgpool' = Msgpool.append msgpool (analysisEnv env) in
  let () = Msgpool.print_warnings msgpool' in
  if Msgpool.contains_error msgpool' then 
    let () = Msgpool.print_errors msgpool' in
      raise Semantic_error
  else
    env
  