open Ast
open Semantic
open Deployer

module StringSet = Set.Make(String)

(*********************
  helper function
 *********************)
let join d strlist =
  let join2 s s' =
    if s = "" then s'
    else if s' = "" then s 
    else s ^ d ^ s'
  in 
    List.fold_left join2 "" strlist
;;

let commaJoin = join "," 

let genObject (objStr) = "{" ^ objStr ^ "}"

let genArray (arrStr) = "[" ^ arrStr ^ "]"

let genStr s = "\"" ^ s ^ "\""

let prop = Printf.sprintf ("\"%s\":%s")

(* key - string : "key" : "string" *)
let kstr k s = prop k (genStr s)

(* key - value : "key" : value *)
let kval k v = prop k v 

(* key - object : "key" : { obj } *)
let kobj k o = prop k (genObject o) 

(* key - array : "key" : [ arr ] *)
let karr k a = prop k (genArray a)

let addToModelSet modelSet = function
  | None -> modelSet
  | Some m -> StringSet.add m modelSet
;;

(*********************
  AST Type codegen
 *********************)

let genRangableType = function
  | T_INT (_) ->
    commaJoin [(kstr "type" "integer"); (kstr "format" "int32")]
    | T_LONG (_) ->
      commaJoin [(kstr "type" "integer"); (kstr "format" "int64")]
    | T_FLOAT (_) ->
      commaJoin [(kstr "type" "number"); (kstr "format" "float")]
    | T_DOUBLE (_) ->
      commaJoin [(kstr "type" "number"); (kstr "format" "double")]
;;

let genPrimitiveType = function
  | RangableType (r) ->
    genRangableType r
    | T_STRING (_) ->
      kstr "type" "string"
    | T_BYTE (_) ->
      commaJoin [(kstr "type" "string"); (kstr "format" "byte")]
    | T_BOOLEAN (_) ->
      kstr "type" "boolean"
    | T_DATE (_) ->
      commaJoin [(kstr "type" "string"); (kstr "format" "date")]
    | T_DATETIME (_) ->
      commaJoin [(kstr "type" "string"); (kstr "format" "date-time")]
    | T_VOID (_) ->
      kstr "type" "void"
;;

let genNumLiteral = function
  | Int (_, i) -> string_of_int i
  (* add a tailing 0 to avoid json format error *)
  | Float (_, f) -> (string_of_float f) ^ "0"
;;

let genConstantLiteral = function
  | NumLiteral (n) -> genNumLiteral n 
  | String (_, s) -> genStr s
;;

let genEnumList enumList =
  let enum = List.map genConstantLiteral enumList in
    karr "enum" (commaJoin enum)
;;

let genCompoundType = function
  | RangeType (_, rtyp, min, max) ->
    let min = kval "mininum" (genNumLiteral min)
    and max = kval "maxinum" (genNumLiteral max)
    and typ = genRangableType rtyp in
      commaJoin [typ; min; max]
  | EnumType (_, ptyp, enumList) ->
    let typ = genPrimitiveType ptyp
    and enumList = genEnumList enumList in
      commaJoin [typ; enumList]
;;

let genModelType is_return modelType =
  let modelId = swgtype_toString (ModelType(modelType)) in
    if is_return then
      (kstr "type" modelId, Some modelId)
    else
      (kstr "$ref" modelId, Some modelId)
;;

let rec genArrayType arrTyp =
  let (unique, typ) = 
    match arrTyp with
        | SWGSet(_, t) -> ("true", t)
        | SWGArray(_, t) -> ("false", t) 
  in
  let arrTyp = kstr "type" "array"
  and (itemTyp, m) = genTyp typ 
  and unique = kval "uniqueItems" unique in
  let items = kobj "items" itemTyp in
    (commaJoin [arrTyp; unique; items], m)

and genTyp ?is_return:(is_return=false) = function
  | ModelType (m) -> genModelType is_return m
  | CompoundType (c) -> (genCompoundType c, None)
  | PrimitiveType (p) -> (genPrimitiveType p, None)
  | ArrayType (a) -> genArrayType a
;;

(*********************
   AST codegen
 *********************)

let genSWGVersion ver = kstr "swaggerVersion" ver

let genAPIVersion ver = kstr "apiVersion" ver

let genBasePath (BasePath (_, URL(_, path))) = kstr "basePath" path

let genResourcePath (URL(_, path)) = kstr "resourcePath" path

let genPath (URL(_, path)) = kstr "path" path 

let genParameters (param) = kstr "parameters" param
  
let genResponses (rep) = karr "responses:" rep

let genHttpMethod (_, httpMethod) =
  let m = match httpMethod with
    | GET   (_) -> "GET"
    | POST  (_) -> "POST"
    | PUT   (_) -> "PUT"
    | DELETE(_) -> "DELETE"
    | HEAD  (_) -> "HEAD"
  in
    kstr "method" m
;;

let genReturnType (_, returnType) = genTyp ~is_return:true returnType

let genSummary (_, (Desc (_, summry))) = kstr "summary" summry

let genNotes (_, (Desc (_, notes))) = kstr "notes" notes

let genResponseMsg (_, (StatusCode(_, status)), model, (Desc(_, desc))) =
  let msg = [(kval "code" (string_of_int status)); (kstr "message" desc)] in
  let (msg, model) = (match model with
                          | None -> (msg, None)
                          | Some m -> 
                            let m = swgtype_toString (ModelType (ModelRef (m))) in
                              (kstr "responseModel" m) :: msg,
                              Some m)
  in
    (genObject (commaJoin msg), model)
;;

let genParamTyp typ = 
  let t = (match typ with
    | PATH(_) -> "path"   
      | BODY(_) -> "body"
      | QUERY(_) -> "query"  
      | HEADER(_) -> "header" 
      | FORM(_) -> "form")
  in
    kstr "paramType" t
;;

let genRequired required =
  let r = (match required with
    | Required -> "true"
    | Optional -> "false")
  in
    kval "required" r
;;  

let genDesc (Desc(_, desc)) = kstr "description" desc

let genParam (_, varDef, paramTyp, desc) =
  let paramTypStr = genParamTyp paramTyp
  and desc = genDesc desc
  and VarDef (_, Identifier(_, name), typ, required) = varDef in
  let requiredStr = genRequired required
  and (typStr, model) = genTyp typ in
    (genObject (commaJoin [paramTypStr; desc; requiredStr; typStr]), model)
;;

let genAPIProp (propList, modelSet, paramList, responsesList) = function
  | Method (httpMethod) ->
    let httpMethod = genHttpMethod httpMethod in
      (httpMethod :: propList, modelSet, paramList, responsesList)
  | Return (return) ->
    let (return, model) = genReturnType return in
    let modelSet = addToModelSet modelSet model in
      (return :: propList, modelSet, paramList, responsesList)
  | Summary (s) -> 
    ((genSummary s) :: propList, modelSet, paramList, responsesList)
  | Notes (n) ->
    ((genNotes n) :: propList, modelSet, paramList, responsesList)
  | ResponseMsg (r) ->
    let (response, model) = genResponseMsg r in
    let modelSet = addToModelSet modelSet model in
      (propList, modelSet, paramList, response :: responsesList)
  | ParamDef (p) ->
    let (param, model) = genParam p in
    let modelSet = addToModelSet modelSet model in
      (propList, modelSet, param :: paramList, responsesList)
;;

let genOperation (OperationDef (_, Identifier(_, name), props)) =
  let (propList, models, params, responses) = 
    List.fold_left genAPIProp ([], StringSet.empty, [], []) props
  in 
  let params = karr "parameters" (commaJoin params)
  and responses = karr "responseMessages" (commaJoin responses)
  and nickname = kstr "nickname" name
  and propDefList = commaJoin propList in
    (genObject (commaJoin [nickname; params; responses; propDefList])), models
;;

(* val genApi : Ast.apiDef -> (string * StringSet.t) *)
let genApi (APIDef (_, (URL(_, url)), operations)) = 
  let (opertionList, modelSetList) = List.split (List.map genOperation operations) in
  let modelSet = List.fold_left StringSet.union StringSet.empty modelSetList
  and operations = karr "operations" (commaJoin opertionList)
  and path = kstr "path" url in
    (genObject (commaJoin [path; operations]), modelSet)
;;

let genModelId id = kstr "id" id

let genModelProp (PropertyDef (_, varDef, desc)) = 
  let VarDef (_, Identifier(_, id), swgtype, required) = varDef in
  let (typ, submodel) = genTyp swgtype
  and desc = genDesc desc 
  and required = (match required with
    | Required -> genStr id
    | Optional -> "") in
  let propDef = commaJoin [desc; typ] in
  let prop = kobj id propDef in 
    (prop, required), submodel
;;

let genModel env model =
  let (id, props) = (match model with
    | ModelDef (_, Identifier(_, id), props) -> (id, props)
    | ModelRef (_, Identifier(_, id), args) as modelRef ->
      let model = fetchModelDefById env id in
      let (_, _, props) = model in
      let propTbl = Hashtbl.create (List.length props) in
      let propTbl = List.fold_left 
                      (fun tbl (PropertyDef(_, varDef, _) as prop) ->
                        let VarDef(_, Identifier(_, id), _, _) = varDef in
                        let () = Hashtbl.add tbl id prop in
                          tbl
                      ) 
                      propTbl 
                      props 
      in
      let propTbl = List.fold_left 
                      (fun tbl (VarDef (_, Identifier(_, id), _, _) as vd) ->
                        try
                          let PropertyDef(pos, _, d) = Hashtbl.find tbl id in
                          let prop = PropertyDef(pos, vd, d) in
                          let () = Hashtbl.replace tbl id prop in
                            tbl
                        with
                        | Not_found ->
                          Printf.printf ("waring: property %s not defined") id;
                          (* id not found, ignore now *)
                          tbl 
                      )
                      propTbl
                      args
      in
      let props = Hashtbl.fold (fun id prop l -> prop :: l) 
                               propTbl
                               []
      and id = swgtype_toString (ModelType (modelRef)) in
        (id, props)
  ) in
  let (propDefList, submodelList) = List.split (List.map genModelProp props) in
  let (propList, requiredList) = List.split propDefList in
  let props = kobj "properties" (commaJoin propList)
  and required = karr "required" (commaJoin requiredList)
  and id_s = genModelId id 
  and submodels = List.fold_left addToModelSet StringSet.empty submodelList in
    (kobj id (commaJoin [id_s; required; props]), submodels)
;;  

(* val genModels : env -> StringSet.t -> string list *)
let rec genModels env ?allmodels:(allModels = StringSet.empty) modelSet =
  let modelList = StringSet.fold 
                    (fun modelId modelList -> 
                      let model = fetchModelById env modelId in
                        model :: modelList
                    )
                    modelSet
                    []
  in
  let genModel = genModel env in
  let (modelList, submodelList) = List.split (List.map genModel modelList) in
  let submodelSet = List.fold_left StringSet.union StringSet.empty submodelList
  and allModels = StringSet.union modelSet allModels in
  let newmodels = StringSet.diff submodelSet allModels in
    if StringSet.equal newmodels StringSet.empty then
      modelList
    else
      let models = genModels env ~allmodels:allModels newmodels in
        modelList @ models
;;

let genResource env apiVersion swgVersion
                path (ResourceDef (_, rpath, desc, basePath, apiDefs)) 
                (resourceDescList, resourceList) =
  let (apiList, modelSetList) = List.split (List.map genApi apiDefs) in
  let modelSet = List.fold_left StringSet.union StringSet.empty modelSetList in
  let modelList = genModels env modelSet in
  let apis = karr "apis" (commaJoin apiList)
  and models = kobj "models" (commaJoin modelList)
  and basePath = genBasePath basePath 
  and resourcePath = genResourcePath rpath in
  let resource = genObject (commaJoin [apiVersion; swgVersion; basePath; 
                                       resourcePath; apis; models])
  in
  let path_j = genPath rpath
  and desc_j = genDesc desc in
  let resourceDesc = genObject (commaJoin [path_j; desc_j]) in
    resourceDesc :: resourceDescList, (path, resource) :: resourceList 
;;

let gen config env =
  let apiVersion = genAPIVersion (Config.apiVersion config)
  and swgVersion = genSWGVersion (Config.swaggerVersion config) in
  let genResource = genResource env apiVersion swgVersion in
  let (resourceDescList, resourceList) = foldResource genResource env ([], []) in
  let apis = karr "apis" (commaJoin resourceDescList) in
  let resourceDescList = genObject (commaJoin [apiVersion; swgVersion; apis]) in
      resourceDescList, resourceList
