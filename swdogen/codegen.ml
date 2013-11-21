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
let kobj k o = if o = "" then "" else prop k (genObject o) 

(* key - array : "key" : [ arr ] *)
let karr k a = if a = "" then "" else prop k (genArray a)

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
  let enum = List.map genConstantLiteral enumList
  and fst::tail = enumList in
    commaJoin [kval "defaultValue" (genConstantLiteral fst); 
               karr "enum" (commaJoin enum)]
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

let genSwgVersion = kstr "swaggerVersion"

let genApiVersion = kstr "apiVersion"

let genResourcePath = kstr "resourcePath"

let genBasePath = kstr "basePath"

let genProducesList pl = karr "produces" (commaJoin (List.map genStr pl))

let genConsumesList cl = karr "consumes" (commaJoin (List.map genStr cl))

let genPath = kstr "path" 

let genNickname = kstr "nickname"

let genParameters ps = karr "parameters" (commaJoin ps)
  
let genResponses rs = karr "responseMessages" (commaJoin rs)

let genSummary = kstr "summary" 

let genNotes = kstr "notes"

let genDesc = kstr "description"

let genHttpMethod httpMethod =
  let m = match httpMethod with
    | GET   (_) -> "GET"
    | POST  (_) -> "POST"
    | PUT   (_) -> "PUT"
    | DELETE(_) -> "DELETE"
    | HEAD  (_) -> "HEAD"
  in
    kstr "method" m
;;

let genReturnType returnType = genTyp ~is_return:true returnType

let genResponse (_, (StatusCode(_, status)), model, (Desc(_, desc))) =
  let msg = [(kval "code" (string_of_int status)); (kstr "message" desc)] in
  let (msg, model) = (match model with
                          | None -> (msg, None)
                          | Some m -> 
                            let m = swgtype_toString
                              (ModelType (ModelRef (m))) 
                            in
                              (kstr "responseModel" m) :: msg,
                              Some m)
  in
    (genObject (commaJoin msg), model)
;;

let genParamTypLiteral = (function
  | PATH(_) -> "path"   
  | BODY(_) -> "body"
  | QUERY(_) -> "query"  
  | HEADER(_) -> "header" 
  | FORM(_) -> "form")
;;

let genParamTyp typ = 
  kstr "paramType" (genParamTypLiteral typ)
;;

let genAuthorization = (function
  | None -> ""
  | Some (AuthApiKey (_, paramType, Identifier(_, id))) ->
    let typ = kstr "type" id
    and passAs = kstr "passAs" (genParamTypLiteral paramType) in 
    let apiKey = kobj "apiKey" (commaJoin [typ; passAs]) in
      kobj "authorizations" apiKey)

let genRequired required =
  let r = (match required with
    | Required -> "true"
    | Optional -> "false")
  in
    kval "required" r
;;  

let genParameter (_, varDef, paramTyp, Desc(_, desc)) =
  let paramTypStr = genParamTyp paramTyp
  and desc = genDesc desc
  and VarDef (_, Identifier(_, name), typ, required) = varDef in
  let requiredStr = genRequired required
  and paramName = kstr "name" name
  and (typStr, model) = genTyp typ in
    (genObject 
      (commaJoin [paramName; paramTypStr; desc; requiredStr; typStr]), model)
;;

let genOperation operation =
  let nickname = genNickname (Som.nickname operation)
  and notes = genNotes (Som.notes operation)
  and summary = genSummary (Som.summary operation)
  and httpMethod = genHttpMethod (Som.httpMethod operation)
  and produces = genProducesList (Som.localProduces operation)
  and consumes = genConsumesList (Som.localConsumes operation)
  and authorization = genAuthorization (Som.localAuth operation)
  and (params, paramModels) = 
    List.split (List.map genParameter (Som.parameters operation)) 
  and (responses, responseModels) =
    List.split (List.map genResponse (Som.responses operation))
  and (return, returnModel) = genReturnType (Som.returnType operation) in
  let addModel modelSet = (function
    | None -> modelSet
    | Some m -> StringSet.add m modelSet) in
  let parameters = genParameters params
  and responses = genResponses responses in
  let models =
    addModel
      (StringSet.union
        (List.fold_left addModel StringSet.empty paramModels)
        (List.fold_left addModel StringSet.empty responseModels))
      returnModel
  and operation = 
    genObject
      (commaJoin [nickname; notes; summary; httpMethod; return; produces; 
                  consumes; authorization; parameters; responses])
  in
    operation, models
;;

(* val genApi : Som.api -> (string * StringSet.t) *)
let genApi api  = 
  let path = genPath (Som.path api) in
  let (opertionList, modelSetList) = 
    List.split (List.map genOperation (Som.operations api))
  in
  let modelSet = List.fold_left StringSet.union StringSet.empty modelSetList
  and operations = karr "operations" (commaJoin opertionList)in
    (genObject (commaJoin [path; operations]), modelSet)
;;

let genModelId = kstr "id"

let genModelProp (PropertyDef (_, varDef, (Desc(_, desc)))) = 
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


let genResource env apiVersion swgVersion som =
  let path = (Som.resourcePath som) in
  let basePath      = genBasePath (Som.basePath som)
  and resourcePath  = genResourcePath path
  and produces      = genProducesList (Som.globalProduces som)
  and consumes      = genConsumesList (Som.globalConsumes som)
  and authorization = genAuthorization (Som.globalAuth som)
  and resrcDclPath  = genPath path 
  and resrcDclDesc  = genDesc (Som.resourceDesc som) in
  let (apiList, modelSetList) = List.split (List.map genApi (Som.apis som)) in
  let modelSet = List.fold_left StringSet.union StringSet.empty modelSetList in
  let modelList = genModels env modelSet in
  let apis = karr "apis" (commaJoin apiList)
  and models = kobj "models" (commaJoin modelList) in
  let resourceDef = 
    genObject (commaJoin 
                [apiVersion; swgVersion; resourcePath; basePath; 
                 produces; consumes; authorization; apis; models])
  and resourceDecl =
    genObject (commaJoin [resrcDclPath; resrcDclDesc])
  in
    resourceDecl, (path, resourceDef)

let gen config env =
  let soms = getSoms env 
  and apiVersion = genApiVersion (Config.apiVersion config)
  and swgVersion = genSwgVersion (Config.swaggerVersion config) in
  let genResource = genResource env apiVersion swgVersion in
  let (resourceDescList, resourceList) = 
    List.split (List.map genResource soms) 
  in
  let apis = karr "apis" (commaJoin resourceDescList) in
  let resourceDescList = 
    genObject (commaJoin [apiVersion; swgVersion; apis]) 
  in
      resourceDescList, resourceList
