open Ast

module SS = Set.Make(String)

type operation = {
  nickname : string;
  returnType : Ast.swgtype;
  notes : string;
  summary : string;
  httpMethod : Ast.httpMethod;
  parameters : (string, Ast.paramDef) Hashtbl.t;
  responses : (string, Ast.response) Hashtbl.t;
  localProduces : SS.t;
  localConsumes : SS.t;
  localAuth : Ast.authorization option;
}

type api = {
  path : string;
  operations : (string, operation) Hashtbl.t;
}

type t = {
  resourcePath : string;
  basePath : string;
  resourceDesc : string;
  globalAuth : Ast.authorization option;
  globalProduces : SS.t;
  globalConsumes : SS.t;
  apis : (string , api) Hashtbl.t;
}

let stringSetToList ss = SS.fold (fun elt lst -> elt :: lst) ss []

let hashTblToList tbl = Hashtbl.fold (fun _ elt lst -> elt :: lst) tbl []

let resourcePath t = t.resourcePath

let basePath t = t.basePath

let resourceDesc t = t.resourceDesc

let globalAuth t = t.globalAuth 

let globalProduces t = stringSetToList t.globalProduces

let globalConsumes t = stringSetToList t.globalConsumes

let apis t = hashTblToList t.apis

let path api = api.path

let operations api = hashTblToList api.operations

let nickname operation = operation.nickname

let returnType operation = operation.returnType

let notes operation = operation.notes

let summary operation = operation.summary

let httpMethod operation = operation.httpMethod

let parameters operation = hashTblToList operation.parameters

let responses operation = hashTblToList operation.responses

let localAuth operation = operation.localAuth

let localProduces operation = stringSetToList operation.localProduces

let localConsumes operation = stringSetToList operation.localConsumes


(* default values *)
let fakeTok = (TokenData ("", -1, 0))
let defaultReturnType = (PrimitiveType (T_VOID (fakeTok)))
let defaultHttpMethod = (GET (fakeTok))

let addLocalMime operation = (function
  | Produces(_, MIME(_, p)) -> 
    { operation with localProduces = SS.add p operation.localProduces }
  | Consumes(_, MIME(_, c)) -> 
    { operation with localConsumes = SS.add c operation.localConsumes }
)

let addParameter operation ((_, VarDef(_, Identifier(_, id), _, _), _, _) as param) =
  if Hashtbl.mem operation.parameters id then operation
  else
    let () = Hashtbl.add operation.parameters id param in operation

let addResponse operation ((_, StatusCode (_, c), _, _) as msg) =
  let codestring = string_of_int c in
    if Hashtbl.mem operation.responses codestring then operation
    else
      let () = Hashtbl.add operation.responses codestring msg in operation

let addOperationProp operation = (function
  | Method (_, httpMethod)    -> { operation with httpMethod = httpMethod }
  | Return (_, returnType)    -> { operation with returnType = returnType }
  | Summary(_, Desc(_, smmy)) -> { operation with summary = smmy }
  | Notes (_, Desc(_, notes)) -> { operation with notes = notes }
  | LocalAuth (auth)          -> { operation with localAuth = Some auth }
  | ResponseMsg (response)    -> addResponse operation response
  | ParamDef (parameter)      -> addParameter operation parameter
  | LocalMIME (mime)          -> addLocalMime operation mime
)

let addOperationProps = List.fold_left addOperationProp

let addOperation api (OperationDef (_, Identifier(_, id), props)) =
  if Hashtbl.mem api.operations id then
    let operation = Hashtbl.find api.operations id in
    let () = Hashtbl.replace api.operations id (addOperationProps operation props) in
      api 
  else
    let operation = {
      nickname = id;
      returnType = defaultReturnType;
      notes = "";
      summary = "";
      httpMethod = defaultHttpMethod;
      parameters = Hashtbl.create 7;
      responses = Hashtbl.create 5;
      localAuth = None;
      localProduces = SS.empty;
      localConsumes = SS.empty;
    } in
    let () = Hashtbl.add api.operations id (addOperationProps operation props) in
     api

let addOperations = List.fold_left addOperation

let addGlobalMime resource = (function
  | Produces(_, MIME(_, mime)) -> 
    { resource with globalProduces = SS.add mime resource.globalProduces}
  | Consumes(_, MIME(_, mime)) -> 
    { resource with globalConsumes = SS.add mime resource.globalConsumes})

let addAPI resource (APIDef (_, (URL (_, url)), operations)) = 
  if Hashtbl.mem resource.apis url then
    let api = Hashtbl.find resource.apis url in
    let () = Hashtbl.replace resource.apis url (addOperations api operations) in
      resource 
  else
    let api = {
      path = url;
      operations = Hashtbl.create (List.length operations)
    } in
    let () = Hashtbl.add resource.apis url (addOperations api operations) in
     resource

let addAPIs = List.fold_left addAPI

let addResourceProp resource (ResourceProps (BasePath(_, (URL (_, path))), auth, mimes)) =
  let resource' = List.fold_left addGlobalMime resource mimes in
    { resource' with basePath = path; globalAuth = auth }

let of_resource (ResourceDef (_, URL(_, path), Desc(_, desc), prop, apis)) =
  let resource = {
    resourcePath = path;
    resourceDesc = desc;
    basePath = "";
    globalAuth = None;
    globalProduces = SS.empty;
    globalConsumes = SS.empty;
    apis = Hashtbl.create (List.length apis)   
  } in
    addAPIs (addResourceProp resource prop) apis

let merge_resource resource (ResourceDef (_, URL(_, path), Desc(_, desc), prop, apis)) =
   addAPIs (addResourceProp resource prop) apis

let merge_operation operation operation' =
  let parameters =
    Hashtbl.fold 
      (fun id param params -> 
        if Hashtbl.mem params id then params
        else let () = Hashtbl.add params id param in params)
      operation.parameters
      operation'.parameters
  and responses =
    Hashtbl.fold 
      (fun id response responses -> 
        if Hashtbl.mem responses id then responses
        else let () = Hashtbl.add responses id response in responses)
      operation.responses
      operation'.responses
  in { operation' with
    parameters = parameters;
    responses = responses;
    localProduces = SS.union operation.localProduces operation'.localProduces;
    localConsumes = SS.union operation.localConsumes operation'.localConsumes;
  }

let merge_api api api' = 
  let operations =
    Hashtbl.fold 
      (fun id operation operations -> 
        if Hashtbl.mem operations id then
          let operation' = Hashtbl.find operations id in
          let () = 
            Hashtbl.replace operations id (merge_operation operation operation')
          in operations
        else
          let () = Hashtbl.add operations id operation in operations)
      api.operations
      api'.operations
  in { api' with
    operations = operations
  } 

let merge_som som som' =
  let apis =
    Hashtbl.fold 
      (fun id api apis -> 
        if Hashtbl.mem apis id then
          let api' = Hashtbl.find apis id in
          let () = Hashtbl.replace apis id (merge_api api api') in apis
        else
          let () = Hashtbl.add apis id api in apis)
      som.apis
      som'.apis
  in { som' with 
    apis = apis;
    globalProduces = SS.union som.globalProduces som'.globalProduces;
    globalConsumes = SS.union som.globalConsumes som'.globalConsumes
  }
