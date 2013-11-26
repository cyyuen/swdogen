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

module MimeSet = Set.Make(struct
  type t = Ast.mime

  let compare (Ast.MIME(_, mime)) (Ast.MIME(_, mime')) =
    String.compare mime mime'
end)

type operation = {
  nickname : Ast.identifier;
  returnType : Ast.swgtype option;
  notes : Ast.desc option;
  summary : Ast.desc option;
  httpMethod : Ast.httpMethod option;
  parameters : (string, Ast.paramDef) Hashtbl.t;
  responses : (string, Ast.response) Hashtbl.t;
  localProduces : MimeSet.t;
  localConsumes : MimeSet.t;
  localAuth : Ast.authorization option;
}

type api = {
  path : Ast.url;
  operations : (string, operation) Hashtbl.t;
}

type t = {
  resourcePath : Ast.url;
  basePath : Ast.url option;
  resourceDesc : Ast.desc;
  globalAuth : Ast.authorization option;
  globalProduces : MimeSet.t;
  globalConsumes : MimeSet.t;
  apis : (string , api) Hashtbl.t;
}

(* default values *)
let fakeTok = (TokenData ("", 0, 0))
let defaultReturnType = (PrimitiveType (T_VOID (fakeTok)))
let defaultHttpMethod = (GET (fakeTok))

let hashTblToList tbl = Hashtbl.fold (fun _ elt lst -> elt :: lst) tbl []

let returnSomeOrDefault default = (function
  | Some a -> a
  | None -> default)

(*****************
    Getters
 *****************)

let resourcePath t = t.resourcePath

let basePath t = t.basePath

let resourceDesc t = t.resourceDesc

let globalAuth t = t.globalAuth 

let globalProduces t = MimeSet.elements t.globalProduces

let globalConsumes t = MimeSet.elements t.globalConsumes

let apis t = hashTblToList t.apis

let path api = api.path

let operations api = hashTblToList api.operations

let nickname operation = operation.nickname

let notes operation = operation.notes

let summary operation = operation.summary

let returnType operation = returnSomeOrDefault defaultReturnType operation.returnType

let httpMethod operation = returnSomeOrDefault defaultHttpMethod operation.httpMethod

let parameters operation = hashTblToList operation.parameters

let responses operation = hashTblToList operation.responses

let localAuth operation = operation.localAuth

let localProduces operation = MimeSet.elements operation.localProduces

let localConsumes operation = MimeSet.elements operation.localConsumes

(*****************
    Inquire
 *****************)

let option_to_bool = (function
  | Some _ -> true
  | None -> false)

let method_is_set operation = option_to_bool operation.httpMethod

let notes_is_set operation = option_to_bool operation.notes

let summary_is_set operation = option_to_bool operation.summary

let returnType_is_set operation = option_to_bool operation.returnType

let parameter_is_defined operation id = Hashtbl.mem operation.parameters id

let parameter_is_required operation id = 
  if parameter_is_defined operation id then
    let (_, VarDef(_, _, _, req), _, _) = Hashtbl.find operation.parameters id in
      (match req with
           | Required -> true
           | Optional -> false)
  else false

(*********************
  Msgpool Handling
 *********************)
let genPropRedefinedMsg = Printf.sprintf ("%s %s is redefined. ")

let addError (TokenData(fname, lnum, cnum)) msg msgpool =
  Msgpool.add_error msgpool fname lnum cnum msg

let addWarning (TokenData(fname, lnum, cnum)) msg msgpool =
  Msgpool.add_warning msgpool fname lnum cnum msg

let addPropHasDefined pos prop name msgpool =
  addWarning pos (Printf.sprintf ("%s %s has defined.") prop name) msgpool

let addPropValueOveride pos prop name msgpool =
  addError pos (Printf.sprintf ("%s %s has defined. would be overrided.") prop name) msgpool

let addPropOveride pos prop msgpool =
  addError pos (Printf.sprintf ("%s has defined. would be overrided.") prop) msgpool

let addCommonMimeSetHasDefined prop (set1: MimeSet.t) (set2: MimeSet.t) = 
  let interSet = MimeSet.inter set1 set2 in
  let commons = MimeSet.elements interSet in 
    List.fold_left (fun msgpool (MIME(pos, mime)) ->
                      addPropHasDefined pos prop mime msgpool) Msgpool.empty commons

(*****************
    Translate
 *****************)

let addLocalMime (operation, msgpool) = (function
  | Produces(_, (MIME(pos, p) as produce)) -> 
    if MimeSet.mem produce operation.localProduces then
      operation, addPropHasDefined pos "@produces" p msgpool
    else
      { operation with localProduces = MimeSet.add produce operation.localProduces },
      msgpool
  | Consumes(_, (MIME(pos, c) as consume)) ->
    if MimeSet.mem consume operation.localConsumes then
      operation, addPropHasDefined pos "@consumes" c msgpool
    else
      { operation with localConsumes = MimeSet.add consume operation.localConsumes },
      msgpool
)

let addParameter (operation, msgpool)
                 ((pos, VarDef(_, Identifier(_, id), _, _), _, _) as param) =
  if Hashtbl.mem operation.parameters id then 
    operation, addPropValueOveride pos "@param" id msgpool
  else
    let () = Hashtbl.add operation.parameters id param in operation, msgpool

let addResponse (operation, msgpool)
                ((pos, StatusCode (_, c), _, _) as msg) =
  let codestring = string_of_int c in
    if Hashtbl.mem operation.responses codestring then 
      operation, addPropValueOveride pos "@response" codestring msgpool
    else
      let () = Hashtbl.add operation.responses codestring msg in 
        operation, msgpool

let addHttpMethod (operation, msgpool) (pos, httpMethod) = 
  let msgpool' = (match operation.httpMethod with
                      | None -> msgpool
                      | Some _ -> addPropOveride pos "@method" msgpool)
  in
    { operation with httpMethod = Some httpMethod }, msgpool' 
  
let addReturnType (operation, msgpool) (pos, returnType) = 
  let msgpool' = (match operation.returnType with
                      | None -> msgpool
                      | Some _ -> addPropOveride pos "@retrun" msgpool)
  in
  { operation with returnType = Some returnType }, msgpool'

let addSummary (operation, msgpool) ((pos, summy)) = 
  let msgpool' = (match operation.summary with
                      | None -> msgpool
                      | Some _ -> addPropOveride pos "@summary" msgpool)
  in
  { operation with summary = Some summy }, msgpool'

let addNotes (operation, msgpool) ((pos, notes)) = 
  let msgpool' = (match operation.notes with
                      | None -> msgpool
                      | Some _ -> addPropOveride pos "@notes" msgpool)
  in
  { operation with notes = Some notes }, msgpool'

let addLocalAuth (operation, msgpool) (AuthApiKey (pos, _, _) as auth) = 
  let msgpool' = (match operation.localAuth with
                      | None -> msgpool
                      | Some _ -> addPropOveride pos "@auth/apiKey" msgpool)
  in
  { operation with localAuth = Some auth }, msgpool'

let addOperationProp (operation, msgpool) = (function
  | Method (httpMethod)    -> addHttpMethod (operation, msgpool) httpMethod
  | Return (returnType)    -> addReturnType (operation, msgpool) returnType
  | Summary (smmy)         -> addSummary    (operation, msgpool) smmy
  | Notes (notes)          -> addNotes      (operation, msgpool) notes
  | LocalAuth (auth)       -> addLocalAuth  (operation, msgpool) auth
  | ResponseMsg (response) -> addResponse   (operation, msgpool) response
  | ParamDef (parameter)   -> addParameter  (operation, msgpool) parameter
  | LocalMIME (mime)       -> addLocalMime  (operation, msgpool) mime
)

let addOperationProps = List.fold_left addOperationProp

let addOperation (api, msgpool) (OperationDef (pos, (Identifier(_, id) as identifier), props)) =
  if Hashtbl.mem api.operations id then
      api, addPropValueOveride pos "operation" id msgpool 
  else
    let operation = {
      nickname = identifier;
      returnType = None;
      notes = None;
      summary = None;
      httpMethod = None;
      parameters = Hashtbl.create 7;
      responses = Hashtbl.create 5;
      localAuth = None;
      localProduces = MimeSet.empty;
      localConsumes = MimeSet.empty;
    } in
    let (operation', msgpool') = addOperationProps (operation,msgpool) props in
    let () = Hashtbl.add api.operations id operation' in
      api, msgpool'

let addOperations = List.fold_left addOperation

let addGlobalMime (resource, msgpool) = function
  | Produces(_, (MIME(pos, mime) as produce)) ->
    if MimeSet.mem produce resource.globalProduces then
      resource, addPropHasDefined pos "@produces" mime msgpool
    else
      {resource with globalProduces = MimeSet.add produce resource.globalProduces}, msgpool
  | Consumes(_, (MIME(pos, mime) as consume)) -> 
    if MimeSet.mem consume resource.globalConsumes then
      resource, addPropHasDefined pos "@consumes" mime msgpool
    else
      {resource with globalConsumes = MimeSet.add consume resource.globalConsumes}, msgpool
  
let addAPI (resource, msgpool) (APIDef (_, (URL (_, url) as path), operations)) = 
  if Hashtbl.mem resource.apis url then
    let api = Hashtbl.find resource.apis url in
    let (api', msgpool') = addOperations (api, msgpool) operations in
    let () = Hashtbl.replace resource.apis url api' in
      resource, msgpool'
  else
    let api = {
      path = path;
      operations = Hashtbl.create (List.length operations)
    } in
    let (api', msgpool') = addOperations (api, msgpool) operations in
    let () = Hashtbl.add resource.apis url api' in
      resource, msgpool'

let addAPIs = List.fold_left addAPI

let addResourceProp (resource, msgpool) 
                    (ResourceProps (
                      BasePath(pos, path), 
                      auth, 
                      mimes)) =
  let (resource', msgpool') = 
    List.fold_left addGlobalMime (resource, msgpool) mimes 
  in
  let msgpool' = 
    (match resource.basePath with
         | Some _ -> addPropOveride pos "@basePath" msgpool'
         | None -> msgpool') in
  let msgpool' = 
    (match (resource.globalAuth, auth) with
         | (Some _, Some auth) -> 
          let AuthApiKey (pos, _, _) = auth in
            addPropOveride pos "@auth/apiKey" msgpool'
         | _ -> msgpool')
  in
    { resource' with basePath = Some path; globalAuth = auth }, msgpool'

let of_resource (ResourceDef (_, path, desc, prop, apis)) =
  let resource = {
    resourcePath = path;
    resourceDesc = desc;
    basePath = None;
    globalAuth = None;
    globalProduces = MimeSet.empty;
    globalConsumes = MimeSet.empty;
    apis = Hashtbl.create (List.length apis)   
  } in
    addAPIs (addResourceProp (resource, Msgpool.empty) prop) apis

let merge_resource resource (ResourceDef (_, _, _, prop, apis)) =
   addAPIs (addResourceProp (resource, Msgpool.empty) prop) apis

let merge_api api api' = 
  let (operations, msgpool) =
    Hashtbl.fold 
      (fun id operation (operations, msgpool) ->
        (* operation can not be redefined *) 
        if Hashtbl.mem operations id then
          let operation = Hashtbl.find operations id in
          let Identifier (pos, _) = operation.nickname in
            operations, addPropValueOveride pos "operation" id msgpool 
        else
          let () = Hashtbl.add operations id operation in 
            operations, msgpool)
      api.operations
      (api'.operations, Msgpool.empty)
  in { api' with
    operations = operations
  }, msgpool

let merge_som som som' =
  let (apis, msgpool) =
    Hashtbl.fold 
      (fun id api (apis, msgpool) -> 
        if Hashtbl.mem apis id then
          let predefinedApi = Hashtbl.find apis id in
          let (api', msgpool') = merge_api api predefinedApi in
          let () = Hashtbl.replace apis id api' in 
            apis, Msgpool.append msgpool msgpool' 
        else
          let () = Hashtbl.add apis id api in 
            apis, msgpool
      )
      som.apis
      (som'.apis, Msgpool.empty)
  in
  let commonProduces = addCommonMimeSetHasDefined "@produces" som.globalProduces som'.globalProduces
  and commonConsumes = addCommonMimeSetHasDefined "@consumes" som.globalConsumes som'.globalConsumes in 
  { 
    som' with 
    apis = apis;
    globalProduces = MimeSet.union som.globalProduces som'.globalProduces;
    globalConsumes = MimeSet.union som.globalConsumes som'.globalConsumes
  }, 
  Msgpool.concat [msgpool; commonProduces; commonConsumes]
