type tokenData = TokenData of ( string * int * int)

type numLiteral = Int   of (tokenData * int)
                | Float of (tokenData * float)

type constantLiteral = NumLiteral of numLiteral
                     | String of (tokenData * string)

type identifier = Identifier of (tokenData * string)

type url = URL of (tokenData * string)

type desc = Desc of (tokenData * string)

type mime = MIME of (tokenData * string)

type required = Required
              | Optional

type compoundType = RangeType     of (tokenData * rangableType * numLiteral * numLiteral)
                  | EnumType      of (tokenData * primitiveType * constantLiteral list)

 and primitiveType = RangableType of rangableType
                   | T_STRING     of tokenData
                   | T_BYTE       of tokenData
                   | T_BOOLEAN    of tokenData
                   | T_DATE       of tokenData
                   | T_DATETIME   of tokenData
                   | T_VOID       of tokenData

 and rangableType = T_INT         of tokenData 
                  | T_LONG        of tokenData 
                  | T_FLOAT       of tokenData 
                  | T_DOUBLE      of tokenData 

type swgtype = ModelType     of modelType
             | CompoundType  of compoundType
             | PrimitiveType of primitiveType
             | ArrayType     of arrayType

 and varDef = VarDef of (tokenData * identifier * swgtype * required)
 
 and propertyDef = PropertyDef of (tokenData * varDef * desc)

 and modelType = ModelRef of modelRef
               | ModelDef of modelDef

 and modelRef = tokenData * identifier * varDef list

 and modelDef = tokenData * identifier * propertyDef list

 and arrayType = SWGSet   of (tokenData * swgtype)
               | SWGArray of (tokenData * swgtype)

type paramType = PATH    of tokenData
               | BODY    of tokenData
               | QUERY   of tokenData
               | HEADER  of tokenData
               | FORM    of tokenData

type httpMethod = GET    of tokenData
                | POST   of tokenData 
                | PUT    of tokenData 
                | DELETE of tokenData     
                | HEAD   of tokenData 

type statusCode = StatusCode of (tokenData * int)

type mimeDef = Produces of (tokenData * mime)
             | Consumes of (tokenData * mime)

type paramDef = (tokenData * varDef * paramType * desc)

type response = (tokenData * statusCode * modelRef option * desc)

type operationProp = Method      of (tokenData * httpMethod)
                   | Return      of (tokenData * swgtype)
                   | Summary     of (tokenData * desc)
                   | Notes       of (tokenData * desc)
                   | ResponseMsg of response
                   | ParamDef    of paramDef
                   | LocalMIME   of mimeDef

type operationDef = OperationDef of (tokenData * identifier * operationProp list)

type apiDef = APIDef of (tokenData * url * operationDef list)

type basePath = BasePath of (tokenData * url)

type resourceProps = ResourceProps of (basePath * mimeDef list)

type resourceDef = ResourceDef of (tokenData * url * desc * resourceProps * apiDef list) 

type swgDoc = ResourceDefs of resourceDef list
            | ModelDefs of modelDef list

type sourceFile = SWGSourceFile of swgDoc list
                | EmptyFile

let rangableType_toString = function
  | T_INT    (_) -> "int"
  | T_LONG   (_) -> "long"
  | T_FLOAT  (_) -> "float"
  | T_DOUBLE (_) -> "double"
;;

let primitiveType_toString = function
  | RangableType(n) -> rangableType_toString n 
  | T_STRING    (_) -> "string"
  | T_BYTE      (_) -> "byte"
  | T_BOOLEAN   (_) -> "boolean"
  | T_DATE      (_) -> "date"
  | T_DATETIME  (_) -> "datetime"
  | T_VOID      (_) -> "void"
;;

let compoundType_toString = function
  | RangeType(_, r, _, _) ->
      rangableType_toString r
  | EnumType(_, e, _) ->
      primitiveType_toString e
;;

let rec arrayType_toString = function
  | SWGSet(_, t) -> 
    let typ = swgtype_toString t in
      "set[" ^ typ ^ "]"
  | SWGArray(_, t) -> 
    let typ = swgtype_toString t in
      "array[" ^ typ ^ "]"

and modelRef_toString (_, Identifier(_, id), args) =
    let arg_toString (VarDef(_, Identifier(_, id), t, r)) =
      let var_prefix = match r with
        | Required -> ""
        | Optional -> "?" 
      in
          var_prefix ^ id ^ "=" ^ (swgtype_toString t)
    in 
    let arguments_string =
      match args with
      | [] -> ""
      | arg :: tl ->
              "(" ^ (List.fold_left (fun args_str arg ->  
                                      args_str ^ "," ^ arg_toString arg) 
                                    (arg_toString arg) 
                                    tl) ^ ")"
    in
      id ^ arguments_string

and modelDef_toString (_, Identifier(_, id), _) = id

and modelType_toString = function
  | ModelRef(m) -> modelRef_toString m
  | ModelDef(m) -> modelDef_toString m

and swgtype_toString = function
  | ModelType(m)     -> modelType_toString m
  | CompoundType(c)  -> compoundType_toString c
  | PrimitiveType(p) -> primitiveType_toString p
  | ArrayType(a)     -> arrayType_toString a
