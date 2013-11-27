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

type requestEndPoint = OAuth2RequestEndPoint of (tokenData * identifier * identifier * url)

type tokenEndPoint = OAuth2TokenEndPoint of (tokenData * identifier * url)

type oauthType = OAuth2Implicit of (tokenData * identifier * url)
               | OAuth2AuthCode of (requestEndPoint * tokenEndPoint)

type oauthScope = OAuthScope of identifier list

type authorization = AuthApiKey of (tokenData * paramType * identifier)
                   | OAuth2 of (tokenData * oauthScope * oauthType) 

type paramDef = (tokenData * varDef * paramType * desc)

type response = (tokenData * statusCode * modelRef option * desc)

type operationProp = Method      of (tokenData * httpMethod)
                   | Return      of (tokenData * swgtype)
                   | Summary     of (tokenData * desc)
                   | Notes       of (tokenData * desc)
                   | ResponseMsg of response
                   | ParamDef    of paramDef
                   | LocalMIME   of mimeDef
                   | LocalAuth   of authorization

type operationDef = OperationDef of (tokenData * identifier * operationProp list)

type apiDef = APIDef of (tokenData * url * operationDef list)

type basePath = BasePath of (tokenData * url)

type resourceProps = ResourceProps of (basePath * authorization option * mimeDef list)

type resourceDef = ResourceDef of (tokenData * url * desc * resourceProps * apiDef list) 

type swgDoc = ResourceDefs of resourceDef list
            | ModelDefs of modelDef list

type sourceFile = SWGSourceFile of swgDoc list
                | EmptyFile

val swgtype_toString : swgtype -> string