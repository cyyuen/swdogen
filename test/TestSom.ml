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

open OUnit2;;
open Swdogen.Ast;;

module SS = Swdogen.Som;;
module SM = Swdogen.Msgpool;;

let resource1_resourcePath = URL (TokenData ("", 2, 13), "/resource")
and resource1_basePath = URL (TokenData ("", 3, 13), "http://www.foo.com")
and resource1_resourceDesc = Desc (TokenData ("", 2, 23), "operation about resource")
and resource1_apiPath = URL (TokenData ("", 4, 8), "/api");
and resource1_nickname = Identifier (TokenData ("", 5, 14), "operation")
and resource1_summary = Desc (TokenData ("", 6, 12), "summary")
and resource1_httpMethod = GET (TokenData ("", 7, 11))
and resource1_notes = Desc (TokenData ("", 8, 10), "notes")
and resource1_return = PrimitiveType (T_VOID (TokenData ("", 10, 11)))
and resource1_param = (TokenData ("", 9, 3), VarDef (TokenData ("", 9, 10), Identifier (TokenData ("", 9, 10), "param"), PrimitiveType (T_STRING (TokenData ("", 9, 16))), Required), QUERY (TokenData ("", 9, 23)), Desc (TokenData ("", 9, 30), "this is a bar"))
and resource1_response = (TokenData ("", 11, 3), StatusCode (TokenData ("", 11, 13), 400), None, Desc (TokenData ("", 11, 17), "response"))
;;

let resource1 =
  ResourceDef
    (TokenData ("", 2, 3),
     resource1_resourcePath,
     resource1_resourceDesc,
     ResourceProps
     (BasePath
       (TokenData ("", 3, 3),
        resource1_basePath),
      None, []),
     [APIDef
        (TokenData ("", 4, 3),
         resource1_apiPath,
         [OperationDef
           (TokenData ("", 5, 3),
            resource1_nickname,
            [Summary
              (TokenData ("", 6, 3),
               resource1_summary); 
             Method
              (TokenData ("", 7, 3),
               resource1_httpMethod);
             Notes
              (TokenData ("", 8, 3),
               resource1_notes); 
             ParamDef
              resource1_param;
             Return
              (TokenData ("", 10, 3),
               resource1_return);
             ResponseMsg
              resource1_response])])])

let test_som1 txt = 
  let (som, msgpool) = SS.of_resource resource1 in
  let () = assert_bool "The msgpool should be empty but not." (SM.is_empty msgpool) in
  (* resource info test *)
  let () = assert_equal ~msg:"resource path test failed" resource1_resourcePath (SS.resourcePath som)
  and () = assert_equal ~msg:"resource desc test failed" resource1_resourceDesc (SS.resourceDesc som)
  and () = assert_equal ~msg:"reosurce base path test failed" (Some resource1_basePath) (SS.basePath som) 
  and () = assert_equal ~msg:"global auth test failed" None (SS.globalAuth som)
  and () = assert_equal ~msg:"global produces test failed" [] (SS.globalProduces som)
  and () = assert_equal ~msg:"global consumes test failed" [] (SS.globalConsumes som)
  in
  (* api test *)
  let apis = SS.apis som in
  let () = assert_equal ~msg:"should only contain 1 api" 1 (List.length apis) in
  let api = List.hd apis in
  let () = assert_equal resource1_apiPath (SS.path api) in
  (* operation test *)
  let operations = SS.operations api in
  let () = assert_equal ~msg:"should only contain 1 operation" 1 (List.length operations) in
  let operation = List.hd operations in
  let () = assert_equal ~msg:"operation name test failed"        resource1_nickname (SS.nickname operation) 
  and () = assert_equal ~msg:"operation summary test failed"     (Some resource1_summary) (SS.summary operation) 
  and () = assert_equal ~msg:"operation notes test failed"       (Some resource1_notes) (SS.notes operation) 
  and () = assert_equal ~msg:"operation return type test failed" resource1_return (SS.returnType operation) 
  and () = assert_equal ~msg:"operation http method test failed" resource1_httpMethod (SS.httpMethod operation)
  and () = assert_equal ~msg:"local auth test failed"            None (SS.localAuth operation)
  and () = assert_equal ~msg:"local produces test failed"        [] (SS.localProduces operation)
  and () = assert_equal ~msg:"local consumes test failed"        [] (SS.localConsumes operation)
  in
  (* parameters test *)
  let parameters = SS.parameters operation in
  let () = assert_equal ~msg:"should only contain 1 parameter" 1 (List.length parameters) in
  let parameter = List.hd parameters in
  let () = assert_equal ~msg:"operation parameter test failed" resource1_param parameter
  in
  (* response test *)
  let responses = SS.responses operation in
  let () = assert_equal ~msg:"should only contain 1 response message" 1 (List.length responses) in
  let response = List.hd responses in
  let () = assert_equal ~msg:"operation response test failed" resource1_response response
  in 
    ()

let tests =
"test_som" >:::
 ["test_som1" >:: test_som1;]
;;

