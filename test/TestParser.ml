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

let data_dir = "data"

(*************************************
          helper function 
 *************************************)
type case = (string * sourceFile)

let createTestSuit suit cases =
  let tests = 
    List.map 
      (fun ((case_name, expected): case) ->
        let case = 
          (fun txt -> 
            let fname = Filename.concat data_dir case_name in
              assert_equal expected (Swdogen.Parser.parse fname)
          )
        in
          ("test_" ^ case_name) >:: case
      )
      cases
  in
    suit >::: tests
;;

let makeToken fname lnum cnum = TokenData (fname, lnum, cnum)

(*************************************
      test suit for empty file 
 *************************************)
let emptyFile1 = ("empty_file1", EmptyFile)
and emptyFile2 = ("empty_file2", EmptyFile)
and emptyFile3 = ("empty_file3", EmptyFile)
;;

let test_emptyFiles = 
  createTestSuit "test_emptyFiles" [emptyFile1; emptyFile2; emptyFile3]
;;

(*************************************
      test suit for resource
**************************************)
let resource1 = (
  "resource1",
  let fname = Filename.concat data_dir "resource1" in
  let makeToken = makeToken fname in
    SWGSourceFile
     [ResourceDefs
       [ResourceDef
         ((makeToken 2 3),
          URL
           ((makeToken 2 13), "/resource"),
          Desc
           ((makeToken 2 23),
            "operation about resource"),
          ResourceProps
           (BasePath
             ((makeToken 3 3),
              URL
               ((makeToken 3 13),
                "http://www.foo.com")),
            None, []),
          [APIDef
            ((makeToken 4 3),
             URL
              ((makeToken 4 8), "/api"),
             [OperationDef
               ((makeToken 5 3),
                Identifier
                 ((makeToken 5 14), "operation"),
                [Summary
                  ((makeToken 6 3),
                   Desc
                    ((makeToken 6 12), "summary"));
                 Method
                  ((makeToken 7 3),
                   GET
                    ((makeToken 7 11)));
                 Notes
                  ((makeToken 8 3),
                   Desc
                    ((makeToken 8 10), "notes"));
                 ParamDef
                  ((makeToken 9 3),
                   VarDef
                    ((makeToken 9 10),
                     Identifier
                      ((makeToken 9 10), "param"),
                     PrimitiveType
                      (T_STRING
                        ((makeToken 9 16))),
                     Required),
                   QUERY
                    ((makeToken 9 23)),
                   Desc
                    ((makeToken 9 30),
                     "this is a bar"));
                 Return
                  ((makeToken 10 3),
                   PrimitiveType
                    (T_VOID
                      ((makeToken 10 11))));
                 ResponseMsg
                  ((makeToken 11 3),
                   StatusCode
                    ((makeToken 11 13), 400),
                   None,
                   Desc
                    ((makeToken 11 17),
                     "response"))])])])]]
)
;;

let test_resources =
  createTestSuit "test_resources" [resource1]

(*************************************
      test suit for models
**************************************)
let model1 = (
  "model1",
  let fname = Filename.concat data_dir "model1" in
  let makeToken = makeToken fname in  
  SWGSourceFile
   [ModelDefs
     [((makeToken 4 3),
       Identifier
        ((makeToken 4 10), "Pet"),
       [PropertyDef
         ((makeToken 9 7),
          VarDef
           ((makeToken 9 17),
            Identifier
             ((makeToken 9 17), "id"),
            CompoundType
             (RangeType
               ((makeToken 9 20),
                T_DOUBLE
                 ((makeToken 9 20)),
                Float
                 ((makeToken 9 27), 0.),
                Float
                 ((makeToken 9 33), 100.))),
            Required),
          Desc
           ((makeToken 9 40),
            "Unique identifier for the Pet"));
        PropertyDef
         ((makeToken 14 7),
          VarDef
           ((makeToken 14 17),
            Identifier
             ((makeToken 14 17), "name"),
            PrimitiveType
             (T_STRING
               ((makeToken 14 22))),
            Required),
          Desc
           ((makeToken 14 29),
            "Friendly name of the pet"));
        PropertyDef
         ((makeToken 19 7),
          VarDef
           ((makeToken 19 17),
            Identifier
             ((makeToken 19 17), "category"),
            ModelType
             (ModelRef
               ((makeToken 19 26),
                Identifier
                 ((makeToken 19 26), "Category"),
                [])),
            Required),
          Desc
           ((makeToken 19 35),
            "Category the pet is in"));
        PropertyDef
         ((makeToken 24 7),
          VarDef
           ((makeToken 24 17),
            Identifier
             ((makeToken 24 17), "photoUrls"),
            ArrayType
             (SWGArray
               ((makeToken 24 27),
                PrimitiveType
                 (T_STRING
                   ((makeToken 24 33))))),
            Optional),
          Desc
           ((makeToken 24 48), "Image URLs"));
        PropertyDef
         ((makeToken 29 7),
          VarDef
           ((makeToken 29 17),
            Identifier
             ((makeToken 29 17), "status"),
            CompoundType
             (EnumType
               ((makeToken 29 24),
                T_STRING
                 ((makeToken 29 24)),
                [String
                  ((makeToken 29 31), "available");
                 String
                  ((makeToken 29 43), "pending");
                 String
                  ((makeToken 29 53), "sold")])),
            Optional),
          Desc
           ((makeToken 29 68),
            "pet status in the store"))])]])
;;

let test_models =
  createTestSuit "test_models" [model1]
;;

let mix1 = (
  "mix1",
  let fname = Filename.concat data_dir "mix1" in
  let makeToken = makeToken fname in  
  SWGSourceFile
   [ResourceDefs
     [ResourceDef
       ((makeToken 2 7),
        URL
         ((makeToken 2 17), "/inlineStyle"),
        Desc
         ((makeToken 2 30),
          "operation about inline style"),
        ResourceProps
         (BasePath
           ((makeToken 3 7),
            URL
             ((makeToken 3 17),
              "http://www.foo.com")),
          None, []),
        [APIDef
          ((makeToken 4 7),
           URL
            ((makeToken 4 12), "/foo/foofoo"),
           [OperationDef
             ((makeToken 5 7),
              Identifier
               ((makeToken 5 18), "foofoo"),
              [Summary
                ((makeToken 6 7),
                 Desc
                  ((makeToken 6 16),
                   "this is a foo API"));
               Method
                ((makeToken 7 7),
                 POST
                  ((makeToken 7 15)));
               Notes
                ((makeToken 8 7),
                 Desc
                  ((makeToken 8 14), "notes"));
               ParamDef
                ((makeToken 9 7),
                 VarDef
                  ((makeToken 9 14),
                   Identifier
                    ((makeToken 9 14), "Bar"),
                   ModelType
                    (ModelRef
                      ((makeToken 9 18),
                       Identifier
                        ((makeToken 9 18), "Bar"),
                       [])),
                   Required),
                 BODY
                  ((makeToken 9 22)),
                 Desc
                  ((makeToken 9 28),
                   "this is a bar"));
               Return
                ((makeToken 10 7),
                 ModelType
                  (ModelRef
                    ((makeToken 10 15),
                     Identifier
                      ((makeToken 10 15), "BarBar"),
                     [VarDef
                       ((makeToken 10 22),
                        Identifier
                         ((makeToken 10 22), "bar"),
                        ModelType
                         (ModelRef
                           ((makeToken 10 26),
                            Identifier
                             ((makeToken 10 26),
                              "Bar"),
                            [])),
                        Required)])));
               ResponseMsg
                ((makeToken 11 7),
                 StatusCode
                  ((makeToken 11 17), 400),
                 Some
                  ((makeToken 11 21),
                   Identifier
                    ((makeToken 11 21), "BarBar"),
                   [VarDef
                     ((makeToken 11 28),
                      Identifier
                       ((makeToken 11 28), "bar"),
                      ModelType
                       (ModelRef
                         ((makeToken 11 32),
                          Identifier
                           ((makeToken 11 32),
                            "Bar"),
                          [])),
                      Required)]),
                 Desc
                  ((makeToken 11 37), "bar"))])])])];
    ModelDefs
     [((makeToken 13 7),
       Identifier
        ((makeToken 13 14), "Bar"),
       [PropertyDef
         ((makeToken 15 7),
          VarDef
           ((makeToken 15 17),
            Identifier
             ((makeToken 15 17), "barbar"),
            ModelType
             (ModelRef
               ((makeToken 15 24),
                Identifier
                 ((makeToken 15 24), "BarBar"),
                [])),
            Required),
          Desc
           ((makeToken 15 31), "barbarbar"))]);
      ((makeToken 17 7),
       Identifier
        ((makeToken 17 14), "BarBar"),
       [PropertyDef
         ((makeToken 19 7),
          VarDef
           ((makeToken 19 17),
            Identifier
             ((makeToken 19 17), "bar"),
            PrimitiveType
             (T_STRING
               ((makeToken 19 21))),
            Required),
          Desc
           ((makeToken 19 28), "barbabrabrbarb ab"))])]]
)
;;

let test_mixs =
  createTestSuit "test_mixs" [mix1]
;;

let tests =
"test_parser">:::
 [test_emptyFiles;
  test_resources;
  test_models;
  test_mixs]
;;