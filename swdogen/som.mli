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

type t

type api

type operation

(* Getters *)

val resourcePath : t -> Ast.url

val basePath : t -> Ast.url option

val resourceDesc : t -> Ast.desc

val globalAuth : t -> Ast.authorization option

val globalProduces : t -> Ast.mime list

val globalConsumes : t -> Ast.mime list

val apis : t -> api list

val path : api -> Ast.url

val operations : api -> operation list

val nickname : operation -> Ast.identifier

val returnType : operation -> Ast.swgtype

val notes : operation -> Ast.desc option

val summary : operation -> Ast.desc option

val httpMethod : operation -> Ast.httpMethod

val parameters : operation -> Ast.paramDef list

val responses : operation -> Ast.response list

val localAuth : operation -> Ast.authorization option

val localProduces : operation -> Ast.mime list

val localConsumes : operation -> Ast.mime list

(* Inquier *)

val method_is_set : operation -> bool

val notes_is_set : operation -> bool

val summary_is_set : operation -> bool

val returnType_is_set : operation -> bool

val parameter_is_defined : operation -> string -> bool

val parameter_is_required : operation -> string -> bool 

(* Translate *)

val of_resource : Ast.resourceDef -> (t * Msgpool.t)

val merge_resource : t -> Ast.resourceDef -> (t * Msgpool.t) 

val merge_som : t -> t -> (t * Msgpool.t)