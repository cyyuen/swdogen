let parse filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Swgparser.single_swg_source_file Swglexer.token lexbuf in
  let () = close_in in_channel in
    ast

let parseFiles files =
  let asts = List.map parse files in
  let rec awc asts = function
    | [] -> asts
    | Ast.EmptyFile :: rl -> awc asts rl
    | Ast.SWGSourceFile(_) as ast:: rl -> awc (ast :: asts) rl
  in awc [] asts
;;