let parse filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Swgparser.single_swg_source_file Swglexer.token lexbuf in
  let () = close_in in_channel in
    ast

let parseFiles files =
  let rec awc asts = function
    | [] -> asts
    | file :: rl ->
      try
        let ast = parse file in
          awc (ast :: asts) rl
      with
      | Ast.Invalid_ast ->
          awc asts rl 
  in awc [] files
;;