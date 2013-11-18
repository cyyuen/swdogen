let parse filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  let ast = Swgparser.single_swg_source_file Swglexer.token lexbuf in
  let () = close_in in_channel in
    ast
