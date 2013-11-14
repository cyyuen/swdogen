let parseFiles files =
  let rec awc asts = function
    | [] -> asts
    | file :: rl ->
      try
        let ast = Parser.parse file in
          awc (ast :: asts) rl
      with
      | Ast.Invalid_ast ->
          awc asts rl 
  in awc [] files
;;
    

let main () =
    (* scan the current dir for config file, .swg.conf *)
    let config = Config.init () in
    (* Get all files under the project root except the excluded ones *)
    let files = Discover.discover config in
    (* Parse files into Ast *)
    let asts = parseFiles files in
    (* Analysis all Asts *)
    let env = Semantic.analysis asts in
    (* Codegen *)
    let (resourceDesc, resourceList) = Codegen.gen config env in
    (* Deploy *)
      Deployer.deploy (Deployer.init config) resourceDesc resourceList
;;

main ()
