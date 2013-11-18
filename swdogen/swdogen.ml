let main () =
  (* scan the current dir for config file, .swg.conf *)
  let config = Config.init () in
  (* Get all files under the project root except the excluded ones *)
  let files = Discover.discover config in
  (* Parse files into Ast *)
  let asts = Parser.parseFiles files in
  (* Analysis all Asts *)
  let env = Semantic.analysis asts in
  (* Codegen *)
  let (resourceDesc, resourceList) = Codegen.gen config env in
  (* Deploy *)
    Deployer.deploy (Deployer.init config) resourceDesc resourceList
;;

main ()
