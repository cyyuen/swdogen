let isExcluded excluded path = List.mem path excluded

let rec discoverFile excluded file =
  if Sys.is_directory file then
    (if isExcluded excluded file then
      []
    else
      let dir = file in
      let files = Array.fold_left (fun fl file -> 
                                    (Filename.concat dir file) :: fl )
                                  []
                                  (Sys.readdir dir) in
      let filesList = List.map (discoverFile excluded) files in
        List.concat filesList
    )
  else
    [file]
;;

let discover config =
  let candidates = (Config.discoverPaths config)
  and discoverFile = discoverFile (Config.excludedPaths config) in
  let filesList = List.map discoverFile candidates in
    List.concat filesList
;;