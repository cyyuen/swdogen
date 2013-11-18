exception Deployment_error of string

type t = {
  outdir : string;
  format : string -> string
}

let init config =
  let outdir = (Config.outputDir config) in
  let format = 
    (if (Config.compact config) then
      (fun ?std:bool x -> x)
    else 
      Yojson.Safe.prettify) 
  in
  let t = { outdir = outdir; 
            format = format 
  } in
  let createAPIDir () = Unix.mkdir t.outdir 0o755 in
  let () = try
            if Sys.is_directory t.outdir then
              ()
            else
              createAPIDir ()
          with
          | _ -> createAPIDir ()
  in t
;;

let trimResourcePath path =
  if path.[0] = '/' then
    String.sub path 1 ((String.length path) - 1) 
  else
    path
;;

let deployResource t resourcePath content =
  let resource = Filename.concat t.outdir (trimResourcePath resourcePath) in
  let out = open_out resource in
  let () = output_string out (t.format content) in
  let () = flush out in
    close_out out
;;

let deployResourceDesc t = deployResource t "api-docs"

let deploy t resourceDesc resourceList =
  let () = deployResourceDesc t resourceDesc in
  let deployResource_t = deployResource t in
    Parmap.pariter (fun (pth, content) -> deployResource_t pth content) (Parmap.L resourceList)