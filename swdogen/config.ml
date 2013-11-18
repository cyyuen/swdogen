let config_file = "_swdogen"

type t = Config_t.t = {
  (* version *)
  apiVersion : string;
  (* output *)
  outputDir : string;
  compact : bool;
  (* project *)
  discoverPaths : string list;
  excludedPaths : string list;
}

exception Configuration_error of string

let raise_config_err msg =
  let error_msg = "config error: " ^ msg in
    raise (Configuration_error error_msg)
;;

let apiVersion config = config.apiVersion 

let swaggerVersion config = "1.2"

let outputDir config = config.outputDir

let compact config = config.compact

let discoverPaths config = config.discoverPaths

let excludedPaths config = config.excludedPaths

let init () = 
  try
    let in_channel = open_in config_file in
    let lex_state = Yojson.Safe.init_lexer ()
    and lexbuf = Lexing.from_channel in_channel in
    let config = Config_j.read_t lex_state lexbuf in
    let () = close_in in_channel in
      config
  with
  | Sys_error(_) ->
    let error_msg = Printf.sprintf ("config file %s not found.") config_file in
      raise_config_err error_msg
  | Ag_oj_run.Error(msg) -> raise_config_err msg