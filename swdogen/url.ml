open Str

let param_regexp = Str.regexp "{\\([A-Za-z][a-zA-Z0-9-]*\\)}"

let rec extract_params url start params =
	try
		let idx = search_forward param_regexp url start in
		let param = matched_group 1 url in
		let start = idx + String.length param in
			extract_params url start (param :: params)
	with
	| Not_found -> params
;;

let url_params url = extract_params url 0 []