open Format

let print_string fmt s = pp_print_string fmt s
let print_string' s fmt = print_string fmt s
let print_param = print_string
let production_begin fmt = fprintf fmt "@[<hov 0>"
let production_end fmt = fprintf fmt "@]"
let enclose print op cl fmt = fprintf fmt "%t%t%t" op print cl

let par e print fmt =
  (if e then enclose print (print_string' "(") (print_string' ")") else print)
    fmt

let print_rule_name_with opening closing print_params fmt name =
  fprintf fmt "%t%a%t%t" opening print_string name print_params closing

let token_name s = Option.value (Hashtbl.find_opt Lexer.tokens s) ~default:s
let token_alias t = Hashtbl.find_opt Lexer.aliases t

let print_symbol_aux symbols opening closing print_params fmt s =
  let is_def =
    match Common.Symbols.is_defined s symbols with
    | Some _ -> true
    | None -> false
  in
  let s =
    let t = token_name s in
    match token_alias t with
    | Some a when not !Options.no_aliases -> "'" ^ a ^ "'"
    | _ -> t
  in
  fprintf fmt "%t%a%t%t"
    (if is_def then opening else fun _ -> ())
    print_string s print_params
    (if is_def then closing else fun _ -> ())
