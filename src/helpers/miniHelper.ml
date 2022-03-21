open Format

let print_string fmt s =
  pp_print_string fmt s
let print_string' s fmt = print_string fmt s

let print_param = print_string

let production_begin fmt =
  fprintf fmt "@[<hov 0>"
let production_end fmt =
  fprintf fmt "@]"

let enclose print op cl fmt =
  fprintf fmt "%t%t%t"
    op print cl

let par e print fmt =
  (if e then enclose print (print_string' "(") (print_string' ")") else print)
    fmt

let print_rule_name_with opening closing print_params fmt name =
  fprintf fmt "%t%a%t%t" opening print_string name print_params closing

let print_symbol_aux symbols opening closing print_params fmt s =
  let is_def = match Common.Symbols.is_defined s symbols with
    | Some _ -> true
    | None -> false
  in
  let s = match Hashtbl.find_opt Lexer.tokens s with
    | Some a -> if !Options.no_aliases then a else "'" ^ s ^ "'"
    | None -> s
  in
  fprintf fmt "%t%a%t%t"
    (if is_def then opening else fun _ -> ())
    print_string s
    print_params
    (if is_def then closing else fun _ -> ())
