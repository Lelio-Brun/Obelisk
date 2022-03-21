open Format

include MiniHelper

let print_header _ _ = ()
let print_footer fmt = fprintf fmt "@."

let def fmt = fprintf fmt " ::= @[<v -2>"
let prod_bar fmt = pp_print_string fmt "| "
let bar fmt = fprintf fmt "@ |@ "
let space fmt = fprintf fmt "@ "
let break fmt = fprintf fmt "@;"
let eps fmt = pp_print_string fmt "epsilon"

let print_rule_name =
  print_rule_name_with (print_string' "<") (print_string' ">")

let rule_begin _ = ()
let rule_end fmt =
  fprintf fmt "@]@;"

let print_symbol symbols =
  print_symbol_aux symbols (print_string' "<") (print_string' ">")

let opt _ print = enclose print (print_string' "[") (print_string' "]")
let plus e print fmt = fprintf fmt "%t%a" (par e print) print_string "+"
let star e print fmt = fprintf fmt "%t%a" (par e print) print_string "*"

let print_sep_list e nonempty print_sep print_x fmt =
  let print fmt =
    fprintf fmt "%t%t%t"
      print_x space
      (star true (fun fmt -> fprintf fmt "%t%t%t" print_sep space print_x))
  in
  (if nonempty then par e else opt false) print fmt
