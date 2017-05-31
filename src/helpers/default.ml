open GenericPrinter

include MiniHelper

let print_header _ = ()
let print_footer () = print_string "@."

let def = " ::= @[<v -2>"
let prod_bar = "| "
let bar = "@ |@ "
let space = "@ "
let break = "@;"
let eps = "epsilon"

let print_rule_name is_not_fun =
  print_fmt (if is_not_fun then "<%s>" else "%s")

let rule_begin () = ()
let rule_end () =
  print_string "@]@;@;"

let print_symbol is_term is_non_term s print_params =
  print_fmt (if is_non_term then "<%s>" else "%s") s;
  print_params ()

let opt e print = enclose print "[" "]"
let plus e print = par e print; print_string "+"
let star e print = par e print; print_string "*"

let print_sep_list e nonempty print_sep print_x =
  let print () =
    print_x (); print_string space;
    star true (fun () -> print_sep (); print_string space; print_x ())
  in
  if nonempty then par e print else enclose print "[" "]"
