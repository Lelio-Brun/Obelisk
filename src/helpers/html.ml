include MiniHelper

let print_header _ =
  print_string
    "@[<v 0><!DOCTYPE html>@;\
     @[<v 2><html>@;\
     @[<v 2><head>@;\
     <title>Grammar</title>@;\
     @[<v 2><style>@;\
     @[<v 2>.specification {@;\
     list-style: none;@;\
     padding: 0;@;\
     margin: 0;\
     @]@;}@;\
     @[<v 2>.specification li {@;\
     margin-bottom: .5em;\
     @]@;}@;\
     @[<v 2>.prods {@;\
     display: inline;@;\
     list-style: none;@;\
     padding-left: .5em;\
     @]@;}@;\
     @[<v 2>.prods li {@;\
     margin-bottom: 0;\
     @]@;}@;\
     @[<v 2>.prods:before {@;\
     content: \"::= \";\
     @]@;}@;\
     @[<v 2>.prods li {@;\
     padding-left: 2em;\
     @]@;}@;\
     @[<v 2>.prods li:before {@;\
     content: \"| \";\
     @]@;}@;\
     @[<v 2>.nonterminal:before {@;\
     content: \"<\";\
     @]@;}@;\
     @[<v 2>.nonterminal:after {@;\
     content: \">\";\
     @]@;}@;\
     @]</style>@;\
     @]</head>@;@;\
     @[<v 2><body>@;@;\
     @[<v 2><ul class=\"specification\">@;@;"

let print_footer () =
  print_string
    "@]@;</ul>@;\
     @]@;</body>@;\
     @]@;</html>@]@."

let def = "@[<v 2><ul class=\"prods\">"
let prod_bar = "<li>"
let bar = " | "
let space = "@ "
let break = "@;"
let eps = "epsilon"

let print_rule_name is_not_fun =
  print_fmt
    (if is_not_fun then "<span class=\"nonterminal\">%s</span>@;" else "%s@;")
let rule_begin () =
  print_string "@[<v 2><li>"
let rule_end () =
  print_string "@]@;</ul>@]@;</li>@;@;"

let production_begin _ =
  print_string "@[<hov 2>"
let production_end not_sing =
  print_string ((if not_sing then "</li>" else "") ^ "@]")

let print_terminal _ is_non_term =
  print_fmt
    (if is_non_term then "<span class=\"nonterminal\">%s</span>" else "%s")

let enclose print op cl =
  print_string op; print (); print_string cl

let par e print =
  if e then enclose print "(" ")" else print ()

let opt e print = enclose print "[" "]"
let plus e print = par e print; print_string "<sup>+</sup>"
let star e print = par e print; print_string "<sup>*</sup>"

let print_sep_list e nonempty print_sep print_x =
  par e (fun () ->
      print_x ();
      print_string (if nonempty then "<sup>+</sup>" else "<sup>*</sup>");
      print_string "<sub>"; print_sep (); print_string "</sub>")
