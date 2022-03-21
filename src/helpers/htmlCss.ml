open Format

include MiniHtml

let print_header _ fmt =
  fprintf fmt
    "@[<v 0><!DOCTYPE html>@;\
     @[<v 2><html>@;\
     @[<v 2><head>@;\
     <title>Grammar</title>@;\
     @[<v 2><style>@;\
     @[<v 4>.specification td, th{@;\
     vertical-align: baseline;@;\
     padding: 0;@;\
     margin: 0;@;\
     font-weight: normal;\
     @]@;}@;\
     @[<v 4>.specification td {@;\
     text-align: left;\
     @]@;}@;\
     @[<v 4>.specification th {@;\
     text-align: right;@;\
     white-space: nowrap;\
     @]@;}@;\
     @[<v 4>.specification th::after {@;\
     content: \"\\a0::=\\a0\";\
     @]@;}@;\
     @[<v 4>.specification th.bar {@;\
     text-align: right;\
     @]@;}@;\
     @[<v 4>.specification th.bar::after {@;\
     content: \"|\\a0\";\
     @]@;}@;\
     @[<v 4>.rule th, td {@;\
     padding-top: .5em;\
     @]@;}@;\
     @[<v 4>.nonterminal::before {@;\
     content: \"<\";\
     @]@;}@;\
     @[<v 4>.nonterminal::after {@;\
     content: \">\";\
     @]@;}@;\
     @[<v 4>.list::after {@;\
     content: \"*\";@;\
     vertical-align: super;@;\
     font-size: smaller;\
     @]@;}@;\
     @[<v 4>.ne_list::after {@;\
     content: \"+\";@;\
     vertical-align: super;@;\
     font-size: smaller;\
     @]@;}@;\
     @[<v 4>.option::before {@;\
     content: \"[\";\
     @]@;}@;\
     @[<v 4>.option::after {@;\
     content: \"]\";\
     @]@;}\
     @]@;</style>\
     @]@;</head>@;@;\
     @[<v 2><body>@;@;\
     @[<v 2><table class=\"specification\">@;@;"

let def fmt = fprintf fmt "</th>@;<td>"
let prod_bar fmt = fprintf fmt "@[<v 2><tr>@;<th class=\"bar\"></th>@;<td>"

let print_rule_name =
  print_rule_name_with
    (print_string' "<th><span class=\"nonterminal\">")
    (print_string' "</span>")

let print_symbol symbols =
  print_symbol_aux symbols
    (print_string' "<span class=\"nonterminal\">")
    (print_string' "</span>")

let opt _ print fmt =
  fprintf fmt "%a%t%a"
    print_string "<span class=\"option\">"
    print
    print_string "</span>"

let plus e print fmt =
  fprintf fmt "%a%t%a"
    print_string "<span class=\"ne_list\">"
    (par e print)
    print_string "</span>"
let star e print fmt =
  fprintf fmt "%a%t%a"
    print_string "<span class=\"list\">"
    (par e print)
    print_string "</span>"
