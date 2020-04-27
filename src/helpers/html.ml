include MiniHelper

let print_header _ =
  print_string
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

let print_footer () =
  print_string
    "@]@;</table>@;\
     @]@;</body>@;\
     @]@;</html>@]@."

let def () = "</th>@;<td>"
let prod_bar () = "@[<v 2><tr>@;<th class=\"bar\"></th>@;<td>"
let bar () = " | "
let space () = "@ "
let break () = "@;"
let eps () = "epsilon"

let print_rule_name =
  print_rule_name_with "<th><span class=\"nonterminal\">" "</span>"

let rule_begin () =
  print_string "@[<v 2><tr class=\"rule\">@;"
let rule_end () =
  print_string "@;@;"

let production_begin () = ()
let production_end () =
  print_string "</td>@]@;</tr>"

let print_symbol symbols =
  print_symbol_aux "<span class=\"nonterminal\">" "</span>" symbols

let opt e print =
  print_string "<span class=\"option\">";
  par e print;
  print_string "</span>"

let plus e print =
  print_string "<span class=\"ne_list\">";
  par e print;
  print_string "</span>"
let star e print =
  print_string "<span class=\"list\">";
  par e print;
  print_string "</span>"

let print_sep_list e nonempty print_sep print_x =
  par e (fun () ->
      print_x ();
      print_string (if nonempty then "<sup>+</sup>" else "<sup>*</sup>");
      print_string "<sub>"; print_sep (); print_string "</sub>")
