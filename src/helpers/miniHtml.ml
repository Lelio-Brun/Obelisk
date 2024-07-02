open Format

include MiniHelper

let print_header_with_style fmt style =
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
     @[<v 4>.specification th.bar {@;\
     text-align: right;\
     @]@;}@;\
     @[<v 4>.rule th, td {@;\
     padding-top: .5em;\
     @]@;}@;\
     %s\
     @]@;</style>\
     @]@;</head>@;@;\
     @[<v 2><body>@;@;\
     @[<v 2><table class=\"specification\">@;@;"
    style

let print_footer fmt =
  fprintf fmt
    "@]@;</table>@;\
     @]@;</body>@;\
     @]@;</html>@]@."

let bar fmt = pp_print_string fmt " | "
let space fmt = fprintf fmt "@ "
let break fmt = fprintf fmt "@;"
let eps fmt = pp_print_string fmt "&epsilon;"

let rule_begin fmt =
  fprintf fmt "@[<v 2><tr class=\"rule\">@;"
let rule_end fmt =
  fprintf fmt "@;@;"

let production_begin _ = ()
let production_end fmt =
  fprintf fmt "</td>@]@;</tr>"

let print_sep_list e nonempty print_sep print_x fmt =
  par e (fun fmt ->
      fprintf fmt "%t%a%a%t%a"
        print_x
        print_string (if nonempty then "<sup>+</sup>" else "<sup>*</sup>")
        print_string "<sub>"
        print_sep
        print_string "</sub>")
    fmt
