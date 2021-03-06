include MiniHelper

let print_header_with_style style =
  print_string
    ("@[<v 0><!DOCTYPE html>@;\
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
      @]@;}@;"
     ^ style ^
     "@]@;</style>\
      @]@;</head>@;@;\
      @[<v 2><body>@;@;\
      @[<v 2><table class=\"specification\">@;@;")

let print_footer () =
  print_string
    "@]@;</table>@;\
     @]@;</body>@;\
     @]@;</html>@]@."

let bar () = " | "
let space () = "@ "
let break () = "@;"
let eps () = "&epsilon;"

let rule_begin () =
  print_string "@[<v 2><tr class=\"rule\">@;"
let rule_end () =
  print_string "@;@;"

let production_begin () = ()
let production_end () =
  print_string "</td>@]@;</tr>"

let print_sep_list e nonempty print_sep print_x =
  par e (fun () ->
      print_x ();
      print_string (if nonempty then "<sup>+</sup>" else "<sup>*</sup>");
      print_string "<sub>"; print_sep (); print_string "</sub>")
