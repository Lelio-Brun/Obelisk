open Format

let p = ref std_formatter
let print_string s = fprintf !p (Scanf.format_from_string s "")
let print_fmt s = fprintf !p s

let production_begin _ =
  print_string "@[<hov 0>"
let production_end _ =
  print_string "@]"

let enclose print op cl =
  print_string op; print (); print_string cl

let par e print =
  if e then enclose print "(" ")" else print ()
