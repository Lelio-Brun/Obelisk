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

let print_opt_params = function
  | Some pps -> pps ()
  | None -> ()

let print_rule opening closing name print_params =
  print_string (opening ^ name);
  print_opt_params print_params;
  print_string closing

let print_symbol_aux opening closing symbols s print_params =
  let is_def = match Common.Symbols.is_defined s symbols with
    | Some _ -> true
    | None -> false
  in
  if is_def then print_string opening;
  print_string s;
  print_params ();
  if is_def then print_string closing
