include MiniLatex

let print_header symbols =
  documentclass
    (usepackage "" "longtable" ^
     usepackage "" "tabu" ^
     "@;\
      \\\\newenvironment{" ^ command "grammar" ^ "}{@;<0 2>\
      \\\\begin{trivlist}@;<0 4>\
      \\\\item[]@;<0 6>\
      \\\\begin{longtabu}{r%@{}c%@{}X%@{}}@;\
      }{@;<0 6>\
      \\\\end{longtabu}@;<0 2>\
      \\\\end{trivlist}@;}@;@;" ^
     newcommand "gramsp" 0 None "\\\\quad" ^
     newcommand "gramdef" 0 None ("$\\\\" ^ command "gramsp" ^ "::=\\\\" ^ command "gramsp" ^ "$") ^
     newcommand "grambar" 0 None ("$\\\\" ^ command "gramsp" ^ "|\\\\" ^ command "gramsp" ^ "$") ^
     newcommand "grameps" 0 None "\\\\ensuremath{\\\\epsilon}" ^
     newcommand "gramnonterm" 1 None "\\\\ensuremath{\\\\langle\\\\textnormal{#1}\\\\rangle}" ^
     newcommand "gramfunc" 1 None "#1" ^
     newcommand "gramterm" 1 None "#1" ^ "@;");
  begin_document "" (Common.Symbols.terminals symbols)

let def () = "& \\\\" ^ command "gramdef" ^ " & "
let prod_bar () = "& \\\\" ^ command "grambar" ^ " &"
let bar () = "@ \\\\" ^ command "grambar" ^ "@ "
let space () = "@ "
let break () = "\\\\\\\\@;"
let eps () = "\\\\" ^ command "grameps"

let print_rule_name is_not_fun name =
  print_fmt "\\%s{%s}" (command (if is_not_fun then "gramnonterm" else "gramfunc"))
    (Str.global_replace (Str.regexp "_") "\\_" name)
let rule_begin () =
  print_string "@[<v 2>"
let rule_end () =
  print_string "@;\\\\\\\\& & \\\\\\\\";
  print_string "@]@;@;"

let print_symbol is_term is_non_term s print_params =
  let s' = Str.global_replace (Str.regexp "_") "\\_" s in
  if is_non_term then print_fmt "\\%s{%s}" (command "gramnonterm") s'
  else if is_term then print_fmt "\\%s{\\%s{}}" (command "gramterm") (command s)
  else print_fmt "\\%s{%s}" (command "gramfunc") s';
  print_params ()
