include MiniLatex

let print_header symbols =
  documentclass
    (usepackage "" "longtable" ^
     usepackage "" "tabu" ^
     "@;\
      \\newenvironment{" ^ grammarname ^ "}{@;<0 2>\
      \\begin{longtabu}{r%@{}c%@{}X%@{}}@;\
      }{@;<0 2>\
      \\end{longtabu}@;}@;@;" ^
     newcommand "gramsp" 0 None "\\quad" ^
     newcommand "gramdef" 0 None ("$\\" ^ command "gramsp" ^ "::=\\" ^ command "gramsp" ^ "$") ^
     newcommand "grambar" 0 None ("$\\" ^ command "gramsp" ^ "|\\" ^ command "gramsp" ^ "$") ^
     newcommand "grameps" 0 None "\\ensuremath{\\epsilon}" ^
     newcommand "gramnonterm" 1 None "\\ensuremath{\\langle\\textnormal{#1}\\rangle}" ^
     newcommand "gramfunc" 1 None ("\\" ^ command "gramnonterm" ^ "{#1}") ^
     newcommand "gramterm" 1 None "#1" ^ "@;");
  begin_document "" symbols

let def () = "& \\" ^ command "gramdef" ^ " & "
let prod_bar () = "& \\" ^ command "grambar" ^ " &"
let bar () = "@ \\" ^ command "grambar" ^ "@ "
let space () = "@ "
let break () = "\\\\@;"
let eps () = "\\" ^ command "grameps"

let print_rule_name name print_params =
  print_fmt "\\%s{" (command "gramfunc");
  print_rule name print_params;
  print_string "}"
let rule_begin () =
  print_string "@[<v 2>"
let rule_end () =
  print_string "@;\\\\& & \\\\";
  print_string "@]@;@;"
