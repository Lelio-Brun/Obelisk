include MiniLatex

let print_header ts =
  documentclass
    "\\\\usepackage{syntax}@;@;\
     \\\\newlength{\\\\spacewidth}@;\
     \\\\settowidth{\\\\spacewidth}{\\\\space}@;@;\
     \\\\newcommand{\\\\term}[1]{#1}@;\
     \\\\newcommand{\\\\gramdef}{::=}@;";
  begin_document "grammar" ts

let print_footer () = end_document "grammar"

let def = " \\\\gramdef{} $\\\\;$"
let prod_bar = "\\\\alt "
let bar = "@ \\\\alt@ "
let space = "@ "
let break = "@;"
let eps = "$\\\\epsilon$"

let print_rule_name is_not_fun =
  print_fmt (if is_not_fun then "<%s>" else "<%s> \\hspace{-\\spacewidth}")
let rule_begin () =
  print_string "@[<v 2>"
let rule_end () =
  print_string "@]@;@;"

let print_terminal is_term _ s =
  if is_term then print_fmt "\\term{\\%s{}}" (command s)
  else print_fmt "\\synt{%s}" s
