open Format

include MiniLatex

let print_header symbols _fmt =
  documentclass
    (fun fmt -> fprintf fmt
        "%a%a@;\
        \\newenvironment{%s}{@;<0 2>\
        \\begin{longtabu}{r%@{}c%@{}X%@{}}@;\
        }{@;<0 2>\
        \\end{longtabu}@;}@;@;\
        %a%a%a%a%a%a%a%a@;\
        %a"
        usepackage ("", "longtable")
        usepackage ("", "tabu")
        grammarname
        newcommand ("gramsp" ,0, None, print_string' "\\quad")
        newcommand ("gramdef", 0, None, fun fmt ->
            fprintf fmt "$\\%s::=\\%s$" (command "gramsp") (command "gramsp"))
        newcommand ("grambar", 0, None, fun fmt ->
            fprintf fmt "$\\%s|\\%s$" (command "gramsp") (command "gramsp"))
        newcommand ("grambaranon", 0, None, print_string' "$|$")
        newcommand ("grameps", 0, None, print_string' "\\ensuremath{\\epsilon}")
        newcommand ("gramnonterm", 1, None, print_string' "\\ensuremath{\\langle\\textnormal{#1}\\rangle}")
        newcommand ("gramfunc", 1, None, fun fmt -> fprintf fmt "\\%s{#1}" (command "gramnonterm"))
        newcommand ("gramterm", 1, None, print_string' "#1")
        (begin_document (fun _ -> ())) symbols)

let def fmt = fprintf fmt "& \\%s & " (command "gramdef")
let prod_bar fmt = fprintf fmt "& \\%s &" (command "grambar")
let bar fmt = fprintf fmt "@ \\%s@ " (command "grambaranon{}")
let space fmt = fprintf fmt "@ "
let break fmt = fprintf fmt "\\\\@;"
let eps fmt = fprintf fmt "\\%s" (command "grameps")

let print_rule_name print_params fmt name =
  fprintf fmt "\\%s{%a}"
    (command "gramfunc")
    (print_rule_name_raw print_params) name
let rule_begin fmt =
  fprintf fmt "@[<v 2>"
let rule_end fmt =
  fprintf fmt "@;\\\\& & \\\\@]@;"
