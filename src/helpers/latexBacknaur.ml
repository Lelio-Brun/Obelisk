open Format

include MiniLatex

let print_header symbols fmt =
  documentclass
    (fun fmt -> fprintf fmt
        "%a@;\
         \\newenvironment{%s}{\\begin{bnf*}}{\\end{bnf*}}@;@;\
         %a%a%a%a%a%a%a%a@;"
        usepackage ("[epsilon]", "backnaur")
        grammarname
        newcommand ("gramsp", 0, None, print_string' "\\ensuremath{\\bnfsp}")
        newcommand ("gramterm", 1, None, print_string' "\\bnfts{#1}")
        newcommand ("gramnonterm", 1, None, print_string' "\\ensuremath{\\bnfpn{#1}}")
        newcommand ("gramfunc", 1, None, print_string' "\\ensuremath{\\bnfpn{#1}}")
        newcommand ("grameps", 0, None, print_string' "\\ensuremath{\\bnfes}")
        newcommand ("gramprod", 3, (Some "\\textwidth"),
                    fun fmt -> fprintf fmt
                        "\\bnfprod{#2}{%%@;<0 2>\
                         \\begin{minipage}[t]{#1}@;<0 4>\
                         $#3$@;<0 2>\
                         \\end{minipage}}")
        newcommand ("grambar", 0, None, print_string' "\\hspace*{-2.5em}\\bnfor\\hspace*{1.2em}")
        newcommand ("grambaranon", 0, None, print_string' "\\ensuremath{\\bnfor}"));
      begin_document (fun _ -> ()) fmt symbols

let def fmt = print_string fmt "}{"
let prod_bar fmt = fprintf fmt "\\%s " (command "grambar")
let bar fmt = fprintf fmt "@ \\%s@ " (command "grambaranon")
let space fmt = fprintf fmt "\\%s@ " (command "gramsp")
let break fmt = fprintf fmt "\\\\@;"
let eps fmt = fprintf fmt "\\%s" (command "grameps")

let print_rule_name =
  print_rule_name_raw
let rule_begin fmt =
  fprintf fmt "@[<v 2>\\%s{" (command "gramprod")
let rule_end fmt =
  fprintf fmt "}\\\\@]"
