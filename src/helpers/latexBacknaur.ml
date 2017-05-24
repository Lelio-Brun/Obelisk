include MiniLatex

let print_header ts =
  documentclass
    "\\\\usepackage[epsilon]{backnaur}@;@;\
     \\\\let\\\\oldbnfsp\\\\bnfsp@;\
     \\\\renewcommand{\\\\bnfsp}{\\\\ensuremath{\\\\oldbnfsp}}@;\
     \\\\let\\\\oldbnfpn\\\\bnfpn@;\
     \\\\renewcommand{\\\\bnfpn}[1]{\\\\ensuremath{\\\\oldbnfpn{#1}}}@;\
     \\\\let\\\\oldbnfprod\\\\bnfprod@;\
     \\\\renewcommand{\\\\bnfprod}[3][\\\\textwidth]{\\\\oldbnfprod{#2}{%%@;<0 2>\
     \\\\begin{minipage}[t]{#1}@;<0 4>\
     $#3$@;<0 2>\
     \\\\end{minipage}}}@;@;\
     \\\\newcommand{\\\\bnfbar}{\\\\hspace*{-2.5em}\\\\bnfor\\\\hspace*{1.2em}}@;@;";
  begin_document "bnf*" ts

let print_footer () = end_document "bnf*"

let def = "}{"
let prod_bar = "\\\\bnfbar "
let bar = "@ \\\\bnfbar@ "
let space = "\\\\bnfsp@ "
let break = "@;\\\\\\\\"
let eps = "\\\\bnfes"

let print_rule_name is_not_fun name =
  print_fmt "%s" (Str.global_replace (Str.regexp "_") "\\_" name)
let rule_begin () =
  print_string "@[<v 2>\\\\bnfprod{"
let rule_end () =
  print_string "}\\\\\\\\@]@;"

let print_terminal is_term _ s =
  let s' = Str.global_replace (Str.regexp "_") "\\_" s in
  if is_term then print_fmt "\\bnfts{\\%s{}}" (command s)
  else print_fmt "\\bnfpn{%s}" s'
