include MiniLatex

let print_header symbols =
  documentclass
    (usepackage "[epsilon]" "backnaur" ^ "@;" ^
     "\\newenvironment{" ^ grammarname ^
     "}{\\begin{bnf*}}{\\end{bnf*}}@;@;" ^
      newcommand "gramsp" 0 None "\\ensuremath{\\bnfsp}" ^
      newcommand "gramterm" 1 None "\\bnfts{#1}" ^
      newcommand "gramnonterm" 1 None "\\ensuremath{\\bnfpn{#1}}" ^
      newcommand "gramfunc" 1 None "\\ensuremath{\\bnfpn{#1}}" ^
      newcommand "grameps" 0 None "\\bnfes" ^
      newcommand "gramprod" 3 (Some "\\textwidth")
        "\\bnfprod{#2}{%%@;<0 2>\
         \\begin{minipage}[t]{#1}@;<0 4>\
         $#3$@;<0 2>\
         \\end{minipage}}" ^
      newcommand "grambar" 0 None "\\hspace*{-2.5em}\\bnfor\\hspace*{1.2em}" ^ "@;");
  begin_document "" symbols

let def () = "}{"
let prod_bar () = "\\" ^ command "grambar" ^ " "
let bar () = "@ \\" ^ command "grambar" ^ "@ "
let space () = "\\" ^ command "gramsp" ^ "@ "
let break () = "\\\\@;"
let eps () = "\\" ^ command "grameps"

let print_rule_name =
  print_rule
let rule_begin () =
  print_fmt "@[<v 2>\\%s{" (command "gramprod")
let rule_end () =
  print_string "}\\\\@]@;"
