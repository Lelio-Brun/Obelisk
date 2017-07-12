include MiniLatex

let print_header symbols =
  documentclass
    (usepackage "[epsilon]" "backnaur" ^ "@;" ^
     "\\\\newenvironment{" ^ command "grammar" ^
     "}{\\\\begin{bnf*}}{\\\\end{bnf*}}@;@;" ^
      newcommand "gramsp" 0 None "\\\\ensuremath{\\\\bnfsp}" ^
      newcommand "gramterm" 1 None "\\\\bnfts{#1}" ^
      newcommand "gramnonterm" 1 None "\\\\ensuremath{\\\\bnfpn{#1}}" ^
      newcommand "grameps" 0 None "\\\\bnfes" ^
      newcommand "gramprod" 3 (Some "\\\\textwidth")
        "\\\\bnfprod{#2}{%%@;<0 2>\
         \\\\begin{minipage}[t]{#1}@;<0 4>\
         $#3$@;<0 2>\
         \\\\end{minipage}}" ^
      newcommand "grambar" 0 None "\\\\hspace*{-2.5em}\\\\bnfor\\\\hspace*{1.2em}" ^ "@;");
  begin_document "" (Common.Symbols.terminals symbols)

let def () = "}{"
let prod_bar () = "\\\\" ^ command "grambar" ^ " "
let bar () = "@ \\\\" ^ command "grambar" ^ "@ "
let space () = "\\\\" ^ command "gramsp" ^ "@ "
let break () = "\\\\\\\\@;"
let eps () = "\\\\" ^ command "grameps"

let print_rule_name is_not_fun name =
  print_fmt "%s" (Str.global_replace (Str.regexp "_") "\\_" name)
let rule_begin () =
  print_fmt "@[<v 2>\\%s{" (command "gramprod")
let rule_end () =
  print_string "}\\\\\\\\@]@;"

let print_symbol is_term _ s print_params =
  let s' = Str.global_replace (Str.regexp "_") "\\_" s in
  if is_term then print_fmt "\\%s{\\%s{}}" (command "gramterm") (command s)
  else print_fmt "\\%s{%s}" (command "gramnonterm") s';
  print_params ()
