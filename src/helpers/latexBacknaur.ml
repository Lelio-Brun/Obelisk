module Make (P : MiniLatex.PACKAGEPRINTER) : module type of Helper = struct

  include MiniLatex.Make(P)

  let print_header symbols =
    documentclass
      (usepackage "[epsilon]" "backnaur" ^
      "@;\
       \\\\newcommand{\\\\gramsp}{\\\\ensuremath{\\\\bnfsp}}@;\
       \\\\newcommand{\\\\gramterm}[1]{\\\\bnfts{#1}}@;\
       \\\\newcommand{\\\\gramnonterm}[1]{\\\\ensuremath{\\\\bnfpn{#1}}}@;\
       \\\\newcommand{\\\\grambar}{\\\\bnfbar}@;\
       \\\\newcommand{\\\\grameps}{\\\\bnfes}@;\
       \\\\newcommand{\\\\gramprod}[3][\\\\textwidth]{\\\\bnfprod{#2}{%%@;<0 2>\
       \\\\begin{minipage}[t]{#1}@;<0 4>\
       $#3$@;<0 2>\
       \\\\end{minipage}}}@;@;\
       \\\\newcommand{\\\\bnfbar}{\\\\hspace*{-2.5em}\\\\bnfor\\\\hspace*{1.2em}}@;@;");
    begin_document "bnf*" (Common.Symbols.terminals symbols)

  let print_footer () = end_document "bnf*"

  let def = "}{"
  let prod_bar = "\\\\grambar "
  let bar = "@ \\\\grambar@ "
  let space = "\\\\gramsp@ "
  let break = "@;\\\\\\\\"
  let eps = "\\\\grameps"

  let print_rule_name is_not_fun name =
    print_fmt "%s" (Str.global_replace (Str.regexp "_") "\\_" name)
  let rule_begin () =
    print_string "@[<v 2>\\\\gramprod{"
  let rule_end () =
    print_string "}\\\\\\\\@]@;"

  let print_symbol is_term _ s print_params =
    let s' = Str.global_replace (Str.regexp "_") "\\_" s in
    if is_term then print_fmt "\\gramterm{\\%s{}}" (command s)
    else print_fmt "\\gramnonterm{%s}" s';
    print_params ()

end
