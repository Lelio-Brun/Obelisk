open Common
open Format

include MiniLatex

let print_header symbols fmt =
  let max =
    let params =
      let rec aux = function
        | [] -> ""
        | [x] -> x
        | x :: xs -> sprintf "%s, %s" x (aux xs)
      in function
        | [] -> ""
        | xs -> sprintf "(%s)" (aux xs)
    in
    let compare_length (s1, xs1) (s2, xs2) =
      compare
        (String.length (s2 ^ params xs2))
        (String.length (s1 ^ params xs1))
    in
    let f, xs = try
        List.(hd (sort compare_length (Symbols.defined symbols)))
      with _ -> " ", []
    in
    let max = f ^ params xs in
    Re.Str.global_replace (Re.Str.regexp "_") "\\_" max
  in
  documentclass (fun fmt ->
      fprintf fmt
        "%a@;\
         \\newenvironment{%s}{\\begin{grammar}}{\\end{grammar}}@;@;\
         %a%a%a%a%a%a%a\
         \\renewcommand\\grammarlabel[2]{\\%s{#1} #2}@;\
         \\newlength{\\%s}@;\
         \\settowidth{\\%s}{\\synt{%s} \\%s{} }@;@;"
        usepackage ("", "syntax")
        grammarname
        newcommand ("gramterm", 1, None, print_string' "\\lit{#1}")
        newdocumentcommand ("gramnonterm", 1, print_string' "\\IfBlankF{#1}{\\ensuremath{\\langle\\textnormal{#1}\\rangle}}")
        newcommand ("gramfunc", 1, None, fun fmt -> fprintf fmt "\\%s{#1}" (command "gramnonterm"))
        newcommand ("gramdef", 0, None, print_string' "::=")
        newcommand ("grambar", 0, None, print_string' "\\alt")
        newcommand ("grambaranon", 0, None, print_string' "\\ensuremath{|}")
        newcommand ("grameps", 0, None, print_string' "\\ensuremath{\\epsilon}")
        (command "gramnonterm")
        (command "grammaxindent")
        (command "grammaxindent")
        max
        (command "gramdef")
    );
  begin_document (fun fmt -> fprintf fmt
                               "\\setlength{\\grammarindent}{\\%s}"
                               (command "grammaxindent"))
    fmt symbols

let def fmt = fprintf fmt " \\%s{} " (command "gramdef")
let prod_bar fmt = fprintf fmt "\\%s " (command "grambar")
let bar fmt = fprintf fmt "@ \\%s@ " (command "grambaranon{}")
let space fmt = fprintf fmt "@ "
let break fmt = fprintf fmt "@;"
let eps fmt = fprintf fmt "\\%s" (command "grameps")

let print_rule_name print_params fmt name =
  fprintf fmt "%a%a%a"
    print_string "<"
    (print_rule_name_raw print_params) name
    print_string ">"
let rule_begin fmt =
  fprintf fmt "@[<v 2>"
let rule_end fmt =
  fprintf fmt "@]@;"
