(* open Common *)
open Format

include MiniLatex

let print_header symbols fmt =
  documentclass (fun fmt ->
      fprintf fmt
        "%a@;\
         @[<v 2>\\newenvironment{%s}{@;\
         @[<v 2>\\begin{bnf}[@;\
         colspec={%@{}l%@{}rcX%@{}l%@{}},@;\
         column{2}={cmd=\\%s}\
         @]@;]@]@;\
         }{@;<0 2>\
         \\end{bnf}@;}@;@;\
         %a%a%a%a%a%a%a@;\
         @[<v 2>\\SetBNFConfig{@;\
         relation-sym-map={{::=}={\\%s}},@;\
         or-sym={\\%s}\
         @]@;}@;@;"
        usepackage ("", "simplebnf")
        grammarname
        (command "gramnonterm")
        newcommand ("gramterm", 1, None, print_string' "#1")
        newcommand ("gramnonterm", 1, None, print_string' "\\IfBlankF{#1}{\\ensuremath{\\langle\\textnormal{#1}\\rangle}}")
        newcommand ("gramfunc", 1, None, fun fmt -> fprintf fmt "\\%s{#1}" (command "gramnonterm"))
        newcommand ("gramdef", 0, None, print_string' "\\ensuremath{\\Coloneqq}")
        newcommand ("grambar", 0, None, print_string' "\\ensuremath{|}")
        newcommand ("grambaranon", 0, None, print_string' "\\ensuremath{|}")
        newcommand ("grameps", 0, None, print_string' "\\ensuremath{\\epsilon}")
        (command "gramdef")
        (command "grambar")
    );
  begin_document (fun _ -> ()) fmt symbols

let def fmt = fprintf fmt " ::= "
let prod_bar fmt = fprintf fmt "| "
let bar fmt = fprintf fmt "@ \\%s@ " (command "grambaranon{}")
let space fmt = fprintf fmt "@ "
let break fmt = fprintf fmt "@;"
let eps fmt = fprintf fmt "\\%s" (command "grameps")

let print_rule_name print_params fmt name =
  fprintf fmt "%a :"
    (print_rule_name_raw print_params) name
let rule_begin fmt =
  fprintf fmt "@[<v 2>"
let rule_end fmt =
  fprintf fmt "@]@;;;"
