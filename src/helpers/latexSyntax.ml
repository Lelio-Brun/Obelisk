open Common
open Format

include MiniLatex

let print_header symbols _fmt =
  let max =
    let compare_length s1 s2 = compare (String.length s2) (String.length s1) in
    let max = try List.(hd (sort compare_length (Symbols.defined symbols))) with _ -> " " in
    let params =
      let rec aux = function
        | [] -> ""
        | [x] -> x
        | x :: xs -> sprintf "%s, %s" x (aux xs)
      in function
        | [] -> ""
        | xs -> sprintf "(%s)" (aux xs)
    in
    let max = match Common.Symbols.is_defined max symbols with
      | Some xs -> max ^ params xs
      | None -> max
    in
    Re.Str.global_replace (Re.Str.regexp "_") "\\_" max
  in
  documentclass
    (fun fmt -> fprintf fmt
        "%a@;\
        \\newenvironment{%s}{\\begin{grammar}}{\\end{grammar}}@;@;\
        %a%a%a%a%a%a%a\
        \\newlength{\\%s}@;\
        \\settowidth{\\%s}{\\synt{%s} \\%s{} }@;@;\
        %a"
        usepackage ("", "syntax")
        grammarname
        newcommand ("gramterm", 1, None, print_string' "\\lit{#1}")
        newcommand ("gramnonterm", 1, None, print_string' "\\synt{#1}")
        newcommand ("gramfunc", 1, None, print_string' "\\synt{#1}")
        newcommand ("gramdef", 0, None, print_string' "::=")
        newcommand ("grambar", 0, None, print_string' "\\alt")
        newcommand ("grambaranon", 0, None, print_string' "\\ensuremath{|}")
        newcommand ("grameps", 0, None, print_string' "\\ensuremath{\\epsilon}")
        (command "grammaxindent")
        (command "grammaxindent")
        max
        (command "gramdef")
        (begin_document (fun fmt -> fprintf fmt
                            "\\setlength{\\grammarindent}{\\%s}"
                            (command "grammaxindent")))
        symbols)

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
