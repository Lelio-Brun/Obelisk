include MiniLatex
open Common

let print_header symbols =
  let max =
    let compare_length s1 s2 = compare (String.length s2) (String.length s1) in
    let max = List.(hd (sort compare_length (Symbols.defined symbols))) in
    let params =
      let rec aux = function
        | [] -> ""
        | [x] -> x
        | x :: xs -> x ^ ", " ^ aux xs
      in function
        | [] -> ""
        | xs -> "(" ^ aux xs ^ ")"
    in
    let max = match Common.Symbols.is_defined max symbols with
      | Some xs -> max ^ params xs
      | None -> assert false
    in
    String.escaped (Str.global_replace (Str.regexp "_") "\\_" max)
  in
  documentclass
    ("\\\\usepackage{syntax}@;@;\
      \\\\newcommand{\\\\term}[1]{#1}@;\
      \\\\newcommand{\\\\gramdef}{::=}@;\
      \\\\newlength{\\\\maxindent}@;\
      \\\\settowidth{\\\\maxindent}{\\\\synt{" ^ max ^
     "} \\\\gramdef{} }@;\
      \\\\setlength{\\\\grammarindent}{\\\\maxindent}@;@;");
  begin_document "grammar" (Common.Symbols.terminals symbols)

let print_footer () = end_document "grammar"

let def = "> \\\\gramdef{} "
let prod_bar = "\\\\alt "
let bar = "@ \\\\alt@ "
let space = "@ "
let break = "@;"
let eps = "$\\\\epsilon$"

let print_rule_name is_not_fun =
  print_fmt (if is_not_fun then "<%s" else "<%s")
let rule_begin () =
  print_string "@[<v 2>"
let rule_end () =
  print_string "@]@;@;"

let print_symbol is_term _ s print_params =
  if is_term then print_fmt "\\term{\\%s{}}" (command s)
  else begin print_fmt "\\synt{%s" s;
    print_params ();
    print_string "}"
  end
