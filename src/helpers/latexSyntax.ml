open Common

include MiniLatex

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
    Str.global_replace (Str.regexp "_") "\\_" max
  in
  documentclass
    (usepackage "" "syntax" ^ "@;" ^
     (if pre () = "" then ""
     else
       "\\newenvironment{" ^ grammarname ^
       "}{\\begin{grammar}}{\\end{grammar}}@;@;") ^
     newcommand "gramterm" 1 None "\\lit{#1}" ^
     newcommand "gramnonterm" 1 None "\\synt{#1}" ^
     newcommand "gramfunc" 1 None "\\synt{#1}" ^
     newcommand "gramdef" 0 None "::=" ^
     newcommand "grambar" 0 None "\\alt" ^
     newcommand "grameps" 0 None "\\ensuremath{\\epsilon}" ^
     "\\newlength{\\" ^ command "grammaxindent" ^
     "}@;\
      \\settowidth{\\" ^ command "grammaxindent" ^
     "}{\\synt{" ^ max ^ "} \\" ^
     command "gramdef" ^ "{} }@;@;");
  begin_document
    ("\\setlength{\\grammarindent}{\\" ^ command "grammaxindent" ^ "}")
    symbols

let def () = " \\" ^ command "gramdef" ^ "{} "
let prod_bar () = "\\" ^ command "grambar" ^ " "
let bar () = "@ \\" ^ command "grambar" ^ "@ "
let space () = "@ "
let break () = "@;"
let eps () = "\\" ^ command "grameps"

let print_rule_name name print_params =
  print_string "<";
  print_rule name print_params;
  print_string ">"
let rule_begin () =
  print_string "@[<v 2>"
let rule_end () =
  print_string "@]@;@;"
