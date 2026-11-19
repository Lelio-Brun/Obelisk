open Format
include MiniLatex

let theme_without_captions fmt =
  fprintf fmt
    "@[<v 2>\\NewTblrTheme{%s}{@;\
     \\DefTblrTemplate{caption}{default}{}@;\
     \\DefTblrTemplate{capcont}{default}{}@;\
     \\DefTblrTemplate{conthead}{default}{}@;\
     \\DefTblrTemplate{contfoot}{default}{}@]@;\
     }@;"
    grammarname

let print_header symbols fmt =
  (* A [longtblr] loses its first row in a [standalone] preview page. *)
  let table, outer_spec =
    if use () then
      ("longtblr", sprintf "[theme=%s, entry=none, label=none]" grammarname)
    else ("tblr", "")
  in
  documentclass (fun fmt ->
      fprintf fmt
        "%a@;\
         %t@[<v 2>\\NewDocumentEnvironment{%s}{+b}{@;\
         \\begin{%s}%s{colspec={%@{}r%@{}c%@{}X%@{}}, column{1}={cmd=\\%s}}@;\
         #1@;\
         \\end{%s}@]@;\
         }{}@;\
         @;\
         %a%a%a%a%a%a%a%a@;"
        usepackage ("", "tabularray")
        (if use () then theme_without_captions else fun _ -> ())
        grammarname table outer_spec (command "gramnonterm") table newcommand
        ("gramsp", 0, None, print_string' "\\quad")
        newcommand
        ( "gramdef",
          0,
          None,
          fun fmt ->
            fprintf fmt "$\\%s::=\\%s$" (command "gramsp") (command "gramsp") )
        newcommand
        ( "grambar",
          0,
          None,
          fun fmt ->
            fprintf fmt "$\\%s|\\%s$" (command "gramsp") (command "gramsp") )
        newcommand
        ("grambaranon", 0, None, print_string' "$|$")
        newcommand
        ("grameps", 0, None, print_string' "\\ensuremath{\\epsilon}")
        newcommand
        ( "gramnonterm",
          1,
          None,
          print_string'
            "\\IfBlankF{#1}{\\ensuremath{\\langle\\textnormal{#1}\\rangle}}" )
        newcommand
        ( "gramfunc",
          1,
          None,
          fun fmt -> fprintf fmt "\\%s{#1}" (command "gramnonterm") )
        newcommand
        ("gramterm", 1, None, print_string' "#1"));
  begin_document (fun _ -> ()) fmt symbols

let def fmt = fprintf fmt "& \\%s & " (command "gramdef")
let prod_bar fmt = fprintf fmt "& \\%s &" (command "grambar")
let bar fmt = fprintf fmt "@ \\%s@ " (command "grambaranon{}")
let space fmt = fprintf fmt "@ "
let break fmt = fprintf fmt "\\\\@;"
let eps fmt = fprintf fmt "\\%s" (command "grameps")
let print_rule_name = print_rule_name_raw
let rule_begin fmt = fprintf fmt "@[<v 2>"
let rule_end fmt = fprintf fmt "@;\\\\@]@;"
