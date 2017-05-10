open ExtendedAst
open Format

module type HELPER = module type of Helper
module type PRINTER = module type of Printer

module Make (H : HELPER) : PRINTER = struct

  module OrdString = struct
    type t = string
    let compare = Pervasives.compare
  end
  module StringSet = Set.Make(OrdString)

  let add_non_terminal nts {name; params} =
    if params = [] then StringSet.add name nts else nts

  let add_terminal nts ts {prods} =
    let rec add_terminal_actual ts = function
      | Symbol (s, ps) ->
        let ts = List.fold_left add_terminal_actual ts ps in
        if not (StringSet.mem s nts) && String.uppercase_ascii s = s
           && ps = []
        then StringSet.add s ts else ts
      | Pattern p ->
        add_terminal_pattern ts p
      | Modifier (a, _) ->
        add_terminal_actual ts a
      | Anonymous ps ->
        List.fold_left add_terminal_prod ts ps
    and add_terminal_prod ts actuals =
      List.fold_left add_terminal_actual ts actuals
    and add_terminal_pattern ts = function
      | Option x | List x | NEList x ->
        add_terminal_actual ts x
      | Pair (x, y) | Preceded (x, y) | Terminated (x, y)
      | SepList (x, y) | SepNEList (x, y) ->
        add_terminal_actual (add_terminal_actual ts x) y
      | SepPair (x, y, z) | Delimited (x, y, z) ->
        add_terminal_actual (add_terminal_actual (add_terminal_actual ts x) y) z
    in
    List.fold_left add_terminal_prod ts prods

  let scan s =
    let nts = List.fold_left add_non_terminal StringSet.empty s in
    let ts = List.fold_left (add_terminal nts) StringSet.empty s in
    ts, nts

  let print_space () = H.print_string H.space
  let print_break () = H.print_string H.break

  let print_sep_encl_gen print sep op cl =
    let rec aux = function
      | [] -> ()
      | [x] -> print x
      | x :: xs ->
        print x; H.print_string sep; aux xs
    in
    function
    | [] -> ()
    | xs ->
      H.print_string op; aux xs; H.print_string cl
  let print_sep_encl print =
    print_sep_encl_gen print
  let print_sep print sep =
    print_sep_encl print sep "" ""

  let rec print_production symbols actuals =
    H.production_begin ();
    H.print_string H.bar;
    print_actuals symbols actuals;
    H.production_end ()

  and print_actuals symbols = function
    | [] -> H.print_string H.eps; print_space ()
    | xs -> print_sep (print_actual symbols false) H.space xs

  and print_actual symbols e = function
    | Symbol (x, ps) ->
      print_symbol symbols e x ps
    | Pattern p ->
      print_pattern symbols e p
    | Modifier (a, m) ->
      H.print_modifier e (fun () -> print_actual symbols e a) m
    | Anonymous ps ->
      print_sep (print_actuals symbols) H.bar ps

  and print_pattern symbols e =
    let print' = print_actual symbols e in
    let print'' x () = print' x in
    function
    | Option x ->
      H.print_modifier false (print'' x) Opt
    | Pair (x, y) ->
      H.par e (fun () -> print' x; print_space (); print' y)
    | SepPair (x, sep, y) ->
      H.par e (fun () ->
          print' x; print_space (); print' sep; print_space (); print' y)
    | Preceded (o, x) ->
      H.par e (fun () -> print' o; print_space (); print' x)
    | Terminated (x, c) ->
      H.par e (fun () -> print' x; print_space (); print' c)
    | Delimited (o, x, c) ->
      H.par e (fun () ->
          print' o; print_space (); print' x; print_space (); print' c)
    | List x ->
      H.print_modifier false (print'' x) Star
    | NEList x ->
      H.print_modifier false (print'' x) Plus
    | SepList (sep, x) ->
      H.print_sep_list false (print'' sep) (print'' x)
    | SepNEList (sep, x) ->
      H.print_sep_list true (print'' sep) (print'' x)

  and print_symbol (ts, nts as symbols) e x ps =
    H.print_terminal (StringSet.mem x ts) (StringSet.mem x nts) x;
    print_sep_encl (print_actual symbols e) ("," ^ H.space) "(" ")" ps

  let print_rule symbols {name; params; prods} =
    H.rule_begin ();
    H.print_rule_name (params = []) name;
    print_sep_encl H.print_string ", " "(" ")" params;
    H.print_string H.def;
    print_break ();
    print_sep (print_production symbols) H.break prods;
    H.rule_end ()

  let print_spec o s =
    let ts, nts = scan s in
    H.p := o;
    H.print_header (StringSet.elements ts);
    H.print_string "@[<v 0>";
    List.iter (print_rule (ts, nts)) s;
    H.print_string "@]";
    H.print_footer ()

end

module MiniHelper = struct
  let p = ref std_formatter
  let print_string s = fprintf !p (Scanf.format_from_string s "")
  let print_fmt s = fprintf !p s
end

module DefaultH : HELPER = struct

  include MiniHelper

  let print_header _ = ()
  let print_footer () = print_string "@."

  let def = " ::="
  let bar = "| "
  let space = "@ "
  let break = "@;"
  let eps = "epsilon"

  let print_rule_name is_not_fun =
    print_fmt (if is_not_fun then "<%s>" else "%s")

  let rule_begin () =
    print_string "@[<v 2>"
  let rule_end () =
    print_string "@]@;@;"

  let production_begin () =
    print_string "@[<hov 2>"
  let production_end () =
    print_string "@]"

  let print_terminal is_term is_non_term =
    print_fmt (if is_non_term then "<%s>" else "%s")

  let enclose print op cl =
    print_string op; print (); print_string cl

  let par e print =
    if e then enclose print "(" ")" else print ()

  let print_modifier e print = function
    | Opt ->
      enclose print "[" "]"
    | Plus ->
      par e print; print_string "+"
    | Star ->
      par e print; print_string "*"

  let print_sep_list nonempty print_sep print_x =
    let print () =
      print_x (); print_string space;
      print_modifier true
        (fun () -> print_sep (); print_string space; print_x ()) Star
    in
    if nonempty then print () else enclose print "[" "]"

end
module Default = Make (DefaultH)

module MiniLatex = struct
  include MiniHelper

  let documentclass () =
    print_string "@[<v 0>\\\\documentclass[preview]{standalone}@;@;"

  let commands ts =
    List.iter (fun nt ->
        print_string ("\\\\newcommand{\\\\" ^ nt ^ "}{" ^ nt ^ "}@;")) ts;
    print_string "@;"

  let begin_document env =
    print_fmt "\\begin{document}@;@;\\begin{%s}@;" env
  let end_document env =
    print_fmt "\\end{%s}@;@;\\end{document}@]@." env
end

module LatexTabularH : HELPER = struct

  include MiniLatex

  let print_header ts =
    documentclass ();
    print_string
      "\\\\usepackage{tabu}@;@;<0 2>\
       \\\\newenvironment{grammar}{@;<0 2>\
       \\\\begin{trivlist}@;<0 4>\
       \\\\item[]@;<0 6>\
       \\\\begin{tabu}{r%@{}c%@{}X%@{}}@;\
       }{@;<0 6>\
       \\\\end{tabu}@;<0 2>\
       \\\\end{trivlist}@;\
       }@;@;\
       \\\\newcommand{\\\\gramsp}{\\\\;\\\\;}@;\
       \\\\newcommand{\\\\gramdef}{$\\\\gramsp::=$}@;\
       \\\\newcommand{\\\\grambar}{$\\\\gramsp|\\\\gramsp$}@;\
       \\\\newcommand{\\\\nonterm}[1]{$\\\\langle$#1$\\\\rangle$}@;\
       \\\\newcommand{\\\\func}[1]{#1}@;\
       \\\\newcommand{\\\\term}[1]{#1}@;";
    commands ts;
    begin_document "grammar"

  let print_footer () = end_document "grammar"

  let def = "& \\\\gramdef & \\\\\\\\"
  let bar = "& \\\\grambar &"
  let space = "@ "
  let break = "@;"
  let eps = "$\\\\epsilon$"

  let print_rule_name is_not_fun name =
    print_fmt (if is_not_fun then "\\nonterm{%s}" else "\\func{%s}")
      (Str.global_replace (Str.regexp "_") "\\_" name)
  let rule_begin () =
    print_string "@[<v 2>"
  let rule_end () =
    print_string "@;& & \\\\\\\\";
    print_string "@]@;@;"

  let production_begin () =
    print_string "@[<hov 2>"
  let production_end () =
    print_string " @] \\\\\\\\"

  let print_terminal is_term is_non_term s =
    print_fmt
      (if is_non_term then "\\nonterm{%s}"
       else if is_term then "\\term{\\%s{}}" else "\\func{%s}")
      (Str.global_replace (Str.regexp "_") "\\_" s)

  let enclose print op cl =
    print_string op; print (); print_string cl

  let par e print =
    if e then enclose print "(" ")" else print ()

  let print_modifier e print = function
    | Opt ->
      enclose print "[" "]"
    | Plus ->
      par e print; print_string "$^+$"
    | Star ->
      par e print; print_string "$^*$"

  let print_sep_list nonempty print_sep print_x =
    print_x (); print_string "$_{"; print_sep ();
    print_string (if nonempty then "}^+$" else "}^*$")

end
module LatexTabular = Make (LatexTabularH)

module LatexSyntaxH : HELPER = struct

  include MiniLatex

  let print_header ts =
    documentclass ();
    print_string
      "\\\\usepackage{syntax}@;@;\
       \\\\newlength{\\\\spacewidth}@;\
       \\\\settowidth{\\\\spacewidth}{\\\\space}@;@;\
       \\\\newcommand{\\\\term}[1]{#1}@;\
       \\\\newcommand{\\\\gramdef}{::=}@;";
    commands ts;
    begin_document "grammar"

  let print_footer () = end_document "grammar"

  let def = " \\\\gramdef{} $\\\\;$"
  let bar = "\\\\alt "
  let space = "@ "
  let break = "@;"
  let eps = "$\\\\epsilon$"

  let print_rule_name is_not_fun name =
    print_fmt (if is_not_fun then "<%s>" else "<%s> \\hspace{-\\spacewidth}")
      (Str.global_replace (Str.regexp "_") "\\_" name)
  let rule_begin () =
    print_string "@[<v 2>"
  let rule_end () =
    print_string "@]@;@;"

  let production_begin () =
    print_string "@[<hov 2>"
  let production_end () =
    print_string " @]"

  let print_terminal is_term is_non_term s =
    print_fmt
      (if is_non_term then "<%s>" else
       if is_term then "\\term{\\%s{}}" else "<%s> \\hspace{-\\spacewidth}")
      (Str.global_replace (Str.regexp "_") "\\_" s)

  let enclose print op cl =
    print_string op; print (); print_string cl

  let par e print =
    if e then enclose print "(" ")" else print ()

  let print_modifier e print = function
    | Opt ->
      enclose print "[" "]"
    | Plus ->
      par e print; print_string "$^+$"
    | Star ->
      par e print; print_string "$^*$"

  let print_sep_list nonempty print_sep print_x =
    print_x (); print_string "$_{"; print_sep ();
    print_string (if nonempty then "}^+$" else "}^*$")

end
module LatexSyntax = Make (LatexSyntaxH)

module LatexBacknaurH : HELPER = struct

  include MiniLatex

  let print_header ts =
    documentclass ();
    print_string
      "\\\\usepackage{backnaur}@;@;\
       \\\\newenvironment{bnfsplit}[1][\\\\textwidth]@;<0 2>\
       {\\\\minipage[t]{#1}$}@;<0 2>\
       {$\\\\endminipage}@;@;";
    commands ts;
    begin_document "bnf*"

  let print_footer () = end_document "bnf*"

  let def = "}{\\\\begin{bnfsplit}"
  let bar = "\\\\hspace*{-2.5em}\\\\bnfor "
  let space = "\\\\bnfsp@ "
  let break = "@;"
  let eps = "\\\\bnfts{$\\\\epsilon$}"

  let print_rule_name is_not_fun name =
    print_fmt "%s" (Str.global_replace (Str.regexp "_") "\\_" name)
  let rule_begin () =
    print_string "@[<v 2>\\\\bnfprod{"
  let rule_end () =
    print_string "\\\\end{bnfsplit}}\\\\\\\\@]@;"

  let production_begin () =
    print_string "@[<hov 2>\\\\\\\\"
  let production_end () =
    print_string "@]"

  let print_terminal is_term is_non_term s =
    print_fmt
      (if is_non_term then "\\bnfpn{%s}" else
       if is_term then "\\bnfts{%s}" else "\\bnfpn{%s}")
      (Str.global_replace (Str.regexp "_") "\\_" s)

  let enclose print op cl =
    print_string op; print (); print_string cl

  let par e print =
    if e then enclose print "(" ")" else print ()

  let print_modifier e print = function
    | Opt ->
      enclose print "[" "]"
    | Plus ->
      par e print; print_string "^+"
    | Star ->
      par e print; print_string "^*"

  let print_sep_list nonempty print_sep print_x =
    let print () =
      print_x (); print_string space;
      print_modifier true
        (fun () -> print_sep (); print_string space; print_x ()) Star
    in
    if nonempty then print () else enclose print "[" "]"

end
module LatexBacknaur = Make (LatexBacknaurH)

module HtmlH : HELPER = struct

  include MiniHelper

  let print_header _ =
    print_string
      "@[<v 0><!DOCTYPE html>@;\
       <html>@;\
       <head>@;\
       <title>Grammar</title>@;\
       <style>@;\
       .specification {@;\
       list-style: none;@;\
       padding: 0;@;\
       margin: 0;@;\
       }@;\
       .specification li {@;\
       margin-bottom: .5em;@;\
       }@;\
       .groups {@;\
       display: inline;@;\
       list-style: none;@;\
       padding-left: .5em;\
       }@;\
       .groups li {@;\
       margin-bottom: 0;@;\
       }@;\
       .groups:before {@;\
       content: \"::=\";@;\
       }@;\
       .groups li {@;\
       padding-left: 2em;@;\
       }@;\
       .groups li:before {@;\
       content: \"|\";@;\
       }@;\
       .nonterminal:before {@;\
       content: \"<\";@;\
       }@;\
       .nonterminal:after {@;\
       content: \">\";@;\
       }@;\
       </style>@;\
       </head>@;\
       <body>@;\
       <ul class=\"specification\">@;@;"

  let print_footer () =
    print_string
      "</ul>@;\
       </body>@;\
       </html>@]@."

  let def = "@[<v 2><ul class=\"groups\">"
  let bar = " "
  let space = "@ "
  let break = "@;"
  let eps = "epsilon"

  let print_rule_name is_not_fun =
    print_fmt (if is_not_fun then "<span class=\"nonterminal\">%s</span>@;" else "%s@;")

  let rule_begin () =
    print_string "@[<v 2><li>"
  let rule_end () =
    print_string "@]@;</ul>@]@;</li>@;@;"

  let production_begin () =
    print_string "@[<hov 2><li>"
  let production_end () =
    print_string "</li>@]"

  let print_terminal is_term is_non_term =
    print_fmt (if is_non_term then "<span class=\"nonterminal\">%s</span>" else "%s")

  let enclose print op cl =
    print_string op; print (); print_string cl

  let par e print =
    if e then enclose print "(" ")" else print ()

  let print_modifier e print = function
    | Opt ->
      enclose print "[" "]"
    | Plus ->
      par e print; print_string "<sup>+</sup>"
    | Star ->
      par e print; print_string "<sup>*</sup>"

  let print_sep_list nonempty print_sep print_x =
    print_x ();
    print_string (if nonempty then "<sup>+</sup>" else "<sup>*</sup>");
    print_string "<sub>"; print_sep (); print_string "</sub>"

end
module Html = Make (HtmlH)
