open Ast
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

  let add_terminal nts ts {groups} =
    let rec add_terminal_actual ts = function
      | Symbol (s, ps) ->
        let ts = List.fold_left add_terminal_actual ts ps in
        if not (StringSet.mem s nts) && String.uppercase_ascii s = s
           && ps = []
        then StringSet.add s ts else ts
      | Modifier (a, _) ->
        add_terminal_actual ts a
      | Anonymous gs ->
        List.fold_left add_terminal_group ts gs
    and add_terminal_producer ts {actual} =
      add_terminal_actual ts actual
    and add_terminal_prod ts {producers} =
      List.fold_left add_terminal_producer ts producers
    and add_terminal_group ts {prods} =
      List.fold_left add_terminal_prod ts prods
    in
    List.fold_left add_terminal_group ts groups

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

  let rec print_group symbols {prods} =
    H.group_begin ();
    H.print_string H.bar;
    begin match prods with
      | [] -> H.print_string H.eps
      | _ -> print_sep (print_production symbols) H.bar prods
    end;
    H.group_end ()

  and print_production symbols {producers} =
    match producers with
      | [] -> H.print_string H.eps; print_space ()
      | _ -> print_sep (print_producer symbols) H.space producers

  and print_producer symbols {actual} =
    print_actual symbols false actual

  and print_actual symbols e = function
    | Symbol (x, ps) ->
      print_symbol symbols e x ps
    | Modifier (a, m) ->
      H.print_modifier e (fun () -> print_actual symbols e a) m
    | Anonymous gs ->
      print_sep (print_group symbols) H.bar gs

  and print_symbol (ts, nts as symbols) e x ps =
    let print' = print_actual symbols e in
    let print'' x () = print' x in
    match x, ps with
    | ("option" | "ioption" | "boption" | "loption"), [x] ->
      H.print_modifier false (print'' x) Opt
    | "pair", [x; y] ->
      H.par e (fun () -> print' x; print_space (); print' y)
    | "separated_pair", [x; sep; y] ->
      H.par e (fun () ->
          print' x; print_space (); print' sep; print_space (); print' y)
    | "preceded", [o; x] ->
      H.par e (fun () -> print' o; print_space (); print' x)
    | "terminated", [x; c] ->
      H.par e (fun () -> print' x; print_space (); print' c)
    | "delimited", [o; x; c] ->
      H.par e (fun () ->
          print' o; print_space (); print' x; print_space (); print' c)
    | "list", [x] ->
      H.print_modifier true (print'' x) Star
    | "nonemptylist", [x] ->
      H.print_modifier true (print'' x) Plus
    | "separated_list", [sep; x] ->
      H.print_modifier true (fun () ->
          print' x; print_space (); print' sep) Star
    | "separated_nonempty_list", [sep; x] ->
      H.print_modifier true (fun () ->
          print' x; print_space (); print' sep) Plus
    | x, _ ->
      H.print_terminal (StringSet.mem x ts) (StringSet.mem x nts) x;
      print_sep_encl (print_actual symbols e) ("," ^ H.space) "(" ")" ps

  let print_rule symbols {name; params; groups} =
    H.rule_begin ();
    H.print_rule_name (params = []) name;
    print_sep_encl H.print_string ", " "(" ")" params;
    H.print_string H.def;
    print_break ();
    print_sep (print_group symbols) H.break groups;
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

  let group_begin () =
    print_string "@[<hov 2>"
  let group_end () =
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

end
module Default = Make (DefaultH)

module MiniLatex = struct
  include MiniHelper

  let documentclass () =
    print_string "@[<v 0>\\\\documentclass{article}@;@;"

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
      "\\\\newenvironment{grammar}{@;<0 2>\
       \\\\begin{trivlist}@;<0 4>\
       \\\\item[]@;<0 6>\
       \\\\begin{tabular}{r%@{}c%@{}l%@{}}@;\
       }{@;<0 6>\
       \\\\end{tabular}@;<0 2>\
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

  let group_begin () =
    print_string "@[<hov 2>"
  let group_end () =
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

  let group_begin () =
    print_string "@[<hov 2>"
  let group_end () =
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
  let eps = "$\\\\epsilon$"

  let print_rule_name is_not_fun name =
    print_fmt "%s" (Str.global_replace (Str.regexp "_") "\\_" name)
  let rule_begin () =
    print_string "@[<v 2>\\\\bnfprod{"
  let rule_end () =
    print_string "\\\\end{bnfsplit}}\\\\vspace*{10em}\\\\\\\\@]@;"

  let group_begin () =
    print_string "@[<hov 2>\\\\\\\\"
  let group_end () =
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

end
module LatexBacknaur = Make (LatexBacknaurH)
