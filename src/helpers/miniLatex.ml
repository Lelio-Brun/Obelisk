open Options
open Format

include MiniHelper

let alias s =
  match Hashtbl.find_opt Lexer.tokens s with
  | Some a -> a
  | None -> s

let use () = !pfile <> ""

let usepackage fmt (opts, s) =
  fprintf fmt "\\%s%s{%s}@;"
    (if use () then "RequirePackage" else "usepackage") opts s

let documentclass header =
  fprintf !formatter_package "@[<v 0>%t%a%t"
    (fun fmt ->
       if use () then
         fprintf fmt "\\NeedsTeXFormat{LaTeX2e}@;\\ProvidesPackage{%s}@;@;" !pfile
       else
         fprintf fmt "\\documentclass[preview]{standalone}@;@;\\usepackage[T1]{fontenc}@;@;")
    usepackage ("", "xparse")
    header

let forall_str p s =
  let exception Exit in
  try
    String.iter (fun c -> if not (p c) then raise Exit) s;
    true
  with Exit -> false

let to_roman i =
  let rec conv (i, x) (d, r as dr) =
    if i mod d < i then
      conv (i - d, x ^ r) dr
    else
      (i, x)
  in
  if forall_str ((=) '0') i then String.map (fun _ -> 'Z') i else
    List.fold_left conv (int_of_string i, "")
      [
        1000, "M";
        900, "CM";
        500, "D";
        400, "CD";
        100, "C";
        90, "XC";
        50, "L";
        40, "XL";
        10, "X";
        9, "IX";
        5, "V";
        4, "IV";
        1, "I";
      ]
    |> snd

let valid x =
  let roman =
    Re.Str.global_substitute (Re.Str.regexp "[0-9]+")
      (fun s -> Re.Str.matched_string s |> to_roman)
  in
  let clear_underscore s =
    Re.Str.global_replace (Re.Str.regexp "_") "UNDERSCORE" s
  in
  let forbid_end x =
    if !prefix = ""
    then Re.Str.global_replace (Re.Str.regexp_case_fold "end") "zzz\\0" x
    else x
  in
  x |> roman |> clear_underscore |> forbid_end

let command x =
  !prefix ^ (valid x) |> valid

let pre () = valid !prefix

let grammarname = command "obeliskgrammar"

let begin_document misc fmt symbols =
  let commands symbols =
    let replace r by = Re.Str.global_replace (Re.Str.regexp r) by in
    let escape s =
      s
      |> replace "\\\\" "\\textbackslash{}"
      |> replace "_" "\\_"
      |> replace "&" "\\&"
      |> replace "%" "\\%"
      |> replace "#" "\\#"
      |> replace "{" "\\{"
      |> replace "}" "\\}"
      |> replace "\\$" "\\$"
    in
    let pp_symbol_cmd star_cmd fmt x =
      fprintf fmt
        "\\NewDocumentCommand\\%s{s}{\
         \\def\\tmp{%s}\
         \\IfBooleanTF#1{\\%s{\\tmp}}{\\tmp}}@;"
        (command (alias x))
        (escape x)
        (command star_cmd)
    in
    let pp_symbol_cmds star_cmd =
      pp_print_list ~pp_sep:(fun _ () -> ()) (pp_symbol_cmd star_cmd)
    in
    fprintf !formatter_package "%a%a%a@;"
      (pp_symbol_cmds "gramterm")
      (Common.Symbols.terminals symbols)
      (pp_symbol_cmds "gramnonterm")
      (Common.Symbols.non_terminals symbols)
      (pp_symbol_cmds "gramfunc")
      (List.map fst (Common.Symbols.functionals symbols))
  in
  commands symbols;
  let pre = pre () in
  fprintf !formatter_package
    "\\NewDocumentCommand\\%sgramopt{sm}{\
     \\def\\tmp{[#2]}\
     \\IfBooleanTF#1{\\tmp}{\\tmp}}@;\
     \\NewDocumentCommand\\%sgramplus{sm}{\
     \\def\\tmp{#2}\
     {\\IfBooleanTF#1{\\%sgramparen{\\tmp}}{\\tmp}}\
     \\ensuremath{^+}}@;\
     \\NewDocumentCommand\\%sgramstar{sm}{\
     \\def\\tmp{#2}\
     {\\IfBooleanTF#1{\\%sgramparen{\\tmp}}{\\tmp}}\
     \\ensuremath{^*}}@;\
     \\NewDocumentCommand\\%sgramseplist{smm}{\
     \\def\\tmp{#3}\
     {\\IfBooleanTF#1{\\%sgramparen{\\tmp}}{\\tmp}}\
     \\ensuremath{_{\\textnormal{#2}}^*}}@;\
     \\NewDocumentCommand\\%sgramsepnelist{smm}{\
     \\def\\tmp{#3}\
     {\\IfBooleanTF#1{\\%sgramparen{\\tmp}}{\\tmp}}\
     \\ensuremath{_{\\textnormal{#2}}^+}}@;\
     \\newcommand\\%sgramparen[1]{(#1)}"
    pre pre pre pre pre
    pre pre pre pre pre;
  fprintf fmt "%s%t\\begin{%s}@;"
    (if use () then "" else "\n\n\\begin{document}\n\n")
    misc
    grammarname

let newcommand fmt (x, n, o, cmd) =
  fprintf fmt "\\newcommand\\%s%s%a%a{%t}@;"
    (pre ()) x
    (fun fmt n -> match n with
       | 0 -> ()
       | _ -> fprintf fmt "[%d]" n)
    n
    (pp_print_option (fun fmt -> fprintf fmt "[%s]")) o
    cmd

let newdocumentcommand fmt (x, n, cmd) =
  fprintf fmt "\\NewDocumentCommand\\%s%s{%s}{%t}@;"
    (pre ()) x
    (String.make n 'm')
    cmd

let end_document fmt =
  fprintf fmt "\\end{%s}%s@]@."
    grammarname
    (if use () then "" else "\n\n\\end{document}")

let print_footer = end_document

let opt, plus, star =
  let cmd s e print fmt =
    fprintf fmt "\\%s%s%s{%t}" (pre ()) s (if e then "*" else "") print
  in
  cmd "gramopt", cmd "gramplus", cmd "gramstar"

let print_sep_list e nonempty print_sep print_x fmt =
  fprintf fmt "\\%sgramsep%slist%s{%t}{%t}"
    (pre ())
    (if nonempty then "ne" else "")
    (if e then "*" else "")
    print_sep
    print_x

let print_comm star fmt c =
  fprintf fmt "\\%s%s{}" (command c) (if star then "*" else "")

let print_term = print_comm true

let print_non_term = print_comm true

let print_fun print_params fmt f =
  fprintf fmt "\\%s{%a%t}"
    (command "gramfunc")
    (print_comm false) f
    print_params

let print_undef fmt  u =
  fprintf fmt "%s" (Re.Str.global_replace (Re.Str.regexp "_") "\\_" u)

let print_param = print_undef

let print_symbol_aux symbols term non_term func undef print_params fmt s =
  let open Common.Symbols in
  match is_defined s symbols with
  | Some [] ->
    non_term fmt s
  | Some _ ->
    func print_params fmt s
  | None ->
    if is_term s symbols then term fmt (alias s) else undef fmt s

let print_symbol symbols =
  print_symbol_aux symbols
    print_term
    print_non_term
    print_fun
    print_undef

let print_rule_name_raw print_params fmt name =
  fprintf fmt "%a%t"
    (print_comm false) name
    print_params
