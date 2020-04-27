open Options

include MiniHelper

let alias s =
  match Hashtbl.find_opt Lexer.tokens s with
  | Some a -> a
  | None -> s

let use () = !pfile <> ""

let print_string_package s =
  Format.fprintf !formatter_package (Scanf.format_from_string s "")
let print_fmt_package s =
  Format.fprintf !formatter_package s

let usepackage opts s =
  Format.sprintf "\\%s%s{%s}@;"
    (if use () then "RequirePackage" else "usepackage") opts s

let documentclass header =
  print_string_package "@[<v 0>";
  if use ()
  then print_fmt_package
      "\\NeedsTeXFormat{LaTeX2e}@;\\ProvidesPackage{%s}@;@;"
      !pfile
  else print_string "\\documentclass[preview]{standalone}@;@;\\usepackage[T1]{fontenc}@;@;";
  print_string_package (usepackage "" "suffix" ^ header)

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

let begin_document misc symbols =
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
    List.iter (fun x ->
        print_fmt_package "\\newcommand\\%s{%s}@;"
          (command (alias x)) (escape x))
      (Common.Symbols.terminals symbols @ Common.Symbols.defined symbols);
    List.iter (fun x ->
        let cx = command (alias x) in
        print_fmt_package "\\WithSuffix\\newcommand\\%s*{\\%s{\\%s}}@;"
          cx (command "gramterm") cx)
      (Common.Symbols.terminals symbols);
    List.iter (fun x ->
        let cx = command x in
        print_fmt_package "\\WithSuffix\\newcommand\\%s*{\\%s{\\%s}}@;"
          cx (command "gramnonterm") cx)
      (Common.Symbols.non_terminals symbols);
    List.iter (fun x ->
        let cx = command x in
        print_fmt_package "\\WithSuffix\\newcommand\\%s*{\\%s{\\%s}}@;"
          cx (command "gramfunc") cx)
      (Common.Symbols.functionals symbols);
   print_string_package "@;"
  in
  commands symbols;
  print_fmt_package
    "\\newcommand\\%sgramopt[1]{[#1]}@;\
     \\newcommand\\%sgramplus[1]{#1\\ensuremath{^+}}@;\
     \\newcommand\\%sgramstar[1]{#1\\ensuremath{^*}}@;\
     \\newcommand\\%sgramseplist[2]{#2{\\ensuremath{_{\\textnormal{#1}}^*}}}@;\
     \\newcommand\\%sgramsepnelist[2]{#2{\\ensuremath{_{\\textnormal{#1}}^+}}}@;\
     \\newcommand\\%sgramparen[1]{(#1)}@;\
     \\WithSuffix\\newcommand\\%sgramopt*[1]{\\gramparen{\\%sgramopt{#1}}}@;\
     \\WithSuffix\\newcommand\\%sgramplus*[1]{\\gramparen{\\%sgramplus{#1}}}@;\
     \\WithSuffix\\newcommand\\%sgramstar*[1]{\\gramparen{\\%sgramstar{#1}}}@;\
     \\WithSuffix\\newcommand\\%sgramseplist*[2]{\\gramparen{\\%sgramseplist{#1}{#2}}}@;\
     \\WithSuffix\\newcommand\\%sgramsepnelist*[2]{\\gramparen{\\%sgramsepnelist{#1}{#2}}}"
    (pre ()) (pre ()) (pre ()) (pre ()) (pre ()) (pre ())
    (pre ()) (pre ()) (pre ()) (pre ()) (pre ())
    (pre ()) (pre ()) (pre ()) (pre ()) (pre ());
  print_fmt "%s%s\\begin{%s}@;"
    (if use () then "" else "\n\n\\begin{document}\n\n")
    (if misc = "" then "" else misc ^ "\n")
    grammarname

let newcommand x n o cmd =
  "\\newcommand\\" ^ pre () ^ x ^
  begin match n with
  | 0 -> ""
  | _ -> "[" ^ string_of_int n ^ "]"
  end
  ^ begin match o with
    | None -> ""
    | Some y -> "[" ^ y ^ "]"
  end
  ^ "{" ^ cmd ^ "}@;"

let end_document () =
  print_fmt "\\end{%s}%s@]@."
    grammarname
    (if use () then "" else "\n\n\\end{document}")

let print_footer = end_document

let opt, plus, star =
  let cmd s e (print: unit -> unit) =
    print_fmt "\\%s%s%s{" (pre ()) s (if e then "*" else "");
    print ();
    print_string "}"
  in
  cmd "gramopt", cmd "gramplus", cmd "gramstar"

let print_sep_list e nonempty print_sep print_x =
  print_fmt "\\%sgramsep%slist%s{"
    (pre ())
    (if nonempty then "ne" else "")
    (if e then "*" else "");
  print_sep ();
  print_string "}{";
  print_x ();
  print_string "}"

let print_comm star c =
  print_fmt "\\%s%s{}" (command c) (if star then "*" else "")

let print_term = print_comm true

let print_non_term = print_comm true

let print_fun f print_params =
  print_fmt "\\%s{" (command "gramfunc");
  print_comm false f;
  print_params ();
  print_string "}"

let print_undef u =
  print_fmt "%s" (Re.Str.global_replace (Re.Str.regexp "_") "\\_" u)

let print_param = print_undef

let print_symbol_aux term non_term func undef symbols s print_params =
  let open Common.Symbols in
  match is_defined s symbols with
  | Some [] -> non_term s
  | Some _ -> func s print_params
  | None ->
    if is_term s symbols then term (alias s) else undef s

let print_symbol symbols =
  print_symbol_aux
    print_term
    print_non_term
    print_fun
    print_undef
    symbols

let print_rule_name_raw name =
  print_comm false name;
  print_opt_params
