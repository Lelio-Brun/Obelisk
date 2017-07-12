open Options

include MiniHelper

let use () = !pfile <> ""

let print_string_package s =
  Format.fprintf !formatter_package (Scanf.format_from_string s "")
let print_fmt_package s =
  Format.fprintf !formatter_package s

let usepackage opts s =
  Format.sprintf "\\\\%s%s{%s}@;"
    (if use () then "RequirePackage" else "usepackage") opts s

let documentclass header =
  print_string_package "@[<v 0>";
  if use ()
  then print_fmt_package
      "\\NeedsTeXFormat{LaTeX2e}@;\\ProvidesPackage{%s}@;@;"
      !pfile
  else print_string "\\\\documentclass[preview]{standalone}@;@;";
  print_string_package (usepackage "" "suffix" ^ header)

let to_roman i =
  let rec conv (i, x) (d, r as dr) =
    if i mod d < i then
      conv (i - d, x ^ r) dr
    else
      (i, x)
  in
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
      1, "I"
    ]
  |> snd

let valid x =
  let roman =
    Str.global_substitute (Str.regexp "[0-9]+")
      (fun s -> Str.matched_string s |> to_roman)
  in
  let clear_underscore =
    Str.global_replace (Str.regexp "_") ""
  in
  x |> roman |> clear_underscore

let command x =
  !prefix ^ x |> valid

let pre () = valid !prefix

let begin_document misc ts =
  let commands ts =
    let escape = Str.global_replace (Str.regexp "_") "\\_" in
    List.iter (fun nt ->
        print_fmt_package "\\newcommand\\%s{%s}@;" (command nt) (escape nt)) ts;
    print_string_package "@;"
  in
  commands ts;
  print_endline (pre ());
  print_fmt_package
    "\\newcommand\\%sgramopt[1]{[#1]}@;\
     \\newcommand\\%sgramplus[1]{#1\\ensuremath{^+}}@;\
     \\newcommand\\%sgramstar[1]{#1\\ensuremath{^*}}@;\
     \\newcommand\\%sgramseplist[2]{#2\\ensuremath{_{\\textnormal{#1}}^*}}@;\
     \\newcommand\\%sgramsepnelist[2]{#2\\ensuremath{_{\\textnormal{#1}}^+}}@;\
     \\newcommand\\%sparen[1]{(#1)}@;\
     \\WithSuffix\\newcommand\\%sgramopt*[1]{\\paren{\\%sgramopt{#1}}}@;\
     \\WithSuffix\\newcommand\\%sgramplus*[1]{\\paren{\\%sgramplus{#1}}}@;\
     \\WithSuffix\\newcommand\\%sgramstar*[1]{\\paren{\\%sgramstar{#1}}}@;\
     \\WithSuffix\\newcommand\\%sgramseplist*[2]{\\paren{\\%sgramseplist{#1}{#2}}}@;\
     \\WithSuffix\\newcommand\\%sgramsepnelist*[2]{\\paren{\\%sgramsepnelist{#1}{#2}}}"
    (pre ()) (pre ()) (pre ()) (pre ()) (pre ()) (pre ())
    (pre ()) (pre ()) (pre ()) (pre ()) (pre ())
    (pre ()) (pre ()) (pre ()) (pre ()) (pre ());
  print_fmt "%s%s\\begin{%s}@;"
    (if use () then "" else "\n\n\\begin{document}\n\n")
    (if misc = "" then "" else misc ^ "\n")
    (command "grammar")

let newcommand x n o cmd =
  "\\\\newcommand\\\\" ^ pre () ^ x ^
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
    (command "grammar")
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
