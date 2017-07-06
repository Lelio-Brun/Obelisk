include MiniHelper

let documentclass header =
  print_string
    ("@[<v 0>\\\\documentclass[preview]{standalone}@;@;\
      \\\\usepackage{suffix}@;" ^ header)

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

let command x =
  let roman =
    Str.global_substitute (Str.regexp "[0-9]+")
      (fun s -> Str.matched_string s |> to_roman)
  in
  let clear_underscore =
    Str.global_replace (Str.regexp "_") ""
  in
  roman x |> clear_underscore

let begin_document env ts =
  let commands ts =
    let escape = Str.global_replace (Str.regexp "_") "\\_" in
    List.iter (fun nt ->
        print_fmt "\\newcommand\\%s{%s}@;" (command nt) (escape nt)) ts;
    print_string "@;"
  in
  commands ts;
  print_fmt
    "\\newcommand\\bnfopt[1]{[#1]}@;\
     \\newcommand\\bnfplus[1]{#1\\ensuremath{^+}}@;\
     \\newcommand\\bnfstar[1]{#1\\ensuremath{^*}}@;\
     \\newcommand\\bnfseplist[2]{#2\\ensuremath{_{\\textnormal{#1}}^*}}@;\
     \\newcommand\\bnfsepnelist[2]{#2\\ensuremath{_{\\textnormal{#1}}^+}}@;\
     \\newcommand\\paren[1]{(#1)}@;\
     \\WithSuffix\\newcommand\\bnfopt*[1]{\\paren{\\bnfopt{#1}}}@;\
     \\WithSuffix\\newcommand\\bnfplus*[1]{\\paren{\\bnfplus{#1}}}@;\
     \\WithSuffix\\newcommand\\bnfstar*[1]{\\paren{\\bnfstar{#1}}}@;\
     \\WithSuffix\\newcommand\\bnfseplist*[2]{\\paren{\\bnfseplist{#1}{#2}}}@;\
     \\WithSuffix\\newcommand\\bnfsepnelist*[2]{\\paren{\\bnfsepnelist{#1}{#2}}}@;@;\
     \\begin{document}@;@;\\begin{%s}@;" env

let end_document env =
  print_fmt "\\end{%s}@;@;\\end{document}@]@." env

let opt, plus, star =
  let cmd s e (print: unit -> unit) =
    print_fmt "\\%s%s{" s (if e then "*" else "");
    print ();
    print_string "}"
  in
  cmd "bnfopt", cmd "bnfplus", cmd "bnfstar"

let print_sep_list e nonempty print_sep print_x =
  print_fmt "\\bnfsep%slist%s{"
    (if nonempty then "ne" else "")
    (if e then "*" else "");
  print_sep ();
  print_string "}{";
  print_x ();
  print_string "}"
