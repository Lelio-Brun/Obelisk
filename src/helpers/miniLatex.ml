module type PACKAGEPRINTER = sig
  val p: Format.formatter
  val package: string
  val prefix: string
end

module Make (P : PACKAGEPRINTER) = struct

  include MiniHelper

  let use = P.package <> ""

  let print_string_package s = Format.fprintf P.p (Scanf.format_from_string s "")
  let print_fmt_package s = Format.fprintf P.p s

  let usepackage opts s =
    Format.sprintf "\\\\%s%s{%s}@;"
      (if use then "RequirePackage" else "usepackage") opts s

  let documentclass header =
    print_string_package "@[<v 0>";
    if use
    then print_fmt_package "\\NeedsTeXFormat{LaTeX2e}@;\\ProvidesPackage{%s}@;@;" P.package
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

  let command x =
    let roman =
      Str.global_substitute (Str.regexp "[0-9]+")
        (fun s -> Str.matched_string s |> to_roman)
    in
    let clear_underscore =
      Str.global_replace (Str.regexp "_") ""
    in
    P.prefix ^ x |> roman |> clear_underscore

  let begin_document env ts =
    let commands ts =
      let escape = Str.global_replace (Str.regexp "_") "\\_" in
      List.iter (fun nt ->
          print_fmt_package "\\newcommand\\%s{%s}@;" (command nt) (escape nt)) ts;
      print_string_package "@;"
    in
    commands ts;
    print_fmt_package
      "\\newcommand\\gramopt[1]{[#1]}@;\
       \\newcommand\\gramplus[1]{#1\\ensuremath{^+}}@;\
       \\newcommand\\gramstar[1]{#1\\ensuremath{^*}}@;\
       \\newcommand\\gramseplist[2]{#2\\ensuremath{_{\\textnormal{#1}}^*}}@;\
       \\newcommand\\gramsepnelist[2]{#2\\ensuremath{_{\\textnormal{#1}}^+}}@;\
       \\newcommand\\paren[1]{(#1)}@;\
       \\WithSuffix\\newcommand\\gramopt*[1]{\\paren{\\gramopt{#1}}}@;\
       \\WithSuffix\\newcommand\\gramplus*[1]{\\paren{\\gramplus{#1}}}@;\
       \\WithSuffix\\newcommand\\gramstar*[1]{\\paren{\\gramstar{#1}}}@;\
       \\WithSuffix\\newcommand\\gramseplist*[2]{\\paren{\\gramseplist{#1}{#2}}}@;\
       \\WithSuffix\\newcommand\\gramsepnelist*[2]{\\paren{\\gramsepnelist{#1}{#2}}}";
    print_fmt "%s\\begin{%s}@;" (if use then "" else "\n\n\\begin{document}\n\n") env

  let end_document env =
    print_fmt "\\end{%s}%s@]@." env (if use then "" else "\n\n\\end{document}")

  let opt, plus, star =
    let cmd s e (print: unit -> unit) =
      print_fmt "\\%s%s{" s (if e then "*" else "");
      print ();
      print_string "}"
    in
    cmd "gramopt", cmd "gramplus", cmd "gramstar"

  let print_sep_list e nonempty print_sep print_x =
    print_fmt "\\gramsep%slist%s{"
      (if nonempty then "ne" else "")
      (if e then "*" else "");
    print_sep ();
    print_string "}{";
    print_x ();
    print_string "}"

end
