let exe = ref ""
let mlys = ref []
let width = ref 0
let amount = ref 0
let tmp = "tmp"
let prefix = "my_prefix42"
let verbose = ref false
let command cmd = Sys.command (cmd ^ if !verbose then "" else "> /dev/null 2>&1") = 0
let has_pdflatex = command "command -v pdflatex"
let is_on_mac =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  uname = "Darwin"
let has_tidy = command "command -v tidy" && not is_on_mac
let pkg = "pkg"
let tmppkg = "tmppkg"
let main = "main"

let too_larges_for_tabular = [
  (* "fsharp.mly"; *)
  "sysver.mly";
  (* "ocaml.mly"; *)
  (* "ocaml_parser_menhir.mly"; *)
  (* "parser_raw.mly"; *)
  (* "reason.3.3.7-reason_parser.mly"; *)
  (* "reason_parser.mly"; *)
  (* "verilog.mly"; *)
]
let too_larges_for_simplebnf = [
  "coccinelle.1.0.2-parser_cocci_menhir.mly";
  "fsharp.mly";
  "ocaml.mly";
  "ocaml_parser_menhir.mly";
  "parser_cocci_menhir.mly";
  "reason_parser.mly";
  "sysver.mly";
  "verilog.mly";
  ]
let too_larges_for_syntax = [
  "coccinelle.1.0.2-parser_cocci_menhir.mly";
  "fsharp.mly";
  "ocaml.mly";
  "ocaml_parser_menhir.mly";
  "parser_cocci_menhir.mly";
  "reason.3.3.7-reason_parser.mly";
  "reason_parser.mly";
  "verilog.mly";
]
let too_larges_for_backnaur = [
  "sysver.mly"
]

type mode =
  | Default
  | Latex of latexmode
  | Html of htmlmode

and latexmode =
  | Tabular
  | Simplebnf
  | Syntax
  | Backnaur

and htmlmode =
  | CSS
  | NoCSS

let flags_of_mode with_pkg = function
  | Default -> ""
  | Latex m ->
    Format.sprintf "latex -%s -prefix %s %s"
      begin match m with
        | Tabular -> "tabular"
        | Simplebnf -> "simplebnf"
        | Syntax -> "syntax"
        | Backnaur -> "backnaur"
      end prefix (if with_pkg then Format.sprintf "-package %s" pkg else "")
  | Html m ->
    Format.sprintf "html -%s"
      begin match m with
        | CSS -> "css"
        | NoCSS -> "nocss"
      end

let is_latex = function Latex _ -> true | _ -> false
let is_html = function Html _ -> true | _ -> false

let ok = "\x1b[32mok\x1b[0m"
let ko = "\x1b[1;31mko\x1b[0m"

let fail = ref false
let local_fail = ref 0

let exec mode with_pkg f =
  let error () =
    fail := true;
    incr local_fail;
    let fmt = Scanf.format_from_string ("%-" ^ string_of_int !width ^ "s%s %s@.") "%s%s%s" in
    Format.printf fmt f (if with_pkg then " (package mode)" else "") ko
  in
  let cmd = Format.sprintf "%s %s -o %s %s"
      !exe (flags_of_mode with_pkg mode) (if with_pkg then tmppkg ^ ".tex" else tmp) f in
  if command cmd then
    begin if is_latex mode && has_pdflatex then
        let pdflatexmode = if !verbose then "-halt-on-error" else "-interaction batchmode" in
        let pdflatex = Format.sprintf "pdflatex %s %s" pdflatexmode (if with_pkg then main else tmp) in
        begin if not (command pdflatex) then error () end
      else if is_html mode && has_tidy then
        let tidy = Format.sprintf "tidy -e -quiet %s" tmp in
        begin if not (command tidy) then error () end
    end
  else error ()
     
let name_of_mode = function
  | Default -> "Default"
  | Latex m ->
    Format.sprintf "LaTeX %s"
      begin match m with
        | Tabular -> "tabular"
        | Simplebnf -> "simplebnf"
        | Syntax -> "syntax"
        | Backnaur -> "backnaur"
      end
  | Html m ->
    Format.sprintf "HTML %s"
      begin match m with
        | CSS -> "with CSS content properties"
        | NoCSS -> "without CSS content properties"
      end

let enter_mode mode =
  Format.printf "@;\x1b[1m%s mode.\x1b[0m@." (name_of_mode mode)

let exec_mode mode with_pkg too_larges =
  local_fail := 0;
  List.iter (exec mode with_pkg) (List.filter (fun f -> not (List.mem f too_larges)) !mlys);
  Format.printf "%s %s(%d/%d)@."
    (if !local_fail <> 0 then ko else ok)
    (if with_pkg then "(package mode) " else "")
    (!amount - !local_fail) !amount

let test mode =
  enter_mode mode;
  exec_mode mode false []

let too_larges_of_mode = function
  | Tabular -> too_larges_for_tabular
  | Simplebnf -> too_larges_for_simplebnf
  | Syntax -> too_larges_for_syntax
  | Backnaur -> too_larges_for_backnaur

let test_latex m =
  let too_larges = too_larges_of_mode m in
  let mode = Latex m in
  enter_mode mode;
  exec_mode mode false too_larges;
  exec_mode mode true too_larges;
  match too_larges with
  | [] -> ()
  | mlys ->
    Format.printf "Too large grammars: %a@."
      (Format.pp_print_list ~pp_sep:(fun p () -> Format.fprintf p ",@ ")
         Format.pp_print_string) mlys

let default () = test Default

let tabular () = test_latex Tabular
let simplebnf () = test_latex Simplebnf
let syntax () = test_latex Syntax
let backnaur () = test_latex Backnaur

let write_main () =
  let oc = open_out main in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt
    "\\documentclass[preview]{standalone}\n\n\
     \\usepackage{%s}\n\n\
     \\begin{document}\n\
     \\include{%s}\n\
     \\end{document}\n"
    pkg tmppkg;
  close_out oc

let latex () =
  write_main ();
  tabular ();
  simplebnf ();
  syntax ();
  backnaur ()

let html () =
  test (Html CSS);
  test (Html NoCSS)

let () =
  exe := Sys.argv.(1);
  mlys := Re.Str.split (Re.Str.regexp " ") Sys.argv.(2);
  width := List.fold_left (fun w s -> max w (String.length s)) 0 !mlys;
  amount := List.length !mlys;
  default ();
  html ();
  latex ();
  if !fail then exit 1
