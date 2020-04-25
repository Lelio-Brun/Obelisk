let exe = ref ""
let mlys = ref []
let tmp = "tmp"
let prefix = "my_prefix42"
let verbose = ref false
let command cmd = Sys.command cmd = 0
let has_pdflatex = command "command -v pdflatex"
let pkg = "pkg"
let tmppkg = "tmppkg"
let main = "main"

type mode =
  | Default
  | Latex of latexmode
  | Html

and latexmode =
  | Tabular
  | Syntax
  | Backnaur

let of_mode with_pkg = function
  | Default -> ""
  | Latex m ->
    Format.sprintf "latex -%s -prefix %s %s"
      begin match m with
        | Tabular -> "tabular"
        | Syntax -> "syntax"
        | Backnaur -> "backnaur"
      end prefix (if with_pkg then Format.sprintf "-package %s" pkg else "")
  | Html -> "html"

let is_latex = function Latex _ -> true | _ -> false

let ok = "\x1b[32mok\x1b[0m"
let ko = "\x1b[1;31mko\x1b[0m"

let exec mode with_pkg f =
  let cmd = Format.sprintf "%s %s -o %s %s"
      !exe (of_mode with_pkg mode) (if with_pkg then tmppkg ^ ".tex" else tmp) f in
  Format.printf "\tTesting %s%s\t%s@." f (if with_pkg then " (package mode)" else "")
    (if command cmd then
       if is_latex mode then
         let pdflatexmode = if !verbose then "-halt-on-error" else "-interaction batchmode" in
         let pdflatex = Format.sprintf "pdflatex %s %s" pdflatexmode (if with_pkg then main else tmp) in
         if command pdflatex then
           ok
         else ko
       else ok
     else ko)

let of_mode'= function
  | Default -> "Default"
  | Latex m ->
    Format.sprintf "LaTeX %s"
      begin match m with
        | Tabular -> "tabular"
        | Syntax -> "syntax"
        | Backnaur -> "backnaur"
      end
  | Html -> "HTML"

let test mode =
  Format.printf "%s mode.@." (of_mode' mode);
  List.iter (exec mode false) !mlys;
  Format.printf "@."

let test' mode =
  Format.printf "%s mode.@." (of_mode' mode);
  List.iter (exec mode false) !mlys;
  List.iter (exec mode true) !mlys;
  Format.printf "@."

let default () = test Default

let tabular () = test' (Latex Tabular)
let syntax () = test' (Latex Syntax)
let backnaur () = test' (Latex Backnaur)

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
  if has_pdflatex then begin
    write_main ();
    tabular ();
    syntax ();
    backnaur ()
  end

let html () = test Html

let () =
  exe := Sys.argv.(1);
  mlys := Str.split (Str.regexp " ") Sys.argv.(2);
  default ();
  latex ();
  html ();
