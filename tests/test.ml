let exe = ref ""
let mlys = ref []
let tmp = "tmp"
let prefix = "my_prefix42"
let verbose = ref false
let has_pdflatex = Sys.command "command -v pdflatex" = 0
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

let exec mode with_pkg f =
  let cmd = Format.sprintf "%s %s -o %s %s"
      !exe (of_mode with_pkg mode) (if with_pkg then tmppkg ^ ".tex" else tmp) f in
  Sys.command cmd |> ignore;
  if is_latex mode then begin
    let pdflatexmode = if !verbose then "-halt-on-error" else "-interaction batchmode" in
    let pdflatex = Format.sprintf "pdflatex %s %s" pdflatexmode (if with_pkg then main else tmp) in
    Sys.command pdflatex |> ignore
  end;
  Format.printf "\tTesting %s%s\tok@." f (if with_pkg then " (package mode)" else "")

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
  mlys := String.split_on_char ' ' Sys.argv.(2);
  default ();
  latex ();
  html ();
