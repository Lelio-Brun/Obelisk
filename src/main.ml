(** The main driver of the application.

    Once parsed the grammars are concatenated into one.

    The resulting grammar is scanned ({!Scan}) to get back the symbols then
    normalized ({!Normalize}) and transformed ({!Transform}) to a simpler form.
    Finally, after an optional pass of pattern-recognition ({!Reduce}) it is
    printed ({!Printers}).  *)

open List
open Position
open Format

(** The input files.*)
let ifiles = ref []
(** The output file. *)
let ofile = ref ""
(** The {i .sty} package file (used only in LaTeX mode). *)
let pfile = ref ""
(** The LaTeX tokens commands prefix. *)
let prefix = ref ""

(** The different output modes. *)
type mode =
  | Default                     (** Standard plain text format. Default. *)
  | Latex of latexmode          (** LaTeX output. *)
  | Html                        (** HTML output. *)

(** The different LaTeX sub-modes *)
and latexmode =
  | Tabular                     (** Table-based layout. Default. *)
  | Syntax                      (** Use the {{:https://www.ctan.org/pkg/syntax-mdw} syntax} package. *)
  | Backnaur                    (** Use the {{:https://www.ctan.org/pkg/backnaur} backnaur} package. *)

(** The chosen mode, default to {!mode.Default}. *)
let mode = ref Default
(** Do we inline inferred patterns ? [false] by default. *)
let inline = ref false

(** Default common command-line options. *)
let options = ref (Arg.align [
    "-o", Arg.Set_string ofile, " Set the output filename";
    "-i", Arg.Set inline, " Inline recognized patterns"
  ])

(** Specify the LaTeX sub-mode to use. *)
let set_latexmode lm () =
  mode := Latex lm

(** LaTeX mode specific options. *)
let latex_opt = [
  "-tabular", Arg.Unit (set_latexmode Tabular), " Use tabular environment (default)";
  "-syntax", Arg.Unit (set_latexmode Syntax), " Use `syntax` package";
  "-backnaur", Arg.Unit (set_latexmode Backnaur), " Use `backnaur` package";
  "-package", Arg.Set_string pfile, " Set the package name, without extension. Use with `-o`";
  "-prefix", Arg.Set_string prefix, " Set the LaTeX tokens commands (macros) prefix"
]

(** Usage message.  *)
let msg = "obelisk [latex|html] [options] <source>"

(** Function called on anonymous arguments.
    It is used to trigger the LaTeX and HTML modes and to get the input file. *)
let parse_cmd =
  let cpt = ref 0 in
  function
  | "latex" when !cpt < 1 ->
    mode := Latex Tabular;
    options := Arg.align (!options @ latex_opt)
  | "html" when !cpt < 1 ->
    mode := Html;
    options := Arg.align !options

  | f ->
    ifiles := f :: !ifiles

(** @return the lexer buffers, a printer chosen from the according passed
    options and a function to finally close the input and output channels.*)
let get () =
  Arg.parse_dynamic options parse_cmd msg;
  let error () = Arg.usage !options msg; exit 1 in
  try
    if !ifiles = [] then error ();
    let outf = if !ofile = "" then stdout else open_out !ofile in
    let formatter = formatter_of_out_channel outf in
    let formatter_package, close_package = match !pfile with
      | "" -> formatter, fun () -> ()
      | pkg ->
        if !ofile = "" then error ();
        let f = open_out (pkg ^ ".sty") in
        formatter_of_out_channel f, fun () -> close_out f
    in
    let module PP : MiniLatex.PACKAGEPRINTER = struct
      let p = formatter_package
      let package = !pfile
      let prefix = !prefix
    end in
    let p = match !mode with
      | Default -> (module Printers.Default : GenericPrinter.PRINTER)
      | Latex Tabular -> (module Printers.LatexTabular(PP))
      | Latex Syntax -> (module Printers.LatexSyntax(PP))
      | Latex Backnaur -> (module Printers.LatexBacknaur(PP))
      | Html -> (module Printers.Html)
    in
    let module P = (val p: GenericPrinter.PRINTER) in
    let print = P.print_spec formatter in
    let files = rev !ifiles in
    let infs = map open_in files in
    let lexbufs = map Lexing.from_channel infs in
    let close () = iter close_in infs; close_out outf; close_package () in
    combine files lexbufs, print, close
  with Sys_error s ->
    eprintf "System Error%s@." s;
    exit 1

(** @return the obtained grammars per input file / lexer buffer.  *)
let parse (file, lexbuf as fl) =
  try
    Lexer.init ();
    Parser.specification Lexer.lexer lexbuf
  with
  | Lexer.LexingError s ->
    err_loc_lexbuf fl (sprintf "Lexing Error: %s" s);
    exit 1
  | Parser.Error ->
    err_loc_lexbuf fl "Parsing Error";
    exit 1

let () =
  let lexbufs, print, close = get () in
  try
    let s = map parse lexbufs |> concat in
    let symbols = Scan.scan s in
    s
    |> Normalize.normalize
    |> Transform.transform symbols
    |> Reduce.reduce !inline
    |> print symbols;
    close ()
  with
  | Sys_error s ->
    eprintf "System Error%s@." s;
    exit 1
