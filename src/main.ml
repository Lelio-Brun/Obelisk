(** The main driver of the application.

    Once parsed, the grammar is scanned ({!Scan}) to get back the symbols then
    normalized ({!Normalize}) and transformed ({!Transform}) to a simpler form.
    Finally, after an optional pass of pattern-recognition ({!Reduce}) it is
    printed ({!Printers}).  *)

open Position
open Format

(** The input file.*)
let ifile = ref ""
(** The output file. *)
let ofile = ref ""

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

(** Specify the filenames of {!ifile} and {!ofile}. *)
let set_file f s = f := s

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
  "-backnaur", Arg.Unit (set_latexmode Backnaur), " Use `backnaur` package"
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
    set_file ifile f

(** @return the lexer buffer, a printer chosen from the according passed options
    and a function to finally close the input channel.*)
let get () =
  Arg.parse_dynamic options parse_cmd msg;
  try
    if !ifile = "" then (Arg.usage !options msg; exit 1);
    let inf = open_in !ifile in
    let outf = if !ofile = "" then stdout else open_out !ofile in
    let lexbuf = Lexing.from_channel inf in
    let formatter = formatter_of_out_channel outf in
    let p = match !mode with
      | Default -> (module Printers.Default : GenericPrinter.PRINTER)
      | Latex Tabular -> (module Printers.LatexTabular)
      | Latex Syntax -> (module Printers.LatexSyntax)
      | Latex Backnaur -> (module Printers.LatexBacknaur)
      | Html -> (module Printers.Html)
    in
    let module P = (val p: GenericPrinter.PRINTER) in
    let print = P.print_spec formatter in
    let close () = close_in inf; close_out outf in
    lexbuf, print, close
  with Sys_error s ->
    eprintf "System Error%s@." s;
    exit 1

let () =
  let lexbuf, print, close = get () in
  try
    let s = Parser.specification Lexer.lexer lexbuf in
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
  | Lexer.LexingError s ->
    err_loc_lexbuf ifile lexbuf;
    eprintf "Lexing Error: %s@." s;
    exit 1
  | Parser.Error ->
    err_loc_lexbuf ifile lexbuf;
    eprintf "Parsing Error@.";
    exit 1
