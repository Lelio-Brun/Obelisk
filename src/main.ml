open Position
open Format

let ifile = ref ""
let ofile = ref ""

type latexmode =
  | Tabular
  | Syntax
  | Backnaur

type mode =
  | Default
  | Latex of latexmode
  | Html

let mode = ref Default
let inline = ref false

let set_file f s = f := s

let options = ref (Arg.align [
    "-o", Arg.Set_string ofile, " Set the output filename";
    "-i", Arg.Set inline, " Inline recognized patterns"
  ])

let set_latexmode lm () =
  mode := Latex lm

let latex_opt = [
  "-tabular", Arg.Unit (set_latexmode Tabular), " Use tabular environment (default)";
  "-syntax", Arg.Unit (set_latexmode Syntax), " Use `syntax` package";
  "-backnaur", Arg.Unit (set_latexmode Backnaur), " Use `backnaur` package"
]

let html_opt = []

let msg = "menhirbrav [latex|html] [options] <source>"

let parse_cmd =
  let cpt = ref 0 in
  function
  | "latex" when !cpt < 1 ->
    mode := Latex Tabular;
    options := Arg.align (!options @ latex_opt)
  | "html" when !cpt < 1 ->
    mode := Html;
    options := Arg.align (!options @ html_opt)

  | f ->
    set_file ifile f

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
    let print spec = P.print_spec formatter spec in
    let close () = close_in inf; close_out outf in
    lexbuf, print, close
  with Sys_error s ->
    eprintf "System Error%s@." s;
    exit 1

let () =
  let lexbuf, print, close = get () in
  try
    Parser.specification Lexer.lexer lexbuf
    |> Normalize.normalize
    |> Transform.transform
    |> Reduce.reduce !inline
    |> print;
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
