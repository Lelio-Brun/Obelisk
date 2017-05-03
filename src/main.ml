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

let mode = ref Default

let set_file f s = f := s

let options = ref [
    "-o", Arg.Set_string ofile, " Set the output filename"
  ]

let set_latexmode lm () =
  mode := Latex lm

let latex_opt = [
  "-tabular", Arg.Unit (set_latexmode Tabular), " Use tabular environment (default)";
  "-syntax", Arg.Unit (set_latexmode Syntax), " Use `syntax` package";
  "-backnaur", Arg.Unit (set_latexmode Backnaur), " Use `backnaur` package"
]

let msg = "menhir2latex [latex] [options] <source>"

let parse_cmd =
  let cpt = ref 0 in
  function
  | "latex" when !cpt < 1 ->
    mode := Latex Tabular;
    options := !options @ latex_opt
  | f ->
    set_file ifile f

let get () =
  Arg.parse_dynamic options parse_cmd msg;
  try
    let inf = open_in !ifile in
    let outf = if !ofile = "" then stdout else open_out !ofile in
    let lexbuf = Lexing.from_channel inf in
    let formatter = formatter_of_out_channel outf in
    let p = match !mode with
      | Default -> (module Printers.Default : Printers.PRINTER)
      | Latex Tabular -> (module Printers.LatexTabular : Printers.PRINTER)
      | Latex Syntax -> (module Printers.LatexSyntax : Printers.PRINTER)
      | Latex Backnaur -> (module Printers.LatexBacknaur : Printers.PRINTER)
    in
    let module P = (val p : Printers.PRINTER) in
    let print spec = P.print_spec formatter spec in
    let close () = close_in inf; close_out outf in
    lexbuf, print, close
  with Sys_error s ->
    eprintf "System Error: %s@." s;
    exit 1

let () =
  let lexbuf, print, close = get () in
  try
    let s = Parser.specification Lexer.lexer lexbuf in
    print s;
    close ()
  with
  | Sys_error s ->
    eprintf "System Error: %s@." s;
    exit 1
  | Lexer.LexingError s ->
    err_loc_lexbuf ifile lexbuf;
    eprintf "Lexing Error: %s@." s;
    exit 1
  | Parser.Error ->
    err_loc_lexbuf ifile lexbuf;
    eprintf "Parsing Error@.";
    exit 1
