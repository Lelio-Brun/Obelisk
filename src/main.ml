(** The main driver of the application.

    Once parsed the grammars are concatenated into one.

    The resulting grammar is scanned ({!Scan}) to get back the symbols then
    normalized ({!Normalize}) and transformed ({!Transform}) to a simpler form.
    Finally, after an optional pass of pattern-recognition ({!Reduce}) it is
    printed ({!Printers}).  *)

open List
open Position
open Format
open Options

(** @return the lexer buffers, a printer chosen from the according passed
    options and a function to finally close the input and output channels.*)
let get () =
  parse_opt ();
  try
    if !ifiles = [] then error ();
    let outf = if !ofile = "" then stdout else open_out !ofile in
    let formatter = formatter_of_out_channel outf in
    let formatter', close_package = match !pfile with
      | "" -> formatter, fun () -> ()
      | pkg ->
        if !ofile = "" then error ();
        let f = open_out (pkg ^ ".sty") in
        formatter_of_out_channel f, fun () -> close_out f
    in
    formatter_package := formatter';
    let p = match !mode with
      | Default -> (module Printers.Default : GenericPrinter.PRINTER)
      | Latex Tabular -> (module Printers.LatexTabular)
      | Latex Syntax -> (module Printers.LatexSyntax)
      | Latex Backnaur -> (module Printers.LatexBacknaur)
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
    eprintf "System Error: %s@." s;
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
    eprintf "System Error: %s@." s;
    exit 1
