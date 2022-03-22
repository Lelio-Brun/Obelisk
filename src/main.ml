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
  let c = parse_opt () in
  try
    let outf = if !ofile = "" then stdout else open_out !ofile in
    let formatter = formatter_of_out_channel outf in
    let formatter', close_package = match !pfile with
      | "" -> formatter, fun () -> ()
      | pkg ->
        if !ofile = "" then begin
          eprintf
            "Option `-p (--package)` must be used in conjunction with option \
             `-o (--output)`.";
          exit 1
        end;
        let f = open_out (pkg ^ ".sty") in
        formatter_of_out_channel f, fun () -> close_out f
    in
    formatter_package := formatter';
    let p = match !mode with
      | Plain Default ->
        (module Printers.Default : GenericPrinter.PRINTER)
      | Plain EBNF ->
        (module Printers.Ebnf)
      | Latex Tabular ->
        (module Printers.LatexTabular)
      | Latex Syntax ->
        (module Printers.LatexSyntax)
      | Latex Backnaur ->
        (module Printers.LatexBacknaur)
      | Html ->
        if !css then (module Printers.HtmlCss) else (module Printers.Html)
    in
    let module P = (val p: GenericPrinter.PRINTER) in
    let print symbols = P.print_spec symbols formatter in
    let files = rev !ifiles in
    let infs = map open_in files in
    let lexbufs = map Lexing.from_channel infs in
    let close () = iter close_in infs; close_out outf; close_package () in
    c, combine files lexbufs, print, close
  with Sys_error s ->
    eprintf "System Error: %s@." s;
    exit 1

(** @return the obtained grammars per input file / lexer buffer.  *)
let parse (_, lexbuf as fl) =
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
  let c, lexbufs, print, close = get () in
  try
    let s = map parse lexbufs |> concat in
    let symbols = Scan.scan s in
    s
    |> Normalize.normalize
    |> Transform.transform symbols
    |> (if !mode = Plain EBNF then Specialize.specialize symbols else fun s -> s)
    |> Reduce.reduce !inline
    |> print symbols;
    close ();
    exit c
  with
  | Sys_error s ->
    eprintf "System Error: %s@." s;
    exit 1
