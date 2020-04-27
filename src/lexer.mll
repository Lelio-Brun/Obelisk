{
open Printf
open Lexing
open Parser

exception LexingError of string
let error s = raise (LexingError s)

let newline lb p = new_line lb; p lb

let tokens = Hashtbl.create 128

let prologue = ref true
}

let blank = [' ' '\t' '\012']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let lalpha = (['a'-'z'] | '_')
let ualpha = ['A'-'Z']
let alpha = (lalpha | ualpha)*
let id_body = (alpha | digit | '_')
let lid = lalpha id_body*
let uid = ualpha id_body*
let qid = ( "\"" ( [' ' - '~'] # ['"' '\\'] + ) "\"" )

(* for point-free actions *)
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let op         =
  symbolchar+
  (* An approximation of OCaml's rules. *)
let whitespace = [ ' ' '\t' '\n' ]

rule main = parse
  | newline  { newline lexbuf main }
  | "(*"     { ocaml_comment lexbuf; main lexbuf }
  | "\""     { ocaml_string lexbuf; main lexbuf }
  | "%token" { token_aliases lexbuf }
  | "%%"     { prologue := false; token lexbuf }
  | eof      { EOF }
  | _        { main lexbuf }

and token_aliases = parse
 | newline                      { newline lexbuf main }
 | blank+                       { token_aliases lexbuf }
 | (uid as t) blank+ (qid as a) { Hashtbl.add tokens Re.Str.(global_replace (regexp "\"") "" a) t; token_aliases lexbuf }
 | _                            { token_aliases lexbuf }

and token = parse
  | newline   { newline lexbuf token }
  | "%%"      { EOF }
  | blank+    { token lexbuf }
  | "/*"      { c_comment lexbuf }
  | "//"      { cpp_comment lexbuf }
  | "(*"      { ocaml_comment lexbuf; token lexbuf }
  | "{"       { ocaml_code lexbuf }
  | "<"       { pointfree_action lexbuf }
  | "("       { LPAR }
  | ")"       { RPAR }
  | "|"       { BAR }
  | ":"       { COLON }
  | "="       { EQ }
  | ":="      { COLONEQ }
  | "=="      { EQEQ }
  | ","       { COMMA }
  | ";"       { SEMICOLON }
  | "~"       { TILDE }
  | "_"       { UNDERSCORE }
  | "?"       { OPT }
  | "+"       { PLUS }
  | "*"       { STAR }
  | "%public" { PUBLIC }
  | "%inline" { INLINE }
  | "%prec"   { PREC }
  | "let"     { LET }
  | "[@"      { attribute lexbuf }
  | eof       { EOF }
  | lid as s  { LID s }
  | uid as s  { UID s }
  | qid as s  { QID Re.Str.(global_replace (regexp "\"") "" s) }
  | _ as c    { error (sprintf "illegal character %C" c) }

and ocaml_code = parse
  | newline { newline lexbuf ocaml_code }
  | "(*"    { ocaml_comment lexbuf; ocaml_code lexbuf }
  | "\""    { ocaml_string lexbuf; ocaml_code lexbuf }
  | "}"     { ACTION }
  | "{"     { ignore (ocaml_code lexbuf); ocaml_code lexbuf}
  | eof     { error "Ocaml action not terminated"}
  | _       { ocaml_code lexbuf }

and ocaml_string = parse
  | newline        { newline lexbuf ocaml_string }
  | "\\" _         { ocaml_string lexbuf }
  | "\""           { () }
  | _              { ocaml_string lexbuf }

and pointfree_action = parse
  | whitespace* (lowercase | uppercase | '`') (identchar | '.')* whitespace* ">"
  | whitespace* '(' op ')' whitespace* ">"
  | ">"    { POINTFREEACTION }
  | eof    { error "Point-free action not terminated"}
  | _ as c { error (sprintf "illegal character %C" c) }

and c_comment = parse
  | newline { newline lexbuf c_comment  }
  | "*/"    { token lexbuf }
  | eof     { error "C-style comment not terminated" }
  | _       { c_comment lexbuf}

and cpp_comment = parse
  | newline { newline lexbuf token }
  | eof     { EOF }
  | _       { cpp_comment lexbuf }

and ocaml_comment = parse
  | newline { newline lexbuf ocaml_comment  }
  | "*)"    { () }
  | "(*"    { ocaml_comment lexbuf; ocaml_comment lexbuf }
  | eof     { error "Ocaml-style comment not terminated" }
  | _       { ocaml_comment lexbuf}

and attribute = parse
  | newline { newline lexbuf attribute }
  | "]"     { ATTRIBUTE }
  | "["     { ignore (attribute lexbuf); attribute lexbuf }
  | eof     { error "Attribute not terminated" }
  | _       { attribute lexbuf }

{
  let init () =
    prologue := true

  let lexer lexbuf =
    (if !prologue then main else token) lexbuf
}
