{
open Printf
open Lexing
open Parser

exception LexingError of string
let error s = raise (LexingError s)

let newline lb p = new_line lb; p lb

let prologue = ref true
}

let blank = [' ' '\t' ';' '\012']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let lalpha = ['a'-'z']
let ualpha = ['A'-'Z']
let alpha = (lalpha | ualpha)*
let id_body = (alpha | digit | '_')
let lid = lalpha id_body*
let uid = ualpha id_body*

rule main = parse
  | newline { newline lexbuf main }
  | "%%"    { prologue := false; token lexbuf }
  | eof     { EOF }
  | _       { main lexbuf }

and token = parse
  | newline             { newline lexbuf token }
  | "%%"                { EOF }
  | blank+              { token lexbuf }
  | "/*"                { c_comment lexbuf }
  | "//"                { cpp_comment lexbuf }
  | "(*"                { ocaml_comment lexbuf; token lexbuf }
  | "{"                 { ocaml_code lexbuf }
  | "("                 { LPAR }
  | ")"                 { RPAR }
  | "|"                 { BAR }
  | ":"                 { COLON }
  | "="                 { EQ }
  | ","                 { COMMA }
  | "?"                 { OPT }
  | "+"                 { PLUS }
  | "*"                 { STAR }
  | "%public"           { PUBLIC }
  | "%inline"           { INLINE }
  | "%prec"             { PREC }
  | "[@"                { attribute lexbuf }
  | eof                 { EOF }
  | lid as s            { LID s }
  | uid as s            { UID s }
  | _ as c              { error (sprintf "illegal character %C" c) }

and ocaml_code = parse
  | newline { newline lexbuf ocaml_code }
  | "}"     { ACTION }
  | "{"     { ignore (ocaml_code lexbuf); ocaml_code lexbuf}
  | eof     { error "Ocaml action not terminated"}
  | _       { ocaml_code lexbuf }

and c_comment = parse
  | newline { newline lexbuf c_comment  }
  | "*/"    { token lexbuf }
  | "/*"    { error "C-style comments cannot be nested" }
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
