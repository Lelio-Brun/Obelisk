(** A generic module for handling locations of errors. *)

open Lexing
open Printf

(** The error is located between two positions in the buffer. *)
type position = {
  start_p: Lexing.position;     (** The start position. *)
  end_p: Lexing.position        (** The end position. *)
}

(** A "dummy" error location. *)
let dummy = {
  start_p = dummy_pos;
  end_p   = dummy_pos
}

(** Build an error location from the start and end positions. *)
let from start_p end_p = { start_p; end_p }

(** Print a located error message on the standard error output. *)
let err_loc file {start_p; end_p} =
  let l = start_p.pos_lnum in
  let start_c = start_p.pos_cnum - start_p.pos_bol in
  let end_c = end_p.pos_cnum - end_p.pos_bol - 1 in
  if start_c = end_c
  then eprintf "File \"%s\", line %d, character %d:\n" !file l start_c
  else eprintf "File \"%s\", line %d, characters %d-%d:\n" !file l start_c end_c

(** Build an error location from the lexer buffer. *)
let get_pos lexbuf =
  from (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

(** Print an error message located with the lexer buffer. *)
let err_loc_lexbuf file lexbuf =
  err_loc file (get_pos lexbuf)
