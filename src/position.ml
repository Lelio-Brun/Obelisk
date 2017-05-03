open Lexing
open Printf

type position = {
  start_p: Lexing.position;
  end_p: Lexing.position
}

let dummy = {
  start_p = dummy_pos;
  end_p   = dummy_pos
}

let from start_p end_p = { start_p; end_p }

let err_loc file {start_p; end_p} =
  let l = start_p.pos_lnum in
  let start_c = start_p.pos_cnum - start_p.pos_bol in
  let end_c = end_p.pos_cnum - end_p.pos_bol - 1 in
  if start_c = end_c
  then eprintf "File \"%s\", line %d, character %d:\n" !file l start_c
  else eprintf "File \"%s\", line %d, characters %d-%d:\n" !file l start_c end_c

let get_pos lexbuf =
  from (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)

let err_loc_lexbuf file lexbuf =
  err_loc file (get_pos lexbuf)
