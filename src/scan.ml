open Ast
open Common
open List

let add_non_terminal symbols {name; params} =
  (match params with
   | [] -> Symbols.def_non_term
   | _ -> Symbols.def_fun)
    name symbols

let add_terminal symbols {groups} =
  let rec add_terminal_actual symbols = function
    | Symbol (s, ps) ->
      let symbols = fold_left add_terminal_actual symbols ps in
      if not (Symbols.is_defined s symbols)
      && String.uppercase_ascii s = s
      && ps = []
      then Symbols.def_term s symbols else symbols
    | Modifier (a, _) ->
      add_terminal_actual symbols a
    | Anonymous gs ->
      fold_left add_terminal_group symbols gs
  and add_terminal_prod symbols = fold_left add_terminal_actual symbols
  and add_terminal_group symbols = fold_left add_terminal_prod symbols
  in
  fold_left add_terminal_group symbols groups

let scan s =
  let symbols = fold_left add_non_terminal Symbols.empty s in
  fold_left add_terminal symbols s
