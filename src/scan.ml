(** Build the set of symbols appearing in a grammar. *)

open Ast
open Common
open List

(** [add_defined symbols r] adds to the [symbols] set the name of [r]
    (left-hand side) as a non terminal (see {!Common.Symbols.def_non_term})
    if the rule has no parameter or as a functional non terminal along with
    its parameters (see {!Common.Symbols.def_fun}) otherwise. *)
let add_defined symbols {name; params; _} =
  match params with
   | [] -> Symbols.def_non_term name symbols
   | _ -> Symbols.def_fun name params symbols

(** [add_terminal symbols r] recursively scans the right-hand side of [r] to add
    the symbols which are not already "defined" in [symbols]
    (see {!Common.Symbols.is_defined}) neither parameters as terminals
    (see {!Common.Symbols.def_term}). *)
let add_terminal symbols {params; groups; _} =
  let rec add_terminal_actual symbols = function
    | Symbol (s, ps) ->
      let symbols = fold_left add_terminal_actual symbols ps in
      if Symbols.is_defined s symbols = None
      && String.uppercase_ascii s = s
      && ps = []
      && not (List.mem s params)
      then Symbols.def_term s symbols else symbols
    | Modifier (a, _) ->
      add_terminal_actual symbols a
    | Anonymous gs ->
      fold_left add_terminal_group symbols gs
  and add_terminal_prod symbols = fold_left add_terminal_actual symbols
  and add_terminal_group symbols = fold_left add_terminal_prod symbols
  in
  fold_left add_terminal_group symbols groups

(** [scan s] first gets the defined symbols of [s] then its terminals and
    returns the whole set.  *)
let scan s =
  let symbols = fold_left add_defined Symbols.empty s in
  fold_left add_terminal symbols s
