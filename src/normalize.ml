(** This module provides a way to "flatten" the groups of productions. *)

open Ast
open List

(** Normalize an actual.
    Note that the anonymous rules are normalized in the same ways as
    the groups, see {!normalize_group}. *)
let rec normalize_actual = function
  | Symbol (s, xs) ->
    Symbol (s, map normalize_actual xs)
  | Modifier (x, m) ->
    Modifier (normalize_actual x, m)
  | Anonymous gs ->
    let gs = fold_right normalize_group gs [] in
    Anonymous gs

(** Normalize a production by normalizing its actuals. *)
and normalize_production p =
  map normalize_actual p

(** Normalize a group.
    Each production of the group is normalized then inserted in a new
    singleton group. As a result, [normalize_group] returns a list of groups
    with only one production each. *)
and normalize_group productions grs =
  let productions = map normalize_production productions in
  let ps = map (fun x -> [x]) productions in
  ps @ grs

(** Normalize a rule by normalizing its groups.
    In general, the number of groups {b increases}. *)
let normalize_rule r =
  let groups = fold_right normalize_group r.groups [] in
  { r with groups }

(** Normalize the grammar by normalizing its rules. *)
let normalize = map normalize_rule
