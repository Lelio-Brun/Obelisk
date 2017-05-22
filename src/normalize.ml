open Ast
open List

let rec normalize_actual = function
  | Symbol (s, xs) ->
    Symbol (s, map normalize_actual xs)
  | Modifier (x, m) ->
    Modifier (normalize_actual x, m)
  | Anonymous gs ->
    let gs = fold_right normalize_group gs [] in
    Anonymous gs

and normalize_production p =
  map normalize_actual p

and normalize_group productions grs =
  let productions = map normalize_production productions in
  let ps = map (fun x -> [x]) productions in
  ps @ grs

let normalize_rule r =
  let groups = fold_right normalize_group r.groups [] in
  { r with groups }

let normalize = map normalize_rule
