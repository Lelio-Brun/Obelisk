open Ast

let rec normalize_actual = function
  | Symbol (s, xs) ->
    Symbol (s, List.map normalize_actual xs)
  | Modifier (x, m) ->
    Modifier (normalize_actual x, m)
  | Anonymous gs ->
    let gs = List.fold_right normalize_group gs [] in
    Anonymous gs

and normalize_production p =
  List.map normalize_actual p

and normalize_group productions grs =
  let productions = List.map normalize_production productions in
  let ps = List.map (fun x -> [x]) productions in
  ps @ grs

let normalize_rule r =
  let groups = List.fold_right normalize_group r.groups [] in
  { r with groups }

let normalize = List.map normalize_rule
