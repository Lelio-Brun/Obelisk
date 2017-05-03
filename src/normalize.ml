open Ast

let normalize_group productions grs =
  let ps = List.map (fun x -> [x]) productions in
  ps @ grs

let normalize_rule r =
  let groups = List.fold_right normalize_group r.groups [] in
  { r with groups }

let normalize = List.map normalize_rule
