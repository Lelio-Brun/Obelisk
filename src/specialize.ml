(** Specialize functional non-terminals. *)

open List
open ExtendedAst

module S = Set.Make(String)
module M = Map.Make(Int)

let hash = Hashtbl.hash

let fresh =
  let h = Hashtbl.create 32 in
  fun f ->
    let n = match Hashtbl.find_opt h f with
      | Some n -> n + 1
      | None -> 0
    in
    Hashtbl.replace h f n;
    Format.sprintf "%s_%d" f n

(** Specialize an actual.
    If a symbol is functional, we introduce a new corresponding specialized
    rule. *)
let rec specialize_actual symbols spec new_rules_map =
  let specialize = specialize_actual symbols spec in
  function
  | Symbol (_, []) as s ->
    new_rules_map, s
  | Symbol (f, xs) when Common.Symbols.is_defined f symbols <> None ->
    let h = hash (f, xs) in
    let new_rules_map, r = match M.find_opt h new_rules_map with
      | Some r ->
        new_rules_map, r
      | None ->
        begin match Common.find_rule f spec with
          | Some r ->
            let name = fresh f in
            let new_rules_map, xs = specialize_actuals symbols spec
                new_rules_map xs
            in
            let s = Subst.make_subst r.params xs in
            let prods = map (map (Subst.subst_actual s)) r.prods in
            let r = { name; prods; params = [] } in
            (* to handle recursive rules *)
            let new_rules_map = M.add h r new_rules_map in
            let new_rules_map, prods = specialize_productions symbols spec
                new_rules_map prods
            in
            let r = { name; prods; params = [] } in
            M.add h r new_rules_map, r
          | None -> assert false
        end
    in
    new_rules_map, Symbol (r.name, [])
  | Symbol (f, xs) ->
    let new_rules_map, xs = specialize_actuals symbols spec new_rules_map xs in
    new_rules_map, Symbol (f, xs)
  | Pattern p ->
    let new_rules_map, p = fold_map_pattern specialize new_rules_map p in
    new_rules_map, Pattern p
  | Modifier (x, m) ->
    let new_rules_map, x = specialize new_rules_map x in
    new_rules_map, Modifier (x, m)
  | Anonymous p ->
    let new_rules_map, p = specialize_productions symbols spec new_rules_map p in
    new_rules_map, Anonymous p

(** Specialize actuals. *)
and specialize_actuals symbols spec new_rules_map xs =
  let new_rules_map, xs = List.fold_left (fun (new_rules_map, actuals) a ->
      let new_rules_map, a = specialize_actual symbols spec new_rules_map a in
      new_rules_map, a :: actuals)
      (new_rules_map, []) xs
  in
  new_rules_map, List.rev xs

(** Specialize a production by specializing its actuals. *)
and specialize_production symbols spec (new_rules_map, prods) p =
  let new_rules_map, p = specialize_actuals symbols spec new_rules_map p in
  new_rules_map, p :: prods

(** Specialize productions. *)
and specialize_productions symbols spec new_rules_map prods =
  let new_rules_map, prods = fold_left (specialize_production symbols spec)
      (new_rules_map, []) prods
  in
  new_rules_map, rev prods

(** Specialize a rule by specializing its productions. *)
let specialize_rule symbols spec (new_rules_map, rules) r =
  if r.params = [] then
    let new_rules_map, prods = specialize_productions symbols spec new_rules_map
        r.prods
    in
    new_rules_map,
    { r with prods } :: rules
  else
    new_rules_map, rules

(** Specialize a grammar by specializing its rules, producing new specialized
    rules. *)
let specialize symbols spec =
  let new_rules_map, spec = fold_left (specialize_rule symbols spec)
      (M.empty, []) spec
  in
  rev spec @ map snd (M.bindings new_rules_map)
