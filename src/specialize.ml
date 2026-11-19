(** Specialize functional non-terminals. *)

open List
open ExtendedAst
module S = Set.Make (String)

module M = Map.Make (struct
  type t = string * actual list

  let compare = Stdlib.compare
end)

let fresh =
  let h = Hashtbl.create 32 in
  fun symbols f ->
    let n, fn =
      match Hashtbl.find_opt h f with
      | Some n ->
          let rec next n =
            let n = n + 1 in
            let fn = Format.sprintf "%s_%d" f n in
            if Common.Symbols.is_symbol fn symbols then next (n + 1) else (n, fn)
          in
          next n
      | None -> (0, Format.sprintf "%s_0" f)
    in
    Hashtbl.replace h f n;
    fn

(** Specialize an actual. If a symbol is functional, we introduce a new
    corresponding specialized rule. *)
let rec specialize_actual symbols spec new_rules_map rule_index =
  let specialize = specialize_actual symbols spec in
  function
  | Symbol (_, []) as s -> (new_rules_map, rule_index, s)
  | Symbol (f, xs) when Common.Symbols.is_defined f symbols <> None ->
      let key = (f, xs) in
      let new_rules_map, rule_index, r =
        match M.find_opt key new_rules_map with
        | Some (_, r) -> (new_rules_map, rule_index, r)
        | None ->
            begin match Common.find_rule f spec with
            | Some r ->
                let name = fresh symbols f in
                let s = Subst.make_subst r.params xs in
                let prods = map (map (Subst.subst_actual s)) r.prods in
                let r = { name; prods; params = [] } in
                (* to handle recursive rules *)
                let new_rules_map = M.add key (rule_index, r) new_rules_map in
                let new_rules_map, next_index, prods =
                  specialize_productions symbols spec new_rules_map
                    (rule_index + 1) prods
                in
                let r = { name; prods; params = [] } in
                (M.add key (rule_index, r) new_rules_map, next_index, r)
            | None -> assert false
            end
      in
      (new_rules_map, rule_index, Symbol (r.name, []))
  | Symbol (f, xs) ->
      let new_rules_map, rule_index, xs =
        specialize_actuals symbols spec new_rules_map rule_index xs
      in
      (new_rules_map, rule_index, Symbol (f, xs))
  | Pattern p ->
      let (new_rules_map, rule_index), p =
        fold_map_pattern
          (fun (new_rules_map, rule_index) x ->
            let new_rules_map, rule_index, x =
              specialize new_rules_map rule_index x
            in
            ((new_rules_map, rule_index), x))
          (new_rules_map, rule_index)
          p
      in
      (new_rules_map, rule_index, Pattern p)
  | Modifier (x, m) ->
      let new_rules_map, rule_index, x =
        specialize new_rules_map rule_index x
      in
      (new_rules_map, rule_index, Modifier (x, m))
  | Anonymous p ->
      let new_rules_map, rule_index, p =
        specialize_productions symbols spec new_rules_map rule_index p
      in
      (new_rules_map, rule_index, Anonymous p)

(** Specialize actuals. *)
and specialize_actuals symbols spec new_rules_map rule_index xs =
  let new_rules_map, rule_index, xs =
    List.fold_left
      (fun (new_rules_map, rule_index, actuals) a ->
        let new_rules_map, rule_index, a =
          specialize_actual symbols spec new_rules_map rule_index a
        in
        (new_rules_map, rule_index, a :: actuals))
      (new_rules_map, rule_index, [])
      xs
  in
  (new_rules_map, rule_index, List.rev xs)

(** Specialize a production by specializing its actuals. *)
and specialize_production symbols spec (new_rules_map, rule_index, prods) p =
  let new_rules_map, rule_index, p =
    specialize_actuals symbols spec new_rules_map rule_index p
  in
  (new_rules_map, rule_index, p :: prods)

(** Specialize productions. *)
and specialize_productions symbols spec new_rules_map rule_index prods =
  let new_rules_map, rule_index, prods =
    fold_left
      (specialize_production symbols spec)
      (new_rules_map, rule_index, [])
      prods
  in
  (new_rules_map, rule_index, rev prods)

(** Specialize a rule by specializing its productions. *)
let specialize_rule symbols spec (new_rules_map, rule_index, rules) r =
  if r.params = [] then
    let new_rules_map, rule_index, prods =
      specialize_productions symbols spec new_rules_map rule_index r.prods
    in
    (new_rules_map, rule_index, { r with prods } :: rules)
  else (new_rules_map, rule_index, rules)

(** Specialize a grammar by specializing its rules, producing new specialized
    rules. *)
let specialize symbols spec =
  let new_rules_map, _, spec =
    fold_left (specialize_rule symbols spec) (M.empty, 0, []) spec
  in
  let new_rules =
    M.bindings new_rules_map |> map snd
    |> sort (fun (i, _) (j, _) -> Int.compare i j)
    |> map snd
  in
  rev spec @ new_rules
