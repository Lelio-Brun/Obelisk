(** Symbol substitution. *)

open ExtendedAst

(** A map with identifiers keys. *)
module M = Map.Make(String)

(** Performs a substitution over an actual.
    Only non functional symbols are substituted.  *)
let rec subst_actual s a =
  let subst_production = List.map (subst_actual s) in
  match a with
  | Symbol (f, []) ->
    begin try
        M.find f s
      with Not_found -> Symbol (f, [])
    end
  | Symbol (f, xs) ->
    Symbol (f, List.map (subst_actual s) xs)
  | Pattern p ->
    Pattern (map_pattern (subst_actual s) p)
  | Modifier (x, m) ->
    Modifier (subst_actual s x, m)
  | Anonymous ps ->
    Anonymous (List.map subst_production ps)

(** [make_subst xs ys] builds a substitution that is a map linking each element
    of [xs] to the corresponding (by index) element of [ys]. *)
let make_subst xs ys =
  List.fold_left2 (fun s x y -> M.add x y s) M.empty xs ys
