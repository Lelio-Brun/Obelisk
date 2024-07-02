(** Pattern-recognition and inlining.  *)

open Lazy
open ExtendedAst

(** {2 Rewriting}  *)

(** [subst (xs, p) ys] substitutes the formal parameters [xs] with the actual
    parameters [ys] in the pattern [p].*)
let subst (xs, p) ys =
  let s = Subst.make_subst xs ys in
  Pattern (map_pattern (Subst.subst_actual s) p)

(** Inline the recognized patterns in an actual.
    If the actual is a symbol [s], then we look in the map if [s] should be
    inlined. In this case, we use {!subst} to perform the inlining, that
    is to instantiate the recognized pattern obtained in the map with the actual
    parameters of [s]. *)

let rec rewrite_actual rws = function
  | Symbol (s, xs) ->
    let xs = List.map (rewrite_actual rws) xs in
    begin try
        let rw = Subst.M.find s rws in
        subst rw xs
      with Not_found -> Symbol (s, xs)
    end
  | Pattern p ->
    Pattern (map_pattern (rewrite_actual rws) p)
  | Modifier (x, m) ->
    Modifier (rewrite_actual rws x, m)
  | Anonymous ps ->
    Anonymous (List.map (rewrite_production rws) ps)

(** Inline the recognized patterns in a production. *)
and rewrite_production rws =
  List.map (rewrite_actual rws)

(** Inline the recognized patterns in a rule. *)
let rewrite_rule rws r =
  let prods = List.map (rewrite_production rws) r.prods in
  { r with prods }

(** Inline the recognized patterns in the grammar. *)
let rewrite rws =
  List.map (rewrite_rule rws)

(** {2 Reducing}  *)

(** {3 Pattern recognition}  *)

(** A lazy "monadic" composition operator for the [option] monad.
    [compose x y] returns [y] if [x] fails otherwise it returns [x]. *)
let compose x y =
  match force x with
  | None -> y
  | _ -> x

(** An infix notation for {!compose}.  *)
let (@@) = compose

(** Define a recursive equivalence between rules by
    transitivity.

    [alias rules r r'] is [true] if [r = r'] or if the rule of name [r'] in
    [rules] is made of a unique production consisting of a unique symbol
    recursively equivalent to [r].  *)
let alias rules r =
  let rec eq r' =
    r = r' ||
    match Common.find_rule r' rules with
    | Some { prods = [[Symbol (s, _)]]; _ } -> eq s
    | _ -> false
  in
  eq

(** Define an equivalence between formal and actual parameters. *)
let equal_params = List.for_all2 (fun x y -> y = Symbol (x, []))

(** [not_occurs s a] is [true] when the symbol [s] does not occur in
    the actual [a]. *)
let rec not_occurs s a =
  let not_occurs_pattern =
    let n = not_occurs s in
    function
    | Option x | List x | NEList x -> n x
    | Pair (x, y) | Preceded (x, y) | Terminated (x, y)
    | SepList (x, y) | SepNEList (x, y) -> n x && n y
    | SepPair (x, y, z) | Delimited (x, y, z) -> n x && n y && n z
  in
  match a with
  | Symbol (f, xs) -> f <> s && List.for_all (not_occurs s) xs
  | Pattern p -> not_occurs_pattern p
  | Modifier (x, _) -> not_occurs s x
  | Anonymous ps -> List.for_all (List.for_all (not_occurs s)) ps

(** Merely wrap the arguments under a [Some]. *)
let generalize xs p = Some (xs, p)

(** Decide if the rule is a list, that is if it has the following shape:
    {v
l(xs) ::=           or    l(xs) ::=
        | eps                     | `cons`
        | `cons`                  | eps
v}
    where
    - [`cons` = s(ys) :: acts]
    - [s] and [l] are equivalent according to {!alias}
    - [xs] and [ys] are equivalent according to {!equal_params}
    - [s] does {!not_occurs} in [acts] *)
let is_list rules r =
  let g = generalize r.params in
  match r.prods with
  | [[]; cons] | [cons; []] ->
    begin match cons with
      | [] -> None
      | Symbol (s, xs) :: acts
        when alias rules r.name s
          && equal_params r.params xs
          && List.for_all (not_occurs s) acts ->
        g (List (Anonymous [acts]))
      | _ ->
        begin match List.rev cons with
          | Symbol (s, xs) :: acts
            when alias rules r.name s
              && equal_params r.params xs
              && List.for_all (not_occurs s) acts ->
            g (List (Anonymous [List.rev acts]))
          | _ -> None
        end
    end
  | _ -> None

(** Decide if the rule is a separated non empty list,
    that is if it has the following shape:
    {v
nel(xs) ::=           or    nel(xs) ::=
          | `base`                    | `cons`
          | `cons`                    | `base`
v}
    where
    - [`cons` = s(ys) :: acts]
    - [s] and [nel] are equivalent according to {!alias}
    - [xs] and [ys] are equivalent according to {!equal_params}
    - [s] does {!not_occurs} in [acts]
    - [acts = `base`]

    This condition is explained for a left-recursive list but the function
    also copes with right-recursion. *)
let is_nonempty_list rules r =
  let g = generalize r.params in
  let f base cons =
    match cons with
    | [] -> None
    | Symbol (s, xs) :: acts
      when alias rules r.name s
        && equal_params r.params xs
        && List.for_all (not_occurs s) acts
        && base = acts ->
      g (NEList (Anonymous [acts]))
    | _ ->
      begin match List.rev cons with
        | Symbol (s, xs) :: acts
          when alias rules r.name s
            && equal_params r.params xs
            && List.for_all (not_occurs s) acts
            && base = acts ->
          g (NEList (Anonymous [List.rev acts]))
        | _ -> None
      end
  in
  match r.prods with
  | [x; y] -> lazy (f x y) @@ lazy (f y x) |> force
  | _ -> None

(** Decide if the rule is a separated non empty list,
    that is if it has the following shape:
    {v
snel(xs) ::=           or    snel(xs) ::=
           | `base`                     | `cons`
           | `cons`                     | `base`
v}
    where
    - [`cons` = s(ys) :: acts]
    - [s] and [snel] are equivalent according to {!alias}
    - [xs] and [ys] are equivalent according to {!equal_params}
    - [s] does {!not_occurs} in [acts]
    - [acts = sep @ `base`]

    This condition is explained for a left-recursive list but the function
    also copes with right-recursion. *)
let is_sep_nonempty_list rules r =
  let take =
    let rec aux pre n xs = match n, xs with
      | 0, xs -> List.rev pre, xs
      | _, [] -> List.rev pre, []
      | n, x::xs -> aux (x :: pre) (n-1) xs
    in
    aux []
  in
  let g = generalize r.params in
  let f base cons =
    match cons with
    | [] -> None
    | Symbol (s, xs) :: acts
      when alias rules r.name s
        && equal_params r.params xs
        && List.for_all (not_occurs s) acts ->
      let base_r, sep_r = take (List.length base) (List.rev acts) in
      if base = List.rev base_r
      then g (SepNEList (Anonymous [List.rev sep_r], Anonymous [base]))
      else None
    | _ ->
      begin match List.rev cons with
        | Symbol (s, xs) :: acts
          when alias rules r.name s
            && equal_params r.params xs
            && List.for_all (not_occurs s) acts ->
          let base', sep = take (List.length base) (List.rev acts) in
          if base = base'
          then g (SepNEList (Anonymous [sep], Anonymous [base]))
          else None
        | _ -> None
      end
  in
  match r.prods with
  | [x; y] -> lazy (f x y) @@ lazy (f y x) |> force
  | _ -> None

(** Decide if the rule is a separated list,
    that is if it has the following shape:
    {v
sl ::=              or    sl ::=
     | eps                     | snel(...)
     | snel(...)               | eps
v}
    where [snel] is recognized as a [separated_nonempty_list] by
    {!is_sep_nonempty_list}.
*)
let is_sep_list rules r =
  let g = generalize r.params in
  match r.prods with
  | [[]; [Symbol (s, xs)]] | [[Symbol (s, xs)]; []] ->
    begin match Common.find_rule s rules with
      | Some r' ->
        begin match is_sep_nonempty_list rules r' with
          | Some rw ->
            begin match subst rw xs with
              | Pattern (SepNEList (sep, x)) -> g (SepList (sep, x))
              | _ -> None
            end
          | _ -> None
        end
      | None -> None
    end
  | _ -> None

(** Decide if the rule is an optional, that is if it has the following shape:
    {v
r ::=       or    r ::=
   | eps              | `some`
   | `some`           | eps
v}
*)
let is_option r =
  match r.prods with
  | [[]; some] | [some; []] ->
    if some = [] then None else generalize r.params (Option (Anonymous [some]))
  | _ -> None

(** Decide if the rule is a standard pattern. *)
let recognize rules r =
  lazy (is_list rules r)
  @@ lazy (is_nonempty_list rules r)
  @@ lazy (is_sep_nonempty_list rules r)
  @@ lazy (is_sep_list rules r)
  @@ lazy (is_option r)
  |> force

(** {3 Rule reducing}  *)

(** Replace the right-hand side of a rule with the given pattern. *)
let replace_prods r (_, p) =
  { r with prods = [[Pattern p]] }

(** [reduce_rule inline rules r (rs, rws)] returns a pair of a list of "reduced"
    rules and a map from identifiers to a pair of a list of parameters and
    a pattern, that is a map of recognized patterns.

    If the rule [r] is recognized as a standard pattern then
    - if [inline] is [true]: [r] is skipped and added to the map of recognized
      patterns along with the according recognized pattern.
    - if [inline] is [false]: the right-hand side of [r] is replaced by the
      recognized pattern. *)
let reduce_rule inline rules r (rs, rws) =
  match recognize rules r with
  | Some rw ->
    if inline then (rs, Subst.M.add r.name rw rws)
    else replace_prods r rw :: rs, rws
  | None ->
    r :: rs, rws

(** {2 All together} *)

(** [reduce inline s] returns the grammar [s] in which the right-hand sides
    recognized as standard patterns are replaced by an instance of this pattern
    (list, optional, ...).

    Moreover, if [inline] is [true] then the concerned
    rules are deleted and inlined at each of their instances. *)
let reduce inline s =
  let rules, rws = List.fold_right (reduce_rule inline s) s
      ([], Subst.M.empty)
  in
  if inline then rewrite rws rules else rules
