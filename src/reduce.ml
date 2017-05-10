open ExtendedAst

module OrdString = struct
  type t = string
  let compare = Pervasives.compare
end
module M = Map.Make(OrdString)

let compose x y =
  match x with
  | None -> y
  | _ -> x

let (@@) = compose

let generalize xs p = Some (xs, p)

let make_subst =
  List.fold_left2 (fun s x y -> M.add x y s) M.empty

let rec subst_actual s = function
  | Symbol (f, []) ->
    begin try
        M.find f s
      with Not_found -> Symbol (f, [])
    end
  | Symbol (f, xs) ->
    Symbol (f, List.map (subst_actual s) xs)
  | Pattern p ->
    Pattern (subst_pattern s p)
  | Modifier (x, m) ->
    Modifier (subst_actual s x, m)
  | Anonymous ps ->
    Anonymous (List.map (subst_production s) ps)

and subst_pattern s =
  let r = subst_actual s in
  function
  | Option x -> Option (r x)
  | Pair (x, y) -> Pair (r x, r y)
  | SepPair (x, sep, y) -> SepPair (r x, r sep, r y)
  | Preceded (o, x) -> Preceded (r o, r x)
  | Terminated (x, c) -> Terminated (r x, r c)
  | Delimited (o, x, c) -> Delimited (r o, r x, r c)
  | List x -> List (r x)
  | NEList x -> NEList (r x)
  | SepList (sep, x) -> SepList (r sep, r x)
  | SepNEList (sep, x) -> SepNEList (r sep, r x)

and subst_production s =
  List.map (subst_actual s)

let subst (xs, p) ys =
  let s = make_subst xs ys in
  let f = subst_actual s in
  let p = match p with
    | Option x -> Option (f x)
    | Pair (x, y) -> Pair (f x, f y)
    | SepPair (x, sep, y) -> SepPair (f x, f sep, f y)
    | Preceded (o, x) -> Preceded (f o, f x)
    | Terminated (x, c) -> Terminated (f x, f c)
    | Delimited (o, x, c) -> Delimited (f o, f x, f c)
    | List x -> List (f x)
    | NEList x -> NEList (f x)
    | SepList (sep, x) -> SepList (f sep, f x)
    | SepNEList (sep, x) -> SepNEList (f sep, f x)
  in
  Pattern p

let equal_params = List.for_all2 (fun x y -> y = Symbol (x, []))

let rec not_occurs s = function
  | Symbol (f, xs) -> f <> s && List.for_all (not_occurs s) xs
  | Pattern p -> not_occurs_pattern s p
  | Modifier (x, _) -> not_occurs s x
  | Anonymous ps -> List.for_all (List.for_all (not_occurs s)) ps

and not_occurs_pattern s =
  let n = not_occurs s in
  function
  | Option x | List x | NEList x -> n x
  | Pair (x, y) | Preceded (x, y) | Terminated (x, y)
  | SepList (x, y) | SepNEList (x, y) -> n x && n y
  | SepPair (x, y, z) | Delimited (x, y, z) -> n x && n y && n z

let is_list r =
  let g = generalize r.params in
  match r.prods with
  | [[]; cons] | [cons; []] ->
    begin match cons with
      | [] -> None
      | Symbol (s, xs) :: acts
        when s = r.name && equal_params r.params xs
             && List.for_all (not_occurs s) acts ->
        g (List (Anonymous [acts]))
      | _ ->
        begin match List.rev cons with
          | Symbol (s, xs) :: acts
            when s = r.name && equal_params r.params xs
                 && List.for_all (not_occurs s) acts ->
            g (List (Anonymous [List.rev acts]))
          | _ -> None
        end
    end
  | _ -> None

let is_nonempty_list r =
  let g = generalize r.params in
  let f base cons =
    match cons with
    | [] -> None
    | Symbol (s, xs) :: acts
      when s = r.name && equal_params r.params xs
           && List.for_all (not_occurs s) acts
           && base = acts ->
      g (NEList (Anonymous [acts]))
    | _ ->
      begin match List.rev cons with
        | Symbol (s, xs) :: acts
          when s = r.name && equal_params r.params xs
               && List.for_all (not_occurs s) acts
               && base = acts ->
          g (NEList (Anonymous [List.rev acts]))
        | _ -> None
      end
  in
  match r.prods with
  | [x; y] -> f x y @@ f y x
  | _ -> None

let take =
  let rec aux pre n xs = match n, xs with
    | 0, xs -> List.rev pre, xs
    | _, [] -> List.rev pre, []
    | n, x::xs -> aux (x :: pre) (n-1) xs
  in
  aux []

let is_sep_nonempty_list r =
  let g = generalize r.params in
  let f base cons =
    match cons with
    | [] -> None
    | Symbol (s, xs) :: acts
      when s = r.name && equal_params r.params xs
           && List.for_all (not_occurs s) acts ->
      let base_r, sep_r = take (List.length base) (List.rev acts) in
      if base = List.rev base_r
      then g (SepNEList (Anonymous [List.rev sep_r], Anonymous [base]))
      else None
    | _ ->
      begin match List.rev cons with
        | Symbol (s, xs) :: acts
          when s = r.name && equal_params r.params xs
               && List.for_all (not_occurs s) acts ->
          let base', sep = take (List.length base) (List.rev acts) in
          if base = base'
          then g (SepNEList (Anonymous [sep], Anonymous [base]))
          else None
        | _ -> None
      end
  in
  match r.prods with
  | [x; y] -> f x y @@ f y x
  | _ -> None

let find_rule r rules =
  try
    Some (List.find (fun {name} -> name = r) rules)
  with Not_found -> None

let is_sep_list rules r =
  let g = generalize r.params in
  match r.prods with
  | [[]; [Symbol (s, xs)]] | [[Symbol (s, xs)]; []] ->
    begin match find_rule s rules with
      | Some r' ->
        begin match is_sep_nonempty_list r' with
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

let is_option r =
  match r.prods with
  | [[]; some] | [some; []] ->
    if some = [] then None else generalize r.params (Option (Anonymous [some]))
  | _ -> None

let recognize rules r =
  is_list r
  @@ is_nonempty_list r
  @@ is_sep_nonempty_list r
  @@ is_sep_list rules r
  @@ is_option r

let reduce_rule rules r (rs, rws) =
  match recognize rules r with
  | Some rw -> rs, M.add r.name rw rws
  | None -> r :: rs, rws

let rec rewrite_actual rws = function
  | Symbol (s, xs) ->
    let xs = List.map (rewrite_actual rws) xs in
    begin try
        let rw = M.find s rws in
        subst rw xs
      with Not_found -> Symbol (s, xs)
    end
  | Pattern p ->
    Pattern (rewrite_pattern rws p)
  | Modifier (x, m) ->
    Modifier (rewrite_actual rws x, m)
  | Anonymous ps ->
    Anonymous (List.map (rewrite_production rws) ps)

and rewrite_pattern rws =
  let r = rewrite_actual rws in
  function
  | Option x -> Option (r x)
  | Pair (x, y) -> Pair (r x, r y)
  | SepPair (x, sep, y) -> SepPair (r x, r sep, r y)
  | Preceded (o, x) -> Preceded (r o, r x)
  | Terminated (x, c) -> Terminated (r x, r c)
  | Delimited (o, x, c) -> Delimited (r o, r x, r c)
  | List x -> List (r x)
  | NEList x -> NEList (r x)
  | SepList (sep, x) -> SepList (r sep, r x)
  | SepNEList (sep, x) -> SepNEList (r sep, r x)

and rewrite_production rws =
  List.map (rewrite_actual rws)

let rewrite_rule rws r =
  let prods = List.map (rewrite_production rws) r.prods in
  { r with prods }

let rewrite rws =
  List.map (rewrite_rule rws)

let reduce s =
  let rules, rws = List.fold_right (reduce_rule s) s ([], M.empty) in
  rewrite rws rules
