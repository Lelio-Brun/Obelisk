open ExtendedAst

let rec transform_actual = function
  | Ast.Symbol (s, xs) -> transform_symbol s xs
  | Ast.Modifier (x, m) -> transform_modifier x m
  | Ast.Anonymous gs -> transform_anonymous gs

and transform_symbol s xs =
  let t = transform_actual in
  match s, xs with
  | ("option" | "ioption" | "boption" | "loption"), [x] ->
    Pattern (Option (t x))
  | "pair", [x; y] ->
    Pattern (Pair (t x, t y))
  | "separated_pair", [x; sep; y] ->
    Pattern (SepPair (t x, t sep, t y))
  | "preceded", [o; x] ->
    Pattern (Preceded (t o, t x))
  | "terminated", [x; c] ->
      Pattern (Terminated (t x, t c))
  | "delimited", [o; x; c] ->
    Pattern (Delimited (t o, t x, t c))
  | "list", [x] ->
    Pattern (List (t x))
  | "nonemptylist", [x] ->
    Pattern (NEList (t x))
  | "separated_list", [sep; x] ->
    Pattern (SepList (t sep, t x))
  | "separated_nonempty_list", [sep; x] ->
    Pattern (SepNEList (t sep, t x))
  | x, _ ->
    Symbol (s, List.map transform_actual xs)

and transform_modifier x m =
  let x = transform_actual x in
  let m = match m with
    | Ast.Opt -> Opt
    | Ast.Plus -> Plus
    | Ast.Star -> Star
  in
  Modifier (x, m)

and transform_anonymous gs =
  let gs = List.map transform_group gs in
  Anonymous gs

and transform_production p = List.map transform_actual p

and transform_group = function
  | [p] -> transform_production p
  | _ -> assert false

let transform_rule r =
  let prods = List.map transform_group r.Ast.groups in
  { name = r.Ast.name; params = r.Ast.params; prods }

let transform = List.map transform_rule
