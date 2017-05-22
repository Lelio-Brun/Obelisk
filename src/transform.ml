open ExtendedAst
open Common
open List

let rec transform_actual symbols = function
  | Ast.Symbol (s, xs) -> transform_symbol symbols s xs
  | Ast.Modifier (x, m) -> transform_modifier symbols x m
  | Ast.Anonymous gs -> transform_anonymous symbols gs

and transform_symbol symbols s xs =
  let xs = map (transform_actual symbols) xs in
  if Symbols.is_defined s symbols then Symbol (s, xs)
  else match s, xs with
    | ("option" | "ioption" | "boption" | "loption"), [x] ->
      Pattern (Option x)
    | "pair", [x; y] ->
      Pattern (Pair (x, y))
    | "separated_pair", [x; sep; y] ->
      Pattern (SepPair (x, sep, y))
    | "preceded", [o; x] ->
      Pattern (Preceded (o, x))
    | "terminated", [x; c] ->
      Pattern (Terminated (x, c))
    | "delimited", [o; x; c] ->
      Pattern (Delimited (o, x, c))
    | "list", [x] ->
      Pattern (List x)
    | "nonemptylist", [x] ->
      Pattern (NEList x)
    | "separated_list", [sep; x] ->
      Pattern (SepList (sep, x))
    | "separated_nonempty_list", [sep; x] ->
      Pattern (SepNEList (sep, x))
    | _, _ ->
      Symbol (s, xs)

and transform_modifier symbols x m =
  let x = transform_actual symbols x in
  let m = match m with
    | Ast.Opt -> Opt
    | Ast.Plus -> Plus
    | Ast.Star -> Star
  in
  Modifier (x, m)

and transform_anonymous symbols gs =
  let gs = map (transform_group symbols) gs in
  Anonymous gs

and transform_production symbols = map (transform_actual symbols)

and transform_group symbols = function
  | [p] -> transform_production symbols p
  | _ -> assert false

let transform_rule symbols r =
  let prods = map (transform_group symbols) r.Ast.groups in
  { name = r.Ast.name; params = r.Ast.params; prods }

let transform symbols = map (transform_rule symbols)
