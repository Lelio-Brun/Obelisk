(** Transform a normalized {!Ast} towards an {!ExtendedAst}. *)

open ExtendedAst
open Common
open List

(** Transform an actual.
    Note that the anonymous rules are transformed with the help
    of {!transform_group}. *)
let rec transform_actual symbols = function
  | Ast.Symbol (s, xs) ->
    transform_symbol symbols s xs
  | Ast.Modifier (x, m) ->
    transform_modifier symbols x m
  | Ast.Anonymous gs ->
    let gs = map (transform_group symbols) gs in
    Anonymous gs

(** Transform all non defined symbols (see {!Common.Symbols.is_defined})
    corresponding to Menhir standard library symbols ([list], [pair], [option],
    etc.) into patterns, and all other symbols into symbols. *)
and transform_symbol symbols s xs =
  let xs = map (transform_actual symbols) xs in
  match Symbols.is_defined s symbols with
  | Some _ -> Symbol (s, xs)
  | None ->
    begin match s, xs with
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
      | "nonempty_list", [x] ->
        Pattern (NEList x)
      | "separated_list", [sep; x] ->
        Pattern (SepList (sep, x))
      | "separated_nonempty_list", [sep; x] ->
        Pattern (SepNEList (sep, x))
      | _, _ ->
        Symbol (s, xs)
    end

(** Transform a "modified" actual. *)
and transform_modifier symbols x m =
  let x = transform_actual symbols x in
  let m = match m with
    | Ast.Opt -> Opt
    | Ast.Plus -> Plus
    | Ast.Star -> Star
  in
  Modifier (x, m)

(** Transform a production by transforming its actuals. *)
and transform_production symbols = map (transform_actual symbols)

(** Transform a supposed singleton group by returning the transformation of
    its unique production. *)
and transform_group symbols = function
  | [p] -> transform_production symbols p
  | _ -> assert false

(** Transform a rule by transforming its groups into productions. *)
let transform_rule symbols r =
  let prods = map (transform_group symbols) r.Ast.groups in
  { name = r.Ast.name; params = r.Ast.params; prods }

(** Transform a grammar by transforming its rules. *)
let transform symbols = map (transform_rule symbols)
