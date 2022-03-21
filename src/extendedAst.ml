(** The simplified Abstract Syntax Tree for grammars.

    Compared to {!Ast}, two differences:
    + groups are sort of "flattened", leaving only productions.
    + Patterns of the standard library of Menhir are explicit. *)

(** The grammar. *)
type spec = rule list

(** The rules.  *)
and rule = {
  name: string;                 (** The left-hand side of the rule.*)
  params: string list;          (** The possible list of parameters. *)
  prods: production list        (** The right-hand side of the rule. *)
}

(** The productions. *)
and production = actual list

(** The actuals. *)
and actual =
  | Symbol of string * actual list (** A possibly applied symbol. *)
  | Pattern of pattern             (** A pattern from the standard library.  *)
  | Modifier of actual * modifier  (** A "modified" actual. *)
  | Anonymous of production list   (** An anonymous rule. *)

(** The modifiers. *)
and modifier =
  | Opt                         (** optionnal *)
  | Plus                        (** non-empty list *)
  | Star                        (** list *)

(** The patterns from the standard library. *)
and pattern =
  | Option of actual                      (** [option(x)] *)
  | Pair of actual * actual               (** [pair(x, y)] *)
  | SepPair of actual * actual * actual   (** [separated_pair(x, sep, y)]  *)
  | Preceded of actual * actual           (** [preceded(opening, x)]  *)
  | Terminated of actual * actual         (** [terminated(x, closing)]  *)
  | Delimited of actual * actual * actual (** [delimited(opening, x, closing)] *)
  | List of actual                        (** [list(x)] *)
  | NEList of actual                      (** [nonempty_list(x)] *)
  | SepList of actual * actual            (** [separated_list(x)] *)
  | SepNEList of actual * actual          (** [separated_nonempty_list(sep, x)] *)

let fold_map_pattern f acc = function
  | Option x ->
    let acc, x = f acc x in
    acc, Option x
  | Pair (x, y) ->
    let acc, x = f acc x in
    let acc, y = f acc y in
    acc, Pair (x, y)
  | SepPair (x, sep, y) ->
    let acc, x = f acc x in
    let acc, sep = f acc sep in
    let acc, y = f acc y in
    acc, SepPair (x, sep, y)
  | Preceded (o, x) ->
    let acc, o = f acc o in
    let acc, x = f acc x in
    acc, Preceded (o, x)
  | Terminated (x, c) ->
    let acc, x = f acc x in
    let acc, c = f acc c in
    acc, Terminated (x, c)
  | Delimited (o, x, c) ->
    let acc, o = f acc o in
    let acc, x = f acc x in
    let acc, c = f acc c in
    acc, Delimited (o, x, c)
  | List x ->
    let acc, x = f acc x in
    acc, List x
  | NEList x ->
    let acc, x = f acc x in
    acc, NEList x
  | SepList (sep, x) ->
    let acc, sep = f acc sep in
    let acc, x = f acc x in
    acc, SepList (sep, x)
  | SepNEList (sep, x) ->
    let acc, sep = f acc sep in
    let acc, x = f acc x in
    acc, SepNEList (sep, x)

let map_pattern f p = fold_map_pattern (fun o p -> o, f p) None p |> snd
