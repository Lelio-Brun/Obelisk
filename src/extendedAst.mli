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
