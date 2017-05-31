(** The raw Abstract Syntax Tree for grammars. *)

(** The grammar. *)
type spec = rule list

(** The rules.  *)
and rule = {
  name: string;                 (** The left-hand side of the rule.  *)
  params: string list;          (** The possible list of parameters.  *)
  groups: group list            (** The right-hand side of the rule.  *)
}

(** The group of productions.  *)
and group = production list

(** The productions. *)
and production = actual list

(** The actuals. *)
and actual =
  | Symbol of string * actual list (** A possibly applied symbol.  *)
  | Modifier of actual * modifier  (** A "modified" actual. *)
  | Anonymous of group list        (** An anonymous rule. *)

(** The modifiers. *)
and modifier =
  | Opt                         (** optionnal *)
  | Plus                        (** non-empty list  *)
  | Star                        (** list *)
