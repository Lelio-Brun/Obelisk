(** The generic signature for the printer helpers.  *)

open ExtendedAst
open Common

(** A reference to the used formatter. *)
val p: Format.formatter ref

(** Printed at the beginning of the output. *)
val print_header: Symbols.t -> unit

(** Printed at the end of the output. *)
val print_footer: unit -> unit

(** Print an escaped string. *)
val print_string: string -> unit

(** [print_rule_name is_not_fun x] prints the left-hand side of a rule where
    [is_not_fun] is [true] if the rule has no parameter and [x] is the name of
    the defined non terminal. *)
val print_rule_name: bool -> string -> unit

(** Printed at the beginning of each rule. *)
val rule_begin: unit -> unit

(** Printed at the end of each rule. *)
val rule_end: unit -> unit

(** Printed at the beginning of each production. *)
val production_begin: unit -> unit

(** Printed at the end of each production. *)
val production_end: unit -> unit

(** [print_symbol is_term is_non_term s print_params] prints the symbol [s] and
    its parameters thanks to [print_params]. A different formatting is possible
    accordingly to [s] being a terminal, a non terminal or
    a functional non terminal. *)
val print_symbol: bool -> bool -> string -> (unit -> unit) -> unit

(** [print_sep_list e nonempty print_sep print_x] prints the possibly non empty
    separated list [separated[_nonempty]_list(sep, x)] where [sep] and [x] are
    respectively printed by [print_sep] and [print_x]. If [e] is [true] then
    the result is parenthesized. *)
val print_sep_list: bool -> bool -> (unit -> unit) -> (unit -> unit) -> unit

(** To print a possibly parenthesized optional. *)
val opt: bool -> (unit -> unit) -> unit

(** To print a possibly parenthesized non empty list.  *)
val plus: bool -> (unit -> unit) -> unit

(** To print a possibly parenthesized list. *)
val star: bool -> (unit -> unit) -> unit

(** The rule definition symbol.  *)
val def: string

(** The bar at the start of each alternative production. *)
val prod_bar: string

(** The bar for the anonymous rules. *)
val bar: string

(** The optionally parenthesizing function. *)
val par: bool -> (unit -> unit) -> unit

(** The space. *)
val space: string

(** The line break. *)
val break: string

(** The empty word epsilon.  *)
val eps: string
