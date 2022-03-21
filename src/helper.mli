(** The generic signature for the printer helpers.  *)

open Common
open Format

(** Printed at the beginning of the output. *)
val print_header: Symbols.t -> formatter -> unit

(** Printed at the end of the output. *)
val print_footer: formatter -> unit

(** Print an escaped string. *)
val print_string: formatter -> string -> unit

(** [print_rule_name print_params fmt x] prints the left-hand side of a rule where
    [x] is the name of the defined non terminal and [print_params] the function
    to print the optional parameters. *)
val print_rule_name: (formatter -> unit) -> formatter -> string -> unit

(** Print a rule parmater. *)
val print_param: formatter -> string -> unit

(** Printed at the beginning of each rule. *)
val rule_begin: formatter -> unit

(** Printed at the end of each rule. *)
val rule_end: formatter -> unit

(** Printed at the beginning of each production. *)
val production_begin: formatter -> unit

(** Printed at the end of each production. *)
val production_end: formatter -> unit

(** [print_symbol symbols print_params fmt s] prints the symbol [s] and
    its parameters thanks to [print_params]. A different formatting is possible
    accordingly to [s] being a terminal, a non terminal or
    a functional non terminal. *)
val print_symbol: Symbols.t -> (formatter -> unit) -> formatter -> string -> unit

(** [print_sep_list e nonempty print_sep print_x fmt] prints the possibly non empty
    separated list [separated[_nonempty]_list(sep, x)] where [sep] and [x] are
    respectively printed by [print_sep] and [print_x]. If [e] is [true] then
    the result is parenthesized. *)
val print_sep_list: bool -> bool -> (formatter -> unit) -> (formatter -> unit) -> formatter -> unit

(** To print a possibly parenthesized optional. *)
val opt: bool -> (formatter -> unit) -> formatter -> unit

(** To print a possibly parenthesized non empty list.  *)
val plus: bool -> (formatter -> unit) -> formatter -> unit

(** To print a possibly parenthesized list. *)
val star: bool -> (formatter -> unit) -> formatter -> unit

(** The rule definition symbol.  *)
val def: formatter -> unit

(** The bar at the start of each alternative production. *)
val prod_bar: formatter -> unit

(** The bar for the anonymous rules. *)
val bar: formatter -> unit

(* (\** The optionally parenthesizing function. *\)
 * val par: bool -> (unit -> unit) -> unit *)

(** The space. *)
val space: formatter -> unit

(** The line break. *)
val break: formatter -> unit

(** The empty word epsilon.  *)
val eps: formatter -> unit
