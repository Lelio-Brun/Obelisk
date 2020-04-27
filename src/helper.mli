(** The generic signature for the printer helpers.  *)

open Common

(** A reference to the used formatter. *)
val p: Format.formatter ref

(** Printed at the beginning of the output. *)
val print_header: Symbols.t -> unit

(** Printed at the end of the output. *)
val print_footer: unit -> unit

(** Print an escaped string. *)
val print_string: string -> unit

(** [print_rule_name x print_params] prints the left-hand side of a rule where
    [x] is the name of the defined non terminal and [print_params] the function
    to print the optional parameters. *)
val print_rule_name: string -> (unit -> unit) option -> unit

(** Print a rule parmater. *)
val print_param: string -> unit

(** Printed at the beginning of each rule. *)
val rule_begin: unit -> unit

(** Printed at the end of each rule. *)
val rule_end: unit -> unit

(** Printed at the beginning of each production. *)
val production_begin: unit -> unit

(** Printed at the end of each production. *)
val production_end: unit -> unit

(** [print_symbol symbols s print_params] prints the symbol [s] and
    its parameters thanks to [print_params]. A different formatting is possible
    accordingly to [s] being a terminal, a non terminal or
    a functional non terminal. *)
val print_symbol: Symbols.t -> string -> (unit -> unit) -> unit

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
val def: unit -> string

(** The bar at the start of each alternative production. *)
val prod_bar: unit -> string

(** The bar for the anonymous rules. *)
val bar: unit -> string

(* (\** The optionally parenthesizing function. *\)
 * val par: bool -> (unit -> unit) -> unit *)

(** The space. *)
val space: unit -> string

(** The line break. *)
val break: unit -> string

(** The empty word epsilon.  *)
val eps: unit -> string
