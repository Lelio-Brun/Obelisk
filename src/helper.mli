open ExtendedAst
open Common

val p: Format.formatter ref

val print_header: Symbols.t -> unit
val print_footer: unit -> unit

val print_string: string -> unit

val print_rule_name: bool -> string -> unit
val rule_begin: unit -> unit
val rule_end: unit -> unit

val production_begin: unit -> unit
val production_end: unit -> unit

val print_symbol: bool -> bool -> string -> (unit -> unit) -> unit
val print_sep_list: bool -> bool -> (unit -> unit) -> (unit -> unit) -> unit

val opt: bool -> (unit -> unit) -> unit
val plus: bool -> (unit -> unit) -> unit
val star: bool -> (unit -> unit) -> unit

val def: string
val prod_bar: string
val bar: string
val par: bool -> (unit -> unit) -> unit
val space: string
val break: string
val eps: string
