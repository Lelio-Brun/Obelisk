open ExtendedAst

val p: Format.formatter ref

val print_header: string list -> unit
val print_footer: unit -> unit

val print_string: string -> unit

val print_rule_name: bool -> string -> unit
val rule_begin: unit -> unit
val rule_end: unit -> unit

val production_begin: bool -> unit
val production_end: bool -> unit

val print_terminal: bool -> bool -> string -> unit
val print_sep_list: bool -> bool -> (unit -> unit) -> (unit -> unit) -> unit

val opt: bool -> (unit -> unit) -> unit
val plus: bool -> (unit -> unit) -> unit
val star: bool -> (unit -> unit) -> unit

val def: string
val bar: string
val par: bool -> (unit -> unit) -> unit
val space: string
val break: string
val eps: string
