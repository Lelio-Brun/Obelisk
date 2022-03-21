(** The generic signature for a printer *)

open Format
open Common
open ExtendedAst

(** [print_spec symbs fmt s] print the grammar [s] with symbols [symbs] on
    the specified formatter [fmt]. *)
val print_spec: Symbols.t -> formatter -> spec -> unit

val print_actual: Symbols.t -> formatter -> actual -> unit
