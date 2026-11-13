(** The generic signature for a printer *)

open Format
open Common
open ExtendedAst

val print_spec : Symbols.t -> formatter -> spec -> unit
(** [print_spec symbs fmt s] print the grammar [s] with symbols [symbs] on the
    specified formatter [fmt]. *)
