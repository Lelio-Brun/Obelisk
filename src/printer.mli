(** The generic signature for a printer *)

open Format
open Common
open ExtendedAst

(** [print_spec fmt symbs s] print the grammar [s] with symbols [symbs] on
    the specified formatter [fmt]. *)
val print_spec: formatter -> Symbols.t -> spec -> unit
