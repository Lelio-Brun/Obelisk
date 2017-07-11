(** This module provides the actual printers, of signature {!Printer}. *)

open GenericPrinter

(** The default plain text printer. *)
module Default = Make(Default)

(** The LaTeX default table-based printer. *)
module LatexTabular (P : MiniLatex.PACKAGEPRINTER) = Make(LatexTabular.Make(P))

(** The LaTeX "syntax" package based printer. *)
module LatexSyntax (P : MiniLatex.PACKAGEPRINTER) = Make(LatexSyntax.Make(P))

(** The LaTeX "backnaur" package based printer. *)
module LatexBacknaur (P : MiniLatex.PACKAGEPRINTER) = Make(LatexBacknaur.Make(P))

(** The HTML printer.  *)
module Html = Make(Html)
