(** This module provides the actual printers, of signature {!Printer}. *)

open GenericPrinter

(** The default plain text printer. *)
module Default = Make(Default)

(** The LaTeX default table-based printer. *)
module LatexTabular = Make(LatexTabular)

(** The LaTeX "syntax" package based printer. *)
module LatexSyntax = Make(LatexSyntax)

(** The LaTeX "backnaur" package based printer. *)
module LatexBacknaur = Make(LatexBacknaur)

(** The HTML printer.  *)
module Html = Make(Html)
