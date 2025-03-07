(** This module provides the actual printers, of signature {!Printer}. *)

open GenericPrinter

(** The default plain text printer. *)
module Default = Make(Default)

(** The EBNF plain text printer. *)
module Ebnf = Make(Ebnf)

(** The LaTeX default table-based printer. *)
module LatexTabular = Make(LatexTabular)

(** The LaTeX "simplebnf" package based printer. *)
module LatexSimplebnf = Make(LatexSimplebnf)

(** The LaTeX "syntax" package based printer. *)
module LatexSyntax = Make(LatexSyntax)

(** The LaTeX "backnaur" package based printer. *)
module LatexBacknaur = Make(LatexBacknaur)

(** The HTML printer with CSS content properties. *)
module HtmlCss = Make(HtmlCss)

(** The HTML printer without CSS content properties. *)
module Html = Make(Html)
