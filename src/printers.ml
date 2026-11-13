(** This module provides the actual printers, of signature {!Printer}. *)

open GenericPrinter

module Default = Make (Default)
(** The default plain text printer. *)

module Ebnf = Make (Ebnf)
(** The EBNF plain text printer. *)

module LatexTabular = Make (LatexTabular)
(** The LaTeX default table-based printer. *)

module LatexSimplebnf = Make (LatexSimplebnf)
(** The LaTeX "simplebnf" package based printer. *)

module LatexSyntax = Make (LatexSyntax)
(** The LaTeX "syntax" package based printer. *)

module LatexBacknaur = Make (LatexBacknaur)
(** The LaTeX "backnaur" package based printer. *)

module HtmlCss = Make (HtmlCss)
(** The HTML printer with CSS content properties. *)

module Html = Make (Html)
(** The HTML printer without CSS content properties. *)
