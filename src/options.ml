(** The command-line options.  *)

open Cmdliner

(** The input files.*)
let ifiles = ref []

(** The output file. *)
let ofile = ref ""

(** The {i .sty} package file (used only in LaTeX mode). *)
let pfile = ref ""

(** The LaTeX commands prefix. *)
let prefix = ref ""

(** The formatter for package output. *)
let formatter_package = ref Format.std_formatter

(** Do we use CSS? [true] by default (used only in HTML mode). *)
let css = ref true

(** The different output modes. *)
type mode =
  | Plain of plainmode          (** Standard plain text format. Default. *)
  | Latex of latexmode          (** LaTeX output. *)
  | Html                        (** HTML output. *)

(** The different plain text sub-modes *)
and plainmode =
  | Default                     (** Default BNF-like mode. *)
  | EBNF                        (** EBNF mode. *)

(** The different LaTeX sub-modes *)
and latexmode =
  | Tabular                     (** Table-based layout. Default. *)
  | Syntax                      (** Use the {{:https://www.ctan.org/pkg/syntax-mdw} syntax} package. *)
  | Backnaur                    (** Use the {{:https://www.ctan.org/pkg/backnaur} backnaur} package. *)

(** The chosen mode, default to {!mode.Plain !plainmode.Default}. *)
let mode = ref (Plain Default)

(** Do we inline inferred patterns? [false] by default. *)
let inline = ref false

(** Do we substitute token aliases? [false] by default. *)
let no_aliases = ref false


(** Default args *)

let parse_default ofile_arg inline_arg no_aliases_arg files_arg =
  mode := Plain Default;
  ofile := ofile_arg;
  inline := inline_arg;
  no_aliases := no_aliases_arg;
  ifiles := files_arg

let ofile_arg =
  let open Arg in
  let info = info
      ~docs:Manpage.s_common_options
      ~docv:"FILE"
      ~doc:"Set the output filename $(docv)."
      [ "o"; "output" ]
  in
  value & opt string "" & info

let inline_arg =
  let open Arg in
  let info = info
      ~docs:Manpage.s_common_options
      ~doc:"Inline recognized patterns."
      [ "i"; "inline" ]
  in
  value & flag & info

let no_aliases_arg =
  let open Arg in
  let info = info
      ~docs:Manpage.s_common_options
      ~doc:"Do not substitute token aliases. \
            Has no effect in LaTeX modes."
      [ "n"; "no-aliases" ]
  in
  value & flag & info

let files_arg =
  let open Arg in
  let info = info
      ~docv:"FILES"
      ~doc:"Input `.mly` Menhir grammar files."
      []
  in
  non_empty & pos_all file [] & info

let default_t =
  Term.(const parse_default
        $ ofile_arg $ inline_arg $ no_aliases_arg $ files_arg)

(** EBNF args *)

let parse_ebnf ofile_arg inline_arg no_aliases_arg files_arg =
  parse_default ofile_arg inline_arg no_aliases_arg files_arg;
  mode := Plain EBNF

let ebnf_t =
  Term.(const parse_ebnf
        $ ofile_arg $ inline_arg $ no_aliases_arg $ files_arg)

let ebnf_cmd =
  let doc = "EBNF mode" in
  let info = Cmd.info "ebnf" ~doc in
  Cmd.v info ebnf_t

(** LaTeX args *)

let parse_latex
    ofile_arg inline_arg no_aliases_arg files_arg
    mode_arg package_arg prefix_arg =
  parse_default ofile_arg inline_arg no_aliases_arg files_arg;
  mode := Latex mode_arg;
  pfile := package_arg;
  prefix := prefix_arg

let mode_conv =
  let parse = function
    | "tabular" -> Ok Tabular
    | "syntax" -> Ok Syntax
    | "backnaur" -> Ok Backnaur
    | s -> Error (`Msg ("unrecognized LaTeX mode " ^ s))
  in
  let print fmt m =
    Format.pp_print_string fmt (match m with
        | Tabular -> "tabular"
        | Syntax -> "syntax"
        | Backnaur -> "backnaur")
  in
  Arg.conv (parse, print)

let mode_arg =
  let open Arg in
  let info = info
      ~docv:"MODE"
      ~doc:"Set the LaTeX package used to format the grammar. \
            Choose $(docv) between $(b,tabular), $(b,syntax), $(b,backnaur)."
      [ "m"; "mode" ]
  in
  value & opt mode_conv Tabular & info

let package_arg =
  let open Arg in
  let info = info
      ~docv:"PACKAGE"
      ~doc:"Set the LaTeX `.sty` package name $(docv), without extension. \
            Use with $(b,-o)."
      [ "p"; "package" ]
  in
  value & opt string "" & info

let prefix_arg =
  let open Arg in
  let info = info
      ~docv:"PREFIX"
      ~doc:"Set the LaTeX commands (macros) prefix $(docv)."
      [ "x"; "prefix" ]
  in
  value & opt string "" & info

let latex_t =
  Term.(const parse_latex
        $ ofile_arg $ inline_arg $ no_aliases_arg $ files_arg
        $ mode_arg $ package_arg $ prefix_arg)

let latex_cmd =
  let doc = "LaTeX mode" in
  let info = Cmd.info "latex" ~doc in
  Cmd.v info latex_t

(** HTML args *)

let parse_html
    ofile_arg inline_arg no_aliases_arg files_arg
    no_css_arg =
  parse_default ofile_arg inline_arg no_aliases_arg files_arg;
  mode := Html;
  css := not no_css_arg

let no_css_arg =
  let open Arg in
  let info = info
      ~doc:"Do not use CSS to format the grammar."
      [ "c"; "no-css" ]
  in
  value & flag & info

let html_t =
  Term.(const parse_html
        $ ofile_arg $ inline_arg $ no_aliases_arg $ files_arg
        $ no_css_arg)

let html_cmd =
  let doc = "HTML mode" in
  let info = Cmd.info "html" ~doc in
  Cmd.v info html_t

(** Main command args *)

let main_cmd =
  let man = [
    `S Manpage.s_authors;
    `Pre "%%PKG_AUTHORS%%\nMaintainer: %%PKG_MAINTAINER%%";
    `S Manpage.s_bugs;
    `P "<%%PKG_ISSUES%%>"
  ]
  in
  let doc = "Pretty-printing for Menhir files." in
  let info = Cmd.info "%%NAME%%" ~version:"%%VERSION%%" ~man ~doc in
  Cmd.group ~default:default_t info [ (* ebnf_cmd; latex_cmd; html_cmd *) ]

let parse_opt () =
  Cmd.eval main_cmd
