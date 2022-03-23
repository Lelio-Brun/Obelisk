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

type common_options = {
  ofile: string;
  inline: bool;
  no_aliases: bool;
  files: string list;
}

let c_opts ofile inline no_aliases files =
  { ofile; inline; no_aliases; files }

let parse_default c_opts =
  mode := Plain Default;
  ofile := c_opts.ofile;
  inline := c_opts.inline;
  no_aliases := c_opts.no_aliases;
  ifiles := c_opts.files

let c_opts_arg =
  let open Arg in
  let ofile_arg =
    let info = info
        ~docs:Manpage.s_common_options
        ~docv:"FILE"
        ~doc:"Set the output filename $(docv)."
        [ "o"; "output" ]
    in
    value & opt string "" & info
  in
  let inline_arg =
    let info = info
        ~docs:Manpage.s_common_options
        ~doc:"Inline recognized patterns rules."
        [ "i"; "inline" ]
    in
    value & flag & info
  in
  let no_aliases_arg =
    let info = info
        ~docs:Manpage.s_common_options
        ~doc:"Do not substitute token aliases. \
              Has no effect in LaTeX modes."
        [ "n"; "no-aliases" ]
    in
    value & flag & info
  in
  let files_arg =
    let info = info
        ~docv:"FILES"
        ~doc:"Input $(i,.mly) Menhir grammar files. \
              If multiple files are specified, Obelisk will output a \
              concatenated result, without consistency checks, so the user is \
              responsible for avoiding eg. name clashes between the several \
              files."
        []
    in
    non_empty & pos_all file [] & info
  in
  Term.(const c_opts $ ofile_arg $ inline_arg $ no_aliases_arg $ files_arg)

(** Plain args *)

let parse_plain = parse_default

let plain_t = Term.(const parse_plain $ c_opts_arg)

let plain_cmd =
  let doc = "Default plain mode." in
  let info = Cmd.info "default" ~doc in
  Cmd.v info plain_t

(** EBNF args *)

let parse_ebnf c_opts =
  parse_default c_opts;
  mode := Plain EBNF

let ebnf_t = Term.(const parse_ebnf $ c_opts_arg)

let ebnf_cmd =
  let doc = "EBNF plain mode." in
  let man = [
    `S Manpage.s_description;
    `P "In EBNF mode, parameterized rules are specialized into dedicated \
         regular rules."
  ]
  in
  let info = Cmd.info "ebnf" ~doc ~man in
  Cmd.v info ebnf_t

(** LaTeX args *)

let parse_latex c_opts mode_arg package_arg prefix_arg =
  parse_default c_opts;
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
            Choose $(docv) between $(b,tabular), $(b,syntax) and $(b,backnaur)."
      [ "m"; "mode" ]
  in
  value & opt mode_conv Tabular & info

let package_arg =
  let open Arg in
  let info = info
      ~docv:"PACKAGE"
      ~doc:"Set the LaTeX $(i,.sty) package name $(docv), without extension. \
            Use with $(b,--output)."
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
  Term.(const parse_latex $ c_opts_arg $ mode_arg $ package_arg $ prefix_arg)

let latex_cmd =
  let doc = "LaTeX mode." in
  let man = [
    `S Manpage.s_description;
    `P "Use the $(b,--mode)=$(i,MODE) option to tweak the LaTeX, where \
        $(i,Mode) is among:"; `Noblank;
    `I ("$(b,tabular)",
        "a tabular-based format from the tabu package (default).");
    `I ("$(b,syntax)",
        "use the syntax package.");
    `I ("$(b,backnaur)",
        "use the backnaur package (not recommended).");
    `P "In either cases, the output may be customized via the use of LaTeX \
        commands that you can redefine to fit your needs. \
        The commands names are auto-generated from the terminal names, and \
        because of LaTeX limitations, underscore are removed and numbers are \
        converted into their roman form.";
    `P "By default in LaTeX mode, the $(b,--output)=$(i,FILE) option will \
        produce the standalone LaTeX file $(i,FILE) which you can directly \
        compile (eg. with pdflatex). \
        But in conjunction with $(b,--output)=$(i,FILE), you can use \
        $(b,--package)=$(i,PACKAGE) to output two files:"; `Noblank;
    `I ("1.",
        "a LaTeX file $(i,FILE) containing only the grammar contents, to \
         be used in a third main LaTeX file with \\\\include{$(i,FILE)} ;");
    `Noblank;
    `I ("2.",
        "a package file $(i,PACKAGE.sty) (the $(i,.sty) extension is added \
         automatically) containing the necessary extra packages requirements \
         and command definitions, to be used with \\\\usepackage{$(i,PACKAGE)}.");
    `P "To avoid name clashes, in particular when using the $(b,--package) \
        option and eg. importing multiple grammars with the same LaTeX commands \
        names, or in the case where one of the syntax construction name matches \
        one already defined LaTeX macro, you can specify a common prefix for \
        the commands with the option $(b,--prefix)=$(i,PREFIX).";
    `P "As $(b,end)-beginning commands are forbidden in LaTeX, commands \
        created from rules with names beginning with $(b,end) are automatically \
        prefixed with $(b,zzz)."
  ]
  in
  let info = Cmd.info "latex" ~doc ~man in
  Cmd.v info latex_t

(** HTML args *)

let parse_html c_opts no_css_arg =
  parse_default c_opts;
  mode := Html;
  css := not no_css_arg

let no_css_arg =
  let open Arg in
  let info = info
      ~doc:"Do not use CSS to format the grammar."
      [ "s"; "no-css" ]
  in
  value & flag & info

let html_t = Term.(const parse_html $ c_opts_arg $ no_css_arg)

let html_cmd =
  let doc = "HTML mode." in
  let man = [
    `S Manpage.s_description;
    `P "The HTML file uses internal CSS stylesheet which allows one to \
        customize the output (in a poorer way than in the $(b,latex) mode). \
        The stylesheet uses $(b,content) properties for some parts of the \
        grammar by default, to make it modular and easily modifiable, but then \
        some symbols are not treated as content and, for example, are not \
        copy-pastable. \
        Use the $(b,--no-css) flag to disable the use of such properties."
  ]
  in
  let info = Cmd.info "html" ~doc ~man in
  Cmd.v info html_t

(** Main command args *)

let main_cmd =
  let man = [
    `S Manpage.s_description;
    `P "Obelisk is a simple tool which produces pretty-printed output from a \
        Menhir parser file ($(i,.mly)).";
    `P "It is inspired from yacc2latex and is also written in OCaml, but is \
        aimed at supporting features from Menhir instead of only those of \
        ocamlyacc.";

    `S "Pattern recognition";
    `P "Obelisk can infer some common patterns (possibly parameterized):";
    `Noblank;
    `P "- options"; `Noblank;
    `P "- lists and non-empty lists"; `Noblank;
    `P "- separated lists and non-empty separated lists";
    `P "Once recognized, if the $(b,--inline) flag is specified the rules are \
        deleted and their instances are replaced with default constructions. \
        Without the $(b,--inline) flag, only the productions of the recognized \
        rules are replaced, the total amount of rules remaining the same.";

    `S "Multi-format output";
    `P "The output format is specified by subcommands among $(b,default), \
        $(b,ebnf), $(b,latex) or $(b,html). \
        With $(b,default) mode, the output format is a simple text format close \
        to the BNF syntax. \
        Use $(b,ebnf), $(b,latex) or $(b,html) to get respectively an EBNF text \
        output, LaTeX output or HTML output.";

    `S Manpage.s_commands;
    `P "Use $(mname) $(i,COMMAND) --help for help on a single command.";

    `S Manpage.s_authors;
    `Pre "%%PKG_AUTHORS%%\nMaintainer: %%PKG_MAINTAINER%%";

    `S Manpage.s_bugs;
    `P "<%%PKG_ISSUES%%>"
  ]
  in
  let doc = "Pretty-printing for Menhir files." in
  let info = Cmd.info "%%NAME%%" ~version:"%%VERSION%%" ~man ~doc in
  let default = Term.(ret (const (fun _ ->
      `Error (true,
              Format.sprintf "Missing command among '%s', '%s', '%s' or '%s'."
                (Cmd.name plain_cmd) (Cmd.name ebnf_cmd)
                (Cmd.name html_cmd) (Cmd.name latex_cmd)))
                           $ c_opts_arg))
  in
  Cmd.group ~default info [ plain_cmd; ebnf_cmd; latex_cmd; html_cmd ]

let parse_opt () =
  Cmd.eval main_cmd
