(** The command-line options.  *)

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

(** The different output modes. *)
type mode =
  | Plain of plainmode          (** Standard plain text format. Default. *)
  | Latex of latexmode          (** LaTeX output. *)
  | Html of htmlmode            (** HTML output. *)

(** The different plain text sub-modes *)
and plainmode =
  | Default                     (** Default BNF-like mode. *)
  | EBNF                        (** EBNF mode. *)

(** The different LaTeX sub-modes *)
and latexmode =
  | Tabular                     (** Table-based layout. Default. *)
  | Syntax                      (** Use the {{:https://www.ctan.org/pkg/syntax-mdw} syntax} package. *)
  | Backnaur                    (** Use the {{:https://www.ctan.org/pkg/backnaur} backnaur} package. *)

(** The different HTML sub-modes *)
and htmlmode =
  | CSS
  | NoCSS

(** The chosen mode, default to {!mode.Plain !plainmode.Default}. *)
let mode = ref (Plain Default)

(** Do we inline inferred patterns? [false] by default. *)
let inline = ref false

(** Do we substitute token aliases? [false] by default. *)
let no_aliases = ref false

(** Default common command-line options. *)
let options = ref (Arg.align [
    "-o",         Arg.Set_string ofile, " Set the output filename";
    "-i",         Arg.Set inline,       " Inline recognized patterns";
    "-noaliases", Arg.Set no_aliases,   " Do not substitute token aliases. Has no effect in LaTeX modes."
  ])

(** Specify the plain sub-mode to use. *)
let set_plainmode m () =
  mode := Plain m

(** Specify the LaTeX sub-mode to use. *)
let set_latexmode m () =
  mode := Latex m

(** LaTeX mode specific options. *)
let latex_opt = [
  "-tabular",  Arg.Unit (set_latexmode Tabular),  " Use tabular environment (default)";
  "-syntax",   Arg.Unit (set_latexmode Syntax),   " Use `syntax` package";
  "-backnaur", Arg.Unit (set_latexmode Backnaur), " Use `backnaur` package";
  "-package",  Arg.Set_string pfile,              " Set the package name, without extension. Use with `-o`";
  "-prefix",   Arg.Set_string prefix,             " Set the LaTeX commands (macros) prefix"
]

(** Specify the HTML sub-mode to use. *)
let set_htmlmode m () =
  mode := Html m

(** HTML mode specific options. *)
let html_opt = [
  "-css",   Arg.Unit (set_htmlmode CSS),   " Use CSS content properties (default)";
  "-nocss", Arg.Unit (set_htmlmode NoCSS), " Do not us CSS content properties"
]

(** Usage message. *)
let msg = "Obelisk version %%VERSION_NUM%%\n\
           Usage: obelisk [latex|html] [options] <source>\n       \
           obelisk <mode> -help for <mode>-specific help."

(** Function called on anonymous arguments.
    It is used to trigger the LaTeX and HTML modes and to get the input file. *)
let parse_cmd =
  let cpt = ref 0 in
  function
  | "ebnf" when !cpt < 1 ->
    incr cpt;
    set_plainmode EBNF ();
    options := Arg.align (!options @ latex_opt)
  | "latex" when !cpt < 1 ->
    incr cpt;
    set_latexmode Tabular ();
    options := Arg.align (!options @ latex_opt)
  | "html" when !cpt < 1 ->
    incr cpt;
    set_htmlmode CSS ();
    options := Arg.align (!options @ html_opt)

  | f ->
    ifiles := f :: !ifiles

let error () =
  Arg.usage !options msg; exit 1

let parse_opt () =
  Arg.parse_dynamic options parse_cmd msg
