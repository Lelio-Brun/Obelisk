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
  | Default                     (** Standard plain text format. Default. *)
  | Latex of latexmode          (** LaTeX output. *)
  | Html                        (** HTML output. *)

(** The different LaTeX sub-modes *)
and latexmode =
  | Tabular                     (** Table-based layout. Default. *)
  | Syntax                      (** Use the {{:https://www.ctan.org/pkg/syntax-mdw} syntax} package. *)
  | Backnaur                    (** Use the {{:https://www.ctan.org/pkg/backnaur} backnaur} package. *)

(** The chosen mode, default to {!mode.Default}. *)
let mode = ref Default
(** Do we inline inferred patterns ? [false] by default. *)
let inline = ref false

(** Default common command-line options. *)
let options = ref (Arg.align [
    "-o", Arg.Set_string ofile, " Set the output filename";
    "-i", Arg.Set inline, " Inline recognized patterns"
  ])

(** Specify the LaTeX sub-mode to use. *)
let set_latexmode lm () =
  mode := Latex lm

(** LaTeX mode specific options. *)
let latex_opt = [
  "-tabular", Arg.Unit (set_latexmode Tabular), " Use tabular environment (default)";
  "-syntax", Arg.Unit (set_latexmode Syntax), " Use `syntax` package";
  "-backnaur", Arg.Unit (set_latexmode Backnaur), " Use `backnaur` package";
  "-package", Arg.Set_string pfile, " Set the package name, without extension. Use with `-o`";
  "-prefix", Arg.Set_string prefix, " Set the LaTeX commands (macros) prefix"
]

(** Usage message. *)
let msg = "obelisk [latex|html] [options] <source>"

(** Function called on anonymous arguments.
    It is used to trigger the LaTeX and HTML modes and to get the input file. *)
let parse_cmd =
  let cpt = ref 0 in
  function
  | "latex" when !cpt < 1 ->
    mode := Latex Tabular;
    options := Arg.align (!options @ latex_opt)
  | "html" when !cpt < 1 ->
    mode := Html;
    options := Arg.align !options

  | f ->
    ifiles := f :: !ifiles

let error () =
  Arg.usage !options msg; exit 1

let parse_opt () =
  Arg.parse_dynamic options parse_cmd msg
