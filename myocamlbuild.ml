open Ocamlbuild_plugin
open Command

let () =
  dispatch begin function
    | After_options ->
      Options.ocaml_docflags :=
        !Options.ocaml_docflags @
        ["-hide-warnings";
         "-colorize-code";
         "-short-functors";
         "-keep-code";
         "-t"; "Obelisk code documentation";
         "-css-style"; "style.css"]
    | _ -> ()
  end
