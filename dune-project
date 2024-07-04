(lang dune 2.0)

(name obelisk)
(version 0.7.0)

(using menhir 2.0)
(generate_opam_files true)

(source (github Lelio-Brun/obelisk))
(homepage https://github.com/Lelio-Brun/Obelisk)
(documentation https://github.com/Lelio-Brun/Obelisk/blob/master/README.md)
(license MIT)
(authors "Lélio Brun")
(maintainers "Lélio Brun <lb@leliobrun.net>")

(package
 (name obelisk)
 (synopsis "Pretty-printing for Menhir files")
 (description "Obelisk is a simple tool which produces pretty-printed output from a Menhir parser file (.mly).\n\
               It is inspired from yacc2latex and is also written in OCaml, but is aimed at supporting features from Menhir instead of only those of ocamlyacc.")
 (depends
  (ocaml (>= 4.08))
  (dune (>= 2.2.0))
  (menhir (>= "20180528"))
  re))
