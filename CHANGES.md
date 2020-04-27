## v0.5.0 - 2020-04-28
This version implements several important changes:
- drop `ocamlbuild` in favor of `dune`
- drop API-doc style documentation (irrelevant)
- fix break hints after epsilons 
- use `\lit` command for literals in `syntax` mode
- change the name of the grammar environment to `obeliskgrammar` in LaTeX modes
- use `re` library instead of `str`
- add support for token aliases, with a dedicated option `-noaliases`
- add support for the new syntax of Menhir rules (fixes issue [#9](https://github.com/Lelio-Brun/Obelisk/issues/9)) 
- fix some lexing and parsing bugs (in particular with Ocaml code and strings in prologue and semantic actions) thanks to the added test benches of Menhir

## v0.4.0 - 2019-03-01
This version fixes issue [#8](https://github.com/Lelio-Brun/Obelisk/issues/8), corrects some parentheses related additional bugs and uses the new OPAM 2.0 format.

## v0.3.2 - 2018-04-25
This patch is a minor fix to 0.3.1: tests in LaTeX mode are now conditionally guarded on the presence of `pdflatex`` in the PATH.

## v0.3.1 - 2018-04-23
This patch fixes [#7](https://github.com/Lelio-Brun/Obelisk/issues/7): in LaTeX mode, if no prefix is specified throug `-prefix` option, `end`-beginning commands are automatically prefixed with `zzz`.

## v0.3.0 - 2017-08-17
This release fixes [#4](https://github.com/Lelio-Brun/Obelisk/issues/4), [#5](https://github.com/Lelio-Brun/Obelisk/issues/5) and [#6](https://github.com/Lelio-Brun/Obelisk/issues/6).
See [#2](https://github.com/Lelio-Brun/Obelisk/issues/2) (comments).

1. in LaTeX modes, macros are now generated for terminals, non-terminals and functionals ;
2. those macros are now defined using the generated generic macros for each class of syntactic construction ;
3. `longtabu` replaces `tabu` in tabular mode.

Also functionals are now surrounded by `<` and `>` in all output formats.

## v0.2.0 - 2017-07-21
This release fixes [#2](https://github.com/Lelio-Brun/Obelisk/issues/2):

1. the option `-package` in LaTeX mode allows the user to specify an additional output file (.sty) to gather the created macros ;
2. all created macros in LaTeX mode can be prefixed with a prefix specified with the option `-prefix`.

## v0.1.1 - 2017-07-06
Remove remaining unneeded *Roman* parts.

## v0.1.0 - 2017-07-06
First release.
