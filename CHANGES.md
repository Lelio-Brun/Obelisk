# Changes

## Unreleased
- Drop `ocamlbuild` and `make` in favor of `dune`.
- Drop API-doc style documentation (irrelevant)

## 0.3.2 - 2018-04-25
This patch is a minor fix to 0.3.1: tests in LaTeX mode are now conditionally guarded on the presence of `pdflatex`` in the PATH.

## 0.3.1 - 2018-04-23
This patch fixes [#7](https://github.com/Lelio-Brun/Obelisk/issues/7): in LaTeX mode, if no prefix is specified throug `-prefix` option, `end`-beginning commands are automatically prefixed with `zzz`.

## 0.3.0 - 2017-08-17
This release fixes [#4](https://github.com/Lelio-Brun/Obelisk/issues/4), [#5](https://github.com/Lelio-Brun/Obelisk/issues/5) and [#6](https://github.com/Lelio-Brun/Obelisk/issues/6).
See [#2](https://github.com/Lelio-Brun/Obelisk/issues/2) (comments).

1. in LaTeX modes, macros are now generated for terminals, non-terminals and functionals ;
2. those macros are now defined using the generated generic macros for each class of syntactic construction ;
3. `longtabu` replaces `tabu` in tabular mode.

Also functionals are now surrounded by `<` and `>` in all output formats.

## 0.2.0 - 2017-07-21
This release fixes [#2](https://github.com/Lelio-Brun/Obelisk/issues/2):

1. the option `-package` in LaTeX mode allows the user to specify an additional output file (.sty) to gather the created macros ;
2. all created macros in LaTeX mode can be prefixed with a prefix specified with the option `-prefix`.

## 0.1.1 - 2017-07-06
Remove remaining unneeded *Roman* parts.

## 0.1.0 - 2017-07-06
First release.
