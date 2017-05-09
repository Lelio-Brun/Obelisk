# MenhirBrav
## Overview
**MenhirBrav** (from *pretty menhir* in Breton) is a simple tool which produces pretty-printed output from a [Menhir] parser file (*.mly*).

It is inspired from [yacc2latex] and is also written in [OCaml], but is aimed at supporting features from Menhir instead of only those of ocamlyacc.

## Installation
### Dependencies
- OCaml 4.04.1
- OCamlbuild 0.11.0
- Menhir 20170418

The Makefile also uses [imagemagick] and [wkhtmltopdf] to build documentation images.

If you use [OPAM], just type :
```
opam install ocamlbuild menhir
```

### Manual build
Just type :
```
make
```

This will provide you with a **menhirbrav** executable which you can feed *.mly* files with.

## Usage
```
menhirbrav [latex|html] [options] <file>
```

By default **menhirbrav** defaults to standard output, use `-o <file>` to specify an output file.

### Multi-format output
By default the output format is a simple text format close to the BNF syntax. You can use the subcommands `latex` or `html` to get a LaTeX (resp. HTML) file.

Use the following options to tweak the LaTeX:
- `-tabular`: a *tabular*-based format from the [tabu] package (default)
- `-syntax`: use the [syntax] package
- `-backnaur`: use the [backnaur] package (not recommended: manual line-wrapping through this [trick](https://tex.stackexchange.com/a/308753))

In either cases, the output is customizable *via* the use of LaTeX commands that you can redefine to fit your needs.

The HTML file uses internal CSS stylesheet which allows one to customize the output (in a poorer way than with the `latex` switch).

### Example
Here are the different formats output obtained by **MenhirBrav** from its own [parser](src/parser.mly).

#### Default
```
<specification> ::=
  | <rule>* EOF

<rule> ::=
  | [PUBLIC] [INLINE] LID parameters(<ident>) COLON <optional_bar> (<group> BAR)+

<optional_bar> ::=
  | epsilon
  | BAR

<group> ::=
  | (<production> BAR)+ ACTION [<precedence>]

<production> ::=
  | <producer>* [<precedence>]

<producer> ::=
  | [LID EQ] <actual>

generic_actual(A, B) ::=
  | <ident> parameters(A)
  | B <modifier>

<actual> ::=
  | generic_actual(<lax_actual>, <actual>)

<lax_actual> ::=
  | generic_actual(<lax_actual>, <actual>)
  | (<group> BAR)+

<modifier> ::=
  | OPT
  | PLUS
  | STAR

<precedence> ::=
  | PREC <ident>

parameters(X) ::=
  | [LPAR (X COMMA)* RPAR]

<ident> ::=
  | UID
  | LID
```

#### LaTeX
##### Tabular
![Tabular](doc/tabular.png)

##### Syntax
![Syntax](doc/syntax.png)

##### Backnaur
![Backnaur](doc/backnaur.png)

#### HTML
![HTML](doc/html.png)

[Menhir]: http://gallium.inria.fr/~fpottier/menhir/
[yacc2latex]: http://www-verimag.imag.fr/~raymond/index.php/yacc2latex/
[OCaml]: http://ocaml.org/
[OPAM]: http://opam.ocaml.org/
[wkhtmltopdf]: https://wkhtmltopdf.org/
[imagemagick]: http://www.imagemagick.org/script/index.php
[tabu]: https://www.ctan.org/pkg/tabu
[syntax]: https://www.ctan.org/pkg/syntax-mdw
[backnaur]: https://www.ctan.org/pkg/backnaur
