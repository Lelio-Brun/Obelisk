# Obelisk <span style="float: right">![Build Status](https://github.com/Lelio-Brun/Obelisk/actions/workflows/main.yml/badge.svg?branch=master) [![Mentioned in Awesome OCaml](https://awesome.re/mentioned-badge.svg)](https://github.com/rizo/awesome-ocaml)</span>

Obelisk is a simple tool that produces pretty-printed output from a [Menhir] parser file (_.mly_).

It is inspired by [yacc2latex] and is also written in [OCaml] but is aimed at supporting features from Menhir instead of only those of [ocamlyacc].

## Installation
### Dependencies
- [OCaml] >= 4.08
- [Dune] >= 2.2.0
- [Menhir]
- [Re]

The Makefile also uses [imagemagick] and [wkhtmltopdf] to build documentation images.

In addition to the package [suffix], which is used to define starred commands, here is a summary of package dependencies for the different LaTeX modes: 

- `-tabular` :
  + [tabu]
  + [longtable] as a dependency of [tabu] to use the environment `longtabu` 
- `-syntax` : [syntax] from the bundle [mdwtools] 
- `-backnaur` : [backnaur]

### OPAM
If you use [OPAM], simply type:
```
opam install obelisk
```

### Manual installation
`git clone` to clone the Obelisk repository,y, then type:
```
dune build
```

This will provide you with an executable which you can feed _.mly_ files with: `dune exec src/main.exe -- <options> <file.mly>`.

If you want to install Obelisk, you can type:
```
dune install [--prefix <the destination directory>]
```

## Usage
```
obelisk [ebnf|latex|html] [options] <files>
```

If multiple files are specified, Obelisk will output a concatenated result without consistency checks. 
The user is responsible for avoiding, e.g., name clashes between the several files.

By default, Obelisk defaults to standard output; use `-o <file>` to specify an output file.

### Pattern recognition
Obelisk can infer some common patterns (possibly parameterized):
- options
- lists and non-empty lists
- separated lists and non-empty separated lists

Once recognized, if the `-i` switch is specified, the rules are deleted, and their instances are replaced with default constructions (e.g., *\_\**, *\_+*, *[\_]*).
Without the `-i` flag, only the productions of the recognized rules are replaced, and the total number of rules remains the same.

For example, on these simple rules (from this [file](misc/reco.mly)):
```
my_option(X, Y):
  |     {}
  | Y X {}

my_list(A):
  |              {}
  | A my_list(A) {}

my_nonempty_list(C):
  | C                     {}
  | C my_nonempty_list(C) {}

my_separated_nonempty_list(X,Y):
  | X                                   {}
  | X Y my_separated_nonempty_list(X,Y) {}

my_separated_list(X,S):
  |                                 {}
  | my_separated_nonempty_list(X,S) {}

my_rule:
  | my_option(E, F)                    {}
  | my_list(E)                         {}
  | my_nonempty_list(F)                {}
  | my_separated_nonempty_list(E,S1)   {}
  | my_separated_list(F,S2)            {}
```
Obelisk (`obelisk misc/reco.mly`) outputs:
```
<my_option(X, Y)> ::= [Y X]

<my_list(A)> ::= A*

<my_nonempty_list(C)> ::= C+

<my_separated_nonempty_list(X, Y)> ::= X (Y X)*

<my_separated_list(X, S)> ::= [X (S X)*]

<my_rule> ::= <my_option(E, F)>
            | <my_list(E)>
            | <my_nonempty_list(F)>
            | <my_separated_nonempty_list(E, S1)>
            | <my_separated_list(F, S2)>
```
And with the `-i` switch (`obelisk -i misc/reco.mly`):
```
<my_rule> ::= [F E]  
            | E*
            | F+
            | E (S1 E)*
            | [F (S2 F)*]
```

### Multi-format output
By default, the output format is a simple text format close to the BNF syntax.
You can use the subcommands `ebnf`, `latex` or `html` to get, respectively, an EBNF text output,  LaTeX output, or HTML output.

In default, EBNF, and HTML mode, the option `-noaliases` avoid printing token aliases in the output.

#### EBNF 
In EBNF mode, parameterized rules are specialized into dedicated regular rules. 
On the example above (`obelisk ebnf misc/reco.mly`):

```
my_rule ::= my_option_0
          | my_list_0
          | my_nonempty_list_0
          | my_separated_nonempty_list_0
          | my_separated_list_0

my_option_0 ::= (F E)?

my_nonempty_list_0 ::= F+

my_separated_nonempty_list_1 ::= F (S2 F)*

my_separated_list_0 ::= (F (S2 F)*)?

my_separated_nonempty_list_0 ::= E (S1 E)*

my_list_0 ::= E*
```
And with the `-i` switch (`obelisk ebnf -i misc/reco.mly`):

```
my_rule ::= (F E)?   
          | E*
          | F+
          | E (S1 E)*
          | (F (S2 F)*)?
```

#### LaTeX
Use the following options to tweak the LaTeX:
- `-tabular`: a *tabular*-based format from the [tabu] package (default)
- `-syntax`: use the [syntax] package
- `-backnaur`: use the [backnaur] package (not recommended: manual line-wrapping through this [trick](https://tex.stackexchange.com/a/308753))

Either way, the output may be customized using LaTeX commands that you can redefine to fit your needs.
The command names are auto-generated from the terminal names, and because of LaTeX limitations, underscores are removed, and numbers are converted into their Roman form.

By default, in LaTeX mode, the `-o <grammar.tex>` switch will produce the standalone LaTeX file _<grammar.tex>_ which you can directly compile (e.g. with _pdflatex_).

But in conjunction with `-o <grammar.tex>`, you can use `-package <definitions>` to output two files:
1. a LaTeX file _<grammar.tex>_ containing only the grammar contents ;
2. a package file _<definitions.sty>_ (the _.sty_ extension is added automatically) containing the necessary extra package requirements and command definitions.

These two files are then intended to be included in a user-provided main LaTeX file following this example skeleton:
```latex
\documentclass[preview]{standalone}

\usepackage{definitions}

\begin{document}

\include{grammar}

\end{document}
```

To avoid name clashes, in particular when using the `-package` option and, e.g., importing multiple grammars with the same LaTeX command names, or in the case where one of the syntax construction names matches one already defined LaTeX macro, you can specify a common prefix for the commands with the option `-prefix <myprefix>`.

> [!WARNING]
> As `end`-beginning commands are forbidden in LaTeX, commands created from rules with names beginning with `end` are automatically prefixed with `zzz`.

#### HTML
The HTML file uses an internal CSS stylesheet that allows customizing the output (in a poorer way than in the `latex` mode).
The stylesheet uses `content` properties for some parts of the grammar by default (`-css` option) to make it modular and easily modifiable. 
Still, some symbols are not treated as content and, for example, are not copy-pastable. 
Use the `-nocss` option to turn off the use of such properties.

### Example
Here are the outputs of the different formats obtained by Obelisk from its own [parser](src/parser. my).

#### Default
```
<specification> ::= <rule>* EOF

<rule> ::= <old_rule>
         | <new_rule>

<old_rule> ::= [<flags>] <ident> ATTRIBUTE* <parameters(<ident>)> COLON
               <optional_bar> <group> (BAR <group>)* SEMICOLON*

<flags> ::= PUBLIC
          | INLINE
          | PUBLIC INLINE
          | INLINE PUBLIC

<optional_bar> ::= [BAR]

<group> ::= <production> (BAR <production>)* ACTION [<precedence>]

<production> ::= <producer>* [<precedence>]

<producer> ::= [LID EQ] <actual> ATTRIBUTE* SEMICOLON*

<generic_actual(A, B)> ::= <ident> <parameters(A)>
                         | B <modifier>

<actual> ::= <generic_actual(<lax_actual>, <actual>)>

<lax_actual> ::= <generic_actual(<lax_actual>, <actual>)>
               | <group> (BAR <group>)*

<new_rule> ::= [PUBLIC] LET LID ATTRIBUTE* <parameters(<ident>)> <binder>
               <expression>

<binder> ::= COLONEQ
           | EQEQ

<expression> ::= <optional_bar> <seq_expression> (BAR <seq_expression>)*

<seq_expression> ::= [<pattern> EQ] <symbol_expression> SEMICOLON
                     <seq_expression>
                   | <symbol_expression>
                   | <action_expression>

<symbol_expression> ::= <ident> <parameters(<expression>)>
                      | <symbol_expression> <modifier>

<action_expression> ::= <action>
                      | <action> <precedence>
                      | <precedence> <action>

<action> ::= ACTION
           | POINTFREEACTION

<pattern> ::= LID
            | UNDERSCORE
            | TILDE
            | LPAR [<pattern> (COMMA <pattern>)*] RPAR

<modifier> ::= OPT
             | PLUS
             | STAR

<precedence> ::= PREC <ident>

<parameters(X)> ::= [LPAR [X (COMMA X)*] RPAR]

<ident> ::= UID
          | LID
          | QID
```

#### EBNF 

```
specification ::= rule* EOF           

rule ::= old_rule
       | new_rule

old_rule ::= flags? ident ATTRIBUTE* parameters_0 COLON optional_bar group
             (BAR group)* SEMICOLON*

flags ::= PUBLIC
        | INLINE
        | PUBLIC INLINE
        | INLINE PUBLIC

optional_bar ::= BAR?

group ::= production (BAR production)* ACTION precedence?

production ::= producer* precedence?

producer ::= (LID EQ)? actual ATTRIBUTE* SEMICOLON*

actual ::= generic_actual_0

lax_actual ::= generic_actual_0
             | group (BAR group)*

new_rule ::= PUBLIC? LET LID ATTRIBUTE* parameters_0 binder expression

binder ::= COLONEQ
         | EQEQ

expression ::= optional_bar seq_expression (BAR seq_expression)*

seq_expression ::= (pattern EQ)? symbol_expression SEMICOLON seq_expression
                 | symbol_expression
                 | action_expression

symbol_expression ::= ident parameters_2
                    | symbol_expression modifier

action_expression ::= action
                    | action precedence
                    | precedence action

action ::= ACTION
         | POINTFREEACTION

pattern ::= LID
          | UNDERSCORE
          | TILDE
          | LPAR (pattern (COMMA pattern)*)? RPAR

modifier ::= OPT
           | PLUS
           | STAR

precedence ::= PREC ident

ident ::= UID
        | LID
        | QID

generic_actual_0 ::= ident parameters_1
                   | actual modifier

parameters_1 ::= (LPAR (lax_actual (COMMA lax_actual)*)? RPAR)?

parameters_0 ::= (LPAR (ident (COMMA ident)*)? RPAR)?

parameters_2 ::= (LPAR (expression (COMMA expression)*)? RPAR)?
```

#### LaTeX
##### Tabular
![Tabular](misc/tabular.png)

##### Syntax
![Syntax](misc/syntax.png)

##### Backnaur
![Backnaur](misc/backnaur.png)

#### HTML
##### With CSS content properties
![HTMLCSS](misc/htmlcss.png) 

##### Without CSS content properties
![HTMLNOCSS](misc/html.png) 

[Menhir]: http://gallium.inria.fr/~fpottier/menhir/
[Re]: https://github.com/ocaml/ocaml-re/
[Dune]: https://github.com/ocaml/dune/
[yacc2latex]: http://www-verimag.imag.fr/~raymond/index.php/yacc2latex/
[ocamlyacc]: https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html#sec307
[OCaml]: http://ocaml.org/
[OPAM]: http://opam.ocaml.org/
[wkhtmltopdf]: https://wkhtmltopdf.org/
[imagemagick]: http://www.imagemagick.org/script/index.php
[suffix]: https://ctan.org/pkg/suffix
[tabu]: https://www.ctan.org/pkg/tabu
[longtable]: https://www.ctan.org/pkg/longtable
[mdwtools]: https://www.ctan.org/pkg/mdwtools
[syntax]: https://www.ctan.org/pkg/syntax-mdw
[backnaur]: https://www.ctan.org/pkg/backnaur
