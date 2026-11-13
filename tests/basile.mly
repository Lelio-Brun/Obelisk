
/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <string> IDENT 
%token STRUCT END
%token EOF
%token T_PAAMAYIM_NEKUDOTAYIM "::" SEMI ";"

/* Définitions des priorités et associativités des tokens */

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs retournées par l'analyseur syntaxique */
%type <Ast.file> file
%type <Ast.pdecl_desc> decl_desc
%type <Ast.pdecl_desc Ast.loc> decl

%%

let file :=
    | d = decl; EOF; { [d] }

decl_desc:
| STRUCT ps = separated(";", param) END
    { PDstruct ({txt="f";loc=(Lexing.dummy_pos, Lexing.dummy_pos)}, ps, false) }
;

let decl :=
| d = mkloc(decl_desc); { d }

let separated(SEP, prod) :=
| /* empty */ { [] }
| SEP; ps = separated(SEP, prod); { ps }
| p = prod; { [p] }
| hd = prod; SEP; tl = separated(SEP, prod); { hd :: tl }

let param :=
| p = mkloc(IDENT); t = option("::"; i = mkloc(IDENT); { i }); { (p, t) }

let mkloc(desc) :=
| txt = desc; { { txt; loc= $sloc } }
