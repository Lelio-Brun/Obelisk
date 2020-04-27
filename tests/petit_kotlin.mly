%{
open Printf
open Ast
open Position

let mk_var (v_mut, v_name, v_type, v_expr) =
  { v_name; v_type; v_expr; v_mut }
let mk_class (c_name, c_types, c_params, c_vars) =
  { c_name; c_types; c_params; c_vars }
let mk_fun (f_types, f_name, f_params, return, f_body) =
  { f_name; f_types; f_params;
    f_return = (match return with
               | Some t -> t
               | None -> locate (TClass ("Unit", [])) dummy);
    f_body;
    f_env = [] }
%}

%token IF ELSE WHILE
%token TRUE FALSE NULL THIS
%token FUN RETURN VAR VAL
%token CLASS DATA

%token <string> IDENT
%token <string> STRING
%token <int> INT

%token EOF

%token SEMICOL ";" COMMA "," COLON ":"
%token ASSIGN "=" ARROW "->" DOT "." IDOT "?."
%token LPAREN "(" RPAREN ")" LBRACE "{" RBRACE "}"

%token REQUAL "===" RDIFF "!==" EQUAL "==" DIFF "!="
%token LT "<" LE "<=" GT ">" GE ">="
%token PLUS "+" MINUS "-" MUL "*" DIV "/" MOD "%"
%token AND "&&" OR "||"
%token NOT "!" INTER "?"

%nonassoc p_then
%nonassoc ELSE
%nonassoc p_while RETURN
%right ASSIGN
%left OR
%left AND
%left REQUAL RDIFF EQUAL DIFF
%left GT GE LT LE
%left PLUS MINUS
%left MUL DIV MOD
%right unaryminus NOT
%left DOT IDOT
%right ARROW
%nonassoc INTER

%start<Ast.file> file

%%

let file := ~=decl*; EOF; <>

let decl := loc(
  | ~=decl_var;   < DeclVar >
  | ~=decl_class; < DeclClass >
  | ~=decl_fun;   < DeclFun >)

let decl_var := ~=var; ";"; <>

let var :=
  ~=mut; ~=loc(IDENT); ~=preceded(":", loc(typ))?; "="; ~=expr;
  < mk_var >

let decl_class :=
  DATA; CLASS; ~=loc(IDENT); ~=poly_param(loc(IDENT));
  "("; ~=separated_nonempty_list(",", class_param); ")";
  ~=loption(delimited("{", loption(sep_term_list(";", var)), "}"));
  < mk_class >

let decl_fun :=
  FUN; ~=poly_param(loc(IDENT)); ~=loc(IDENT);
  "("; ~=separated_list(",", param); ")";
  ~=preceded(":", loc(typ))?; ~=loc(block);
  < mk_fun >

let mut :=
  | VAR; { true }
  | VAL; { false }

let class_param := b=mut; x=loc(IDENT); ":"; t=loc(typ); { x, t, b }

let param := ~=loc(IDENT); ":"; ~=loc(typ); <>

let poly_param(X) :=
  loption(delimited("<", separated_nonempty_list(",", X), ">"))

let typ :=
  | ~=IDENT; ~=poly_param(typ);                        < TClass >
  | ~=typ; "?";                                        < TNullable >
  | "("; ts=separated_list(",", typ); ")";             { match ts with
                                                         | [t] -> t
                                                         | _ -> raise Common.ParserError }
  | "("; ~=separated_list(",", typ); ")"; "->"; ~=typ; < TArrow >

let expr := loc(untyped(
  | ~=INT;                                                < Int >
  | ~=STRING;                                             < String >
  | TRUE;                                                 { Bool true }
  | FALSE;                                                { Bool false }
  | THIS;                                                 { This }
  | NULL;                                                 { Null }
  | ~=access;                                             < Access >
  | ~=access; "="; ~=expr;                                < Assign >
  | ~=loc(IDENT); "("; ~=separated_list(",", expr); ")";  < Funcall >
  | "!"; ~=expr;                                          < Not >
  | "-"; ~=expr;                        %prec unaryminus  < Neg >
  | e1=expr; op=binop; e2=expr;                           { Binop (op, e1, e2) }
  | IF; "("; ~=expr; ")"; ~=blockexpr; ELSE; b=blockexpr; < If >
  | IF; "("; e=expr; ")"; t=blockexpr;       %prec p_then { If (e, t, locate [] dummy) }
  | WHILE; "("; ~=expr; ")"; ~=blockexpr;   %prec p_while < While >
  | RETURN; ~=ioption(expr);                              < Return >
  | FUN; "("; ~=separated_list(",", param); ")";
    ~=preceded(":", loc(typ))?; ~=loc(block);             < Fun >
  | "("; ~=expr; ")";                                     { (cont expr).cont }))

let block :=
  "{"; ~=loption(sep_term_list(";", block_elt)); "}"; <>

let block_elt :=
  | ~=var;  < BlockVar >
  | ~=expr; < BlockExpr >

let blockexpr ==
  | ~=loc(block); <>
  | e=expr;       { locate [BlockExpr e] (pos e) }

let binop ==
  | "==="; { RefEqual }
  | "!=="; { RefDiff }
  | "==";  { Equal }
  | "!=";  { Diff }
  | "<";   { Lt }
  | "<=";  { Le }
  | ">";   { Gt }
  | ">=";  { Ge }
  | "+";   { Add }
  | "-";   { Sub }
  | "*";   { Mul }
  | "/";   { Div }
  | "%";   { Mod }
  | "&&";  { And }
  | "||";  { Or }

let access :=
  | ~=loc(IDENT);               < Var >
  | ~=expr; "."; ~=loc(IDENT);  < Field >
  | ~=expr; "?."; ~=loc(IDENT); < SafeField >

let loc(X) == x=X; { locate_from x $sloc }

let untyped(X) == cont=X; { { cont; typ = None } }

let sep_term_list(SEP, X) :=
  | x=X; ioption(";");                  { [x] }
  | x=X; ";"; xs=sep_term_list(SEP, X); { x :: xs }
