(* https://raw.githubusercontent.com/drjdn/ocaml_lua_parser/master/src/lib/parser.mly *)
(*
 The MIT License

 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

%{
 open Ast
%}


%token <string> NUM_CONST STR_CONST BOOL IDENT
%token PLUS MINUS MULT DIV MOD CARAT GT LT GE LE EQ NE ASSIGN DOT CAT
%token ELLIPSIS COLON DCOLON SEMI COMMA HASH LCB RCB LPAR RPAR LB RB AND
%token BREAK DO ELSE ELSEIF END FOR FUNCTION IF IN LOCAL NOT OR REPEAT
%token RETURN THEN UNTIL WHILE GOTO EOF NOARG

%nonassoc NOARG
%nonassoc LPAR
%left OR
%left AND
%left LT GT LE GE NE EQ
%right CAT
%left PLUS MINUS
%left MULT DIV MOD
%left NOT HASH
%right CARAT

%start <Ast.ast> prog


%%

prog:
    | chunk EOF   { $1 }
    ;

chunk:
     | statlist retstat OSC   { Slist((extract_list $1) @ [$2]) }
     | statlist               { $1 }
     ;

statlist:
        | OSC                 { Slist([]) }
        | statlist stat OSC   { Slist((extract_list $1) @ [$2]) }
        ;

block:
     | chunk   { $1 }
     ;

stat:
    | varlist ASSIGN explist                                  { Assign($1, $3) }
    | functioncall %prec NOARG                                { $1 }
    | label                                                   { $1 }
    | GOTO ident                                              { Goto($2) }
    | DO block END                                            { Do($2) }
    | WHILE exp DO block END                                  { While($2, $4) }
    | REPEAT block UNTIL exp                                  { Repeat($2, $4) }
    | IF exp THEN block END                                   { If1($2, $4) }
    | IF exp THEN block ELSE block END                        { If2($2, $4, $6) }
    | IF exp THEN block elseifps END                          { If3($2, $4, $5) }
    | IF exp THEN block elseifps ELSE block END               { If4($2, $4, $5, $7) }
    | FOR ident ASSIGN exp COMMA exp DO block END             { For1($2, $4, $6, $8) }
    | FOR ident ASSIGN exp COMMA exp COMMA exp DO block END   { For2($2, $4, $6, $8, $10) }
    | FOR namelist IN explist DO block END                    { Forin($2, $4, $6) }
    | FUNCTION funcname funcbody                              { Function($2, $3) }
    | LOCAL FUNCTION ident funcbody                           { Lfunction($3, $4) }
    | LOCAL namelist                                          { Lnames($2) }
    | LOCAL namelist ASSIGN explist                           { Lassign($2, $4) }
    ;

elseifp:
       | ELSEIF exp THEN block   { Elseif($2, $4) }
       ;

elseifps:
        | elseifp            { Slist([$1]) }
        | elseifps elseifp   { Slist((extract_list $1) @ [$2]) }
        ;

retstat:
       | RETURN            { Return(Elist([])) }
       | RETURN explist    { Return($2) }
       | BREAK             { Break }
       ;

label:
     | DCOLON ident DCOLON   { Label($2) }
     ;

funcname:
        | fname               { $1 }
        | fname COLON ident   { Member($1, $3) }
        ;

varlist:
       | var                 { Elist([$1]) }
       | varlist COMMA var   { Elist((extract_list $1) @ [$3]) }
       ;

var:
   | ident               { $1 }
   | bracket key         { Clist([$1; $2]) }
   | functioncall key    { Clist([$1; $2]) }
   | var key             { Clist([$1; $2]) }
   ;

key:
   | LB exp RB   { Key1($2) }
   | DOT ident   { Key2($2) }
   ;

fname:
     | ident             { FNlist([$1]) }
     | fname DOT ident   { FNlist((extract_list $1) @ [$3]) }
     ;

namelist:
        | ident                  { Elist([$1]) }
        | namelist COMMA ident   { Elist((extract_list $1) @ [$3]) }
        ;

explist:
       | exp                 { Elist([$1]) }
       | explist COMMA exp   { Elist((extract_list $1) @ [$3]) }
       ;

exp:
   | primary                    { $1 }
   | var %prec NOARG            { $1 }
   | functioncall %prec NOARG   { $1 }
   | exp OR exp                 { Binop("or", $1, $3) }
   | exp AND exp                { Binop("and", $1, $3) }
   | exp LT exp                 { Binop("<", $1, $3) }
   | exp GT exp                 { Binop(">", $1, $3) }
   | exp LE exp                 { Binop("<=", $1, $3) }
   | exp GE exp                 { Binop(">=", $1, $3) }
   | exp NE exp                 { Binop("~=", $1, $3) }
   | exp EQ exp                 { Binop("==", $1, $3) }
   | exp CAT exp                { Binop("..", $1, $3) }
   | exp PLUS exp               { Binop("+", $1, $3) }
   | exp MINUS exp              { Binop("-", $1, $3) }
   | exp MULT exp               { Binop("*", $1, $3) }
   | exp DIV exp                { Binop("/", $1, $3) }
   | exp MOD exp                { Binop("%", $1, $3) }
   | exp CARAT exp              { Binop("^", $1, $3) }
   | NOT exp                    { Unop("not ", $2) }
   | HASH exp                   { Unop("#", $2) }
   | MINUS exp %prec NOT        { Unop("- ", $2) }
   ;

primary:
       | BOOL                  { Bool($1) }
       | NUM_CONST             { Number($1) }
       | STR_CONST             { String($1) }
       | ELLIPSIS              { Ellipsis }
       | functiondef           { $1 }
       | tableconstructor      { $1 }
       | bracket %prec NOARG   { $1 }
       ;

bracket:
       | LPAR exp RPAR         { Pexp($2) }
       ;

functioncall:
            | bracket args                    { Clist([$1; $2]) }
            | bracket COLON ident args        { Mcall($1, $3, $4) }
            | var args                        { Clist([$1; $2]) }
            | var COLON ident args            { Mcall($1, $3, $4) }
            | functioncall args               { Clist((extract_list $1) @ [$2]) }
            | functioncall COLON ident args   { Mcall($1, $3, $4) }
            ;

args:
    | LPAR RPAR           { Args(Elist([])) }
    | LPAR explist RPAR   { Args($2) }
    | tableconstructor    { $1 }
    | STR_CONST           { String($1) }
    ;

functiondef:
           | FUNCTION funcbody   { FunctionE($2) }
           ;

funcbody:
        | LPAR RPAR block END           { Fbody(Elist([]), $3) }
        | LPAR parlist RPAR block END   { Fbody($2, $4) }
        ;

parlist:
       | namelist                  { $1 }
       | namelist COMMA ELLIPSIS   { Vargs($1) }
       | ELLIPSIS                  { Ellipsis }
       ;

tableconstructor:
                | LCB RCB             { Table(Elist([])) }
                | LCB fieldlist RCB   { Table($2) }
                ;

fieldlist:
         | fields            { $1 }
         | fields fieldsep   { $1 }
         ;

fields:
      | field                   { Elist([$1]) }
      | fields fieldsep field   { Elist((extract_list $1) @ [$3]) }
      ;

field:
     | LB exp RB ASSIGN exp   { Fassign($2, $5) }
     | ident ASSIGN exp       { Assign($1, $3) }
     | exp                    { $1 }
     ;

fieldsep:
        | COMMA   {}
        | SEMI    {}
        ;

ident:
     | IDENT   { Ident($1) }
     ;

OSC :
    |        {}
    | SEMI   {}
    ;
