(* Origin: https://gitlab.com/o-labs/solidity-parser-ocaml/-/blob/master/src/lib_parser/parser.mly *)
%{
(**************************************************************************)
(*                                                                        *)
(*  Copyright 2020 Origin-Labs, Dune Network and OCamlPro                 *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  any later version.                                                    *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <https://www.gnu.org/licenses/>.*)
(**************************************************************************)

  open Solidity_types

  type ambiguous_type_name_or_expression =
    | AmbiguousIdentifier of string list
    | AmbiguousArray of ambiguous_type_name_or_expression * expression option

  let rec expression_of_identifiers = function
    | [] -> assert false
    | [x] -> IdentifierExpression x
    | x :: y -> FieldExpression (expression_of_identifiers y, x)

  let rec expression_of_ambiguity = function
    | AmbiguousIdentifier l -> expression_of_identifiers (List.rev l)
    | AmbiguousArray (a, expo) -> ArrayAccess (expression_of_ambiguity a, expo)

  let rec type_name_of_ambiguity = function
    | AmbiguousIdentifier l -> UserDefinedType l
    | AmbiguousArray (a, expo) -> Array (type_name_of_ambiguity a, expo)

  let import import_from import_symbols =
    { import_from; import_symbols }

  let rec put_in_none_list l n =
    if n <= 0 then l
    else put_in_none_list (None :: l) (n-1)

  let decimal_to_rational (i, d, e) =
    let i = match i with Some i -> i | None -> Z.zero in
    let d = match d with Some d -> d | None -> Z.zero in
    let e = match e with Some e -> e | None -> 0 in
    let nb_dec =
      if Z.equal Z.zero d then 0
      else String.length (Z.to_string d)
    in
    let i = Q.make i Z.one in
    let d = Q.make d (Z.pow (Z.of_int 10) nb_dec) in
    let e = Q.make (Z.pow (Z.of_int 10) e) Z.one in
    let n = Q.mul (Q.add i d) e in
    Q.num n, Q.den n

  let ctxt_modifier = ref false
%}

(* Keywords *)
%token <Solidity_types.Ident.t * string> PRAGMA
%token IMPORT
%token AS
%token FROM
%token ABSTRACT
%token CONTRACT
%token INTERFACE
%token LIBRARY
%token IS
%token USING
%token PUBLIC
%token PRIVATE
%token EXTERNAL
%token INTERNAL
%token PAYABLE
%token VIEW
%token PURE
%token CONSTANT
%token IMMUTABLE
%token OVERRIDE
%token VIRTUAL
%token MEMORY
%token STORAGE
%token CALLDATA
%token INDEXED
%token ANONYMOUS
%token FUNCTION
%token MODIFIER
%token CONSTRUCTOR
%token RECEIVE
%token FALLBACK
%token RETURNS
%token EVENT
%token STRUCT
%token ENUM
%token MAPPING
%token BOOL
%token <int option> INT
%token <int option> UINT
%token <(int * int) option> FIXED
%token <(int * int) option> UFIXED
%token ADDRESS
%token STRING
%token <int option> BYTES
%token VAR
%token RETURN
%token CONTINUE
%token BREAK
%token THROW
%token DELETE
%token NEW
%token EMIT
%token IF
%token ELSE
%token FOR
%token WHILE
%token DO
%token TRY
%token CATCH

(* Punctuation *)
%token SHARP
%token SEMI
%token COMMA
%token DOT
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token PLUS
%token MINUS
%token DIV
%token PERCENT
%token STAR
%token STARSTAR
%token PLUSPLUS
%token MINUSMINUS
%token GREATERGREATER
%token LESSLESS
%token PIPE
%token AMPER
%token XOR
%token NOT
%token BANG
%token PIPEPIPE
%token AMPERAMPER
%token EQUALEQUAL
%token BANGEQUAL
%token LESS
%token LESSEQUAL
%token GREATER
%token GREATEREQUAL
%token EQUAL
%token PLUSEQUAL
%token MINUSEQUAL
%token STAREQUAL
%token DIVEQUAL
%token PERCENTEQUAL
%token PIPEEQUAL
%token AMPEREQUAL
%token XOREQUAL
%token LESSLESSEQUAL
%token GREATERGREATEREQUAL
%token QUESTION
%token COLON
%token EQUALGREATER

(* Literals *)
%token <bool> BOOLEANLITERAL
%token <Z.t option * Z.t option * int option> NUMBER
%token <Solidity_types.number_unit> NUMBERUNIT
%token <string> ADDRESSLITERAL
%token <string> HEXNUMBER
%token <string> STRINGLITERAL
%token <string> HEXSTRINGLITERAL
%token <string> IDENTIFIER
%token EOF

(* Some convenience precedences *)
%nonassoc below_IDENTIFIER
%nonassoc IDENTIFIER
%nonassoc FROM CONSTRUCTOR RECEIVE FALLBACK

%nonassoc below_mutability
%nonassoc CONSTANT (* IMMUTABLE PURE VIEW PAYABLE *)

%nonassoc below_visibility
%nonassoc PUBLIC PRIVATE INTERNAL (* EXTERNAL *)

%nonassoc below_SEMI
%nonassoc SEMI

%nonassoc below_RETURNS
%nonassoc RETURNS

%nonassoc below_ELSE
%nonassoc ELSE

(* Operator precedence according to the Solidity documentation *)
%left COMMA
%right EQUAL PIPEEQUAL XOREQUAL AMPEREQUAL LESSLESSEQUAL GREATERGREATEREQUAL
       PLUSEQUAL MINUSEQUAL STAREQUAL DIVEQUAL PERCENTEQUAL
%right QUESTION COLON
%left PIPEPIPE
%left AMPERAMPER
%left EQUALEQUAL BANGEQUAL
%left LESS GREATER LESSEQUAL GREATEREQUAL
%left PIPE
%left XOR
%left AMPER
%left LESSLESS GREATERGREATER
%left PLUS MINUS
%left STAR DIV PERCENT
%left STARSTAR
%right unary_op (* PLUSPLUS MINUSMINUS PLUS MINUS DELETE BANG NOT *)

%left PLUSPLUS MINUSMINUS (* postfix *)

%nonassoc below_paren
%nonassoc LPAREN RPAREN

%nonassoc below_LBRACKET
%nonassoc LBRACKET (* RBRACKET *)

%nonassoc below_DOT
%left DOT

(* Entry points *)
%type <Solidity_types.module_> top_module
%start top_module

%%

top_module:
  | maybe_sharp source_units { $2 }
;;

maybe_sharp:
  | (* epsilon *)          { () }
  | SHARP IDENTIFIER COLON { () }
  | SHARP IDENTIFIER       { () }
;;

source_units:
  | EOF                                { [] }
  | PRAGMA source_units                { Pragma ($1) :: $2 }
  | import_directive SEMI source_units { Import ($1) :: $3 }
  | contract_definition source_units   { ContractDefinition ($1) :: $2 }
  | type_definition source_units       { GlobalTypeDefinition ($1) :: $2 }
;;

import_directive:
  | IMPORT STRINGLITERAL maybe_as_identifier
      { import $2 (ImportAll (false, $3)) }
  | IMPORT STAR maybe_as_identifier FROM STRINGLITERAL
      { import $5 (ImportAll (true, $3)) }
  | IMPORT identifier maybe_as_identifier FROM STRINGLITERAL
      { import $5 (ImportIdents [($2, $3)]) }
  | IMPORT LBRACE import_declarations RBRACE FROM STRINGLITERAL
      { import $6 (ImportIdents $3) }
;;

maybe_as_identifier:
  | (* epsilon *) { None }
  | AS identifier { Some ($2) }
;;

import_declarations:
  | identifier maybe_as_identifier                           { [($1, $2)] }
  | identifier maybe_as_identifier COMMA import_declarations { ($1, $2) :: $4 }
;;

contract_definition:
  | maybe_abstract contract_kind identifier maybe_is_inheritance_specifiers
        LBRACE contract_parts RBRACE
      { { contract_abstract = $1;
          contract_kind = $2;
          contract_name = $3;
          contract_inheritance = $4;
          contract_parts = $6; } }
;;

maybe_abstract:
  | (* epsilon *) { false }
  | ABSTRACT      { true }

contract_kind:
  | CONTRACT  { KindContract }
  | LIBRARY   { KindLibrary }
  | INTERFACE { KindInterface }
;;

maybe_is_inheritance_specifiers:
  | (* epsilon *)             { [] }
  | IS inheritance_specifiers { $2 }
;;

inheritance_specifiers:
  | inheritance_specifier                              { [$1] }
  | inheritance_specifier COMMA inheritance_specifiers { $1 :: $3 }
;;

inheritance_specifier:
  | long_ident                               { ($1, []) }
  | long_ident LPAREN expression_list RPAREN { ($1, $3) }
;;

contract_parts:
  | (* epsilon *)                { [] }
  | contract_part contract_parts { $1 :: $2 }
;;

contract_part:
  | type_name_no_function variable_modifier_list identifier
        maybe_equal_expression SEMI
      { StateVariableDeclaration {
            var_name = $3;
            var_type = $1;
            var_modifiers = $2;
            var_init = $4; } }
  | function_type_name variable_modifier_list identifier
        maybe_equal_expression SEMI
      { StateVariableDeclaration {
            var_name = $3;
            var_type = $1;
            var_modifiers = $2;
            var_init = $4; } }
  | USING long_ident FOR type_name_or_star SEMI
      { UsingForDeclaration ($2, None) }
  | type_definition
      { TypeDefinition ($1) }
  | _MODIFIER identifier maybe_parameter_list modifier_modifier_list block
      { ctxt_modifier := false;
        ModifierDefinition {
            mod_name = $2;
            mod_params = $3;
            mod_modifiers = $4;
            mod_body = $5; } }
  | FUNCTION parameter_list anonymous_function_modifier_list
        maybe_returns maybe_function_body
      { FunctionDefinition {
            fun_name = "*fallback*";
            fun_params = $2;
            fun_modifiers = $3;
            fun_return = $4;
            fun_body = $5; } }
  | function_descriptor parameter_list function_modifier_list
        maybe_returns maybe_function_body
      { FunctionDefinition {
            fun_name = $1;
            fun_params = $2;
            fun_modifiers = $3;
            fun_return = $4;
            fun_body = $5; } }
  | EVENT identifier event_parameter_list maybe_anonymous SEMI
      { EventDefinition {
            event_name = $2;
            event_params = $3;
            event_anon = $4; } }
;;

_MODIFIER:
  | MODIFIER { ctxt_modifier := true }

type_name_or_star:
  | type_name { Some ($1) }
  | STAR      { None }
;;

type_definition:
  | STRUCT identifier LBRACE variable_declaration_semi_list RBRACE
      { StructDefinition ($2, $4) }
  | ENUM identifier LBRACE enum_value_list RBRACE
      { EnumDefinition ($2, $4) }
;;

variable_declaration_semi_list:
  | (* epsilon *)                                            { [] }
  | variable_declaration SEMI variable_declaration_semi_list { $1 :: $3 }
;;

override_specifier:
  | OVERRIDE                               { ModifierOverride ([]) }
  | OVERRIDE LPAREN long_ident_list RPAREN { ModifierOverride ($3) }
;;

long_ident_list:
  | (* epsilon *)            { [] }
  | long_ident_nonempty_list { $1 }
;;

long_ident_nonempty_list:
  | long_ident                                { [$1] }
  | long_ident COMMA long_ident_nonempty_list { $1 :: $3 }
;;

function_descriptor:
  | FUNCTION identifier { $2 }
  | CONSTRUCTOR         { "*constructor*" }
  | RECEIVE             { "*receive*" }
  | FALLBACK            { "*fallback*" }
;;

return_list:
  | LPAREN RPAREN                      { [] }
  | LPAREN return_nonempty_list RPAREN { $2 }
;;

%inline maybe_returns:
  | (* epsilon *)       { None }
  | RETURNS return_list { Some ($2) }
;;

return_nonempty_list:
  | type_name maybe_storage                            { [($1, $2)] }
  | type_name maybe_storage COMMA return_nonempty_list { ($1, $2) :: $4 }

maybe_function_body:
  | SEMI  { None }
  | block { Some ($1) }
;;

function_modifier_list:
  | (* epsilon *)                            { [] }
  | function_modifier function_modifier_list { $1 :: $2 }
;;

function_modifier:
  | modifier_invocation { $1 }
  | state_mutability    { $1 }
  | internal_external   { $1 }
  | public_private      { $1 }
  | VIRTUAL             { ModifierVirtual }
  | override_specifier  { $1 }
;;

modifier_modifier_list:
  | (* epsilon *)                            { [] }
  | modifier_modifier modifier_modifier_list { $1 :: $2 }
;;

modifier_modifier:
  | VIRTUAL             { ModifierVirtual }
  | override_specifier  { $1 }
;;

(* Parsing of anonymous functions has some restrictions
   on the arrangement of modifiers that can be parsed.
   This is a parse-level restriction that also exists in the original parser.
   Basically, the following arrangements are allowed:
     - state_mutability*
     - state_mutability* (public|private|mod_invoc)
                                 extra_anonymous_function_modifier*
     - state_mutability* (internal|external) state_mutability*
     - state_mutability* (internal|external) state_mutability*
                                 (mod_invoc) extra_anonymous_function_modifier*
   These arrangements can be expressed using the function_type_modifier_list
   rule defined later, extended with modifier_invocation *)
%inline anonymous_function_modifier_list:
  | function_type_modifier_list { $1 }
  | function_type_modifier_list modifier_invocation
        extra_anonymous_function_modifier_list
      { $1 @ ($2 :: $3) }
;;

extra_anonymous_function_modifier_list:
  | (* epsilon *) { [] }
  | extra_anonymous_function_modifier extra_anonymous_function_modifier_list
      { $1 :: $2 }
;;

extra_anonymous_function_modifier:
  | extra_function_type_modifier { $1 }
  | modifier_invocation          { $1 }
;;

%inline variable_modifier_list:
  | (* epsilon *)                   { [] }
  | variable_modifier_nonempty_list { $1 }
;;

variable_modifier_nonempty_list:
  | variable_modifier                                 { [$1] }
  | variable_modifier variable_modifier_nonempty_list { $1 :: $2 }
;;

variable_modifier:
  | public_private     { $1 }
  | INTERNAL           { ModifierInternal }
  | CONSTANT           { ModifierConstant }
  | IMMUTABLE          { ModifierImmutable }
  | override_specifier { $1 }
;;

(* The following expression is ambiguous:
     function () x;
   It can be either:
     - an anonymous (fallback) function with a modifier x and no body
     - a variable x of function type
   The original Solidity compiler favors the second case. We obtain
   this behavior by giving a higher precedence to the semicolon, so
   that the identifier is not reduced as a modifier_invocation. *)
modifier_invocation:
  | identifier %prec below_SEMI { ModifierInvocation ($1, None) }
  | identifier LPAREN expression_list RPAREN
      { ModifierInvocation ($1, Some ($3)) }
;;

enum_value_list:
  | (* epsilon *)            { [] }
  | enum_value_nonempty_list { $1 }
;;

enum_value_nonempty_list:
  | enum_value                                { [$1] }
  | enum_value COMMA enum_value_nonempty_list { $1 :: $3 }
;;

enum_value:
  | identifier { $1 }
;;

maybe_parameter_list:
  | (* epsilon *)  { None }
  | parameter_list { Some ($1) }
;;

parameter_list:
  | LPAREN RPAREN                         { [] }
  | LPAREN parameter_nonempty_list RPAREN { $2 }
;;

parameter_nonempty_list:
  | type_name maybe_storage maybe_identifier { [$1, $2, $3] }
  | type_name maybe_storage maybe_identifier COMMA parameter_nonempty_list
      { ($1, $2, $3) :: $5 }
;;

maybe_storage:
  | (* epsilon *)    { None }
  | storage_location { Some ($1) }
;;

event_parameter_list:
  | LPAREN RPAREN                               { [] }
  | LPAREN event_parameter_nonempty_list RPAREN { $2 }
;;

event_parameter_nonempty_list:
  | type_name maybe_indexed maybe_identifier { [($1, $2, $3)] }
  | type_name maybe_indexed maybe_identifier COMMA
        event_parameter_nonempty_list
      { ($1, $2, $3) :: $5 }
;;

maybe_indexed:
  | (* epsilon *) { NotIndexed }
  | INDEXED       { Indexed }
;;

maybe_anonymous:
  | (* epsilon *) { NotAnonymous }
  | ANONYMOUS     { Anonymous }
;;

variable_declaration:
  | type_name maybe_storage_location identifier { ($1, $2, $3) }
;;

maybe_storage_location:
  | (* epsilon *)    { None }
  | storage_location { Some ($1) }
;;

%inline type_name:
  | non_ambiguous_type_name           { $1 }
  | ambiguous_type_name_or_expression { type_name_of_ambiguity $1 }
;;

type_name_no_function:
  | non_ambiguous_type_name_no_function { $1 }
  | ambiguous_type_name_or_expression   { type_name_of_ambiguity $1 }
;;

non_ambiguous_type_name:
  | non_ambiguous_type_name_no_function    { $1 }
  | function_type_name %prec below_RETURNS { $1 }
;;

non_ambiguous_type_name_no_function:
  | elementary_type_name
      { ElementaryType ($1) }
  | MAPPING LPAREN elementary_type_name EQUALGREATER type_name RPAREN
      { Mapping ($3, $5) }
  | non_ambiguous_type_name LBRACKET maybe_expression RBRACKET
      { Array ($1, $3) }
;;

ambiguous_type_name_or_expression:
  | long_ident
      { AmbiguousIdentifier ($1) }
  | ambiguous_type_name_or_expression LBRACKET maybe_expression RBRACKET
      { AmbiguousArray ($1, $3) }
;;

long_ident:
  | identifier %prec below_DOT { [$1] }
  | identifier DOT long_ident  { $1 :: $3 }
;;

%inline function_type_name:
  | FUNCTION parameter_list function_type_modifier_list maybe_returns
     { let (fun_params, fun_modifiers, fun_return) = ($2, $3, $4) in
        FunctionType {
            fun_type_params = fun_params;
            fun_type_modifiers = fun_modifiers;
            fun_type_returns = fun_return; } }
;;

(* When parsing a function type (as opposed to a function definition),
   only certain arrangements of modifiers are allowed, so as to
   avoid ambiguities when used in a variable declaration.
   This is a parse-level restriction that also exists in the original parser.
   These arrangements are as follows:
     - state_mutability*
     - state_mutability* (internal|external) state_mutability*
     - state_mutability* (public|private) extra_function_type_modifier* *)
function_type_modifier_list:
  | state_mutability_list %prec below_visibility { $1 }
  | state_mutability_list internal_external state_mutability_list
      { $1 @ ($2 :: $3) }
  | state_mutability_list public_private extra_function_type_modifier_list
      { $1 @ ($2 :: $3) }
;;

state_mutability_list:
  | (* epsilon *) %prec below_mutability   { [] }
  | state_mutability state_mutability_list { $1 :: $2 }
;;

extra_function_type_modifier_list:
  | (* epsilon *) %prec below_visibility                           { [] }
  | extra_function_type_modifier extra_function_type_modifier_list { $1 :: $2 }
;;

extra_function_type_modifier:
  | internal_external { $1 }
  | public_private    { $1 }
  | state_mutability  { $1 }
;;

internal_external:
  | EXTERNAL { ModifierExternal }
  | INTERNAL { ModifierInternal }
;;

public_private:
  | PUBLIC   { ModifierPublic }
  | PRIVATE  { ModifierPrivate }
;;

storage_location:
  | MEMORY   { Memory }
  | STORAGE  { Storage }
  | CALLDATA { Calldata }
;;

state_mutability:
  | PURE      { ModifierPure }
  | CONSTANT  { ModifierConstant }
  | VIEW      { ModifierView }
  | PAYABLE   { ModifierPayable }
;;

block:
  | LBRACE statement_list RBRACE { $2 }
;;

statement_list:
  | (* epsilon *)            { [] }
  | statement statement_list { $1 :: $2 }
;;

statement:
  | statement_no_semi          { $1 }
  | statement_before_semi SEMI { $1 }
;;

statement_no_semi:
  | if_statement    { $1 }
  | for_statement   { $1 }
  | while_statement { $1 }
  | try_statement   { $1 }
  | block           { Block ($1) }
;;

statement_before_semi:
  | simple_statement        { $1 }
  | do_while_statement      { $1 }
  | RETURN maybe_expression { Return ($2) }
  | CONTINUE                { Continue }
  | BREAK                   { Break }
  | THROW                   { Throw }
  | EMIT function_call      { let (f, a) = $2 in Emit (f, a) }
;;

if_statement:
  | IF LPAREN expression RPAREN statement %prec below_ELSE
      { IfStatement ($3, $5, None) }
  | IF LPAREN expression RPAREN statement ELSE statement
      { IfStatement ($3, $5, Some ($7)) }
;;

for_statement:
  | FOR LPAREN maybe_simple_statement SEMI maybe_expression SEMI
        maybe_expression RPAREN statement
      { ForStatement ($3, $5, $7, $9) }
;;

while_statement:
  | WHILE LPAREN expression RPAREN statement { WhileStatement ($3, $5) }
;;

(* For now, Solidity only accepts very specific catch clauses.
   It can be either a single one of the following, or a combination
   of the first with one among the second or third:
     - catch Error(string memory ...) { ... }
     - catch (bytes memory ...) { ... }
     - catch { ... } *)
try_statement:
  | TRY expression maybe_returns block catch_clause_nonempty_list
      { TryStatement ($2, $3, $4, $5) }
;;

catch_clause_nonempty_list:
  | catch_clause                            { [$1] }
  | catch_clause catch_clause_nonempty_list { $1 :: $2 }
;;

catch_clause:
  | CATCH block                                 { (None, [], $2) }
  | CATCH maybe_identifier parameter_list block { ($2, $3, $4) }
;;

maybe_simple_statement:
  | (* epsilon *)    { None }
  | simple_statement { Some ($1) }
;;

simple_statement:
  | variable_declaration_statement { VariableDefinition ($1) }
  | expression {
      (* When under a modifier, a single underscore at the
         level of statements is no longer an identifier *)
      match !ctxt_modifier, $1 with
      | true, IdentifierExpression "_" -> PlaceholderStatement
      | _, s -> ExpressionStatement (s) }
;;

do_while_statement:
  | DO statement WHILE LPAREN expression RPAREN { DoWhileStatement ($2, $5) }
;;

variable_declaration_statement:
  | VAR maybe_identifier_list maybe_equal_expression
      { VarInfer ($2, $3) }
  | maybe_variable_declaration_list maybe_equal_expression
      { VarType ($1, $2) }
;;

maybe_identifier_list:
  | identifier                                   { [Some ($1)] }
  | LPAREN RPAREN                                { [] }
  | LPAREN identifier RPAREN                     { [Some ($2)] }
  | LPAREN maybe_identifier_nonempty_list RPAREN { $2 }
;;

maybe_identifier_nonempty_list:
  | maybe_identifier COMMA maybe_identifier               { [$1; $3] }
  | maybe_identifier COMMA maybe_identifier_nonempty_list { $1 :: $3 }
;;

maybe_variable_declaration_list:
  | variable_declaration                                    { [Some ($1)] }
  | LPAREN maybe_variable_declaration_nonempty_list RPAREN  { $2 }
;;

maybe_variable_declaration_nonempty_list:
  | variable_declaration                     { [Some ($1)] }
  | comma_nonempty_list variable_declaration { put_in_none_list [Some ($2)] $1 }
  | maybe_variable_declaration_nonempty_list COMMA maybe_variable_declaration
      { $1 @ [$3] }
;;

comma_nonempty_list:
  | COMMA                     { 1 }
  | COMMA comma_nonempty_list { 1 + $2 }
;;

maybe_variable_declaration:
  | (* epsilon *)        { None }
  | variable_declaration { Some ($1) }
;;

%inline maybe_equal_expression:
  | (* epsilon *)    { None }
  | EQUAL expression { Some ($2) }
;;

elementary_type_name:
  | BOOL                       { TypeBool }
  | INT                        { TypeInt ($1) }
  | UINT                       { TypeUint ($1) }
  | FIXED                      { TypeFixed ($1) }
  | UFIXED                     { TypeUfixed ($1) }
  | ADDRESS                    { TypeAddress (false) }
  | ADDRESS PAYABLE            { TypeAddress (true) }
  | STRING                     { TypeString }
  | BYTES                      { TypeBytes ($1) }
  | VAR %prec below_IDENTIFIER { TypeVar }
;;

expression:
  | non_ambiguous_expression %prec below_LBRACKET { $1 }
  | ambiguous_type_name_or_expression %prec below_LBRACKET
      { expression_of_ambiguity $1 }
;;

maybe_expression:
  | (* epsilon *) { None }
  | expression    { Some ($1) }
;;

expression_list:
  | (* epsilon *)            { [] }
  | expression_nonempty_list { $1 }
;;

expression_nonempty_list:
  | expression                                { [$1] }
  | expression COMMA expression_nonempty_list { $1 :: $3 }
;;

non_ambiguous_expression:
  | NEW type_name %prec below_LBRACKET
      { NewExpression ($2) }
  | non_ambiguous_expression LBRACKET maybe_expression RBRACKET
      { ArrayAccess ($1, $3) }
  | expression LBRACKET maybe_expression COLON maybe_expression RBRACKET
      { ArraySlice ($1, $3, $5) }
  | LBRACKET expression_nonempty_list RBRACKET
      { ImmediateArray ($2) }
  | expression DOT identifier
      { FieldExpression ($1, $3) }
  | function_call
      { let (f, a) = $1 in FunctionCallExpression (f, a) }
  | LPAREN expression RPAREN
      { $2 }
  | tuple_expression
      { $1 }
  | literal_expression
      { $1 }
  | non_ambiguous_type_name LPAREN expression RPAREN
      { FunctionCallExpression (TypeExpression ($1), ExpressionList ([$3])) }
  | LPAREN non_ambiguous_type_name RPAREN LPAREN expression RPAREN
      { FunctionCallExpression (TypeExpression ($2), ExpressionList ([$5])) }
  | PAYABLE LPAREN expression RPAREN
      { FunctionCallExpression
          (TypeExpression (ElementaryType (TypeAddress (true))),
           ExpressionList ([$3])) }
  | inc_dec_op expression %prec unary_op
      { PrefixExpression ($1, $2) }
  | expression inc_dec_op
      { SuffixExpression ($1, $2) }
  | unop expression %prec unary_op
      { PrefixExpression ($1, $2) }
  | expression binop expression
      { BinaryExpression ($1, $2, $3) }
  | expression assignop expression
      { (* When used on the left side of an assignment,
           (exp,) is understood as a 2-element tuple,
           so we undo the 1-element tuple trick *)
        let lexp = match $1 with
          | TupleExpression [Some ($1)] -> TupleExpression [Some ($1); None]
          | e -> e
        in
        AssignExpression (lexp, $2, $3) }
  | expression QUESTION expression COLON expression
      { IfExpression ($1, $3, $5) }
;;

inc_dec_op:
  | PLUSPLUS   { "++" }
  | MINUSMINUS { "--" }

unop:
  | BANG   { "!" }
  | NOT    { "~" }
  | PLUS   { "+" }
  | MINUS  { "-" }
  | DELETE { "delete" }
;;

%inline binop:
  | PLUS                { "+" }
  | MINUS               { "-" }
  | DIV                 { "/" }
  | PERCENT             { "%" }
  | STAR                { "*" }
  | STARSTAR            { "**" }
  | LESSLESS            { "<<" }
  | GREATERGREATER      { ">>" }
  | AMPER               { "&" }
  | XOR                 { "^" }
  | PIPE                { "|" }
  | LESS                { "<" }
  | GREATER             { ">" }
  | LESSEQUAL           { "<=" }
  | GREATEREQUAL        { ">=" }
  | EQUALEQUAL          { "==" }
  | BANGEQUAL           { "!=" }
  | AMPERAMPER          { "&&" }
  | PIPEPIPE            { "||" }
;;

%inline assignop:
  | EQUAL               { "=" }
  | PIPEEQUAL           { "|=" }
  | XOREQUAL            { "^=" }
  | AMPEREQUAL          { "&=" }
  | LESSLESSEQUAL       { "<<=" }
  | GREATERGREATEREQUAL { ">>=" }
  | PLUSEQUAL           { "+=" }
  | MINUSEQUAL          { "-=" }
  | STAREQUAL           { "*=" }
  | DIVEQUAL            { "/=" }
  | PERCENTEQUAL        { "%=" }
;;

literal_expression:
  | BOOLEANLITERAL    { BooleanLiteral ($1) }
  | STRINGLITERAL     { StringLiteral ($1) }
  | HEXSTRINGLITERAL  { HexLiteral ($1) }
  | ADDRESSLITERAL    { AddressLiteral ($1) }
  | NUMBER maybe_unit { NumberLiteral (decimal_to_rational $1, $2, None) }
  | HEXNUMBER         { NumberLiteral ((Z.of_string $1, Z.one),
                                       None, Some (String.length $1)) }

function_call:
  | expression LPAREN function_call_arguments RPAREN { ($1, $3) }
;;

function_call_arguments:
  | expression_list                        { ExpressionList ($1) }
  | LBRACE name_value_nonempty_list RBRACE { NameValueList ($2) }
;;

name_value_nonempty_list:
  | identifier COLON expression       { [($1, $3)] }
  | identifier COLON expression COMMA { [($1, $3)] }
  | identifier COLON expression COMMA name_value_nonempty_list
      { ($1, $3) :: $5 }
;;

(* There is an ambiguity in the following expression:
     (exp)
   It is either:
     - the expression 'exp' between parenthesis
     - a tuple of arity 1
    The original Solidity parser uses a trick to distinguish the two cases:
    if a single expression between parentheses ends with a comma, then
    it is a single-element tuple. We use the same trick here.
    Why didn't they just forbid tuples of arity < 2 ? *)
tuple_expression:
  | LPAREN RPAREN                         { TupleExpression ([]) }
  | LPAREN expression COMMA RPAREN        { TupleExpression ([Some ($2)]) }
  | LPAREN maybe_expression_nontrivial_list RPAREN { TupleExpression ($2) }
;;

maybe_expression_nontrivial_list:
  | expression COMMA %prec below_paren       { [Some ($1); None] }
  | expression COMMA expression              { [Some ($1); Some ($3)] }
  | comma_nonempty_list maybe_expression     { put_in_none_list [$2] $1 }
  | maybe_expression_nontrivial_list COMMA maybe_expression { $1 @ [$3] }
;;

maybe_unit:
  | (* epsilon *) { None }
  | NUMBERUNIT    { Some ($1) }
;;

identifier:
  | IDENTIFIER  { $1 }
  | FROM        { "from" }
  | CONSTRUCTOR { "constructor" }
  | RECEIVE     { "receive" }
  | FALLBACK    { "fallback" }
(*  | ADDRESS     { "address" } *)  (* The official grammar allows these two
(*  | CALLDATA    { "calldata" } *)    keywords to be used as identifiers,
                                       but the actual compiler does not
                                       (it causes complex conflicts) *)
;;

maybe_identifier:
  | (* epsilon *) { None }
  | identifier    { Some ($1) }
;;

%%
