%{
(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro & Origin Labs                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

  open Solidity_common
  open Solidity_ast
  open Solidity_exceptions

  let to_pos pos =
    let open Lexing in
    let ({ pos_lnum = l1; pos_bol = b1; pos_cnum = c1; _ },
         { pos_lnum = l2; pos_bol = b2; pos_cnum = c2; _ }) = pos in
    let c1 = c1 - b1 in
    let c2 = c2 - b2 in
    let l1 = min l1 65535 in
    let l2 = min l2 65535 in
    let c1 = min c1 255 in
    let c2 = min c2 255 in ((l1, c1), (l2, c2))

  let error pos fmt =
    Format.kasprintf (fun s -> raise (SyntaxError (s, to_pos pos))) fmt

  type modifier =
    | Visibility of visibility
    | VMutability of var_mutability
    | FMutability of fun_mutability
    | Virtual
    | Override of longident list
    | Invocation of longident * expression list option

  let add_free_var_modifiers pos var ml =
    let has_mut = ref false in
    List.fold_left (fun var m ->
      match m with
        | VMutability mut ->
            if !has_mut then
              error pos "Mutability already specified";
            has_mut := true;
            { var with var_mutability = mut }
        | _ ->
            error pos "Invalid modifier for free variable declaration"
      ) var ml

  let add_var_modifiers pos var ml =
    let has_vis = ref false in
    let has_mut = ref false in
    let has_over = ref false in
    List.fold_left (fun var m ->
      match m with
        | Visibility vis ->
            if !has_vis then
              error pos "Visibility already specified";
            if is_external vis then
              error pos "Variable visibility can't be external";
            has_vis := true;
            { var with var_visibility = vis }
        | VMutability mut ->
            if !has_mut then
              error pos "Mutability already specified";
            has_mut := true;
            { var with var_mutability = mut }
        | Override ol ->
            if !has_over then
              error pos "Override already specified";
            has_over := true;
            { var with var_override = Some ol }
        | _ ->
            error pos "Invalid modifier for variable declaration"
      ) var ml

  let add_fun_modifiers pos fct ml =
    let has_vis = ref false in
    let has_mut = ref false in
    let has_over = ref false in
    let fct = List.fold_left (fun fct m ->
      match m with
        | Visibility vis ->
            if !has_vis then
              error pos "Visibility already specified";
            has_vis := true;
            { fct with fun_visibility = vis }
        | FMutability mut ->
            if !has_mut then
              error pos "Mutability already specified";
            has_mut := true;
            { fct with fun_mutability = mut }
        | Override ol ->
            if !has_over then
              error pos "Override already specified";
            has_over := true;
            { fct with fun_override = Some ol }
        | Virtual ->
            if fct.fun_virtual then
              error pos "Virtual already specified";
            { fct with fun_virtual = true }
        | Invocation (lid, exp_list_opt) ->
            { fct with fun_modifiers =
                         (lid, exp_list_opt) :: fct.fun_modifiers }
        | _ ->
            error pos "Invalid modifier for function declaration"
      ) fct ml
    in
    { fct with fun_modifiers = List.rev fct.fun_modifiers }

  let add_fun_type_modifiers pos ft ml =
    let has_vis = ref false in
    let has_mut = ref false in
    List.fold_left (fun ft m ->
      match m with
        | Visibility vis ->
            if !has_vis then
              error pos "Visibility already specified";
            if not (is_external vis || is_internal vis) then
              error pos "Function type visibility must be internal or external";
            has_vis := true;
            { ft with fun_type_visibility = vis }
        | FMutability mut ->
            if !has_mut then
              error pos "Mutability already specified";
            has_mut := true;
            { ft with fun_type_mutability = mut }
        | _ ->
            error pos "Invalid modifier for function type"
      ) ft ml

  let add_mod_modifiers pos md ml =
    List.fold_left (fun md m ->
      match m with
        | Override ol ->
            begin match md.mod_override with
              | None -> { md with mod_override = Some ol }
              | Some _ -> error pos "Override already specified"
            end
        | Virtual ->
            begin match md.mod_virtual with
              | false -> { md with mod_virtual = true }
              | true -> error pos "Virtual already specified"
            end
        | _ ->
            error pos "Invalid modifier for function declaration"
      ) md ml

  let mk pos c =
    { contents = c; annot = ANone; pos = to_pos pos }

  type raw_ambiguous_type_name_or_expression =
    | AmbiguousIdentifier of ident list
    | AmbiguousArray of ambiguous_type_name_or_expression * expression option

  and ambiguous_type_name_or_expression =
    raw_ambiguous_type_name_or_expression node

  let rec expression_of_identifiers = function
    | [] ->
        assert false
    | [x] ->
        { contents = IdentifierExpression x; annot = ANone; pos = x.pos }
    | x :: y ->
        let e = expression_of_identifiers y in
	let pos = match x.pos, e.pos with
          | ((l1, c1), _), (_, (l2, c2)) -> ((l1, c1), (l2, c2))
        in
        { contents = FieldExpression (e, x); annot = ANone; pos }

  let rec expression_of_ambiguity a =
    match a.contents with
    | AmbiguousIdentifier l ->
        expression_of_identifiers (List.rev l)
    | AmbiguousArray (a, expo) ->
        { a with contents = ArrayAccess (expression_of_ambiguity a, expo) }

  let rec type_name_of_ambiguity a =
    match a.contents with
    | AmbiguousIdentifier l ->
        let contents =
          LongIdent.of_ident_list_rel (List.map (fun id -> id.contents) l) in
        UserDefinedType { a with contents }
    | AmbiguousArray (a, expo) ->
        Array (type_name_of_ambiguity a, expo)

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
    Q.mul (Q.add i d) e

  let is_placeholder { contents; _ } =
    String.equal (Ident.to_string contents) "_"

  let ctxt_modifier = ref false
  let ctxt_interface = ref false
%}

(* Keywords *)
%token <Solidity_common.Ident.t * string> PRAGMA
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
%token BYTE
%token VAR
%token RETURN
%token CONTINUE
%token BREAK
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
%token <string> ADDRESSLITERAL
%token <string> HEXNUMBER
%token <string> STRINGLITERAL
%token <string> HEXSTRINGLITERAL
%token <Z.t option * Z.t option * int option> NUMBER
%token <Solidity_ast.number_unit> NUMBERUNIT
%token <Solidity_common.Ident.t> IDENTIFIER
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

%nonassoc LBRACE

%nonassoc below_LBRACKET
%nonassoc LBRACKET (* RBRACKET *)

%nonassoc below_DOT
%left DOT

(* Entry points *)
%type <Solidity_ast.module_units> module_units
%start module_units

%%

module_units:
  | midrule({ ctxt_modifier := false; ctxt_interface := false })
      source_unit* EOF { $2 }
;;

source_unit:
  | PRAGMA                       { mk $loc (Pragma ($1)) }
  | IMPORT import_directive SEMI { mk $loc (Import ($2)) }
  | contract_definition          { mk $loc (ContractDefinition ($1)) }
  | type_definition              { mk $loc (GlobalTypeDefinition ($1)) }
  | type_name_no_function variable_modifiers identifier
        equal_expression? SEMI
      { mk $loc (GlobalVariableDefinition (add_free_var_modifiers $loc {
            var_name = $3;
            var_type = $1;
            var_visibility = VInternal;
            var_mutability = MMutable;
            var_override = None;
            var_init = $4; } $2)) }
  | function_descriptor parameters function_modifier*
        returns_opt function_body_opt
      { mk $loc (GlobalFunctionDefinition (add_fun_modifiers $loc {
            fun_name = $1;
            fun_params = $2;
            fun_returns = $4;
            fun_modifiers = [];
            fun_visibility = VInternal;
            fun_mutability = MNonPayable;
            fun_override = None;
            fun_virtual = false;
            fun_body = $5; } $3)) }
;;

import_directive:
  | STRINGLITERAL as_identifier?
      { import $1 (ImportAll ($2)) }
  | STAR as_identifier FROM STRINGLITERAL
      { import $4 (ImportAll (Some $2)) }
  | LBRACE import_declarations RBRACE FROM STRINGLITERAL
      { import $5 (ImportIdents $2) }
;;

import_declarations:
  | separated_nonempty_list(COMMA, pair(identifier, as_identifier?)) { $1 }
;;

as_identifier:
  | AS identifier { $2 }
;;

contract_definition:
  | boption(ABSTRACT) contract_kind identifier
        loption(is_inheritance_specifiers) LBRACE contract_part* RBRACE
      { ctxt_interface := false;
        { contract_name = $3;
          contract_kind = $2;
          contract_abstract = $1;
          contract_inheritance = $4;
          contract_parts = $6; } }
;;

contract_kind:
  | CONTRACT  { Contract }
  | LIBRARY   { Library }
  | INTERFACE { ctxt_interface := true; Interface }
;;

is_inheritance_specifiers:
  | IS separated_nonempty_list(COMMA, inheritance_specifier) { $2 }
;;

inheritance_specifier:
  | long_ident loption(delimited(LPAREN, expression_list, RPAREN)) { ($1, $2) }
;;

contract_part:
  | type_definition
      { mk $loc (TypeDefinition ($1)) }
  | type_name_no_function variable_modifiers identifier
        equal_expression? SEMI
      { mk $loc (StateVariableDeclaration (add_var_modifiers $loc {
            var_name = $3;
            var_type = $1;
            var_visibility = VInternal;
            var_mutability = MMutable;
            var_override = None;
            var_init = $4; } $2)) }
  | ifunction_type_name ivariable_modifiers iidentifier
        ioption(equal_expression) SEMI
      { mk $loc (StateVariableDeclaration (add_var_modifiers $loc {
            var_name = $3;
            var_type = $1;
            var_visibility = VInternal;
            var_mutability = MMutable;
            var_override = None;
            var_init = $4; } $2)) }
  | midrule(MODIFIER { ctxt_modifier := true }) identifier
        loption(parameters) modifier_modifier* function_body_opt
      { ctxt_modifier := false;
        mk $loc (ModifierDefinition (add_mod_modifiers $loc {
            mod_name = $2;
            mod_params = $3;
            mod_override = None;
            mod_virtual = false;
            mod_body = $5; } $4)) }
  | FUNCTION parameters ianonymous_function_modifiers
        returns_opt function_body_opt
      { mk $loc (FunctionDefinition (add_fun_modifiers $loc {
            fun_name = mk $loc($1) (Ident.fallback);
            fun_params = $2;
            fun_returns = $4;
            fun_modifiers = [];
            fun_visibility = if !ctxt_interface then VExternal else VPublic;
            fun_mutability = MNonPayable;
            fun_override = None;
            fun_virtual = false;
            fun_body = $5; } $3)) }
  | function_descriptor parameters function_modifier*
        returns_opt function_body_opt
      { mk $loc (FunctionDefinition (add_fun_modifiers $loc {
            fun_name = $1;
            fun_params = $2;
            fun_returns = $4;
            fun_modifiers = [];
            fun_visibility = if !ctxt_interface then VExternal else VPublic;
            fun_mutability = MNonPayable;
            fun_override = None;
            fun_virtual = false;
            fun_body = $5; } $3)) }
  | EVENT identifier event_parameters boption(ANONYMOUS) SEMI
      { mk $loc (EventDefinition {
            event_name = $2;
            event_params = $3;
            event_anonymous = $4; }) }
  | USING long_ident FOR type_name_or_star SEMI
      { mk $loc (UsingForDeclaration ($2, None)) }
;;

type_name_or_star:
  | type_name { Some ($1) }
  | STAR      { None }
;;

type_definition:
  | ENUM identifier LBRACE enum_values RBRACE     { EnumDefinition ($2, $4) }
  | STRUCT identifier LBRACE struct_fields RBRACE { StructDefinition ($2, $4) }
;;

enum_values:
  | separated_list(COMMA, identifier) { $1 }
;;

struct_fields:
  | (* epsilon *)      { [] }
  | separated_and_terminated_nonempty_list(SEMI, pair(type_name, identifier))
      { $1 }
;;

function_descriptor:
  | FUNCTION identifier { $2 }
  | CONSTRUCTOR         { mk $loc Ident.constructor }
  | RECEIVE             { mk $loc Ident.receive }
  | FALLBACK            { mk $loc Ident.fallback }
;;

%inline ireturns_opt:
  | (* epsilon *)                                                { [] }
  | RETURNS LPAREN separated_nonempty_list(COMMA, return) RPAREN { $3 }
;;

returns_opt:
  | ireturns_opt { $1 }
;;

return:
  | type_name storage_location? identifier? { ($1, $2, $3) }

function_body_opt:
  | block { Some ($1) }
  | SEMI  { None }
;;

function_modifier:
  | modifier_invocation { $1 }
  | fun_mutability      { $1 }
  | internal_external   { $1 }
  | public_private      { $1 }
  | VIRTUAL             { Virtual }
  | override_specifier  { $1 }
;;

modifier_modifier:
  | VIRTUAL             { Virtual }
  | override_specifier  { $1 }
;;

(* Parsing of anonymous functions has some restrictions
   on the arrangement of modifiers that can be parsed.
   This is a parse-level restriction that also exists in the original parser.
   Basically, the following arrangements are allowed:
     - fun_mutability*
     - fun_mutability* (public|private|mod_invoc)
                                 extra_anonymous_function_modifier*
     - fun_mutability* (internal|external) fun_mutability*
     - fun_mutability* (internal|external) fun_mutability*
                                 (mod_invoc) extra_anonymous_function_modifier*
   These arrangements can be expressed using the function_type_modifiers
   rule defined later, extended with modifier_invocation *)
%inline ianonymous_function_modifiers:
  | function_type_modifiers { $1 }
  | function_type_modifiers imodifier_invocation
        extra_anonymous_function_modifier*
      { $1 @ ($2 :: $3) }
;;

extra_anonymous_function_modifier:
  | extra_function_type_modifier { $1 }
  | imodifier_invocation         { $1 }
;;

%inline ivariable_modifiers:
  | (* epsilon *)      { [] }
  | variable_modifier+ { $1 }
;;

variable_modifiers:
  | ivariable_modifiers { $1 }
;;

variable_modifier:
  | public_private     { $1 }
  | INTERNAL           { Visibility VInternal }
  | CONSTANT           { VMutability MConstant }
  | IMMUTABLE          { VMutability MImmutable }
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
%inline imodifier_invocation:
  | ilong_ident /*%prec below_SEMI*/         { Invocation ($1, None) }
  | long_ident LPAREN expression_list RPAREN { Invocation ($1, Some ($3)) }
;;

modifier_invocation:
  | imodifier_invocation { $1 }
;;

override_specifier:
  | OVERRIDE loption(override_parameters) { Override ($2) }
;;

override_parameters:
  | LPAREN separated_list(COMMA, long_ident) RPAREN { $2 }
;;

event_parameters:
  | LPAREN separated_list(COMMA, event_parameter) RPAREN { $2 }
;;

event_parameter:
  | type_name boption(INDEXED) identifier? { ($1, $2, $3) }
;;

parameters:
  | LPAREN separated_list(COMMA, parameter) RPAREN { $2 }
;;

parameter:
  | type_name storage_location? identifier? { ($1, $2, $3) }
;;

variable_declaration:
  | type_name storage_location? identifier { ($1, $2, $3) }
;;

type_name:
  | non_ambiguous_type_name %prec below_LBRACKET
      { $1 }
  | ambiguous_type_name_or_expression %prec below_LBRACKET
      { type_name_of_ambiguity $1 }

type_name_no_function:
  | non_ambiguous_type_name_no_function { $1 }
  | ambiguous_type_name_or_expression   { type_name_of_ambiguity $1 }
;;

non_ambiguous_type_name:
  | non_ambiguous_type_name_no_function     { $1 }
  | ifunction_type_name %prec below_RETURNS { $1 }
;;

non_ambiguous_type_name_no_function:
  | elementary_type_name
      { ElementaryType ($1) }
  | MAPPING LPAREN type_name EQUALGREATER type_name RPAREN
      { Mapping ($3, $5) }
  | non_ambiguous_type_name LBRACKET expression? RBRACKET
      { Array ($1, $3) }
;;

ambiguous_type_name_or_expression:
  | ident_list
      { mk $loc (AmbiguousIdentifier ($1)) }
  | ambiguous_type_name_or_expression LBRACKET expression? RBRACKET
      { mk $loc (AmbiguousArray ($1, $3)) }
;;

%inline ilong_ident:
  | long_ident_raw { mk $loc $1 }
;;

long_ident:
  | ilong_ident     { $1 }
;;

long_ident_raw:
  | identifier_raw %prec below_SEMI   { LongIdent.of_ident_rel $1 }
  | identifier_raw DOT long_ident_raw { LongIdent.prepend $1 $3 }
;;

ident_list:
  | identifier %prec below_DOT { [$1] }
  | identifier DOT ident_list  { $1 :: $3 }
;;

%inline ifunction_type_name:
  | FUNCTION parameters function_type_modifiers ireturns_opt
     { let (fun_params, fun_modifiers, fun_returns) = ($2, $3, $4) in
       let fun_returns =
         List.map (function
             | _, _, Some _ ->
                 error $loc "Return parameters in function \
                             types may not be named."
             | t, loc_opt, None -> t, loc_opt
           ) fun_returns
       in
       FunctionType (add_fun_type_modifiers $loc {
         fun_type_params = fun_params;
         fun_type_visibility = VInternal;
         fun_type_mutability = MNonPayable;
         fun_type_returns = fun_returns; } fun_modifiers) }
;;

(* When parsing a function type (as opposed to a function definition),
   only certain arrangements of modifiers are allowed, so as to
   avoid ambiguities when used in a variable declaration.
   This is a parse-level restriction that also exists in the original parser.
   These arrangements are as follows:
     - fun_mutability*
     - fun_mutability* (internal|external) fun_mutability*
     - fun_mutability* (public|private) extra_function_type_modifier* *)
function_type_modifiers:
  | fun_mutability_list %prec below_visibility { $1 }
  | fun_mutability_list internal_external fun_mutability_list
      { $1 @ ($2 :: $3) }
  | fun_mutability_list public_private extra_function_type_modifiers
      { $1 @ ($2 :: $3) }
;;

fun_mutability_list:
  | (* epsilon *) %prec below_mutability { [] }
  | fun_mutability fun_mutability_list   { $1 :: $2 }
;;

extra_function_type_modifiers:
  | (* epsilon *) %prec below_visibility                       { [] }
  | extra_function_type_modifier extra_function_type_modifiers { $1 :: $2 }
;;

extra_function_type_modifier:
  | internal_external { $1 }
  | public_private    { $1 }
  | fun_mutability    { $1 }
;;

internal_external:
  | EXTERNAL { Visibility VExternal }
  | INTERNAL { Visibility VInternal }
;;

public_private:
  | PUBLIC   { Visibility VPublic }
  | PRIVATE  { Visibility VPrivate }
;;

storage_location:
  | MEMORY   { Memory }
  | STORAGE  { Storage }
  | CALLDATA { Calldata }
;;

fun_mutability:
  | PURE      { FMutability MPure }
  | CONSTANT  { FMutability MView }
  | VIEW      { FMutability MView }
  | PAYABLE   { FMutability MPayable }
;;

block:
  | LBRACE statement* RBRACE { $2 }
;;

statement:
  | statement_no_semi          { $1 }
  | statement_before_semi SEMI { $1 }
;;

statement_no_semi:
  | if_statement    { mk $loc ($1) }
  | for_statement   { mk $loc ($1) }
  | while_statement { mk $loc ($1) }
  | try_statement   { mk $loc ($1) }
  | block           { mk $loc (Block ($1)) }
;;

statement_before_semi:
  | simple_statement   { $1 }
  | do_while_statement { mk $loc ($1) }
  | RETURN expression? { mk $loc (Return ($2)) }
  | CONTINUE           { mk $loc (Continue) }
  | BREAK              { mk $loc (Break) }
  | EMIT function_call { let (f, a) = $2 in mk $loc (Emit (f, a)) }
;;

if_statement:
  | IF LPAREN expression RPAREN statement %prec below_ELSE
      { IfStatement ($3, $5, None) }
  | IF LPAREN expression RPAREN statement ELSE statement
      { IfStatement ($3, $5, Some ($7)) }
;;

for_statement:
  | FOR LPAREN simple_statement? SEMI expression?
        SEMI expression? RPAREN statement
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
  | TRY expression ireturns_opt block catch_clause+
      { TryStatement ($2, $3, $4, $5) }
;;

catch_clause:
  | CATCH block                        { (None, [], $2) }
  | CATCH identifier? parameters block { ($2, $3, $4) }
;;

simple_statement:
  | variable_declaration_statement { mk $loc (VariableDefinition ($1)) }
  | expression {
      (* When under a modifier, a single underscore at the
         level of statements is no longer an identifier *)
      match !ctxt_modifier, $1.contents with
      | true, IdentifierExpression (id_node) when is_placeholder id_node ->
          mk $loc (PlaceholderStatement)
      | _, _e ->
          mk $loc (ExpressionStatement ($1)) }
;;

do_while_statement:
  | DO statement WHILE LPAREN expression RPAREN { DoWhileStatement ($2, $5) }
;;

variable_declaration_statement:
  | VAR identifier_opt_list
      { error $loc "Assignment necessary for type detection" }
  | VAR identifier_opt_list equal_expression        { VarInfer ($2, $3) }
  | variable_declaration_opt_list equal_expression? { VarType ($1, $2) }
;;

identifier_opt_list:
  | identifier { [Some ($1)] }
  | LPAREN separated_nonempty_list(COMMA, identifier?) RPAREN
      { match $2 with [None] -> [] | l -> l }
;;

variable_declaration_opt_list:
  | variable_declaration                                 { [Some ($1)] }
  | LPAREN variable_declaration_opt_nonempty_list RPAREN { $2 }
;;

variable_declaration_opt_nonempty_list:
  | variable_declaration                     { [Some ($1)] }
  | comma_nonempty_list variable_declaration { put_in_none_list [Some ($2)] $1 }
  | variable_declaration_opt_nonempty_list COMMA variable_declaration?
      { $1 @ [$3] }
;;

comma_nonempty_list:
  | COMMA                     { 1 }
  | COMMA comma_nonempty_list { 1 + $2 }
;;

equal_expression:
  | EQUAL expression { $2 }
;;

elementary_type_name:
  | VAR %prec below_IDENTIFIER { error $loc "Expected explicit type name" }
  | ADDRESS boption(PAYABLE)   { TypeAddress ($2) }
  | STRING                     { TypeString }
  | BYTES                      { TypeBytes ($1) }
  | BYTE                       { TypeBytes (Some 1) }
  | BOOL                       { TypeBool }
  | INT                        { TypeInt (Option.value ~default:256 $1) }
  | UINT                       { TypeUint (Option.value ~default:256 $1) }
  | FIXED
      { let (sz, dec) = Option.value ~default:(128,18) $1 in
        TypeFixed (sz, dec) }
  | UFIXED
      { let (sz, dec) = Option.value ~default:(128,18) $1 in
        TypeUfixed (sz, dec) }
;;

expression:
  | non_ambiguous_expression
      { mk $loc $1 }
  | ambiguous_type_name_or_expression %prec below_LBRACKET
      { expression_of_ambiguity $1 }
;;

expression_list:
  | separated_list(COMMA, expression) { $1 }
;;

expression_nonempty_list:
  | separated_nonempty_list(COMMA, expression) { $1 }
;;

non_ambiguous_expression:
  | LPAREN expression RPAREN
      { $2.contents }
  | literal_expression
      { $1 }
  | tuple_expression
      { $1 }
  | NEW type_name
      { NewExpression ($2) }
  | non_ambiguous_type_name %prec below_LBRACKET
      { TypeExpression $1 }
  | expression DOT identifier
      { FieldExpression ($1, $3) }
  | expression LBRACKET expression? RBRACKET
      { ArrayAccess ($1, $3) }
  | expression LBRACKET expression? COLON expression? RBRACKET
      { ArraySlice ($1, $3, $5) }
  | LBRACKET expression_nonempty_list RBRACKET
      { ImmediateArray ($2) }
  | function_call
      { let (f, a) = $1 in FunctionCallExpression (f, a) }
  | expression LBRACE name_value_nonempty_list RBRACE
      { CallOptions ($1, $3) }
  | PAYABLE LPAREN expression RPAREN
      { FunctionCallExpression (
            mk $loc($1) (TypeExpression (ElementaryType (TypeAddress (true)))),
            ExpressionList ([$3])) }
  | inc_dec_op expression %prec unary_op
      { PrefixExpression ($1, $2) }
  | expression inc_dec_op
      { SuffixExpression ($1, $2) }
  | unop expression %prec unary_op
      { PrefixExpression ($1, $2) }
  | expression ibinop expression
      { BinaryExpression ($1, $2, $3) }
  | expression icmpop expression
      { CompareExpression ($1, $2, $3) }
  | expression EQUAL expression
      { (* When used on the left side of an assignment,
           (exp,) is understood as a 2-element tuple,
           so we undo the 1-element tuple trick *)
        let lexp = match $1.contents with
          | TupleExpression [Some (e)] ->
              { $1 with contents = TupleExpression [Some (e); None] }
          | _e ->
              $1
        in
        AssignExpression (lexp, $3) }
  | expression iassignop expression
      { (* When used on the left side of an assignment,
           (exp,) is understood as a 2-element tuple,
           so we undo the 1-element tuple trick *)
        let lexp = match $1.contents with
          | TupleExpression [Some (e)] ->
              { $1 with contents = TupleExpression [Some (e); None] }
          | _e ->
              $1
        in
        AssignBinaryExpression (lexp, $2, $3) }
  | expression QUESTION expression COLON expression
      { IfExpression ($1, $3, $5) }
;;

inc_dec_op:
  | PLUSPLUS   { UInc }
  | MINUSMINUS { UDec }

unop:
  | BANG   { UNot }
  | NOT    { ULNot }
  | PLUS   { UPlus }
  | MINUS  { UMinus }
  | DELETE { UDelete }
;;

%inline icmpop:
  | EQUALEQUAL          { CEq }
  | BANGEQUAL           { CNeq }
  | LESS                { CLt }
  | GREATER             { CGt }
  | LESSEQUAL           { CLeq }
  | GREATEREQUAL        { CGeq }

%inline ibinop:
  | PLUS                { BPlus }
  | MINUS               { BMinus }
  | DIV                 { BDiv }
  | PERCENT             { BMod }
  | STAR                { BTimes }
  | STARSTAR            { BExp }
  | LESSLESS            { BLShift }
  | GREATERGREATER      { BRShift }
  | AMPER               { BAnd }
  | XOR                 { BXor }
  | PIPE                { BOr }
  | AMPERAMPER          { BLAnd }
  | PIPEPIPE            { BLOr }
;;

%inline iassignop:
  | PIPEEQUAL           { BOr }
  | XOREQUAL            { BXor }
  | AMPEREQUAL          { BAnd }
  | LESSLESSEQUAL       { BLShift }
  | GREATERGREATEREQUAL { BRShift }
  | PLUSEQUAL           { BPlus }
  | MINUSEQUAL          { BMinus }
  | STAREQUAL           { BTimes }
  | DIVEQUAL            { BDiv }
  | PERCENTEQUAL        { BMod }
;;

literal_expression:
  | BOOLEANLITERAL     { BooleanLiteral ($1) }
  | ADDRESSLITERAL     { AddressLiteral ($1) }
  | STRINGLITERAL      { StringLiteral ($1) }
  | HEXSTRINGLITERAL   { StringLiteral ($1) }
  | NUMBER             { NumberLiteral (decimal_to_rational $1, Unit, None) }
  | NUMBER NUMBERUNIT  { NumberLiteral (decimal_to_rational $1, $2, None) }
  | HEXNUMBER          { NumberLiteral (Q.make (Z.of_string ("0x" ^ $1)) Z.one,
                                        Unit, Some (String.length $1)) }

function_call:
  | expression LPAREN function_call_arguments RPAREN { ($1, $3) }
;;

function_call_arguments:
  | expression_list                        { ExpressionList ($1) }
  | LBRACE name_value_nonempty_list RBRACE { NameValueList ($2) }
;;

name_value_nonempty_list:
  | separated_or_terminated_nonempty_list(COMMA, name_value) { $1 }

name_value:
  | identifier COLON expression { ($1, $3) }

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
  | LPAREN RPAREN                       { TupleExpression ([]) }
  | LPAREN expression COMMA RPAREN      { TupleExpression ([Some ($2)]) }
  | LPAREN expression_opt_nontrivial_list RPAREN { TupleExpression ($2) }
;;

expression_opt_nontrivial_list:
  | expression COMMA %prec below_paren { [Some ($1); None] }
  | expression COMMA expression        { [Some ($1); Some ($3)] }
  | comma_nonempty_list expression?    { put_in_none_list [$2] $1 }
  | expression_opt_nontrivial_list COMMA expression? { $1 @ [$3] }
;;

%inline iidentifier:
  | identifier_raw { mk $loc $1 }

identifier:
  | iidentifier    { $1 }

identifier_raw:
  | IDENTIFIER  { $1 }
  | FROM        { Ident.of_string "from" }
  | CONSTRUCTOR { Ident.of_string "constructor" }
  | RECEIVE     { Ident.of_string "receive" }
  | FALLBACK    { Ident.of_string "fallback" }
(*  | ADDRESS     { "address" } *)  (* The official grammar allows these two
(*  | CALLDATA    { "calldata" } *)    keywords to be used as identifiers,
                                       but the actual compiler does not
                                       (it causes complex conflicts) *)
;;

separated_or_terminated_nonempty_list(delimiter, X):
  | x = X
      { [x] }
  | x = X delimiter
      { [x] }
  | x = X delimiter xs = separated_or_terminated_nonempty_list(delimiter, X)
      { x :: xs }
;;

separated_and_terminated_nonempty_list(delimiter, X):
  | x = X delimiter
      { [x] }
  | x = X delimiter xs = separated_and_terminated_nonempty_list(delimiter, X)
      { x :: xs }
;;

%%
