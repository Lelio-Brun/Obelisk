%{ open Ast %}

%token PUBLIC INLINE PREC LET
%token LPAR RPAR BAR COLON EQ COLONEQ EQEQ SEMICOLON COMMA
%token OPT PLUS STAR
%token TILDE UNDERSCORE

%token ACTION POINTFREEACTION ATTRIBUTE
%token <string> LID UID QID

%token EOF

%start <Ast.spec> specification

/* These declarations solve a shift-reduce conflict in favor of
   shifting: when the declaration of a non-terminal symbol begins with
   a leading bar, it is understood as an (insignificant) leading
   optional bar, *not* as an empty right-hand side followed by a bar.
   This ambiguity arises due to the existence of a new notation for
   letting several productions share a single semantic action. */
/* cf fancy-parser.mly of Menhir */
%nonassoc no_optional_bar
%nonassoc BAR

%%

specification:
  rules=rule* EOF { rules }

rule:
  | r=old_rule { r }
  | r=new_rule { r }

old_rule:
  flags? name=ident ATTRIBUTE* params=parameters(ident) COLON
  optional_bar groups=separated_nonempty_list(BAR, group) SEMICOLON*
  { { name; params; groups } }

flags:
  | PUBLIC        { () }
  | INLINE        { () }
  | PUBLIC INLINE { () }
  | INLINE PUBLIC { () }

optional_bar:
  /* epsilon */ %prec no_optional_bar
  | BAR { () }

group:
  prods=separated_nonempty_list(BAR, production) ACTION ioption(precedence)
  ATTRIBUTE*
  { prods }

production:
  producers=producer* ioption(precedence) { producers }

/* ------------------------------------------------------------------------- */
/* A producer is an actual parameter, possibly preceded by a
   binding, and possibly followed with attributes.
   Because both [ioption] and [terminated] are defined as inlined by
   the standard library, this definition expands to two productions,
   one of which begins with id = LID, the other of which begins with
   p = actual. The token LID is in FIRST(actual),
   but the LR(1) formalism can deal with that. If [option] was used
   instead of [ioption], an LR(1) conflict would arise -- looking
   ahead at LID would not allow determining whether to reduce an
   empty [option] or to shift. */
/* cf fancy-parser.mly of Menhir */

producer:
  ioption(terminated(LID, EQ)) actual=actual ATTRIBUTE* SEMICOLON* { actual }

/* ------------------------------------------------------------------------- */
/* The ideal syntax of actual parameters includes:
   1. a symbol, optionally applied to a list of actual parameters;
   2. an actual parameter followed with a modifier;
   3. an anonymous rule. (Not delimited by parentheses! Otherwise
      one would often end up writing two pairs of parentheses.) */
/* In order to avoid a few ambiguities, we restrict this ideal syntax as
   follows:
   a. Within a %type declaration, we use [strict_actual], which
      allows 1- and 2- (this is undocumented; the documentation says we
      require a symbol) but not 3-, which would not make semantic sense
      anyway.
   b. Within a producer, we use [actual], which allows 1- and
      2- but not 3-. Case 3- is allowed by switching to [lax_actual]
      within the actual arguments of an application, which are clearly
      delimited by parentheses and commas.
   c. In front of a modifier, we can never allow [lax_actual],
      as this would create an ambiguity: basically, [A | B?] could be
      interpreted either as [(A | B)?] or as [A | (B?)].
*/
/* cf fancy-parser.mly of Menhir */

%inline generic_actual(A, B):
  (* 1- *)
  | id=ident ps=parameters(A) { Symbol (id, ps) }
  (* 2- *)
  | a=B m=modifier            { Modifier (a, m) }

actual:
  a=generic_actual(lax_actual, actual) { a }

lax_actual:
  | a=generic_actual(lax_actual, /* cannot be lax_ */ actual) { a }
  (* 3- *)
  | gs=separated_nonempty_list(BAR, group)                    { Anonymous gs }


let new_rule :=
  PUBLIC?; LET; name=LID; ATTRIBUTE*; params=parameters(ident);
  binder; es=expression;
  { { name; params; groups = [es] } }

let binder == COLONEQ | EQEQ

let expression := optional_bar; ~=separated_nonempty_list(BAR, seq_expression); < >

let seq_expression :=
  | ioption(terminated(pattern, EQ)); ~=symbol_expression; SEMICOLON; ~=seq_expression; < (::) >
  | e=symbol_expression;                                                                { [e] }
  | action_expression;                                                                  { [] }

let symbol_expression :=
  | id=ident; ps=parameters(expression); { Symbol (id, List.map (fun g -> Anonymous [g]) ps) }
  | ~=symbol_expression; ~=modifier;     < Modifier >

let action_expression :=
  | action
  | action; precedence
  | precedence; action

let action := ACTION | POINTFREEACTION

let pattern :=
  | LID;                                                   { () }
  | UNDERSCORE;                                            { () }
  | TILDE;                                                 { () }
  | delimited(LPAR, separated_list(COMMA, pattern), RPAR); { () }

%inline modifier:
  | OPT  { Opt }
  | PLUS { Plus }
  | STAR { Star }

%inline precedence:
  PREC ident { () }

%inline parameters(X):
  ps=loption(delimited(LPAR, separated_list(COMMA, X), RPAR)) { ps }

%inline ident:
  | id=UID { id }
  | id=LID { id }
  | id=QID { id }
