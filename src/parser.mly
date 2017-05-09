%{ open Ast %}

%token PUBLIC INLINE PREC
%token LPAR RPAR BAR COLON EQ COMMA
%token OPT PLUS STAR

%token ACTION
%token <string> LID UID

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
  PUBLIC? INLINE? name=LID params=parameters(ident) COLON
  optional_bar groups=separated_nonempty_list(BAR, group)
  { { name; params; groups } }

optional_bar:
  /* epsilon */ %prec no_optional_bar
  | BAR { () }

group:
  prods=separated_nonempty_list(BAR, production) ACTION ioption(precedence)
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
  ioption(terminated(LID, EQ)) actual=actual { actual }

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

%inline modifier:
  | OPT  { Opt }
  | PLUS { Plus }
  | STAR { Star }

%inline precedence:
  PREC p=ident { p }

%inline parameters(X):
  ps=loption(delimited(LPAR, separated_list(COMMA, X), RPAR)) { ps }

%inline ident:
  | id=UID { id }
  | id=LID { id }
