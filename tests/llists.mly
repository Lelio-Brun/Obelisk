%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token EOL

%left PLUS        /* lowest precedence */
%left TIMES       /* higher precedence */

%start <int list> main
%type  <int>      expr

%%

main:
| es = rev(llist(expr)) EOL
    { es }

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }

llist(X):
  /* nothing */
    { [] }
| xs = llist(X) x = X
    { x :: xs }
