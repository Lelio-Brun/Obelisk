/* The symbol [infinite] generates the empty language.
   So, the start symbol [dummy] generates a language
   that consists of only one sentence, namely [A]. */

%token A
%start <unit> dummy

%%

dummy:
  A { () }
| A infinite { () }

infinite:
  A infinite
    { () }
