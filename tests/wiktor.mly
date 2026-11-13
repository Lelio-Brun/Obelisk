(*
Prior to 2025/09/12, compiling this grammar with the new code back-end causes
the following internal error:

StackLangCheck: internal error.
Failed stack subtype check (jump) : 0 known cells; 1 expected cells.
Actual shape:
Expected shape: BACKSLASH
 *)

%token ATOM
%token BACKSLASH "\\"
%token BAR "|"

%nonassoc BAR

%type <unit> bar_list expr
%type <unit list> loption(separated_nonempty_list(BAR,ATOM)) separated_nonempty_list(BAR,ATOM)
%start expr
%%

let expr :=
  | "\\"; bar_list; {}
  | "\\"; separated_list("|", ATOM); {}

let bar_list :=
  | expr; {} %prec BAR
  | expr; "|"; bar_list; {}
