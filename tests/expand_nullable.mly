%token DECLARE A B EOF
%start<(unit option * unit option) list> main
%%

main:
  declarations EOF { $1 }

declarations:
  declaration*     { $1 }

declaration:
  DECLARE A? B?    { $2, $3 }
