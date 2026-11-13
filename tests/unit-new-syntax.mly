%token A EOF
%start<AST.bar> bar

(* For this test to type-check, one would need the module AST to define
   type foo = FOO of unit
   type bar = BAR of foo
   Note that [type foo = FOO] would not work.
 *)

%%
let foo := A; <FOO>

let bar := foo; EOF; <BAR>
