(* This file contains several grammars that serve as benchmarks. *)

(* The grammars can use different lexers, but the lexers must share
   the type [token]. *)

%token A
%token ADD MUL
%token LPAREN RPAREN
%token EOL

%start<unit> arith0
%start<unit> arith
%start<unit> arithr
%start<unit> arithb
%start<unit> gamma5r
%start<unit> gamma5l

%merge { fun _ _ _ -> assert false }

%{
  open Auxiliary
  open Expr
%}

%%

(* -------------------------------------------------------------------------- *)

(* This is a trivial grammar of arithmetic expressions with constants and
   addition. No priority declarations. It is analogous to the grammar EEb in
   McPeak's paper.

   With this grammar, the space and time complexity are both O(n^3).
   McPeak reports that the performance of Elkhound on this grammar is O(n^4). *)

arith0:
  arith0_expr EOL {}

arith0_expr:
| A
    { make @@ ELeaf }
| e1 = arith0_expr ADD e2 = arith0_expr
    { make @@ EBinOp (e1, Add, e2) }
%merge
    { fun e1 e2 -> make @@ EDisj (e1, e2) }

(* -------------------------------------------------------------------------- *)

(* This is a simple grammar of arithmetic expressions, with addition,
   multiplication, and parentheses. No priority declarations. *)

(* With this grammar, the space and time complexity are both O(n^3). In
   practice, the complexity depends on the frequency of parentheses,
   because parentheses break ambiguity. When using randomly generated
   expressions with a 10% chance of generating a pair of parentheses,
   the observed complexity is about O(n^2.5). *)

arith:
  arith_expr EOL {}

arith_expr:
| A
    { make @@ ELeaf }
| LPAREN e = arith_expr RPAREN
    { make @@ EParen e }
| e1 = arith_expr op = binop e2 = arith_expr
    { make @@ EBinOp (e1, op, e2) }
%merge
    { fun e1 e2 -> make @@ EDisj (e1, e2) }

%inline binop:
  ADD { Add }
| MUL { Mul }

(* -------------------------------------------------------------------------- *)

(* This is a simple grammar of arithmetic expressions, with addition,
   multiplication, and parentheses. No priority declarations. The smart
   constructor [smart_binop] uses [reject()] to reject the reductions that do
   not obey the desired priority rules. This does NOT decrease the number of
   reductions that are considered, which remains O(n^3). *)

arithr:
  arithr_expr EOL {}

arithr_expr:
| A
    { make @@ ELeaf }
| LPAREN e = arithr_expr RPAREN
    { make @@ EParen e }
| e1 = arithr_expr op = binop e2 = arithr_expr
    { make @@ smart_binop e1 op e2 }
%merge
    { fun e1 e2 -> make @@ EDisj (e1, e2) }

(* -------------------------------------------------------------------------- *)

(* This is a binarized variant of arithr. The idea is to allow rejections
   to take place earlier, and (perhaps) to simulate the behavior of a
   deterministic LR parser, which discards certain possibilities without
   parsing the second operand of the binary operator.

   The result is better than arithr: both space and time are O(n^2).

   We are still far from matching the performance of a deterministic LR
   parser, though -- even though we have expressed the same disambiguation
   rules.

   Furthermore, the exercise is a bit silly: if one are willing to go this
   way and alter the grammar, one might just as well write the grammar
   under a stratified form that is in the class LR(1). *)

arithb:
  arithb_expr EOL {}

arithb_expr:
| A
    { make @@ ELeaf }
| LPAREN e = arithb_expr RPAREN
    { make @@ EParen e }
| lhs = arithb_lhs e2 = arithb_expr
    { let (e1, op) = lhs in
      make @@ smart_binop e1 op e2 }
%merge
    { fun e1 e2 -> make @@ EDisj (e1, e2) }

arithb_lhs:
  e1 = arithb_expr op = binop
    { smart_lhs e1 op }

(* -------------------------------------------------------------------------- *)

(* This is Γ5, from Scott and Johnstone's RNGLR paper.

     S := rlist(A) A EOL

   With this grammar, the space and time complexity are both O(n^2).
   Indeed, for every pair of indices i and j, the parser recognizes
   that rlist(A) can be recognized in the interval [i, j).

   This grammar is not ambiguous; no %merge function is needed. *)

gamma5r:
  rlist(A) A EOL { tick() }

(* -------------------------------------------------------------------------- *)

(* Γ5l is a variant of Γ5 where the list is left-recursive.

     S := llist(A) A EOL

   With this grammar, the space and time complexity are both O(n).
   Indeed, for every index j, the parser recognizes that llist(A)
   can be recognized in the interval [0, j). There is never a request
   to start recognizing a list at a nonzero index i.

   This grammar is not ambiguous; no %merge function is needed. *)

gamma5l:
  llist(A) A EOL { tick() }

(* -------------------------------------------------------------------------- *)

(* Standard definitions. *)

(* Left-recursive lists. *)

%public llist(X):
  (* epsilon *)
    { tick() }
| llist(X) X
    { tick() }

(* Right-recursive lists. *)

%public rlist(X):
  (* epsilon *)
    { tick() }
| X rlist(X)
    { tick() }
