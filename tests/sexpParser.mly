(* https://github.com/sufrin/InteractiveSedlexMenhirExample/blob/main/sexpParser.mly *)
(*
        Parser for not-quite-S-expressions

        The not-quiteness is in the treatment of DOT, which has
        been more-or-less arbitrarily chosen to retrieve intelligible
        structure in the presence of dots at arbitrary places within
        lists.  The usual equivalence of e.g. (a b c d) and
        (a.b.c.d.()) no longer holds.
*)

%token          EOF
%token          AT
%token          BRA KET
%token          PLUS
%token          MINUS
%token          DOT
%token<string>  ID
%token<string>  STRING
%token<int>     INT

%{
  (* exception ParseError of token * Lexing.position * Lexing.position *)

  let make_loc pos = pos

  open Sexp

  let locate pos sexp =
      let location = make_loc pos in At (location, sexp)

  (*  (* the interpretation where (c c c c c . d) means (c c c c (c . d)) *)
      let mkPair (cars, cdr) =
          match List.rev cars with
          | [car]          -> Pair(car, cdr)
          | last::revfront -> List (List.rev(Pair(last, cdr) :: revfront))
          | _              -> assert false
  *)

  let mkList = function [s] -> s | ss -> List ss

  (* the interpretation where (c c c c c . d) means ((c c c c c) . d)) *)
  let mkPair (cars, cdr) = Pair(mkList cars, cdr)



%}

%start <Sexp.sexp> sexp

%%

let sexp :=
  | ~=sexpr; { sexpr }
  | EOF;     { EndFile }

(*
let sexprs :=
  | { [] }
  | first=sexpr; rest=sexprs; { first::rest }
*)


let sexprs == sexpr*

let number ==
  | PLUS?; i = INT;     { i   }
  | MINUS; i = INT;     { - i }

(* Use the new menhir notation, just for fun *)

let sexpr :=
  | ~ = STRING;                  <String>
  | ~ = number;                  <Int>
  | ~ = ID;                      <Id>
  | BRA; ~=sexprs; KET;          <List>
  | BRA; car=sexprs; DOT; cdr=pairs; KET;  <mkPair>
  | AT; s=sexpr;                 {locate $loc s}

let pairs :=
  | ~=sexpr;                    <>
  | car=sexprs; DOT; cdr=pairs;  <mkPair>
