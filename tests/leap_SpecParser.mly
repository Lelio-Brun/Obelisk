
%{

(***********************************************************************)
(*                                                                     *)
(*                                 LEAP                                *)
(*                                                                     *)
(*               Alejandro Sanchez, IMDEA Software Institute           *)
(*                                                                     *)
(*                                                                     *)
(*      Copyright 2011 IMDEA Software Institute                        *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.                                         *)
(*  See the License for the specific language governing permissions    *)
(*  and limitations under the License.                                 *)
(*                                                                     *)
(***********************************************************************)

open Printf

open LeapLib
open Global
open Hashtbl

module Expr = Expression
module NumExpr = NumExpression
module F = Formula

let get_name id = fst id
let get_line id = snd id

%}
%token <string*int> IDENT  // second param is line number
%token <int> NUMBER

%token OPEN_PAREN CLOSE_PAREN
%token COLON EQUALS NOT_EQUALS
%token LOGICAL_AND LOGICAL_OR LOGICAL_NOT LOGICAL_THEN LOGICAL_IFF
%token LOGICAL_TRUE LOGICAL_FALSE

%token MATH_PLUS MATH_MINUS MATH_MULT MATH_DIV MATH_LESS MATH_GREATER
%token MATH_LESS_EQ MATH_GREATER_EQ

%token EOF

%nonassoc EQUALS NOT_EQUALS MATH_LESS MATH_GREATER MATH_LESS_EQ MATH_GREATER_EQ
%nonassoc IDENT

%right LOGICAL_AND
%right LOGICAL_OR
%right LOGICAL_THEN
%right LOGICAL_IFF
%nonassoc LOGICAL_NOT


%left MATH_PLUS MATH_MINUS
%left MATH_MULT MATH_DIV
%right MATH_NEG


%start invariant_list
%start specification


%type <(string * NumExpression.formula)> one_spec
%type <(string * NumExpression.formula) list> specification
%type <(string * (NumExpression.conjunctive_formula)) list> invariant_list
%type <(string * NumExpression.conjunctive_formula)> one_invariant

%type <NumExpression.formula> formula
%type <NumExpression.literal> literal
%type <NumExpression.conjunctive_formula> conjunction_of_literals
%type <NumExpression.literal list> literal_list
%type <NumExpression.integer> integer
%type <NumExpression.term> term


%%
specification :
one_spec 
{ [$1] }
| one_spec specification
{ $1::$2 }

one_spec :
IDENT COLON formula
{ (get_name $1,$3) }

invariant_list:
|  one_invariant
  {  [ $1 ] }
| one_invariant invariant_list
  {  $1::$2 }

one_invariant:
  | IDENT COLON conjunction_of_literals
   {   (get_name $1,$3) }

formula :
  | OPEN_PAREN formula CLOSE_PAREN
      { $2 }
  | literal
      { F.Literal $1 }
  | LOGICAL_TRUE
      { F.True }
  | LOGICAL_FALSE
      { F.False }
  | LOGICAL_NOT formula
      { F.Not $2 }
  | formula LOGICAL_AND formula
      { F.And ($1, $3) }
  | formula LOGICAL_OR formula
      { F.Or ($1, $3) }
  | formula LOGICAL_THEN formula
      { F.Implies ($1, $3) }
  | formula LOGICAL_IFF formula
      { F.Iff ($1, $3) }


conjunction_of_literals : 
  | LOGICAL_FALSE
      { F.FalseConj }
  | LOGICAL_TRUE
      { F.TrueConj }
  |  literal_list
      { F.Conj $1 }

literal_list :
  | literal
      { [$1] }
  | literal LOGICAL_AND literal_list
      { $1::$3 }

literal :
   integer MATH_LESS integer
    {
      F.Atom (NumExpr.Less ($1, $3))
    }
  | integer MATH_GREATER integer
    {
      F.Atom (NumExpr.Greater ($1, $3))
    }
  | integer MATH_LESS_EQ integer
    {
      F.Atom (NumExpr.LessEq ($1, $3))
    }
  | integer MATH_GREATER_EQ integer
    {
      F.Atom (NumExpr.GreaterEq ($1, $3))
    }
  | term EQUALS term
    {
      F.Atom (NumExpr.Eq($1,$3))
    }
  | term NOT_EQUALS term
    {
      F.Atom (NumExpr.InEq($1,$3))
    }


term :
  | integer
    { NumExpr.IntV $1 }
/* Remains the support for sets, if necessary */


integer :
  | IDENT
      {
        let v = NumExpr.build_var (get_name $1) NumExpr.Int false
                                  NumExpr.V.Shared NumExpr.V.GlobalScope
        in
          NumExpr.Var v
      }
  | OPEN_PAREN integer CLOSE_PAREN
    { $2 }
  | NUMBER
    { NumExpr.Val $1 }
  | MATH_MINUS integer %prec MATH_NEG
    {
      NumExpr.Neg $2
    }
  | integer MATH_PLUS integer
    {
      NumExpr.Add ($1,$3)
    }
  | integer MATH_MINUS integer
    {
      NumExpr.Sub ($1,$3)
    }
  | integer MATH_MULT integer
    {
      NumExpr.Mul ($1,$3)
    }
  | integer MATH_DIV integer
    {
      NumExpr.Div ($1,$3)
    }


