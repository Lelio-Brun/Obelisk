
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

module Expr = Expression


exception WrongType of Expr.term


(* slow way to project: traverse one time per entry *)
let get_name id = fst id
let get_line id = snd id


let inv_table = Hashtbl.create 100


%}
%token <string*int> IDENT  // second param is line number
%token <int> NUMBER

%token INVARIANT AT LOC
%token TRUE FALSE LOGICAL_AND
%token MATH_MULT MATH_DIV
%token MATH_PLUS MATH_MINUS
%token EQUALS LESS_EQ GREATER_EQ
%token OPEN_PAREN CLOSE_PAREN
%token UNDERSCORE DOUBLE_UNDERSCORE PRIME
%token ANSWER
%token EOF

%right LOGICAL_AND

%left MATH_PLUS MATH_MINUS
%left MATH_MULT MATH_DIV
%right MATH_NEG

%start exists_projector
%start invariant_info
%start conj_list


%type <Expression.formula> exists_projector
%type <(int list, Expression.formula) Hashtbl.t> invariant_info
%type <unit> invariant_list
%type <(int list * Expr.formula)> invariant
%type <int list> pos_list
%type <Expression.formula list> conj_list
%type <Expr.formula> literal
%type <Expr.integer> integer
%type <Expr.tid option> th_param
%type <bool> primed



%%


/* EXIST PROJECTOR */

exists_projector :
  | ANSWER OPEN_PAREN conj_list CLOSE_PAREN
    {
      Expr.conj_list $3
    }


/* INVARIANT LIST */

invariant_info :
  | invariant_list
    {
      let copy = Hashtbl.copy inv_table in
      let _    = Hashtbl.clear inv_table
      in
        copy
    }


invariant_list :
  |
    { () }
  | invariant invariant_list
    {
      let (loc,inv) = $1 in
      let _ = Hashtbl.add inv_table loc inv in
        ()
    }


invariant :
  | INVARIANT AT LOC UNDERSCORE pos_list OPEN_PAREN conj_list CLOSE_PAREN
    {
      let p_list = $5 in
      let expr  = $7 in

      (p_list, Expr.conj_list expr)
    }


pos_list :
  | NUMBER
    { [$1] }
  | NUMBER UNDERSCORE pos_list
    { $1 :: $3 }


conj_list :
  | literal
    { [$1] }
  | literal LOGICAL_AND conj_list
    { $1 :: $3 }


literal :
  | TRUE
    { Expr.True }
  | FALSE
    { Expr.False }
  | integer EQUALS integer
    {
      let n1 = $1 in
      let n2 = $3 in

      Expr.eq_int n1 n2
    }
  | integer LESS_EQ integer
    {
      let n1 = $1 in
      let n2 = $3 in

      Expr.lesseq_form n1 n2
    }
  | integer GREATER_EQ integer
    {
      let n1 = $1 in
      let n2 = $3 in

      Expr.greatereq_form n1 n2
    }


th_param :
  |
    {
      None
    }
  | DOUBLE_UNDERSCORE NUMBER
    {
      Some (Expr.VarTh (Expr.build_num_tid_var $2))
    }
  | DOUBLE_UNDERSCORE IDENT
    {
      let id = get_name $2
      in
        Some (Expr.VarTh (Expr.build_global_var id Expr.Tid))
    }

primed :
  |
    { false }
  | PRIME
    { true } 


integer :
  | NUMBER
    {
      Expr.IntVal $1
    }
  | IDENT th_param primed
    {
      let id         = get_name $1 in
      let pr         = $3 in
      let th         = $2 in
      let proc_owner = None in
      let kind       = Expr.Normal in

      Expr.VarInt (Expr.build_var id Expr.Int pr th proc_owner kind)
    }
  | IDENT UNDERSCORE IDENT th_param primed
    {
      let id         = get_name $3 in
      let pr         = $5 in
      let th         = $4 in
      let proc_owner = Some (get_name $1) in
      let kind       = Expr.Normal in

      Expr.VarInt (Expr.build_var id Expr.Int pr th proc_owner kind)
    }
  | MATH_MINUS integer %prec MATH_NEG
    {
      Expr.IntNeg $2
    }
  | integer MATH_PLUS integer
    {
      Expr.IntAdd ($1, $3)
    }
  | integer MATH_MINUS integer
    {
      Expr.IntSub ($1, $3)
    }
  | integer MATH_MULT integer
    {
      Expr.IntMul ($1, $3)
    }
  | integer MATH_DIV integer
    {
      Expr.IntDiv ($1, $3)
    }
