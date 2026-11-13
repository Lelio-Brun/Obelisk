
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

(* Type rename *)

module GM = GenericModel

(* slow way to project: traverse one time per entry *)
let get_name id = fst id
let get_line id = snd id


(* Mappings *)
let model = GM.new_model ()


(* ID counter *)
let id_count : int ref = ref 0


(* Record counter *)
let record_count : int ref = ref 0


(* Updates and propagates a cell id update *)
let gen_fresh_id () : int =
  let _ = incr id_count
  in
    !id_count

let gen_fresh_record_id () : string =
  incr record_count;
  "record" ^ (string_of_int !record_count)


let gen_record_id (i:int) : string =
  "rec" ^ (string_of_int i)


(* Temporary record storage *)
let record_tbl : ((GM.id * GM.vals) list, string) Hashtbl.t = Hashtbl.create 5
let record_synonyms : (int, string) Hashtbl.t = Hashtbl.create 5


let record_lookup (fields:(GM.id * GM.vals) list) : string =
  try
    Hashtbl.find record_tbl fields
  with
    Not_found ->
      begin
        let fresh_id = gen_fresh_record_id () in
        Hashtbl.add record_tbl fields fresh_id;
        GM.sm_decl_const (GM.get_aux_sort_map model) fresh_id GM.pair_s;
        fresh_id
      end


let record_add (fields:(GM.id * GM.vals) list) (i:int) : string =
  let id = gen_record_id i in
  Hashtbl.add record_tbl fields id;
  GM.sm_decl_const (GM.get_aux_sort_map model) id GM.pair_s;
  id




%}
%token <string*int> IDENT  // second param is line number
%token <int> NUMBER

%token <string> TID_ELEM
%token <string> ADDR_ELEM
%token <string> ELEM_ELEM
%token <bool> BOOL

%token EQUAL OPEN_PAREN CLOSE_PAREN DOUBLE_COLON SELECT MK_RECORD MINUS
%token NO_THREAD ERROR NULL
%token EOF


%start generic_model


%type <GenericModel.t> generic_model

%type <unit> assertion_list
%type <unit> assertion
%type <unit> sel
%type <GM.var> var
%type <GM.id> fun_name
%type <(GM.id option) list> param_list
%type <GM.value> value
%type <GM.vals> constant
%type <int> number
%type <GM.id * (GM.id * GM.vals) list> record
%type <(GM.id * GM.vals) list> field_list
%type <GM.id * GM.vals> field


%%


/* MODEL FOR COUNTER EXAMPLE PARSER */

generic_model :
  |
    { GM.new_model() }
  | assertion_list
    {
      Hashtbl.iter (fun fields id ->
        GM.decl_const model id (GM.Record("mk-record", fields))
      ) record_tbl;
      let m = GM.copy_model model in
      let _ = GM.clear_model model
      in
        m
    }


assertion_list :
  | assertion
    { () }
  | assertion assertion_list
    { () }


assertion :
  | OPEN_PAREN EQUAL record NUMBER CLOSE_PAREN
    {
      let fields = snd $3 in
      let _ = record_add fields $4 in
      ()
    }
  | OPEN_PAREN EQUAL var var CLOSE_PAREN
    {
      GM.unify model $3 $4
    }
  | OPEN_PAREN EQUAL var value CLOSE_PAREN
    {
      match $3 with
      | GM.Var v               -> GM.decl_const model v $4
      | GM.Function (f,params) -> GM.decl_fun model f params $4
    }
  | OPEN_PAREN EQUAL value var CLOSE_PAREN
    {
      match $4 with
      | GM.Var v               -> GM.decl_const model v $3
      | GM.Function (f,params) -> GM.decl_fun model f params $3
    }
  | OPEN_PAREN EQUAL OPEN_PAREN SELECT NUMBER IDENT CLOSE_PAREN value CLOSE_PAREN
    { () }
  | OPEN_PAREN EQUAL OPEN_PAREN SELECT NUMBER NUMBER CLOSE_PAREN value CLOSE_PAREN
    { () }
  | OPEN_PAREN EQUAL OPEN_PAREN SELECT sel IDENT CLOSE_PAREN value CLOSE_PAREN
    { () }
  | OPEN_PAREN EQUAL OPEN_PAREN SELECT sel NUMBER CLOSE_PAREN value CLOSE_PAREN
    { () }




sel :
  | var
    { () }
  | record
    { () }


var :
  | ERROR
    { GM.Var "error" }
  | IDENT
    { GM.Var (get_name $1) }
  | OPEN_PAREN fun_name param_list CLOSE_PAREN
    { GM.Function ($2, $3) }
  | OPEN_PAREN fun_name record CLOSE_PAREN
    {
      let record_id = record_lookup (snd $3) in
      GM.Function ($2, [Some record_id])
    } 


fun_name :
  | IDENT
    { get_name $1 }
  | constant
    { $1 }


param_list :
  | constant
    { [Some $1] }
  | constant param_list
    { Some $1 :: $2 }


value :
  | constant
    { GM.Single $1 }
  | record
    { GM.Record $1 }


constant :
  | BOOL
    { if $1 then "true" else "false" }
  | TID_ELEM
    { $1 }
  | ADDR_ELEM
    { $1 }
  | ELEM_ELEM
    { $1 }
  | number
    { (string_of_int $1) }
  | NO_THREAD
    { "NoThread" }
  | NULL
    { "null" }


number :
  | NUMBER
    { $1 }
  | MINUS NUMBER
    { - $2 }


record :
  | OPEN_PAREN MK_RECORD field_list CLOSE_PAREN
    {
      ignore (record_lookup $3);
      ("mk-record", $3)
    }


field_list :
  | field
    { [$1] }
  | field field_list
    { $1 :: $2 }


field :
  | IDENT DOUBLE_COLON constant
    { (get_name $1, $3) }
