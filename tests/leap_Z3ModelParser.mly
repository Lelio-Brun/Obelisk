
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

open LeapLib

(* Type rename *)

module GM = GenericModel



type value_t =
  | Constant of string
  | Record of (GM.id * abs_value_t list)
  | FunMap of ((GM.id option) list, abs_value_t) Hashtbl.t
  | Store of (abs_value_t * GM.id * value_t)


and abs_value_t =
  | Concrete of value_t
  | AsArray of int


exception Not_implemented of string

(* slow way to project: traverse one time per entry *)
let get_name id = fst id
let get_line id = snd id


(* Mappings *)
let model = GM.new_model ()


(* Mapping from identifiers to array ids*)
let var_array_map : (GM.id, int) Hashtbl.t = Hashtbl.create 10

(* Mapping from identifiers to values *)
let var_value_map : (GM.id, value_t) Hashtbl.t = Hashtbl.create 10

(* Mapping form array ids to values *)
let array_value_map : (int, value_t) Hashtbl.t = Hashtbl.create 10


let abs_to_value (a:abs_value_t) : value_t =
  match a with
  | Concrete v -> v
  | AsArray i -> Hashtbl.find array_value_map i


let rec value_to_vals (v:value_t) : GM.vals =
  match v with
  | Constant c -> c
  | FunMap tbl -> "[" ^ (Hashtbl.fold (fun id_list abs_val str ->
                          let ids = String.concat "," (List.map (Option.default "_") id_list) in
                          str ^ ids ^ ":" ^ (value_to_vals (abs_to_value abs_val)) ^ ";"
                        ) tbl "") ^ "]"
  | _          -> "IMPOSSIBLE TO REPRESENT =("


let convert (v:value_t) : GM.value =
  match v with
  | Constant c -> GM.Single c
  | Record (name, vs) -> let lbls = List.map string_of_int
                                      (LeapLib.rangeList 0 (List.length vs-1))in
                         let vs' = List.map (abs_to_value>>value_to_vals) vs
                         in
                           GM.Record (name, List.combine lbls vs')
  | FunMap _ -> raise(Not_implemented "conversion over function maps")
  | Store _ -> raise(Not_implemented "conversion over array store")


let rec build_fun (tbl:((GM.id option) list, abs_value_t) Hashtbl.t)
                  (prevs:(GM.id option) list)
                  (av:abs_value_t) : unit =
  match av with
  | Concrete (Constant _)      -> Hashtbl.add tbl prevs av
  | Concrete (Record (_,_))    -> Hashtbl.add tbl prevs av
  | Concrete (Store (av,_,_))  -> Hashtbl.add tbl prevs av
  | Concrete (FunMap fMap)     -> Hashtbl.iter (fun ids av ->
                                    build_fun tbl (prevs@ids) av
                                  ) fMap
  | AsArray i                  -> build_fun tbl prevs
                                    (Concrete (Hashtbl.find array_value_map i))


let build_fun_from (i:int) : ((GM.id option) list, abs_value_t) Hashtbl.t =
  let tbl = Hashtbl.create 10 in
  let av = Hashtbl.find array_value_map i in
    (build_fun tbl [] (Concrete av); tbl)


(* Defines a new value in the model *)
let define (id:GM.id) (v:value_t) : unit =
  match v with
  | Constant _        -> GM.decl_const model id (convert v)
  | Record (_, _)     -> GM.decl_const model id (convert v)
  | FunMap fMap       -> Hashtbl.iter (fun ps v ->
                           GM.decl_fun model id ps ((abs_to_value>>convert) v)
                         ) fMap
  | Store (abs, i, v) -> let _ = GM.decl_fun model id [Some i] (convert v)
                         in
                           match abs with
                           | Concrete c -> GM.decl_fun model
                                             id [None] (convert c)
                           | AsArray j  -> Hashtbl.iter (fun ps av ->
                                             GM.decl_fun model id ps
                                               ((abs_to_value>>convert) av)
                                           ) (build_fun_from j)


(* Fills the model with the parsed information *)
let fill_model () : unit =
  Hashtbl.iter (fun v i ->
    try
      define v (FunMap (build_fun_from i))
    with _ -> ()
  ) var_array_map;
  Hashtbl.iter (fun v x ->
    define v x
  ) var_value_map



%}
%token <string*int> IDENT  // second param is line number
%token <int> NUMBER

%token <string> TID_ELEM
%token <string> ADDR_ELEM
%token <string> ELEM_ELEM
%token <bool> BOOL
%token <int> ARRAY_ID

%token EQUAL OPEN_PAREN CLOSE_PAREN DOUBLE_SEMICOLON UNDERSCORE
%token MODEL DECLARE_FUN DEFINE_FUN ARRAY AS_ARRAY
%token AS CONST STORE
%token FORALL OR AND ITE
%token NO_THREAD ERROR
%token EOL EOF


%start generic_model


%type <GenericModel.t> generic_model

%type <unit> assertion_list
%type <unit> assertion
%type <unit> universe_decl
%type <unit> cardinality_constraint
%type <unit> conds
%type <unit> define_fun
%type <GM.id> var
%type <unit> params
%type <unit> param_list
%type <value_t> value
%type <abs_value_t> abs_value
%type <abs_value_t list> abs_value_list
%type <(abs_value_t * GM.id * value_t)> store
%type <GM.vals> constant
%type <value_t> const_array
%type <GM.id * abs_value_t list> record
%type <int> as_array
%type <((GM.id option) list, abs_value_t) Hashtbl.t> ite
%type <((GM.id option) list, abs_value_t) Hashtbl.t> else_ite
%type <GM.id list> cond_list
%type <GM.id> cond_elem
%type <(GM.id option) list> cond_elem_as_arguments
%type <unit> type_decl


%%


/* MODEL FOR COUNTER EXAMPLE PARSER */

generic_model :
  |
    { GM.new_model() }
  | OPEN_PAREN MODEL assertion_list CLOSE_PAREN
    { let _ = fill_model () in
      let m = GM.copy_model model in
      let _ = GM.clear_model model in
      let _ = Hashtbl.clear var_array_map in
      let _ = Hashtbl.clear var_value_map in
      let _ = Hashtbl.clear array_value_map
      in
        m
    }


assertion_list :
  | assertion
    { () }
  | assertion assertion_list
    { () }


assertion :
  | universe_decl
    { () }
  | cardinality_constraint
    { () }
  | define_fun
    { () }


universe_decl:
  | OPEN_PAREN DECLARE_FUN IDENT params type_decl CLOSE_PAREN
    { () }


cardinality_constraint :
  | OPEN_PAREN FORALL params conds CLOSE_PAREN
    { () }


conds :
  | cond_elem
    { () }
  | OPEN_PAREN OR cond_list CLOSE_PAREN
    { () }


cond_list :
  |
    { [] }
  | cond_elem cond_list
    { $1 :: $2 }


cond_elem :
  | OPEN_PAREN EQUAL IDENT constant CLOSE_PAREN
    { $4 }


cond_elem_as_arguments :
  | cond_elem
    { [Some $1] }
  | OPEN_PAREN AND cond_list CLOSE_PAREN
    { List.map (fun c -> Some c) $3 }


define_fun :
  | OPEN_PAREN DEFINE_FUN var params type_decl as_array CLOSE_PAREN
    {
      Hashtbl.add var_array_map $3 $6
    }
  | OPEN_PAREN DEFINE_FUN var params type_decl value CLOSE_PAREN
    {
      Hashtbl.add var_value_map $3 $6
    }
  | OPEN_PAREN DEFINE_FUN ARRAY_ID params type_decl value CLOSE_PAREN
    {
      Hashtbl.add array_value_map $3 $6;
    }


var :
  | ERROR
    { "error" }
  | IDENT
    { get_name $1 }


params :
  | OPEN_PAREN param_list CLOSE_PAREN
    { () }


param_list :
  |
    { () }
  | OPEN_PAREN IDENT type_decl CLOSE_PAREN param_list
    { () }

as_array :
  | OPEN_PAREN UNDERSCORE AS_ARRAY ARRAY_ID CLOSE_PAREN
    { $4 }


abs_value :
  | value
    { Concrete $1 }
  | as_array
    { AsArray $1 }


value :
  | constant
    { Constant $1 }
  | record
    { Record $1 }
  | ite
    { FunMap $1 }
  | const_array
    { $1 }
  | store
    { Store $1 }


constant :
  | BOOL
    { if $1 then "true" else "false" }
  | TID_ELEM
    { $1 }
  | ADDR_ELEM
    { $1 }
  | ELEM_ELEM
    { $1 }
  | NUMBER
    { string_of_int $1 }
  | NO_THREAD
    { "NoThread" }
  | IDENT
    { get_name $1 }


record :
  | OPEN_PAREN IDENT abs_value_list CLOSE_PAREN
    { (get_name $2, $3) }


ite :
  | OPEN_PAREN ITE cond_elem_as_arguments abs_value else_ite CLOSE_PAREN
    {
      let conds = $3 in
      let cte = $4 in
      let tbl = $5 in
      let _ = Hashtbl.add tbl conds cte
      in
        tbl
    }


else_ite :
  | constant
    { (*(Some (Concrete (Constant $1)), Hashtbl.create 10)*)
      let tbl = Hashtbl.create 10 in
      (Hashtbl.add tbl [None] (Concrete (Constant $1)); tbl)
    }
  | record
    { (*(Some (Concrete (Record $1)), Hashtbl.create 10)*)
      let tbl = Hashtbl.create 10 in
      (Hashtbl.add tbl [None] (Concrete (Record $1)); tbl)
    }
  | as_array
    { (*(Some (AsArray $1), Hashtbl.create 10)*)
      let tbl = Hashtbl.create 10 in
      (Hashtbl.add tbl [None] (AsArray $1); tbl)
    }
  | ite
    { $1 }


const_array :
  | OPEN_PAREN OPEN_PAREN AS CONST type_decl CLOSE_PAREN value CLOSE_PAREN
    { $7 }


store :
  | OPEN_PAREN STORE abs_value constant value CLOSE_PAREN
    { ($3, $4, $5) }


abs_value_list :
  | abs_value
    { [$1] }
  | abs_value abs_value_list
    { $1 :: $2 }


type_decl :
  | IDENT
    { () (*GM.Const (get_name $1)*) }
  | OPEN_PAREN ARRAY type_decl type_decl CLOSE_PAREN
    { () (*GM.Fun ([get_name $3], [get_name $4])*) }
