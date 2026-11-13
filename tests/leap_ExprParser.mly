
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

module E      = Expression
module Symtbl = ExprSymTable

(* ALE: This code should be changed in the future *)

exception WrongType of E.term
exception Sort_mismatch of E.V.id * E.sort * E.sort
exception Boolean_var_expected of E.term
exception Not_sort_name of string
exception Unknown_procedure of string
exception Variable_not_in_procedure of E.V.id * string
exception Unexpected_statement of string
exception Different_argument_length of string * string
exception Wrong_edge_acceptance_argument of string


let invVars = System.empty_var_table ()

let empty_tbl = Hashtbl.create 1

let curr_box_counter = ref 0

let curr_tag : string ref = ref ""


(* Looks for a term sort in the global and temporal var tables. *)
let get_sort (t:E.term) : E.sort =
  let p = E.term_scope t in
  let gVars = System.get_global !Symtbl.sys in
  let (iVars,lVars) = match p with
                        E.V.Scope proc  -> (System.get_input_by_name !Symtbl.sys proc,
                                             System.get_local_by_name !Symtbl.sys proc)
                      | E.V.GlobalScope -> (System.empty_var_table (),
                                             System.empty_var_table ())
  in
    System.get_sort_from_term gVars iVars lVars invVars t


(* Parsing error message function *)
let parser_error (msg:string) =
  let msg = sprintf "Error at line %i:\n%s" (Global.get_linenum ()) msg in
    raise(ParserError msg)



let parser_typing_error (term:E.term)
                        (a_sort:E.sort)
                        (get_expr:unit -> string) =
  let term_str = (E.term_to_str term) in
  let term_sort_str = (E.sort_to_str (get_sort term)) in
  let sort_str = (E.sort_to_str a_sort) in
  let str_expr = (get_expr ()) in
  let str = sprintf "Term \"%s\" is of sort %s, but it was \
                     expected to be of sort %s in expression \"%s\""
                     term_str term_sort_str sort_str str_expr in
  parser_error str



let parser_types_incompatible (t1:E.term)
                              (t2:E.term)
                              (get_expr_str:unit -> string) =
  let t1_str = (E.term_to_str t1) in
  let s1_str = (E.sort_to_str (get_sort t1)) in
  let t2_str = (E.term_to_str t2) in
  let s2_str = (E.sort_to_str (get_sort t2)) in
  let str_expr = (get_expr_str ()) in
  let str = (Printf.sprintf "Unexpectedly \"%s\" is of type \"%s\" and  \
                             \"%s\" is of type \"%s\", when they should \
                             have the same type in expression \"%s\"."
                            t1_str s1_str t2_str s2_str str_expr) in
    parser_error str



let parser_check_compatibility (t1:E.term)
                               (t2:E.term)
                               (get_expr_str:unit -> string) =
  let s1 = get_sort t1 in
  let s2 = get_sort t2 in
    if (s1 != s2) then
      parser_types_incompatible t1 t2 get_expr_str



let parser_check_type checker a_term a_sort get_expr_str =
  try
    checker a_term
  with
    | WrongType(_) -> parser_typing_error a_term a_sort get_expr_str


let decl_inv_var (v:E.V.id) (s:E.sort) (e:E.initVal_t option) : unit =
  System.add_var invVars v s e E.V.Shared E.RealVar



(* Slow way to project: traverse one time per entry. *)
let get_name id = fst id
let get_line id = snd id


let check_sort_var (v:E.V.t) =
  let generic_var = E.VarT (E.build_var (E.V.id v) E.Unknown false E.V.Shared
                                        (E.V.scope v) ~nature:(E.var_nature v)) in
  let knownSort = get_sort generic_var in
    if (knownSort != (E.V.sort v)) then
      begin
        Interface.Err.msg "Mismatch variable type" $
          sprintf "Variable %s is of sort %s, while it is trying to be \
                   assigned to an expression of sort %s"
                    (E.V.id v) (E.sort_to_str knownSort) (E.sort_to_str (E.V.sort v));
        raise(Sort_mismatch((E.V.id v),knownSort,(E.V.sort v)))
      end


let wrong_sort_msg_for (t:E.term) (s:E.sort) =
  Interface.Err.msg "Wrong type" $
  sprintf "A term of sort %s was expected, but term \"%s\" has sort %s."
              (E.sort_to_str s) (E.term_to_str t)
              (E.sort_to_str (get_sort t))


let parser_check_boolean_type (a_term:E.term)
                              (get_expr_str:unit -> string) : E.formula =
  match a_term with
    | E.VarT v -> let var = E.V.set_sort v E.Bool in
                       check_sort_var var;
                       E.boolvar var
    | _           -> parser_typing_error a_term E.Bool get_expr_str


let check_type_int t =
  match t with
      E.IntT i -> i
    | E.VarT v -> let var = E.V.set_sort v E.Int in
                       check_sort_var var;
                       E.VarInt var
    | _           -> raise(WrongType t)


let check_type_pair t =
  match t with
      E.PairT p -> p
    | E.VarT v -> let var = E.V.set_sort v E.Pair in
                       check_sort_var var;
                       E.VarPair var
    | _           -> raise(WrongType t)


let check_type_set t =
  match t with
      E.SetT s -> s
    | E.VarT v -> let var = E.V.set_sort v E.Set in
                       check_sort_var var;
                       E.VarSet var
    | _           -> raise(WrongType t)


let check_type_elem t =
  match t with
      E.ElemT e -> e
    | E.VarT v  -> let var = E.V.set_sort v E.Elem in
                        check_sort_var var;
                        E.VarElem var
    | _            -> raise(WrongType t)


let check_type_tid t =
  match t with
      E.TidT th -> th
    | E.VarT v   -> let var = E.V.set_sort v E.Tid in
                         check_sort_var var;
                         E.VarTh var
    | _             -> raise(WrongType t)


let check_type_addr t =
  match t with
      E.AddrT a -> a
    | E.VarT v  -> let var = E.V.set_sort v E.Addr in
                        check_sort_var var;
                        E.VarAddr var
    | _            -> raise(WrongType t)


let check_type_cell t =
  match t with
      E.CellT c -> c
    | E.VarT v  -> let var = E.V.set_sort v E.Cell in
                        check_sort_var var;
                        E.VarCell var
    | _            -> raise(WrongType t)


let check_type_setth t =
  match t with
      E.SetThT sth -> sth
    | E.VarT v     -> let var = E.V.set_sort v E.SetTh in
                           check_sort_var var;
                           E.VarSetTh var
    | _               -> raise(WrongType t)


let check_type_setint t =
  match t with
      E.SetIntT sth -> sth
    | E.VarT v      -> let var = E.V.set_sort v E.SetInt in
                            check_sort_var var;
                            E.VarSetInt var
    | _                -> raise(WrongType t)


let check_type_setelem t =
  match t with
      E.SetElemT se -> se
    | E.VarT v      -> let var = E.V.set_sort v E.SetElem in
                            check_sort_var var;
                            E.VarSetElem var
    | _                -> raise(WrongType t)


let check_type_setpair t =
  match t with
      E.SetPairT sp -> sp
    | E.VarT v      -> let var = E.V.set_sort v E.SetPair in
                            check_sort_var var;
                            E.VarSetPair var
    | _                -> raise(WrongType t)


let check_type_path t =
  match t with
      E.PathT p -> p
    | E.VarT v  -> let var = E.V.set_sort v E.Path in
                        check_sort_var var;
                        E.VarPath var
    | _            -> raise(WrongType t)


let check_type_mem t =
  match t with
      E.MemT m  -> m
    | E.VarT v  -> let var = E.V.set_sort v E.Mem in
                        check_sort_var var;
                        E.VarMem var
    | _            -> raise(WrongType t)


let check_type_addrarr t =
  match t with
      E.AddrArrayT arr -> arr
    | E.VarT v         -> let var = E.V.set_sort v E.AddrArray in
                               check_sort_var var;
                               E.VarAddrArray var
    | _                   -> raise(WrongType t)


let check_type_tidarr t =
  match t with
      E.TidArrayT arr -> arr
    | E.VarT v        -> let var = E.V.set_sort v E.TidArray in
                              check_sort_var var;
                              E.VarTidArray var
    | _                  -> raise(WrongType t)


let check_type_mark t =
  match t with
      E.MarkT m -> m
    | E.VarT v  -> let var = E.V.set_sort v E.Mark in
                     check_sort_var var;
                     E.VarMark var
    | _         -> raise(WrongType t)


let check_type_bucket t =
  match t with
      E.BucketT b -> b
    | E.VarT v  -> let var = E.V.set_sort v E.Bucket in
                     check_sort_var var;
                     E.VarBucket var
    | _         -> raise(WrongType t)


let check_type_bucketarr t =
  match t with
      E.BucketArrayT arr -> arr
    | E.VarT v  -> let var = E.V.set_sort v E.BucketArray in
                     check_sort_var var;
                     E.VarBucketArray var
    | _         -> raise(WrongType t)


let check_type_lock t =
  match t with
      E.LockT l -> l
    | E.VarT v  -> let var = E.V.set_sort v E.Lock in
                     check_sort_var var;
                     E.VarLock var
    | _         -> raise(WrongType t)


let check_type_lockarr t =
  match t with
      E.LockArrayT arr -> arr
    | E.VarT v  -> let var = E.V.set_sort v E.LockArray in
                     check_sort_var var;
                     E.VarLockArray var
    | _         -> raise(WrongType t)


let check_and_get_sort (id:string) : E.sort =
  match id with
    "tid"       -> E.Tid
  | "elem"      -> E.Elem
  | "addr"      -> E.Addr
  | "cell"      -> E.Cell
  | "mem"       -> E.Mem
  | "path"      -> E.Path
  | "bool"      -> E.Bool
  | "addrSet"   -> E.Set
  | "tidSet"    -> E.SetTh
  | "intSet"    -> E.SetInt
  | "elemSet"   -> E.SetElem
  | "int"       -> E.Int
  | "array"     -> E.Array
  | "addrarr"   -> E.AddrArray
  | "tidarr"    -> E.TidArray
  | "mark"      -> E.Mark
  | "bucket"    -> E.Bucket
  | "bucketarr" -> E.BucketArray
  | "tlock"     -> E.Lock
  | "lockarr"   -> E.LockArray
  | _ -> begin
           Interface.Err.msg "Unrecognized sort" $
             sprintf "A sort was expected, but \"%s\" was found" id;
           raise(Not_sort_name id)
         end


let check_is_procedure (id:string) =
  if not (System.is_proc !Symtbl.sys id) then
    begin
      Interface.Err.msg "Unknown procedure" $
              sprintf "Identifier \"%s\" is used as a procedure identifier, \
                       but no procedure with such name has been parsed." id;
      raise(Unknown_procedure id)
    end


let inject_sort (exp:E.term) : E.term =
  match exp with
    E.VarT v -> let s = get_sort exp in
                   let var = E.V.set_sort v s in
                     begin
                       match s with
                         E.Set         -> E.SetT         (E.VarSet       var)
                       | E.Elem        -> E.ElemT        (E.VarElem      var)
                       | E.Tid         -> E.TidT         (E.VarTh        var)
                       | E.Addr        -> E.AddrT        (E.VarAddr      var)
                       | E.Cell        -> E.CellT        (E.VarCell      var)
                       | E.SetTh       -> E.SetThT       (E.VarSetTh     var)
                       | E.SetInt      -> E.SetIntT      (E.VarSetInt    var)
                       | E.SetElem     -> E.SetElemT     (E.VarSetElem   var)
                       | E.SetPair     -> E.SetPairT     (E.VarSetPair   var)
                       | E.Path        -> E.PathT        (E.VarPath      var)
                       | E.Mem         -> E.MemT         (E.VarMem       var)
                       | E.Bool        -> E.VarT         (var)
                       | E.Int         -> E.IntT         (E.VarInt       var)
                       | E.Pair        -> E.PairT        (E.VarPair      var)
                       | E.Array       -> E.ArrayT       (E.VarArray     var)
                       | E.AddrArray   -> E.AddrArrayT   (E.VarAddrArray var)
                       | E.TidArray    -> E.TidArrayT    (E.VarTidArray  var)
                       | E.BucketArray -> E.BucketArrayT (E.VarBucketArray  var)
                       | E.Mark        -> E.MarkT        (E.VarMark      var)
                       | E.Bucket      -> E.BucketT      (E.VarBucket    var)
                       | E.Lock        -> E.LockT        (E.VarLock      var)
                       | E.LockArray   -> E.LockArrayT   (E.VarLockArray var)
                       | E.Unknown     -> E.VarT         (var)
                     end
  | _           -> exp


let unexpected_statement get_str_expr =
  let str_expr = (get_str_expr()) in
    Interface.Err.msg "Unexpected statement" $
      sprintf "Ghost and atomic statements admit only assignments or \
               conditional statements. However, the following statement \
               was found:\n\n%s\n" str_expr;
    raise(Unexpected_statement str_expr)


let check_var_belongs_to_procedure (v:E.V.id) (p_name:string) =
  let p_info = System.get_proc_by_name !Symtbl.sys p_name in
  let iVars = System.proc_info_get_input p_info in
  let lVars = System.proc_info_get_local p_info in
    if not (System.mem_var iVars v || System.mem_var lVars v) then
      begin
        Interface.Err.msg "Variable not declared in procedure" $
                sprintf "Variable \"%s\" does not belong to procedure %s"
                        v p_name;
        raise(Variable_not_in_procedure(v,p_name))
      end


let check_delta_sort (s:E.sort) : unit =
  match s with
    E.Int    -> ()
  | E.Set    -> ()
  | E.SetTh  -> ()
  | E.SetInt -> ()
  | _        -> Interface.Err.msg "Wrong ranking function sort" $
                  sprintf "By the moment, only expressions of sort %s are \
                           accepted for ranking functions. Instead, an \
                           expression of sort %s was found."
                          (E.sort_to_str E.Int)
                          (E.sort_to_str s)


let define_ident (proc_name:E.V.procedure_name)
                 (id:string)
                 (th:E.V.shared_or_local) : E.term =
      let k = match proc_name with
              | E.V.Scope p ->      check_is_procedure p;
                                    check_var_belongs_to_procedure id p;
                                    let proc_info = System.get_proc_by_name !Symtbl.sys p in
                                    let iVars     = System.proc_info_get_input proc_info in
                                    let lVars     = System.proc_info_get_local proc_info in
                                    if System.mem_var iVars id then
                                      System.find_var_kind iVars id
                                    else
                                      System.find_var_kind lVars id
              | E.V.GlobalScope -> try
                                      let gVars = System.get_global !Symtbl.sys in
                                        System.find_var_kind gVars id
                                    with _ -> E.RealVar in
      inject_sort (E.VarT (E.build_var id E.Unknown false th proc_name ~nature:k))


%}
%token <string*int> IDENT  // second param is line number
%token <int> NUMBER

%token DIAGRAM SUPPORT BOXES NODES INITIAL EDGES ACCEPTANCE
%token EDGE_ARROW EDGE_ARROW_OPEN EDGE_ARROW_CLOSE

%token BEGIN END

%token ERROR MKCELL DATA NEXT ARR TIDS MAX LOCKID LOCK UNLOCK LOCKAT UNLOCKAT
%token NEXTAT
%token MEMORY_READ
%token DOT COMMA
%token NULL UPDATE
%token LOWEST_ELEM HIGHEST_ELEM
%token EPSILON
%token EMPTYSET UNION INTR SETDIFF
%token EMPTYSETTH UNIONTH INTRTH SETDIFFTH SINGLETH
%token EMPTYSETINT UNIONINT INTRINT SETDIFFINT SINGLEINT SETLOWER
%token EMPTYSETELEM UNIONELEM INTRELEM SETDIFFELEM SINGLEELEM SET2ELEM
%token PATH2SET ADDR2SET GETP FIRSTLOCKED LASTLOCKED LOCKSET ORDERLIST SKIPLIST
%token APPEND REACH
%token IN SUBSETEQ
%token INTH SUBSETEQTH
%token ININT SUBSETEQINT
%token INELEM SUBSETEQELEM
%token SETINTMIN SETINTMAX
%token INTOF TIDOF SETPAIRMIN SETPAIRMAX
%token SETPAIREMPTY SETPAIRSINGLE SETPAIRUNION SETPAIRINTR SETPAIRDIFF SETPAIRLOWER
%token SETPAIRIN SETPAIRSUBSETEQ
%token SETPAIRININTPAIR SETPAIRINTIDPAIR SETPAIRUNIQUETID SETPAIRUNIQUEINT
%token THREAD
%token MARK_T MARK_F MARKED
%token MKBUCKET BINIT BEND BREGION BTID BARRAYUPD
%token HASHTBL
%token OPEN_BRACKET CLOSE_BRACKET
%token OPEN_SET CLOSE_SET
%token OPEN_PAREN CLOSE_PAREN
%token OPEN_ANGLE CLOSE_ANGLE
%token GOOD BAD
%token VERTICAL_BAR
%token COLON DOUBLECOLON SEMICOLON EQUALS NOT_EQUALS
%token ASSIGN
%token LOGICAL_AND LOGICAL_OR LOGICAL_NOT LOGICAL_THEN LOGICAL_IFF
%token LOGICAL_TRUE LOGICAL_FALSE
%token ARR_UPDATE
%token WF_INTSUBSET WF_PAIRSUBSET WF_INTLESS WF_ADDRSUBSET WF_ELEMSUBSET WF_TIDSUBSET

%token AXIOM INVARIANT FORMULA VARS
%token AT UNDERSCORE SHARP
%token MATH_PLUS MATH_MINUS MATH_MULT MATH_DIV MATH_LESS MATH_GREATER MATH_MOD
%token MATH_LESS_EQ MATH_GREATER_EQ


%token TID_CONSTRAINT RHO GOAL TRANSITION_TID LINE


%token EOF

%nonassoc EQUALS NOT_EQUALS MATH_LESS MATH_GREATER MATH_LESS_EQ MATH_GREATER_EQ
%nonassoc IDENT

%nonassoc ASSIGN

%right LOGICAL_AND
%right LOGICAL_OR
%right LOGICAL_THEN
%nonassoc LOGICAL_NOT

%left UNION INTR SETDIFF
%left UNIONTH INTRTH SETDIFFTH
%left UNIONINT INTRINT SETDIFFINT

%nonassoc IN SUBSETEQ
%nonassoc INTH SUBSETEQTH
%nonassoc ININT SUBSETEQINT
%nonassoc INELEM SUBSETEQELEM


%nonassoc GHOST_DELIMITER
%nonassoc OPEN_BRACKET CLOSE_BRACKET
%nonassoc OPEN_PAREN CLOSE_PAREN
%nonassoc VERTICAL_BAR


%left MATH_PLUS MATH_MINUS
%left MATH_MULT MATH_DIV MATH_MOD
%right MATH_NEG

%left DOT



%start invariant
%start axiom
%start formula
%start vc_info
%start single_formula
%start pvd

%type <Tactics.vc_info> vc_info

%type <Expression.formula> single_formula

%type <System.var_table_t * Tag.f_tag option * (Tag.f_tag option * Expression.formula) list> invariant
%type <System.var_table_t * Tag.f_tag option * (Tag.f_tag option * Expression.formula) list> axiom
%type <(Tag.f_tag option * Expression.formula) list> formula_decl_list
%type <(Tag.f_tag option * Expression.formula)> formula_decl

%type <unit> inv_var_declarations
%type <unit> inv_var_decl_list
%type <unit> inv_var_decl
%type <Tag.f_tag option> formula_tag

%type <E.term list> term_list

%type <Expression.formula> formula
%type <E.V.shared_or_local> opt_th_param
%type <E.V.shared_or_local> th_param
%type <E.literal> literal
%type <E.term> term
%type <E.cell> cell
%type <E.tid> tid
%type <E.elem> elem
%type <E.addr> addr
%type <E.mem> mem
%type <E.path> path
%type <E.set> set
%type <E.setth> setth
%type <E.setint> setint
%type <E.setelem> setelem
%type <E.setpair> setpair
%type <E.integer> integer
%type <E.pair> pair
%type <E.eq> equals
%type <E.diseq> disequals
%type <E.term> arrays
%type <E.addrarr> addrarr
%type <E.tidarr> tidarr
%type <E.mark> mark
%type <E.bucket> bucket
%type <E.lock> lock

%type <PVD.t> pvd
%type <(PVD.node_id_t * E.formula) list> node_list
%type <(PVD.node_id_t * E.formula)> node
%type <PVD.node_id_t list> node_id_list
%type <(PVD.box_id_t * PVD.node_id_t list * E.ThreadSet.elt) list> box_list
%type <(PVD.box_id_t * PVD.node_id_t list * E.ThreadSet.elt)> box
%type <(int * E.V.t) list> trans_list
%type <(int * E.V.t)> trans
%type <(PVD.node_id_t * PVD.node_id_t * (PVD.edge_type_t * PVD.trans_t)) list> edge_list
%type <(PVD.node_id_t * PVD.node_id_t * (PVD.edge_type_t * PVD.trans_t))> edge
%type <(PVD.accept_triple_t) list> accept_edge_list
%type <(PVD.accept_triple_t)> accept_edge
%type <PVD.wf_op_t> wf_op
%type <(PVD.accept_triple_t list * PVD.accept_triple_t list * (E.term * PVD.wf_op_t) list) list> acceptance_list
%type <(PVD.accept_triple_t list * PVD.accept_triple_t list * (E.term * PVD.wf_op_t) list)> acceptance
%type <(E.term * PVD.wf_op_t) list> delta_list
%type <(E.term * PVD.wf_op_t)> delta



%%


/*********************     DIAGRAMS    *************************/

pvd :
  | DIAGRAM OPEN_BRACKET IDENT CLOSE_BRACKET
    NODES COLON node_list
    BOXES COLON box_list
    INITIAL COLON node_id_list
    EDGES COLON edge_list
    ACCEPTANCE COLON acceptance_list
    {
      let name = get_name $3 in
      let nodes = $7 in
      let boxes = $10 in
      let initial = $13 in
      let edges = $16 in
      let acceptance = $19 in
      PVD.new_pvd name nodes boxes initial edges acceptance
    }


node_list :
  | node
    { [$1] }
  | node COMMA node_list
    { $1 :: $3 }


node :
  | IDENT
    {
      let n = get_name $1 in
      (n, Formula.True)
    }
  | IDENT OPEN_SET formula CLOSE_SET
    { let n = get_name $1 in
      let phi = $3 in
      (n,phi)
    }


node_id_list :
  | IDENT
    { [get_name $1] }
  | IDENT COMMA node_id_list
    { (get_name $1) :: $3 }


box_list :
  |
    { [] }
  | box box_list
    { $1 :: $2 }


box :
  | OPEN_SET IDENT OPEN_BRACKET IDENT CLOSE_BRACKET COLON node_id_list CLOSE_SET
    { let box_id = get_name $2 in
      let param = E.VarTh (E.build_global_var (get_name $4) E.Tid) in
      let nodes = $7 in
      (box_id, nodes, param)
    }


trans_list :
  | trans
    { [$1] }
  | trans COMMA trans_list
    { $1 :: $3 }


trans :
  | NUMBER OPEN_PAREN IDENT CLOSE_PAREN
    {
      let i = $1 in
      let t = E.build_global_var (get_name $3) E.Tid in
      (i, t)
    }

edge_list :
  | edge
    { [$1] }
  | edge edge_list
    { $1 :: $2 }


edge :
  | OPEN_BRACKET IDENT EDGE_ARROW IDENT CLOSE_BRACKET SEMICOLON
    {
      let n1 = get_name $2 in
      let n2 = get_name $4 in
      (n1, n2, (PVD.Pres, PVD.NoLabel))
    }
  | IDENT EDGE_ARROW IDENT SEMICOLON
    {
      let n1 = get_name $1 in
      let n2 = get_name $3 in
      (n1, n2, (PVD.Any, PVD.NoLabel))
    }
  | OPEN_BRACKET IDENT EDGE_ARROW_OPEN trans_list EDGE_ARROW_CLOSE IDENT
    CLOSE_BRACKET SEMICOLON
    {
      let n1 = get_name $2 in
      let n2 = get_name $6 in
      let trans = $4 in
      (n1, n2, (PVD.Pres, PVD.Label trans))
    }
  | IDENT EDGE_ARROW_OPEN trans_list EDGE_ARROW_CLOSE IDENT SEMICOLON
    {
      let n1 = get_name $1 in
      let n2 = get_name $5 in
      let trans = $3 in
      (n1, n2, (PVD.Any, PVD.Label trans))
    }


acceptance_list :
  | acceptance
    { [$1] }
  | acceptance acceptance_list
    { $1 :: $2 }


accept_edge_list :
  |
    { [] }
  | accept_edge
    { [$1] }
  | accept_edge COMMA accept_edge_list
    { $1 :: $3 }


accept_edge :
  | OPEN_PAREN IDENT COMMA IDENT COMMA IDENT CLOSE_PAREN
    {
      let n1 = get_name $2 in
      let n2 = get_name $4 in
      let p = match (get_name $6) with
              | "any" -> PVD.Any
              | "pres" -> PVD.Pres
              | _ -> raise(Wrong_edge_acceptance_argument (get_name $6)) in
      (n1,n2,p)
    }


acceptance :
  | OPEN_ANGLE BAD COLON OPEN_SET accept_edge_list CLOSE_SET SEMICOLON
               GOOD COLON OPEN_SET accept_edge_list CLOSE_SET SEMICOLON
               OPEN_BRACKET delta_list CLOSE_BRACKET CLOSE_ANGLE
    {
      let bad = $5 in
      let good = $11 in
      let delta = $15 in
      (bad, good, delta)
    }


delta_list :
  | delta
    { [$1] }
  | delta SEMICOLON delta_list
    { $1 :: $3}


delta :
  | OPEN_PAREN term COMMA wf_op CLOSE_PAREN
    { ($2, $4) }


wf_op :
  | WF_INTSUBSET
    { PVD.WFIntSubset }
  | WF_PAIRSUBSET
    { PVD.WFPairSubset }
  | WF_ADDRSUBSET
    { PVD.WFAddrSubset }
  | WF_ELEMSUBSET
    { PVD.WFElemSubset }
  | WF_TIDSUBSET
    { PVD.WFTidSubset }
  | WF_INTLESS
    { PVD.WFIntLess }


/*********************     AXIOMS    *************************/

axiom :
  | param COLON inv_var_declarations AXIOM formula_tag COLON formula_decl_list
    { let declInvVars = System.copy_var_table invVars in
      let tag         = $5 in
      let inv_decl    = $7 in
      let _           = System.clear_table invVars
      in
        (declInvVars, tag, inv_decl)
    }



/*********************     INVARIANTS    *************************/

invariant :
  | param COLON inv_var_declarations INVARIANT formula_tag COLON formula_decl_list
    { let declInvVars = System.copy_var_table invVars in
      let tag         = $5 in
      let inv_decl    = $7 in
      let _           = System.clear_table invVars
      in
        (declInvVars, tag, inv_decl)
    }


formula_tag :
  |
    { None }
  | OPEN_BRACKET IDENT CLOSE_BRACKET
    {
      let tag_name = get_name $2 in
      curr_tag := tag_name;
      Some (Tag.new_tag tag_name "")
    }


formula_decl_list :
  | formula_decl
    { [$1] }
  | formula_decl formula_decl_list
    { $1 :: $2 }


formula_decl :
  | formula
    { (None, $1) }
  | SHARP IDENT COLON formula
    {
      let tag_name = get_name $2 in
      (Some (Tag.new_tag !curr_tag tag_name), $4)
    }


param :
  | VARS
    { }


inv_var_declarations:
  |
    { }
  | inv_var_decl_list
    { }


inv_var_decl_list:
  | inv_var_decl
    { () }
  | inv_var_decl inv_var_decl_list
    { () }


inv_var_decl:
  | IDENT IDENT
    {
      let s      = check_and_get_sort (get_name $1) in
      let v_name = get_name $2 in
      decl_inv_var v_name s None
    }


/***********************    FORMULAS    ************************/



/* FORMULAS */

formula :
  | OPEN_PAREN formula CLOSE_PAREN
      { $2 }
  | literal
      { Formula.Literal $1 }
  | LOGICAL_TRUE
      { Formula.True }
  | LOGICAL_FALSE
      { Formula.False }
  | LOGICAL_NOT formula
      { Formula.Not $2 }
  | formula LOGICAL_AND formula
      { Formula.And ($1, $3) }
  | formula LOGICAL_OR formula
      { Formula.Or ($1, $3) }
  | formula LOGICAL_THEN formula
      { Formula.Implies ($1, $3) }
  | formula EQUALS formula
      { Formula.Iff ($1, $3) }
  | AT NUMBER opt_th_param DOT
      {
        let line_num = $2 in
        let th_p     = $3 in
          E.pc_form line_num th_p false
      }
  | AT IDENT opt_th_param DOT
      {
        let label_name = get_name $2 in
        let th_p       = $3 in
        let labelTbl   = System.get_labels !Symtbl.sys in
        let pc_pos     = System.get_label_pos labelTbl label_name in
        let pc_expr    = match pc_pos with
                           None -> parser_error ("Unknown label: " ^ label_name)
                         | Some (i,e) -> if i = e then
                                           E.pc_form i th_p false
                                         else
                                           E.pcrange_form i e th_p false
        in
          pc_expr
      }


/* THREAD VARS */


opt_th_param:
  |
    { E.V.Shared }
  | th_param
    { $1 }


th_param:
  | OPEN_PAREN IDENT CLOSE_PAREN
    {
      let th_id = get_name $2 in
        E.V.Local (E.build_global_var th_id E.Tid)
    }
  | OPEN_PAREN NUMBER CLOSE_PAREN
    {
      let th_id = $2 in
        E.V.Local (E.build_num_tid_var th_id)
    }



/* LITERALS */

literal :
  | APPEND OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "append(%s,%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5)
                                                       (E.term_to_str $7) in
      let p1   = parser_check_type check_type_path $3 E.Path get_str_expr in
      let p2   = parser_check_type check_type_path $5 E.Path get_str_expr in
      let pres = parser_check_type check_type_path $7 E.Path get_str_expr in
        Formula.Atom (E.Append (p1,p2,pres))
    }
  | REACH OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "reach(%s,%s,%s,%s)" (E.term_to_str $3)
                                                         (E.term_to_str $5)
                                                         (E.term_to_str $7)
                                                         (E.term_to_str $9) in
      let h      = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a_from = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr $7 E.Addr get_str_expr in
      let p      = parser_check_type check_type_path $9 E.Path get_str_expr in
        Formula.Atom (E.Reach (h,a_from,a_to,p))
    }
  | REACH OPEN_PAREN term COMMA term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "reach(%s,%s,%s,%s,%s)" (E.term_to_str $3)
                                                            (E.term_to_str $5)
                                                            (E.term_to_str $7)
                                                            (E.term_to_str $9)
                                                            (E.term_to_str $11) in
      let h      = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a_from = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr $7 E.Addr get_str_expr in
      let p      = parser_check_type check_type_path $9 E.Path get_str_expr in
      let l      = parser_check_type check_type_int $11 E.Int get_str_expr in
        Formula.Atom (E.ReachAt (h,a_from,a_to,l,p))
    }
  | ORDERLIST OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "orderlist(%s,%s,%s)" (E.term_to_str $3)
                                                          (E.term_to_str $5)
                                                          (E.term_to_str $7) in
      let h      = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a_from = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr $7 E.Addr get_str_expr in
        Formula.Atom (E.OrderList (h,a_from,a_to))
    }
  | SKIPLIST OPEN_PAREN term COMMA term COMMA term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "skiplist(%s,%s,%s,%s,%s,%s)"
                                        (E.term_to_str $3)
                                        (E.term_to_str $5)
                                        (E.term_to_str $7)
                                        (E.term_to_str $9)
                                        (E.term_to_str $11)
                                        (E.term_to_str $13) in
      let h      = parser_check_type check_type_mem      $3 E.Mem get_str_expr in
      let s      = parser_check_type check_type_set      $5 E.Set get_str_expr in
      let l      = parser_check_type check_type_int      $7 E.Int get_str_expr in
      let a_from = parser_check_type check_type_addr     $9 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr    $11 E.Addr get_str_expr in
      let elems  = parser_check_type check_type_setelem $13 E.SetElem get_str_expr in
        Formula.Atom (E.Skiplist (h,s,l,a_from,a_to,elems))
    }
  | HASHTBL OPEN_PAREN term COMMA term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "hashtbl(%s,%s,%s,%s,%s)"
                                        (E.term_to_str $3)
                                        (E.term_to_str $5)
                                        (E.term_to_str $7)
                                        (E.term_to_str $9)
                                        (E.term_to_str $11) in
      let h  = parser_check_type check_type_mem        $3 E.Mem get_str_expr in
      let s  = parser_check_type check_type_set        $5 E.Set get_str_expr in
      let se = parser_check_type check_type_setelem    $7 E.SetElem get_str_expr in
      let bb = parser_check_type check_type_bucketarr  $9 E.BucketArray get_str_expr in
      let i  = parser_check_type check_type_int       $11 E.Int get_str_expr in
        Formula.Atom (E.Hashtbl (h,s,se,bb,i))
    }
  | IN OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "in(%s,%s)" (E.term_to_str $3)
                                                (E.term_to_str $5) in
      let a = parser_check_type check_type_addr $3 E.Addr get_str_expr in
      let r = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Formula.Atom (E.In (a,r))
    }
  | SUBSETEQ OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "subseteq(%s,%s)" (E.term_to_str $3)
                                                      (E.term_to_str $5) in
      let s = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let r = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Formula.Atom (E.SubsetEq(s,r))
    }
  | INTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "tin(%s,%s)" (E.term_to_str $3)
                                                 (E.term_to_str $5) in
      let th = parser_check_type check_type_tid  $3 E.Tid get_str_expr in
      let s  = parser_check_type check_type_setth $5 E.SetTh get_str_expr in
        Formula.Atom (E.InTh (th,s))
    }
  | SUBSETEQTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "tsubseteq(%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5) in
      let r = parser_check_type check_type_setth $3 E.SetTh get_str_expr in
      let s = parser_check_type check_type_setth $5 E.SetTh get_str_expr in
        Formula.Atom (E.SubsetEqTh(r,s))
    }
  | ININT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "iin(%s,%s)" (E.term_to_str $3)
                                                 (E.term_to_str $5) in
      let i = parser_check_type check_type_int $3 E.Int get_str_expr in
      let s = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Formula.Atom (E.InInt (i,s))
    }
  | SUBSETEQINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "isubseteq(%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5) in
      let r = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let s = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Formula.Atom (E.SubsetEqInt(r,s))
    }
  | INELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "ein(%s,%s)" (E.term_to_str $3)
                                                 (E.term_to_str $5) in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let s = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Formula.Atom (E.InElem (e,s))
    }
  | SUBSETEQELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "esubseteq(%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5) in
      let r = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Formula.Atom (E.SubsetEqElem(r,s))
    }
  | SETPAIRIN OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spin(%s,%s)" (E.term_to_str $3)
                                                  (E.term_to_str $5) in
      let p = parser_check_type check_type_pair $3 E.Pair get_str_expr in
      let s = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Formula.Atom (E.InPair (p,s))
    }
  | SETPAIRSUBSETEQ OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spsubseteq(%s,%s)" (E.term_to_str $3)
                                                        (E.term_to_str $5) in
      let r = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
      let s = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Formula.Atom (E.SubsetEqPair(r,s))
    }


  | SETPAIRINTIDPAIR OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "inintpair(%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5) in
      let t = parser_check_type check_type_tid $3 E.Tid get_str_expr in
      let s = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Formula.Atom (E.InTidPair(t,s))
    }
  | SETPAIRININTPAIR OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "inintpair(%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5) in
      let i = parser_check_type check_type_int $3 E.Int get_str_expr in
      let s = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Formula.Atom (E.InIntPair(i,s))
    }
  | SETPAIRUNIQUETID OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "uniquetid(%s)" (E.term_to_str $3) in
      let s = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
        Formula.Atom (E.UniqueTid(s))
    }
  | SETPAIRUNIQUEINT OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "uniqueint(%s)" (E.term_to_str $3) in
      let s = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
        Formula.Atom (E.UniqueInt(s))
    }
  | term MATH_LESS term
    {
      let get_str_expr () = sprintf "%s < %s" (E.term_to_str $1)
                                              (E.term_to_str $3) in
      try
        let e1 = parser_check_type check_type_elem $1 E.Elem get_str_expr in
        let e2 = parser_check_type check_type_elem $3 E.Elem get_str_expr in
          Formula.Atom (E.LessElem (e1, e2))
      with
        _ -> let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
             let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
               Formula.Atom (E.Less (i1, i2))
    }
  | term MATH_GREATER term
    {
      let get_str_expr () = sprintf "%s > %s" (E.term_to_str $1)
                                              (E.term_to_str $3) in
      try
        let e1 = parser_check_type check_type_elem $1 E.Elem get_str_expr in
        let e2 = parser_check_type check_type_elem $3 E.Elem get_str_expr in
          Formula.Atom (E.GreaterElem (e1, e2))
      with _ -> let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
                let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
                  Formula.Atom (E.Greater (i1, i2))
    }
  | term MATH_LESS_EQ term
    {
      let get_str_expr () = sprintf "%s <= %s" (E.term_to_str $1)
                                               (E.term_to_str $3) in
      let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
        Formula.Atom (E.LessEq (i1, i2))
    }
  | term MATH_GREATER_EQ term
    {
      let get_str_expr () = sprintf "%s >= %s" (E.term_to_str $1)
                                               (E.term_to_str $3) in
      let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
        Formula.Atom (E.GreaterEq (i1, i2))
    }
  | equals
    { Formula.Atom (E.Eq($1)) }
  | disequals
    { Formula.Atom (E.InEq($1)) }
  | DOT ident DOT
    {
      match $2 with
      | E.VarT v -> Formula.Atom(E.BoolVar v)
      | _           -> raise(Boolean_var_expected $2)
    }
  | DOT IDENT DOUBLECOLON IDENT th_param DOT
    {
      let p = get_name $2 in
      let proc_name = E.V.Scope p in
      let id = get_name $4 in
      let th = $5 in

      check_is_procedure p;
      check_var_belongs_to_procedure id p;
      let proc_info = System.get_proc_by_name !Symtbl.sys p in
      let iVars     = System.proc_info_get_input proc_info in
      let lVars     = System.proc_info_get_local proc_info in
      let k = if System.mem_var iVars id then
                System.find_var_kind iVars id
              else
                System.find_var_kind lVars id
              in
      let v = E.build_var id E.Bool false th proc_name ~nature:k in
      Formula.Atom(E.BoolVar v)
    }



/* EQUALS */

equals :
  | term EQUALS term
    {
      let get_str_expr () = sprintf "%s = %s" (E.term_to_str $1)
                                              (E.term_to_str $3) in
      let t1 = $1 in
      let t2 = $3 in

      parser_check_compatibility t1 t2 get_str_expr ;
      (inject_sort t1, inject_sort t2)
    }


/* DISEQUALS */

disequals :
  | term NOT_EQUALS term
    {
      let get_str_expr () = sprintf "%s != %s" (E.term_to_str $1)
                                               (E.term_to_str $3) in
      let t1= $1 in
      let t2= $3 in

      parser_check_compatibility t1 t2 get_str_expr ;
      (inject_sort t1, inject_sort t2)
    }


/* TERMS */

term :
  | ident
    { $1 }
  | set
    { E.SetT($1) }
  | elem
    { E.ElemT($1) }
  | tid
    { E.TidT($1) }
  | addr
    { E.AddrT($1) }
  | cell
    { E.CellT($1) }
  | setth
    { E.SetThT($1) }
  | setint
    { E.SetIntT($1) }
  | setelem
    { E.SetElemT($1) }
  | setpair
    { E.SetPairT($1) }
  | path
    { E.PathT($1) }
  | mem
    { E.MemT($1) }
  | integer
    { E.IntT($1) }
  | pair
    { E.PairT($1) }
  | arrays
    { $1 }
  | addrarr
    { E.AddrArrayT($1) }
  | tidarr
    { E.TidArrayT($1) }
  | mark
    { E.MarkT($1) }
  | bucket
    { E.BucketT($1) }
  | lock
    { E.LockT($1) }
  | OPEN_PAREN term CLOSE_PAREN
    { $2 }



/* IDENT */

ident :
  IDENT
    {
      define_ident E.V.GlobalScope (get_name $1) E.V.Shared
    }
  | IDENT DOUBLECOLON IDENT
    {
      define_ident (E.V.Scope (get_name $1)) (get_name $3) E.V.Shared
    }
  | IDENT DOUBLECOLON IDENT th_param
    {
      define_ident (E.V.Scope (get_name $1)) (get_name $3) $4
    }


/* SET terms*/

set :
  | EMPTYSET
    { E.EmptySet }
  | OPEN_SET term CLOSE_SET
    {
      let get_str_expr() = sprintf "{ %s }" (E.term_to_str $2) in
      let a = parser_check_type check_type_addr $2 E.Addr get_str_expr in
        E.Singl(a)
    }
  | UNION OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "union(%s,%s)" (E.term_to_str $3)
                                                  (E.term_to_str $5) in
      let s1 = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let s2 = parser_check_type check_type_set  $5 E.Set get_str_expr in
        E.Union(s1,s2)
    }
  | INTR OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "intr(%s,%s)" (E.term_to_str $3)
                                                 (E.term_to_str $5) in
      let s1 = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let s2 = parser_check_type check_type_set  $5 E.Set get_str_expr in
        E.Intr(s1,s2)
    }
  | SETDIFF OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "diff(%s,%s)" (E.term_to_str $3)
                                                    (E.term_to_str $5) in
      let s1 = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let s2 = parser_check_type check_type_set  $5 E.Set get_str_expr in
        E.Setdiff(s1,s2)
    }
  | PATH2SET OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "path2set(%s)" (E.term_to_str $3) in
      let p = parser_check_type check_type_path $3 E.Path get_str_expr in
        E.PathToSet(p)
    }
  | ADDR2SET OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "addr2set(%s,%s)" (E.term_to_str $3)
                                                      (E.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
        E.AddrToSet(h,a)
    }
  | ADDR2SET OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "addr2set(%s,%s,%s)" (E.term_to_str $3)
                                                         (E.term_to_str $5)
                                                         (E.term_to_str $7) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let l = parser_check_type check_type_int $7 E.Int get_str_expr in
        E.AddrToSetAt(h,a,l)
    }
  | term DOT BREGION
    {
      let get_str_expr () = sprintf "%s.bregion" (E.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        E.BucketRegion(b)
    }



/* ELEM terms */

elem :
  | term DOT DATA
    {
      let get_str_expr () = sprintf "%s.data" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell  $1 E.Cell get_str_expr in
        E.CellData(c)
    }
  | LOWEST_ELEM
    {
      E.LowestElem
    }
  | HIGHEST_ELEM
    {
      E.HighestElem
    }


/* THID terms */

tid :
  | term DOT LOCKID
    {
      let get_str_expr () = sprintf "%s.lockid" (E.term_to_str $1) in
      try
        let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
          E.CellLockId(c)
      with _ ->
        let l = parser_check_type check_type_lock $1 E.Lock get_str_expr in
          E.LockId(l)
    }
  | SHARP
    { E.NoTid }
  | TIDOF OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "tid_of(%s)" (E.term_to_str $3) in
      let p = parser_check_type check_type_pair $3 E.Pair get_str_expr in
        E.PairTid(p)
    }
  | term DOT BTID
    {
      let get_str_expr () = sprintf "%s.btid" (E.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        E.BucketTid(b)
    }



/* ADDR terms */

addr :
  | NULL
    { E.Null }
  | term DOT NEXT
    {
      let get_str_expr () = sprintf "%s.next" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell  $1 E.Cell get_str_expr in
        E.Next(c)
    }
  | term DOT NEXTAT OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s.nextat[%s]" (E.term_to_str $1)
                                                    (E.term_to_str $5) in
      let c = parser_check_type check_type_cell  $1 E.Cell get_str_expr in
      let l = parser_check_type check_type_int   $5 E.Cell get_str_expr in
        E.NextAt(c,l)
    }

  | FIRSTLOCKED OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "firstlocked(%s,%s)" (E.term_to_str $3)
                                                         (E.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let p = parser_check_type check_type_path $5 E.Path get_str_expr in
        E.FirstLocked(h,p)
    }
  | FIRSTLOCKED OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "firstlocked(%s,%s,%s)"
                                          (E.term_to_str $3)
                                          (E.term_to_str $5)
                                          (E.term_to_str $7) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let p = parser_check_type check_type_path $5 E.Path get_str_expr in
      let l = parser_check_type check_type_int $7 E.Int get_str_expr in
        E.FirstLockedAt(h,p,l)
    }
  | LASTLOCKED OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "lastlocked(%s,%s)" (E.term_to_str $3)
                                                        (E.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let p = parser_check_type check_type_path $5 E.Path get_str_expr in
        E.LastLocked(h,p)
    }
  | term DOT BINIT
    {
      let get_str_expr () = sprintf "%s.binit" (E.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        E.BucketInit(b)
    }
  | term DOT BEND
    {
      let get_str_expr () = sprintf "%s.bend" (E.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        E.BucketEnd(b)
    }


/* CELL terms */

cell :
  | ERROR
    { E.Error }
  | MKCELL OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mkcell(%s,%s,%s)"
                                           (E.term_to_str $3)
                                           (E.term_to_str $5)
                                           (E.term_to_str $7) in
      let d  = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let a  = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let th = parser_check_type check_type_tid $7 E.Tid get_str_expr in
        E.MkCell(d,a,th)
    }
  | MKCELL OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mkcell(%s,%s,%s,%s)"
                                           (E.term_to_str $3)
                                           (E.term_to_str $5)
                                           (E.term_to_str $7)
                                           (E.term_to_str $9) in
      let e  = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let aa = parser_check_type check_type_addrarr $5 E.AddrArray get_str_expr in
      let ta = parser_check_type check_type_tidarr $7 E.TidArray get_str_expr in
      let l  = parser_check_type check_type_int $9 E.Int get_str_expr in
        E.MkSLCell(e,aa,ta,l)
    }
  | MKCELL OPEN_PAREN term COMMA
                      OPEN_BRACKET term_list CLOSE_BRACKET COMMA
                      OPEN_BRACKET term_list CLOSE_BRACKET CLOSE_PAREN
    {
      let list_term_to_str ts = String.concat "," (List.map E.term_to_str ts) in
      let addrs_str = list_term_to_str $6 in
      let tids_str = list_term_to_str $10 in
      let get_str_expr () = sprintf "mkcell(%s,[%s],[%s])"
                                           (E.term_to_str $3)
                                           (addrs_str)
                                           (tids_str) in
      let e  = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let addrs = List.map (fun a ->
                    parser_check_type check_type_addr a E.Addr get_str_expr
                  ) $6 in
      let tids = List.map (fun t ->
                   parser_check_type check_type_tid t E.Tid get_str_expr
                 ) $10 in
      if List.length addrs <> List.length tids then
        begin
          Interface.Err.msg "Different argument lengths" $
            sprintf "mkcell is invoked with an unequal number of addresses [%s] \
                     and thread ids [%s]." addrs_str tids_str;
          raise(Different_argument_length(addrs_str,tids_str))
        end
      else
        E.MkSLKCell(e,addrs,tids)
    }
  | term DOT LOCK OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "%s.lock(%s)" (E.term_to_str $1)
                                                  (E.term_to_str $5) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
      let t = parser_check_type check_type_tid $5 E.Tid get_str_expr in
        E.CellLock(c,t)
    }
  | term DOT LOCKAT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "%s.lock(%s)" (E.term_to_str $1)
                                                  (E.term_to_str $5) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
      let l = parser_check_type check_type_int  $5 E.Int get_str_expr in
      let t = parser_check_type check_type_tid $7 E.Tid get_str_expr in
        E.CellLockAt(c,l,t)
    }
  | term DOT UNLOCK
    {
      let get_str_expr () = sprintf "%s.unlock" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
        E.CellUnlock(c)
    }
  | term DOT UNLOCKAT OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "%s.unlock" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
      let l = parser_check_type check_type_int  $5 E.Int get_str_expr in
        E.CellUnlockAt(c,l)
    }
  | MEMORY_READ OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "%s [ %s ]" (E.term_to_str $3)
                                                (E.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
        E.CellAt(h,a)
    }


term_list :
  | term COMMA term
    { [$1;$3] }
  | term COMMA term_list
    { $1 :: $3 }



/* SETTH terms*/

setth :
  | EMPTYSETTH
  { E.EmptySetTh }
  | SINGLETH OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tsingle(%s)" (E.term_to_str $3) in
      let th = parser_check_type check_type_tid  $3 E.Tid get_str_expr in
        E.SinglTh(th)
    }
  | UNIONTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tunion(%s,%s)" (E.term_to_str $3)
                                                    (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setth  $3 E.SetTh get_str_expr in
      let s2 = parser_check_type check_type_setth  $5 E.SetTh get_str_expr in
        E.UnionTh(s1,s2)
    }
  | INTRTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tintr(%s,%s)" (E.term_to_str $3)
                                                   (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setth  $3 E.SetTh get_str_expr in
      let s2 = parser_check_type check_type_setth  $5 E.SetTh get_str_expr in
        E.IntrTh(s1,s2)
    }
  | SETDIFFTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tdiff(%s,%s)" (E.term_to_str $3)
                                                      (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setth  $3 E.SetTh get_str_expr in
      let s2 = parser_check_type check_type_setth  $5 E.SetTh get_str_expr in
        E.SetdiffTh(s1,s2)
    }
  | LOCKSET OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "lockset(%s,%s)" (E.term_to_str $3)
                                                    (E.term_to_str $5) in
      let m = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let p = parser_check_type check_type_path $5 E.Path get_str_expr in
        E.LockSet(m,p)
    }


/* SETINT terms*/
setint :
  | EMPTYSETINT
     { E.EmptySetInt }
  | SINGLEINT OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "isingle(%s)" (E.term_to_str $3) in
      let th = parser_check_type check_type_int $3 E.Int get_str_expr in
        E.SinglInt(th)
    }
  | UNIONINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "iunion(%s,%s)" (E.term_to_str $3)
                                                     (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setint  $3 E.SetInt get_str_expr in
      let s2 = parser_check_type check_type_setint  $5 E.SetInt get_str_expr in
        E.UnionInt(s1,s2)
    }
  | INTRINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "iintr(%s,%s)" (E.term_to_str $3)
                                                    (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setint  $3 E.SetInt get_str_expr in
      let s2 = parser_check_type check_type_setint  $5 E.SetInt get_str_expr in
        E.IntrInt(s1,s2)
    }
  | SETDIFFINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "idiff(%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let s2 = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        E.SetdiffInt(s1,s2)
    }
  | SETLOWER OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "setLower(%s,%s)" (E.term_to_str $3)
                                                     (E.term_to_str $5) in
      let s = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let i = parser_check_type check_type_int $5 E.Int get_str_expr in
        E.SetLower(s,i)
    }


/* SETPAIR terms*/
setpair :
  | SETPAIREMPTY
     { E.EmptySetPair }
  | SETPAIRSINGLE OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spsingle(%s)" (E.term_to_str $3) in
      let p = parser_check_type check_type_pair $3 E.Pair get_str_expr in
        E.SinglPair(p)
    }
  | SETPAIRUNION OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spunion(%s,%s)" (E.term_to_str $3)
                                                    (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setpair  $3 E.SetPair get_str_expr in
      let s2 = parser_check_type check_type_setpair  $5 E.SetPair get_str_expr in
        E.UnionPair(s1,s2)
    }
  | SETPAIRINTR OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spintr(%s,%s)" (E.term_to_str $3)
                                                   (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setpair  $3 E.SetPair get_str_expr in
      let s2 = parser_check_type check_type_setpair  $5 E.SetPair get_str_expr in
        E.IntrPair(s1,s2)
    }
  | SETPAIRDIFF OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spdiff(%s,%s)" (E.term_to_str $3)
                                                   (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
      let s2 = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        E.SetdiffPair(s1,s2)
    }
  | SETPAIRLOWER OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "splower(%s,%s)" (E.term_to_str $3)
                                                    (E.term_to_str $5) in
      let s = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
      let i = parser_check_type check_type_int $5 E.Int get_str_expr in
        E.LowerPair(s,i)
    }


/* SETELEM terms*/
setelem :
  | EMPTYSETELEM
     { E.EmptySetElem }
  | SINGLEELEM OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "esingle(%s)" (E.term_to_str $3) in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
        E.SinglElem(e)
    }
  | UNIONELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "eunion(%s,%s)" (E.term_to_str $3)
                                                      (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s2 = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        E.UnionElem(s1,s2)
    }
  | INTRELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "eintr(%s,%s)" (E.term_to_str $3)
                                                     (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s2 = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        E.IntrElem(s1,s2)
    }
  | SETDIFFELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "ediff(%s,%s)" (E.term_to_str $3)
                                                        (E.term_to_str $5) in
      let s1 = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s2 = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        E.SetdiffElem(s1,s2)
    }
  | SET2ELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "set2elem(%s,%s)" (E.term_to_str $3)
                                                      (E.term_to_str $5) in
      let m = parser_check_type check_type_mem $3 E.Mem get_str_expr in
      let s = parser_check_type check_type_set $5 E.Set get_str_expr in
        E.SetToElems(s,m)
    }


/* PATH terms */

path :
  | EPSILON
    { E.Epsilon }
  | OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "[ %s ]" (E.term_to_str $2) in
      let a = parser_check_type check_type_addr $2 E.Addr get_str_expr in
        E.SimplePath(a)
    }
  | GETP OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "getp(%s,%s,%s)" (E.term_to_str $3)
                                                     (E.term_to_str $5)
                                                     (E.term_to_str $7) in
      let h     = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let first = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let last  = parser_check_type check_type_addr $7 E.Addr get_str_expr in
        E.GetPath(h,first,last)
    }
  | GETP OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "getp(%s,%s,%s,%s)" (E.term_to_str $3)
                                                        (E.term_to_str $5)
                                                        (E.term_to_str $7)
                                                        (E.term_to_str $9) in
      let h     = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let first = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let last  = parser_check_type check_type_addr $7 E.Addr get_str_expr in
      let l     = parser_check_type check_type_int $9 E.Int get_str_expr in
        E.GetPathAt(h,first,last,l)
  }


/* MEM terms */

mem :
  | UPDATE OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "update(%s,%s,%s)" (E.term_to_str $3)
                                                       (E.term_to_str $5)
                                                       (E.term_to_str $7) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let c = parser_check_type check_type_cell $7 E.Cell get_str_expr in
        E.Update(h,a,c)
    }


/* INTEGER terms*/

integer :
  | NUMBER
    { E.IntVal $1 }
  | MATH_MINUS term %prec MATH_NEG
    {
      let get_str_expr () = sprintf "-%s" (E.term_to_str $2) in
      let i  = parser_check_type check_type_int $2 E.Int get_str_expr in
        E.IntNeg i
    }
  | term MATH_PLUS term
    {
      let get_str_expr () = sprintf "%s+%s" (E.term_to_str $1)
                                            (E.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        E.IntAdd (i1,i2)
    }
  | term MATH_MINUS term
    {
      let get_str_expr () = sprintf "%s-%s" (E.term_to_str $1)
                                            (E.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        E.IntSub (i1,i2)
    }
  | term MATH_MULT term
    {
      let get_str_expr () = sprintf "%s*%s" (E.term_to_str $1)
                                            (E.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        E.IntMul (i1,i2)
    }
  | term MATH_DIV term
    {
      let get_str_expr () = sprintf "%s/%s" (E.term_to_str $1)
                                            (E.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        E.IntDiv (i1,i2)
    }
  | term MATH_MOD term
    {
      let get_str_expr () = sprintf "%s %% %s" (E.term_to_str $1)
                                               (E.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        E.IntMod (i1,i2)
    }
  | SETINTMIN OPEN_PAREN term CLOSE_PAREN
    {
      let iSet = $3 in
      let get_str_expr () = sprintf "setIntMin(%s)" (E.term_to_str iSet) in
      let s  = parser_check_type check_type_setint iSet E.SetInt get_str_expr
      in
        E.IntSetMin (s)
    }
  | SETINTMAX OPEN_PAREN term CLOSE_PAREN
    {
      let iSet = $3 in
      let get_str_expr () = sprintf "setIntMax(%s)" (E.term_to_str iSet) in
      let s  = parser_check_type check_type_setint iSet E.SetInt get_str_expr
      in
        E.IntSetMax (s)
    }
  | term DOT MAX
    {
      let get_str_expr () = sprintf "%s.max" (E.term_to_str $1) in
      let c  = parser_check_type check_type_cell $1 E.Cell get_str_expr
      in
        E.CellMax (c)
    }
  | INTOF OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "int_of(%s)" (E.term_to_str $3) in
      let p  = parser_check_type check_type_pair $3 E.Pair get_str_expr
      in
        E.PairInt (p)
    }


/* PAIR terms*/

pair :
  | OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "(%s,%s)" (E.term_to_str $2)
                                              (E.term_to_str $4) in
      let i  = parser_check_type check_type_int $2 E.Int get_str_expr in
      let t  = parser_check_type check_type_tid $4 E.Tid get_str_expr in
        E.IntTidPair(i,t)
    }
  | SETPAIRMIN OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spmin(%s)" (E.term_to_str $3) in
      let s  = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
        E.SetPairMin(s)
    }
  | SETPAIRMAX OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spmax(%s)" (E.term_to_str $3) in
      let s  = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
        E.SetPairMax(s)
    }


/* ARRAY terms */
arrays :
  | term OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s[%s]" (E.term_to_str $1)
                                             (E.term_to_str $3) in
      let i = parser_check_type check_type_int $3 E.Int get_str_expr in
      try
        let at = parser_check_type check_type_tidarr $1 E.TidArray get_str_expr in
          E.TidT (E.TidArrRd (at,i))
      with _ ->
        try
          let aa = parser_check_type check_type_addrarr $1 E.AddrArray get_str_expr in
            E.AddrT (E.AddrArrRd (aa,i))
        with e ->
          try
            let bb = parser_check_type check_type_bucketarr $1 E.BucketArray get_str_expr in
              E.BucketT (E.BucketArrRd (bb,i))
            with e ->
              try
                let ll = parser_check_type check_type_lockarr $1 E.LockArray get_str_expr in
                  E.LockT (E.LockArrRd (ll,i))
              with e ->
                try
                  let t = parser_check_type check_type_tid $1 E.Tid get_str_expr in
                  match t with
                  | E.CellLockId c -> E.TidT (E.CellLockIdAt (c,i))
                  | _                 -> raise(e)
                with e ->
                  let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
                  match a with
                  | E.Next c -> E.AddrT (E.ArrAt (c,i))
                  | _           -> raise(e)
    }
  | ARR_UPDATE OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "arrUpd (%s,%s,%s)" (E.term_to_str $3)
                                                        (E.term_to_str $5)
                                                        (E.term_to_str $7) in
      let i = parser_check_type check_type_int $5 E.Int get_str_expr in
      try
        let ll = parser_check_type check_type_lockarr $3 E.LockArray get_str_expr in
        let l = parser_check_type check_type_lock $7 E.Lock get_str_expr in
          E.LockArrayT (E.LockArrayUp (ll,i,l))
      with _ ->
        try
          let bb = parser_check_type check_type_bucketarr $3 E.BucketArray get_str_expr in
          let b = parser_check_type check_type_bucket $7 E.Bucket get_str_expr in
            E.BucketArrayT (E.BucketArrayUp (bb,i,b))
        with _ ->
          try
            let aa = parser_check_type check_type_addrarr $3 E.AddrArray get_str_expr in
            let a = parser_check_type check_type_addr $7 E.Addr get_str_expr in
              E.AddrArrayT (E.AddrArrayUp (aa,i,a))
          with _ ->
            let at = parser_check_type check_type_tidarr $3 E.TidArray get_str_expr in
            let t = parser_check_type check_type_tid $7 E.Tid get_str_expr in
              E.TidArrayT (E.TidArrayUp (at,i,t))
    }


/* ADDRARR term */
addrarr :
  | term DOT ARR
    {
      let get_str_expr () = sprintf "%s.arr" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
        E.CellArr(c)
    }


/* TIDARR term */
tidarr :
  | term DOT TIDS
    {
      let get_str_expr () = sprintf "%s.tids" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
        E.CellTids(c)
    }


/* MARK term */
mark :
  | MARK_T
    {
      E.MarkTrue
    }
  | MARK_F
    {
      E.MarkFalse
    }
  | term DOT MARKED
    {
      let get_str_expr () = sprintf "%s.marked" (E.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
        E.Marked(c)
    }


/* BUCKET term */
bucket :
  | MKBUCKET OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mkbucket(%s,%s,%s,%s)"
                                           (E.term_to_str $3)
                                           (E.term_to_str $5)
                                           (E.term_to_str $7)
                                           (E.term_to_str $9) in
      let i = parser_check_type check_type_addr $3 E.Addr get_str_expr in
      let e = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let s = parser_check_type check_type_set $7 E.Set get_str_expr in
      let t = parser_check_type check_type_tid $9 E.Tid get_str_expr in
        E.MkBucket(i,e,s,t)
    }


/* LOCK term */
lock :
  | LOCK OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "lock(%s,%s)"
                                           (E.term_to_str $3)
                                           (E.term_to_str $5) in
      let l = parser_check_type check_type_lock $3 E.Lock get_str_expr in
      let t = parser_check_type check_type_tid $5 E.Tid get_str_expr in
        E.LLock(l,t)
    }
  | UNLOCK OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "unlock(%s)"
                                           (E.term_to_str $3) in
      let l = parser_check_type check_type_lock $3 E.Lock get_str_expr in
        E.LUnlock(l)
    }


/************************   TEMPORARY VERIFICATION CONDITIONS **********************/


vc_info :
  | param COLON inv_var_declarations
    SUPPORT COLON formula_list
    TID_CONSTRAINT COLON tid_constraint_list
    RHO COLON formula
    GOAL COLON formula
    TRANSITION_TID COLON term
    LINE COLON NUMBER
      {
        let _ = System.clear_table invVars in
        let supp_list = $6 in
        let tid_const = $9 in
        let rho_phi = $12 in
        let goal_phi = $15 in
        let trans_tid = parser_check_type check_type_tid $18 E.Tid (fun _ -> (E.term_to_str $18)) in
        let (tid_eqs, tid_ineqs, voc_const) =
          List.fold_left (fun (eqs,ineqs,voc) a ->
                            match a with
                            | E.Eq (E.TidT t1, E.TidT t2) ->
                                let t_voc = E.ThreadSet.union (E.voc_tid t1) (E.voc_tid t2) in
                                ((t1,t2)::eqs, ineqs, E.ThreadSet.union t_voc voc)
                            | E.InEq (E.TidT t1, E.TidT t2) ->
                                let t_voc = E.ThreadSet.union (E.voc_tid t1) (E.voc_tid t2) in
                                (eqs,(t1,t2)::ineqs, E.ThreadSet.union t_voc voc)
                            | _ -> (eqs, ineqs, voc)
                         ) ([],[], E.ThreadSet.empty) tid_const in
        let tid_constraint = Tactics.new_tid_constraint tid_eqs tid_ineqs in

        let line = $21 in
        let vocab = E.ThreadSet.union (E.voc (Formula.conj_list [rho_phi;goal_phi]))
                                      voc_const in
        Tactics.create_vc_info supp_list tid_constraint rho_phi goal_phi vocab trans_tid line
      }

formula_list :
  |
    { [] }
  | formula formula_list
    { $1 :: $2 }


tid_constraint_list :
  |
    { [] }
  | tid_constraint SEMICOLON tid_constraint_list
    { $1 :: $3 }


tid_constraint :
  | equals
    {
      let (t1,t2) = $1 in
      let get_str_expr () = sprintf "%s = %s" (E.term_to_str t1)
                                              (E.term_to_str t2) in
      let t1' = parser_check_type check_type_tid t1 E.Tid get_str_expr in
      let t2' = parser_check_type check_type_tid t2 E.Tid get_str_expr
      in
        E.Eq (E.TidT t1', E.TidT t2')
    }
  | disequals
    {
      let (t1,t2) = $1 in
      let get_str_expr () = sprintf "%s != %s" (E.term_to_str t1)
                                               (E.term_to_str t2) in
      let t1' = parser_check_type check_type_tid t1 E.Tid get_str_expr in
      let t2' = parser_check_type check_type_tid t2 E.Tid get_str_expr
      in
        E.InEq (E.TidT t1', E.TidT t2')
    }


/************************   STRAIGHT FORMULAS   **********************/


single_formula :
  | param COLON inv_var_declarations
    FORMULA COLON formula
      {
        let _ = System.clear_table invVars in
        let phi = $6 in
        phi
      }
