
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

module E = Expression
module Sys  = System
module Stm  = Statement
module F = Formula


type cond_op_t =
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | In
  | SubsetEq
  | InTh
  | SubsetEqTh
  | InInt
  | SubsetEqInt
  | InElem
  | SubsetEqElem
  | InPair
  | SubsetEqPair


exception WrongType of Stm.term
exception Sort_mismatch of E.V.id * E.sort * E.sort
exception Not_sort_name of string
exception Duplicated_local_var of E.V.id * E.sort
exception No_main
exception No_valid_main
exception Unknown_procedure of string
exception Variable_not_in_procedure of E.V.id * string
exception Wrong_assignment of Stm.term
exception Atomic_double_assignment of Stm.expr_t
exception Unexpected_statement of string
exception Ghost_var_in_global_decl
              of E.V.id * E.sort * E.initVal_t option * E.var_nature
exception Ghost_var_in_local_decl
              of E.V.id * E.sort * E.initVal_t option * E.var_nature
exception Ghost_vars_in_assignment of Stm.term list
exception Normal_vars_in_ghost_assignment of Stm.term list
exception Procedure_args_mismatch of string
exception Incompatible_call_sort of Stm.term * string
exception Incompatible_return_sort of string
exception Different_argument_length of string * string


(* Temporal variable tables for input and local variables *)
let globalVars = System.empty_var_table ()
let inputVars = System.empty_var_table ()
let localVars = System.empty_var_table ()
let invVars = System.empty_var_table ()

let transitions = System.new_tran_table

let labelTbl : (string, (E.pc_t * E.pc_t)) Hashtbl.t =
  Hashtbl.create System.initLabelNum

let procedures : (string * System.proc_info_t) list ref = ref []

let current_proc : string ref = ref ""

let call_points : (string, E.pc_t) Hashtbl.t = Hashtbl.create 5

let return_points : (string, E.pc_t) Hashtbl.t = Hashtbl.create 5

let flag_parsingInv : bool ref = ref false

let flag_parsingDia : bool ref = ref false

let undefTids : E.V.id list ref = ref []

let cond_stm_list : (E.pc_t list) ref = ref []


(* Position and jump management for procedures *)
let pos : int ref = ref 1
let pos_st : (E.pc_t, string * Statement.statement_t) Hashtbl.t =
  Hashtbl.create 400




(* Variable declaration functions *)

let decl_global_var (v:E.V.id)
                    (s:E.sort)
                    (e:E.initVal_t option)
                    (k:E.var_nature) : unit =
  let cond = Option.lift (E.get_initVal_restriction) e in
  let _ = match k with
            E.RealVar -> let ghosts = Option.map_default
                                          (E.var_kind E.GhostVar) [] cond
                            in
                            if ghosts <> [] then
                              begin
                                Interface.Err.msg "Ghost variable used in \
                                                   non-ghost declaration" $
                                sprintf "Global variable \"%s\" of sort \"%s\" \
                                         is assigned in its declaration to \
                                         expression \"%s\", which contains \
                                         ghost variables: %s."
                                 (v)
                                 (E.sort_to_str s)
                                 (Option.map_default E.expr_to_str "" cond)
                                 (String.concat ", " $
                                     List.map E.term_to_str ghosts);
                                raise(Ghost_var_in_global_decl(v,s,e,k))
                              end
           | E.GhostVar -> ()
  in
  System.add_var globalVars v s e E.V.Shared k


let decl_input_var (v:E.V.id)
                   (s:E.sort)
                   (e:E.initVal_t option) : unit =
  System.add_var inputVars v s e E.V.Shared E.RealVar


let decl_local_var (v:E.V.id)
                   (s:E.sort)
                   (e:E.initVal_t option)
                   (k:E.var_nature) : unit =
  let cond = Option.lift (E.get_initVal_restriction) e in
  if System.mem_var inputVars v then
    begin
      Interface.Err.msg "Input and local variables conflict" $
              sprintf "Variable \"%s\" of sort %s cannot be defined as local \
                       since its name conflicts with a procedure input \
                       variable." v (E.sort_to_str s);
      raise(Duplicated_local_var(v, s))
    end
  else
    begin
    let _ = match k with
              E.RealVar -> let ghosts = Option.map_default
                                            (E.var_kind E.GhostVar) [] cond
                             in
                             if ghosts <> [] then
                               begin
                                 Interface.Err.msg "Ghost variable used in \
                                                    non-ghost declaration" $
                                 sprintf "Local variable \"%s\" of sort \
                                          \"%s\" is assigned in its \
                                          declaration to expression \"%s\", \
                                          which contains ghost variables: \
                                          %s."
                                   (v)
                                   (E.sort_to_str s)
                                   (Option.map_default E.expr_to_str "" cond)
                                   (String.concat ", " $
                                       List.map E.term_to_str ghosts);
                                 raise(Ghost_var_in_local_decl(v,s,e,k))
                              end
            | E.GhostVar  -> ()
    in
      System.add_var localVars v s e E.V.Shared k
    end




let decl_inv_var (v:E.V.id) (s:E.sort) (e:E.initVal_t option) : unit =
  System.add_var invVars v s e E.V.Shared E.RealVar


let get_sort_from_tables (stm_t:Stm.term)
                         (inpTbl:Sys.var_table_t)
                         (locTbl:Sys.var_table_t) : E.sort =
  match stm_t with
  | Stm.AddrT (Stm.Malloc _)    -> E.Addr
  | Stm.AddrT (Stm.MallocSL _)  -> E.Addr
  | Stm.AddrT (Stm.MallocSLK _) -> E.Addr
  | _ -> let t = Stm.term_to_expr_term stm_t
         in
           System.get_sort_from_term globalVars inpTbl locTbl invVars t


(* Looks for a term sort in the global and temporal var tables. *)
(* BEWARE! Works only with current local tables. Hence, if called from
   outside a procedure, it will assign tid to a local variable not belonging
   to the current procedure *)
let get_sort (stm_t:Stm.term) : E.sort =
  get_sort_from_tables stm_t inputVars localVars


let get_var_kind (v:E.V.id) : E.var_nature =
  let k = if System.mem_var localVars v then
            System.find_var_kind localVars v
          else if System.mem_var inputVars v then
            System.find_var_kind inputVars v
          else if System.mem_var globalVars v then
            System.find_var_kind globalVars v
          else if !flag_parsingInv then
            System.find_var_kind invVars v
          else
            let _ = undefTids := v :: !undefTids in
            let _ = decl_global_var v E.Tid None E.RealVar in
              E.RealVar
  in
    k

(* Parsing error message funtion *)
let parser_error msg =
  let msg = sprintf "Error at line %i:\n%s" (Global.get_linenum ()) msg in
    raise(ParserError msg)



let parser_typing_error term a_sort get_expr =
  let term_str = (Stm.term_to_str term) in
  let term_sort_str = (E.sort_to_str (get_sort term)) in
  let sort_str = (E.sort_to_str a_sort) in
  let str_expr = (get_expr ()) in
  let str = sprintf "Term \"%s\" is of sort %s, but it was \
                     expected to be of sort %s in expression \"%s\""
                     term_str term_sort_str sort_str str_expr in
  parser_error str



let parser_types_incompatible (t1:Stm.term) (t2:Stm.term) get_expr_str =
  let t1_str = (Stm.term_to_str t1) in
  let s1_str = (E.sort_to_str (get_sort t1)) in
  let t2_str = (Stm.term_to_str t2) in
  let s2_str = (E.sort_to_str (get_sort t2)) in
  let str_expr = (get_expr_str ()) in
  let str = (Printf.sprintf "Unexpectedly \"%s\" is of type \"%s\" and  \
                             \"%s\" is of type \"%s\", when they should \
                             have the same type in expression \"%s\"."
                            t1_str s1_str t2_str s2_str str_expr) in
    parser_error str



let parser_check_compatibility t1 t2 get_expr_str =
  let s1 = get_sort t1 in
  let s2 = get_sort t2 in
    if (s1 != s2) then
      parser_types_incompatible t1 t2 get_expr_str


let parser_check_compatibility_with_op_cond t1 t2 get_expr_str op =
  let s1 = get_sort t1 in
  let s2 = get_sort t2 in
  match op with
    In          -> if (s1 != E.Addr || s2 != E.Set) then
                     parser_types_incompatible t1 t2 get_expr_str
  | SubsetEq    -> if (s1 != E.Set || s2 != E.Set) then
                     parser_types_incompatible t1 t2 get_expr_str
  | InTh        -> if (s1 != E.Tid || s2 != E.SetTh) then
                     parser_types_incompatible t1 t2 get_expr_str
  | SubsetEqTh  -> if (s1 != E.SetTh || s2 != E.SetTh) then
                     parser_types_incompatible t1 t2 get_expr_str
  | InInt       -> if (s1 != E.Int || s2 != E.SetInt) then
                     parser_types_incompatible t1 t2 get_expr_str
  | SubsetEqInt -> if (s1 != E.SetInt || s2 != E.SetInt) then
                     parser_types_incompatible t1 t2 get_expr_str
  | _           -> if (s1 != s2) then
                     parser_types_incompatible t1 t2 get_expr_str



let parser_check_var_assign v s1 s2 get_expr_str =
  let str_expr = (get_expr_str()) in
  if (s1 != s2) then
    begin
      Interface.Err.msg "Unexpected sort" $
              sprintf "Variable %s has sort %s, but sort %s was expected \
                       in:\n\n%s"
                      v (E.sort_to_str s1) (E.sort_to_str s2) (str_expr);
      raise(Sort_mismatch(v, s1, s2))
    end


let parser_check_type checker a_term a_sort get_expr_str =
  try
    checker a_term
  with
    | WrongType(_) -> parser_typing_error a_term a_sort get_expr_str



(* slow way to project: traverse one time per entry *)
let get_name id = fst id
let get_line id = snd id


let check_sort_var (v:Stm.varId)
                   (p:Stm.procedure_name)
                   (s:E.sort)
                   (k:E.var_nature) : unit =
  let generic_var = Stm.VarT (Stm.build_var v E.Unknown p ~nature:k) in
  let knownSort = get_sort generic_var in
    if (knownSort != s) then
      begin
        Interface.Err.msg "Mismatch variable type" $
          sprintf "Variable %s is of sort %s, while it is trying to be \
                   assigned to an expression of sort %s"
                    v (E.sort_to_str knownSort) (E.sort_to_str s);
        raise(Sort_mismatch(v, knownSort, s))
      end


let wrong_sort_msg_for (t:Stm.term) (s:E.sort) : unit =
  Interface.Err.msg "Wrong type" $
  sprintf "A term of sort %s was expected, but term \"%s\" has sort %s."
              (E.sort_to_str s) (Stm.term_to_str t)
              (E.sort_to_str (get_sort t))


let parser_check_boolean_type a_term get_expr_str =
  match a_term with
    | Stm.VarT v -> check_sort_var v.Stm.id v.Stm.scope E.Bool v.Stm.nature; a_term
    | _          -> parser_typing_error a_term E.Bool get_expr_str


let check_type_int t =
  match t with
      Stm.IntT(i) -> i
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.Int v.Stm.nature; Stm.VarInt v
    | _           -> raise(WrongType t)


let check_type_pair t =
  match t with
      Stm.PairT(i) -> i
    | Stm.VarT v   -> check_sort_var v.Stm.id v.Stm.scope E.Pair v.Stm.nature; Stm.VarPair v
    | _            -> raise(WrongType t)


let check_type_set t =
  match t with
      Stm.SetT(s) -> s
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.Set v.Stm.nature; Stm.VarSet v
    | _           -> raise(WrongType t)


let check_type_elem t =
  match t with
      Stm.ElemT(e) -> e
    | Stm.VarT v   -> check_sort_var v.Stm.id v.Stm.scope E.Elem v.Stm.nature; Stm.VarElem v
    | _            -> raise(WrongType t)


let check_type_tid t =
  match t with
      Stm.TidT(th) -> th
    | Stm.VarT v    -> check_sort_var v.Stm.id v.Stm.scope E.Tid v.Stm.nature; Stm.VarTh v
    | _             -> raise(WrongType t)


let check_type_addr t =
  match t with
      Stm.AddrT(a) -> a
    | Stm.VarT v   -> check_sort_var v.Stm.id v.Stm.scope E.Addr v.Stm.nature; Stm.VarAddr v
    | _            -> raise(WrongType t)


let check_type_cell t =
  match t with
      Stm.CellT(c) -> c
    | Stm.VarT v   -> check_sort_var v.Stm.id v.Stm.scope E.Cell v.Stm.nature; Stm.VarCell v
    | _            -> raise(WrongType t)


let check_type_setth t =
  match t with
      Stm.SetThT(sth) -> sth
    | Stm.VarT v      -> check_sort_var v.Stm.id v.Stm.scope E.SetTh v.Stm.nature; Stm.VarSetTh v
    | _               -> raise(WrongType t)


let check_type_setint t =
  match t with
      Stm.SetIntT(sth) -> sth
    | Stm.VarT v       -> check_sort_var v.Stm.id v.Stm.scope E.SetInt v.Stm.nature; Stm.VarSetInt v
    | _                -> raise(WrongType t)


let check_type_setelem t =
  match t with
      Stm.SetElemT(se) -> se
    | Stm.VarT v       -> check_sort_var v.Stm.id v.Stm.scope E.SetElem v.Stm.nature; Stm.VarSetElem v
    | _                -> raise(WrongType t)


let check_type_setpair t =
  match t with
      Stm.SetPairT(sp) -> sp
    | Stm.VarT v       -> check_sort_var v.Stm.id v.Stm.scope E.SetPair v.Stm.nature; Stm.VarSetPair v
    | _                -> raise(WrongType t)


let check_type_path t =
  match t with
      Stm.PathT(p) -> p
    | Stm.VarT v   -> check_sort_var v.Stm.id v.Stm.scope E.Path v.Stm.nature; Stm.VarPath v
    | _            -> raise(WrongType t)


let check_type_mem t =
  match t with
      Stm.MemT(m) -> m
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.Mem v.Stm.nature; Stm.VarMem v
    | _           -> raise(WrongType t)


let check_type_addrarr t =
  match t with
      Stm.AddrArrayT(arr) -> arr
    | Stm.VarT v          -> check_sort_var v.Stm.id v.Stm.scope E.AddrArray v.Stm.nature; Stm.VarAddrArray v
    | _                   -> raise(WrongType t)


let check_type_tidarr t =
  match t with
      Stm.TidArrayT(arr) -> arr
    | Stm.VarT v         -> check_sort_var v.Stm.id v.Stm.scope E.TidArray v.Stm.nature; Stm.VarTidArray v
    | _                  -> raise(WrongType t)


let check_type_mark t =
  match t with
      Stm.MarkT m -> m
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.Mark v.Stm.nature; Stm.VarMark v
    | _           -> raise(WrongType t)


let check_type_bucket t =
  match t with
      Stm.BucketT b -> b
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.Bucket v.Stm.nature; Stm.VarBucket v
    | _           -> raise(WrongType t)


let check_type_bucketarr t =
  match t with
      Stm.BucketArrayT arr -> arr
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.BucketArray v.Stm.nature; Stm.VarBucketArray v
    | _           -> raise(WrongType t)


let check_type_lock t =
  match t with
      Stm.LockT l -> l
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.Lock v.Stm.nature; Stm.VarLock v
    | _           -> raise(WrongType t)


let check_type_lockarr t =
  match t with
      Stm.LockArrayT arr -> arr
    | Stm.VarT v  -> check_sort_var v.Stm.id v.Stm.scope E.LockArray v.Stm.nature; Stm.VarLockArray v
    | _           -> raise(WrongType t)

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
  | "pairSet"   -> E.SetPair
  | "int"       -> E.Int
  | "pair"      -> E.Pair
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


  let check_is_procedure (id:string) : unit =
    if not (List.mem_assoc id !procedures) then
      begin
        Interface.Err.msg "Unknown procedure" $
                sprintf "Identifier \"%s\" is used as a procedure identifier, \
                         but no procedure with such name has been parsed." id;
        raise(Unknown_procedure id)
      end


  let check_call_args (p_info:Sys.proc_info_t)
                      (p_name:string)
                      (ps:Stm.term list) : unit =
    let args_signature = Sys.proc_info_get_args_sig p_info in
    let call_args = List.map get_sort ps in
    if (args_signature <> call_args) then
      begin
        Interface.Err.msg "Procedure arguments signature mismatched" $
          Printf.sprintf "Procedure %s expected arguments of sort\n\t%s\n \
                          but it was called with arguments (%s), of sort\n\t %s"
                          p_name
                          (String.concat " -> " $
                            List.map E.sort_to_str args_signature)
                          (String.concat ", " $ List.map Stm.term_to_str ps)
                          (String.concat " -> " $
                            List.map E.sort_to_str call_args);
        raise(Procedure_args_mismatch p_name)
      end


  let inject_sort (exp:Stm.term) : Stm.term =
    match exp with
      Stm.VarT v ->
        let s = get_sort exp in
        let modif_v = Stm.var_replace_sort v s in
          begin
            match s with
              E.Set          -> Stm.SetT         (Stm.VarSet          modif_v)
            | E.Elem         -> Stm.ElemT        (Stm.VarElem         modif_v)
            | E.Tid          -> Stm.TidT         (Stm.VarTh           modif_v)
            | E.Addr         -> Stm.AddrT        (Stm.VarAddr         modif_v)
            | E.Cell         -> Stm.CellT        (Stm.VarCell         modif_v)
            | E.SetTh        -> Stm.SetThT       (Stm.VarSetTh        modif_v)
            | E.SetInt       -> Stm.SetIntT      (Stm.VarSetInt       modif_v)
            | E.SetElem      -> Stm.SetElemT     (Stm.VarSetElem      modif_v)
            | E.SetPair      -> Stm.SetPairT     (Stm.VarSetPair      modif_v)
            | E.Path         -> Stm.PathT        (Stm.VarPath         modif_v)
            | E.Mem          -> Stm.MemT         (Stm.VarMem          modif_v)
            | E.Bool         -> Stm.VarT         modif_v
            | E.Int          -> Stm.IntT         (Stm.VarInt          modif_v)
            | E.Pair         -> Stm.PairT        (Stm.VarPair         modif_v)
            | E.Array        -> Stm.ArrayT       (Stm.VarArray        modif_v)
            | E.AddrArray    -> Stm.AddrArrayT   (Stm.VarAddrArray    modif_v)
            | E.TidArray     -> Stm.TidArrayT    (Stm.VarTidArray     modif_v)
            | E.BucketArray  -> Stm.BucketArrayT (Stm.VarBucketArray  modif_v)
            | E.Mark         -> Stm.MarkT        (Stm.VarMark         modif_v)
            | E.Bucket       -> Stm.BucketT      (Stm.VarBucket       modif_v)
            | E.Lock         -> Stm.LockT        (Stm.VarLock         modif_v)
            | E.LockArray    -> Stm.LockArrayT   (Stm.VarLockArray    modif_v)
            | E.Unknown      -> Stm.VarT         modif_v
          end
    | _                   -> exp


  let check_assignment_term (t:Stm.term) (get_expr_str:unit -> string) =
    match t with
      Stm.VarT _                               -> ()
    | Stm.SetT(Stm.VarSet _)                   -> ()
    | Stm.ElemT(Stm.VarElem _)                 -> ()
    | Stm.ElemT(Stm.CellData(Stm.VarCell _ ))  -> ()
    | Stm.TidT(Stm.VarTh _ )                   -> ()
    | Stm.BucketT(Stm.VarBucket _ )            -> ()
    | Stm.BucketT(Stm.BucketArrRd _)           -> ()
    | Stm.LockT(Stm.VarLock _ )                -> ()
    | Stm.LockT(Stm.LockArrRd _)               -> ()
    | Stm.MarkT(Stm.VarMark _ )                -> ()
    | Stm.AddrT(Stm.VarAddr _)                 -> ()
    | Stm.AddrT(Stm.Next(Stm.VarCell _))       -> ()
    | Stm.AddrT(Stm.NextAt(Stm.VarCell _,_))   -> ()
    | Stm.MarkT(Stm.Marked(Stm.VarCell _))     -> ()
    | Stm.CellT(Stm.VarCell _)                 -> ()
    | Stm.SetThT(Stm.VarSetTh _ )              -> ()
    | Stm.PathT(Stm.VarPath _ )                -> ()
    | Stm.MemT(Stm.VarMem _)                   -> ()
    | Stm.IntT(Stm.VarInt _)                   -> ()
    | Stm.ElemT(Stm.PointerData _)             -> ()
    | Stm.AddrT(Stm.PointerNext _)             -> ()
    | Stm.AddrT(Stm.PointerNextAt _)           -> ()
    | Stm.AddrT(Stm.PointerArrAt _)            -> ()
    | Stm.AddrT(Stm.AddrArrRd _)               -> ()
    | Stm.TidT(Stm.PointerLockid _)            -> ()
    | Stm.TidT(Stm.PointerLockidAt _)          -> ()
    | Stm.MarkT(Stm.PointerMarked _)           -> ()
    | Stm.TidT(Stm.TidArrRd _)                 -> ()
    | Stm.AddrT(Stm.BucketInit _)              -> ()
    | Stm.AddrT(Stm.BucketEnd _)               -> ()
    | Stm.SetT(Stm.BucketRegion _)             -> ()
    | Stm.TidT(Stm.BucketTid _)                -> ()
    | _ -> begin
             Interface.Err.msg "Invalid assignment" $
                      sprintf "The assignment \"%s\" is invalid. Assignments \
                               can be done only over variables, cells fields \
                               or array locations."
                               (get_expr_str ());
             raise(Wrong_assignment t)
           end


  let check_assignment_kind (t:Stm.term) (e:Stm.expr_t) (str:string) : unit =
    let t_ghost = Stm.var_kind (E.GhostVar) (Stm.Term t) in
    let e_ghost = Stm.var_kind (E.GhostVar) e in
    if (t_ghost <> [] || e_ghost <> []) then
      begin
        let ghost_list = t_ghost @ e_ghost in
        Interface.Err.msg "Ghost variable in assignment" $
                  sprintf "Ghost variables [%s] are present in the \
                           assignment:\n%s\n"
                           (String.concat ", "
                              (List.map Stm.term_to_str ghost_list))
                           (str);
        raise(Ghost_vars_in_assignment ghost_list)
      end


  let check_ghost_assignment_kind (t:Stm.term)
                                  (str:string) : unit =
    let t_normal = Stm.var_kind (E.RealVar) (Stm.Term t) in
    if (t_normal <> []) then
      begin
        Interface.Err.msg "No ghost variable in ghost assignment" $
                  sprintf "No ghost variables [%s] are assigned within the \
                           ghost assignment:\n%s\n"
                           (String.concat ", "
                              (List.map Stm.term_to_str t_normal))
                           (str);
        raise(Normal_vars_in_ghost_assignment t_normal)
      end


  let check_no_assigned (e:Stm.expr_t)
                        (l:Stm.expr_t list)
                        (st_str:string) : unit =
    if List.mem e l then
      begin
        Interface.Err.msg "Double assignment to term" $
          sprintf "The term [%s] is assigned twice within the following \
                   ghost code or atomic assignment:\n\n%s\n"
                  (Stm.expr_to_str e) st_str;
        raise(Atomic_double_assignment e)
      end


  let check_no_double_assignment (l1:Stm.expr_t list)
                                 (l2:Stm.expr_t list)
                                 (st_str:string): Stm.expr_t list =
    let rec find xs ys zs = match xs with
                              []   -> zs
                            | e::l -> let _ = check_no_assigned e ys st_str
                                      in
                                        find l ys (e::zs)
    in
      find l1 l2 l2


let unexpected_statement get_str_expr =
  let str_expr = (get_str_expr()) in
    Interface.Err.msg "Unexpected statement" $
      sprintf "Ghost and atomic statements admit only assignments or \
               conditional statements. However, the following statement \
               was found:\n\n%s\n" str_expr;
    raise(Unexpected_statement str_expr)


let check_var_belongs_to_procedure (v:E.V.id) (p_name:string) =
  let p_info = List.assoc p_name !procedures in
  let iVars = System.proc_info_get_input p_info in
  let lVars = System.proc_info_get_local p_info in
    if not (System.mem_var iVars v || System.mem_var lVars v) then
      begin
        Interface.Err.msg "Variable not declared in procedure" $
                sprintf "Variable \"%s\" does not belong to procedure %s"
                        v p_name;
        raise(Variable_not_in_procedure(v, p_name))
      end


let check_call_sort (t_opt:Stm.term option)
                    (t_proc_info:Sys.proc_info_t)
                    (p_name:string)
                    (proc_info:Sys.proc_info_t) : unit =
  let tlVars = Sys.proc_info_get_local t_proc_info in
  let tiVars = Sys.proc_info_get_input t_proc_info in
  let proc_sort = Sys.proc_info_get_sort proc_info in
  let find_sort x = get_sort_from_tables x tiVars tlVars in
  match (t_opt,proc_sort) with
  | (None  , None  ) -> ()
  | (None  , Some _) -> ()
  | (Some t, None  ) -> begin
                          let t_sort = find_sort t in
                          Interface.Err.msg "Incompatible call assignment" $
                            sprintf "Term %s of sort %s is assigned the value \
                                     returned by procedure %s. But %s returns \
                                     no value." (Stm.term_to_str t)
                                                (E.sort_to_str t_sort)
                                                (p_name) (p_name) ;
                          raise(Incompatible_call_sort(t, p_name))
                        end
  | (Some t, Some s) -> let t_sort = find_sort t in
                        if t_sort <> s then
                          begin
                            Interface.Err.msg "Incompatible call assignment" $
                              sprintf "Term %s is of sort %s, while procedure \
                                       %s returns a value of sort %s."
                                        (Stm.term_to_str t)
                                        (E.sort_to_str t_sort)
                                        (p_name)
                                        (E.sort_to_str s);
                            raise(Incompatible_call_sort(t, p_name))
                          end


let check_return_sort (t_opt:Stm.term option)
                      (p_name:string)
                      (proc_info:Sys.proc_info_t) : unit =
  let p_sort = Sys.proc_info_get_sort proc_info in
  match (t_opt,p_sort) with
  | (None  , None  ) -> ()
  | (None  , Some s) -> begin
                          Interface.Err.msg "Return value expected" $
                            sprintf "Procedure %s expects to return a value of \
                                     sort %s, but no value was returned."
                              (p_name) (E.sort_to_str s) ;
                          raise(Incompatible_return_sort p_name)
                        end
  | (Some t, None  ) -> begin
                          let iVars = Sys.proc_info_get_input proc_info in
                          let lVars = Sys.proc_info_get_local proc_info in
                          let t_sort = get_sort_from_tables t iVars lVars in
                          Interface.Err.msg "Return value unexpected" $
                            sprintf "Procedure %s returns term %s of sort %s, \
                                     but no sort was declared for such procedure."
                              p_name (Stm.term_to_str t) (E.sort_to_str t_sort);
                          raise(Incompatible_return_sort p_name)
                        end
  | (Some t, Some s) -> begin
                          let iVars = Sys.proc_info_get_input proc_info in
                          let lVars = Sys.proc_info_get_local proc_info in
                          let t_sort = get_sort_from_tables t iVars lVars in
                          if t_sort <> s then
                            begin
                              Interface.Err.msg "Return value incompatibility" $
                                sprintf "Procedure %s expects to return a \
                                         value of sort %s, but term %s of \
                                         sort %s is returned."
                                  p_name (E.sort_to_str s)
                                  (Stm.term_to_str t) (E.sort_to_str t_sort);
                              raise(Incompatible_return_sort p_name)
                            end
                        end


let global_decl_cond (k:E.var_nature)
                     (sort_name:string)
                     (v_name:E.V.id)
                     (op:cond_op_t)
                     (t:Stm.term) : unit =
  let s      = check_and_get_sort sort_name in
  let var    = Stm.build_var v_name s Stm.GlobalScope ~nature:k in
  let (op_symb_str, expr_cond) =
    match op with
      Less        -> ("<",  Stm.Less     (Stm.VarInt var,Stm.term_to_integer t))
    | Greater     -> (">",  Stm.Greater  (Stm.VarInt var,Stm.term_to_integer t))
    | LessEq      -> ("<=", Stm.LessEq   (Stm.VarInt var,Stm.term_to_integer t))
    | GreaterEq   -> (">=", Stm.GreaterEq(Stm.VarInt var,Stm.term_to_integer t))
    | In          -> ("in", Stm.In       (Stm.VarAddr var, Stm.term_to_set t))
    | SubsetEq    -> ("subseteq",    Stm.SubsetEq    (Stm.VarSet var,
                                                      Stm.term_to_set t))
    | InTh        -> ("inTh",        Stm.InTh        (Stm.VarTh var,
                                                      Stm.term_to_setth t))
    | SubsetEqTh  -> ("subseteqTh",  Stm.SubsetEqTh  (Stm.VarSetTh var,
                                                      Stm.term_to_setth t))
    | InInt       -> ("inInt",       Stm.InInt       (Stm.VarInt var,
                                                      Stm.term_to_setint t))
    | SubsetEqInt -> ("subseteqInt", Stm.SubsetEqInt (Stm.VarSetInt var,
                                                      Stm.term_to_setint t))
    | InElem      -> ("inElem",      Stm.InElem      (Stm.VarElem var,
                                                      Stm.term_to_setelem t))
    | SubsetEqElem-> ("subseteqElem",Stm.SubsetEqElem(Stm.VarSetElem var,
                                                      Stm.term_to_setelem t))
    | InPair      -> ("spin",        Stm.InPair      (Stm.VarPair var,
                                                      Stm.term_to_setpair t))
    | SubsetEqPair-> ("spsubseteq",  Stm.SubsetEqPair(Stm.VarSetPair var,
                                                      Stm.term_to_setpair t)) in



  let cond = Stm.boolean_to_expr_formula (Formula.atom_to_formula expr_cond) in
  let get_str_expr () = sprintf "%s %s %s %s" (E.sort_to_str s)
                                              (v_name)
                                              (op_symb_str)
                                              (Stm.term_to_str t) in
  let _    = decl_global_var v_name s (Some (E.Condition cond)) k
  in
    parser_check_compatibility_with_op_cond (Stm.VarT var) t get_str_expr op


let lock_pos_to_str (pos:Stm.integer option) : string =
  match pos with
  | None   -> ""
  | Some i -> sprintf "[%s]" (Stm.term_to_str (Stm.IntT i))


let fix_conditional_jumps () : unit =
  List.iter (fun n ->
    try
      let (_,stm) = Hashtbl.find pos_st n in
      match stm with
      | Stm.StIf(_,Stm.StSeq xs,else_stm,_,_) ->
          begin
            let jump_pos = match else_stm with
                           | None -> (Stm.get_st_info stm).Stm.else_pos
                           | Some ys -> (Stm.get_last_st_info ys).Stm.next_pos in
            let last_xs = lastElem xs in
            match last_xs with
            | Stm.StWhile _
            | Stm.StReturn _ -> ()
            | Stm.StIf (_,Stm.StSeq _,None,_,_) ->
                begin
                  (Stm.get_st_info last_xs).Stm.else_pos <- jump_pos
                end
            | Stm.StIf (_,Stm.StSeq xs,Some ys,_,_) ->
                begin
                  (match (lastElem xs) with
                   | Stm.StIf _ -> ()
                   | _ -> (Stm.get_st_info (lastElem xs)).Stm.next_pos <- jump_pos
                  );
                  (Stm.get_last_st_info ys).Stm.next_pos <- jump_pos
                end
            | _ -> begin
                    (Stm.get_st_info last_xs).Stm.next_pos <- jump_pos
                   end
          end
      | _ -> ()
    with Not_found -> ()
  ) !cond_stm_list




%}
%token <string*int> IDENT  // second param is line number
%token <int> NUMBER

%token GLOBAL
%token GHOST
%token ASSUME
%token PROCEDURE BEGIN END CALL RETURN

%token ST_SKIP ST_ASSERT ST_AWAIT ST_NONCRITICAL ST_CRITICAL
%token ST_IF ST_THEN ST_ELSE ST_ENDIF
%token ST_WHILE ST_DO ST_ENDWHILE
%token ST_CHOICE ST_OR ST_ENDCHOICE

%token MALLOC MALLOCSL MALLOCSLK
%token POINTER
%token ME

%token ERROR MKCELL DATA NEXT NEXTAT LOCKID LOCK UNLOCK ARR
%token MARKED MARK_T MARK_F
%token HAVOCLISTELEM HAVOCSKIPLISTELEM LOWEST_ELEM HIGHEST_ELEM
%token SKIPLIST
%token HAVOCLEVEL HASHCODE
%token MEMORY_READ
%token COMMA
%token NULL UPDATE
%token ARR_UPDATE
%token EPSILON SINGLE_PATH
%token EMPTYSET UNION INTR SETDIFF
%token EMPTYSETTH UNIONTH INTRTH SETDIFFTH SINGLETH
%token EMPTYSETINT UNIONINT INTRINT SETDIFFINT SINGLEINT
%token EMPTYSETELEM UNIONELEM INTRELEM SETDIFFELEM SINGLEELEM SET2ELEM
%token PATH2SET ADDR2SET GETP FIRSTLOCKED LASTLOCKED ORDERLIST
%token APPEND REACH
%token IN SUBSETEQ
%token INTH SUBSETEQTH
%token ININT SUBSETEQINT
%token INELEM SUBSETEQELEM
%token SETINTMIN SETINTMAX
%token INTOF TIDOF SETPAIRMIN SETPAIRMAX
%token SETPAIREMPTY SETPAIRSINGLE SETPAIRUNION SETPAIRINTR SETPAIRDIFF
%token SETPAIRIN SETPAIRSUBSETEQ
%token THREAD
%token MKBUCKET BINIT BEND BREGION BTID BARRAYUPD
%token HASHTBL
%token OPEN_BRACKET CLOSE_BRACKET
%token OPEN_SET CLOSE_SET
%token OPEN_PAREN CLOSE_PAREN
%token GHOST_DELIMITER
%token VERTICAL_BAR
%token COLON DOUBLECOLON SEMICOLON EQUALS NOT_EQUALS
%token ASSIGN
%token LOGICAL_AND LOGICAL_OR LOGICAL_NOT LOGICAL_THEN LOGICAL_IFF
%token LOGICAL_TRUE LOGICAL_FALSE
%token DOT

%token INVARIANT PARAM
%token AT UNDERSCORE SHARP
%token MATH_PLUS MATH_MINUS MATH_MULT MATH_DIV MATH_MOD MATH_LESS MATH_GREATER
%token MATH_LESS_EQ MATH_GREATER_EQ

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
%left POINTER



%start system


%type <(System.t * Expression.V.id list)> system

%type <Stm.boolean option> initial_assumption

%type <unit> global_declarations
%type <unit> global_decl_list
%type <unit> global_decl

%type <unit> local_declarations
%type <unit> local_decl_list
%type <unit> local_decl

%type <E.var_nature> kind

%type <unit> procedure_list
%type <unit> procedure
%type <E.sort option> procedure_sort
%type <string> procedure_name
%type <(E.V.id * E.sort) list> args
%type <(E.V.id * E.sort) list> arg_list
%type <(E.V.id * E.sort)> arg

%type <Stm.term list> params
%type <Stm.term list> param_list

%type <int> if_keyword
%type <unit> if_ghost_atomic_keyword
%type <int> while_keyword
%type <int> choice_keyword

%type <Stm.statement_t option> program
%type <Stm.statement_t list> statement_list
%type <Stm.statement_t> statement
%type <Stm.statement_t option> statements_else_if
%type <Stm.statement_t list> statements_choice

%type <Stm.statement_t option> ghost_block_or_semicolon
%type <Stm.statement_t option> ghost_block_or_nothing
%type <Stm.statement_t option> ghost_block
%type <Stm.expr_t list * Stm.statement_t list> ghost_statement_list
%type <Stm.expr_t list * Stm.statement_t> ghost_statement
%type <Stm.expr_t list * Stm.statement_t option> ghost_statements_else_if
%type <(Stm.expr_t list * Stm.statement_t) list> ghost_statements_choice

%type <Stm.expr_t list * Stm.statement_t list> atomic_statement_list
%type <Stm.expr_t list * Stm.statement_t> atomic_statement
%type <Stm.expr_t list * Stm.statement_t option>atomic_statements_else_if
%type <(Stm.expr_t list * Stm.statement_t) list> atomic_statements_choice

%type <Stm.term option> maybe_term
%type <Stm.term list> term_list

%type <Statement.boolean> formula
%type <Stm.term> term
%type <Stm.cell> cell
%type <Stm.tid> tid
%type <Stm.elem> elem
%type <Stm.addr> addr
%type <Stm.mem> mem
%type <Stm.path> path
%type <Stm.set> set
%type <Stm.setth> setth
%type <Stm.setint> setint
%type <Stm.setelem> setelem
%type <Stm.setpair> setpair
%type <Stm.integer> integer
%type <Stm.pair> pair
%type <Stm.atom> atom
%type <Stm.eq> equals
%type <Stm.diseq> disequals
%type <Stm.term> arraylookup
%type <Stm.term> arrayupd
%type <Stm.integer option> lock_pos
%type <Stm.mark> mark
%type <Stm.bucket> bucket
%type <Stm.lock> lock



%%


/***********************    SYSTEMS    *************************/

system :
  GLOBAL global_declarations initial_assumption procedure_list
    {
      (* ALE: Check if conditional jump positions is correct. *)
      fix_conditional_jumps();

      let proc_tbl    = System.new_proc_table_from_list !procedures in
      let assume_cond = $3 in
      (* Update pos_st hashtbl with information about calls and returns *)
      let _ = Hashtbl.iter (fun _ pc ->
                match Hashtbl.find pos_st pc with
                | (p, Stm.StCall (t,p_name, ps, g, Some info)) ->
                    let _ = check_is_procedure p_name in
                    let proc_info = Hashtbl.find proc_tbl p_name in
                    let _ = check_call_args proc_info p_name ps in
                    (* Check possible assigned variable sort and procedure sort *)
                    let t_proc_info = Hashtbl.find proc_tbl p in
                    let _ = check_call_sort t t_proc_info p_name proc_info in
                    let init_line = Sys.proc_init_line proc_info in
                    let _ = info.Stm.call_pos <- Some init_line in
                    Hashtbl.replace pos_st pc
                      (p, Stm.StCall (t,p_name,ps,g,Some info))
                | _ -> ()
              ) call_points in
      let _ = Hashtbl.iter (fun proc pc ->
                match Hashtbl.find pos_st pc with
                | (p,  Stm.StReturn (t, g, Some info)) ->
                    (* Check return type matches procedure type *)
                    let proc_info = Hashtbl.find proc_tbl p in
                    let _ = check_return_sort t p proc_info in
                    (* Update of return position *)
                    let ret_pos_list = Hashtbl.find_all call_points proc in
                    let (calls, rets) = List.fold_left (fun (cs,rs) pos ->
                                          let (_,st) = Hashtbl.find pos_st pos in
                                            (Stm.get_st_pos st :: cs,
                                             Stm.get_st_next_pos st :: rs)
                                        ) ([],[]) ret_pos_list in
                    let _ = info.Stm.called_from_pos <- calls in
                    let _ = info.Stm.return_pos <- rets in
                    Hashtbl.replace pos_st pc (p, Stm.StReturn (t,g,Some info))
                | _ -> ()
              ) return_points in
      let sys = System.new_system (System.copy_var_table globalVars)
                                  assume_cond
                                  proc_tbl
                                  transitions
                                  []
                                  (Hashtbl.copy pos_st)
                                  labelTbl in
      let _      = current_proc := "" in
      let ts     = !undefTids in
      let _      = undefTids := [] in
      let _      = cond_stm_list := [] in

      if System.is_proc sys Sys.defMainProcedure then
        (* Check whether the last statement in main is a return() *)
        let main_info = Sys.get_proc_by_name sys Sys.defMainProcedure in
        let lst_main = Sys.proc_last_line main_info in
        let (_,st) = Sys.get_statement_at sys lst_main in
        match st with
        | Stm.StReturn(None,_,Some info) ->
            let _ = info.Stm.return_pos <- [(Sys.get_trans_num sys)+1] in
            (sys, ts)
        | Stm.StReturn(Some t,_,_) ->
            begin
              let lVars = Sys.proc_info_get_local main_info in
              let iVars = Sys.proc_info_get_input main_info in
              let t_sort = get_sort_from_tables t iVars lVars in
              Interface.Err.msg "Main procedure returns value" $
                sprintf "Main procedure returns term %s os sort %s \
                         but no value was expected to be returned."
                        (Stm.term_to_str t)
                        (E.sort_to_str t_sort);
              raise(No_valid_main)
            end
        | _ -> begin
                 Interface.Err.msg ("No return at end of \"" ^
                                     Sys.defMainProcedure ^ "\" procedure") $
                   sprintf "The last statement at \"%s\" should be a return, \
                            but instead, statement\n\t %s\n was found."
                            (Sys.defMainProcedure)
                            (Stm.statement_to_str 0 st);
                 raise(No_valid_main)
               end
      else
        begin
          Interface.Err.msg ("No \"" ^ Sys.defMainProcedure ^ "\" \
                            procedure defined")
                  ("A \"" ^ Sys.defMainProcedure ^ "\" procedure could \
                   not be found in the system description.");
          raise(No_main)
        end
    }



/* GLOBAL DECLARATIONS */

global_declarations :
  |
    { (decl_global_var Conf.heap_name E.Mem None E.RealVar) }
  | global_decl_list
    { (decl_global_var Conf.heap_name E.Mem None E.RealVar) }

global_decl_list :
  global_decl
    { () }
  | global_decl global_decl_list
    { () }

global_decl :
  kind IDENT IDENT
    {
      let k      = $1 in
      let s      = check_and_get_sort (get_name $2) in
      let v_name = get_name $3 in

      decl_global_var v_name s None k
    }
  | kind IDENT IDENT ASSIGN term
    {
      let k      = $1 in
      let s      = check_and_get_sort (get_name $2) in
      let v_name = get_name $3 in
      let t      = $5 in
      let v      = Stm.VarT(Stm.build_var v_name s Stm.GlobalScope ~nature:k) in
      let get_str_expr () = sprintf "%s %s := %s" (E.sort_to_str s)
                                                  (v_name)
                                                  (Stm.term_to_str t) in
      let _        = decl_global_var v_name s
                      (Some (E.Equality (Stm.term_to_expr_term t))) k
      in
        parser_check_compatibility v t get_str_expr
    }
  | kind IDENT IDENT ASSIGN formula
    {
      let k      = $1 in
      let s      = check_and_get_sort (get_name $2) in
      let v_name = get_name $3 in
      let b      = Stm.boolean_to_expr_formula $5 in
      let get_str_expr () = sprintf "%s %s := %s" (E.sort_to_str s)
                                                  (v_name)
                                                  (E.formula_to_str b) in
      let cond = b in
      let _ = decl_global_var v_name s (Some (E.Condition cond)) k
      in
        parser_check_var_assign v_name s (E.Bool) get_str_expr
    }
  | kind IDENT IDENT MATH_LESS term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) Less $5
    }
  | kind IDENT IDENT MATH_GREATER term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) Greater $5
    }
  | kind IDENT IDENT MATH_LESS_EQ term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) LessEq $5
    }
  | kind IDENT IDENT MATH_GREATER_EQ term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) GreaterEq $5
    }
  | kind IDENT IDENT IN term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) In $5
    }
  | kind IDENT IDENT SUBSETEQ term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) SubsetEq $5
    }
  | kind IDENT IDENT INTH term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) InTh $5
    }
  | kind IDENT IDENT SUBSETEQTH term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) SubsetEqTh $5
    }
  | kind IDENT IDENT ININT term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) InInt $5
    }
  | kind IDENT IDENT SUBSETEQINT term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) SubsetEqInt $5
    }
  | kind IDENT IDENT INELEM term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) InElem $5
    }
  | kind IDENT IDENT SUBSETEQELEM term
    {
      global_decl_cond $1 (get_name $2) (get_name $3) SubsetEqElem $5
    }
  | kind IDENT SETPAIRIN OPEN_PAREN IDENT COMMA term CLOSE_PAREN
    {
      global_decl_cond $1 (get_name $2) (get_name $5) InPair $7
    }
  | kind IDENT SETPAIRSUBSETEQ OPEN_PAREN IDENT COMMA term CLOSE_PAREN
    {
      global_decl_cond $1 (get_name $2) (get_name $5) SubsetEqPair $7
    }


/* INITIAL ASSUMPTIONS */
initial_assumption :
  |
    { None }
  |
    ASSUME formula
    { Some $2 }


/* LOCAL DECLARATIONS */

local_declarations :
  |
    { () }
  | local_decl_list
    { () }

local_decl_list :
  local_decl
    { () }
  | local_decl local_decl_list
    { () }

local_decl :
  kind IDENT IDENT
    {
      let k      = $1 in
      let v_name = get_name $3 in
      let s      = check_and_get_sort (get_name $2) in
      decl_local_var v_name s None k
    }
  | kind IDENT IDENT ASSIGN term
    {
      let conv_term = Stm.term_to_expr_term in
      let k      = $1 in
      let s      = check_and_get_sort (get_name $2) in
      let v_name = get_name $3 in
      let t      = $5 in
      let v      = Stm.VarT (Stm.build_var v_name s (Stm.Scope !current_proc) ~nature:k) in
      let get_str_expr () = sprintf "%s %s := %s" (E.sort_to_str s)
                                                  (v_name)
                                                  (Stm.term_to_str t) in
      let _ = decl_local_var v_name s (Some (E.Equality (conv_term t))) k
      in
        parser_check_compatibility v t get_str_expr
    }
  | kind IDENT IDENT ASSIGN formula
    {
      let k      = $1 in
      let s      = check_and_get_sort (get_name $2) in
      let v_name = get_name $3 in
      let b      = Stm.boolean_to_expr_formula $5 in
      let get_str_expr () = sprintf "%s %s := %s" (E.sort_to_str s)
                                                  (v_name)
                                                  (E.formula_to_str b) in
      let cond = b in
      let _ = decl_local_var v_name s (Some (E.Condition cond)) k
      in
        parser_check_var_assign v_name s (E.Bool) get_str_expr
    }

kind :
  |
    { E.RealVar }
  | GHOST
    { E.GhostVar }



/* PROCEDURES */

procedure_list :
  procedure
    { () }
  | procedure procedure_list
    { () }


procedure :
  PROCEDURE procedure_name args procedure_sort
  local_declarations
  BEGIN
    program
  END
    {
      let proc_sort = $4 in
      let proc_name  = $2 in
      let args_signature = $3 in
      let proc_input = System.copy_var_table inputVars in
      let proc_local = System.copy_var_table localVars in
      let statements = $7 in
      let (firstLine, lastLine) =
        ( match statements with
            Some (Stm.StSeq xs) -> (Stm.get_fst_st_pos (List.hd xs),
                                    Stm.get_last_st_pos (lastElem xs))
          | _                   -> (0,0)
        ) in
      let proc_info = System.new_proc_info proc_sort
                                           proc_input
                                           proc_local
                                           args_signature
                                           firstLine
                                           lastLine
                                           statements
                                           in

      procedures := (proc_name, proc_info) :: !procedures;
      System.clear_table inputVars;
      System.clear_table localVars;
    }

procedure_sort :
  | { None }
  | COLON IDENT
    {
      let s = check_and_get_sort (get_name $2) in
      Some s
    }


procedure_name:
  IDENT
  {
    let p_name = get_name $1 in
    current_proc := p_name; p_name
  }


args:
  | OPEN_PAREN CLOSE_PAREN
    { [] }
  | OPEN_PAREN arg_list CLOSE_PAREN
    { $2 }

arg_list :
  | arg
    { [$1] }
  | arg COMMA arg_list
    { $1 :: $3 }

arg :
  IDENT COLON IDENT
    {
      let v_name = get_name $1 in
      let s      = check_and_get_sort (get_name $3) in
      let _      = decl_input_var v_name s None
      in
        (v_name, s)
    }


/********************   ATOMIC STATEMENTS   ********************/

atomic_statement_list :
  | atomic_statement
    {
      let (assign_list, code) = $1 in
      (assign_list, [code])
    }
  | atomic_statement atomic_statement_list
    {
      let (assign_body, code) = $1 in
      let (assign_list, code_list) = $2 in
      let st_list = code::code_list in
      let st_str = Stm.statement_to_str 1 (Stm.StSeq st_list) in
      let all_assign = check_no_double_assignment assign_body assign_list st_str
      in
        (all_assign, st_list)
    }


atomic_statement:
  | ST_SKIP SEMICOLON
    {
      ([], Stm.StSkip (None, None))
    }
  | ST_ASSERT formula SEMICOLON
    {
      let cond = $2 in
      let get_str_expr () = sprintf "assert %s" (Stm.boolean_to_str cond) in
      unexpected_statement get_str_expr
    }
  | ST_AWAIT formula SEMICOLON
    {
      let cond = $2 in
      ([], Stm.StAwait (cond, None, None))
    }
  | ST_NONCRITICAL SEMICOLON
    {

      let get_str_expr () = sprintf "noncritical" in
      unexpected_statement get_str_expr
    }
  | ST_CRITICAL SEMICOLON
    {
      let get_str_expr () = sprintf "critical" in
      unexpected_statement get_str_expr
    }
  | term ASSIGN term SEMICOLON
    {
      let t1 = $1 in
      let t2 = $3 in
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str t1)
                                              (Stm.term_to_str t2) in

      let _ = parser_check_compatibility t1 t2 get_str_expr in
      let _ = check_assignment_term t1 get_str_expr in
      let _ = check_assignment_kind t1 (Stm.Term t2) (get_str_expr()) in

      ([Stm.Term t1], Stm.StAssign (t1, Stm.Term t2, None, None))
    }
  | IDENT ASSIGN term SEMICOLON
    {
      let v = get_name $1 in
      let t = $3 in

      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in

      let k = get_var_kind v in
      let var = inject_sort (Stm.construct_var_from_sort
                               v p_name (E.Unknown) k) in
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str var)
                                              (Stm.term_to_str t) in
      let _ = parser_check_compatibility var t get_str_expr in
      let _   = check_assignment_kind var (Stm.Term t) (get_str_expr())in

      ([Stm.Term var], Stm.StAssign (var, Stm.Term t, None, None))
    }
  | IDENT ASSIGN formula SEMICOLON
    {
      let v = get_name $1 in
      let b = $3 in
      let _ = check_sort_var v (Stm.Scope !current_proc) E.Bool in

      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in

      let k = get_var_kind v in
      let var = Stm.construct_var_from_sort v p_name E.Bool k in
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str var)
                                              (Stm.boolean_to_str b) in
      let _   = check_assignment_kind var (Stm.Formula b) (get_str_expr()) in
      ([Stm.Term var], Stm.StAssign (var, Stm.Formula b, None, None))
    }
  | if_ghost_atomic_keyword formula ST_THEN
      atomic_statement_list
    atomic_statements_else_if ST_ENDIF
    {
      let cond = $2 in
      let (then_assign, then_st) = $4 in
      let (else_assign, else_st) = $5 in
      let st = Stm.StIf (cond, Stm.StSeq then_st, else_st, None, None) in
      (* Check no repetition between then_assign and else_assign *)
      let all_assign =
        (* check_no_double_assignment then_assign else_assign st_str *)
        then_assign @ else_assign
      in
        (all_assign, st)
    }
  | while_keyword formula ST_DO atomic_statement_list ST_ENDWHILE
    {
      let cond = Stm.boolean_to_expr_formula $2 in
      let (_, body_code) = $4 in
      let body = Stm.StSeq body_code in
      let get_str_expr () = sprintf "while (%s) do \n%s\n endwhile"
                                    (E.formula_to_str cond)
                                    (Stm.statement_to_str 1 body)
      in

      unexpected_statement get_str_expr
    }
  | choice_keyword atomic_statements_choice ST_ENDCHOICE
    {
      let (_, choice_list) = List.split $2 in
      (* ALE: Eliminate also duplicated variables
         let assign_list = List.flatten assign_lists in *)
      let get_str_expr () = sprintf "choice \n%s\n endchoice"
                              (String.concat " _or_ " $
                                List.map (Stm.statement_to_str 1) choice_list)
      in
      unexpected_statement get_str_expr
    }


atomic_statements_else_if:
  |
    { ([], None) }
  | ST_ELSE atomic_statement_list
    {
      let (assign_list, body_code) = $2 in
      (assign_list, Some (Stm.StSeq body_code))
    }


atomic_statements_choice:
  | atomic_statement_list
    {
      let (assign_list, body_code) = $1 in
      [(assign_list, Stm.StSeq body_code)]
    }
  | atomic_statement_list ST_OR atomic_statements_choice
    {
      let (assign_list, body_code) = $1 in
      (assign_list, Stm.StSeq body_code) :: $3
    }


/*************************   GHOST CODE    ****************************/


ghost_block_or_semicolon :
  | SEMICOLON
    { None }
  | ghost_block
    { $1 }


ghost_block_or_nothing :
  |
    { None }
  | ghost_block
    { $1 }


ghost_block:
  | GHOST_DELIMITER GHOST_DELIMITER
    { None }
  | GHOST_DELIMITER ghost_statement_list GHOST_DELIMITER
    {
      let (_, st_list) = $2 in
      Some (Stm.StSeq st_list)
    }


ghost_statement_list :
  | ghost_statement
    {
      let (assign_list, code) = $1 in
      (assign_list, [code])
    }
  | ghost_statement ghost_statement_list
    {
      let (assign_body, code) = $1 in
      let (assign_list, code_list) = $2 in
      let st_list = code::code_list in
      let st_str = Stm.statement_to_str 1 (Stm.StSeq st_list) in
      let all_assign = check_no_double_assignment assign_body assign_list st_str
      in
        (all_assign, st_list)
    }


ghost_statement:
  | ST_SKIP SEMICOLON
    {
      ([], Stm.StSkip (None, None))
    }
  | ST_ASSERT formula SEMICOLON
    {
      let cond = Stm.boolean_to_expr_formula $2 in
      let get_str_expr () = sprintf "assert %s" (E.formula_to_str cond) in
      unexpected_statement get_str_expr
    }
  | ST_AWAIT formula SEMICOLON
    {
      let cond = Stm.boolean_to_expr_formula $2 in
      let get_str_expr () = sprintf "await %s" (E.formula_to_str cond) in
      unexpected_statement get_str_expr
    }
  | ST_NONCRITICAL SEMICOLON
    {
      let get_str_expr () = sprintf "noncritical" in
      unexpected_statement get_str_expr
    }
  | ST_CRITICAL SEMICOLON
    {
      let get_str_expr () = sprintf "critical" in
      unexpected_statement get_str_expr
    }
  | term ASSIGN term SEMICOLON
    {
      let t1 = $1 in
      let t2 = $3 in
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str t1)
                                              (Stm.term_to_str t2) in

      let _ = parser_check_compatibility t1 t2 get_str_expr in
      let _ = check_assignment_term t1 get_str_expr in
      let _ = check_ghost_assignment_kind t1 (get_str_expr()) in

      ([Stm.Term t1], Stm.StAssign (t1, Stm.Term t2, None, None))
    }
  | IDENT ASSIGN term SEMICOLON
    {
      let v = get_name $1 in
      let t = $3 in

      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in

      let k = get_var_kind v in
      let var = inject_sort (Stm.construct_var_from_sort
                               v p_name (E.Unknown) k) in

      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str var)
                                              (Stm.term_to_str t) in

      let _ = parser_check_compatibility var t get_str_expr in
      let _ = check_ghost_assignment_kind var (get_str_expr())in

      ([Stm.Term var], Stm.StAssign (var, Stm.Term t, None, None))
    }
  | IDENT ASSIGN formula SEMICOLON
    {
      let v = get_name $1 in
      let b = $3 in
      let _ = check_sort_var v (Stm.Scope !current_proc) E.Bool in

      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in

      let k = get_var_kind v in
      let var = Stm.construct_var_from_sort v p_name E.Bool k in

      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str var)
                                              (Stm.boolean_to_str b) in
      let _   = check_ghost_assignment_kind var (get_str_expr()) in
      ([Stm.Term var], Stm.StAssign (var, Stm.Formula b, None, None))
    }
  | if_ghost_atomic_keyword formula ST_THEN
      ghost_statement_list
    ghost_statements_else_if ST_ENDIF
    {
      let cond = $2 in
      let (then_assign, then_st) = $4 in
      let (else_assign, else_st) = $5 in
      let st = Stm.StIf (cond, Stm.StSeq then_st, else_st, None, None) in
      (* No need to search for repeated assignments between both branches
         of an IF statement *)
      let all_assign =
        (* check_no_double_assignment then_assign else_assign st_str *)
        then_assign @ else_assign
      in
        (all_assign, st)
    }
  | while_keyword formula ST_DO ghost_statement_list ST_ENDWHILE
    {
      let cond = Stm.boolean_to_expr_formula $2 in
      let (_, body_code) = $4 in
      let body = Stm.StSeq body_code in
      let get_str_expr () = sprintf "while (%s) do \n%s\n endwhile"
                                    (E.formula_to_str cond)
                                    (Stm.statement_to_str 1 body)
      in
      unexpected_statement get_str_expr
    }
  | choice_keyword ghost_statements_choice ST_ENDCHOICE
    {
      let (_, choice_list) = List.split $2 in
      (* ALE: Eliminates duplicated variables.
         let assign_list = List.flatten assign_lists in *)
      let get_str_expr () = sprintf "choice \n%s\n endchoice"
                              (String.concat " _or_ " $
                                List.map (Stm.statement_to_str 1) choice_list)
      in
      unexpected_statement get_str_expr
    }


ghost_statements_else_if:
  |
    { ([], None) }
  | ST_ELSE ghost_statement_list
    {
      let (assign_list, body_code) = $2 in
      (assign_list, Some (Stm.StSeq body_code))
    }


ghost_statements_choice:
  | ghost_statement_list
    {
      let (assign_list, body_code) = $1 in
      [(assign_list, Stm.StSeq body_code)]
    }
  | ghost_statement_list ST_OR ghost_statements_choice
    {
      let (assign_list, body_code) = $1 in
      (assign_list, Stm.StSeq body_code) :: $3
    }


/*************************    PROGRAM    ************************/


program :
  |
    { None }
  | statement_list
    { Some (Stm.StSeq $1) }


line_label_list :
  |
    { () }
  | line_label line_label_list
    { () }


line_label :
  | COLON IDENT
    {
      let label_name = get_name $2 in
        System.add_single_label labelTbl label_name !pos
    }
  | COLON IDENT OPEN_BRACKET
    {
      let label_name = get_name $2 in
        System.add_open_label labelTbl label_name !pos
    }
  | COLON IDENT CLOSE_BRACKET
    {
      let label_name = get_name $2 in
        System.add_close_label labelTbl label_name (!pos-1)
    }

if_keyword:
  | line_label_list ST_IF
    { pos := !pos+1; !pos-1 }

if_ghost_atomic_keyword:
  | line_label_list ST_IF
    { }

while_keyword:
  | line_label_list ST_WHILE
    { pos := !pos+1; !pos-1 }


choice_keyword:
  | line_label_list ST_CHOICE
    { pos := !pos+1; !pos-1 }


/***********************    STATEMENTS    **********************/


statement_list :
  | statement line_label_list
    { [$1] }
  | statement statement_list
    { ($1 :: $2) }


statement:
  | line_label_list ST_SKIP ghost_block_or_semicolon
    {
      let g_code  = $3 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StSkip (g_code, Some st_info) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list ST_ASSERT formula ghost_block_or_semicolon
    {
      let cond    = $3 in
      let g_code  = $4 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StAssert (cond, g_code, Some st_info) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list ST_AWAIT formula ghost_block_or_semicolon
    {
      let cond    = $3 in
      let g_code  = $4 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StAwait (cond, g_code, Some st_info) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list ST_NONCRITICAL ghost_block_or_semicolon
    {
      let g_code  = $3 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StNonCrit (g_code, Some st_info) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list ST_CRITICAL ghost_block_or_semicolon
    {
      let g_code  = $3 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StCrit (g_code, Some st_info) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list term ASSIGN term ghost_block_or_semicolon
    {
      let t1 = $2 in
      let t2 = $4 in
      let g_code = $5 in
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str t1)
                                              (Stm.term_to_str t2) in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StAssign (t1, Stm.Term t2, g_code, Some st_info) in

      let _ = parser_check_compatibility t1 t2 get_str_expr in
      let _ = check_assignment_term t1 get_str_expr in
      let _ = check_assignment_kind t1 (Stm.Term t2) (get_str_expr()) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list IDENT ASSIGN term ghost_block_or_semicolon
    {
      let v = get_name $2 in
      let t = $4 in
      let g_code = $5 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in

      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in

      let k = get_var_kind v in
      let var = inject_sort (Stm.construct_var_from_sort
                              v p_name (E.Unknown) k) in
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str var)
                                              (Stm.term_to_str t) in

      let _ = parser_check_compatibility var t get_str_expr in

      let st = Stm.StAssign (var, Stm.Term t, g_code, Some st_info) in

      let _ = check_assignment_kind var (Stm.Term t) (get_str_expr()) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list IDENT ASSIGN formula ghost_block_or_semicolon
    {
      let v = get_name $2 in
      let b = $4 in
      let g_code = $5 in
      let _ = check_sort_var v (Stm.Scope !current_proc) E.Bool in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in

      let k = get_var_kind v in
      let var = Stm.construct_var_from_sort v p_name E.Bool k in

      let st = Stm.StAssign (var, Stm.Formula b, g_code, Some st_info) in

      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str var)
                                              (Stm.boolean_to_str b) in
      let _ = check_assignment_kind var (Stm.Formula b) (get_str_expr()) in
      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list term ASSIGN CALL IDENT OPEN_PAREN params CLOSE_PAREN
      ghost_block_or_semicolon
    {
      let t = $2 in
      let proc_name = get_name $5 in
      let ps = $7 in
      let g_code = $9 in
      let get_str_expr () = sprintf "%s = %s(%s)"
                              (Stm.term_to_str t)
                              (proc_name)
                              (String.concat "," $ List.map Stm.term_to_str ps) in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = 0;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let _ = Hashtbl.add call_points proc_name !pos in
      let st = Stm.StCall (Some t, proc_name, ps, g_code, Some st_info) in

      let _ = check_assignment_term t get_str_expr in
      let _ = check_assignment_kind t (Stm.Term t) (get_str_expr()) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos + 1;
      st
    }
  | line_label_list IDENT ASSIGN CALL IDENT OPEN_PAREN params CLOSE_PAREN
      ghost_block_or_semicolon
    {
      let v = get_name $2 in
      let proc_name = get_name $5 in
      let ps = $7 in
      let g_code = $9 in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = 0;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in

      let p_name = if System.mem_var localVars v ||
                      System.mem_var inputVars v then
                        Stm.Scope !current_proc
                   else
                        Stm.GlobalScope in
      let k = get_var_kind v in
      let var = inject_sort (Stm.construct_var_from_sort
                              v p_name (E.Unknown) k) in
      let get_str_expr () = sprintf "%s = %s(%s)"
                              (Stm.term_to_str var)
                              (proc_name)
                              (String.concat "," $ List.map Stm.term_to_str ps) in
      let _ = Hashtbl.add call_points proc_name !pos in
      let st = Stm.StCall (Some var, proc_name, ps, g_code, Some st_info) in
      let _ = check_assignment_kind var (Stm.Term var) (get_str_expr()) in

      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos + 1;
      st
    }
  | line_label_list term POINTER LOCK lock_pos ghost_block_or_semicolon
    {
      let get_str_expr () = sprintf "%s->lock%s" (Stm.term_to_str $2) (lock_pos_to_str $5) in
      let a = parser_check_type check_type_addr $2 E.Addr get_str_expr in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      (* ALE: I'm not verifying whether I am working with a non ghost address *)
      let g_code = $6 in
      let st = Stm.StUnit ((match $5 with
                           | None   -> Stm.UnitLock a
                           | Some i -> Stm.UnitLockAt (a,i)
                           ), g_code, Some st_info) in
      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | line_label_list term POINTER UNLOCK lock_pos ghost_block_or_semicolon
    {
      let get_str_expr () = sprintf "%s->unlock%s" (Stm.term_to_str $2) (lock_pos_to_str $5) in
      let a = parser_check_type check_type_addr $2 E.Addr get_str_expr in
      let st_info = { Stm.pos             = !pos;
                      Stm.next_pos        = !pos+1;
                      Stm.else_pos        = !pos+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      (* ALE: I'm not verifying whether I am working with a non ghost address *)
      let g_code = $6 in
      let st = Stm.StUnit ((match $5 with
                            | None   -> Stm.UnitUnlock a
                            | Some i -> Stm.UnitUnlockAt (a,i)
                            ), g_code, Some st_info) in
      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos+1;
      st
    }
  | if_keyword formula ST_THEN statement_list statements_else_if ST_ENDIF
      ghost_block_or_nothing
    {
      let n       = $1 in
      let cond    = $2 in
      let then_st = $4 in
      let else_st = $5 in
      let g_code  = $7 in

      let (next_p, else_p) =
        match (then_st, else_st) with
        | ([], None) -> (n+1, n+1)
        | ([], Some ys) -> (Stm.get_last_st_pos ys + 1, Stm.get_fst_st_pos ys)
        | (xs, None) -> begin
                          let last_xs = lastElem xs in
                          cond_stm_list := n :: (!cond_stm_list);
                          (n+1, (Stm.get_last_st_info last_xs).Stm.else_pos);
                        end
        | (xs, Some ys) -> begin
                             let last_xs = lastElem xs in
                             cond_stm_list := n :: (!cond_stm_list);
                             (match last_xs with
                              | Stm.StIf (_,_,Some _,_,_) -> ()
                              | _ -> (Stm.get_st_info last_xs).Stm.else_pos <-
                                        (Stm.get_last_st_info ys).Stm.next_pos);
                             (n+1, Stm.get_fst_st_pos ys)
                           end in

      let st_info = { Stm.pos             = n;
                      Stm.next_pos        = next_p;
                      Stm.else_pos        = else_p;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in

      let st = Stm.StIf (cond, Stm.StSeq then_st, else_st, g_code, Some st_info) in

      Hashtbl.replace pos_st n (!current_proc, st);
      st
    }
  | while_keyword formula ST_DO statement_list ST_ENDWHILE
    {
      let n = $1 in
      let cond = $2 in
      let body = Stm.StSeq $4 in
      let end_p = (match $4 with
                     [] -> n+1
                   | xs -> Stm.get_last_st_pos (lastElem xs) + 1
                  ) in
      (* Update jump position for last statement in the body *)
      let _ = match $4 with
                [] -> ()
              | xs -> let lst = lastElem xs in
                      let zs = match lst with
                               | Stm.StSelect (ys,_,_) -> ys
                               | Stm.StIf (_,t,e,_,info) ->
                                  begin
                                    match e with
                                    | Some s -> [t;s]
                                    | None   -> begin
                                                  (match info with
                                                   | None -> ()
                                                   | Some i -> i.Stm.else_pos <- n);
                                                  [t]
                                                end
                                  end
                               | _ -> [lst]
                      in
                        List.iter (fun st ->
                          (Stm.get_last_st_info st).Stm.next_pos <- n
                        ) zs in
      let st_info = { Stm.pos             = n;
                      Stm.next_pos        = n+1;
                      Stm.else_pos        = end_p;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = [];
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StWhile (cond, body, None, Some st_info) in

      Hashtbl.replace pos_st n (!current_proc, st);
      st
    }
  | choice_keyword statements_choice ST_ENDCHOICE
    {
      let n = $1 in
      let opt = $2 in
      let end_p = Stm.get_last_st_pos (lastElem opt) + 1 in
      let opt_p = List.map Stm.get_fst_st_pos opt in
      let st_info = { Stm.pos             = n;
                      Stm.next_pos        = n+1;
                      Stm.else_pos        = n+1;
                      Stm.call_pos        = None;
                      Stm.opt_pos         = opt_p;
                      Stm.called_from_pos = [];
                      Stm.return_pos      = []; } in
      let st = Stm.StSelect (opt, None, Some st_info) in

      List.iter (fun x ->
                  (Stm.get_last_st_info x).Stm.next_pos <- end_p) opt;
      Hashtbl.replace pos_st n (!current_proc, st);
      st
    }
  | line_label_list OPEN_SET atomic_statement_list CLOSE_SET
      ghost_block_or_nothing
    {
      let (_, st_list) = $3 in
      let g_code       = $5 in
      let st_info      = { Stm.pos              = !pos;
                           Stm.next_pos         = !pos+1;
                           Stm.else_pos         = !pos+1;
                           Stm.call_pos         = None;
                           Stm.opt_pos          = [];
                           Stm.called_from_pos  = [];
                           Stm.return_pos       = []; } in
      let st           = Stm.StAtomic (st_list, g_code, Some st_info) in
      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos + 1;
      st
    }
  | line_label_list CALL IDENT OPEN_PAREN params CLOSE_PAREN
      ghost_block_or_semicolon

    {
      let proc_name = get_name $3 in
      let ps = $5 in
      let g_code = $7 in
      let st_info = {Stm.pos              = !pos;
                     Stm.next_pos         = !pos+1;
                     Stm.else_pos         = 0;
                     Stm.call_pos         = None;
                     Stm.opt_pos          = [];
                     Stm.called_from_pos  = [];
                     Stm.return_pos       = []; } in
      let _ = Hashtbl.add call_points proc_name !pos in
      let st = Stm.StCall (None, proc_name, ps, g_code, Some st_info) in
      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos + 1;
      st
    }
  | line_label_list RETURN OPEN_PAREN maybe_term CLOSE_PAREN
      ghost_block_or_semicolon
    {
      let t = $4 in
      let g_code = $6 in
      let st_info = {Stm.pos              = !pos;
                     Stm.next_pos         = 0;
                     Stm.else_pos         = !pos+1;
                     Stm.call_pos         = None;
                     Stm.opt_pos          = [];
                     Stm.called_from_pos  = [];
                     Stm.return_pos       = []; } in
      let _ = Hashtbl.add return_points !current_proc !pos in
      let st = Stm.StReturn (t, g_code, Some st_info) in
      Hashtbl.replace pos_st !pos (!current_proc, st);
      pos := !pos + 1;
      st
    }


lock_pos :
  |
    { None }
  | OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "[%s]" (Stm.term_to_str $2) in
      let i = parser_check_type check_type_int $2 E.Int get_str_expr in
        Some i
    }


maybe_term :
  |
    { None }
  | term
    { Some $1 }


term_list :
  | term COMMA term
    { [$1;$3] }
  | term COMMA term_list
    { $1 :: $3 }


params :
  |
    { [] }
  | param_list
    { $1 }


param_list :
  | term
    { [$1] }
  | term COMMA param_list
    { $1 :: $3 }


statements_else_if:
  |
    { None }
  | ST_ELSE statement_list
    { Some (Stm.StSeq $2) }


statements_choice:
  | statement_list
    { [Stm.StSeq $1] }
  | statement_list ST_OR statements_choice
    { ((Stm.StSeq $1) :: $3) }


/***********************    FORMULAS    ************************/



/* FORMULAS */

formula :
  | OPEN_PAREN formula CLOSE_PAREN
      { $2 }
  | atom
      { F.atom_to_formula $1 }
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
  | formula EQUALS formula
      { F.Iff ($1, $3) }
  | OPEN_PAREN IDENT CLOSE_PAREN
      {
        let v        = get_name $2 in
        let is_input = System.mem_var inputVars v in
        let is_local = System.mem_var localVars v in
        let k        = get_var_kind v in

        if is_input || is_local then
          let c_proc = if !current_proc <> "" then
                         Stm.Scope !current_proc
                       else
                         Stm.GlobalScope
          in
            F.atom_to_formula (Stm.BoolVar (Stm.build_var v E.Bool c_proc ~nature:k))
        else
            F.atom_to_formula (Stm.BoolVar (Stm.build_var v E.Bool Stm.GlobalScope ~nature:k))
      }



/* LITERALS */

atom :
  | APPEND OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "append(%s,%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5)
                                                       (Stm.term_to_str $7) in
      let p1   = parser_check_type check_type_path $3 E.Path get_str_expr in
      let p2   = parser_check_type check_type_path $5 E.Path get_str_expr in
      let pres = parser_check_type check_type_path $7 E.Path get_str_expr in
        Stm.Append (p1,p2,pres)
    }
  | REACH OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "reach(%s,%s,%s,%s)" (Stm.term_to_str $3)
                                                         (Stm.term_to_str $5)
                                                         (Stm.term_to_str $7)
                                                         (Stm.term_to_str $9) in
      let h      = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a_from = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr $7 E.Addr get_str_expr in
      let p      = parser_check_type check_type_path $9 E.Path get_str_expr in
        Stm.Reach (h,a_from,a_to,p)
    }

  | ORDERLIST OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "orderlist(%s,%s,%s)" (Stm.term_to_str $3)
                                                          (Stm.term_to_str $5)
                                                          (Stm.term_to_str $7) in
      let h      = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a_from = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr $7 E.Addr get_str_expr in
        Stm.OrderList (h,a_from,a_to)
    }
  | SKIPLIST OPEN_PAREN term COMMA term COMMA term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "skiplist(%s,%s,%s,%s,%s)"
                                        (Stm.term_to_str $3)
                                        (Stm.term_to_str $5)
                                        (Stm.term_to_str $7)
                                        (Stm.term_to_str $9)
                                        (Stm.term_to_str $11) in
      let h      = parser_check_type check_type_mem      $3 E.Mem get_str_expr in
      let s      = parser_check_type check_type_set      $5 E.Set get_str_expr in
      let l      = parser_check_type check_type_int      $7 E.Int get_str_expr in
      let a_from = parser_check_type check_type_addr     $9 E.Addr get_str_expr in
      let a_to   = parser_check_type check_type_addr    $11 E.Addr get_str_expr in
      let elems  = parser_check_type check_type_setelem $13 E.SetElem get_str_expr in
        Stm.Skiplist (h,s,l,a_from,a_to,elems)
    }
  | IN OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "in(%s,%s)" (Stm.term_to_str $3)
                                                (Stm.term_to_str $5) in
      let a = parser_check_type check_type_addr $3 E.Addr get_str_expr in
      let r = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Stm.In (a,r)
    }
  | SUBSETEQ OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "subseteq(%s,%s)" (Stm.term_to_str $3)
                                                      (Stm.term_to_str $5) in
      let s = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let r = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Stm.SubsetEq(s,r)
    }
  | INTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "tin(%s, %s" (Stm.term_to_str $3)
                                                 (Stm.term_to_str $5) in
      let th = parser_check_type check_type_tid  $3 E.Tid get_str_expr in
      let s  = parser_check_type check_type_setth $5 E.SetTh get_str_expr in
        Stm.InTh (th,s)
    }
  | SUBSETEQTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "tsubseteq(%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5) in
      let r = parser_check_type check_type_setth $3 E.SetTh get_str_expr in
      let s = parser_check_type check_type_setth $5 E.SetTh get_str_expr in
        Stm.SubsetEqTh(r,s)
    }
  | ININT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "iin(%s,%s)" (Stm.term_to_str $3)
                                                 (Stm.term_to_str $5) in
      let i = parser_check_type check_type_int $3 E.Int get_str_expr in
      let s = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Stm.InInt (i,s)
    }
  | SUBSETEQINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "isubseteq(%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5) in
      let r = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let s = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Stm.SubsetEqInt(r,s)
    }
  | INELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "ein(%s,%s)" (Stm.term_to_str $3)
                                                 (Stm.term_to_str $5) in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let s = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Stm.InElem (e,s)
    }
  | SUBSETEQELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "esubseteq(%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5) in
      let r = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Stm.SubsetEqElem(r,s)
    }
  | SETPAIRIN OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spin(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let p = parser_check_type check_type_pair $3 E.Pair get_str_expr in
      let s = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Stm.InPair (p,s)
    }
  | SETPAIRSUBSETEQ OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spsubseteq(%s,%s)" (Stm.term_to_str $3)
                                                        (Stm.term_to_str $5) in
      let r = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
      let s = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Stm.SubsetEqPair(r,s)
    }
  | HASHTBL OPEN_PAREN term COMMA term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "hashtbl(%s,%s,%s,%s,%s)"
                                        (Stm.term_to_str $3)
                                        (Stm.term_to_str $5)
                                        (Stm.term_to_str $7)
                                        (Stm.term_to_str $9)
                                        (Stm.term_to_str $11) in
      let h  = parser_check_type check_type_mem        $3 E.Mem get_str_expr in
      let s  = parser_check_type check_type_set        $5 E.Set get_str_expr in
      let se = parser_check_type check_type_setelem    $7 E.SetElem get_str_expr in
      let bb = parser_check_type check_type_bucketarr  $9 E.BucketArray get_str_expr in
      let i  = parser_check_type check_type_int       $11 E.Int get_str_expr in
        Stm.Hashtbl (h,s,se,bb,i)
    }
  | term MATH_LESS term
    {
      let get_str_expr () = sprintf "%s < %s" (Stm.term_to_str $1)
                                              (Stm.term_to_str $3) in
      try
        let e1 = parser_check_type check_type_elem $1 E.Elem get_str_expr in
        let e2 = parser_check_type check_type_elem $3 E.Elem get_str_expr in
          Stm.LessElem (e1, e2)
      with
        _ -> let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
             let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
               Stm.Less (i1, i2)
    }
  | term MATH_GREATER term
    {
      let get_str_expr () = sprintf "%s > %s" (Stm.term_to_str $1)
                                              (Stm.term_to_str $3) in
      try
        let e1 = parser_check_type check_type_elem $1 E.Elem get_str_expr in
        let e2 = parser_check_type check_type_elem $3 E.Elem get_str_expr in
          Stm.GreaterElem (e1, e2)
      with
        _ -> let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
             let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
               Stm.Greater (i1, i2)
    }
  | term MATH_LESS_EQ term
    {
      let get_str_expr () = sprintf "%s <= %s" (Stm.term_to_str $1)
                                               (Stm.term_to_str $3) in
      let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.LessEq (i1, i2)
    }
  | term MATH_GREATER_EQ term
    {
      let get_str_expr () = sprintf "%s >= %s" (Stm.term_to_str $1)
                                               (Stm.term_to_str $3) in
      let i1 = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2 = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.GreaterEq (i1, i2)
    }
  | equals
    { Stm.Eq($1) }
  | disequals
    { Stm.InEq($1) }



/* EQUALS */

equals :
  | term EQUALS term
    {
      let get_str_expr () = sprintf "%s = %s" (Stm.term_to_str $1)
                                              (Stm.term_to_str $3) in
      let t1 = $1 in
      let t2 = $3 in

      parser_check_compatibility t1 t2 get_str_expr ;
      (inject_sort t1, inject_sort t2)
    }


/* DISEQUALS */

disequals :
  | term NOT_EQUALS term
    {
      let get_str_expr () = sprintf "%s != %s" (Stm.term_to_str $1)
                                               (Stm.term_to_str $3) in
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
    { Stm.SetT($1) }
  | elem
    { Stm.ElemT($1) }
  | tid
    { Stm.TidT($1) }
  | addr
    { Stm.AddrT($1) }
  | cell
    { Stm.CellT($1) }
  | setth
    { Stm.SetThT($1) }
  | setint
    { Stm.SetIntT($1) }
  | setelem
    { Stm.SetElemT($1) }
  | setpair
    { Stm.SetPairT($1) }
  | path
    { Stm.PathT($1) }
  | mem
    { Stm.MemT($1) }
  | integer
    { Stm.IntT($1) }
  | pair
    { Stm.PairT($1) }
  | mark
    { Stm.MarkT($1) }
  | arraylookup
    { $1 }
  | arrayupd
    { $1 }
  | bucket
    { Stm.BucketT($1) }
  | lock
    { Stm.LockT($1) }
  | OPEN_PAREN term CLOSE_PAREN
    { $2 }



/* IDENT */

ident :
  | IDENT
    {
      let v        = get_name $1 in
      let is_input = System.mem_var inputVars v in
      let is_local = System.mem_var localVars v in
      let k        = get_var_kind v in

      if is_input || is_local then
        let c_proc = if !current_proc <> "" then
                       Stm.Scope !current_proc
                     else
                       Stm.GlobalScope
        in
          inject_sort $ Stm.VarT
                          (Stm.build_var (get_name $1) E.Unknown c_proc ~nature:k)
      else
          inject_sort $ Stm.VarT (Stm.build_var v E.Unknown Stm.GlobalScope ~nature:k)
    }


/* SET terms*/

set :
  | EMPTYSET
    { Stm.EmptySet }
  | OPEN_SET term CLOSE_SET
    {
      let get_str_expr() = sprintf "{ %s }" (Stm.term_to_str $2) in
      let a = parser_check_type check_type_addr $2 E.Addr get_str_expr in
        Stm.Singl(a)
    }
  | UNION OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "union(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let s2 = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Stm.Union(s1,s2)
    }
  | INTR OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "intr(%s,%s)" (Stm.term_to_str $3)
                                                 (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let s2 = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Stm.Intr(s1,s2)
    }
  | SETDIFF OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "diff(%s,%s)" (Stm.term_to_str $3)
                                                 (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_set  $3 E.Set get_str_expr in
      let s2 = parser_check_type check_type_set  $5 E.Set get_str_expr in
        Stm.Setdiff(s1,s2)
    }
  | PATH2SET OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "path2set(%s)" (Stm.term_to_str $3) in
      let p = parser_check_type check_type_path $3 E.Path get_str_expr in
        Stm.PathToSet(p)
    }
  | ADDR2SET OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "addr2set(%s,%s)" (Stm.term_to_str $3)
                                                      (Stm.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
        Stm.AddrToSet(h,a)
    }
  | ADDR2SET OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "addr2set(%s,%s,%s)" (Stm.term_to_str $3)
                                                         (Stm.term_to_str $5)
                                                         (Stm.term_to_str $7) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let l = parser_check_type check_type_int  $7 E.Int get_str_expr in
        Stm.AddrToSetAt(h,a,l)
    }
  | term DOT BREGION
    {
      let get_str_expr () = sprintf "%s.bregion" (Stm.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        Stm.BucketRegion(b)
    }


/* ELEM terms */

elem :
  | term DOT DATA
    {
      let get_str_expr () = sprintf "%s.data" (Stm.term_to_str $1) in
      let c = parser_check_type check_type_cell  $1 E.Cell get_str_expr in
        Stm.CellData(c)
    }
  | term POINTER DATA
    {
      let get_str_expr () = sprintf "%s->data" (Stm.term_to_str $1) in
      let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
        Stm.PointerData a
    }
  | HAVOCLISTELEM OPEN_PAREN CLOSE_PAREN
    {
      Stm.HavocListElem
    }
  | HAVOCSKIPLISTELEM OPEN_PAREN CLOSE_PAREN
    {
      Stm.HavocSkiplistElem
    }
  | LOWEST_ELEM
    {
      Stm.LowestElem
    }
  | HIGHEST_ELEM
    {
      Stm.HighestElem
    }



/* THID terms */

tid :
  | term DOT LOCKID
    {

      let get_str_expr () = sprintf "%s.lockid" (Stm.term_to_str $1) in
      try
        let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
          Stm.CellLockId(c)
      with _ ->
        let l = parser_check_type check_type_lock $1 E.Lock get_str_expr in
          Stm.LockId(l)
    }
  | SHARP
    {
      Stm.NoTid
    }
  | term POINTER LOCKID
    {
      let get_str_expr () = sprintf "%s->lockid" (Stm.term_to_str $1) in
      let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
        Stm.PointerLockid a
    }
  | ME
    {
      Stm.VarTh (Stm.build_var Sys.me_tid E.Tid Stm.GlobalScope)
    }
  | TIDOF OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "tid_of(%s)" (Stm.term_to_str $3) in
      let p = parser_check_type check_type_pair $3 E.Pair get_str_expr in
        Stm.PairTid(p)
    }
  | term DOT BTID
    {
      let get_str_expr () = sprintf "%s.btid" (Stm.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        Stm.BucketTid(b)
    }


/* ADDR terms */

addr :
  | NULL
    { Stm.Null }
  | term DOT NEXT
    {
      let get_str_expr () = sprintf "%s.next" (Stm.term_to_str $1) in
      let c = parser_check_type check_type_cell  $1 E.Cell get_str_expr in
        Stm.Next(c)
    }
  | term DOT NEXTAT OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s.nextat[%s]" (Stm.term_to_str $1)
                                                    (Stm.term_to_str $5) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
      let l = parser_check_type check_type_int $5 E.Int get_str_expr in
        Stm.NextAt(c,l)
    }
  | term DOT ARR OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s.arr[%s]" (Stm.term_to_str $1)
                                                 (Stm.term_to_str $5) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
      let l = parser_check_type check_type_int $5 E.Int get_str_expr in
        Stm.ArrAt(c,l)
    }
  | FIRSTLOCKED OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "firstlocked(%s,%s)" (Stm.term_to_str $3)
                                                         (Stm.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let p = parser_check_type check_type_path $5 E.Path get_str_expr in
        Stm.FirstLocked(h,p)
    }
  | LASTLOCKED OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "lastlocked(%s,%s)" (Stm.term_to_str $3)
                                                        (Stm.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let p = parser_check_type check_type_path $5 E.Path get_str_expr in
        Stm.LastLocked(h,p)
    }
  | MALLOC OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "malloc(%s,%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5)
                                                       (Stm.term_to_str $7)
      in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let t = parser_check_type check_type_tid $7 E.Tid get_str_expr in

        Stm.Malloc(e,a,t)
    }
  | MALLOCSL OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mallocSL(%s,%s)" (Stm.term_to_str $3)
                                                      (Stm.term_to_str $5)
      in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let l = parser_check_type check_type_int $5 E.Int get_str_expr in
        Stm.MallocSL(e,l)
    }
  | MALLOCSLK OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mallocSLK(%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5)
      in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let l = parser_check_type check_type_int $5 E.Int get_str_expr in
        Stm.MallocSLK(e,l)
    }
  | term POINTER NEXT
    {
      let get_str_expr () = sprintf "%s->next" (Stm.term_to_str $1) in
      let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
        Stm.PointerNext a
    }
  | term POINTER NEXTAT OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s->nextat[%s]" (Stm.term_to_str $1)
                                                     (Stm.term_to_str $5) in
      let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
      let l = parser_check_type check_type_int $5 E.Int get_str_expr in
        Stm.PointerNextAt (a,l)
    }
  | term POINTER ARR OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s->arr[%s]" (Stm.term_to_str $1)
                                                   (Stm.term_to_str $5) in
      let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
      let l = parser_check_type check_type_int $5 E.Int get_str_expr in
        Stm.PointerArrAt (a,l)
    }
  | term DOT BINIT
    {
      let get_str_expr () = sprintf "%s.binit" (Stm.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        Stm.BucketInit(b)
    }
  | term DOT BEND
    {
      let get_str_expr () = sprintf "%s.bend" (Stm.term_to_str $1) in
      let b = parser_check_type check_type_bucket $1 E.Bucket get_str_expr in
        Stm.BucketEnd(b)
    }



/* CELL terms */

cell :
  | ERROR
    { Stm.Error }
  | MKCELL OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mkcell(%s,%s,%s)"
                                           (Stm.term_to_str $3)
                                           (Stm.term_to_str $5)
                                           (Stm.term_to_str $7) in
      let d  = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let a  = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let th = parser_check_type check_type_tid $7 E.Tid get_str_expr in
        Stm.MkCell(d,a,th)
    }

  | MKCELL OPEN_PAREN term COMMA
                      OPEN_BRACKET term_list CLOSE_BRACKET COMMA
                      OPEN_BRACKET term_list CLOSE_BRACKET CLOSE_PAREN
    {
      let list_term_to_str ts = String.concat "," (List.map Stm.term_to_str ts) in
      let addrs_str = list_term_to_str $6 in
      let tids_str = list_term_to_str $10 in
      let get_str_expr () = sprintf "mkcell(%s,[%s],[%s])"
                                           (Stm.term_to_str $3)
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
        Stm.MkSLKCell(e,addrs,tids)
    }
  | MKCELL OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mkslcell(%s,%s,%s,%s)"
                                           (Stm.term_to_str $3)
                                           (Stm.term_to_str $5)
                                           (Stm.term_to_str $7)
                                           (Stm.term_to_str $9) in
      let e  = parser_check_type check_type_elem $3 E.Elem get_str_expr in
      let aa = parser_check_type check_type_addrarr $5 E.AddrArray get_str_expr in
      let ta = parser_check_type check_type_tidarr $7 E.TidArray get_str_expr in
      let l  = parser_check_type check_type_int $9 E.Int get_str_expr in
        Stm.MkSLCell(e,aa,ta,l)
    }
  | term DOT LOCK
    {
      let get_str_expr () = sprintf "%s.lock" (Stm.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
        Stm.CellLock(c)
    }
  | term DOT UNLOCK
    {

      let get_str_expr () = sprintf "%s.unlock" (Stm.term_to_str $1) in
      let c = parser_check_type check_type_cell $1 E.Cell get_str_expr in
        Stm.CellUnlock(c)
    }
  | MEMORY_READ OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "%s [ %s ]" (Stm.term_to_str $3)
                                                (Stm.term_to_str $5) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
        Stm.CellAt(h,a)
    }


/* MARK terms*/

mark :
  | MARK_T
    { Stm.MarkTrue }
  | MARK_F
    { Stm.MarkFalse }
  | term DOT MARKED
    {
      let get_str_expr () = sprintf "%s.marked" (Stm.term_to_str $1) in
      let c = parser_check_type check_type_cell  $1 E.Cell get_str_expr in
        Stm.Marked(c)
    }
  | term POINTER MARKED
    {
      let get_str_expr () = sprintf "%s->marked" (Stm.term_to_str $1) in
      let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
        Stm.PointerMarked a
    }



/* SETTH terms*/

setth :
  | EMPTYSETTH
    { Stm.EmptySetTh }
  | SINGLETH OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tsingle(%s)" (Stm.term_to_str $3) in
      let th = parser_check_type check_type_tid  $3 E.Tid get_str_expr in
        Stm.SinglTh(th)
    }
  | UNIONTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tunion(%s,%s)" (Stm.term_to_str $3)
                                                   (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setth  $3 E.SetTh get_str_expr in
      let s2 = parser_check_type check_type_setth  $5 E.SetTh get_str_expr in
        Stm.UnionTh(s1,s2)
    }
  | INTRTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tintr(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setth  $3 E.SetTh get_str_expr in
      let s2 = parser_check_type check_type_setth  $5 E.SetTh get_str_expr in
        Stm.IntrTh(s1,s2)
    }
  | SETDIFFTH OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "tdiff(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setth  $3 E.SetTh get_str_expr in
      let s2 = parser_check_type check_type_setth  $5 E.SetTh get_str_expr in
        Stm.SetdiffTh(s1,s2)
    }


/* SETINT terms*/
setint :
  | EMPTYSETINT
     { Stm.EmptySetInt }
  | SINGLEINT OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "isingle(%s)" (Stm.term_to_str $3) in
      let th = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.SinglInt(th)
    }
  | UNIONINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "iunion(%s,%s)" (Stm.term_to_str $3)
                                                   (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let s2 = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Stm.UnionInt(s1,s2)
    }
  | INTRINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "iintr(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let s2 = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Stm.IntrInt(s1,s2)
    }
  | SETDIFFINT OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "idiff(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setint $3 E.SetInt get_str_expr in
      let s2 = parser_check_type check_type_setint $5 E.SetInt get_str_expr in
        Stm.SetdiffInt(s1,s2)
    }


/* SETELEM terms*/
setelem :
  | EMPTYSETELEM
     { Stm.EmptySetElem }
  | SINGLEELEM OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "esingle(%s)" (Stm.term_to_str $3) in
      let e = parser_check_type check_type_elem $3 E.Elem get_str_expr in
        Stm.SinglElem(e)
    }
  | UNIONELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "eunion(%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s2 = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Stm.UnionElem(s1,s2)
    }
  | INTRELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "eintr(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s2 = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Stm.IntrElem(s1,s2)
    }
  | SETDIFFELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "ediff(%s,%s)" (Stm.term_to_str $3)
                                                  (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setelem $3 E.SetElem get_str_expr in
      let s2 = parser_check_type check_type_setelem $5 E.SetElem get_str_expr in
        Stm.SetdiffElem(s1,s2)
    }
  | SET2ELEM OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "set2elem(%s,%s)" (Stm.term_to_str $3)
                                                      (Stm.term_to_str $5) in
      let m = parser_check_type check_type_mem $3 E.Mem get_str_expr in
      let s = parser_check_type check_type_set $5 E.Set get_str_expr in
        Stm.SetToElems(s,m)
    }


/* SETPAIR terms*/
setpair :
  | SETPAIREMPTY
     { Stm.EmptySetPair }
  | SETPAIRSINGLE OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spsingle(%s)" (Stm.term_to_str $3) in
      let p = parser_check_type check_type_pair $3 E.Pair get_str_expr in
        Stm.SinglPair(p)
    }
  | SETPAIRUNION OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spunion(%s,%s)" (Stm.term_to_str $3)
                                                    (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setpair  $3 E.SetPair get_str_expr in
      let s2 = parser_check_type check_type_setpair  $5 E.SetPair get_str_expr in
        Stm.UnionPair(s1,s2)
    }
  | SETPAIRINTR OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spintr(%s,%s)" (Stm.term_to_str $3)
                                                   (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setpair  $3 E.SetPair get_str_expr in
      let s2 = parser_check_type check_type_setpair  $5 E.SetPair get_str_expr in
        Stm.IntrPair(s1,s2)
    }
  | SETPAIRDIFF OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr() = sprintf "spdiff(%s,%s)" (Stm.term_to_str $3)
                                                   (Stm.term_to_str $5) in
      let s1 = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
      let s2 = parser_check_type check_type_setpair $5 E.SetPair get_str_expr in
        Stm.SetdiffPair(s1,s2)
    }



/* PATH terms */

path :
  | EPSILON
    { Stm.Epsilon }
  | SINGLE_PATH OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "[ %s ]" (Stm.term_to_str $3) in
      let a = parser_check_type check_type_addr $3 E.Addr get_str_expr in
        Stm.SimplePath(a)
    }
  | GETP OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "getp(%s,%s,%s)" (Stm.term_to_str $3)
                                                     (Stm.term_to_str $5)
                                                     (Stm.term_to_str $7) in
      let h     = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let first = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let last  = parser_check_type check_type_addr $7 E.Addr get_str_expr in
        Stm.GetPath(h,first,last)
  }


/* MEM terms */

mem :
  | UPDATE OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "update(%s,%s,%s)" (Stm.term_to_str $3)
                                                       (Stm.term_to_str $5)
                                                       (Stm.term_to_str $7) in
      let h = parser_check_type check_type_mem  $3 E.Mem get_str_expr in
      let a = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let c = parser_check_type check_type_cell $7 E.Cell get_str_expr in
        Stm.Update(h,a,c)
    }


/* INTEGER terms*/

integer :
  | NUMBER
    { Stm.IntVal $1 }
  | MATH_MINUS term %prec MATH_NEG
    {
      let get_str_expr () = sprintf "-%s" (Stm.term_to_str $2) in
      let i  = parser_check_type check_type_int $2 E.Int get_str_expr in
        Stm.IntNeg i
    }
  | term MATH_PLUS term
    {
      let get_str_expr () = sprintf "%s+%s" (Stm.term_to_str $1)
                                            (Stm.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.IntAdd (i1,i2)
    }
  | term MATH_MINUS term
    {
      let get_str_expr () = sprintf "%s-%s" (Stm.term_to_str $1)
                                            (Stm.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.IntSub (i1,i2)
    }
  | term MATH_MULT term
    {
      let get_str_expr () = sprintf "%s*%s" (Stm.term_to_str $1)
                                            (Stm.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.IntMul (i1,i2)
    }
  | term MATH_DIV term
    {
      let get_str_expr () = sprintf "%s/%s" (Stm.term_to_str $1)
                                            (Stm.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.IntDiv (i1,i2)
    }
  | term MATH_MOD term
    {
      let get_str_expr () = sprintf "%s %% %s" (Stm.term_to_str $1)
                                               (Stm.term_to_str $3) in
      let i1  = parser_check_type check_type_int $1 E.Int get_str_expr in
      let i2  = parser_check_type check_type_int $3 E.Int get_str_expr in
        Stm.IntMod (i1,i2)
    }
  | SETINTMIN OPEN_PAREN term CLOSE_PAREN
    {
      let iSet = $3 in
      let get_str_expr () = sprintf "setIntMin(%s)" (Stm.term_to_str iSet) in
      let s  = parser_check_type check_type_setint iSet E.SetInt get_str_expr
      in
        Stm.IntSetMin (s)
    }
  | SETINTMAX OPEN_PAREN term CLOSE_PAREN
    {
      let iSet = $3 in
      let get_str_expr () = sprintf "setIntMax(%s)" (Stm.term_to_str iSet) in
      let s  = parser_check_type check_type_setint iSet E.SetInt get_str_expr
      in
        Stm.IntSetMax (s)
    }
  | HAVOCLEVEL OPEN_PAREN CLOSE_PAREN
    {
      Stm.HavocLevel
    }
  | HASHCODE OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "hashCode(%s)" (Stm.term_to_str $3) in
      let e  = parser_check_type check_type_elem $3 E.Elem get_str_expr
      in
        Stm.HashCode (e)
    }
  | INTOF OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "int_of(%s)" (Stm.term_to_str $3) in
      let p  = parser_check_type check_type_pair $3 E.Pair get_str_expr
      in
        Stm.PairInt(p)
    }


/* PAIR terms*/

pair :
  | OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "(%s,%s)" (Stm.term_to_str $2)
                                              (Stm.term_to_str $4) in
      let i  = parser_check_type check_type_int $2 E.Int get_str_expr in
      let t  = parser_check_type check_type_tid $4 E.Tid get_str_expr in
        Stm.IntTidPair(i,t)
    }
  | SETPAIRMIN OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spmin(%s)" (Stm.term_to_str $3) in
      let s  = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
        Stm.SetPairMin(s)
    }
  | SETPAIRMAX OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "spmax(%s)" (Stm.term_to_str $3) in
      let s  = parser_check_type check_type_setpair $3 E.SetPair get_str_expr in
        Stm.SetPairMax(s)
    }



/* ARRAYLOOKUP terms */

arraylookup :
  | term OPEN_BRACKET term CLOSE_BRACKET
    {
      let get_str_expr () = sprintf "%s[%s]" (Stm.term_to_str $1)
                                             (Stm.term_to_str $3) in
      let i = parser_check_type check_type_int $3 E.Int get_str_expr in
      try
        let arr = parser_check_type check_type_tidarr $1 E.TidArray get_str_expr in
          Stm.TidT (Stm.TidArrRd (arr,i))
        with _ ->
          try
            let arr = parser_check_type check_type_addrarr $1 E.AddrArray get_str_expr in
              Stm.AddrT (Stm.AddrArrRd (arr,i))
          with _ ->
            try
              let arr = parser_check_type check_type_bucketarr $1 E.BucketArray get_str_expr in
                Stm.BucketT (Stm.BucketArrRd (arr,i))
            with _ ->
              try
                let arr = parser_check_type check_type_lockarr $1 E.LockArray get_str_expr in
                  Stm.LockT (Stm.LockArrRd (arr,i))
              with e ->
                let a = parser_check_type check_type_addr $1 E.Addr get_str_expr in
                  match a with
                  | Stm.PointerNext a -> Stm.AddrT (Stm.PointerArrAt (a,i))
                  | _ -> raise(e)
    }


/* ARRAYUPDATE terms */

arrayupd :
  | ARR_UPDATE OPEN_PAREN term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "arrUpd (%s,%s,%s)" (Stm.term_to_str $3)
                                                        (Stm.term_to_str $5)
                                                        (Stm.term_to_str $7) in
      let i = parser_check_type check_type_int $5 E.Int get_str_expr in
      try
        let ll = parser_check_type check_type_lockarr $3 E.LockArray get_str_expr in
        let l = parser_check_type check_type_lock $7 E.Lock get_str_expr in
          Stm.LockArrayT (Stm.LockArrayUp (ll,i,l))
      with _ ->
        try
          let bb = parser_check_type check_type_bucketarr $3 E.BucketArray get_str_expr in
          let b = parser_check_type check_type_bucket $7 E.Bucket get_str_expr in
            Stm.BucketArrayT (Stm.BucketArrayUp (bb,i,b))
        with _ ->
          try
            let aa = parser_check_type check_type_addrarr $3 E.AddrArray get_str_expr in
            let a = parser_check_type check_type_addr $7 E.Addr get_str_expr in
              Stm.AddrArrayT (Stm.AddrArrayUp (aa,i,a))
          with _ ->
            let at = parser_check_type check_type_tidarr $3 E.TidArray get_str_expr in
            let t = parser_check_type check_type_tid $7 E.Tid get_str_expr in
              Stm.TidArrayT (Stm.TidArrayUp (at,i,t))
    }


/* BUCKET term */
bucket :
  | MKBUCKET OPEN_PAREN term COMMA term COMMA term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "mkbucket(%s,%s,%s,%s)"
                                           (Stm.term_to_str $3)
                                           (Stm.term_to_str $5)
                                           (Stm.term_to_str $7)
                                           (Stm.term_to_str $9) in
      let i = parser_check_type check_type_addr $3 E.Addr get_str_expr in
      let e = parser_check_type check_type_addr $5 E.Addr get_str_expr in
      let s = parser_check_type check_type_set $7 E.Set get_str_expr in
      let t = parser_check_type check_type_tid $9 E.Tid get_str_expr in
        Stm.MkBucket(i,e,s,t)
    }


/* LOCK term */
lock :
  | LOCK OPEN_PAREN term COMMA term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "lock(%s,%s)"
                                           (Stm.term_to_str $3)
                                           (Stm.term_to_str $5) in
      let l = parser_check_type check_type_lock $3 E.Lock get_str_expr in
      let t = parser_check_type check_type_tid $5 E.Tid get_str_expr in
        Stm.LLock(l,t)
    }
  | UNLOCK OPEN_PAREN term CLOSE_PAREN
    {
      let get_str_expr () = sprintf "unlock(%s)"
                                           (Stm.term_to_str $3) in
      let l = parser_check_type check_type_lock $3 E.Lock get_str_expr in
        Stm.LUnlock(l)
    }
