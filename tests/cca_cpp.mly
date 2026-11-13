(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>
   Copyright 2020 Chiba Institute of Technology

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

(* Author: Masatomo Hashimoto <m.hashimoto@stair.center> *)

%{

module L = Label
module Aux = Parser_aux.F (Stat)
module T = Tokens_.T
module I = Pinfo
module NS = Pinfo.Name.Namespace
module NestedNS = Pinfo.Name.NestedNamespace

open Common
open Aux
open Stat

let rec add_to_last n = function
  | [] -> []
  | [x] -> [x+n]
  | h::t -> h::(add_to_last n t)

let mknode = Ast.mknode env

let mkleaf = Ast.mkleaf env

let _reloc = Ast.reloc env
let _reloc_end = Ast.reloc_end env

let reloc sp ep x = Ast.reloc env sp ep x; x

let add_attrs_l al_opt sp ep x =
  begin
    match al_opt with
    | Some al -> begin
        let pvec =
          match x#pvec with
          | h::t -> ((List.length al)+h)::t
          | [] -> [List.length al]
        in
        _reloc sp ep x;
        x#add_children_l al;
        x#set_pvec pvec;
    end
    | None -> ()
  end;
  x

let add_to_last_pvec_elem nd j =
  let pvec =
    match List.rev nd#pvec with
    | h::t -> List.rev ((h+j)::t)
    | [] -> [j]
  in
  nd#set_pvec pvec

let add_attrs_r al_opt sp ep x =
  begin
    match al_opt with
    | Some al -> begin
        add_to_last_pvec_elem x (List.length al);
        x#add_children_r al;
        _reloc sp ep x;
    end
    | None -> ()
  end;
  x

let mkanode al_opt sp ep lab x =
  match al_opt with
  | Some al ->
      let pvec = [List.length al; 1] in
      mknode ~pvec sp ep lab (al @ [x])
  | None -> x

let mknodea al_opt sp ep lab x =
  match al_opt with
  | Some al ->
      let pvec = [1; List.length al] in
      mknode ~pvec sp ep lab (x::al)
  | None -> x

let make_gnu_asm_lab a tl =
  let s = Token.seq_to_repr tl in
  let len = String.length s in
  let s =
    if len > 1 && s.[0] = '(' && s.[len-1] = ')' then
      String.sub s 1 (len-2)
    else
      s
  in
  L.GnuAsmBlock(a, s)

let make_asm_lab tl =
  match (tl : Token.t list) with
  | [LPAREN,_,_;STR_LITERAL s,_,_;RPAREN,_,_] -> L.AsmDefinition s
  | _ -> make_gnu_asm_lab "asm" tl

let mktok sp ep t = t, sp, ep

let rawtok_to_lab rt =
  match (rt : T.token) with
  | INT_LITERAL i | USER_INT_LITERAL i -> L.IntegerLiteral i
  | CHAR_LITERAL c | USER_CHAR_LITERAL c -> L.CharacterLiteral c
  | FLOAT_LITERAL f | USER_FLOAT_LITERAL f -> L.FloatingLiteral f
  | STR_LITERAL s | USER_STR_LITERAL s -> L.StringLiteral s
  | BOOL_LITERAL b -> L.BooleanLiteral b
  | NULLPTR       -> L.Nullptr
  | IDENT i       -> L.Identifier i
  | PTR_AMP       -> L.Amp "&"
  | PTR_AMP_AMP   -> L.AmpAmp "&&"
  | PTR_STAR      -> L.Star
  | TEMPL_LT      -> L.Lt
  | PLUS          -> L.Plus
  | MINUS         -> L.Minus
  | STAR          -> L.Star
  | SLASH         -> L.Slash
  | PERC          -> L.Perc
  | HAT i         -> L.Hat i
  | AMP i         -> L.Amp i
  | BAR i         -> L.Bar i
  | TILDE i       -> L.Tilde i
  | LT_LT         -> L.ShiftExpressionLeft
  | GT_GT         -> L.ShiftExpressionRight
  | GT_GT_GT      -> L.ShiftExpressionRightU
  | PLUS_EQ       -> L.PlusEq
  | MINUS_EQ      -> L.MinusEq
  | STAR_EQ       -> L.StarEq
  | SLASH_EQ      -> L.SlashEq
  | PERC_EQ       -> L.PercEq
  | HAT_EQ i      -> L.HatEq i
  | AMP_EQ i      -> L.AmpEq i
  | BAR_EQ i      -> L.BarEq i
  | LT_LT_EQ      -> L.LtLtEq
  | GT_GT_EQ      -> L.GtGtEq
  | EQ            -> L.Eq
  | EQ_EQ         -> L.EqEq
  | EXCLAM_EQ i   -> L.ExclamEq i
  | LT            -> L.Lt
  | GT            -> L.Gt
  | LT_EQ         -> L.LtEq
  | GT_EQ         -> L.GtEq
  | LT_EQ_GT      -> L.LtEqGt
  | AMP_AMP i     -> L.AmpAmp i
  | BAR_BAR i     -> L.BarBar i
  | COMMA         -> L.Comma
  | DOT_STAR      -> L.DotStar
  | MINUS_GT_STAR -> L.MinusGtStar
  | FINAL         -> L.VirtSpecifierFinal
  | OVERRIDE      -> L.VirtSpecifierOverride
  | CHAR          -> L.Char
  | CHAR8_T       -> L.Char8_t
  | CHAR16_T      -> L.Char16_t
  | CHAR32_T      -> L.Char32_t
  | WCHAR_T       -> L.Wchar_t
  | BOOL          -> L.Bool
  | SHORT         -> L.Short
  | INT           -> L.Int
  | LONG          -> L.Long
  | SIGNED        -> L.Signed
  | UNSIGNED      -> L.Unsigned
  | FLOAT         -> L.Float
  | DOUBLE        -> L.Double
  | VOID          -> L.Void
  | INLINE        -> L.Inline
  | DELETE        -> L.Delete
  | NOEXCEPT      -> L.Noexcept
  | ELSE          -> L.ElseStatement
  | NAMESPACE     -> L.NamespaceHead ""
  | STRUCT        -> L.ClassHeadStruct
  | AT            -> L.At
  | PP_LINE       -> L.PpLine(0, "")
  | _ -> failwith "rawtok_to_lab"

let check_macro_body ?(name="") sp ep (tl : Token.t list) tl_obj =
  match tl with
  | [] -> [], false

  | (MS_ASM a,_,_)::r -> begin
      let lab = L.MsAsmBlock(a, Token.seq_to_repr r) in
      [mkleaf sp ep lab], false
  end

  | [DOT,_,_;IDENT i,_,_] -> [mkleaf sp ep (L.AsmDirective i)], false

  | (DOT,_,_)::(IDENT _,_,_)::_ -> [mkleaf sp ep (L.AsmDefinition (Token.seq_to_repr tl))], false

  | [FLOAT_LITERAL f0,_,_;FLOAT_LITERAL f1,_,_]
    -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | [IDENT i,_,_;COMMA,_,_] -> begin
      let nd = mkleaf sp ep (L.Identifier i) in
      nd#add_suffix ", ";
      [nd], false
  end

  | [IDENT i,s0,e0;rt,s1,e1] when begin
      match rt with
      |EQ|PLUS_EQ|MINUS_EQ|STAR_EQ|SLASH_EQ|PERC_EQ|LT_LT_EQ|GT_GT_EQ|AMP_EQ _|HAT_EQ _|BAR_EQ _ -> true
      | _ -> false
  end -> begin
    let i_ = mkleaf s0 e0 (L.Identifier i) in
    let d = mkleaf e1 e1 L.DummyExpr in
    let lab =
      match rt with
      | EQ       -> L.AssignmentExpressionEq
      | PLUS_EQ  -> L.AssignmentExpressionPlus
      | MINUS_EQ -> L.AssignmentExpressionMinus
      | STAR_EQ  -> L.AssignmentExpressionMult
      | SLASH_EQ -> L.AssignmentExpressionDiv
      | PERC_EQ  -> L.AssignmentExpressionMod
      | LT_LT_EQ -> L.AssignmentExpressionShiftLeft
      | GT_GT_EQ -> L.AssignmentExpressionShiftRight
      | AMP_EQ a -> L.AssignmentExpressionAnd a
      | HAT_EQ h -> L.AssignmentExpressionXor h
      | BAR_EQ b -> L.AssignmentExpressionOr b
      | _ -> assert false
    in
    [mknode sp ep lab [i_;d]], false
  end

  | [IDENT i,s,e;ELLIPSIS,_,_] -> begin
      let i_ = mkleaf s e (L.Identifier i) in
      let i__ = mknode sp ep L.PackExpansion [i_] in
      i__#add_suffix "...";
      [i__], false
  end

  | [IDENT i,_,_;SHARP_SHARP,_,_;PP_STRINGIZED x,_,_] -> begin
      let i_ = mkleaf sp ep (L.Identifier (sprintf "%s ## #%s" i x)) in
      [i_], false
  end

  | [COMMA,_,_;IDENT i,s1,e1;ELLIPSIS,s2,e2] -> begin
      let i_ = mkleaf s1 e1 (L.Identifier i) in
      i_#add_prefix ", ";
      let i__ = mknode s1 e2 L.PackExpansion [i_] in
      i__#add_suffix "...";
      [i__], false
  end

  | [COMMA,_,_;IDENT i,_,_] -> begin
      let nd = mkleaf sp ep (L.Identifier i) in
      nd#add_prefix ", ";
      [nd], false
  end

  | [EXTERN,_,_;STR_LITERAL s,_,_] -> [mkleaf sp ep (L.LinkageSpecification s)], false

  | [TY_LPAREN,_,_;RPAREN,_,_] -> [mkleaf sp ep L.ParametersAndQualifiers], false

  | [EQ,_,_;DEFAULT,_,_] -> [mkleaf sp ep L.FunctionBodyDefault], false

  | [EQ,_,_;DELETE,_,_] -> [mkleaf sp ep L.FunctionBodyDelete], false

  | (INT_LITERAL _,_,_)::(MINUS,_,_)::(INT_LITERAL _,_,_)::_
    -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (INT_LITERAL i0,s0,e0)::(INT_LITERAL i1,s1,e1)::[]
    -> [mkleaf s0 e0 (L.IntegerLiteral i0); mkleaf s1 e1 (L.IntegerLiteral i1)], false

  | [INLINE,s,e;NAMESPACE,_,_]
    -> [mknode sp ep (L.NamespaceHead "") [mkleaf s e L.Inline]], false

  | [CASE,_,_;IDENT i,s,e]
    -> [mknode ~pvec:[0; 1] sp ep L.CaseLabel [mkleaf s e (L.Identifier i)]], false

  | [COLON,_,_;IDENT _,_,_] -> [mkleaf sp ep (L.VaArgs (Token.seq_to_repr tl))], false

  | (TEMPL_LT,_,_)::rest when begin
      match List.rev rest with
      | (TY_TEMPL_GT,_,_)::(IDENT x,_,_)::(DOT,_,_)::_ -> begin
          match x with
          | "h" | "hh" | "hpp" | "H" -> true
          | _ -> false
      end
      | _ -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (IDENT x,_,_)::(EQ,_,_)::(IDENT _,_,_)::(IDENT _,_,_)::_ when begin
      try
        let pat = Str.regexp "compiler\\|linker\\|command" in
        let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
        true
      with
        Not_found -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (IDENT x,_,_)::(EQ,_,_)::(MINUS,_,_)::(IDENT _,_,_)::(IDENT _,_,_)::(EQ,_,_)::_ when begin
      try
        let pat = Str.regexp "opt" in
        let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
        true
      with
        Not_found -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (IDENT x,_,_)::(MINUS,_,_)::(IDENT _,_,_)::_ when begin
      try
        let pat = Str.regexp "linker\\|files" in
        let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
        true
      with
        Not_found -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (IDENT x,_,_)::(COLON,_,_)::(SLASH,_,_)::_ when begin
      try
        let pat = Str.regexp "dir\\|lib" in
        let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
        true
      with
        Not_found -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (DOT,_,_)::(SLASH,_,_)::(IDENT x,_,_)::rest when begin
      true
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (SLASH,_,_)::(IDENT x,_,_)::rest when begin
      ((x = "lib" || x = "sys" || x = "usr") &&
       try
         let pat = Str.regexp "objects\\|lib_\\|_lib\\|libs_\\|_libs\\|gnulib\\|ld_\\|linker" in
         let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
         true
       with
         Not_found -> false) ||
      match List.rev rest with
      | (IDENT "ld",_,_)::(SLASH,_,_)::_ when String.lowercase_ascii name = "linker" -> true
      | (IDENT x,_,_)::(DOT,_,_)::_ -> begin
          match x with
          | "o" | "a" -> true
          | _ -> false
      end
      | (IDENT x,_,_)::(MINUS,_,_)::_ -> begin
          try
            let pat = Str.regexp "switch\\|option\\|lib_\\|_lib\\|libs_\\|_libs\\|gnulib" in
            let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
            true
          with
            Not_found -> false
      end
      | _ -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (MINUS,_,_)::((IDENT _|STATIC),_,_)::(((MINUS|COMMA|SLASH|COLON|IDENT _|INT_LITERAL _|STR_LITERAL _),_,_)::_|[]) when begin
      try
        let pat = Str.regexp "switch\\|option\\|lib_\\|_lib\\|libs_\\|_libs" in
        let _ = Str.search_forward pat (String.lowercase_ascii name) 0 in
        true
      with
        Not_found -> false
  end -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | (MINUS_MINUS,_,_)::(IDENT _,_,_)::(IDENT _,_,_)::_ -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false
  | (MINUS_MINUS,_,_)::(MINUS_MINUS,_,_)::_ -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false
  | (MINUS_MINUS,_,_)::(MINUS,_,_)::_ -> [mkleaf sp ep (L.StringLiteral (Token.seq_to_repr tl))], false

  | [TRY,_,_;LBRACE,_,_] -> [mknode ~pvec:[0; 0] sp ep L.TryBlock []], false

  | (TY_LPAREN,_,_)::(TY_LPAREN,_,_)::(IDENT "__VA_ARGS__",_,_)::_
    -> [mkleaf sp ep (L.VaArgs (Token.seq_to_repr tl))], false

  (*| [LBRACE,_,_] -> [mkleaf sp ep L.OpeningBrace], false
  | [RBRACE,_,_] -> [mkleaf sp ep L.ClosingBrace], false
  | [RBRACE,_,ep0;COMMA,sp1,_] -> [mkleaf sp ep0 L.ClosingBrace;mkleaf sp1 ep L.Comma], false
  | [RBRACE,_,ep0;SEMICOLON _,sp1,_] -> [mkleaf sp ep0 L.ClosingBrace;mkleaf sp1 ep L.Semicolon], false*)

  | [LBRACKET,_,_] -> [mkleaf sp ep L.OpeningBracket], false

  | [RBRACKET,_,_] -> [mkleaf sp ep L.ClosingBracket], false

  | [RBRACKET,_,ep0;COMMA,sp1,_] -> [mkleaf sp ep0 L.ClosingBracket;mkleaf sp1 ep L.Comma], false

  | [RBRACKET,_,ep0;SEMICOLON _,sp1,_] -> [mkleaf sp ep0 L.ClosingBracket;mkleaf sp1 ep L.Semicolon], false

  | [rt0,_,ep0;COMMA,sp1,ep1;rt2,sp2,_] -> begin
      try
        let lab0 = rawtok_to_lab rt0 in
        let lab2 = rawtok_to_lab rt2 in
        [mkleaf sp ep0 lab0;mkleaf sp1 ep1 L.Comma;mkleaf sp2 ep lab2], false
      with
        _ -> [mkleaf sp ep (L.TokenSeq tl_obj)], true
  end

  | [rt,_,_] -> begin
      let lab, p =
        try
          rawtok_to_lab rt, false
        with
          _ -> L.TokenSeq tl_obj, true
      in
      [mkleaf sp ep lab], p
  end
  | _ when
      List.for_all
        (function
          | (T.IDENT _|STR_LITERAL _),_,_ -> true
          | _ -> false
        ) tl -> begin
          let children =
            List.map
              (function
                | T.IDENT s, sp, ep -> mkleaf sp ep (L.StringMacro s)
                | STR_LITERAL s, sp, ep -> mkleaf sp ep (L.StringLiteral s)
                | _ -> assert false
              ) tl
          in
          [mknode sp ep L.ConcatenatedString children], false
        end
  | _ -> [mkleaf sp ep (L.TokenSeq tl_obj)], true

let get_pp_if_cond p =
  match p#label with
  | L.PpEndif x | PpIf x | PpElif x | PpElse x -> x
  | _ -> ""

let _pp_if_group p = L.PpIfGroup (get_pp_if_cond p)
let _pp_elif_group p = L.PpElifGroup (get_pp_if_cond p)
let _pp_else_group p = L.PpElseGroup (get_pp_if_cond p)

let pp_if_group () = L.PpIfGroup ""
let pp_if () = L.PpIf ""

let relab_if_group nd pp_if_cond =
  nd#relab (L.PpIfGroup pp_if_cond);
  match nd#children with
  | p::_ -> begin
      match p#label with
      | L.PpIf "" -> p#relab (L.PpIf pp_if_cond)
      | _ -> ()
  end
  | _ -> ()

let warning = Parserlib_base.parse_warning

%}

%parameter <Stat : Parser_aux.STATE_T>

%token MARKER (*STMT_MARKER*) STR_MARKER COND_MARKER LAM_MARKER SECTION_MARKER SUFFIX_MARKER
%token DUMMY_STMT DUMMY_EXPR DUMMY_BODY DUMMY_DTOR DUMMY_TYPE

%token EOF NEWLINE
%token <string> STR_MACRO INT_MACRO DECL_MACRO STMT_MACRO VIRT_SPEC_MACRO OP_MACRO
%token <string> PARAMS_MACRO PARAMS_BODY_MACRO ARGS_MACRO ARG_MACRO NEW_INIT_MACRO ATTR_MACRO ACC_SPEC_MACRO
%token <string> DECL_SPEC_MACRO CV_MACRO NOEXCEPT_MACRO NS_MACRO EMPTY_MACRO DELIM_MACRO BASE_MACRO
%token <string> SUFFIX_MACRO BODY_MACRO BLOCK_HEAD_MACRO BLOCK_END_MACRO TYPE_MACRO CC_MACRO
%token <string> PARAM_DECL_MACRO PTR_MACRO BASE_SPEC_MACRO DTOR_MACRO CLASS_HEAD_MACRO FUNC_HEAD_MACRO

%token SUB_REQUIRES ELAB_ENUM ELAB_CLASS ODD_FOR ODD_ELSE
%token TEMPL_LT TEMPL_LT_ TEMPL_GT LAM_LBRACKET ATTR_LBRACKET INI_LBRACE CLASS_LBRACE
%token FOLD_LPAREN TY_LPAREN TY_LPAREN_ PP_LPAREN SS_LPAREN PS_LPAREN
%token PTR_STAR PTR_AMP PTR_AMP_AMP PTR_HAT TY_HAT TY_TILDE ELLIPSIS_
%token HEAD_COLON_COLON PURE_ZERO BASE_COLON TY_TEMPL_GT TY_TEMPL_GT_
%token <Common.ident> IDENT IDENT_ IDENT_V IDENT_B IDENT_C IDENT_E IDENT_LPAREN
%token <Common.ident> IDENT_AGM IDENT_AM IDENT_BEM IDENT_BHM IDENT_BM IDENT_CHM IDENT_CM
%token <Common.ident> IDENT_DM IDENT_DSM IDENT_EM IDENT_IM IDENT_LM IDENT_LOM IDENT_NSM IDENT_OM
%token <Common.ident> IDENT_PDM IDENT_PM IDENT_PBM IDENT_SM IDENT_SXM IDENT_TM IDENT_TPM IDENT_VM
%token PP_IF_A PP_IFDEF_A PP_IFNDEF_A PP_IF_ATTR PP_IFDEF_ATTR PP_IFNDEF_ATTR PP_IF_B PP_IFDEF_B PP_IFNDEF_B
%token PP_IF_X PP_IFDEF_X PP_IFNDEF_X
%token PP_IF_C PP_IFDEF_C PP_IFNDEF_C PP_IF_CB PP_IFDEF_CB PP_IFNDEF_CB
%token PP_IF_D PP_IFDEF_D PP_IFNDEF_D PP_IF_E PP_IFDEF_E PP_IFNDEF_E PP_IF_EH PP_IFDEF_EH PP_IFNDEF_EH
%token PP_IF_H PP_IFDEF_H PP_IFNDEF_H PP_IF_I PP_IFDEF_I PP_IFNDEF_I
%token PP_IF_O PP_IFDEF_O PP_IFNDEF_O PP_IF_P PP_IFDEF_P PP_IFNDEF_P PP_IF_S PP_IFDEF_S PP_IFNDEF_S
%token PP_IF_COND PP_IFDEF_COND PP_IFNDEF_COND PP_IF_COND_ PP_IFDEF_COND_ PP_IFNDEF_COND_
%token PP_IF_CLOSING PP_IFDEF_CLOSING PP_IFNDEF_CLOSING PP_IF_SHIFT PP_IFDEF_SHIFT PP_IFNDEF_SHIFT
%token PP_IF_CLOSE_OPEN PP_IFDEF_CLOSE_OPEN PP_IFNDEF_CLOSE_OPEN
%token (*PP_ODD_ENDIF *)PP_ODD_IF PP_ODD_IFDEF PP_ODD_IFNDEF (*PP_ODD_ELIF PP_ODD_ELSE*)
%token ODD_LBRACE ODD_RBRACE BEGIN_STMTS END_STMTS BEGIN_ETORS END_ETORS COMMA_BROKEN BEGIN_ASM
%token <string> PP_STRINGIZED BAR_BAR_BROKEN AMP_AMP_BROKEN
%token <string> PP_ODD_ENDIF PP_ODD_ELSE PP_ODD_ELIF
%token <string> BS_IDENT
%token ASM_SHADER

(*
REQUIRES : SUB_REQUIRES
ENUM     : ELAB_ENUM

LT       : TEMPL_LT
GT       : TEMPL_GT, TY_TEMPL_GT
LBRACKET : LAM_LBRACKET, ATTR_LBRACKET, MS_ATTR_LBRACKET
LPAREN   : FOLD_LPAREN, TY_LPAREN, PP_LPAREN SS_LPAREN PS_LPAREN
LBRACE   : INI_LBRACE, CLASS_LBRACE

STAR        : PTR_STAR
AMP         : PTR_AMP
AMP_AMP     : PTR_AMP_AMP
HAT         : PTR_HAT TY_HAT
TILDE       : TY_TILDE
ELLIPSIS    : ELLIPSIS_
COLON_COLON : HEAD_COLON_COLON
COLON       : BASE_COLON
ZERO        : PURE_ZERO

IDENT: IDENT_V(value), IDENT_B(member_declarator:bit_field), IDENT_C(type_constraint)
       IDENT_E(enumerator)
       IDENT_EM(expr macro), IDENT_SM(stmt macro), IDENT_SXM(suffx macro), IDENT_TM(type macro), IDENT_IM(ident macro)
       IDENT_PM(params macro), IDENT_CM(cv qualifier macro), IDENT_LM(literal macro), IDENT_AM(attr/args macro)
       IDENT_TPM(templ param macro), IDENT_NSM(namespace macro), IDENT_DSM(decl or stmt macro)
       IDENT_BHM(block head macro), IDENT_BEM(block end macro), IDENT_CHM(cast/class head macro) IDENT_OM(op macro)
       IDENT_DM(decl spec macro), IDENT_AGM(arg macro), IDENT_LOM(lor macro), IDENT_VM(virt-spec macro)
PP_IF     : PP_IF_E     PP_IF_SHIFT     PP_IF_CLOSING
PP_IFDEF  : PP_IFDEF_E  PP_IFDEF_SHIFT  PP_IFDEF_CLOSING
PP_IFNDEF : PP_IFNDEF_E PP_IFNDEF_SHIFT PP_IFNDEF_CLOSING
*)


(* literal *)
%token <string> INT_LITERAL CHAR_LITERAL FLOAT_LITERAL STR_LITERAL BOOL_LITERAL
%token <string> USER_INT_LITERAL USER_FLOAT_LITERAL USER_STR_LITERAL USER_CHAR_LITERAL

(* keyword *)
%token ALIGNAS ALIGNOF ASM AUTO BOOL BREAK CASE CATCH CHAR CHAR8_T CHAR16_T CHAR32_T CLASS
%token CONCEPT CONST CONSTEVAL CONSTEXPR CONSTINIT CONST_CAST CONTINUE CO_AWAIT CO_RETURN
%token CO_YIELD DECLTYPE DEFAULT DELETE DO DOUBLE DYNAMIC_CAST ELSE ENUM EXPLICIT EXPORT
%token EXTERN FALSE FLOAT FOR FRIEND GOTO IF INLINE INT LONG MUTABLE NAMESPACE NEW NOEXCEPT
%token NULLPTR OPERATOR PRIVATE PROTECTED PUBLIC REGISTER REINTERPRET_CAST REQUIRES RETURN
%token SHORT SIGNED SIZEOF STATIC STATIC_ASSERT STATIC_CAST STRUCT SWITCH TEMPLATE THIS
%token THREAD_LOCAL THROW THROW_ TRUE TRY TYPEDEF TYPEID TYPENAME UNION UNSIGNED USING
%token VIRTUAL VOID VOLATILE WCHAR_T WHILE


(* for C99 *)
%token <string> RESTRICT

(* for GNU *)
%token <string> GNU_ASM GNU_ATTR

(* for VAX *)
%token VAX_GLOBALDEF

(* for WIN *)
%token <string> MS_STDCALL MS_CDECL MS_ASM MS_PRAGMA
%token END_ASM MS_REF MS_PROPERTY MS_SEALED MS_ATTR_LBRACKET

(* for CUDA C *)
%token CUDA_LT_LT_LT CUDA_GT_GT_GT

(* for JS? *)
%token GT_GT_GT

(* for QT *)
%token BEGIN_QPROP END_QPROP

(* for Doxygen *)
%token <string> DOXYGEN_CMD DOXYGEN_LINE

(* for Objective-C *)
%token OBJC_INTERFACE OBJC_END OBJC_PROPERTY OBJC_PRIVATE OBJC_PUBLIC OBJC_PROTECTED OBJC_PACKAGE
%token OBJC_PLUS OBJC_MINUS OBJC_CLASS OBJC_PROTOCOL OBJC_OPTIONAL OBJC_REQUIRED OBJC_SELECTOR
%token OBJC_ENCODE OBJC_SYNCHRONIZED OBJC_DEFS OBJC_SYNTHESIZE OBJC_DYNAMIC
%token OBJC_TRY OBJC_THROW OBJC_CATCH OBJC_FINALLY OBJC_AUTORELEASEPOOL OBJC_AVAILABLE
%token <string> OBJC_UNKNOWN
%token PP_IMPORT IN OBJC_LBRACKET

(* quasi-keyword *)
%token AUDIT AXIOM FINAL OVERRIDE IMPORT (*MODULE*)
%token EXPECTS ENSURES ASSERT
(*%token CARRIES_DEPENDENCY DEPRECATED FALLTHROUGH LIKELY MAYBE_UNUSED NO_UNIQUE_ADDRESS*)
(*%token NODISCARD NORETURN UNLIKELY*)

%token DEFINED HAS_INCLUDE HAS_CPP_ATTRIBUTE

(* preprocessing-op-or-punc *)
%token LBRACE RBRACE LBRACKET RBRACKET SHARP SHARP_SHARP LPAREN RPAREN LT_COLON COLON_GT
%token LT_PERC PERC_GT PERC_COLON PERC_COLON_PERC_COLON COLON ELLIPSIS QUEST
%token COLON_COLON DOT DOT_STAR MINUS_GT MINUS_GT_STAR PLUS MINUS STAR SLASH PERC
%token EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERC_EQ EQ_EQ EQ_EQ_EQ
%token LT GT LT_EQ GT_EQ LT_EQ_GT LT_LT GT_GT LT_LT_EQ GT_GT_EQ
%token <string> AMP_AMP BAR_BAR HAT EXCLAM AMP BAR TILDE AMP_EQ BAR_EQ HAT_EQ EXCLAM_EQ
%token PLUS_PLUS MINUS_MINUS COMMA AT BS
%token AND OR XOR NOT BITAND BITOR COMPL AND_EQ OR_EQ XOR_EQ NOT_EQ
%token <bool> SEMICOLON (* true if not generated *)

(* directive *)
%token PP_INCLUDE PP_DEFINE PP_UNDEF PP_LINE PP_ERROR PP_PRAGMA PP_
%token PP_IF PP_IFDEF PP_IFNDEF (*PP_ELIF PP_ELSE PP_ENDIF*) PP_ENDIF_
%token <int> BRACE_LEVEL
%token <string> PP_UNKNOWN
%token <string ref> PP_ELIF PP_ELSE PP_ENDIF

%nonassoc HEAD_COLON_COLON
%nonassoc PREC
%nonassoc SEMICOLON
%nonassoc ELSE ODD_ELSE WHILE
%nonassoc EQ
%nonassoc LBRACKET LPAREN INI_LBRACE
%right ELAB_ENUM CLASS ELAB_CLASS TYPENAME STRUCT UNION DECLTYPE MS_REF
%nonassoc IDENT IDENT_C IDENT_TM IDENT_IM IDENT_CM
%right SIGNED UNSIGNED
%nonassoc WCHAR_T VOID SHORT LONG INT FLOAT DOUBLE CHAR8_T CHAR32_T CHAR16_T CHAR BOOL
%nonassoc VOLATILE CONST AUTO RESTRICT MS_STDCALL MS_CDECL CC_MACRO CV_MACRO TYPE_MACRO
%nonassoc PTR_STAR PTR_AMP PTR_AMP_AMP PTR_HAT PTR_MACRO
%nonassoc ATTR_MACRO IDENT_AM OBJC_UNKNOWN PP_IF_ATTR PP_IFDEF_ATTR PP_IFNDEF_ATTR
%right ALIGNAS ATTR_LBRACKET GNU_ATTR

%start main
%start decls_sub
%start mem_decls_sub
%start stmts_sub
%start expr_sub
%start init_sub
%start type_sub
%start specs_sub
%start dtors_sub
%start etors_sub

%start special_token
%type <unit> special_token

%type <Ast.node> main
%type <Ast.node> decls_sub
%type <Ast.node> mem_decls_sub
%type <Ast.node> stmts_sub
%type <Ast.node> expr_sub
%type <Ast.node> init_sub
%type <Ast.node> type_sub
%type <Ast.node> specs_sub
%type <Ast.node> dtors_sub
%type <Ast.node> etors_sub

%type <Ast.L.t> equality_op relational_op shift_op additive_op multiplicative_op pm_op
%type <Ast.L.t> unary_operator assignment_operator
%type <Ast.L.ClassKey.t> class_key
%type <Ast.L.EnumKey.t> enum_key
%type <Ast.L.macro_kind> pp_param_list
%type <Token.t> token

%%
(********** Rules **********)

%inline
clist0(X):
| l_opt=ioption(separated_nonempty_list(COMMA, X)) { list_opt_to_list l_opt }
;

%inline
clist(X):
| l=separated_nonempty_list(COMMA, X) { l }
;

special_token:
| PP_ENDIF_ { }
| IDENT_ { }
| TEMPL_LT_ { }
| TY_LPAREN_ { }
| TY_TEMPL_GT_ { }
| AT { }
| BS { }
| OBJC_DEFS { }
| OBJC_SYNTHESIZE { }
| OBJC_DYNAMIC { }
;

main:
| t=translation_unit { t }
;

translation_unit:
|                    EOF { mkleaf $startpos $endpos L.TranslationUnit }
| dl=declaration_seq EOF { mknode $startpos $endpos(dl) L.TranslationUnit dl }
| MARKER il=initializer_list EOF { mknode $startpos $endpos(il) L.BracedInitList il }
;

decls_sub:
| dl=declaration_seq o_opt=ioption(odd_decl) EOF
    { mknode $startpos $endpos(o_opt) L.DECLS (dl @ (opt_to_list o_opt))}
| o=odd_decl EOF { o }
| o=ODD_RBRACE c=COMMA EOF
    { 
      ignore o;
      ignore c;
      let o_ = mkleaf $startpos $endpos(o) L.ClosingBrace in
      let c_ = mkleaf $startpos(c) $endpos(c) L.Comma in
      mknode $startpos $endpos(c) L.PARTIAL_CONSTRUCT [o_; c_]
    }
| o=odd_decl d=_static_assert_declaration EOF { mknode $startpos $endpos L.DECLS [o; d] }
| t=template_head EOF { t }
| q=qualified_id EOF { q }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq EQ i=initializer_clause EOF
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 0; 1] in
      i#add_prefix "= ";
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ [i])
    }
| MARKER il=initializer_list EOF { mknode $startpos $endpos(il) L.BracedInitList il }
;
odd_decl:
| s=_simple_declaration { s }
| al=attribute_specifier_seq { mknode $startpos $endpos L.AttributeDeclaration al }
| TEMPLATE TEMPL_LT TEMPL_GT s=_simple_declaration
    { mknode $startpos $endpos L.ExplicitSpecialization [s] }
| TEMPLATE TEMPL_LT TEMPL_GT n=_nodeclspec_function_definition
    { mknode $startpos $endpos L.ExplicitSpecialization [n] }
| t=template_head o=odd_decl
    { mknode ~pvec:[1; 1] $startpos $endpos(o) L.TemplateDeclaration [t; o] }
| e_opt=ioption(extern) TEMPLATE d=odd_decl
    { 
      let el = opt_to_list e_opt in
      let pvec = [List.length el; 1] in
      mknode ~pvec $symbolstartpos $endpos L.ExplicitInstantiation (el @ [d])
    }
| EXTERN s=STR_LITERAL d=odd_decl { mknode $startpos $endpos (L.LinkageSpecification s) [d] }
| EXTERN s=STR_LITERAL LBRACE d=odd_decl RBRACE { mknode $startpos $endpos (L.LinkageSpecification s) [d] }
| a=_alias_declaration { a }
| u=_using_declaration { u }
| s=_static_assert_declaration { s }
| f=func_head b=odd_func_body
    { 
      f#relab L.FunctionDefinition;
      f#add_children_r [b];
      f#set_pvec (f#pvec @ [1]);
      reloc $startpos $endpos f
    }
| c=class_head_macro { c }
;
%inline
odd_func_body:
| EQ DEFAULT { mkleaf $startpos $endpos L.FunctionBodyDefault }
| EQ DELETE  { mkleaf $startpos $endpos L.FunctionBodyDelete }
;
odd_mem_decl:
| f=func_head b=odd_func_body
    { 
      f#relab L.FunctionDefinition;
      f#add_children_r [b];
      f#set_pvec (f#pvec @ [1]);
      reloc $startpos $endpos f
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq ml=member_declarator_list
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; List.length ml] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.MemberDeclarationDecl (al @ dl @ ml) in
      env#register_members nd;
      nd
    }
;
mem_decls_sub:
| ml=mem_decl_seq EOF { mknode $startpos $endpos(ml) L.MEM_DECLS ml }
| LBRACE ml=mem_decl_seq RBRACE EOF
    { 
      let m = mknode $startpos $endpos(ml) L.MEM_DECLS ml in
      m#add_prefix "{ ";
      m#add_suffix " }";
      m 
    }
| ml=mem_decl_seq o=odd_mem_decl EOF { mknode $startpos $endpos(ml) L.MEM_DECLS (ml @ [o]) }
| ml=mem_decl_seq t=template_head o=odd_decl EOF
    { 
      let n_ = mknode ~pvec:[1; 1] $startpos(t) $endpos(o) L.TemplateDeclaration [t; o] in
      mknode $startpos $endpos(ml) L.MEM_DECLS (ml@[n_])
    }
;
stmts_sub:
| sl=statement_seq o_opt=ioption(odd_stmt) EOF
    { mknode $startpos $endpos(o_opt) L.STMTS (sl @ (opt_to_list o_opt)) }
(*| SEMICOLON c=CATCH TY_LPAREN e=exception_declaration r=RPAREN EOF
    { 
      ignore c;
      ignore r;
      mknode ~pvec:[1; 0] $startpos(c) $endpos(r) L.Handler [e]
    }
| SEMICOLON h=handler EOF { h }*)
;

odd_stmt:
| s=_odd_stmt { s }
| b=_block_declaration { b }
| t=objc_try { mknode ~pvec:[1; 0; 0] $startpos $endpos L.ObjcTryBlock [t] }
| ll=label_seq i_opt=ioption(IDENT)
    { 
      let il =
        match i_opt with
        | Some i -> [mkleaf $startpos(i_opt) $endpos (L.Identifier i)]
        | None -> []
      in
      mknode $startpos $endpos L.LABELS (ll @ il)
    }
| ll=label_seq c=CASE e=constant_expression
    { 
      ignore c;
      let c_ = mknode $startpos(c) $endpos L.CaseLabel [e] in
      mknode $startpos $endpos L.LABELS (ll @ [c_])
    }
| CASE e=constant_expression
    { mknode $startpos $endpos L.CaseLabel [e] }
| ll=label_seq s=_odd_stmt
    { 
      mknode ~pvec:[List.length ll; 1] $startpos $endpos L.LabeledStatement (ll @ [s])
    }
;
%inline
_odd_stmt:
| e=expression { e }
| e=expression ODD_RBRACE { e }
| a=GNU_ASM tl=list(gnu_asm_token) { mkleaf $startpos $endpos(tl) (make_gnu_asm_lab a tl) }
| j=_jump_statement { j }
| j=_jump_statement ODD_RBRACE { j }
| FOR { mkleaf ~pvec:[0; 0; 0; 0] $startpos $endpos L.ForStatement }
| FOR LPAREN
    i=init_statement c_opt=ioption(condition) SEMICOLON e_opt=ioption(expression)
    RPAREN s_opt=ioption(odd_stmt)
    { 
      let cl = opt_to_list c_opt in
      let el = opt_to_list e_opt in
      let sl = opt_to_list s_opt in
      let pvec = [1; List.length cl; List.length el; List.length sl] in
      mknode ~pvec $startpos $endpos L.ForStatement (i :: cl @ el @ sl)
    }
| FOR LPAREN
    i_opt=ioption(init_statement) f=for_range_declaration COLON fi=for_range_initializer
    RPAREN s_opt=ioption(odd_stmt)
    { 
      let il = opt_to_list i_opt in
      let sl = opt_to_list s_opt in
      let pvec = [List.length il; 1; 1; List.length sl] in
      mknode ~pvec $startpos $endpos L.RangeBasedForStatement (il @ (f::fi::sl))
    }
| FOR LPAREN i=objc_identifier IN e=expression RPAREN s_opt=ioption(odd_stmt)
    { 
      let sl = opt_to_list s_opt in
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos L.ForInStatement (i::e::sl)
    }
| WHILE LPAREN c=condition RPAREN s=odd_stmt
    { mknode ~pvec:[1; 1] $startpos $endpos L.WhileStatement [c; s] }
| WHILE LPAREN c=condition RPAREN
    { mknode ~pvec:[1; 0] $startpos $endpos L.WhileStatement [c] }
| DO { mknode ~pvec:[0; 0] $startpos $endpos L.DoStatement [] }
| SWITCH LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    { 
      let il = opt_to_list i_opt in
      begin
        match (i_opt : Ast.node option) with
        | Some i -> i#add_prefix "("
        | None -> c#add_prefix "("
      end;
      c#add_suffix ")";
      mknode ~pvec:[List.length il; 1; 0] $startpos $endpos L.SwitchStatement (il @ [c])
    }
| o=odd_if_stmt { o }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    s0=odd_stmt
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s0])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    s0=statement ELSE
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s0])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    s0=statement ELSE s1=odd_stmt
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 1] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s0; s1])
    }
| ELSE s=odd_stmt { mknode $startpos $endpos L.ElseStatement [s] }
| SWITCH LPAREN i_opt=ioption(init_statement) c=condition RPAREN s=odd_stmt
    { 
      let il = opt_to_list i_opt in
      begin
        match (i_opt : Ast.node option) with
        | Some i -> i#add_prefix "("
        | None -> c#add_prefix "("
      end;
      c#add_suffix ")";
      mknode ~pvec:[List.length il; 1; 1] $startpos $endpos L.SwitchStatement (il @ [c; s])
    }
;
specs_sub:
| vl=virt_specifier_seq EOF { mknode $startpos $endpos(vl) L.SPECS vl }
| vl=virt_specifier_seq b=function_body EOF
    { 
      let pvec = [0; 0; 0; List.length vl; 1] in
      mknode ~pvec $startpos $endpos(b) L.FunctionDefinition (vl@[b])
    }

| (*cl_opt=cv_qualifier_seq_opt*)
    r_opt=ioption(ref_qualifier)
    n_opt=ioption(noexcept_specifier)
    al_opt=attribute_specifier_seq_opt
    t=trailing_return_type
    b=function_body
    EOF
    { 
      (*let cl = list_opt_to_list cl_opt in*)
      let rl = opt_to_list r_opt in
      let nl = opt_to_list n_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [0; (*List.length cl*)0; List.length rl; List.length nl; List.length al] in
      let p_ = mknode ~pvec $startpos $endpos(n_opt) L.ParametersAndQualifiers ((*cl @ *)rl @ nl @ al) in
      let d_ = mknode ~pvec:[0; 1; 1] $startpos $endpos(t) L.DeclaratorFunc [p_; t] in
      mknode ~pvec:[0; 0; 1; 0; 0; 1] $startpos $endpos(b) L.FunctionDefinition [d_; b]
    }

| al=attribute_specifier_seq EOF { mknode $startpos $endpos(al) L.SPECS al }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq ad_opt=ioption(abstract_declarator) EOF
    { 
      let al = list_opt_to_list al_opt in
      let adl = opt_to_list ad_opt in
      mknode $symbolstartpos $endpos(ad_opt) L.SPECS (al@dl@adl)
    }
| n=noexcept_specifier EOF { n }
| n=nested_name_specifier EOF { n }
| a=GNU_ATTR LPAREN al=attribute_list r=RPAREN EOF
    { ignore r; mknode $startpos $endpos(r) (L.GnuAttribute a) al }
| p=parameter_declaration COMMA EOF { p }
| a=access_specifier v_opt=ioption(virtual_) c=class_or_decltype ioption(COMMA) EOF
    { 
      let vl = opt_to_list v_opt in
      let pvec = [0; 1; List.length vl; 0; 1] in
      mknode ~pvec $symbolstartpos $endpos(c) L.BaseSpecifier (a :: vl @ [c])
    }
;
init_sub:
(*| d=declarator EOF { d }
| d=declarator COMMA il=initializer_list EOF { mknode $startpos $endpos(il) L.INITS (d::il) }
| d=declarator COMMA il=designated_initializer_list EOF { mknode $startpos $endpos(il) L.INITS (d::il) }*)
| b=(*brace_or_*)equal_initializer ioption(COMMA) ioption(SEMICOLON) EOF { b }
| b=brace_or_equal_initializer o=ODD_RBRACE sl=statement_seq EOF
    { 
      ignore o;
      let o = mkleaf $startpos(o) $endpos(o) L.ClosingBrace in
      let s = mknode $startpos(sl) $endpos(sl) L.STMTS sl in
      mknode $startpos $endpos(sl) L.INITS [b; o; s]
    }
| b=(*brace_or_*)equal_initializer COMMA il=initializer_list EOF { mknode $startpos $endpos(il) L.INITS (b::il) }
| il=designated_initializer_list EOF { mknode $startpos $endpos(il) L.INITS il }
| il=initializer_list ioption(SEMICOLON) EOF { mknode $startpos $endpos(il) L.INITS il }
(*| p=parameter_declaration_clause EOF { p }
| TY_LPAREN p=parameter_declaration_clause r=RPAREN EOF
    { 
      ignore r;
      mknode ~pvec:[1; 0; 0; 0; 0] $startpos $endpos(r) L.ParametersAndQualifiers [p]
    }
| TY_LPAREN p=parameter_declaration_clause r=RPAREN c=ctor_initializer EOF
    { 
      ignore r;
      let p_ = mknode ~pvec:[1; 0; 0; 0; 0] $startpos $endpos(r) L.ParametersAndQualifiers [p] in
      mknode $startpos $endpos(c) L.PARTIAL_CONSTRUCT [p_; c]
    }*)
;

dtors_sub:
| d=declarator EOF { d }
| d=declarator COMMA il=initializer_list EOF { mknode $startpos $endpos(il) L.INITS (d::il) }
| d=declarator COMMA il=designated_initializer_list EOF { mknode $startpos $endpos(il) L.INITS (d::il) }
| p=parameter_declaration_clause EOF { p }
| TY_LPAREN p=parameter_declaration_clause r=RPAREN EOF
    { 
      ignore r;
      mknode ~pvec:[1; 0; 0; 0; 0] $startpos $endpos(r) L.ParametersAndQualifiers [p]
    }
| TY_LPAREN p=parameter_declaration_clause r=RPAREN c=ctor_initializer EOF
    { 
      ignore r;
      let p_ = mknode ~pvec:[1; 0; 0; 0; 0] $startpos $endpos(r) L.ParametersAndQualifiers [p] in
      mknode $startpos $endpos(c) L.PARTIAL_CONSTRUCT [p_; c]
    }
;

expr_sub:
| ioption(COMMA) e=expression ioption(COMMA) EOF { e }
| LBRACKET e=expr_or_braced_init_list RBRACKET
    { mknode ~pvec:[0; 1] $startpos $endpos L.PostfixExpressionSubscr [e] }
| o=odd_expr EOF { o }
;

odd_expr:
| l=logical_or_expression ao=assignment_operator i=odd_expr
    { mknode ~pvec:[1; 1] $startpos $endpos ao [l; i] }
| a=additive_expression ao=additive_op o=odd_mult_expr
    { mknode ~pvec:[1; 1] $startpos $endpos ao [a; o] }
| TY_LPAREN t=type_id RPAREN
    { mknode ~pvec:[0; 1; 0] $startpos $endpos L.CastExpression [t] }
;
odd_mult_expr:
| m=multiplicative_expression mo=multiplicative_op
    { mknode ~pvec:[1; 0] $startpos $endpos mo [m] }
;

type_sub:
| TY_LPAREN t=type_id r=RPAREN EOF
    { 
      ignore r;
      mknode ~pvec:[0; 1; 0] $startpos $endpos(r) L.CastExpression [t]
    }
(*| t=type_parameter EOF { t }*)
(*| tl=template_argument_list EOF { mknode $startpos $endpos L.TEMPL_ARGS tl }*)
| MINUS_GT t=type_id EOF { mknode $startpos $endpos(t) L.TrailingReturnType [t] }
| MINUS_GT t=type_id b=function_body EOF
    { 
      let r = mknode $startpos $endpos(t) L.TrailingReturnType [t] in
      mknode $startpos $endpos L.AMBIGUOUS_CONSTRUCT [r; b]
    }
| ioption(COMMA) tl=template_parameter_list EOF { mknode $startpos $endpos(tl) L.TEMPL_PARAMS tl }
| ql=QUEST+ EOF { mkleaf $startpos $endpos(ql) (L.Identifier (String.make (List.length ql) '?') ) }
;

etors_sub:
| el=enumerator_list EOF { mknode $startpos $endpos L.ETORS el }
| tl=template_argument_list EOF { mknode $startpos $endpos L.TEMPL_ARGS tl }
;

type_id:
| tl=type_specifier_seq a_opt=ioption(abstract_declarator)
    { 
      let al = opt_to_list a_opt in
      let pvec = [List.length tl; List.length al] in
      mknode ~pvec $startpos $endpos L.TypeId (tl @ al)
    }
| p=pp_decl_spec_if_section { p }
;

compound_statement:
| LBRACE sl_opt=statement_seq_opt RBRACE
    { mknode $startpos $endpos L.CompoundStatement (list_opt_to_list sl_opt) }
| LBRACE sl_opt=statement_seq_opt p=pp_stmt_if_section_closing
    { 
      env#stack#exit_block();
      mknode $startpos $endpos L.CompoundStatement ((list_opt_to_list sl_opt)@[p])
    }
;

statement_seq_opt:
| sl_opt=ioption(statement_seq) { sl_opt }
;

%inline
statement_seq:
| sl=_statement_seq { List.rev sl }
;
_statement_seq:
| s=statement { [s] }
| sl=_statement_seq s=statement { s::sl }
;

pp_stmt_if_section:
| p=pp_stmt_if_group pl=pp_stmt_elif_group* p_opt=ioption(pp_stmt_else_group) pe=pp_endif
    l_opt=ioption(BRACE_LEVEL)
    { 
      let pl1 = opt_to_list p_opt in
      let blv =
        match l_opt with
        | Some lv -> lv
        | None -> 0
      in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(blv, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
%inline
stmt_seq0:
| sl_opt=statement_seq_opt { list_opt_to_list sl_opt }
;
%inline
odd_if_stmt:
| i=IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    { 
      ignore i;
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      mknode ~pvec $startpos(i) $endpos L.IfStatement (cl @ il @ [c])
    }
;
%inline
odd_else_stmt:
| RBRACE ELSE o=odd_if_stmt_open
    { 
      let e = mknode $startpos $endpos L.ElseStatement [o] in
      e#add_prefix "} ";
      e
    }
| RBRACE ELSE LBRACE sl=stmt_seq0
    { 
      let e = mknode $startpos $endpos L.ElseStatement sl in
      e#add_prefix "} ";
      if sl <> [] then
        (List.hd sl)#add_prefix " {"
      else
        e#add_suffix " {";
      e
    }
;

pp_stmt_if_group:
| p=pp_ifx sl=stmt_seq0
    { mknode ~pvec:[1; List.length sl] $startpos $endpos (pp_if_group()) (p::sl) }
| p=pp_ifx sl=stmt_seq0 ol=odd_else_stmt+
    { mknode ~pvec:[1; List.length ol] $startpos $endpos (pp_if_group()) (p::sl@ol) }
| p=pp_ifx o=odd_stmt { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; o] }
| p=pp_ifx sl=statement_seq o=odd_stmt
    { mknode ~pvec:[1; List.length sl + 1] $startpos $endpos (pp_if_group()) (p::sl@[o]) }
| p=pp_ifx_d f=function_definition { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; f] }
;
pp_stmt_elif_group:
| p=pp_elif sl=stmt_seq0
    { mknode ~pvec:[1; List.length sl] $startpos $endpos (_pp_elif_group p) (p::sl) }
| p=pp_elif sl=stmt_seq0 ol=odd_else_stmt+
    { mknode ~pvec:[1; List.length ol] $startpos $endpos (pp_if_group()) (p::sl@ol) }
| p=pp_elif o=odd_stmt { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; o] }
| p=pp_elif sl=statement_seq o=odd_stmt
    { mknode ~pvec:[1; List.length sl + 1] $startpos $endpos (_pp_elif_group p) (p::sl@[o]) }
;
pp_stmt_else_group:
| p=pp_else sl=stmt_seq0
    { mknode ~pvec:[1; List.length sl] $startpos $endpos (_pp_else_group p) (p::sl) }
| p=pp_else sl=stmt_seq0 ol=odd_else_stmt+
    { mknode ~pvec:[1; List.length ol] $startpos $endpos (pp_if_group()) (p::sl@ol) }
| p=pp_else o=odd_stmt { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; o] }
| p=pp_else sl=statement_seq o=odd_stmt
    { mknode ~pvec:[1; List.length sl + 1] $startpos $endpos (_pp_else_group p) (p::sl@[o]) }
;
%inline
odd_if_stmt_open:
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    l=LBRACE sl=stmt_seq0
    { 
      ignore l;
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let s_ = mknode $startpos(l) $endpos L.CompoundStatement sl in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s_])
    }
| WHILE LPAREN c=condition RPAREN l=LBRACE sl=stmt_seq0
    { 
      ignore l;
      let s_ = mknode $startpos(l) $endpos L.CompoundStatement sl in
      mknode ~pvec:[1; 1] $startpos $endpos L.WhileStatement [c; s_]
    }
;

pp_stmt_if_section_closing:
| p=pp_stmt_if_group_closing
    pl=pp_stmt_elif_group_closing*
    p_opt=ioption(pp_stmt_else_group_closing)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_stmt_if_group_closing:
| p=pp_ifx_closing sl=stmt_seq0 RBRACE sl1=stmt_seq0
    { 
      let pvec = [1; List.length sl; List.length sl1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::sl@sl1)
    }
;
pp_stmt_elif_group_closing:
| p=pp_elif sl=stmt_seq0 mid_brace_close RBRACE sl1=stmt_seq0
    { 
      (*env#pstat#open_brace();*)
      let pvec = [1; List.length sl; List.length sl1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p::sl@sl1)
    }
;
pp_stmt_else_group_closing:
| p=pp_else sl=stmt_seq0 mid_brace_close RBRACE sl1=stmt_seq0
    { 
      (*env#pstat#open_brace();*)
      let pvec = [1; List.length sl; List.length sl1] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p::sl@sl1)
    }
| p=pp_else { mknode ~pvec:[1; 0; 0] $startpos $endpos (pp_if_group()) [p] } (* for invalid cases *)
;

mid_brace_close:
| { if env#brace_level > 0 || env#pp_if_section_rel_brace_level < -1 then env#pstat#open_brace() }
;

%inline
odd_if_stmt_broken:
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition b=BAR_BAR_BROKEN
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let c_ = mknode $startpos(c) $endpos (L.LogicalOrExpression b) [c] in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c_])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition a=AMP_AMP_BROKEN
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let c_ = mknode $startpos(c) $endpos (L.LogicalAndExpression a) [c] in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c_])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c])
    }
| IF c_opt=ioption(constexpr) LPAREN
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 0; 0; 0; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement cl
    }
;

pp_stmt_if_section_broken:
| p=pp_stmt_if_group_broken
    pl=pp_stmt_elif_group_broken*
    p_opt=ioption(pp_stmt_else_group_broken)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionBroken (p :: pl @ pl1 @ [pe])
    }
;
pp_stmt_if_group_broken:
| p=pp_ifx o=odd_if_stmt_broken
    { 
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [p; o]
    }
| p=pp_ifx sl=statement_seq o=odd_if_stmt_broken
    { 
      let pvec = [1; (List.length sl)+1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::sl@[o])
    }
| p=pp_ifx dl=decl_specifier_seq d=declarator EQ
    { 
      let o = mknode ~pvec:[0; List.length dl; 1] $startpos(dl) $endpos L.SimpleDeclaration (dl @ [d]) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [p; o]
    }
| pi=pp_ifx p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (pi::[e_])
    }
| pi=pp_ifx p=postfix_expression LPAREN el=expression_list COMMA_BROKEN b=broken_expr
    { 
      let pvec = [1; (List.length el)+1] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[b]) in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (pi::[e_])
    }
| pi=pp_ifx
    l=logical_or_expression ao=assignment_operator
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let e = mknode ~pvec:[1; 1] $startpos(l) $endpos ao [l; e_] in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (pi::[e])
    }
| pi=pp_ifx sl=statement_seq p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (pi::sl@[e_])
    }
| pi=pp_ifx sl=statement_seq p=postfix_expression LPAREN el=expression_list COMMA_BROKEN b=broken_expr
    { 
      let pvec = [1; (List.length el)+1] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[b]) in
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (pi::sl@[e_])
    }
| pi=pp_ifx p=pp_stmt_if_section_broken
    { 
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [pi; p]
    }
| pi=pp_ifx r=RETURN LPAREN lh=_lambda_expression LBRACE
    { 
      ignore r;
      lh#add_prefix "(";
      lh#add_suffix " {";
      let s = mknode $startpos(r) $endpos L.ReturnStatement [lh] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; s]
    }
| pi=pp_ifx r=RETURN p=postfix_expression LPAREN el=expression_list c_opt=ioption(COMMA_BROKEN)
    { 
      ignore r;
      p#add_suffix "(";
      if c_opt <> None then
        (Xlist.last el)#add_suffix ",";
      let e = mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (p::el) in
      let s = mknode $startpos(r) $endpos L.ReturnStatement [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; s]
    }
| pi=pp_ifx p=postfix_expression LPAREN lh=_lambda_expression LBRACE
    { 
      p#add_suffix "(";
      lh#add_suffix " {";
      let e = mknode $startpos $endpos L.PostfixExpressionFunCall [p; lh] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; e]
    }
| pi=pp_ifx sl=statement_seq p=postfix_expression LPAREN lh=_lambda_expression LBRACE
    { 
      p#add_suffix "(";
      lh#add_suffix " {";
      let e = mknode $startpos(p) $endpos L.PostfixExpressionFunCall [p; lh] in
      mknode ~pvec:[1; (List.length sl) + 1] $startpos $endpos (pp_if_group()) (pi::sl@[e])
    }
| pi=pp_ifx l=LBRACE
    { 
      ignore l;
      let s = mknode $startpos(l) $endpos L.CompoundStatement [] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; s]
    }
| pi=pp_ifx o=odd_if_stmt_open
    { 
      if
        try
          let info2 = env#pp_if_section_nth_info 2 in
          info2.Pinfo.i_broken
        with _ -> false
      then begin
        env#stack#exit_block();
        env#pstat#close_brace();
      end;
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [pi; o]
    }
| pi=pp_ifx sl=statement_seq o=odd_if_stmt_open
    { 
      let pvec = [1; (List.length sl) + 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (pi::sl@[o])
    }
;
pp_stmt_elif_group_broken:
| p=pp_elif o=odd_if_stmt_broken
    { 
      env#pstat#close_paren();
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p; o]
    }
| p=pp_elif dl=decl_specifier_seq d=declarator EQ
    { 
      env#pstat#close_paren();
      let o = mknode ~pvec:[0; List.length dl; 1] $startpos(dl) $endpos L.SimpleDeclaration (dl @ [d]) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p; o]
    }
| p=pp_elif sl=statement_seq o=odd_if_stmt_broken
    { 
      env#pstat#close_paren();
      let pvec = [1; (List.length sl) + 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::sl@[o])
    }
| pi=pp_elif
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      env#pstat#close_paren();
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) (pi::[e_])
    }
| pi=pp_elif
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN b=broken_expr
    { 
      env#pstat#close_paren();
      let pvec = [1; (List.length el) + 1] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[b]) in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) (pi::[e_])
    }
| pi=pp_elif
    l=logical_or_expression ao=assignment_operator
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      env#pstat#close_paren();
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let e = mknode ~pvec:[1; 1] $startpos(l) $endpos ao [l; e_] in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) (pi::[e])
    }
| pi=pp_elif sl=statement_seq
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN b=broken_expr
    { 
      env#pstat#close_paren();
      let pvec = [1; (List.length el) + 1] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[b]) in
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) (pi::sl@[e_])
    }
| pi=pp_elif p=pp_stmt_if_section_broken
    { 
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) [pi; p]
    }
| pi=pp_elif r=RETURN LPAREN lh=_lambda_expression LBRACE
    { 
      ignore r;
      env#stack#exit_block();
      env#pstat#close_brace();
      env#pstat#close_paren();
      lh#add_prefix "(";
      lh#add_suffix " {";
      let s = mknode $startpos(r) $endpos L.ReturnStatement [lh] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pi) [pi; s]
    }
| pi=pp_elif r=RETURN p=postfix_expression LPAREN el=expression_list c_opt=ioption(COMMA_BROKEN)
    { 
      ignore r;
      env#pstat#close_paren();
      p#add_suffix "(";
      if c_opt <> None then
        (Xlist.last el)#add_suffix ",";
      let e = mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (p::el) in
      let s = mknode $startpos(r) $endpos L.ReturnStatement [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pi) [pi; s]
    }
| pi=pp_elif p=postfix_expression LPAREN lh=_lambda_expression LBRACE
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      env#pstat#close_paren();
      p#add_suffix "(";
      lh#add_suffix " {";
      let e = mknode $startpos(p) $endpos L.PostfixExpressionFunCall [p; lh] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pi) [pi; e]
    }
| pi=pp_elif sl=statement_seq p=postfix_expression LPAREN lh=_lambda_expression LBRACE
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      env#pstat#close_paren();
      p#add_suffix "(";
      lh#add_suffix " {";
      let e = mknode $startpos(p) $endpos L.PostfixExpressionFunCall [p; lh] in
      mknode ~pvec:[1; (List.length sl) + 1] $startpos $endpos (_pp_elif_group pi) (pi::sl@[e])
    }
| pi=pp_elif l=LBRACE
    { 
      ignore l;
      env#stack#exit_block();
      env#pstat#close_brace();
      let s = mknode $startpos(l) $endpos L.CompoundStatement [] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pi) [pi; s]
    }
| pi=pp_elif o=odd_if_stmt_open
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) [pi; o]
    }
| pi=pp_elif sl=statement_seq o=odd_if_stmt_open
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      let pvec = [1; (List.length sl) + 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group pi) (pi::sl@[o])
    }
;
pp_stmt_else_group_broken:
| p=pp_else o=odd_if_stmt_broken
    { 
      env#pstat#close_paren();
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p; o]
    }
| p=pp_else dl=decl_specifier_seq d=declarator EQ
    { 
      env#pstat#close_paren();
      let o = mknode ~pvec:[0; List.length dl; 1] $startpos(dl) $endpos L.SimpleDeclaration (dl @ [d]) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p; o]
    }
| p=pp_else sl=statement_seq o=odd_if_stmt_broken
    { 
      env#pstat#close_paren();
      let pvec = [1; (List.length sl) + 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::sl@[o])
    }
| pi=pp_else
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      env#pstat#close_paren();
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) (pi::[e_])
    }
| pi=pp_else
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN b=broken_expr
    { 
      env#pstat#close_paren();
      let pvec = [1; (List.length el) + 1] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[b]) in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) (pi::[e_])
    }
| pi=pp_else
    l=logical_or_expression ao=assignment_operator
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN
    { 
      env#pstat#close_paren();
      let pvec = [1; List.length el] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      let e = mknode ~pvec:[1; 1] $startpos(l) $endpos ao [l; e_] in
      let pvec = [1; 0; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) (pi::[e])
    }
| pi=pp_else sl=statement_seq
    p=postfix_expression LPAREN el=expression_list COMMA_BROKEN b=broken_expr
    { 
      env#pstat#close_paren();
      let pvec = [1; (List.length el) + 1] in
      p#add_suffix "(";
      (Xlist.last el)#add_suffix ",";
      let e_ = mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[b]) in
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) (pi::sl@[e_])
    }
| pi=pp_else p=pp_stmt_if_section_broken
    { 
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) [pi; p]
    }
| pi=pp_else r=RETURN LPAREN lh=_lambda_expression LBRACE
    { 
      ignore r;
      env#stack#exit_block();
      env#pstat#close_brace();
      env#pstat#close_paren();
      lh#add_prefix "(";
      lh#add_suffix " {";
      let s = mknode $startpos(r) $endpos L.ReturnStatement [lh] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pi) [pi; s]
    }
| pi=pp_else r=RETURN p=postfix_expression LPAREN el=expression_list c_opt=ioption(COMMA_BROKEN)
    { 
      ignore r;
      env#pstat#close_paren();
      p#add_suffix "(";
      if c_opt <> None then
        (Xlist.last el)#add_suffix ",";
      let e = mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (p::el) in
      let s = mknode $startpos(r) $endpos L.ReturnStatement [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pi) [pi; s]
    }
| pi=pp_else p=postfix_expression LPAREN lh=_lambda_expression LBRACE
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      env#pstat#close_paren();
      p#add_suffix "(";
      lh#add_suffix " {";
      let e = mknode $startpos(p) $endpos L.PostfixExpressionFunCall [p; lh] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pi) [pi; e]
    }
| pi=pp_else sl=statement_seq p=postfix_expression LPAREN lh=_lambda_expression LBRACE
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      env#pstat#close_paren();
      p#add_suffix "(";
      lh#add_suffix " {";
      let e = mknode $startpos(p) $endpos L.PostfixExpressionFunCall [p; lh] in
      mknode ~pvec:[1; (List.length sl) + 1] $startpos $endpos (_pp_else_group pi) (pi::sl@[e])
    }
| pi=pp_else l=LBRACE
    { 
      ignore l;
      env#stack#exit_block();
      env#pstat#close_brace();
      let s = mknode $startpos(l) $endpos L.CompoundStatement [] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pi) [pi; s]
    }
| pi=pp_else o=odd_if_stmt_open
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) [pi; o]
    }
| pi=pp_else sl=statement_seq o=odd_if_stmt_open
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      let pvec = [1; (List.length sl) + 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group pi) (pi::sl@[o])
    }
;

statement:
| l=labeled_statement { l }
| u=unlabeled_statement { u }
(*| b=_block_declaration g=gnu_asm { mknode $startpos $endpos L.STMTS [b; g] }*)
;
%inline
unlabeled_statement:
| d=decl_OR_expr sc=SEMICOLON { if sc then d#add_suffix ";"; reloc $startpos $endpos d }
| d=decl_OR_expr s=DELIM_MACRO { d#add_suffix (" "^s); reloc $startpos $endpos d }

| b=braced_init_list sc=SEMICOLON { if sc then b#add_suffix ";"; reloc $startpos $endpos b }

| al_opt=attribute_specifier_seq_opt sc=SEMICOLON
    { 
      let al = list_opt_to_list al_opt in
      let s = mknode ~pvec:[List.length al; 0] $symbolstartpos $endpos L.ExpressionStatement al in
      if sc then s#add_suffix ";";
      s
    }

| DUMMY_STMT { mkleaf $startpos $endpos L.DummyStmt }

| l=logical_or_expression ao=assignment_operator p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos ao [l; p] }

(*| (*attribute_specifier_seq_opt*) expression_statement { }(*decl_OR_expr SEMICOLON*)*)
(*| declaration_statement { }(*decl_OR_expr SEMICOLON*)*)

| al_opt=attribute_specifier_seq_opt c=compound_statement
    { mkanode al_opt $symbolstartpos $endpos L.Statement c }
| al_opt=attribute_specifier_seq_opt s=selection_statement
    { mkanode al_opt $symbolstartpos $endpos L.Statement s }
| al_opt=attribute_specifier_seq_opt i=iteration_statement
    { mkanode al_opt $symbolstartpos $endpos L.Statement i }
| al_opt=attribute_specifier_seq_opt j=jump_statement
    { mkanode al_opt $symbolstartpos $endpos L.Statement j }
| al_opt=attribute_specifier_seq_opt t=try_block
    { mkanode al_opt $symbolstartpos $endpos L.Statement t }
| a=MS_ASM LBRACE tl=list(asm_token) RBRACE
    { mkleaf $startpos $endpos (L.MsAsmBlock(a, (Token.seq_to_repr tl))) }
| a=MS_ASM tl=list(asm_token) END_ASM
    { mkleaf $startpos $endpos (L.MsAsmBlock(a, (Token.seq_to_repr tl))) }

| g=gnu_asm { g }

| m=ms_pragma { m }

| s=STMT_MACRO { mkleaf $startpos $endpos (L.StatementMacro s) }
| i=iteration_macro { i }
| s=stmt_macro_call { s }
| p=pp_control_line { p }
| p=pp_stmt_if_section { p }
| p=pp_stmt_if_section da=dot_or_arrow i=id_expression
    LPAREN el_opt=expression_list_opt r=RPAREN SEMICOLON
    { 
      ignore r;
      let pvec = [1; 0; 1] in
      let p_ = mknode ~pvec $startpos $endpos(i) da (p::[i]) in
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el] in
      if el_opt <> None then begin
        p_#add_suffix "(";
        (Xlist.last el)#add_suffix ");"
      end
      else
        p_#add_suffix "()";
      let e_ = mknode ~pvec $startpos $endpos(r) L.PostfixExpressionFunCall (p_::el) in
      mknode ~pvec:[0; 1] $symbolstartpos $endpos L.ExpressionStatement [e_]
    }
| p=pp_stmt_if_section_broken e=assignment_expression SEMICOLON
    { 
      p#add_children_r [e];
      p#set_pvec (p#pvec @ [1]);
      reloc $startpos $endpos p
    }
(*| p=pp_stmt_if_section_broken c=condition RPAREN s=statement
    { 
      p#add_children_r [c; s];
      p#set_pvec (p#pvec @ [1; 1]);
      p#relab L.PpIfSectionBrokenIf;
      reloc $startpos $endpos p
    }*)
| p=pp_stmt_if_section_broken el=expression_list RPAREN s=statement
    { 
      p#add_children_r (el@[s]);
      p#set_pvec (p#pvec @ [List.length el; 1]);
      (*p#relab L.PpIfSectionBrokenIf;*)
      reloc $startpos $endpos p
    }
| p=pp_stmt_if_section_broken b=BAR_BAR e=logical_or_expression RPAREN s=statement
    { 
      let e_ = mknode ~pvec:[0; 1] $startpos(b) $endpos(e) (L.LogicalOrExpression b) [e] in
      p#add_children_r ([e_; s]);
      p#set_pvec (p#pvec @ [1; 1]);
      (*p#relab L.PpIfSectionBrokenIf;*)
      reloc $startpos $endpos p
    }
| p=pp_stmt_if_section_broken LAM_MARKER
    ld=lambda_declarator r_opt=ioption(requires_clause) c=compound_statement
    COMMA el=expression_list r=RPAREN SEMICOLON
    { 
      ignore r;
      let rl = opt_to_list r_opt in
      let pvec = [0; 0; 0; 1; List.length rl; 1] in
      let l_ = mknode ~pvec $startpos(ld) $endpos(c) L.LambdaExpression (ld :: rl @ [c]) in
      let pvec = [0; (List.length el)+1] in
      l_#add_suffix ",";
      (Xlist.last el)#add_suffix ")";
      let e_ = mknode ~pvec $startpos(ld) $endpos(r) L.PostfixExpressionFunCall (l_::el) in
      p#add_children_r [e_];
      p#set_pvec (p#pvec @ [1]);
      reloc $startpos $endpos p
    }
| p=pp_stmt_if_section_broken MARKER sl_opt=statement_seq_opt RBRACE
    { 
      let sl = list_opt_to_list sl_opt in
      p#add_children_r sl;
      p#set_pvec (p#pvec @ [List.length sl]);
      reloc $startpos $endpos p
    }

| p=pp_stmt_if_section_broken MARKER MARKER sl_opt=statement_seq_opt RBRACE RPAREN
    { 
      let sl = list_opt_to_list sl_opt in
      p#add_children_r sl;
      p#set_pvec (p#pvec @ [List.length sl]);
      reloc $startpos $endpos p
    }

| p=pp_stmt_if_section_broken MARKER MARKER sl_opt=statement_seq_opt RBRACE pc=pp_args_if_section_closing
    { 
      let sl = list_opt_to_list sl_opt in
      p#add_children_r sl;
      p#add_children_r [pc];
      p#set_pvec (p#pvec @ [List.length sl + 1]);
      reloc $startpos $endpos p
    }

| p=pp_stmt_if_section_broken COMMA el=expression_list RPAREN SEMICOLON
    { 
      p#add_children_r el;
      p#set_pvec (p#pvec @ [List.length el]);
      reloc $startpos $endpos p
    }
| ODD_LBRACE { mkleaf $startpos $endpos L.OpeningBrace }
| ODD_RBRACE { mkleaf $startpos $endpos L.ClosingBrace }
| ODD_ELSE { mkleaf $startpos $endpos L.ElseStatement }

(*| p=postfix_expression LPAREN el=expression_list pp=pp_args_if_section_closing
    { 
      let pvec = [1; (List.length el) + 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionFunCall (p::el@[pp])
    }*)
(*| l=logical_or_expression ao=assignment_operator
    p=postfix_expression LPAREN el=expression_list pp=pp_args_if_section_closing
    { 
      let i =
        let pvec = [1; (List.length el) + 1] in
        mknode ~pvec $startpos(p) $endpos L.PostfixExpressionFunCall (p::el@[pp])
      in
      mknode ~pvec:[1; 1] $startpos $endpos ao [l; i]
    }*)

| THROW s=simple_type_specifier LPAREN el_opt=expression_list_opt pp=pp_args_if_section_closing
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; (List.length el) + 1] in
      let n_ =
        mknode ~pvec $startpos(s) $endpos L.PostfixExpressionExplicitTypeConvExpr (s::el@[pp])
      in
      mknode $startpos $endpos L.ThrowExpression [n_]
    }
| d=decl_OR_stmt_macro_call_ { d }
| d=DECL_MACRO { mkleaf $startpos $endpos (L.DeclarationMacro d) }
| al=class_attr_spec_seq h=block_head_macro sl_opt=statement_seq_opt e=block_end_macro
    { 
      let sl = list_opt_to_list sl_opt in
      mknode $startpos $endpos L.DeclStmtBlock (al @ h :: sl @ [e])
    }
| OBJC_AUTORELEASEPOOL c=compound_statement { mknode $startpos $endpos L.ObjcAutoreleasepool [c] }
| t=objc_try_block { t }
| OBJC_SYNCHRONIZED LPAREN e=expression RPAREN s=statement
    { mknode $startpos $endpos L.ObjcSynchronized [e; s] }
| OBJC_THROW e=expression SEMICOLON
    { mknode $startpos $endpos L.ObjcThrow [e] }
| ELLIPSIS { mkleaf $startpos $endpos L.Ellipsis }
;

%inline
gnu_asm:
| a=GNU_ASM tl=list(gnu_asm_token) sc=SEMICOLON
    { 
      let n = mkleaf $startpos $endpos (make_gnu_asm_lab a tl) in
      if sc then n#add_suffix ";";
      n
    }

| a=GNU_ASM g=gnu_asm_fragment gl=gnu_asm_frag_seq sc=SEMICOLON
    { 
      let n = mknode $startpos $endpos (L.GnuAsmBlockFragmented a) (g::gl) in
      if sc then n#add_suffix ";";
      n
    }
;

objc_try_block:
| t=objc_try cl=objc_catch_clause+ f_opt=ioption(objc_finally)
    { 
      let fl = opt_to_list f_opt in
      mknode ~pvec:[1; List.length cl; List.length fl] $startpos $endpos L.ObjcTryBlock (t :: cl @ fl)
    }
| t=objc_try f=objc_finally { mknode ~pvec:[1; 0; 1] $startpos $endpos L.ObjcTryBlock [t; f] }
;
objc_try:
| OBJC_TRY c=compound_statement { mknode $startpos $endpos L.ObjcTry [c] }
;
objc_catch_clause:
| OBJC_CATCH TY_LPAREN d=exception_declaration RPAREN c=compound_statement
    { mknode ~pvec:[1; 1] $startpos $endpos L.ObjcCatchClause [d; c] }
;
objc_finally:
| OBJC_FINALLY c=compound_statement { mknode $startpos $endpos L.ObjcFinally [c] }
;

gnu_asm_frag_seq:
| pl=pp_gnu_asm_if_section+ g=gnu_asm_fragment { pl @ [g] }
| gl=gnu_asm_frag_seq p=pp_gnu_asm_if_section { gl @ [p] }
| gl=gnu_asm_frag_seq p=pp_gnu_asm_if_section g=gnu_asm_fragment { gl @ [p; g] }
| pl=pp_control_line+ g=gnu_asm_fragment { pl @ [g] }
| gl=gnu_asm_frag_seq pl=pp_control_line+ g=gnu_asm_fragment { gl @ pl @ [g] }
;
%inline
gnu_asm_fragment:
| tl=gnu_asm_token+
    { mkleaf $startpos $endpos (L.GnuAsmFragment (Token.seq_to_repr tl)) }
;

pp_gnu_asm_if_section:
| p=pp_gnu_asm_if_group
    pl=list(pp_gnu_asm_elif_group)
    p_opt=ioption(pp_gnu_asm_else_group)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_gnu_asm_if_group:
| p=pp_ifx_e g=gnu_asm_fragment
    { mknode ~pvec:[1; 1; 0] $startpos $endpos (pp_if_group()) (p::[g]) }
| p=pp_ifx_e g=gnu_asm_fragment gl=gnu_asm_frag_seq
    { mknode ~pvec:[1; 1; List.length gl] $startpos $endpos (pp_if_group()) (p::g::gl) }
| p=pp_ifx_e c=pp_control_line
    { mknode ~pvec:[1; 1; 0] $startpos $endpos (pp_if_group()) [p; c] }
;
pp_gnu_asm_elif_group:
| p=pp_elif g=gnu_asm_fragment
    { mknode ~pvec:[1; 1; 0] $startpos $endpos (_pp_elif_group p) (p::[g]) }
| p=pp_elif g=gnu_asm_fragment gl=gnu_asm_frag_seq
    { mknode ~pvec:[1; 1; List.length gl] $startpos $endpos (_pp_elif_group p) (p::g::gl) }
| p=pp_elif c=pp_control_line
    { mknode ~pvec:[1; 1; 0] $startpos $endpos (_pp_elif_group p) [p; c] }
;
pp_gnu_asm_else_group:
| p=pp_else g=gnu_asm_fragment
    { mknode ~pvec:[1; 1; 0] $startpos $endpos (_pp_else_group p) (p::[g]) }
| p=pp_else g=gnu_asm_fragment gl=gnu_asm_frag_seq
    { mknode ~pvec:[1; 1; List.length gl] $startpos $endpos (_pp_else_group p) (p::g::gl) }
| p=pp_else c=pp_control_line
    { mknode ~pvec:[1; 1; 0] $startpos $endpos (_pp_else_group p) [p; c] }
;

%inline
gnu_asm_token:
| i=IDENT          { mktok $startpos $endpos (T.IDENT i) }
| i=IDENT_V        { mktok $startpos $endpos (T.IDENT i) }
| i=IDENT_LM       { mktok $startpos $endpos (T.IDENT i) }
| i=BS_IDENT       { mktok $startpos $endpos (T.BS_IDENT i) }
| s=STR_LITERAL    { mktok $startpos $endpos (T.STR_LITERAL s) }
| i=STR_MACRO      { mktok $startpos $endpos (T.STR_MACRO i) }
| i=INT_LITERAL    { mktok $startpos $endpos (T.INT_LITERAL i) }
| i=USER_INT_LITERAL { mktok $startpos $endpos (T.USER_INT_LITERAL i) }
| f=FLOAT_LITERAL  { mktok $startpos $endpos (T.FLOAT_LITERAL f) }
| EQ               { mktok $startpos $endpos T.EQ }
| LPAREN           { mktok $startpos $endpos T.LPAREN }
| TY_LPAREN        { mktok $startpos $endpos T.LPAREN }
| RPAREN           { mktok $startpos $endpos T.RPAREN }
| TEMPL_LT         { mktok $startpos $endpos T.TEMPL_LT }
| LT               { mktok $startpos $endpos T.LT }
| GT               { mktok $startpos $endpos T.GT }
| LT_EQ            { mktok $startpos $endpos T.LT_EQ }
| GT_EQ            { mktok $startpos $endpos T.GT_EQ }
| EQ_EQ            { mktok $startpos $endpos T.EQ_EQ }
| e=EXCLAM_EQ      { mktok $startpos $endpos (T.EXCLAM_EQ e) }
| TY_TEMPL_GT      { mktok $startpos $endpos T.TY_TEMPL_GT }
| TEMPL_GT         { mktok $startpos $endpos T.TEMPL_GT }
| LBRACKET         { mktok $startpos $endpos T.LBRACKET }
| RBRACKET         { mktok $startpos $endpos T.RBRACKET }
| LBRACE           { mktok $startpos $endpos T.LBRACE }
| RBRACE           { mktok $startpos $endpos T.RBRACE }
| DOT              { mktok $startpos $endpos T.DOT }
| PLUS             { mktok $startpos $endpos T.PLUS }
| MINUS            { mktok $startpos $endpos T.MINUS }
| SLASH            { mktok $startpos $endpos T.SLASH }
| PERC             { mktok $startpos $endpos T.PERC }
| STAR             { mktok $startpos $endpos T.STAR }
| AMP              { mktok $startpos $endpos T.PTR_AMP }
| b=BAR            { mktok $startpos $endpos (T.BAR b) }
| e=EXCLAM         { mktok $startpos $endpos (T.EXCLAM e) }
| h=HAT            { mktok $startpos $endpos (T.HAT h) }
| PTR_STAR         { mktok $startpos $endpos T.PTR_STAR }
| PTR_AMP          { mktok $startpos $endpos T.PTR_AMP }
| LT_LT            { mktok $startpos $endpos T.LT_LT }
| GT_GT            { mktok $startpos $endpos T.GT_GT }
| PTR_AMP_AMP      { mktok $startpos $endpos T.PTR_AMP_AMP }
| a=AMP_AMP        { mktok $startpos $endpos (T.AMP_AMP a) }
| b=BAR_BAR        { mktok $startpos $endpos (T.BAR_BAR b) }
| MINUS_GT         { mktok $startpos $endpos T.MINUS_GT }
| t=TILDE          { mktok $startpos $endpos (T.TILDE t) }
| SHARP            { mktok $startpos $endpos T.SHARP }
| COLON            { mktok $startpos $endpos T.COLON }
| COLON_COLON      { mktok $startpos $endpos T.COLON_COLON }
| HEAD_COLON_COLON { mktok $startpos $endpos T.COLON_COLON }
| COMMA            { mktok $startpos $endpos T.COMMA }
| VOLATILE         { mktok $startpos $endpos T.VOLATILE }
| BOOL             { mktok $startpos $endpos T.BOOL }
| CHAR             { mktok $startpos $endpos T.CHAR }
| SHORT            { mktok $startpos $endpos T.SHORT }
| INT              { mktok $startpos $endpos T.INT }
| LONG             { mktok $startpos $endpos T.LONG }
| FLOAT            { mktok $startpos $endpos T.FLOAT }
| DOUBLE           { mktok $startpos $endpos T.DOUBLE }
| SIGNED           { mktok $startpos $endpos T.SIGNED }
| UNSIGNED         { mktok $startpos $endpos T.UNSIGNED }
| INLINE           { mktok $startpos $endpos T.INLINE }
| CONST            { mktok $startpos $endpos T.CONST }
| STRUCT           { mktok $startpos $endpos T.STRUCT }
| GOTO             { mktok $startpos $endpos T.GOTO }
| IF               { mktok $startpos $endpos T.IF }
| ELSE             { mktok $startpos $endpos T.ELSE }
| STATIC_CAST      { mktok $startpos $endpos T.STATIC_CAST }
| REINTERPRET_CAST { mktok $startpos $endpos T.REINTERPRET_CAST }
| DYNAMIC_CAST     { mktok $startpos $endpos T.DYNAMIC_CAST }
| CONST_CAST       { mktok $startpos $endpos T.CONST_CAST }
| s=PP_STRINGIZED  { mktok $startpos $endpos (T.PP_STRINGIZED s) }
| BS               { mktok $startpos $endpos T.BS }
| s=OBJC_UNKNOWN   { mktok $startpos $endpos (T.OBJC_UNKNOWN s) }
;

%inline
asm_token:
| LPAREN    { mktok $startpos $endpos T.LPAREN }
| TY_LPAREN { mktok $startpos $endpos T.TY_LPAREN }
| PP_LPAREN { mktok $startpos $endpos T.PP_LPAREN }
| RPAREN    { mktok $startpos $endpos T.RPAREN }
| LBRACKET  { mktok $startpos $endpos T.LBRACKET }
| RBRACKET  { mktok $startpos $endpos T.RBRACKET }
| m=MS_ASM  { mktok $startpos $endpos (T.MS_ASM m) }
| PP_DEFINE { mktok $startpos $endpos T.PP_DEFINE }
| PP_UNDEF  { mktok $startpos $endpos T.PP_UNDEF }
| PP_       { mktok $startpos $endpos T.PP_ }
| NEWLINE   { mktok $startpos $endpos T.NEWLINE }
| t=token_no_paren { t }
;

stmt_macro_call:
| i=IDENT_SM ml=macro_args { mknode $startpos $endpos (L.StatementMacroInvocation i) ml }
| i=IDENT_SM SS_LPAREN sl=statement_seq RPAREN { mknode $startpos $endpos (L.StatementMacroInvocation i) sl }
;

iteration_macro:
| e=expression c=compound_statement
    { 
      if e#nchildren > 0 then
        let c0 = List.hd e#children in
        match c0#label with
        | L.Identifier i ->
            mknode ~pvec:[1; 1] $startpos $endpos (L.IterationMacroInvocation i) [e; c]
        | _ -> mknode ~pvec:[1; 1] $startpos $endpos L.AMBIGUOUS_CONSTRUCT [e; c]
      else
        match e#label with
        | L.Identifier i -> mknode $startpos $endpos (L.IterationMacro i) [c]
        | _ -> mknode ~pvec:[1; 1] $startpos $endpos L.AMBIGUOUS_CONSTRUCT [e; c]
    } (* for FOREACH-like macro *)
;

try_block:
(*| TRY c=compound_statement hl=handler_seq
    { mknode ~pvec:[1; List.length hl] $startpos $endpos L.TryBlock (c::hl) }
| p=pp_ifx t=TRY pp_endif c=compound_statement hl=handler_seq
    { 
      ignore t;
      let t_ = mkleaf $startpos(t) $endpos(t) L.Try in
      let n_ = mknode $startpos $endpos(t) (pp_if_group()) [p; t_] in
      mknode ~pvec:[1; 1; List.length hl] $startpos $endpos L.PpIfSectionTryBlock (n_::c::hl)
    }*)
| TRY c=compound_statement { mknode ~pvec:[1; 0] $startpos $endpos L.TryBlock [c] }

| p=pp_ifx t=TRY pe=pp_endif c=compound_statement
    { 
      ignore t;
      let t_ = mkleaf $startpos(t) $endpos(t) L.Try in
      let n_ = mknode $startpos $endpos(t) (_pp_if_group pe) [p; t_] in
      mknode ~pvec:[1; 1; 0] $startpos $endpos L.PpIfSectionTryBlock [n_; c]
    }
(*| c=CATCH TY_LPAREN e=exception_declaration r=RPAREN { mknode ~pvec:[1; 0] $startpos $endpos L.Handler [e] }*)
| h=handler { h }
;

iteration_statement:
| WHILE LPAREN c=condition RPAREN s=statement
    { mknode ~pvec:[1; 1] $startpos $endpos L.WhileStatement [c; s] }
| DO s=statement
    { mknode ~pvec:[1; 0] $startpos $endpos L.DoStatement [s] } %prec PREC
| DO s=statement WHILE LPAREN e=expression RPAREN SEMICOLON
    { mknode ~pvec:[1; 1] $startpos $endpos L.DoStatement [s; e] }

| ODD_FOR LPAREN i=init_statement c_opt=ioption(condition) SEMICOLON e_opt=ioption(expression) RPAREN
    { 
      let cl = opt_to_list c_opt in
      let el = opt_to_list e_opt in
      let pvec = [1; List.length cl; List.length el; 0] in
      mknode ~pvec $startpos $endpos L.ForStatement (i :: cl @ el)
    }
| FOR LPAREN i=init_statement c_opt=ioption(condition) SEMICOLON e_opt=ioption(expression) RPAREN
    s=statement
    { 
      let cl = opt_to_list c_opt in
      let el = opt_to_list e_opt in
      let pvec = [1; List.length cl; List.length el; 1] in
      mknode ~pvec $startpos $endpos L.ForStatement (i :: cl @ el @ [s])
    }
| FOR LPAREN
    i_opt=ioption(init_statement) f=for_range_declaration COLON fi=for_range_initializer
    RPAREN s=statement
    { 
      let il = opt_to_list i_opt in
      let pvec = [List.length il; 1; 1; 1] in
      mknode ~pvec $startpos $endpos L.RangeBasedForStatement (il @ [f; fi; s])
    }
| FOR LPAREN i=objc_identifier IN e=expression RPAREN s=statement
    { mknode ~pvec:[1; 1; 1] $startpos $endpos L.ForInStatement [i; e; s] }
;

for_range_declaration:
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 0; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ForRangeDeclaration (al @ dl @ [dt])
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq r_opt=ioption(ref_qualifier)
    LBRACKET il=identifier_list RBRACKET
    { 
      let al = list_opt_to_list al_opt in
      let rl = opt_to_list r_opt in
      let pvec = [List.length al; List.length dl; 0; List.length rl; List.length il] in
      mknode ~pvec $symbolstartpos $endpos L.ForRangeDeclaration (al @ dl @ rl @ il)
    }
;

for_range_initializer:
| e=expr_or_braced_init_list { e }
;

jump_statement:
| j=_jump_statement sc=SEMICOLON { if sc then j#add_suffix ";"; reloc $startpos $endpos j }
;
%inline
_jump_statement:
| BREAK { mkleaf $startpos $endpos L.BreakStatement }
| CONTINUE { mkleaf $startpos $endpos L.ContinueStatement }
| RETURN e_opt=ioption(expr_or_braced_init_list) { mknode $startpos $endpos L.ReturnStatement (opt_to_list e_opt) }
| GOTO i=IDENT
    { 
      let n = mkleaf $startpos $endpos (L.GotoStatement i) in
      env#register_label n;
      n
    }
| GOTO e=unary_expression { mknode $startpos $endpos L.ComputedGotoStatement [e] }
| CO_RETURN e_opt=ioption(expr_or_braced_init_list)
    { mknode $startpos $endpos L.CoroutineReturnStatement (opt_to_list e_opt) }

| RETURN lp=LPAREN c=condition b=BAR_BAR_BROKEN p=pp_ifstmt_if_section_closing
    { 
      ignore lp;
      let o_ = mknode ~pvec:[1; 1] $startpos(c) $endpos (L.LogicalOrExpression b) [c; p] in
      let e_ = mknode $startpos(lp) $endpos L.ParenthesizedExpression [o_] in
      mknode $startpos $endpos L.ReturnStatement [e_]
    }
;

(*declaration_statement:
| block_declaration { }
;*)

block_declaration:
| b=_block_declaration sc=SEMICOLON { if sc then b#add_suffix ";"; reloc $startpos $endpos b }
| b=_block_declaration s=DELIM_MACRO { b#add_suffix (" "^s); reloc $startpos $endpos b }
;

_block_declaration:
| s=_simple_declaration { s }
| a=_asm_declaration { a }
| n=_namespace_alias_definition { n }
| u=_using_declaration { u }
| u=_using_directive { u }
| s=_static_assert_declaration { s }
| a=_alias_declaration { a }
| o=_opaque_enum_declaration { o }
;

%inline
enum_head_name:
| i=IDENT
    { 
      let uqn = Ast.encode_ident i in
      mkleaf ~pvec:[0] $startpos $endpos (L.EnumHeadName uqn)
    }
| n=nested_name_specifier i=IDENT
    { 
      let uqn = Ast.encode_ident i in
      mknode ~pvec:[1] $startpos $endpos (L.EnumHeadName uqn) [n]
    }
;

_opaque_enum_declaration:
| e=enum_key al_opt=attribute_specifier_seq_opt eh=enum_head_name e_opt=ioption(enum_base)
    { 
      let lab = L.EnumKey.to_opaque_enum_declaration e in
      let al = list_opt_to_list al_opt in
      let el = opt_to_list e_opt in
      let pvec = [List.length al; 1; List.length el] in
      mknode ~pvec $startpos $endpos lab (al @ eh :: el)
    }
;

enum_base:
| COLON tl=type_specifier_seq { mknode $startpos $endpos L.EnumBase tl }
| i=BASE_MACRO { mkleaf $startpos $endpos (L.BaseMacro i) }
;

enum_key:
| ENUM        { Ast.L.EnumKey.Enum }
| ENUM CLASS  { Ast.L.EnumKey.EnumClass }
| ENUM STRUCT { Ast.L.EnumKey.EnumStruct }
;

_alias_declaration:
| USING i=IDENT al_opt=attribute_specifier_seq_opt EQ d=defining_type_id
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[List.length al; 1] $startpos $endpos (L.AliasDeclaration i) (al @ [d])
    }
;

defining_type_id:
| dl=defining_type_specifier_seq a_opt=ioption(abstract_declarator)
    { 
      let al = opt_to_list a_opt in
      let pvec = [List.length dl; List.length al] in
      mknode ~pvec $startpos $endpos L.DefiningTypeId (dl @ al)
    }
;

defining_type_specifier_seq:
| d=defining_type_specifier al_opt=attribute_specifier_seq_opt
    { [mknodea al_opt $startpos $endpos L.DefiningTypeSpecifier d] }
| dl=defining_type_specifier_seq d=defining_type_specifier { dl@[d] }
;

_static_assert_declaration:
| STATIC_ASSERT LPAREN c=constant_expression RPAREN
    { mknode ~pvec:[1; 0] $startpos $endpos L.Static_assertDeclaration [c] }
| STATIC_ASSERT LPAREN c=constant_expression COMMA s=string_literal_ RPAREN
    { mknode ~pvec:[1; 1] $startpos $endpos L.Static_assertDeclaration [c; s] }
;

_using_directive:
| al_opt=attribute_specifier_seq_opt USING NAMESPACE q_i=qualified_namespace_specifier
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; 1] in
      let q, i = q_i in
      begin
        try
          (env#lookup_namespace i)#def_adder()
        with
          Not_found -> ()
      end;
      mknode ~pvec $symbolstartpos $endpos (L.UsingDirective i) (al @ [q])
    }
;

_namespace_alias_definition:
| NAMESPACE i=IDENT EQ q_i=qualified_namespace_specifier
    { 
      let q, oi = q_i in
      begin
        try
          env#stack#top#register i (env#lookup_namespace oi)
        with
          Not_found -> ()
      end;
      mknode $startpos $endpos (L.NamespaceAliasDefinition i) [q]
    }
;

%inline
qualified_namespace_specifier:
| i=IDENT { (mkleaf $startpos $endpos (L.QualifiedNamespaceSpecifier i)), i }
| n=nested_name_specifier i=IDENT
    { 
      (*let pi = (Ast.prefix_of_nested_name_specifier n)^i in*)
      (mknode $startpos $endpos (L.QualifiedNamespaceSpecifier i) [n]), i
    }
;

_asm_declaration:
| al_opt=attribute_specifier_seq_opt ASM tl=list(gnu_asm_token)
    { 
      let lab = make_asm_lab tl in
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[List.length al; 0] $symbolstartpos $endpos lab al
    }
| al_opt=attribute_specifier_seq_opt ASM g=gnu_asm_fragment gl=gnu_asm_frag_seq
    { 
      let al = list_opt_to_list al_opt in
      let l = g :: gl in
      let pvec = [List.length al; List.length l] in
      mknode ~pvec $symbolstartpos $endpos (L.AsmDefinition "") (al @ l)
    }
;

labeled_statement:
(*| al_opt=attribute_specifier_seq_opt i=IDENT COLON s=statement
    { mknode $startpos $endpos (L.LabeledStatement i) ((list_opt_to_list al_opt) @ [s]) }
| al_opt=attribute_specifier_seq_opt CASE c=constant_expression COLON s=statement
    { mknode $startpos $endpos L.CaseStatement ((list_opt_to_list al_opt) @ [c; s]) }
| al_opt=attribute_specifier_seq_opt DEFAULT COLON s=statement
    { mknode $startpos $endpos L.DefaultStatement ((list_opt_to_list al_opt) @ [s]) }*)
| ll=label_seq u=unlabeled_statement
    { 
      List.iter
        (fun l ->
          try
            env#register_label ~replace:true l
          with _ -> ()
        ) ll;
      mknode ~pvec:[List.length ll; 1] $startpos $endpos L.LabeledStatement (ll @ [u])
    }
;

label_seq:
| l=label { [l] }
| ll=label_seq l=label { ll @ [l] }
;

label:
| al_opt=attribute_specifier_seq_opt i=IDENT COLON
    { mknode $symbolstartpos $endpos (L.Label i) (list_opt_to_list al_opt) }
| al_opt=attribute_specifier_seq_opt CASE c=constant_expression COLON
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[List.length al; 1] $symbolstartpos $endpos L.CaseLabel (al @ [c])
    }
| al_opt=attribute_specifier_seq_opt DEFAULT COLON
    { mknode $symbolstartpos $endpos L.DefaultLabel (list_opt_to_list al_opt) }
| al_opt=attribute_specifier_seq_opt CASE l0=literal_or_ident ELLIPSIS l1=literal_or_ident COLON
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[List.length al; 1; 1] $symbolstartpos $endpos L.RangedCaseLabel (al @ [l0; l1])
    }
| i=id_macro_call COLON
    { 
      i#relab (L.LabelMacroInvocation i#get_name);
      reloc $startpos $endpos i
    }
;

%inline
literal_or_ident:
| l=literal { l }
| i=identifier { i }
;

selection_statement:
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    s=statement
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s])
    } %prec PREC
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    s=statement ODD_ELSE
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    s0=statement ELSE s1=statement
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 1] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; s0; s1])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) RPAREN
    s0=statement ELSE s1=statement
    { 
      warning $startpos $endpos "no condition found in if-statement";
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 0; 1; 1] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [s0; s1])
    }
| ELSE s=statement { mknode $startpos $endpos L.ElseStatement [s] }
| SWITCH LPAREN i_opt=ioption(init_statement) c=condition RPAREN s=statement
    { 
      let il = opt_to_list i_opt in
      begin
        match (i_opt : Ast.node option) with
        | Some i -> i#add_prefix "("
        | None -> c#add_prefix "("
      end;
      c#add_suffix ")";
      mknode ~pvec:[List.length il; 1; 1] $startpos $endpos L.SwitchStatement (il @ [c; s])
    }
| SWITCH e=expr_macro_call s=statement
    { mknode ~pvec:[0; 1; 1] $startpos $endpos L.SwitchStatement [e; s] }
| IF e=expr_macro_call s=statement
    { mknode ~pvec:[0; 0; 1; 1; 0] $startpos $endpos L.IfStatement [e; s] }

| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition AMP_AMP_BROKEN
    p=pp_ifstmt_if_section_closing
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; p])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition BAR_BAR_BROKEN
    p=pp_ifstmt_if_section_closing
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; p])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition MARKER
    p=pp_ifstmt_if_section_closing
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; p])
    }
| IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition MARKER
    p=_pp_land_if_section el=expression_list RPAREN SEMICOLON sl_opt=statement_seq_opt RBRACE
    { 
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let sl = list_opt_to_list sl_opt in
      p#add_children_r (el@sl);
      p#set_pvec (p#pvec @ [List.length el; List.length sl]);
      let pvec = [List.length cl; List.length il; 1; 1; 0] in
      mknode ~pvec $startpos $endpos L.IfStatement (cl @ il @ [c; p])
    }
;
%inline
constexpr:
| CONSTEXPR { mkleaf $startpos $endpos L.Constexpr }
;

pp_ifstmt_if_section_closing:
| p=pp_ifstmt_if_group_closing
    pl=pp_ifstmt_elif_group_closing*
    p_opt=ioption(pp_ifstmt_else_group_closing)
    pe=pp_endif l_opt=ioption(BRACE_LEVEL)
    { 
      let pl1 = opt_to_list p_opt in
      let blv =
        match l_opt with
        | Some lv -> lv
        | None -> 0
      in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(blv, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_ifstmt_if_group_closing:
| p=pp_ifx_e c=condition RPAREN s_opt=ioption(SEMICOLON)
    { 
      c#add_suffix ")";
      begin
        match s_opt with
        | Some true -> begin
            c#add_suffix ";";
            _reloc_end $endpos c
        end
        | _ -> ()
      end;
      let pvec = [1; 1; 0] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [p; c]
    }
| p=pp_ifx_e b=BAR_BAR c=condition RPAREN
    { 
      let o = mknode ~pvec:[0; 1] $startpos(b) $endpos(c) (L.LogicalOrExpression b) [c] in
      o#add_suffix ")";
      mknode ~pvec:[1; 1; 0] $startpos $endpos (pp_if_group()) [p; o]
    }
| p=pp_ifx_e c=condition RPAREN s=compound_statement
    { 
      let pvec = [1; 1; 1] in
      c#add_suffix ")";
      mknode ~pvec $startpos $endpos (pp_if_group()) [p; c; s]
    }
| p=pp_ifx_e pi=pp_ifstmt_if_section_closing { mknode $startpos $endpos (pp_if_group()) [p; pi] }
;
pp_ifstmt_elif_group_closing:
| p=pp_elif c=condition RPAREN s_opt=ioption(SEMICOLON)
    { 
      c#add_suffix ")";
      begin
        match s_opt with
        | Some true -> begin
            c#add_suffix ";";
            _reloc_end $endpos c
        end
        | _ -> ()
      end;
      let pvec = [1; 1; 0] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p; c]
    }
| p=pp_elif b=BAR_BAR c=condition RPAREN
    { 
      let o = mknode ~pvec:[0; 1] $startpos(b) $endpos(c) (L.LogicalOrExpression b) [c] in
      o#add_suffix ")";
      mknode ~pvec:[1; 1; 0] $startpos $endpos (_pp_elif_group p) [p; o]
    }
| p=pp_elif c=condition RPAREN s=compound_statement
    { 
      let pvec = [1; 1; 1] in
      c#add_suffix ")";
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p; c; s]
    }
| p=pp_elif pi=pp_ifstmt_if_section_closing { mknode $startpos $endpos (_pp_elif_group p) [p; pi] }
;
pp_ifstmt_else_group_closing:
| p=pp_else c=condition RPAREN s_opt=ioption(SEMICOLON)
    { 
      c#add_suffix ")";
      begin
        match s_opt with
        | Some true -> begin
            c#add_suffix ";";
            _reloc_end $endpos c
        end
        | _ -> ()
      end;
      let pvec = [1; 1; 0] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p; c]
    }
| p=pp_else b=BAR_BAR c=condition RPAREN
    { 
      let o = mknode ~pvec:[0; 1] $startpos(b) $endpos(c) (L.LogicalOrExpression b) [c] in
      o#add_suffix ")";
      mknode ~pvec:[1; 1; 0] $startpos $endpos (_pp_else_group p) [p; o]
    }
| p=pp_else c=condition RPAREN s=compound_statement
    { 
      let pvec = [1; 1; 1] in
      c#add_suffix ")";
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p; c; s]
    }
| p=pp_else pi=pp_ifstmt_if_section_closing { mknode $startpos $endpos (_pp_else_group p) [p; pi] }
;


decl_OR_expr:
| b=_block_declaration
    { 
      begin
        match b#label with
        | L.SimpleDeclaration -> b#relab L.DeclarationStatement
        | _ -> ()
      end;
      b
    }
| al_opt=attribute_specifier_seq_opt e=expression
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[List.length al; 1] $symbolstartpos $endpos L.ExpressionStatement (al @ [e])
    }
;

init_statement:
| i=_init_statement sc=SEMICOLON { if sc then i#add_suffix ";"; reloc $startpos $endpos i }
| sc=SEMICOLON
    { 
      let s = mkleaf $startpos $endpos L.ExpressionStatement in
      if sc then s#add_suffix ";";
      s
    }
(*| expression_statement { }
| simple_declaration { }*)
;

_init_statement:
| e=expression { mknode $startpos $endpos L.ExpressionStatement [e] }
| s=_simple_declaration { s#relab L.DeclarationStatement; s }
;

_simple_declaration:
| dl=decl_specifier_seq
    { 
      let pvec = [0; List.length dl; 0] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration dl in
      (*env#register_variables nd;*)
      nd
     }
| dl=decl_specifier_seq il=init_declarator_list
    { 
      let pvec = [0; List.length dl; List.length il] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration (dl @ il) in
      env#register_variables nd;
      nd
     }
| DUMMY_TYPE il=init_declarator_list
    { 
      let pvec = [0; 0; List.length il] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration il in
      (*env#register_variables nd;*)
      nd
     }
| al=attribute_specifier_seq dl=decl_specifier_seq il=init_declarator_list
    { 
      let pvec = [List.length al; List.length dl; List.length il] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration (al @ dl @ il) in
      env#register_variables nd;
      nd
    }
| al=attribute_specifier_seq dl=decl_specifier_seq d=decl_spec_macro_call
    { 
      let pvec = [List.length al; List.length dl; 1] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration (al @ dl @ [d]) in
      env#register_variables nd;
      nd
    }
| d=decl_spec_macro_call COMMA il=init_declarator_list
    { 
      let pvec = [0; 1; List.length il] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration (d :: il) in
      (*env#register_variables nd;*)
      nd
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq r_opt=ioption(ref_qualifier)
    LBRACKET il=identifier_list RBRACKET i=initializer_
    { 
      let al = list_opt_to_list al_opt in
      let rl = opt_to_list r_opt in
      let pvec = [List.length al; List.length dl; 0; List.length rl; List.length il; 1] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration (al @ dl @ rl @ il @ [i])
      in
      env#register_variables nd;
      nd
    }
;

%inline
simple_declaration_broken:
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq d=declarator MARKER EQ LBRACE
    { 
      let al = list_opt_to_list al_opt in
      let d_ = mknode ~pvec:[0; 1; 0] $startpos(d) $endpos(d) L.InitDeclarator [d] in
      let pvec = [List.length al; List.length dl; 1] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.SimpleDeclaration (al @ dl @ [d_]) in
      env#register_variables nd;
      nd
    }
;

(*simple_declaration:
| _simple_declaration SEMICOLON { }
;*)

init_declarator_list:
| i=init_declarator { [i] }
| p=pp_idtor_if_section { [p] }
| p=_pp_idtor_if_section { [p] }
| il=init_declarator_list COMMA { (Xlist.last il)#add_suffix ","; il }
| il=init_declarator_list COMMA i=init_declarator
    { 
      (Xlist.last il)#add_suffix ",";
      il @ [add_attrs_l None $startpos(i) $endpos i]
    }
| il=init_declarator_list COMMA al=gnu_attribute_seq i=init_declarator
    { 
      (Xlist.last il)#add_suffix ",";
      il @ [add_attrs_l (Some al) $startpos(al) $endpos i]
    }
| il=init_declarator_list COMMA p=pp_idtor_if_section
    { 
      (Xlist.last il)#add_suffix ",";
      il @ [p]
    }
| il=init_declarator_list COMMA p=pp_idtor_if_section i=init_declarator
    { 
      (Xlist.last il)#add_suffix ",";
      il @ [p; i]
    }
| p=pp_idtor_if_section i=init_declarator { [p; i] }

| il=init_declarator_list SECTION_MARKER p=pp_idtor_if_section { il @ [p] }
;


_pp_idtor_if_section:
| p=ptr_operator i=pp_idtor_if_section
    { 
      let pvec = [0; 1; 0; 1] in
      mknode ~pvec $startpos $endpos L.PtrDeclaratorPtr [p; i]
    }
;

pp_idtor_if_section:
| p=pp_idtor_if_group pl=pp_idtor_elif_group* p_opt=ioption(pp_idtor_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_idtor_if_group:
| p=pp_ifx_i ioption(COMMA) il=init_declarator_list ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length il] $startpos $endpos (pp_if_group()) (p::il) }
| p=pp_ifx_i ioption(COMMA) il=init_declarator_list SEMICOLON dl=declaration_seq
    { mknode ~pvec:[1; List.length il+List.length dl] $startpos $endpos (pp_if_group()) (p::il@dl) }
;
pp_idtor_elif_group:
| p=pp_elif ioption(COMMA) il=init_declarator_list ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length il] $startpos $endpos (_pp_elif_group p) (p::il) }
| p=pp_elif ioption(COMMA) il=init_declarator_list SEMICOLON dl=declaration_seq
    { mknode ~pvec:[1; List.length il+List.length dl] $startpos $endpos (_pp_elif_group p) (p::il@dl) }
;
pp_idtor_else_group:
| p=pp_else ioption(COMMA) il=init_declarator_list ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length il] $startpos $endpos (_pp_else_group p) (p::il) }
| p=pp_else ioption(COMMA) il=init_declarator_list SEMICOLON dl=declaration_seq
    { mknode ~pvec:[1; List.length il+List.length dl] $startpos $endpos (_pp_else_group p) (p::il@dl) }
;

init_declarator:
| d=declarator i_opt=ioption(initializer_)
    { 
      let il = opt_to_list i_opt in
      mknode ~pvec:[0; 1; List.length il] $startpos $endpos L.InitDeclarator (d::il)
    }
| d=declarator r=requires_clause
    { mknode ~pvec:[0; 1; 1] $startpos $endpos L.InitDeclarator [d; r] }
| d=declarator i=pp_init_if_section
    { 
      mknode ~pvec:[0; 1; 1] $startpos $endpos L.InitDeclarator [d; i]
    }
;

identifier_list:
| i=identifier { [i] }
| p=PARAM_DECL_MACRO i=identifier { [mkleaf $startpos $endpos(p) (L.ParamDeclMacro p); i] }
| il=identifier_list COMMA i=identifier { (Xlist.last il)#add_suffix ","; il @ [i] }
| il=identifier_list MARKER pi=pp_ifx_e COMMA i=identifier pe=pp_endif
    { 
      i#add_prefix ",";
      let ifg = mknode ~pvec:[1; 1] $startpos(pi) $endpos(i) (_pp_if_group pe) [pi; i] in
      let ifs = mknode ~pvec:[1; 0; 0; 1] $startpos(pi) $endpos(pe) (L.PpIfSection(0, get_pp_if_cond pe)) [ifg; pe] in
      il @ [ifs]
    }
;
%inline
identifier:
| i=IDENT_V { mkleaf $startpos $endpos (L.Identifier i) }
;

condition:
| e=expression { e }
| al_opt=attribute_specifier_seq_opt
    dl=decl_specifier_seq dt=declarator b=brace_or_equal_initializer
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 1] in
      mknode ~pvec $symbolstartpos $endpos L.Condition (al @ dl @ [dt; b])
    }
| o=objc_available { o }
;

(*expression_statement:
| expression SEMICOLON { }
;*)


declarator:
| p=ptr_declarator { p }
| n=noptr_declarator p=parameters_and_qualifiers t=trailing_return_type
    { mknode ~pvec:[1; 1; 1] $startpos $endpos L.DeclaratorFunc [n; p; t] }
;

ptr_declarator:
| n=noptr_declarator { n }
| c_opt=ioption(calling_convention) p=ptr_operator pd=ptr_declarator
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 1; 0; 1] in
      mknode ~pvec $symbolstartpos $endpos L.PtrDeclaratorPtr (cl @ [p; pd])
    }
| p=ptr_operator c=calling_convention pd=noptr_declarator
    { 
      let pvec = [0; 1; 1; 1] in
      mknode ~pvec $startpos $endpos L.PtrDeclaratorPtr [p; c; pd]
    }
;
%inline
calling_convention:
| m=ms_stdcall { m }
| m=ms_cdecl { m }
| c=cc_macro { c }
| p=pp_cc_if_section { p }
;
%inline
ms_stdcall:
| m=MS_STDCALL { mkleaf $startpos $endpos (L.MsStdcall m) }
;
%inline
ms_cdecl:
| m=MS_CDECL { mkleaf $startpos $endpos (L.MsCdecl m) }
;
%inline
cc_macro:
| i=CC_MACRO { mkleaf $startpos $endpos (L.CallingConvention i) }
;
pp_cc_if_section:
| p=pp_cc_if_group pl=pp_cc_elif_group* p_opt=ioption(pp_cc_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_cc_if_group:
| p=pp_ifx_c c=calling_convention
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[c]) }
;
pp_cc_elif_group:
| p=pp_elif c=calling_convention
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[c]) }
;
pp_cc_else_group:
| p=pp_else c=calling_convention
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[c]) }
;

noptr_declarator:
| DUMMY_DTOR { mkleaf $startpos $endpos L.DummyDtor }
| i=DTOR_MACRO { mkleaf $startpos $endpos (L.DtorMacro i) }
| d=declarator_id al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[1; List.length al] $startpos $endpos L.NoptrDeclaratorId (d::al)
    }
| d=declarator_id g=gnu_asm_attr al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[1; List.length al + 1] $startpos $endpos L.NoptrDeclaratorId (d::g::al)
    }
| n=noptr_declarator p=parameters_and_qualifiers
    { mknode ~pvec:[1; 1] $startpos $endpos L.NoptrDeclaratorFunc [n; p] }

| n=noptr_declarator PS_LPAREN il=identifier_list RPAREN ol_opt=ioption(old_param_decl_list)
    { 
      let ol = list_opt_to_list ol_opt in
      let pvec = [1; List.length il; List.length ol] in
      mknode ~pvec $startpos $endpos L.NoptrDeclaratorOldFunc (n :: il @ ol)
    }
| n=noptr_declarator LBRACKET pl=pp_control_line* c_opt=ioption(constant_expression) RBRACKET
    g_opt=ioption(gnu_asm_attr) al_opt=attribute_specifier_seq_opt
    { 
      let cl = opt_to_list c_opt in
      let gl = opt_to_list g_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length pl; List.length cl; List.length gl + List.length al] in
      mknode ~pvec $startpos $endpos L.NoptrDeclaratorArray (n :: pl @ cl @ gl @ al)
    }
| TY_LPAREN p=ptr_declarator RPAREN { mknode $startpos $endpos L.NoptrDeclaratorParen [p] }
| p=pp_dtor_if_section { p }
| p=pp_dtor_if_section ol=old_param_decl_list
    { 
      let pvec = [1; 0; List.length ol] in
      mknode ~pvec $startpos $endpos L.NoptrDeclaratorOldFunc (p :: ol)
    }
| n=noptr_declarator p=pp_dtor_if_section { mknode $startpos $endpos L.NoptrDeclarator [n; p] }
| n=noptr_declarator s=suffix_macro { mknode $startpos $endpos L.NoptrDeclarator [n; s] }
;

%inline
suffix_macro:
| i=SUFFIX_MACRO { mkleaf $startpos $endpos (L.SuffixMacro i) }
| s=suffix_macro_call { s }
;

suffix_macro_call:
| i=IDENT_SXM ml=macro_args { mknode $startpos $endpos (L.SuffixMacroInvocation i) ml }
;

pp_attr_if_section:
| p=pp_attr_if_group pl=pp_attr_elif_group* p_opt=ioption(pp_attr_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_attr_if_group:
| p=pp_ifx_attr al_opt=attribute_specifier_seq_opt
    { 
      let a_ = mknode $startpos(al_opt) $endpos L.SPECS (list_opt_to_list al_opt) in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; a_]
    }
;
pp_attr_elif_group:
| p=pp_elif al_opt=attribute_specifier_seq_opt
    { 
      let a_ = mknode $startpos(al_opt) $endpos L.SPECS (list_opt_to_list al_opt) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; a_]
    }
;
pp_attr_else_group:
| p=pp_else al_opt=attribute_specifier_seq_opt
    { 
      let a_ = mknode $startpos(al_opt) $endpos L.SPECS (list_opt_to_list al_opt) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; a_]
    }
;

(*%inline*)
gnu_asm_attr:
| a=GNU_ASM LPAREN e=expression RPAREN
    { 
      e#add_prefix "(";
      e#add_suffix ")";
      mknode $startpos $endpos (L.GnuAsmBlockFragmented a) [e]
    }
;

pp_dtor_if_section:
| p=pp_dtor_if_group pl=pp_dtor_elif_group* p_opt=ioption(pp_dtor_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_dtor_if_group:
| p=pp_ifx_e ioption(COMMA) n=noptr_declarator ioption(SEMICOLON)
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[n]) }
| p=pp_ifx_e pq=parameters_and_qualifiers ioption(SEMICOLON)
    { 
      let d = mknode ~pvec:[0; 1] $startpos(pq) $endpos L.NoptrDeclaratorFunc [pq] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[d])
    }
| p=pp_ifx_e ps=PS_LPAREN il=identifier_list RPAREN ol_opt=ioption(old_param_decl_list)
    { 
      ignore ps;
      let ol = list_opt_to_list ol_opt in
      let pvec = [0; List.length il; List.length ol] in
      let d = mknode ~pvec $startpos(ps) $endpos L.NoptrDeclaratorOldFunc (il @ ol) in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[d])
    }
| p=pp_ifx_e pq=parameters_and_qualifiers b=function_body
    { 
      env#clear_in_body_brace_flag();
      let d = mknode ~pvec:[0; 1] $startpos(pq) $endpos(pq) L.NoptrDeclaratorFunc [pq] in
      let h = mknode ~pvec:[0; 0; 1; 0; 0; 1] $startpos(pq) $endpos L.FunctionDefinition [d; b] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[h])
    }
;
pp_dtor_elif_group:
| p=pp_elif ioption(COMMA) n=noptr_declarator ioption(SEMICOLON)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[n]) }
| p=pp_elif pq=parameters_and_qualifiers ioption(SEMICOLON)
    { 
      let d = mknode ~pvec:[0; 1] $startpos(pq) $endpos L.NoptrDeclaratorFunc [pq] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[d])
    }
| p=pp_elif ps=PS_LPAREN il=identifier_list RPAREN ol_opt=ioption(old_param_decl_list)
    { 
      ignore ps;
      let ol = list_opt_to_list ol_opt in
      let pvec = [0; List.length il; List.length ol] in
      let d = mknode ~pvec $startpos(ps) $endpos L.NoptrDeclaratorOldFunc (il @ ol) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[d])
    }
| p=pp_elif pq=parameters_and_qualifiers b=function_body
    { 
      env#clear_in_body_brace_flag();
      let d = mknode ~pvec:[0; 1] $startpos(pq) $endpos(pq) L.NoptrDeclaratorFunc [pq] in
      let h = mknode ~pvec:[0; 0; 1; 0; 0; 1] $startpos(pq) $endpos L.FunctionDefinition [d; b] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[h])
    }
;
pp_dtor_else_group:
| p=pp_else ioption(COMMA) n=noptr_declarator ioption(SEMICOLON)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[n]) }
| p=pp_else pq=parameters_and_qualifiers ioption(SEMICOLON)
    { 
      let d = mknode ~pvec:[0; 1] $startpos(pq) $endpos L.NoptrDeclaratorFunc [pq] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[d])
    }
| p=pp_else ps=PS_LPAREN il=identifier_list RPAREN ol_opt=ioption(old_param_decl_list)
    { 
      ignore ps;
      let ol = list_opt_to_list ol_opt in
      let pvec = [0; List.length il; List.length ol] in
      let d = mknode ~pvec $startpos(ps) $endpos L.NoptrDeclaratorOldFunc (il @ ol) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[d])
    }
| p=pp_else pq=parameters_and_qualifiers b=function_body
    { 
      env#clear_in_body_brace_flag();
      let d = mknode ~pvec:[0; 1] $startpos(pq) $endpos(pq) L.NoptrDeclaratorFunc [pq] in
      let h = mknode ~pvec:[0; 0; 1; 0; 0; 1] $startpos(pq) $endpos L.FunctionDefinition [d; b] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[h])
    }
;

pp_old_param_decl_list_if_section:
| p=pp_old_param_decl_list_if_group
    pl=list(pp_old_param_decl_list_elif_group) p_opt=ioption(pp_old_param_decl_list_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_old_param_decl_list_if_group:
| p=pp_ifx_d pl=old_param_decl_list
    { 
      let pvec = [1; List.length pl] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p :: pl)
    }
| p=pp_ifx_d pl=pp_control_line+
    { 
      let pvec = [1; List.length pl] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p :: pl)
    }
;
pp_old_param_decl_list_elif_group:
| p=pp_elif pl=old_param_decl_list
    { 
      let pvec = [1; List.length pl] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p :: pl)
    }
| p=pp_elif pl=pp_control_line+
    { 
      let pvec = [1; List.length pl] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p :: pl)
    }
;
pp_old_param_decl_list_else_group:
| p=pp_else pl=old_param_decl_list
    { 
      let pvec = [1; List.length pl] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p :: pl)
    }
| p=pp_else pl=pp_control_line+
    { 
      let pvec = [1; List.length pl] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p :: pl)
    }
;

old_param_decl_list:
| o=old_param_decl { [o] }
| p=pp_old_param_decl_list_if_section { [p] }
| ol=old_param_decl_list o=old_param_decl { ol @ [o] }
| ol=old_param_decl_list p=pp_old_param_decl_list_if_section { ol @ [p] }
;

old_param_decl:
| dl=decl_specifier_seq il=old_init_decl_list SEMICOLON
    { 
      let pvec = [0; List.length dl; List.length il] in
      mknode ~pvec $startpos $endpos L.SimpleDeclaration (dl @ il)
    }
| d=DECL_MACRO { mkleaf $startpos $endpos (L.DeclarationMacro d) }
;

old_init_decl_list:
| o=old_init_decl { [o] }
| ol=old_init_decl_list COMMA o=old_init_decl { ol @ [o] }
| ol=old_init_decl_list COMMA c=cv_qualifier o=old_init_decl
    { 
      o#set_pvec (1::(List.tl o#pvec));
      o#add_children_l [c];
      ol @ [o]
    }
;

%inline
old_init_decl:
| d=ptr_declarator { mknode ~pvec:[0; 1; 0] $startpos $endpos L.InitDeclarator [d] }
| d=ptr_declarator e=EQ i=initializer_clause
    { 
      ignore e;
      let i_ = mknode $startpos(e) $endpos L.EqualInitializer [i] in
      mknode ~pvec:[0; 1; 1] $startpos $endpos L.InitDeclarator [d; i_]
    }
;

parameters_and_qualifiers:
| TY_LPAREN p=parameter_declaration_clause RPAREN cl_opt=cv_qualifier_seq_opt
    r_opt=ioption(ref_qualifier) n_opt=ioption(noexcept_specifier)
    g_opt=ioption(gnu_asm_attr) al_opt=attribute_specifier_seq_opt
    { 
      let cl = list_opt_to_list cl_opt in
      let rl = opt_to_list r_opt in
      let nl = opt_to_list n_opt in
      let gl = opt_to_list g_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length cl; List.length rl; List.length nl; List.length gl + List.length al] in
      mknode ~pvec $startpos $endpos L.ParametersAndQualifiers (p :: cl @ rl @ nl @ gl @ al)
    }
| TY_LPAREN p=parameter_declaration_clause RPAREN pp=pp_spec_if_section
    { 
      let pvec = [1; 0; 0; 1; 0] in
      mknode ~pvec $startpos $endpos L.ParametersAndQualifiers (p :: [pp])
    }
| p=params_macro { p }
;

%inline
params_macro:
| p=PARAMS_MACRO { mkleaf $startpos $endpos (L.ParametersMacro p) }
| i=IDENT_PM ml=macro_args { mknode $startpos $endpos (L.ParametersMacroInvocation i) ml }
;

pp_spec_if_section:
| p=pp_spec_if_group pl=pp_spec_elif_group* p_opt=ioption(pp_spec_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_spec_if_group:
| p=pp_ifx_d n_opt=ioption(noexcept_specifier) al_opt=attribute_specifier_seq_opt
    { 
      let n_l =
        match n_opt, al_opt with
        | Some n, Some al -> [mknode $startpos(n_opt) $endpos L.SPECS (n::al)]
        | Some n, None -> [mknode $startpos(n_opt) $endpos L.SPECS [n]]
        | None, Some al -> [mknode $startpos(al_opt) $endpos L.SPECS al]
        | None, None -> []
      in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p :: n_l)
    }
;
pp_spec_elif_group:
| p=pp_elif n_opt=ioption(noexcept_specifier) al_opt=attribute_specifier_seq_opt
    { 
      let n_l =
        match n_opt, al_opt with
        | Some n, Some al -> [mknode $startpos(n_opt) $endpos L.SPECS (n::al)]
        | Some n, None -> [mknode $startpos(n_opt) $endpos L.SPECS [n]]
        | None, Some al -> [mknode $startpos(al_opt) $endpos L.SPECS al]
        | None, None -> []
      in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p :: n_l)
    }
;
pp_spec_else_group:
| p=pp_else n_opt=ioption(noexcept_specifier) al_opt=attribute_specifier_seq_opt
    { 
      let n_l =
        match n_opt, al_opt with
        | Some n, Some al -> [mknode $startpos(n_opt) $endpos L.SPECS (n::al)]
        | Some n, None -> [mknode $startpos(n_opt) $endpos L.SPECS [n]]
        | None, Some al -> [mknode $startpos(al_opt) $endpos L.SPECS al]
        | None, None -> []
      in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p :: n_l)
    }
;

parameter_declaration_clause:
| ioption(COMMA) pl_opt=ioption(parameter_declaration_list)
    { 
      let pl = list_opt_to_list pl_opt in
      let nd = mknode $symbolstartpos $endpos (L.ParameterDeclarationClause false) pl in
      env#register_param_decl_clause nd;
      nd
    }
| c_opt=ioption(COMMA) pl_opt=ioption(parameter_declaration_list) ELLIPSIS
    { 
      let pl = list_opt_to_list pl_opt in
      let nd = mknode $symbolstartpos $endpos (L.ParameterDeclarationClause true) pl in
      env#register_param_decl_clause nd;
      if c_opt <> None then
        nd#add_prefix ", ";
      nd#add_suffix "...";
      nd
    }
| c_opt=ioption(COMMA) pl=parameter_declaration_list COMMA ELLIPSIS
    { 
      let nd = mknode $symbolstartpos $endpos (L.ParameterDeclarationClause true) pl in
      env#register_param_decl_clause nd;
      if c_opt <> None then
        nd#add_prefix ", ";
      nd#add_suffix ",...";
      nd
    }
;

parameter_declaration_list:
| p=parameter_declaration { [p] }
| p=pp_param_if_section { [p] }
| p=param_decl_macro { [p] }
| p=param_decl_macro pd=parameter_declaration { [p; pd] }
| p=pp_param_if_section pd=parameter_declaration { [p; pd] }
| pl=parameter_declaration_list COMMA p=parameter_declaration
    { 
      (Xlist.last pl)#add_suffix ",";
      pl @ [p]
    }
| pl=parameter_declaration_list COMMA DUMMY_TYPE { (Xlist.last pl)#add_suffix ","; pl }
| pl=parameter_declaration_list COMMA p=pp_param_if_section
    { 
      (Xlist.last pl)#add_suffix ",";
      pl @ [p]
    }
| pl=parameter_declaration_list COMMA p=pp_param_if_section pd=parameter_declaration
    { 
      (Xlist.last pl)#add_suffix ",";
      pl @ [p; pd]
    }
| pl=parameter_declaration_list MARKER p=pp_param_if_section { pl @ [p] }
| pl=parameter_declaration_list MARKER p=pp_param_if_section pp_param_if_section { pl @ [p] }
| pl=parameter_declaration_list MARKER p=parameter_declaration { pl @ [p] }
| pl=parameter_declaration_list p=param_decl_macro { pl @ [p] }
;

%inline
param_decl_macro:
| i=PARAM_DECL_MACRO { mkleaf $startpos(i) $endpos (L.ParamDeclMacro i) }
| i=IDENT_PDM ml=macro_args { mknode $startpos $endpos (L.ParamDeclMacroInvocation i) ml }
| i=IDENT_AGM ml=macro_args { mknode $startpos $endpos (L.ParamDeclMacroInvocation i) ml }
;

parameter_declaration:
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ [dt])
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator
    EQ i_opt=ioption(initializer_clause)
    { 
      let al = list_opt_to_list al_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length al; List.length dl; 1; List.length il] in
      begin
        match (i_opt : Ast.node option) with
        | Some i -> i#add_prefix "= "
        | None -> dt#add_suffix " ="
      end;
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ (dt :: il))
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 0; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl)
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq a=abstract_declarator
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ [a])
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq a_opt=ioption(abstract_declarator)
    EQ i_opt=ioption(initializer_clause)
    { 
      let al = list_opt_to_list al_opt in
      let al1 = opt_to_list a_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length al; List.length dl; List.length al1; List.length il] in
      begin
        match (i_opt : Ast.node option) with
        | Some i -> i#add_prefix "= "
        | None -> begin
            match (a_opt : Ast.node option) with
            | Some a -> a#add_suffix " ="
            | None -> (Xlist.last dl)#add_suffix " ="
        end
      end;
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ al1 @ il)
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator (* for macro-arg *)
    l=LPAREN el=expression_list RPAREN
    { 
      ignore l;
      let al = list_opt_to_list al_opt in
      let i = mknode $startpos(l) $endpos L.ParenthesizedInitList el in
      let pvec = [List.length al; List.length dl; 1; 1] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ [dt; i])
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator (* for macro-arg *)
    vl=virt_specifier_seq
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ (dt :: vl))
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator (* for macro-arg *)
    vl_opt=virt_specifier_seq_opt b=function_body
    { 
      let al = list_opt_to_list al_opt in
      let vl = list_opt_to_list vl_opt in
      let pvec = [List.length al; List.length dl; 1; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ (dt :: vl @ [b]))
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq dt=declarator (* for macro-arg *)
    vl_opt=virt_specifier_seq_opt p=pure_specifier
    { 
      let al = list_opt_to_list al_opt in
      let vl = list_opt_to_list vl_opt in
      let pvec = [List.length al; List.length dl; 1; 0] in
      mknode ~pvec $symbolstartpos $endpos L.ParameterDeclaration (al @ dl @ (dt :: vl @ [p]))
    }
;

pp_param_if_section:
| ifg=pp_param_if_group
      elifg=list(pp_param_elif_group) elseg_opt=ioption(pp_param_else_group) pe=pp_endif
    { 
      let el = opt_to_list elseg_opt in
      let pvec = [1; List.length elifg; List.length el; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group ifg pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (ifg :: elifg @ el @ [pe])
    }
;
pp_param_if_group:
| pi=pp_ifx_e { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [pi] }
| pi=pp_ifx_e ioption(COMMA) pl=parameter_declaration_list ioption(COMMA)
    { mknode ~pvec:[1; List.length pl] $startpos $endpos (pp_if_group()) (pi::pl) }
| pi=pp_ifx_e cl=pp_control_line+ pl=parameter_declaration_list
    { mknode ~pvec:[1; List.length cl + List.length pl] $startpos $endpos (pp_if_group()) (pi::cl@pl) }
(*| pi=pp_ifx_e p=pp_param_if_section { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; p] }*)
;
pp_param_elif_group:
| pe=pp_elif { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group pe) [pe] }
| pe=pp_elif ioption(COMMA) pl=parameter_declaration_list ioption(COMMA)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pe) (pe::pl) }
| pe=pp_elif cl=pp_control_line+ pl=parameter_declaration_list
    { mknode ~pvec:[1; List.length cl + List.length pl] $startpos $endpos (_pp_elif_group pe) (pe::cl@pl) }
(*| pe=pp_elif p=pp_param_if_section { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pe) [pe; p] }*)
;
pp_param_else_group:
| pe=pp_else { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group pe) [pe] }
| pe=pp_else ioption(COMMA) pl=parameter_declaration_list ioption(COMMA)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pe) (pe::pl) }
| pe=pp_else cl=pp_control_line+ pl=parameter_declaration_list
    { mknode ~pvec:[1; List.length cl + List.length pl] $startpos $endpos (_pp_else_group pe) (pe::cl@pl) }
(*| pe=pp_else p=pp_param_if_section { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pe) [pe; p] }*)
;

braced_init_list:
| INI_LBRACE il=initializer_list (*ioption(COMMA)*) RBRACE
    { mknode $startpos $endpos L.BracedInitList il }
| INI_LBRACE dl=designated_initializer_list ioption(COMMA) RBRACE
    { mknode $startpos $endpos L.BracedInitList dl }
| INI_LBRACE RBRACE { mkleaf $startpos $endpos L.BracedInitList }
;

%inline
designated_initializer_list:
| dl=_designated_initializer_list { List.rev dl }
;
_designated_initializer_list:
| d=designated_initializer_clause { [d] }
| p=pp_dinit_if_section { [p] }
| p=pp_dinit_if_section d=designated_initializer_clause { [d; p] }
| dl=_designated_initializer_list p=pp_control_line { p::dl }
| dl=_designated_initializer_list p=pp_control_line d=designated_initializer_clause { d::p::dl }
| dl=_designated_initializer_list COMMA d=designated_initializer_clause { d::dl }
| dl=_designated_initializer_list COMMA p=pp_control_line { p::dl }
| dl=_designated_initializer_list COMMA p=pp_control_line i=initializer_clause { i::p::dl }
| dl=_designated_initializer_list COMMA i=initializer_clause { i::dl }
| dl=_designated_initializer_list COMMA p=pp_dinit_if_section { p::dl }
| dl=_designated_initializer_list COMMA pl=_pp_dinit_if_section_list d=designated_initializer_clause { d::pl@dl }
;

_pp_dinit_if_section_list:
| p=pp_dinit_if_section { [p] }
| pl=_pp_dinit_if_section_list p=pp_dinit_if_section { p::pl }
;

pp_dinit_if_section:
| p=pp_dinit_if_group pl=pp_dinit_elif_group* p_opt=ioption(pp_dinit_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_dinit_if_group:
| p=pp_ifx_i dl=designated_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (pp_if_group()) (p::dl) }
;
pp_dinit_elif_group:
| p=pp_elif dl=designated_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_elif_group p) (p::dl) }
| p=pp_elif il=initializer_list
    { mknode ~pvec:[1; List.length il] $startpos $endpos (_pp_elif_group p) (p::il) }
;
pp_dinit_else_group:
| p=pp_else dl=designated_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_else_group p) (p::dl) }
| p=pp_else il=initializer_list
    { mknode ~pvec:[1; List.length il] $startpos $endpos (_pp_else_group p) (p::il) }
;

designated_initializer_clause:
| dl=designator+ b=brace_or_equal_initializer
    { 
      let pvec = [List.length dl; 1] in
      mknode ~pvec $startpos $endpos L.DesignatedInitializerClause (dl @ [b])
    }
| i=IDENT_V c=COLON e=initializer_clause
    { 
      ignore c;
      let d = mkleaf $startpos $endpos(c) (L.DesignatorFieldOld i) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos L.DesignatedInitializerClause [d; e]
    }
;

designator:
| DOT i=IDENT_V { mkleaf $startpos $endpos (L.DesignatorField i) }
| LBRACKET c=constant_expression RBRACKET { mknode $startpos $endpos L.DesignatorIndex [c] }
;

trailing_return_type:
| MINUS_GT t=type_id { mknode $startpos $endpos L.TrailingReturnType [t] }
;

ptr_operator:
| PTR_STAR al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[0; List.length al; 0] $startpos $endpos L.PtrOperatorStar al
    } %prec PREC
| PTR_STAR al_opt=attribute_specifier_seq_opt cl=cv_qualifier_seq
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [0; List.length al; List.length cl] in
      mknode ~pvec $startpos $endpos L.PtrOperatorStar (al @ cl)
    }
| n=nested_name_specifier PTR_STAR al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length al; 0] in
      mknode ~pvec $startpos $endpos L.PtrOperatorStar (n :: al)
    } %prec PREC
| n=nested_name_specifier PTR_STAR al_opt=attribute_specifier_seq_opt cl=cv_qualifier_seq
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length al; List.length cl] in
      mknode ~pvec $startpos $endpos L.PtrOperatorStar (n :: al @ cl)
    }
| PTR_AMP al_opt=attribute_specifier_seq_opt
    { mknode $startpos $endpos L.PtrOperatorAmp (list_opt_to_list al_opt) }
| PTR_AMP_AMP al_opt=attribute_specifier_seq_opt
    { mknode $startpos $endpos L.PtrOperatorAmpAmp (list_opt_to_list al_opt) }
| PTR_HAT { mkleaf $startpos $endpos L.PtrOperatorHat } (* Apple's block extension *)
| i=PTR_MACRO { mkleaf $startpos $endpos (L.PtrMacro i) }
;

%inline
cv_qualifier_seq_opt:
| cl_opt=ioption(cv_qualifier_seq) { cl_opt }
;

cv_qualifier_seq:
| c=cv_qualifier { [c] } %prec PREC
| c=cv_qualifier cl=cv_qualifier_seq { c::cl }
;

cv_qualifier:
| CONST    { mkleaf $startpos $endpos L.Const }
| VOLATILE { mkleaf $startpos $endpos L.Volatile }
| e=extra_qualifier { e }
;

%inline
extra_qualifier:
| r=RESTRICT { mkleaf $startpos $endpos (L.Restrict r) }
| m=ms_stdcall { m }
| m=ms_cdecl { m }
| c=cc_macro { c }
| c=CV_MACRO { mkleaf $startpos $endpos (L.CvMacro c) }
| i=IDENT_CM ml=macro_args { mknode $startpos $endpos (L.CvMacroInvocation i) ml }
;

ref_qualifier:
| AMP     { mkleaf $startpos $endpos L.RefQualifierAmp }
| AMP_AMP { mkleaf $startpos $endpos L.RefQualifierAmpAmp }
;

declarator_id:
|           i=id_expression { i }
| ELLIPSIS_ i=id_expression
    { 
      let i_ = mknode $startpos $endpos L.PackExpansion [i] in
      i_#add_prefix "...";
      i_
    }
;

nested_name_specifier:
| HEAD_COLON_COLON { mkleaf ~pvec:[0; 0] $startpos $endpos L.NestedNameSpecifierHead }
| i=IDENT COLON_COLON
    { 
      let i_ = mkleaf $startpos $endpos(i) (L.Identifier i) in
      let uqn = Ast.encode_ident i in
      mknode ~pvec:[0; 1] $startpos $endpos (L.NestedNameSpecifierIdent uqn) [i_]
    }
| i=id_macro_call COLON_COLON
    { 
      let uqn = Ast.uqn_of_ident_macro_invocation i in
      mknode ~pvec:[0; 1] $startpos $endpos (L.NestedNameSpecifierIdent uqn) [i]
    }
| n=nested_name_specifier i=IDENT COLON_COLON
    { 
      let i_ = mkleaf $startpos(i) $endpos(i) (L.Identifier i) in
      let uqn = Ast.encode_ident i in
      mknode ~pvec:[1; 1] $startpos $endpos (L.NestedNameSpecifierIdent uqn) [n; i_]
    }
| s=simple_template_id COLON_COLON
    { 
      let uqn = Ast.uqn_of_simple_template_id s in
      mknode ~pvec:[0; 1] $startpos $endpos (L.NestedNameSpecifierTempl uqn) [s]
    }
| n=nested_name_specifier ioption(TEMPLATE) s=simple_template_id COLON_COLON
    { 
      let uqn = Ast.uqn_of_simple_template_id s in
      mknode ~pvec:[1; 1] $startpos $endpos (L.NestedNameSpecifierTempl uqn) [n; s]
    }
| d=decltype_specifier COLON_COLON
    { mknode $startpos $endpos L.NestedNameSpecifierDeclty [d] }
;

(*nested_name_specifier:
| COLON_COLON { }
| type_name COLON_COLON { }
| namespace_name COLON_COLON { }(*contained in type_name COLON_COLON*)
| decltype_specifier COLON_COLON { }
| nested_name_specifier IDENT COLON_COLON { }
| nested_name_specifier ioption(TEMPLATE) simple_template_id COLON_COLON { }
;*)

(*namespace_name:
| IDENT { }
| namespace_alias:
;
namespace_alias:
| IDENT { }
;*)

declaration_seq:
| d=declaration { [d] }
| a=mem_access_spec { [a] }
| dl=declaration_seq d=declaration
    { 
      let last = Xlist.last dl in
      begin
        match last#label with
        | L.PpDefine i -> begin
            match d#label with
            | L.STMTS -> begin
                warning $startpos(d) $endpos(d) "malformed macro definition: %s" i;
                env#register_malformed_macro i
            end
            | _ -> ()
        end
        | _ -> ()
      end;
      dl @ [d]
    }
| dl=declaration_seq a=mem_access_spec { dl @ [a] }
;

pp_decl_if_section:
(*| p=pp_decl_if_group pl=list(pp_decl_elif_group) p_opt=ioption(pp_decl_else_group) pe=pp_endif
    l_opt=ioption(BRACE_LEVEL)
    { 
      let pl1 = opt_to_list p_opt in
      let blv =
        match l_opt with
        | Some lv -> lv
        | None -> 0
      in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      mknode ~pvec $startpos $endpos (L.PpIfSection blv) (p :: pl @ pl1 @ [pe])
    }*)
| p=pp_decl_if_group pl=list(pp_decl_elif_group) p_opt=ioption(pp_decl_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
| p=pp_decl_if_group pl=list(pp_decl_elif_group) p_opt=ioption(pp_decl_else_group) pe=pp_endif
    blv=BRACE_LEVEL
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(blv, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_decl_if_group:
| p=pp_ifx { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [p] }
| p=pp_ifx dl=declaration_seq
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (pp_if_group()) (p::dl) }
| p=pp_ifx e=EXTERN s=STR_LITERAL
    { 
      ignore e;
      let e_ = mkleaf $startpos(e) $endpos (L.LinkageSpecification s) in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e_]
    }
;
pp_decl_elif_group:
| p=pp_elif { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group p) [p] }
| p=pp_elif dl=declaration_seq
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_elif_group p) (p::dl) }
;
pp_decl_else_group:
| p=pp_else { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group p) [p] }
| p=pp_else dl=declaration_seq
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_else_group p) (p::dl) }
;

pp_decl_if_section_broken:
| p=pp_decl_if_group_broken
    pl=list(pp_decl_elif_group_broken)
    p_opt=ioption(pp_decl_else_group_broken)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_decl_if_group_broken:
| p=pp_ifx d=simple_declaration_broken
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; d] }
| p=pp_ifx dl=declaration_seq t=template_head f=func_head
    { 
      let n_ = mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; f] in
      mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (pp_if_group()) (p::dl@[n_])
    }
| p=pp_ifx dl=declaration_seq f=func_head
    { mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (pp_if_group()) (p::dl@[f]) }
| p=pp_ifx dl=declaration_seq SECTION_MARKER
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (pp_if_group()) (p::dl) }
| p=pp_ifx f=func_head c_opt=ioption(ctor_initializer)
    { 
      let n_ =
        match c_opt with
        | Some c -> begin
            let b_ = mknode ~pvec:[1; 0] $startpos(c_opt) $endpos L.FunctionBody [c] in
            f#relab L.FunctionDefinition;
            f#add_children_r [b_];
            f#set_pvec (f#pvec @ [1]);
            reloc $startpos(f) $endpos f
        end
        | _ -> f
      in
      mknode $startpos $endpos (pp_if_group()) [p; n_]
    }
| p=pp_ifx dl_opt=ioption(declaration_seq) t=template_head c=class_head
    { 
      let dl = list_opt_to_list dl_opt in
      let n_ = mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; c] in
      mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (pp_if_group()) (p::dl@[n_])
    }
| p=pp_ifx i=IDENT_V ml=macro_args LBRACE
    { 
      let pvec = [0; List.length ml; 0; 0; 0] in
      let n_ = mknode ~pvec $startpos(i) $endpos (L.DeclarationMacroInvocation i) ml in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; n_]
    }
;
pp_decl_elif_group_broken:
| p=pp_elif d=simple_declaration_broken
    { 
      env#pstat#close_brace();
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; d]
    }
| p=pp_elif dl=declaration_seq t=template_head f=func_head
    { 
      let n_ = mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; f] in
      mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (_pp_elif_group p) (p::dl@[n_])
    }
| p=pp_elif dl=declaration_seq f=func_head
    { mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (_pp_elif_group p) (p::dl@[f]) }
| p=pp_elif dl=declaration_seq SECTION_MARKER
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_elif_group p) (p::dl) }
| p=pp_elif f=func_head c_opt=ioption(ctor_initializer)
    { 
      let n_ =
        match c_opt with
        | Some c -> begin
            let b_ = mknode ~pvec:[1; 0] $startpos(c_opt) $endpos L.FunctionBody [c] in
            f#relab L.FunctionDefinition;
            f#add_children_r [b_];
            f#set_pvec (f#pvec @ [1]);
            reloc $startpos(f) $endpos f
        end
        | _ -> f
      in
      mknode $startpos $endpos (_pp_elif_group p) [p; n_]
    }
| p=pp_elif dl_opt=ioption(declaration_seq) t=template_head c=class_head
    { 
      env#stack#exit_class();
      env#stack#exit_template();
      let dl = list_opt_to_list dl_opt in
      let n_ = mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; c] in
      mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (_pp_elif_group p) (p::dl@[n_])
    }
| p=pp_elif i=IDENT_V ml=macro_args LBRACE
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      let pvec = [0; List.length ml; 0; 0; 0] in
      let n_ = mknode ~pvec $startpos(i) $endpos (L.DeclarationMacroInvocation i) ml in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; n_]
    }
;
pp_decl_else_group_broken:
| p=pp_else d=simple_declaration_broken
    { 
      env#pstat#close_brace();
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; d]
    }
| p=pp_else dl=declaration_seq t=template_head f=func_head
    { 
      let n_ = mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; f] in
      mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (_pp_else_group p) (p::dl@[n_])
    }
| p=pp_else dl=declaration_seq f=func_head
    { mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (_pp_else_group p) (p::dl@[f]) }
| p=pp_else dl=declaration_seq SECTION_MARKER
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_else_group p) (p::dl) }
| p=pp_else f=func_head c_opt=ioption(ctor_initializer)
    { 
      let n_ =
        match c_opt with
        | Some c -> begin
            let b_ = mknode ~pvec:[1; 0] $startpos(c_opt) $endpos L.FunctionBody [c] in
            f#relab L.FunctionDefinition;
            f#add_children_r [b_];
            f#set_pvec (f#pvec @ [1]);
            reloc $startpos(f) $endpos f
        end
        | _ -> f
      in
      mknode $startpos $endpos (_pp_else_group p) [p; n_]
    }
| p=pp_else dl_opt=ioption(declaration_seq) t=template_head c=class_head
    { 
      env#stack#exit_class();
      env#stack#exit_template();
      let dl = list_opt_to_list dl_opt in
      let n_ = mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; c] in
      mknode ~pvec:[1; (List.length dl)+1] $startpos $endpos (_pp_else_group p) (p::dl@[n_])
    }
| p=pp_else i=IDENT_V ml=macro_args LBRACE
    { 
      env#stack#exit_block();
      env#pstat#close_brace();
      let pvec = [0; List.length ml; 0; 0; 0] in
      let n_ = mknode ~pvec $startpos(i) $endpos (L.DeclarationMacroInvocation i) ml in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; n_]
    }
;

asm_block:
| BEGIN_ASM tl=list(gnu_asm_token_) END_ASM { mkleaf $startpos(tl) $endpos(tl) (make_gnu_asm_lab "" tl) }
| BEGIN_ASM gl=gnu_asm_frag_seq END_ASM { mknode $startpos $endpos (L.GnuAsmBlockFragmented "") gl }
| BEGIN_ASM g=gnu_asm_fragment gl=gnu_asm_frag_seq END_ASM
    { mknode $startpos $endpos (L.GnuAsmBlockFragmented "") (g::gl) }
;

%inline
gnu_asm_token_:
| g=gnu_asm_token { g }
| b=SEMICOLON { mktok $startpos $endpos (T.SEMICOLON b) }
;

module_import_declaration:
| IMPORT s=STR_LITERAL SEMICOLON { mkleaf $startpos $endpos (L.ImportDeclaration s) }
;

declaration:
| b=block_declaration { b }
| n=nodeclspec_function_definition { n }
| f=function_definition { f }
| t=template_declaration { t }
| d=deduction_guide { d }
| e=explicit_instantiation { e }
| e=explicit_specialization { e }
| l=linkage_specification { l }
| n=namespace_definition { n }
| e=empty_declaration { e }
| a=attribute_declaration { a }
| m=module_import_declaration { m }
| g=gnu_asm { g }
| a=asm_block { a }
| m=ms_pragma { m }
| p=pp_control_line { p }
| p=pp_decl_if_section { p }
| p=pp_decl_if_section_broken dl=designated_initializer_list ioption(COMMA) RBRACE mid_init SEMICOLON
    { 
      p#add_children_r dl;
      p#set_pvec (p#pvec @ [List.length dl]);
      reloc $startpos $endpos p
    }
| p=pp_decl_if_section_broken dl=initializer_list RBRACE mid_init SEMICOLON
    { 
      p#add_children_r dl;
      p#set_pvec (p#pvec @ [List.length dl]);
      reloc $startpos $endpos p
    }
| p=pp_decl_if_section_broken b=function_body
    { 
      p#add_children_r [b];
      p#set_pvec (p#pvec @ [1]);
      reloc $startpos $endpos p
    }
| p=pp_decl_if_section_broken SEMICOLON { p }

| p=pp_decl_if_section_broken ol=old_param_decl_list b=function_body
    { 
      p#add_children_r (ol@[b]);
      p#set_pvec (p#pvec @ [List.length ol; 1]);
      reloc $startpos $endpos p
    }
| p=pp_decl_if_section_broken MARKER sl_opt=statement_seq_opt RBRACE
    { 
      env#stack#exit_block();
      let b = mknode $endpos(p) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      p#add_children_r [b];
      p#set_pvec (p#pvec @ [1]);
      reloc $startpos $endpos p
    }
| p=pp_decl_if_section_broken CLASS_LBRACE m_opt=ioption(member_specification) RBRACE
    mid_templ_decl SEMICOLON
    { 
      let ml = opt_to_list m_opt in
      p#set_pvec (p#pvec @ [List.length ml]);
      p#add_children_r ml;
      reloc $startpos $endpos p
    }
| dl_opt=decl_specifier_seq_opt p=_pp_func_head_if_section ol_opt=ioption(old_param_decl_list) f=function_body
    { 
      let dl = list_opt_to_list dl_opt in
      let ol = list_opt_to_list ol_opt in
      p#add_children_l dl;
      p#add_children_r ol;
      p#add_children_r [f];
      p#set_pvec ([List.length dl] @ p#pvec @ [List.length ol; 1]);
      reloc $startpos $endpos p
    }
| dl_opt=decl_specifier_seq_opt p=_pp_func_head_if_section ol_opt=ioption(old_param_decl_list) f=pp_func_body_if_section
    { 
      let dl = list_opt_to_list dl_opt in
      let ol = list_opt_to_list ol_opt in
      p#add_children_l dl;
      p#add_children_r ol;
      p#add_children_r [f];
      p#set_pvec ([List.length dl] @ p#pvec @ [List.length ol; 1]);
      reloc $startpos $endpos p
    }
|  p=pp_func_head_if_section_broken sl_opt=statement_seq_opt RBRACE
    { 
      let sl = list_opt_to_list sl_opt in
      p#add_children_r sl;
      p#set_pvec (p#pvec @ [List.length sl]);
      reloc $startpos $endpos p
    }
|  dl=decl_specifier_seq SECTION_MARKER p=pp_func_head_if_section_broken sl_opt=statement_seq_opt RBRACE
    { 
      let sl = list_opt_to_list sl_opt in
      p#add_children_l dl;
      p#add_children_r sl;
      p#set_pvec ([List.length dl] @ p#pvec @ [List.length sl]);
      reloc $startpos $endpos p
    }
(*| dl=decl_specifier_seq i=IDENT_V p=pp_dtor_if_section
    { 
      p#relab (L.PpIfSectionFuncDef i);
      p#add_children_l dl;
      p#set_pvec ((List.length dl)::p#pvec);
      reloc $startpos $endpos p
    }*)
| d=decl_macro_call_ { d }
| d=decl_OR_stmt_macro_call_ { d }
| al=class_attr_spec_seq d=DECL_MACRO { mknode $startpos $endpos (L.DeclarationMacro d) al }
| al=class_attr_spec_seq d=DECL_MACRO s=compound_statement { mknode $startpos $endpos (L.DeclarationMacro d) (al @ [s]) }
| al=class_attr_spec_seq d=DECL_MACRO i=braced_init_list { mknode $startpos $endpos (L.DeclarationMacro d) (al @ [i]) }
| ODD_LBRACE { mkleaf $startpos $endpos L.OpeningBrace }
| ODD_RBRACE { mkleaf $startpos $endpos L.ClosingBrace }
| t=top_stmts { t }
| BEGIN_ETORS el=enumerator_list END_ETORS { mknode $startpos(el) $endpos(el) L.ETORS el }
| al=class_attr_spec_seq h=block_head_macro sl_opt=statement_seq_opt e=block_end_macro
    { 
      let sl = list_opt_to_list sl_opt in
      mknode $startpos $endpos L.DeclStmtBlock (al @ h :: sl @ [e])
    }
| ASM_SHADER tl=token_seq NEWLINE { mkleaf $startpos $endpos (L.AsmShader (Token.seq_to_repr tl)) }
| i=IDENT_EM ml=macro_args EQ b=braced_init_list
    { 
      let pvec = [0; List.length ml; 0; 0; 1] in
      b#add_prefix "= ";
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) (ml @ [b])
    }

| i=objc_class_interface    { i }
| c=objc_category_interface { c }
| c=objc_class_decl_list    { c }
| p=objc_protocol_decl      { p }
| p=objc_protocol_decl_list { p }
;

%inline
top_stmts:
| BEGIN_STMTS sl=statement_seq            END_STMTS { mknode $startpos(sl) $endpos(sl) L.STMTS sl }
| BEGIN_STMTS sl=statement_seq o=odd_stmt END_STMTS { mknode $startpos(sl) $endpos(o) L.STMTS (sl@[o]) }
;

mid_init:
| { env#exit_braced_init() }
;

objc_protocol_decl_list:
| OBJC_PROTOCOL pl=objc_protocol_name_list SEMICOLON { mknode $startpos $endpos L.ObjcProtocolDeclarationList pl }
;
objc_protocol_decl:
| al_opt=attribute_specifier_seq_opt
    OBJC_PROTOCOL i=IDENT p_opt=ioption(objc_protocol_ref_list)
    dl_opt=ioption(objc_idecl_seq) ql_opt=ioption(objc_qualified_idecl_seq)
    OBJC_END
    { 
      let al = list_opt_to_list al_opt in
      let pl = opt_to_list p_opt in
      let dl = list_opt_to_list dl_opt in
      let ql = list_opt_to_list ql_opt in
      let pvec = [List.length al; List.length pl; List.length dl; List.length ql] in
      mknode ~pvec $symbolstartpos $endpos (L.ObjcProtocolDeclaration i) (al @ pl @ dl @ ql)
    }
;
objc_qualified_idecl_seq:
| d=objc_qualified_interface_decl { [d] }
| dl=objc_qualified_idecl_seq d=objc_qualified_interface_decl { dl @ [d] }
;
objc_qualified_interface_decl:
| OBJC_OPTIONAL dl=objc_idecl_seq { mknode $startpos $endpos L.ObjcProtocolInterfaceDeclarationOptional dl }
| OBJC_REQUIRED dl=objc_idecl_seq { mknode $startpos $endpos L.ObjcProtocolInterfaceDeclarationRequired dl }
;
objc_class_decl_list:
| OBJC_CLASS cl=clist(objc_class_name) SEMICOLON { mknode $startpos $endpos L.ObjcClassDeclarationList cl }
;
objc_class_name:
| i=objc_ident { mkleaf $startpos $endpos (L.ObjcClassName i) }
;
objc_class_interface:
| al_opt=attribute_specifier_seq_opt
    OBJC_INTERFACE i=IDENT s_opt=ioption(objc_superclass) p_opt=ioption(objc_protocol_ref_list)
    v_opt=ioption(objc_instance_vars) dl_opt=ioption(objc_idecl_seq)
    OBJC_END
    { 
      let al = list_opt_to_list al_opt in
      let sl = opt_to_list s_opt in
      let pl = opt_to_list p_opt in
      let vl = opt_to_list v_opt in
      let dl = list_opt_to_list dl_opt in
      let pvec = [List.length al; List.length sl; List.length pl; List.length vl; List.length dl] in
      mknode ~pvec $symbolstartpos $endpos (L.ObjcClassInterface i) (al @ sl @ pl @ vl @ dl)
    }
objc_category_interface:
| al_opt=attribute_specifier_seq_opt
    OBJC_INTERFACE i=IDENT TY_LPAREN i_opt=ioption(IDENT) RPAREN p_opt=ioption(objc_protocol_ref_list)
    mid_objc_cat_iface
    dl_opt=ioption(objc_idecl_seq)
  OBJC_END
    { 
      let al = list_opt_to_list al_opt in
      let cat = string_opt_to_string i_opt in
      let pl = opt_to_list p_opt in
      let dl = list_opt_to_list dl_opt in
      let pvec = [List.length al; List.length pl; List.length dl] in
      mknode ~pvec $symbolstartpos $endpos (L.ObjcCategoryInterface(i, cat)) (al @ pl @ dl)
    }
;
mid_objc_cat_iface:
| { env#clear_objc_cat_flag() }
;
objc_superclass:
| COLON i=IDENT { mkleaf $startpos $endpos (L.ObjcSuperclass i) }
;
objc_protocol_ref_list:
| TEMPL_LT il=objc_protocol_name_list objc_templ_gt { mknode $startpos $endpos L.ObjcProtocolReferenceList il }
;
objc_protocol_name_list:
| p=objc_protocol_name { [p] }
| p=pp_objc_proto_name_if_section pn=objc_protocol_name { [p; pn] }
| pl=objc_protocol_name_list COMMA p=objc_protocol_name { pl @ [p] }
;
pp_objc_proto_name_if_section:
| pi=pp_ifx ioption(COMMA) pl=objc_protocol_name_list c_opt=ioption(COMMA)
    elifg=list(pp_objc_proto_name_elif_group) elseg_opt=ioption(pp_objc_proto_name_else_group) pe=pp_endif
    { 
      ignore c_opt;
      let el = opt_to_list elseg_opt in
      let pp_if_cond = get_pp_if_cond pe in
      pi#relab (L.PpIf pp_if_cond);
      let ifg = mknode $startpos $endpos(c_opt) (L.PpIfGroup pp_if_cond) (pi::pl) in
      let pvec = [1; List.length elifg; List.length el; 1] in
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (ifg :: elifg @ el @ [pe])
    }
;
pp_objc_proto_name_elif_group:
| pe=pp_elif ioption(COMMA) pl=objc_protocol_name_list ioption(COMMA)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pe) (pe::pl) }
;
pp_objc_proto_name_else_group:
| pe=pp_else ioption(COMMA) pl=objc_protocol_name_list ioption(COMMA)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pe) (pe::pl) }
;
%inline
objc_templ_gt:
| TY_TEMPL_GT { }
| TEMPL_GT    { }
;
objc_protocol_name:
| i=IDENT { mkleaf $startpos $endpos (L.ObjcProtocolName i) }
;
objc_instance_vars:
| LBRACE RBRACE { mknode $startpos $endpos L.ObjcInstanceVariables [] }
| LBRACE dl=objc_instance_var_decl+ RBRACE { mknode $startpos $endpos L.ObjcInstanceVariables dl }
;
objc_instance_var_decl:
| v=objc_visibility_spec { v }
| d=objc_struct_decl { d }
| v=objc_instance_vars { v }
| p=pp_objc_ivar_decl_if_section { p }
;
pp_objc_ivar_decl_if_section:
| p=pp_objc_ivar_decl_if_group
    pl=list(pp_objc_ivar_decl_elif_group) p_opt=ioption(pp_objc_ivar_decl_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_objc_ivar_decl_if_group:
| p=pp_ifx { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [p] }
| p=pp_ifx dl=objc_instance_var_decl+
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (pp_if_group()) (p::dl) }
;
pp_objc_ivar_decl_elif_group:
| p=pp_elif { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group p) [p] }
| p=pp_elif dl=objc_instance_var_decl+
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_elif_group p) (p::dl) }
;
pp_objc_ivar_decl_else_group:
| p=pp_else { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group p) [p] }
| p=pp_else dl=objc_instance_var_decl+
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_else_group p) (p::dl) }
;
objc_visibility_spec:
| OBJC_PRIVATE   { mkleaf $startpos $endpos L.ObjcPrivate }
| OBJC_PUBLIC    { mkleaf $startpos $endpos L.ObjcPublic }
| OBJC_PACKAGE   { mkleaf $startpos $endpos L.ObjcPackage }
| OBJC_PROTECTED { mkleaf $startpos $endpos L.ObjcProtected }
;
objc_struct_decl:
| sl=decl_specifier_seq dl=clist(objc_struct_dtor) SEMICOLON
    { 
      let pvec = [List.length sl; List.length dl] in
      mknode ~pvec $startpos $endpos L.ObjcStructDeclaration (sl @ dl)
    }
;
objc_struct_dtor:
| d=declarator { d }
| d_opt=ioption(declarator) COLON e=constant_expression
    { 
      let dl = opt_to_list d_opt in
      let pvec = [List.length dl; 1] in
      mknode ~pvec $symbolstartpos $endpos L.ObjcStructDeclarator (dl @ [e])
    }
;
objc_interface_decl:
| d=block_declaration  { d }
| p=objc_property_decl { p }
| m=objc_method_decl   { m }
| p=pp_control_line          { p }
| p=pp_objc_idecl_if_section { p }
;
objc_idecl_seq:
| d=objc_interface_decl { [d] }
| dl=objc_idecl_seq d=objc_interface_decl { dl @ [d] }
;
pp_objc_idecl_if_section:
| p=pp_objc_idecl_if_group
    pl=list(pp_objc_idecl_elif_group) p_opt=ioption(pp_objc_idecl_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_objc_idecl_if_group:
| p=pp_ifx { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [p] }
| p=pp_ifx dl=objc_idecl_seq
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (pp_if_group()) (p::dl) }
;
pp_objc_idecl_elif_group:
| p=pp_elif { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group p) [p] }
| p=pp_elif dl=objc_idecl_seq
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_elif_group p) (p::dl) }
;
pp_objc_idecl_else_group:
| p=pp_else { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group p) [p] }
| p=pp_else dl=objc_idecl_seq
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_else_group p) (p::dl) }
;
objc_property_decl:
| OBJC_PROPERTY p_opt=ioption(objc_property_attrs_decl) s=objc_struct_decl
    { 
      let pl = opt_to_list p_opt in
      let pvec = [List.length pl; 1] in
      mknode ~pvec $startpos $endpos L.ObjcPropertyDeclaration (pl @ [s])
    }
;
objc_property_attrs_decl:
| TY_LPAREN pl=clist(objc_property_attr) RPAREN { mknode $startpos $endpos L.ObjcPropertyAttributesDeclaration pl }
;
objc_property_attr:
| i=IDENT { mkleaf $startpos $endpos (L.ObjcPropertyAttribute i) }
| i=IDENT EQ j=objc_identifier { mknode $startpos $endpos (L.ObjcPropertyAttribute i) [j] }
;
objc_method_decl:
| OBJC_PLUS  t_opt=ioption(objc_method_type) m=objc_method_selector SEMICOLON
    { 
      let tl = opt_to_list t_opt in
      let pvec = [List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.ObjcClassMethodDeclaration (tl @ [m])
    }
| OBJC_MINUS t_opt=ioption(objc_method_type) m=objc_method_selector SEMICOLON
    { 
      let tl = opt_to_list t_opt in
      let pvec = [List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.ObjcInstanceMethodDeclaration (tl @ [m])
    }
| OBJC_PLUS i=IDENT_V ml=macro_args SEMICOLON
    { 
      let m = mknode $startpos(i) $endpos(ml) (L.ObjcMethodMacroInvocation i) ml in
      mknode ~pvec:[0; 1] $startpos $endpos L.ObjcClassMethodDeclaration [m]
    }
| OBJC_MINUS i=IDENT_V ml=macro_args SEMICOLON
    { 
      let m = mknode $startpos(i) $endpos(ml) (L.ObjcMethodMacroInvocation i) ml in
      mknode ~pvec:[0; 1] $startpos $endpos L.ObjcInstanceMethodDeclaration [m]
    }
;
objc_method_type:
| TY_LPAREN sl=decl_specifier_seq d_opt=ioption(abstract_declarator) RPAREN
    { 
      let dl = opt_to_list d_opt in
      let pvec = [List.length sl; List.length dl] in
      mknode ~pvec $startpos $endpos L.ObjcMethodType (sl @ dl)
    }
;
objc_method_selector:
| s=objc_selector al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length al] in
      mknode ~pvec $startpos $endpos L.ObjcMethodSelector (s :: al)
    }
| s=objc_keyword_selector al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length al] in
      mknode ~pvec $startpos $endpos L.ObjcMethodSelector (s :: al)
    }
| s=objc_keyword_selector COMMA ELLIPSIS al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length al] in
      mknode ~pvec $startpos $endpos L.ObjcMethodSelectorPack (s :: al)
    }
;
objc_selector:
| i=objc_ident { mkleaf $startpos $endpos (L.ObjcSelector i) }
;
objc_keyword_selector:
| kl=objc_keyword_dtor+ { mknode $startpos $endpos L.ObjcKeywordSelector kl }
;
objc_keyword_dtor:
| s_opt=ioption(objc_selector) COLON t_opt=ioption(objc_method_type) i=objc_ident
    { 
      let sl = opt_to_list s_opt in
      let tl = opt_to_list t_opt in
      let pvec = [List.length sl; List.length tl] in
      mknode ~pvec $symbolstartpos $endpos (L.ObjcKeywordDeclarator i) (sl @ tl)
    }
;
%inline
objc_ident:
| i=IDENT   { i }
| i=IDENT_V { i }
;
objc_identifier:
| i=objc_ident { mkleaf $startpos $endpos (L.Identifier i) }
;

block_head_macro:
| b=BLOCK_HEAD_MACRO { mknode $startpos $endpos (L.BlockHeadMacro b) [] }
| i=IDENT_BHM ml=macro_args { mknode $startpos $endpos (L.BlockHeadMacro i) ml }
;

block_end_macro:
| b=BLOCK_END_MACRO { mknode $startpos $endpos (L.BlockEndMacro b) [] }
| i=IDENT_BEM ml=macro_args { mknode $startpos $endpos (L.BlockEndMacro i) ml }
;

decl_OR_stmt_macro_call:
| al_opt=attribute_specifier_seq_opt dl_opt=decl_specifier_seq_opt i=IDENT_DSM ml=macro_args
    { 
      let al = list_opt_to_list al_opt in
      let dl = list_opt_to_list dl_opt in
      let pvec = [List.length al; List.length dl; List.length ml; 0; 0; 0] in
      mknode ~pvec $symbolstartpos $endpos (L.DeclarationMacroInvocation i) (al @ dl @ ml)
    }
;

%inline
subscript:
| LBRACKET RBRACKET { [] }
| LBRACKET e=constant_expression RBRACKET { [e] }
;
%inline
subscripts:
| el=subscript { el }
| el0=subscript el1=subscript { el0 @ el1 }
| el0=subscript el1=subscript el2=subscript { el0 @ el1 @ el2 }
;
%inline
decl_OR_stmt_macro_call_:
| d=decl_OR_stmt_macro_call { d }
| d=decl_OR_stmt_macro_call MARKER b=compound_statement
    { 
      d#add_children_r [b];
      let pvec =
        match List.rev d#pvec with
        | _::_::_::tl -> List.rev (0::0::1::tl)
        | _ -> assert false
      in
      d#set_pvec pvec;
      reloc $startpos $endpos d
    }
| d=decl_OR_stmt_macro_call EQ b=braced_init_list
    { 
      d#add_children_r [b];
      let pvec =
        match List.rev d#pvec with
        | _::tl -> List.rev (1::tl)
        | [] -> assert false
      in
      d#set_pvec pvec;
      b#add_prefix "= ";
      reloc $startpos $endpos d
    }
| d=decl_OR_stmt_macro_call EQ e=conditional_expression sc=SEMICOLON
    { 
      d#add_children_r [e];
      let pvec =
        match List.rev d#pvec with
        | _::tl -> List.rev (1::tl)
        | [] -> assert false
      in
      d#set_pvec pvec;
      e#add_prefix "= ";
      if sc then e#add_suffix ";";
      reloc $startpos $endpos d
    }
| d=decl_OR_stmt_macro_call el=subscripts
    { 
      d#add_children_r el;
      let pvec =
        match List.rev d#pvec with
        | _::_::tl -> List.rev (0::(List.length el)::tl)
        | _ -> assert false
      in
      d#set_pvec pvec;
      reloc $startpos $endpos d
    }
| d=decl_OR_stmt_macro_call el=subscripts EQ b=braced_init_list
    { 
      d#add_children_r el;
      d#add_children_r [b];
      let pvec =
        match List.rev d#pvec with
        | _::_::tl -> List.rev (1::(List.length el)::tl)
        | _ -> assert false
      in
      d#set_pvec pvec;
      b#add_prefix "= ";
      reloc $startpos $endpos d
    }
;

template_declaration:
| t=template_head d=declaration
    { mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; d] }
| t=template_head c=concept_definition
    { mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; c] }
;

mid_templ_decl:
| { env#stack#exit_class() }
;

concept_definition:
| CONCEPT c=concept_name EQ ce=constant_expression SEMICOLON
    { mknode $startpos $endpos (L.ConceptDefinition c) [ce] }
;

%inline
concept_name:
| i=IDENT { i }
;

attribute_declaration:
| al=attribute_specifier_seq sc=SEMICOLON
    { 
      let a = mknode $startpos $endpos L.AttributeDeclaration al in
      if sc then a#add_suffix ";";
      a
    }
;

namespace_definition:
| n=named_namespace_definition   { n }
| u=unnamed_namespace_definition { u }
| n=nested_namespace_definition  { n }
| i=IDENT_NSM ml=macro_args LBRACE n=namespace_body RBRACE
    { 
      let pvec = [List.length ml; 1] in
      mknode ~pvec $startpos $endpos (L.NamespaceDefinitionMacro i) (ml @ n)
    }
;

named_namespace_definition_head:
| i_opt=ioption(inline) NAMESPACE al_opt=attribute_specifier_seq_opt i=IDENT
    { 
      let il = opt_to_list i_opt in
      let al = list_opt_to_list al_opt in
      let inline = match i_opt with Some _ -> true | _ -> false in
      let nn = NestedNS.mk1 (NS.mk ~inline i) in
      env#stack#enter_namespace nn;
      il, al, i, []
    }
| n=NS_MACRO al_opt=attribute_specifier_seq_opt i=IDENT
    { 
      let il = [mkleaf $startpos $endpos(n) (L.NamespaceHead n)] in
      let al = list_opt_to_list al_opt in
      let nn = NestedNS.mk1 (NS.mk i) in
      env#stack#enter_namespace nn;
      il, al, i, []
    }
| i_opt=ioption(inline) NAMESPACE al_opt=attribute_specifier_seq_opt m=id_macro_call
    { 
      let il = opt_to_list i_opt in
      let al = list_opt_to_list al_opt in
      let inline = match i_opt with Some _ -> true | _ -> false in
      let i = sprintf "%s()" m#get_name in
      let nn = NestedNS.mk1 (NS.mk ~inline i) in
      env#stack#enter_namespace nn;
      il, al, i, [m]
    }
;
named_namespace_definition:
| h=named_namespace_definition_head LBRACE nl=namespace_body RBRACE
    { 
      let frm = env#stack#exit_namespace() in
      let il, al, i, ml = h in
      let pvec = [List.length il; List.length al; List.length ml; List.length nl] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos (L.NamedNamespaceDefinition i) (il @ al @ ml @ nl)
      in
      env#register_namespace i nd frm;
      nd
    }
| h=named_namespace_definition_head ODD_LBRACE
    { 
      let il, al, i, ml = h in
      let pvec = [List.length il; List.length al; List.length ml; 0] in
      mknode ~pvec $symbolstartpos $endpos (L.NamedNamespaceDefinition i) (il @ al @ ml)
    }
;
(*named_namespace_definition:
| i_opt=ioption(inline) NAMESPACE al_opt=attribute_specifier_seq_opt i=IDENT
    LBRACE n=namespace_body RBRACE
    { 
      let il = opt_to_list i_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [List.length il; List.length al; 1] in
      mknode ~pvec $symbolstartpos $endpos (L.NamedNamespaceDefinition i) (il @ al @ n)
    }
;*)

unnamed_namespace_definition_head:
| i_opt=ioption(inline) NAMESPACE al_opt=attribute_specifier_seq_opt
    { 
      let il = opt_to_list i_opt in
      let al = list_opt_to_list al_opt in
      let inline = match i_opt with Some _ -> true | _ -> false in
      let i = "@" in
      let nn = NestedNS.mk1 (NS.mk ~inline i) in
      env#stack#enter_namespace nn;
      il, al, i
    }
;
unnamed_namespace_definition:
| h=unnamed_namespace_definition_head LBRACE nl=namespace_body RBRACE
    { 
      let frm = env#stack#exit_namespace() in
      let il, al, i = h in
      let pvec = [List.length il; List.length al; 1] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.UnnamedNamespaceDefinition (il @ al @ nl)
      in
      env#register_namespace i nd frm;
      nd
    }
;
(*unnamed_namespace_definition:
| i_opt=ioption(inline) NAMESPACE al_opt=attribute_specifier_seq_opt
    LBRACE n=namespace_body RBRACE
    { 
      let il = opt_to_list i_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [List.length il; List.length al; 1] in
      mknode ~pvec $symbolstartpos $endpos L.UnnamedNamespaceDefinition (il @ al @ n)
    }
;*)

%inline
inline:
| INLINE { mkleaf $startpos $endpos L.Inline }
;

nested_namespace_definition_head:
| NAMESPACE e=enclosing_namespace_specifier COLON_COLON i_opt=ioption(inline) i=IDENT
    { 
      let il = opt_to_list i_opt in
      let inline = match i_opt with Some _ -> true | _ -> false in
      let nn = NestedNS.append (Ast.nested_namespace_of_node e) (NS.mk ~inline i) in
      env#stack#enter_namespace nn;
      e, il, i
    }
;
nested_namespace_definition:
| h=nested_namespace_definition_head LBRACE nl=namespace_body RBRACE
    { 
      let frm = env#stack#exit_namespace() in
      let e, il, i = h in
      let pvec = [1; List.length il; 1] in
      let nd =
        mknode ~pvec $startpos $endpos (L.NestedNamespaceDefinition i) (e :: il @ nl)
      in
      env#register_namespace i nd frm;
      nd
    }
;
(*nested_namespace_definition:
| NAMESPACE e=enclosing_namespace_specifier COLON_COLON i_opt=ioption(inline) i=IDENT
    LBRACE n=namespace_body RBRACE
    { 
      let il = opt_to_list i_opt in
      let pvec = [1; List.length il; 1] in
      mknode ~pvec $symbolstartpos $endpos (L.NestedNamespaceDefinition i) (e :: il @ n)
    }
;*)

enclosing_namespace_specifier:
| i=IDENT { mkleaf ~pvec:[0; 0] $symbolstartpos $endpos (L.EnclosingNamespaceSpecifier i) }
| e=enclosing_namespace_specifier COLON_COLON i_opt=ioption(inline) i=IDENT
    { 
      let il = opt_to_list i_opt in
      let pvec = [1; List.length il] in
      mknode ~pvec $symbolstartpos $endpos (L.EnclosingNamespaceSpecifier i) (e::il)
    }
;

namespace_body:
| dl_opt=ioption(declaration_seq) { list_opt_to_list dl_opt }
;

explicit_instantiation:
| e_opt=ioption(extern) TEMPLATE d=declaration
    { 
      let el = opt_to_list e_opt in
      let pvec = [List.length el; 1] in
      mknode ~pvec $symbolstartpos $endpos L.ExplicitInstantiation (el @ [d])
    }
;
%inline
extern:
| EXTERN { mkleaf $startpos $endpos L.Extern }
;

explicit_specialization:
| TEMPLATE TEMPL_LT TEMPL_GT d=declaration
    { mknode $startpos $endpos L.ExplicitSpecialization [d] }
;

linkage_specification:
| s=_linkage_specification LBRACE dl_opt=ioption(declaration_seq) RBRACE
    { mknode $startpos $endpos (L.LinkageSpecification s) (list_opt_to_list dl_opt) }
| s=_linkage_specification d=declaration
    { mknode $startpos $endpos (L.LinkageSpecification s) [d] }
;

%inline
_linkage_specification:
| EXTERN s=STR_LITERAL { s }
;

nodeclspec_function_definition:
| n=_nodeclspec_function_definition sc=SEMICOLON { if sc then n#add_suffix ";"; reloc $startpos $endpos n }
;

%inline
_nodeclspec_function_definition:
| al_opt=attribute_specifier_seq_opt d=declarator
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; 1] in
      mknode ~pvec $symbolstartpos $endpos L.NodeclspecFunctionDeclaration (al @ [d])
    }
;

%inline
virt_specifier_seq_opt:
| vl_opt=ioption(virt_specifier_seq) { vl_opt }
;

virt_specifier_seq:
| v=virt_specifier { [v] }
| vl=virt_specifier_seq v=virt_specifier { vl @ [v] }
| vl=virt_specifier_seq a=attr_macro_call { vl @ [a] }
;

virt_specifier:
| FINAL    { mkleaf $startpos $endpos L.VirtSpecifierFinal }
| OVERRIDE { mkleaf $startpos $endpos L.VirtSpecifierOverride }
| i=VIRT_SPEC_MACRO { mkleaf $startpos $endpos (L.VirtSpecifierMacro i) }
| i=IDENT_VM ml=macro_args { mknode $startpos $endpos (L.VirtSpecifierMacroInvocation i) ml }
;


decl_specifier:
| s=storage_class_specifier { s }
| d=defining_type_specifier { d }
| f=function_specifier { f }
| FRIEND    { mkleaf ~pvec:[0] $startpos $endpos L.DeclSpecifierFriend }
| TYPEDEF   { mkleaf ~pvec:[0] $startpos $endpos L.DeclSpecifierTypedef }
| CONSTEXPR { mkleaf ~pvec:[0] $startpos $endpos L.DeclSpecifierConstexpr }
| CONSTEVAL { mkleaf ~pvec:[0] $startpos $endpos L.DeclSpecifierConsteval }
| CONSTINIT { mkleaf ~pvec:[0] $startpos $endpos L.DeclSpecifierConstinit }
| INLINE    { mkleaf ~pvec:[0] $startpos $endpos L.DeclSpecifierInline }
| d=decl_spec_macro { d }
| d=decl_spec_macro_call { d }
;

decl_spec_macro:
| d=DECL_SPEC_MACRO { mkleaf ~pvec:[0] $startpos $endpos (L.DeclSpecifierMacro d) }
;

decl_spec_macro_call:
| i=IDENT_DM ml=macro_args
    { 
      let pvec = [List.length ml; 0] in
      mknode ~pvec $startpos $endpos (L.DeclSpecifierMacroInvocation i) ml
    }
;

ty_macro_call:
| i=IDENT_TM ml=macro_args
    { 
      let pvec = [List.length ml; 0] in
      mknode ~pvec $startpos $endpos (L.TypeMacroInvocation i) ml
    }
;

storage_class_specifier:
| STATIC       { mkleaf ~pvec:[0] $startpos $endpos L.StorageClassSpecifierStatic }
| THREAD_LOCAL { mkleaf ~pvec:[0] $startpos $endpos L.StorageClassSpecifierThread_local }
| EXTERN       { mkleaf ~pvec:[0] $startpos $endpos L.StorageClassSpecifierExtern }
| MUTABLE      { mkleaf ~pvec:[0] $startpos $endpos L.StorageClassSpecifierMutable }
| REGISTER     { mkleaf ~pvec:[0] $startpos $endpos L.StorageClassSpecifierRegister } (* C *)
| VAX_GLOBALDEF LBRACE s=string_literal RBRACE (* VAX-C *)
    { 
      mknode $startpos $endpos L.StorageClassSpecifierVaxGlobaldef [s]
    }
;

function_specifier:
| VIRTUAL { mkleaf ~pvec:[0] $startpos $endpos L.FunctionSpecifierVirtual }
| e=explicit_specifier { e }
;

explicit_specifier:
| EXPLICIT LPAREN c=constant_expression RPAREN
    { mknode ~pvec:[1; 0] $startpos $endpos L.ExplicitSpecifier [c] }
| EXPLICIT { mkleaf ~pvec:[0] $startpos $endpos L.ExplicitSpecifier }(* %prec PREC*)
;

defining_type_specifier:
| t=type_specifier  { t }
| c=class_specifier { c }
| e=enum_specifier  { e }
;

%inline
access_specifier_opt:
| a_opt=ioption(access_specifier) { a_opt }
;

enum_specifier:
| a_opt=access_specifier_opt e=enum_head LBRACE el_opt=ioption(enumerator_list) RBRACE
    { 
      let al = opt_to_list a_opt in
      let el = list_opt_to_list el_opt in
      mknode ~pvec:[List.length al; 1; List.length el] $symbolstartpos $endpos L.EnumSpecifier (al@e::el)
    }
| a_opt=access_specifier_opt e=enum_head LBRACE el=enumerator_list COMMA RBRACE
    { 
      let al = opt_to_list a_opt in
      mknode ~pvec:[List.length al; 1; List.length el] $symbolstartpos $endpos L.EnumSpecifier (al@e::el)
    }
| p=pp_enum_head_if_section LBRACE el=enumerator_list ioption(COMMA) c=pp_enum_if_section_closing
    { 
      mknode ~pvec:[0; 1; List.length el; 1] $startpos $endpos L.EnumSpecifier (p::el@[c])
    }
;

pp_enum_if_section_closing:
| p=pp_enum_if_group_closing
    pl=pp_enum_elif_group_closing*
    p_opt=ioption(pp_enum_else_group_closing)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_enum_if_group_closing:
| p=pp_ifx_closing RBRACE i_opt=ioption(identifier) ioption(SEMICOLON)
    { 
      let il = opt_to_list i_opt in
      let pvec = [1; List.length il] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::il)
    }
;
pp_enum_elif_group_closing:
| p=pp_elif RBRACE i_opt=ioption(identifier) ioption(SEMICOLON)
    { 
      let il = opt_to_list i_opt in
      env#pstat#open_brace();
      let pvec = [1; List.length il] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p::il)
    }
;
pp_enum_else_group_closing:
| p=pp_else RBRACE i_opt=ioption(identifier) ioption(SEMICOLON)
    { 
      let il = opt_to_list i_opt in
      env#pstat#open_brace();
      let pvec = [1; List.length il] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p::il)
    }
| p=pp_else { mknode ~pvec:[1; 0; 0] $startpos $endpos (pp_if_group()) [p] } (* for invalid cases *)
;

pp_enum_head_if_section:
| p=pp_enum_head_if_group
    pl=list(pp_enum_head_elif_group) p_opt=ioption(pp_enum_head_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_enum_head_if_group:
| p=pp_ifx_eh dl_opt=ioption(declaration_seq) sl_opt=decl_specifier_seq_opt e=enum_head
    { 
      let dl = list_opt_to_list dl_opt in
      let sl = list_opt_to_list sl_opt in
      mknode ~pvec:[1; List.length dl; List.length sl; 1] $startpos $endpos (pp_if_group()) (p::dl@sl@[e])
    }
;
pp_enum_head_elif_group:
| p=pp_elif dl_opt=ioption(declaration_seq) sl_opt=decl_specifier_seq_opt e=enum_head
    { 
      let dl = list_opt_to_list dl_opt in
      let sl = list_opt_to_list sl_opt in
      env#stack#exit_enum();
      mknode ~pvec:[1; List.length dl; List.length sl; 1] $startpos $endpos (_pp_elif_group p) (p::dl@sl@[e])
    }
;
pp_enum_head_else_group:
| p=pp_else dl_opt=ioption(declaration_seq) sl_opt=decl_specifier_seq_opt e=enum_head
    { 
      let dl = list_opt_to_list dl_opt in
      let sl = list_opt_to_list sl_opt in
      env#stack#exit_enum();
      mknode ~pvec:[1; List.length dl; List.length sl; 1] $startpos $endpos (_pp_else_group p) (p::dl@sl@[e])
    }
;

enumerator_list:
| e=enumerator_definition { [e] }
| el=enumerator_list ioption(COMMA) e=enumerator_definition { (Xlist.last el)#add_suffix ","; el @ [e] }
;

enumerator_definition:
| e=enumerator
    { 
      env#register_enumerator e;
      e
    }
| e=enumerator EQ al=class_attr_spec_seq c=constant_expression
    { 
      let nd = mknode ~pvec:[1; List.length al; 1] $startpos $endpos L.EnumeratorDefinition (e::al@[c]) in
      env#register_enumerator nd;
      nd
    }
| e=enumerator EQ al=class_attr_spec_seq p=pp_expr_if_section
    { 
      let nd = mknode ~pvec:[1; List.length al + 1; 0] $startpos $endpos L.EnumeratorDefinition (e::al@[p]) in
      env#register_enumerator nd;
      nd
    }
| d=STMT_MACRO
    { 
      let nd = mkleaf $startpos $endpos (L.EnumeratorDefinitionMacro d) in
      env#register_enumerator nd;
      nd
    }
| p=pp_control_line { p }
| p=pp_edef_if_section { p }
;

pp_edef_if_section:
| p=pp_edef_if_group pl=list(pp_edef_elif_group) p_opt=ioption(pp_edef_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_edef_if_group:
| p=pp_ifx ioption(COMMA) el=enumerator_list ioption(COMMA)
    { mknode ~pvec:[1; List.length el] $startpos $endpos (pp_if_group()) (p::el) }
;
pp_edef_elif_group:
| p=pp_elif ioption(COMMA) el=enumerator_list ioption(COMMA)
    { mknode ~pvec:[1; List.length el] $startpos $endpos (_pp_elif_group p) (p::el) }
;
pp_edef_else_group:
| p=pp_else ioption(COMMA) el=enumerator_list ioption(COMMA)
    { mknode ~pvec:[1; List.length el] $startpos $endpos (_pp_else_group p) (p::el) }
;


enumerator:
| i=IDENT_E al_opt=attribute_specifier_seq_opt
    { mknode $startpos $endpos (L.Enumerator i) (list_opt_to_list al_opt) }
| i=IDENT_E ml=macro_args { mknode $startpos $endpos (L.EnumeratorMacroInvocation i) ml }
| i=IDENT_DSM ml=macro_args { mknode $startpos $endpos (L.EnumeratorMacroInvocation i) ml }
;

enum_head:
| e=enum_key
    al_opt=attribute_specifier_seq_opt e_opt=ioption(enum_head_name) eb_opt=ioption(enum_base)
    { 
      let lab = L.EnumKey.to_enum_head e in
      let al = list_opt_to_list al_opt in
      let el = opt_to_list e_opt in
      let ebl = opt_to_list eb_opt in
      let pvec = [List.length al; List.length el; List.length ebl] in
      let nd = mknode ~pvec $symbolstartpos $endpos lab (al @ el @ ebl) in
      let qn = env#register_enum_head nd in
      env#stack#enter_enum qn;
      nd
    }
| i=IDENT_E ml=macro_args
    { 
      let pvec = [0; 0; 0; List.length ml] in
      let nd = mknode ~pvec $startpos $endpos (L.EnumHeadEnumMacro i) ml in
      let qn =
        try
          env#register_enum_head nd
        with
          _ -> ""
      in
      env#stack#enter_enum qn;
      nd
    }
;

%inline
decl_specifier_seq_opt:
| dl_opt=ioption(decl_specifier_seq) { dl_opt }
;

decl_specifier_seq:
| p=pp_decl_spec_if_section { [p] }
| d=decl_specifier { [d] }
| dl=decl_specifier_seq d=decl_specifier { dl @ [d] }
| dl=decl_specifier_seq p=pp_decl_spec_if_section { dl @ [p] }
| dl=decl_specifier_seq a=attribute_specifier
    { 
      let last_d = Xlist.last dl in
      _reloc_end $endpos last_d;
      add_to_last_pvec_elem last_d 1;
      last_d#add_children_r [a];
      dl
    }
;

pp_decl_spec_if_section:
| p=pp_decl_spec_if_group
    pl=list(pp_decl_spec_elif_group) p_opt=ioption(pp_decl_spec_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_decl_spec_if_group:
| p=pp_ifx_d al_opt=attribute_specifier_seq_opt dl_opt=decl_specifier_seq_opt ad_opt=ioption(abstract_declarator)
    { 
      let al = list_opt_to_list al_opt in
      let dl = list_opt_to_list dl_opt in
      let adl = opt_to_list ad_opt in
      let pvec = [1; List.length al + List.length dl + List.length adl] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p :: al @ dl @ adl)
    }
| p=pp_ifx_d e=EXTERN s=STR_LITERAL
    { 
      ignore e;
      let e_ = mkleaf $startpos(e) $endpos (L.LinkageSpecification s) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [p; e_]
    }
| p=pp_ifx_d s=STR_LITERAL
    { 
      let s_ = mkleaf $startpos(s) $endpos (L.StringLiteral s) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [p; s_]
    }
;
pp_decl_spec_elif_group:
| p=pp_elif al_opt=attribute_specifier_seq_opt dl_opt=decl_specifier_seq_opt ad_opt=ioption(abstract_declarator)
    { 
      let al = list_opt_to_list al_opt in
      let dl = list_opt_to_list dl_opt in
      let adl = opt_to_list ad_opt in
      let pvec = [1; List.length al + List.length dl + List.length adl] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p :: al @ dl @ adl)
    }
| p=pp_elif e=EXTERN s=STR_LITERAL
    { 
      ignore e;
      let e_ = mkleaf $startpos(e) $endpos (L.LinkageSpecification s) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p; e_]
    }
| p=pp_elif s=STR_LITERAL
    { 
      let s_ = mkleaf $startpos(s) $endpos (L.StringLiteral s) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p; s_]
    }
;
pp_decl_spec_else_group:
| p=pp_else al_opt=attribute_specifier_seq_opt dl_opt=decl_specifier_seq_opt ad_opt=ioption(abstract_declarator)
    { 
      let al = list_opt_to_list al_opt in
      let dl = list_opt_to_list dl_opt in
      let adl = opt_to_list ad_opt in
      let pvec = [1; List.length al + List.length dl + List.length adl] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p :: al @ dl @ adl)
    }
| p=pp_else e=EXTERN s=STR_LITERAL
    { 
      ignore e;
      let e_ = mkleaf $startpos(e) $endpos (L.LinkageSpecification s) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p; e_]
    }
| p=pp_else s=STR_LITERAL
    { 
      let s_ = mkleaf $startpos(s) $endpos (L.StringLiteral s) in
      let pvec = [1; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p; s_]
    }
;

%inline
func_head:
| al_opt=attribute_specifier_seq_opt d=declarator vl_opt=virt_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      let vl = list_opt_to_list vl_opt in
      let pvec = [List.length al; 0; 1; List.length vl; 0] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.FunctionHead (al @ [d] @ vl)
      in
      env#register_function nd;
      nd
    }
| al_opt=attribute_specifier_seq_opt d=declarator r=requires_clause
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; 0; 1; 0; 1] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.FunctionHead (al @ [d; r])
      in
      env#register_function nd;
      nd
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq d=declarator
    vl_opt=virt_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      let vl = list_opt_to_list vl_opt in
      let pvec = [List.length al; List.length dl; 1; List.length vl; 0] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.FunctionHead (al @ dl @ [d] @ vl)
      in
      env#register_function nd;
      nd
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq d=declarator r=requires_clause
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 0; 1] in
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.FunctionHead (al @ dl @ [d; r])
      in
      env#register_function nd;
      nd
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq
    d=pp_dtor_if_section_broken p=parameter_declaration_clause RPAREN
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length dl; 1; 0; 0] in
      d#add_children_r [p];
      d#set_pvec (d#pvec @ [1]);
      _reloc $startpos(d) $endpos d;
      let nd =
        mknode ~pvec $symbolstartpos $endpos L.FunctionHead (al @ dl @ [d])
      in
      env#register_function nd;
      nd
    }
| i=FUNC_HEAD_MACRO
    { 
      let h = mkleaf $startpos $endpos (L.FunctionHeadMacro i) in
      let pvec = [0; 0; 0; 0; 0; 1] in
      mknode ~pvec $symbolstartpos $endpos L.FunctionHead [h]
    }
;

pp_dtor_if_section_broken:
| p=pp_dtor_if_group_broken
    pl=pp_dtor_elif_group_broken* p_opt=ioption(pp_dtor_else_group_broken) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionBrokenDtorFunc (p :: pl @ pl1 @ [pe])
    }
;
pp_dtor_if_group_broken:
| p=pp_ifx_d n=noptr_declarator TY_LPAREN
    { 
      let n_ = mknode $startpos(n) $endpos L.NoptrDeclaratorFunc [n] in
      mknode ~pvec:[1; 0; 1] $startpos $endpos (pp_if_group()) (p::[n_])
    }
| p=pp_ifx_d pl=pp_control_line+ n=noptr_declarator TY_LPAREN
    { 
      let n_ = mknode $startpos(n) $endpos L.NoptrDeclaratorFunc [n] in
      mknode ~pvec:[1; List.length pl; 1] $startpos $endpos (pp_if_group()) (p::pl@[n_])
    }
;
pp_dtor_elif_group_broken:
| p=pp_elif n=noptr_declarator TY_LPAREN
    { 
      env#pstat#close_paren();
      let n_ = mknode $startpos(n) $endpos L.NoptrDeclaratorFunc [n] in
      mknode ~pvec:[1; 0; 1] $startpos $endpos (_pp_elif_group p) (p::[n_])
    }
| p=pp_elif pl=pp_control_line+ n=noptr_declarator TY_LPAREN
    { 
      env#pstat#close_paren();
      let n_ = mknode $startpos(n) $endpos L.NoptrDeclaratorFunc [n] in
      mknode ~pvec:[1; List.length pl; 1] $startpos $endpos (_pp_elif_group p) (p::pl@[n_])
    }
;
pp_dtor_else_group_broken:
| p=pp_else n=noptr_declarator TY_LPAREN
    { 
      env#pstat#close_paren();
      let n_ = mknode $startpos(n) $endpos L.NoptrDeclaratorFunc [n] in
      mknode ~pvec:[1; 0; 1] $startpos $endpos (_pp_else_group p) (p::[n_])
    }
| p=pp_else pl=pp_control_line+ n=noptr_declarator TY_LPAREN
    { 
      env#pstat#close_paren();
      let n_ = mknode $startpos(n) $endpos L.NoptrDeclaratorFunc [n] in
      mknode ~pvec:[1; List.length pl; 1] $startpos $endpos (_pp_else_group p) (p::pl@[n_])
    }
;


function_definition:
| f=func_head b=function_body
    { 
      f#relab L.FunctionDefinition;
      f#add_children_r [b];
      f#set_pvec (f#pvec @ [1]);
      reloc $startpos $endpos f
    }
| f=func_head b=pp_func_body_if_section
    { 
      f#relab L.FunctionDefinition;
      f#add_children_r [b];
      f#set_pvec (f#pvec @ [1]);
      reloc $startpos $endpos f
    }
| m=id_macro_call b=params_body_macro
    { mknode ~pvec:[0; 0; 1; 0; 0; 1] $startpos $endpos L.FunctionDefinition [m; b] }
;

params_body_macro:
| i=PARAMS_BODY_MACRO { mkleaf $startpos $endpos (L.FunctionBodyMacro i) }
| p=params_body_macro_call { p }
;

params_body_macro_call:
| i=IDENT_PBM ml=macro_args { mknode $startpos $endpos (L.FunctionBodyMacroInvocation i) ml }
;

pp_func_head_if_section_broken:
| p=pp_func_head_if_group_broken
    pl=list(pp_func_head_elif_group_broken)
    p_opt=ioption(pp_func_head_else_group_broken)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionBrokenFuncDef (p :: pl @ pl1 @ [pe])
    }
;
pp_func_head_if_group_broken:
| p=pp_ifx f=func_head c_opt=ioption(ctor_initializer) l=LBRACE sl_opt=statement_seq_opt
    { 
      ignore l;
      let c_ = mknode $startpos(l) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      let b_ =
        match c_opt with
        | Some c -> mknode ~pvec:[1; 1] $startpos(c_opt) $endpos L.FunctionBody [c; c_]
        | None -> mknode ~pvec:[0; 1] $startpos(l) $endpos L.FunctionBody [c_]
      in
      f#relab L.FunctionDefinition;
      f#add_children_r [b_];
      f#set_pvec (f#pvec @ [1]);
      _reloc $startpos(f) $endpos f;
      mknode $startpos $endpos (pp_if_group()) [p; f]
    }
| p=pp_ifx_d pl=pp_control_line+ f=func_head c_opt=ioption(ctor_initializer) l=LBRACE mid_brace_open sl_opt=statement_seq_opt
    { 
      ignore l;
      let c_ = mknode $startpos(l) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      let b_ =
        match c_opt with
        | Some c -> mknode ~pvec:[1; 1] $startpos(c_opt) $endpos L.FunctionBody [c; c_]
        | None -> mknode ~pvec:[0; 1] $startpos(l) $endpos L.FunctionBody [c_]
      in
      f#relab L.FunctionDefinition;
      f#add_children_r [b_];
      f#set_pvec (f#pvec @ [1]);
      _reloc $startpos(f) $endpos f;
      mknode $startpos $endpos (pp_if_group()) (p :: pl @ [f])
    }
| pi=pp_ifx p=pp_func_head_if_section_broken
    { mknode $startpos $endpos (pp_if_group()) [pi; p] }
;
mid_brace_open:
| { env#clear_virtual_func_flag(); env#stack#enter_block $startpos.Lexing.pos_lnum; }
;
pp_func_head_elif_group_broken:
| p=pp_elif f=func_head c_opt=ioption(ctor_initializer) l=LBRACE sl_opt=statement_seq_opt
    { 
      env#pstat#close_brace();
      env#close_in_body_brace();
      ignore l;
      let c_ = mknode $startpos(l) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      let b_ =
        match c_opt with
        | Some c -> mknode ~pvec:[1; 1] $startpos(c_opt) $endpos L.FunctionBody [c; c_]
        | None -> mknode ~pvec:[0; 1] $startpos(l) $endpos L.FunctionBody [c_]
      in
      f#relab L.FunctionDefinition;
      f#add_children_r [b_];
      f#set_pvec (f#pvec @ [1]);
      _reloc $startpos(f) $endpos f;
      mknode $startpos $endpos (_pp_elif_group p) [p; f]
    }
| p=pp_elif pl=pp_control_line+ f=func_head c_opt=ioption(ctor_initializer) l=LBRACE sl_opt=statement_seq_opt
    { 
      env#pstat#close_brace();
      env#close_in_body_brace();
      ignore l;
      let c_ = mknode $startpos(l) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      let b_ =
        match c_opt with
        | Some c -> mknode ~pvec:[1; 1] $startpos(c_opt) $endpos L.FunctionBody [c; c_]
        | None -> mknode ~pvec:[0; 1] $startpos(l) $endpos L.FunctionBody [c_]
      in
      f#relab L.FunctionDefinition;
      f#add_children_r [b_];
      f#set_pvec (f#pvec @ [1]);
      _reloc $startpos(f) $endpos f;
      mknode $startpos $endpos (_pp_elif_group p) (p :: pl @ [f])
    }
| pi=pp_elif p=pp_func_head_if_section_broken
    { mknode $startpos $endpos (_pp_elif_group p) [pi; p] }
;
pp_func_head_else_group_broken:
| p=pp_else f=func_head c_opt=ioption(ctor_initializer) l=LBRACE sl_opt=statement_seq_opt
    { 
      env#pstat#close_brace();
      env#close_in_body_brace();
      ignore l;
      let c_ = mknode $startpos(l) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      let b_ =
        match c_opt with
        | Some c -> mknode ~pvec:[1; 1] $startpos(c_opt) $endpos L.FunctionBody [c; c_]
        | None -> mknode ~pvec:[0; 1] $startpos(l) $endpos L.FunctionBody [c_]
      in
      f#relab L.FunctionDefinition;
      f#add_children_r [b_];
      f#set_pvec (f#pvec @ [1]);
      _reloc $startpos(f) $endpos f;
      mknode $startpos $endpos (_pp_else_group p) [p; f]
    }
| p=pp_else pl=pp_control_line+ f=func_head c_opt=ioption(ctor_initializer) l=LBRACE sl_opt=statement_seq_opt
    { 
      env#pstat#close_brace();
      env#close_in_body_brace();
      ignore l;
      let c_ = mknode $startpos(l) $endpos L.CompoundStatement (list_opt_to_list sl_opt) in
      let b_ =
        match c_opt with
        | Some c -> mknode ~pvec:[1; 1] $startpos(c_opt) $endpos L.FunctionBody [c; c_]
        | None -> mknode ~pvec:[0; 1] $startpos(l) $endpos L.FunctionBody [c_]
      in
      f#relab L.FunctionDefinition;
      f#add_children_r [b_];
      f#set_pvec (f#pvec @ [1]);
      _reloc $startpos(f) $endpos f;
      mknode $startpos $endpos (_pp_else_group p) (p :: pl @ [f])
    }
| pi=pp_else p=pp_func_head_if_section_broken
    { mknode $startpos $endpos (_pp_else_group pi) [pi; p] }
;

_pp_func_head_if_section:
| p=_pp_func_head_if_group
    pl=list(_pp_func_head_elif_group) p_opt=ioption(_pp_func_head_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionAltFuncDef (p :: pl @ pl1 @ [pe])
    }
;
_pp_func_head_if_group:
| p=pp_ifx_d f=func_head c_opt=ioption(ctor_initializer)
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; 1 + List.length cl] $startpos $endpos (pp_if_group()) ([p; f] @ cl)
    }
| p=pp_ifx_d dl=pp_control_line+ f=func_head c_opt=ioption(ctor_initializer)
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; List.length dl + 1 + List.length cl] $startpos $endpos (pp_if_group()) (p :: dl @ [f] @ cl)
    }
| p=pp_ifx_d s=_linkage_specification f=func_head c_opt=ioption(ctor_initializer)
    { 
      let s_ = mkleaf $startpos(s) $endpos(s) (L.LinkageSpecification s) in
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; 2 + List.length cl] $startpos $endpos (pp_if_group()) ([p; s_; f] @ cl)
    }
| p=pp_ifx_d f=_pp_func_head_if_section { mknode $startpos $endpos (pp_if_group()) [p; f] }
;
_pp_func_head_elif_group:
| p=pp_elif f=func_head c_opt=ioption(ctor_initializer)
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; 1 + List.length cl] $startpos $endpos (_pp_elif_group p) ([p; f] @ cl)
    }
(*| p=pp_elif dl=declaration_seq f=func_head c_opt=ioption(ctor_initializer)
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; List.length dl + 1 + List.length cl] $startpos $endpos (_pp_elif_group p) (p :: dl @ [f] @ cl)
    }*)
| p=pp_elif s=_linkage_specification f=func_head c_opt=ioption(ctor_initializer)
    { 
      let s_ = mkleaf $startpos(s) $endpos(s) (L.LinkageSpecification s) in
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; 2 + List.length cl] $startpos $endpos (_pp_elif_group p) ([p; s_; f] @ cl)
    }
| p=pp_elif f=_pp_func_head_if_section { mknode $startpos $endpos (_pp_elif_group p) [p; f] }
;
_pp_func_head_else_group:
| p=pp_else f=func_head c_opt=ioption(ctor_initializer)
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; 1 + List.length cl] $startpos $endpos (_pp_else_group p) ([p; f] @ cl)
    }
(*| p=pp_else dl=declaration_seq f=func_head c_opt=ioption(ctor_initializer)
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; List.length dl + 1 + List.length cl] $startpos $endpos (_pp_else_group p) (p :: dl @ [f] @ cl)
    }*)
| p=pp_else s=_linkage_specification f=func_head c_opt=ioption(ctor_initializer)
    { 
      let s_ = mkleaf $startpos(s) $endpos(s) (L.LinkageSpecification s) in
      let cl = opt_to_list c_opt in
      mknode ~pvec:[1; 2 + List.length cl] $startpos $endpos (_pp_else_group p) ([p; s_; f] @ cl)
    }
| p=pp_else f=_pp_func_head_if_section { mknode $startpos $endpos (_pp_else_group p) [p; f] }
;

function_body:
| c=compound_statement { mknode ~pvec:[0; 1] $symbolstartpos $endpos L.FunctionBody [c] }
| ci=ctor_initializer c=compound_statement { mknode ~pvec:[1; 1] $symbolstartpos $endpos L.FunctionBody [ci; c] }
| ci=ctor_initializer MARKER COMMA p=pp_func_body_if_section
    { mknode ~pvec:[1; 1] $symbolstartpos $endpos L.FunctionBody [ci; p] }
| f=function_try_block { f }
| EQ DEFAULT SEMICOLON { mkleaf $startpos $endpos L.FunctionBodyDefault }
| EQ DELETE  SEMICOLON { mkleaf $startpos $endpos L.FunctionBodyDelete }
| i=BODY_MACRO         { mkleaf $startpos $endpos (L.FunctionBodyMacro i) }
| i=IDENT_BM ml=macro_args { mknode $startpos $endpos (L.FunctionBodyMacroInvocation i) ml }
| DUMMY_BODY { mkleaf $startpos $endpos L.DummyBody }
| LBRACE sl0=stmt_seq0 p=pp_stmt_if_section_close_open sl1=stmt_seq0 RBRACE
    { 
      let c = mknode $startpos $endpos L.CompoundStatement (sl0@[p]@sl1) in
      mknode ~pvec:[0; 1] $symbolstartpos $endpos L.FunctionBody [c]
    }
;

pp_stmt_if_section_close_open:
| p=pp_stmt_if_group_close_open
    pl=pp_stmt_elif_group_close_open*
    p_opt=ioption(pp_stmt_else_group_close_open)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, get_pp_if_cond pe)) (p :: pl @ pl1 @ [pe])
    }
;
pp_stmt_if_group_close_open:
| p=pp_ifx_close_open sl=stmt_seq0 RBRACE fh=func_head lb=LBRACE sl1=stmt_seq0
    { 
      ignore lb;
      let b = mknode $startpos(lb) $endpos L.FunctionBody sl1 in
      fh#relab L.FunctionDefinition;
      fh#add_children_r [b];
      fh#set_pvec (fh#pvec@[1]);
      _reloc $startpos(fh) $endpos fh;
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::sl@[fh])
    }
(*| p=pp_ifx_close_open sl=stmt_seq0
    { 
      let pvec = [1; List.length sl; 0] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::sl)
    }*)
;
pp_stmt_elif_group_close_open:
| p=pp_elif sl=stmt_seq0 RBRACE fh=func_head lb=LBRACE sl1=stmt_seq0
    { 
      ignore lb;
      let b = mknode $startpos(lb) $endpos L.FunctionBody sl1 in
      fh#relab L.FunctionDefinition;
      fh#add_children_r [b];
      fh#set_pvec (fh#pvec@[1]);
      _reloc $startpos(fh) $endpos fh;
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p::sl@sl1)
    }
| p=pp_elif sl=stmt_seq0
    { 
      let pvec = [1; List.length sl; 0] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p::sl)
    }
;
pp_stmt_else_group_close_open:
| p=pp_else sl=stmt_seq0 RBRACE fh=func_head lb=LBRACE sl1=stmt_seq0
    { 
      ignore lb;
      let b = mknode $startpos(lb) $endpos L.FunctionBody sl1 in
      fh#relab L.FunctionDefinition;
      fh#add_children_r [b];
      fh#set_pvec (fh#pvec@[1]);
      _reloc $startpos(fh) $endpos fh;
      let pvec = [1; List.length sl; 1] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p::sl@sl1)
    }
| p=pp_else sl=stmt_seq0
    { 
      let pvec = [1; List.length sl; 0] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p::sl)
    }
;

%inline
body_tail:
| BRACE_LEVEL c=compound_statement { c }
;

pp_func_body_if_section:
| p=pp_func_body_if_group
    pl=list(pp_func_body_elif_group) p_opt=ioption(pp_func_body_else_group) pe=pp_endif b_opt=ioption(body_tail)
    { 
      let pl1 = opt_to_list p_opt in
      let bl =
        match b_opt with
        | Some b when b#children <> [] -> [b]
        | _ -> []
      in
      let pvec = [1; List.length pl; List.length pl1; 1; List.length bl] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionAltFuncDef (p :: pl @ pl1 @ [pe] @ bl)
    }
;
%inline
_function_body:
| f=function_body { [f] }
| m=mem_initializer f=function_body { [m; f] }
| pl=pp_control_line+ f=function_body { pl @ [f] }
;
pp_func_body_if_group:
| p=pp_ifx fl=_function_body { mknode $startpos $endpos (pp_if_group()) (p::fl) }
| p=pp_ifx fl=_function_body dl=declaration_seq
    { mknode ~pvec:[1; 1; List.length dl] $startpos $endpos (pp_if_group()) (p :: fl @ dl) }
| p=pp_ifx fl=_function_body dl=declaration_seq d=odd_decl
    { mknode ~pvec:[1; 1; List.length dl + 1] $startpos $endpos (pp_if_group()) (p :: fl @ dl @ [d]) }
| p=pp_ifx SEMICOLON { mknode $startpos $endpos (pp_if_group()) [p] }
| p=pp_ifx pl=pp_control_line+ SEMICOLON { mknode $startpos $endpos (pp_if_group()) (p::pl) }
;
pp_func_body_elif_group:
| p=pp_elif fl=_function_body { mknode $startpos $endpos (_pp_elif_group p) (p::fl) }
| p=pp_elif fl=_function_body dl=declaration_seq
    { mknode ~pvec:[1; 1; List.length dl] $startpos $endpos (_pp_elif_group p) (p :: fl @ dl) }
| p=pp_elif fl=_function_body dl=declaration_seq d=odd_decl
    { mknode ~pvec:[1; 1; List.length dl + 1] $startpos $endpos (_pp_elif_group p) (p :: fl @ dl @ [d]) }
| p=pp_elif SEMICOLON { mknode $startpos $endpos (_pp_elif_group p) [p] }
| p=pp_elif pl=pp_control_line+ SEMICOLON { mknode $startpos $endpos (_pp_elif_group p) (p::pl) }
;
pp_func_body_else_group:
| p=pp_else fl=_function_body { mknode $startpos $endpos (_pp_else_group p) (p::fl) }
| p=pp_else fl=_function_body dl=declaration_seq
    { mknode ~pvec:[1; 1; List.length dl] $startpos $endpos (_pp_else_group p) (p :: fl @ dl) }
| p=pp_else fl=_function_body dl=declaration_seq d=odd_decl
    { mknode ~pvec:[1; 1; List.length dl + 1] $startpos $endpos (_pp_else_group p) (p :: fl @ dl @ [d]) }
| p=pp_else SEMICOLON { mknode $startpos $endpos (_pp_else_group p) [p] }
| p=pp_else pl=pp_control_line+ SEMICOLON { mknode $startpos $endpos (_pp_else_group p) (p::pl) }
;

function_try_block:
| TRY c_opt=ioption(ctor_initializer) c=compound_statement hl=handler_seq
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 1; List.length hl] in
      mknode ~pvec $startpos $endpos L.FunctionTryBlock (cl @ c :: hl)
    }
;

ctor_initializer:
| COLON c=_ctor_initializer { c#add_prefix ": "; reloc $startpos $endpos c }
| p=pp_ctor_init_if_section cl_opt=ioption(mem_initializer_list)
    { 
      let cl = list_opt_to_list cl_opt in
      mknode ~pvec:[1; List.length cl] $symbolstartpos $endpos L.CtorInitializer (p::cl)
    }
;

%inline
_ctor_initializer:
| (*COLON*) ml=mem_initializer_list { mknode $startpos $endpos L.CtorInitializer ml }
;

pp_ctor_init_if_section:
| p=pp_ctor_init_if_group
    pl=pp_ctor_init_elif_group* p_opt=ioption(pp_ctor_init_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_ctor_init_if_group:
| p=pp_ifx COLON c=_ctor_initializer ioption(COMMA)
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c]
    }
;
pp_ctor_init_elif_group:
| p=pp_elif COLON c=_ctor_initializer ioption(COMMA)
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c]
    }
;
pp_ctor_init_else_group:
| p=pp_else COLON c=_ctor_initializer ioption(COMMA)
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c]
    }
;

mem_initializer_list:
| m=mem_initializer { [m] }
| m=mem_init_macro_call { [m] }
| p=pp_minit_if_section m_opt=ioption(mem_initializer) { p :: (opt_to_list m_opt) }
| m=mem_initializer ELLIPSIS
    { 
      let m_ = mknode $startpos $endpos L.PackExpansion [m] in
      m_#add_suffix "...";
      [m_]
    }
| p=pp_control_line { [p] }
| m=mem_init_macro_call mi=mem_initializer { [m; mi] }
| ml=mem_initializer_list COMMA m=mem_initializer
    { 
      (Xlist.last ml)#add_suffix ",";
      ml @ [m]
    }
| ml=mem_initializer_list COMMA m=mem_initializer ELLIPSIS
    { 
      let m_ = mknode $startpos(m) $endpos L.PackExpansion [m] in
      m_#add_suffix "...";
      (Xlist.last ml)#add_suffix ",";
      ml @ [m_]
    }
| ml=mem_initializer_list COMMA p=pp_minit_if_section m_opt=ioption(mem_initializer)
    { 
      (Xlist.last ml)#add_suffix ",";
      ml @ p :: (opt_to_list m_opt)
    }
| ml=mem_initializer_list p=pp_minit_if_section m_opt=ioption(mem_initializer)
    { ml @ p :: (opt_to_list m_opt) }
| ml=mem_initializer_list c_opt=ioption(COMMA) m=mem_init_macro_call
    { 
      if c_opt <> None then
        (Xlist.last ml)#add_suffix ",";
      ml @ [m]
    }
| ml=mem_initializer_list c_opt=ioption(COMMA) m0=mem_init_macro_call m1=mem_initializer
    { 
      if c_opt <> None then
        (Xlist.last ml)#add_suffix ",";
      ml @ [m0; m1]
    }
| ml=mem_initializer_list c_opt=ioption(COMMA) p=pp_control_line
    { 
      if c_opt <> None then
        (Xlist.last ml)#add_suffix ",";
      ml @ [p]
    }
| ml=mem_initializer_list c_opt=ioption(COMMA) p=pp_control_line m=mem_initializer
    { 
      if c_opt <> None then
        (Xlist.last ml)#add_suffix ",";
      ml @ [p; m]
    }
;

pp_minit_if_section:
| p=pp_minit_if_group
    pl=list(pp_minit_elif_group) p_opt=ioption(pp_minit_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_minit_if_group:
| p=pp_ifx ioption(COMMA) ml=mem_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos (pp_if_group()) (p::ml) }
;
pp_minit_elif_group:
| p=pp_elif ioption(COMMA) ml=mem_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos (_pp_elif_group p) (p::ml) }
;
pp_minit_else_group:
| p=pp_else ioption(COMMA) ml=mem_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos (_pp_else_group p) (p::ml) }
;

mem_initializer:
| m=mem_initializer_id LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      if el_opt <> None then begin
        m#add_suffix "(";
        (Xlist.last el)#add_suffix ")"
      end
      else
        m#add_suffix "()";
      mknode ~pvec:[1; List.length el] $startpos $endpos L.MemInitializer (m::el)
    }
| m=mem_initializer_id LPAREN el_opt=expression_list_opt pp=pp_args_if_section_closing
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el + 1] in
      m#add_suffix "(";
      mknode ~pvec $startpos $endpos L.MemInitializer (m::el@[pp])
    }
| p=pp_minit_if_section_broken c_opt=ioption(COMMA) el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      if el_opt <> None then
        (Xlist.last el)#add_suffix ")"
      else begin
        if c_opt <> None then
          p#add_suffix ",)"
        else
          p#add_suffix ")"
      end;
      mknode ~pvec:[1; List.length el] $startpos $endpos L.MemInitializer (p::el)
    }
| m=mem_initializer_id b=braced_init_list
    { mknode ~pvec:[1; 1] $startpos $endpos L.MemInitializer [m; b] }
| m=mem_initializer_id a=args_macro
    { mknode ~pvec:[1; 1] $startpos $endpos L.MemInitializer [m; a] }
;

pp_minit_if_section_broken:
| p=pp_minit_if_group_broken
    pl=pp_minit_elif_group_broken*
    p_opt=ioption(pp_minit_else_group_broken)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionBrokenIf (p :: pl @ pl1 @ [pe])
    }
;
pp_minit_if_group_broken:
| pi=pp_ifx_e m=mem_initializer_id LPAREN el_opt=expression_list_opt
    { 
      let el = list_opt_to_list el_opt in
      m#add_suffix "(";
      let e_ = mknode ~pvec:[1; List.length el] $startpos(m) $endpos L.PostfixExpressionFunCall (m::el) in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; e_]
    }
;
pp_minit_elif_group_broken:
| pi=pp_elif m=mem_initializer_id LPAREN el_opt=expression_list_opt
    { 
      env#pstat#close_paren();
      let el = list_opt_to_list el_opt in
      m#add_suffix "(";
      let e_ = mknode ~pvec:[1; List.length el] $startpos(m) $endpos L.PostfixExpressionFunCall (m::el) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pi) [pi; e_]
    }
;
pp_minit_else_group_broken:
| pi=pp_else m=mem_initializer_id LPAREN el_opt=expression_list_opt
    { 
      env#pstat#close_paren();
      let el = list_opt_to_list el_opt in
      m#add_suffix "(";
      let e_ = mknode ~pvec:[1; List.length el] $startpos(m) $endpos L.PostfixExpressionFunCall (m::el) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pi) [pi; e_]
    }
;

mem_init_macro_call:
(*| i=IDENT_EM LPAREN COMMA m=mem_initializer RPAREN
    { mknode $startpos $endpos (L.MemInitMacroInvocation i) [m] }*)
| i=IDENT_EM ml=macro_args { mknode $startpos $endpos (L.MemInitMacroInvocation i) ml }
| i=IDENT_DSM ml=macro_args { mknode $startpos $endpos (L.MemInitMacroInvocation i) ml }
| i=IDENT_AGM ml=macro_args { mknode $startpos $endpos (L.MemInitMacroInvocation i) ml }
;

mem_initializer_id:
| c=class_or_decltype { c }
(*| IDENT { }*)(*contained in type_name*)
| i=identifier { i }
;

class_or_decltype:
| t=type_name
    { 
      let uqn = Ast.uqn_of_type_name t in
      env#set_type_binding uqn t;
      t
    }
| n=nested_name_specifier t=type_name
    { 
      let uqn = Ast.uqn_of_type_name t in
      let p = (Ast.encode_nested_name_spec n) in
      let nd = mknode ~pvec:[1; 1] $startpos $endpos L.QualifiedTypeName [n; t] in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| d=decltype_specifier { d }
;

decltype_specifier:
| DECLTYPE LPAREN e=expression RPAREN
    { mknode ~pvec:[1; 0] $startpos $endpos L.DecltypeSpecifier [e] }
;

handler_seq:
(*| h=handler hl_opt=ioption(handler_seq) { h::(list_opt_to_list hl_opt) }*)
| h=handler { [h] }
| h=handler hl=handler_seq { h::hl }
;

handler:
| CATCH TY_LPAREN e=exception_declaration RPAREN DUMMY_STMT
    { 
      env#register_exc_decl e;
      mknode ~pvec:[1; 0] $startpos $endpos L.Handler [e]
    }
| CATCH TY_LPAREN e=exception_declaration RPAREN c=compound_statement
    { 
      env#register_exc_decl e;
      mknode ~pvec:[1; 1] $startpos $endpos L.Handler [e; c]
    }
| p=pp_handler_if_section { p }
| p=pp_handler_if_section_broken c=compound_statement
    { 
      p#add_children_r [c];
      p#set_pvec (p#pvec @ [1]);
      reloc $startpos $endpos p
    }
;

pp_handler_if_section:
| p=pp_handler_if_group pl=pp_handler_elif_group* p_opt=ioption(pp_handler_else_group)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_handler_if_group:
| p=pp_ifx_h hl=handler_seq
    { 
      mknode ~pvec:[1; List.length hl] $startpos $endpos (pp_if_group()) (p::hl)
    }
;
pp_handler_elif_group:
| p=pp_elif hl=handler_seq
    { 
      mknode ~pvec:[1; List.length hl] $startpos $endpos (_pp_elif_group p) (p::hl)
    }
;
pp_handler_else_group:
| p=pp_else hl=handler_seq
    { 
      mknode ~pvec:[1; List.length hl] $startpos $endpos (_pp_else_group p) (p::hl)
    }
;

pp_handler_if_section_broken:
| p=pp_handler_if_group_broken
    pl=pp_handler_elif_group_broken* p_opt=ioption(pp_handler_else_group_broken)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionHandler (p :: pl @ pl1 @ [pe])
    }
;
pp_handler_if_group_broken:
| p=pp_ifx_h c=CATCH TY_LPAREN e=exception_declaration RPAREN
    { 
      ignore c;
      env#register_exc_decl e;
      let n_ = mknode ~pvec:[1; 0] $startpos(c) $endpos L.Handler [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[n_])
    }
| p=pp_ifx_h
    i=IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    { 
      ignore i;
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      let n_ = mknode ~pvec $startpos(i) $endpos L.IfStatement (cl @ il @ [c]) in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) (p::[n_])
    }
;
pp_handler_elif_group_broken:
| p=pp_elif c=CATCH TY_LPAREN e=exception_declaration RPAREN
    { 
      ignore c;
      env#register_exc_decl e;
      let n_ = mknode ~pvec:[1; 0] $startpos(c) $endpos L.Handler [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[n_])
    }
| p=pp_elif
    i=IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    { 
      ignore i;
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      let n_ = mknode ~pvec $startpos(i) $endpos L.IfStatement (cl @ il @ [c]) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) (p::[n_])
    }
;
pp_handler_else_group_broken:
| p=pp_else c=CATCH TY_LPAREN e=exception_declaration RPAREN
    { 
      ignore c;
      env#register_exc_decl e;
      let n_ = mknode ~pvec:[1; 0] $startpos(c) $endpos L.Handler [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[n_])
    }
| p=pp_else
    i=IF c_opt=ioption(constexpr) LPAREN i_opt=ioption(init_statement) c=condition RPAREN
    { 
      ignore i;
      let cl = opt_to_list c_opt in
      let il = opt_to_list i_opt in
      let pvec = [List.length cl; List.length il; 1; 0; 0] in
      let n_ = mknode ~pvec $startpos(i) $endpos L.IfStatement (cl @ il @ [c]) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) (p::[n_])
    }
;


exception_declaration:
| al_opt=attribute_specifier_seq_opt tl=type_specifier_seq d=declarator
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; List.length tl; 1] in
      mknode ~pvec $symbolstartpos $endpos L.ExceptionDeclaration (al @ tl @ [d])
    }
| al_opt=attribute_specifier_seq_opt tl=type_specifier_seq a_opt=ioption(abstract_declarator)
    { 
      let al = list_opt_to_list al_opt in
      let al1 = opt_to_list a_opt in
      let pvec = [List.length al; List.length tl; List.length al1] in
      mknode ~pvec $symbolstartpos $endpos L.ExceptionDeclaration (al @ tl @ al1)
    }
| ELLIPSIS { mkleaf $startpos $endpos L.Ellipsis }
;

abstract_declarator:
| p=ptr_abstract_declarator { p }
| n_opt=ioption(noptr_abstract_declarator) p=parameters_and_qualifiers t=trailing_return_type
    { 
      let nl = opt_to_list n_opt in
      let pvec = [List.length nl; 1; 1] in
      mknode ~pvec $symbolstartpos $endpos L.AbstractDeclaratorFunc (nl @ [p; t])
    }
| a=abstract_pack_declarator { a }
;

ptr_abstract_declarator:
| n=noptr_abstract_declarator { n }
| c_opt=ioption(calling_convention) p=ptr_operator p_opt=ioption(ptr_abstract_declarator)
    { 
      let cl = opt_to_list c_opt in
      let pl = opt_to_list p_opt in
      let pvec = [List.length cl; 1; List.length pl] in
      mknode ~pvec $startpos $endpos L.PtrAbstractDeclaratorPtr (cl@p::pl)
    }
;

noptr_abstract_declarator:
| n_opt=ioption(noptr_abstract_declarator) p=parameters_and_qualifiers
    { 
      let nl = opt_to_list n_opt in
      let pvec = [List.length nl; 1] in
      mknode ~pvec $symbolstartpos $endpos L.NoptrAbstractDeclaratorFunc (nl @ [p])
    }
| n_opt=ioption(noptr_abstract_declarator)
    LBRACKET c_opt=ioption(constant_expression) RBRACKET al_opt=attribute_specifier_seq_opt
    { 
      let nl = opt_to_list n_opt in
      let cl = opt_to_list c_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [List.length nl; List.length cl; List.length al] in
      mknode ~pvec $symbolstartpos $endpos L.NoptrAbstractDeclaratorArray (nl @ cl @ al)
    }
| TY_LPAREN p=ptr_abstract_declarator RPAREN
    { mknode $startpos $endpos L.NoptrAbstractDeclaratorParen [p] }
;

abstract_pack_declarator:
| n=noptr_abstract_pack_declarator { n }
| p=ptr_operator a=abstract_pack_declarator
    { mknode ~pvec:[1; 1] $startpos $endpos L.AbstractPackDeclarator [p; a] }
;

noptr_abstract_pack_declarator:
| n=noptr_abstract_pack_declarator p=parameters_and_qualifiers
    { mknode ~pvec:[1; 1] $startpos $endpos L.NoptrAbstractPackDeclaratorFunc [n; p] }
| n=noptr_abstract_pack_declarator
    LBRACKET c_opt=ioption(constant_expression) RBRACKET al_opt=attribute_specifier_seq_opt
    { 
      let cl = opt_to_list c_opt in
      let al = list_opt_to_list al_opt in
      let pvec = [1; List.length cl; List.length al] in
      mknode ~pvec $startpos $endpos L.NoptrAbstractPackDeclaratorArray (n :: cl @ al)
    }
| ELLIPSIS_ { mkleaf $startpos $endpos L.AbstractPack }
;

%inline
class_attr_spec_seq:
| { [] }
| al=attribute_specifier_seq { al }
| d=decl_spec_macro { [d] }
| d=decl_spec_macro_call { [d] }
| d=pp_decl_spec_if_section { [d] }
| al=attribute_specifier_seq d=decl_spec_macro { al @ [d] }
| al=attribute_specifier_seq d=decl_spec_macro_call { al @ [d] }
;

%inline
attribute_specifier_seq_opt:
| a_opt=ioption(attribute_specifier_seq) { a_opt }
;

attribute_specifier_seq:
| a=attribute_specifier { [a] }
| p=pp_attr_if_section { [p] }
| al=attribute_specifier_seq a=attribute_specifier { al @ [a] }
;

attribute_specifier:
| ATTR_LBRACKET LBRACKET a_opt=attribute_using_prefix_opt al1=attribute_list RBRACKET RBRACKET
    { 
      let al = opt_to_list a_opt in
      let pvec = [List.length al; List.length al1] in
      mknode ~pvec $startpos $endpos L.StandardAttributeSpecifier (al @ al1)
    }
| c=contract_attribute_specifier { c }
| a=alignment_specifier { a }
| g=gnu_attribute { g }
| u=OBJC_UNKNOWN { mkleaf $startpos $endpos (L.AttributeToken u) }
;

objc_available:
| OBJC_AVAILABLE TY_LPAREN al=clist(objc_avail_item) RPAREN { mknode $startpos $endpos L.ObjcAvailable al }
;

objc_avail_item:
| o=objc_identifier l=literal { mknode $startpos $endpos L.Attribute [o; l] }
| PTR_STAR { mkleaf $startpos $endpos L.Star }
;

(*%inline
gnu_attribute_seq_opt:
| a_opt=ioption(gnu_attribute_seq) { a_opt }
;*)

gnu_attribute_seq:
| a=gnu_attribute { [a] }
| al=gnu_attribute_seq a=gnu_attribute { al @ [a] }
;

gnu_attribute:
| a=GNU_ATTR LPAREN LPAREN al=attribute_list RPAREN RPAREN
    { 
      let n = mknode $startpos $endpos (L.GnuAttribute a) al in
      if al <> [] then begin
        (List.hd al)#add_prefix "((";
        (Xlist.last al)#add_suffix "))"
      end
      else
        n#add_suffix "(())";
      n
    }
| a=ATTR_MACRO { mkleaf $startpos $endpos (L.AttributeMacro a) }
| a=attr_macro_call { a }
;

attr_macro_call:
| i=IDENT_AM ml=macro_args { mknode $startpos $endpos (L.AttributeMacroInvocation i) ml }
;

alignment_specifier:
| ALIGNAS TY_LPAREN t=type_id opt=ioption(ELLIPSIS) RPAREN
    { mknode $startpos $endpos (L.AlignmentAttributeSpecifier (opt_to_bool opt)) [t] }
| ALIGNAS LPAREN c=constant_expression opt=ioption(ELLIPSIS) RPAREN
    { mknode $startpos $endpos (L.AlignmentAttributeSpecifier (opt_to_bool opt)) [c] }
;

%inline
attribute_using_prefix_opt:
| a_opt=ioption(attribute_using_prefix) { a_opt }
;

attribute_using_prefix:
| USING a=attribute_namespace COLON { mknode $startpos $endpos L.AttributeUsingPrefix [a] }
;

attribute_list:
| a_opt=ioption(attribute) { opt_to_list a_opt }
| al=attribute_list COMMA a_opt=ioption(attribute) { al @ (opt_to_list a_opt) }
| a=attribute ELLIPSIS
    { 
      let a_ = mknode $startpos $endpos L.PackExpansion [a] in
      a_#add_suffix "...";
      [a_]
    }
| al=attribute_list COMMA a=attribute ELLIPSIS
    { 
      let a_ = mknode $startpos(a) $endpos L.PackExpansion [a] in
      a_#add_suffix "...";
      al @ [a_]
    }
;

attribute:
| a=attribute_token a_opt=attribute_argument_clause_opt
    { 
      let al = opt_to_list a_opt in
      mknode ~pvec:[1; List.length al] $startpos $endpos L.Attribute (a::al)
    }
;

attribute_token:
| i=IDENT   { mkleaf $startpos $endpos (L.AttributeToken i) }
| i=IDENT_V { mkleaf $startpos $endpos (L.AttributeToken i) }
| CONST     { mkleaf $startpos $endpos L.Const }
| a=attribute_scoped_token { a }
;

attribute_scoped_token:
| al=attribute_namespace_seq i=IDENT
    { mknode $startpos $endpos (L.AttributeScopedToken i) al }
| al=attribute_namespace_seq i=IDENT_V
    { mknode $startpos $endpos (L.AttributeScopedToken i) al }
;

attribute_namespace_seq:
| a=attribute_namespace { [a] }
| al=attribute_namespace_seq a=attribute_namespace { al @ [a] }
;
%inline
attribute_namespace:
| i=IDENT COLON_COLON { mkleaf $startpos $endpos (L.AttributeNamespace i) }
;

attribute_argument_clause_opt:
| a_opt=ioption(attribute_argument_clause) { a_opt }
;

attribute_argument_clause:
| LPAREN b=balanced_token_seq_opt RPAREN
    { mknode $startpos $endpos L.AttributeArgumentClause (list_opt_to_list b) }
| TY_LPAREN b=balanced_token_seq_opt RPAREN
    { mknode $startpos $endpos L.AttributeArgumentClause (list_opt_to_list b) }
| i=IDENT_V { mknode $startpos $endpos (L.AttributeArgumentClauseMacro i) [] }
;

balanced_token_seq_opt:
| b_opt=ioption(balanced_token_seq) { b_opt }
;

balanced_token_seq:
| b=balanced_token { [b] }
| bl=balanced_token_seq b=balanced_token { bl @ [b] }
;

balanced_token:
| LPAREN     b_opt=balanced_token_seq_opt RPAREN
    { mknode $startpos $endpos L.BalancedTokenParen (list_opt_to_list b_opt) }
| TY_LPAREN  b_opt=balanced_token_seq_opt RPAREN
    { mknode $startpos $endpos L.BalancedTokenParen (list_opt_to_list b_opt) }
| LBRACKET   b_opt=balanced_token_seq_opt RBRACKET
    { mknode $startpos $endpos L.BalancedTokenBracket (list_opt_to_list b_opt) }
| LBRACE     b_opt=balanced_token_seq_opt RBRACE
    { mknode $startpos $endpos L.BalancedTokenBrace (list_opt_to_list b_opt) }
| t=token_no_paren
    { mkleaf $startpos $endpos (L.BalancedTokenSingle ((*Obj.repr*)Token.to_repr t)) }
;

contract_attribute_specifier:
| ATTR_LBRACKET LBRACKET
    EXPECTS c_opt=contract_level_opt COLON c=conditional_expression RBRACKET RBRACKET
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 1] in
      mknode ~pvec $startpos $endpos L.ContractAttributeSpecifierExpects (cl @ [c])
    }
| ATTR_LBRACKET LBRACKET
    ENSURES c_opt=contract_level_opt         COLON c=conditional_expression RBRACKET RBRACKET
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 1] in
      mknode ~pvec $startpos $endpos (L.ContractAttributeSpecifierEnsures "") (cl @ [c])
    }
| ATTR_LBRACKET LBRACKET
    ENSURES c_opt=contract_level_opt i=IDENT COLON c=conditional_expression RBRACKET RBRACKET
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 1] in
      mknode ~pvec $startpos $endpos (L.ContractAttributeSpecifierEnsures i) (cl @ [c])
    }
| ATTR_LBRACKET LBRACKET
    ASSERT c_opt=contract_level_opt COLON c=conditional_expression RBRACKET RBRACKET
    { 
      let cl = opt_to_list c_opt in
      let pvec = [List.length cl; 1] in
      mknode ~pvec $startpos $endpos L.ContractAttributeSpecifierAssert (cl @ [c])
    }
;

contract_level_opt:
| c_opt=ioption(contract_level) { c_opt }
;

contract_level:
| DEFAULT { mkleaf $startpos $endpos L.ContractLevelDefault }
| AUDIT   { mkleaf $startpos $endpos L.ContractLevelAudit }
| AXIOM   { mkleaf $startpos $endpos L.ContractLevelAxiom }
;

requires_clause:
| REQUIRES c=constraint_logical_or_expression { mknode $startpos $endpos L.RequiresClause [c] }
;

noexcept_specifier:
| NOEXCEPT LPAREN c=constant_expression RPAREN
    { mknode ~pvec:[1] $startpos $endpos L.NoexceptSpecifier [c] }
| NOEXCEPT { mkleaf ~pvec:[0] $startpos $endpos L.NoexceptSpecifier } %prec PREC
| THROW TY_LPAREN RPAREN { mkleaf $startpos $endpos L.NoexceptSpecifierThrow } (* ~C++17 *)
| THROW TY_LPAREN tl=clist(type_id) RPAREN
    { mknode $startpos $endpos L.NoexceptSpecifierThrow tl } (* ~C++11 *)
| THROW TY_LPAREN ELLIPSIS RPAREN { mkleaf $startpos $endpos L.NoexceptSpecifierThrowAny } (* MSVC++ *)
| n=NOEXCEPT_MACRO { mkleaf $startpos $endpos (L.NoexceptSpecifierMacro n) }
;

constraint_logical_or_expression:
| c=constraint_logical_and_expression { c }
| c=constraint_logical_or_expression b=BAR_BAR ca=constraint_logical_and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.ConstraintLogicalOrExpression b) [c; ca] }
;

constraint_logical_and_expression:
| p=primary_expression { p }
| c=constraint_logical_and_expression a=AMP_AMP p=primary_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.ConstraintLogicalAndExpression a) [c; p] }
;

id_expression:
| u=unqualified_id { u }
| q=qualified_id   { q }
;

postfix_expression:
| p=primary_expression { p }
| p=postfix_expression LBRACKET e=expr_or_braced_init_list RBRACKET
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionSubscr [p; e] }
| p=postfix_expression LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      if el_opt <> None then begin
        p#add_suffix "(";
        (Xlist.last el)#add_suffix ")"
      end
      else
        p#add_suffix "()";
      mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (p::el)
    }

| p=postfix_expression LPAREN el_opt=expression_list_opt pp=pp_args_if_section_closing
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; (List.length el) + 1] in
      p#add_suffix "(";
      mknode ~pvec $startpos $endpos L.PostfixExpressionFunCall (p::el@[pp])
    }

| p=pp_p_if_section LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      if el_opt <> None then begin
        p#add_suffix "(";
        (Xlist.last el)#add_suffix ")"
      end
      else
        p#add_suffix "()";
      mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (p::el)
    }
| p=pp_expr_if_section_broken c_opt=ioption(COMMA) el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      if el_opt <> None then begin
        if c_opt <> None then
          p#add_suffix ",";
        (Xlist.last el)#add_suffix ")"
      end
      else begin
        if c_opt <> None then
          p#add_suffix ",)"
        else
          p#add_suffix ")"
      end;
      mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (p::el)
    }
| i=IDENT_LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      mknode $startpos $endpos (L.PostfixExpressionFunCallMacro i) el
    }
| p=postfix_expression i=EMPTY_MACRO LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el] in
      mknode ~pvec $startpos $endpos (L.PostfixExpressionFunCallGuarded i) (p::el)
    }
| p=postfix_expression c=cuda_exec_config LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      mknode ~pvec:[1; 1; List.length el] $startpos $endpos L.CudaKernelCall (p::c::el)
    }
| s=simple_type_specifier LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el] in
      let lab =
        match s#label with
        | L.SimpleTypeSpecifier x when env#scanner_keep_flag && s#nchildren = 0 -> begin
            s#relab (L.Identifier s#get_name);
            L.PostfixExpressionFunCall
        end
        | _ -> L.PostfixExpressionExplicitTypeConvExpr
      in
      mknode ~pvec $startpos $endpos lab (s::el)
    }
| t=typename_specifier LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionExplicitTypeConvExpr (t::el)
    }
| s=simple_type_specifier b=braced_init_list
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionExplicitTypeConvBraced [s; b] }
| t=typename_specifier b=braced_init_list
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionExplicitTypeConvBraced [t; b] }
| p=postfix_expression DOT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionDot (p :: tl @ [i])
    }
| p=postfix_expression MINUS_GT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (p :: tl @ [i])
    }
| p=postfix_expression MINUS_GT t_opt=ioption(template) DUMMY_EXPR
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 0] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (p :: tl)
    }
| DUMMY_EXPR t=template i=id_expression
    { 
      let pvec = [0; 1; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpression [t; i]
    }
| p=pp_a_if_section DOT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionDot (p :: tl @ [i])
    }
| p=pp_a_if_section MINUS_GT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (p :: tl @ [i])
    }
| p=postfix_expression lab=dot_or_arrow t_opt=ioption(template) op=OP_MACRO i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 0] in
      let e = mknode ~pvec $startpos $endpos lab (p :: tl) in
      mknode $startpos $endpos (L.OperatorMacro op) [e; i]
    }
| p=postfix_expression PLUS_PLUS
    { mknode $startpos $endpos L.PostfixExpressionIncr [p] }
| p=postfix_expression MINUS_MINUS
    { mknode $startpos $endpos L.PostfixExpressionDecr [p] }
| c=cast_key TEMPL_LT t=type_id TEMPL_GT LPAREN e=expression RPAREN
    { 
      e#add_prefix "(";
      e#add_suffix ")";
      mknode ~pvec:[1; 1] $startpos $endpos c [t; e]
    }
| c=cast_key TEMPL_LT t=type_id TEMPL_GT i=identifier
    { mknode ~pvec:[1; 1] $startpos $endpos c [t; i] }
| TYPEID LPAREN e=expression RPAREN
    { mknode $startpos $endpos L.PostfixExpressionTypeidExpr [e] }
| TYPEID TY_LPAREN t=type_id RPAREN
    { mknode $startpos $endpos L.PostfixExpressionTypeidTy [t] }
| d=defined_macro_expression { d }
| h=has_include_expression { h }
| h=has_attribute_expression { h }
| e=expr_macro_call { e }
| p=postfix_expression a=args_macro
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionFunCall [p; a] }
| p=postfix_expression pp=pp_p_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpression [p; pp] }
| TY_LPAREN t=type_id RPAREN b=braced_init_list (* compound-literal *)
    { mknode ~pvec:[1; 1] $startpos $endpos L.CompoundLiteralExpression [t; b] }
;

%inline
dot_or_arrow:
| DOT      { L.PostfixExpressionDot }
| MINUS_GT { L.PostfixExpressionArrow }
;

pp_p_if_section:
| p=pp_p_if_group pl=list(pp_p_elif_group) p_opt=ioption(pp_p_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_p_if_group:
| p=pp_ifx_p da=dot_or_arrow e=postfix_expression ioption(COMMA) ioption(SEMICOLON)
    { 
      let e_ = mknode ~pvec:[0; 0; 1] $startpos(da) $endpos da [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e_]
    }
| p=pp_ifx_p e=postfix_expression ioption(COMMA) ioption(SEMICOLON)
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e] }
| p=pp_ifx_p SEMICOLON { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [p] }
;
pp_p_elif_group:
| p=pp_elif da=dot_or_arrow e=postfix_expression ioption(COMMA) ioption(SEMICOLON)
    { 
      let e_ = mknode ~pvec:[0; 0; 1] $startpos(da) $endpos da [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e_]
    }
| p=pp_elif e=postfix_expression ioption(COMMA) ioption(SEMICOLON)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e] }
| p=pp_elif SEMICOLON { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group p) [p] }
;
pp_p_else_group:
| p=pp_else da=dot_or_arrow e=postfix_expression ioption(COMMA) ioption(SEMICOLON)
    { 
      let e_ = mknode ~pvec:[0; 0; 1] $startpos(da) $endpos da [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e_]
    }
| p=pp_else e=postfix_expression ioption(COMMA) ioption(SEMICOLON)
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e] }
| p=pp_else SEMICOLON { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group p) [p] }
;


cuda_exec_config:
| CUDA_LT_LT_LT el=expression_list CUDA_GT_GT_GT
    { mknode $startpos $endpos L.CudaExecutionConfiguration el }
;

pp_args_if_section_closing:
| p=pp_args_if_group_closing
    pl=pp_args_elif_group_closing*
    p_opt=ioption(pp_args_else_group_closing)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_args_if_group_closing:
| p=pp_ifx_closing ioption(RPAREN) SEMICOLON
    { 
      let pvec = [1; 0] in
      mknode ~pvec $startpos $endpos (pp_if_group()) [p]
    }
| p=pp_ifx_closing el=expression_list RPAREN ioption(COMMA) ioption(SEMICOLON)
    { 
      let pvec = [1; List.length el] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::el)
    }
| p=pp_ifx_closing el=expression_list RPAREN SEMICOLON sl=statement_seq
    { 
      let pvec = [1; List.length el; List.length sl] in
      mknode ~pvec $startpos $endpos (pp_if_group()) (p::el@sl)
    }
;
mid_paren_close:
| { env#pstat#open_paren_arg() }
;
pp_args_elif_group_closing:
| p=pp_elif SEMICOLON
    { 
      let pvec = [1; 0] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p]
    }
| p=pp_elif mid_paren_close RPAREN SEMICOLON
    { 
      let pvec = [1; 0] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) [p]
    }
| p=pp_elif mid_paren_close el=expression_list RPAREN ioption(COMMA) ioption(SEMICOLON)
    { 
      let pvec = [1; List.length el] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p::el)
    }
| p=pp_elif mid_paren_close el=expression_list RPAREN SEMICOLON sl=statement_seq
    { 
      let pvec = [1; List.length el; List.length sl] in
      mknode ~pvec $startpos $endpos (_pp_elif_group p) (p::el@sl)
    }
;
pp_args_else_group_closing:
| p=pp_else SEMICOLON
    { 
      let pvec = [1; 0] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p]
    }
| p=pp_else mid_paren_close RPAREN SEMICOLON
    { 
      let pvec = [1; 0] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) [p]
    }
| p=pp_else mid_paren_close el=expression_list RPAREN ioption(COMMA) ioption(SEMICOLON)
    { 
      let pvec = [1; List.length el] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p::el)
    }
| p=pp_else mid_paren_close el=expression_list RPAREN SEMICOLON sl=statement_seq
    { 
      let pvec = [1; List.length el; List.length sl] in
      mknode ~pvec $startpos $endpos (_pp_else_group p) (p::el@sl)
    }
;

%inline
arg_macro:
| a=ARG_MACRO { mkleaf $startpos $endpos (L.ArgumentsMacro a) }
| i=IDENT_AGM ml=macro_args { mknode $startpos $endpos (L.ArgumentsMacroInvocation i) ml }
;

%inline
args_macro:
| a=ARGS_MACRO { mkleaf $startpos $endpos (L.ArgumentsMacro a) }
| i=IDENT_AM ml=macro_args { mknode $startpos $endpos (L.ArgumentsMacroInvocation i) ml }
;

%inline
template:
| TEMPLATE { mkleaf $startpos $endpos L.Template }
;

expr_macro_call:
| i=IDENT_EM ml=macro_args { mknode $startpos $endpos (L.ExpressionMacroInvocation i) ml }
| i=IDENT_EM SS_LPAREN sl=statement_seq RPAREN { mknode $startpos $endpos (L.ExpressionMacroInvocation i) sl }
| i=IDENT_EM SS_LPAREN sl=statement_seq COMMA ioption(MARKER) statement_seq RPAREN
    { mknode $startpos $endpos (L.ExpressionMacroInvocation i) sl }
;

defined_macro_expression:
| DEFINED i=IDENT_V               { mkleaf $startpos $endpos (L.DefinedMacroExpression i) }
| DEFINED LPAREN i=IDENT_V RPAREN { mkleaf $startpos $endpos (L.DefinedMacroExpression i) }
| DEFINED i=IDENT                 { mkleaf $startpos $endpos (L.DefinedMacroExpression i) }
| DEFINED LPAREN i=IDENT RPAREN   { mkleaf $startpos $endpos (L.DefinedMacroExpression i) }
| DEFINED HAS_INCLUDE
    { mkleaf $startpos $endpos (L.DefinedMacroExpression "__has_include") }
| DEFINED LPAREN HAS_INCLUDE RPAREN
    { mkleaf $startpos $endpos (L.DefinedMacroExpression "__has_include") }
;

has_include_expression:
| HAS_INCLUDE LPAREN sl=header_name_token+ RPAREN
    { mkleaf $startpos $endpos (L.HasIncludeExpression (String.concat "" sl)) }
| HAS_INCLUDE LPAREN s=STR_LITERAL RPAREN
    { mkleaf $startpos $endpos (L.HasIncludeExpression s) }
;
%inline
header_name_token:
| i=IDENT     { i }
| i=IDENT_V   { i }
| SLASH       { "/" }
| LT          { "<" }
| TEMPL_LT    { "<" }
| TY_TEMPL_GT { ">" }
| TEMPL_GT    { ">" }
| GT          { ">" }
;

has_attribute_expression:
| HAS_CPP_ATTRIBUTE LPAREN al=attribute_list RPAREN
    { mknode $startpos $endpos L.HasAttributeExpression al }
;

expr_or_braced_init_list:
| e=expression { e }
| b=braced_init_list { b }
;

cast_key:
| DYNAMIC_CAST     { L.PostfixExpressionDynamic_cast }
| STATIC_CAST      { L.PostfixExpressionStatic_cast }
| REINTERPRET_CAST { L.PostfixExpressionReinterpret_cast }
| CONST_CAST       { L.PostfixExpressionConst_cast }
;

expression_list_opt:
| il_opt=ioption(initializer_list) { il_opt }
;

expression_list:
| il=initializer_list { il }
;

initializer_list:
| il=_initializer_list { List.rev il }
;

_initializer_list:
| il=__initializer_list c_opt=ioption(COMMA)
    { 
      if c_opt <> None then
        (List.hd il)#add_suffix ",";
      il
    }
;
__initializer_list:
| i=initializer_clause { [i] }
| i=init_pack          { [i] }
| t=typename_specifier { [t] } (* for FOREACH-like macro *)
| p=pp_control_line    { [p] }
| p=pp_init_if_section { [p] }
| a=arg_macro          { [a] }

| a=arg_macro i=initializer_clause { [i; a] }
| a=arg_macro i=init_pack          { [i; a] }
| a=arg_macro t=typename_specifier { [t; a] }

| il=__initializer_list COMMA i=initializer_clause { (List.hd il)#add_suffix ","; i::il }
| il=__initializer_list COMMA i=designated_initializer_clause { (List.hd il)#add_suffix ","; i::il } (* extension? *)
| il=__initializer_list COMMA i=init_pack          { (List.hd il)#add_suffix ","; i::il }
| il=__initializer_list COMMA t=typename_specifier { (List.hd il)#add_suffix ","; t::il } (* for FOREACH-like macro *)

| il=_initializer_list p=pp_control_line    { p::il }
| il=_initializer_list p=pp_init_if_section { p::il }
| il=__initializer_list a=arg_macro         { a::il }

| il=__initializer_list a=arg_macro i=initializer_clause { i::a::il }
| il=__initializer_list a=arg_macro i=init_pack          { i::a::il }
| il=__initializer_list a=arg_macro t=typename_specifier { t::a::il }

| il=__initializer_list COMMA a=arg_macro i=initializer_clause { (List.hd il)#add_suffix ","; i::a::il }
| il=__initializer_list COMMA a=arg_macro i=init_pack          { (List.hd il)#add_suffix ","; i::a::il }
| il=__initializer_list COMMA a=arg_macro t=typename_specifier { (List.hd il)#add_suffix ","; t::a::il }
| il=__initializer_list COMMA a0=arg_macro a1=arg_macro        { (List.hd il)#add_suffix ","; a1::a0::il }

| p=pp_control_line i=initializer_clause { [i; p] }
| p=pp_control_line i=init_pack          { [i; p] }
| il=_initializer_list p=pp_control_line i=initializer_clause { i::p::il }
| il=_initializer_list p=pp_control_line i=init_pack          { i::p::il }

| p=pp_init_if_section i=initializer_clause { [i; p] }
| p=pp_init_if_section i=init_pack          { [i; p] }
| il=_initializer_list p=pp_init_if_section i=initializer_clause { i::p::il }
| il=_initializer_list p=pp_init_if_section i=init_pack          { i::p::il }
;

%inline
init_pack:
| i=initializer_clause ELLIPSIS
    { 
      let i_ = mknode $startpos $endpos L.PackExpansion [i] in
      i_#add_suffix "...";
      i_
    }
;

initializer_clause:
| a=assignment_expression { a }
| b=braced_init_list { b }
| p=pp_ifx_e t=typename pe=pp_endif a=assignment_expression
    { 
      env#exit_typename();
      let g_ = mknode $startpos $endpos(t) (_pp_if_group pe) [p; t] in
      let s_ = mknode $startpos $endpos(pe) (L.PpIfSection(0, get_pp_if_cond pe)) [g_; pe] in
      mknode $startpos $endpos L.InitializerClause [s_; a]
    }
;

expression:
| a=assignment_expression { a }
| e=expression COMMA a=assignment_expression
    { mknode ~pvec:[1; 1] $startpos $endpos L.ExpressionPair [e; a] }
| p=pp_expr_if_section { p }
| e=expression COMMA p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos L.ExpressionPair [e; p] }
| e=expression p=pp_init_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos L.ExpressionPair [e; p] }
;

pp_init_if_section:
| p=pp_init_if_group pl=list(pp_init_elif_group) p_opt=ioption(pp_init_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_init_if_group:
| p=pp_ifx_i c_opt=ioption(COMMA) il=initializer_list s_opt=ioption(SEMICOLON)
    { 
      begin
        match c_opt with
        | Some _ -> (List.hd il)#add_prefix ","
        | _ -> ()
      end;
      begin
        match s_opt with
        | Some b -> (*env#set_semicolon_info();*) if b then (Xlist.last il)#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; List.length il] $startpos $endpos (pp_if_group()) (p::il)
    }
(*| p=pp_ifx_i dl=designated_initializer_list ioption(COMMA)
    { mknode ~pvec:[1; List.length dl] $startpos $endpos (pp_if_group()) (p::dl) }*)
| p=pp_ifx_i { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [p] }
| p=pp_ifx_i e=EQ i=initializer_clause s_opt=ioption(SEMICOLON)
    { 
      ignore e;
      begin
        match s_opt with
        | Some b -> (*env#set_semicolon_info();*) if b then i#add_suffix ";"
        | _ -> ()
      end;
      let i_ = mknode $startpos(e) $endpos L.EqualInitializer [i] in
      i_#add_prefix "= ";
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; i_]
    }
;
pp_init_elif_group:
| p=pp_elif c_opt=ioption(COMMA) il=initializer_list s_opt=ioption(SEMICOLON)
    { 
      begin
        match c_opt with
        | Some _ -> (List.hd il)#add_prefix ","
        | _ -> ()
      end;
      begin
        match s_opt with
        | Some b -> (*env#set_semicolon_info();*) if b then (Xlist.last il)#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; List.length il] $startpos $endpos (_pp_elif_group p) (p::il)
    }
| p=pp_elif dl=designated_initializer_list c_opt=ioption(COMMA)
    { 
      begin
        match c_opt with
        | Some _ -> (Xlist.last dl)#add_suffix ","
        | _ -> ()
      end;
      mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_elif_group p) (p::dl)
    }
| p=pp_elif { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group p) [p] }
| p=pp_elif e=EQ i=initializer_clause s_opt=ioption(SEMICOLON)
    { 
      ignore e;
      begin
        match s_opt with
        | Some b -> (*env#set_semicolon_info();*) if b then i#add_suffix ";"
        | _ -> ()
      end;
      let i_ = mknode $startpos(e) $endpos L.EqualInitializer [i] in
      i_#add_prefix "= ";
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; i_]
    }
;
pp_init_else_group:
| p=pp_else c_opt=ioption(COMMA) il=initializer_list s_opt=ioption(SEMICOLON)
    { 
      begin
        match c_opt with
        | Some _ -> (List.hd il)#add_prefix ","
        | _ -> ()
      end;
      begin
        match s_opt with
        | Some b -> (*env#set_semicolon_info();*) if b then (Xlist.last il)#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; List.length il] $startpos $endpos (_pp_else_group p) (p::il)
    }
| p=pp_else dl=designated_initializer_list c_opt=ioption(COMMA)
    { 
      begin
        match c_opt with
        | Some _ -> (Xlist.last dl)#add_suffix ","
        | _ -> ()
      end;
      mknode ~pvec:[1; List.length dl] $startpos $endpos (_pp_else_group p) (p::dl)
    }
| p=pp_else { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group p) [p] }
| p=pp_else e=EQ i=initializer_clause s_opt=ioption(SEMICOLON)
    { 
      ignore e;
      begin
        match s_opt with
        | Some b -> (*env#set_semicolon_info();*) if b then i#add_suffix ";"
        | _ -> ()
      end;
      let i_ = mknode $startpos(e) $endpos L.EqualInitializer [i] in
      i_#add_prefix "= ";
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; i_]
    }
;

pp_expr_if_section:
| p=pp_expr_if_group pl=list(pp_expr_elif_group) p_opt=ioption(pp_expr_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_expr_if_group:
| p=pp_ifx_e pl=pp_control_line+
    { mknode ~pvec:[1; List.length pl] $startpos $endpos (pp_if_group()) (p::pl) }

| p=pp_ifx_e e=expression pl=pp_control_line+
    { mknode ~pvec:[1; List.length pl + 1] $startpos $endpos (pp_if_group()) (p::e::pl) }

| p=pp_ifx_e c0_opt=ioption(COMMA) e=expression c1_opt=ioption(COMMA) s_opt=ioption(SEMICOLON)
    { 
      if c0_opt <> None then e#add_prefix ",";
      if c1_opt <> None then e#add_suffix ",";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e]
    }
| p=pp_ifx_e e=braced_init_list c_opt=ioption(COMMA) s_opt=ioption(SEMICOLON)
    { 
      if c_opt <> None then e#add_suffix ",";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e]
    }
| p=pp_ifx_e e=expression sc=SEMICOLON sl=statement_seq
    { 
      if sc then e#add_suffix ";";
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos (pp_if_group()) ([p; e]@sl)
    }
| p=pp_ifx_e e=expression sc=SEMICOLON sl=statement_seq o=odd_stmt
    { 
      if sc then e#add_suffix ";";
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos (pp_if_group()) ([p; e]@sl@[o])
    }
(*| p=pp_ifx_e e=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e] }*)
;
pp_expr_elif_group:
| p=pp_elif pl=pp_control_line+
    { mknode ~pvec:[1; List.length pl] $startpos $endpos (_pp_elif_group p) (p::pl) }

| p=pp_elif e=expression pl=pp_control_line+
    { mknode ~pvec:[1; List.length pl + 1] $startpos $endpos (_pp_elif_group p) (p::e::pl) }

| p=pp_elif c0_opt=ioption(COMMA) e=expression c1_opt=ioption(COMMA) s_opt=ioption(SEMICOLON)
    { 
      if c0_opt <> None then e#add_prefix ",";
      if c1_opt <> None then e#add_suffix ",";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e]
    }
| p=pp_elif e=braced_init_list c_opt=ioption(COMMA) s_opt=ioption(SEMICOLON)
    { 
      if c_opt <> None then e#add_suffix ",";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e]
    }
| p=pp_elif e=expression sc=SEMICOLON sl=statement_seq
    { 
      if sc then e#add_suffix ";";
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos (_pp_elif_group p) ([p; e]@sl)
    }
| p=pp_elif e=expression sc=SEMICOLON sl=statement_seq o=odd_stmt
    { 
      if sc then e#add_suffix ";";
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos (_pp_elif_group p) ([p; e]@sl@[o])
    }
(*| p=pp_elif e=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e] }*)
;
pp_expr_else_group:
| p=pp_else pl=pp_control_line+
    { mknode ~pvec:[1; List.length pl] $startpos $endpos (_pp_else_group p) (p::pl) }

| p=pp_else e=expression pl=pp_control_line+
    { mknode ~pvec:[1; List.length pl + 1] $startpos $endpos (_pp_else_group p) (p::e::pl) }

| p=pp_else c0_opt=ioption(COMMA) e=expression c1_opt=ioption(COMMA) s_opt=ioption(SEMICOLON)
    { 
      if c0_opt <> None then e#add_prefix ",";
      if c1_opt <> None then e#add_suffix ",";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e]
    }
| p=pp_else e=braced_init_list c_opt=ioption(COMMA) s_opt=ioption(SEMICOLON)
    { 
      if c_opt <> None then e#add_suffix ",";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e]
    }
| p=pp_else e=expression sc=SEMICOLON sl=statement_seq
    { 
      if sc then e#add_suffix ";";
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos (_pp_else_group p) ([p; e]@sl)
    }
| p=pp_else e=expression sc=SEMICOLON sl=statement_seq o=odd_stmt
    { 
      if sc then e#add_suffix ";";
      mknode ~pvec:[1; 1; List.length sl] $startpos $endpos (_pp_else_group p) ([p; e]@sl@[o])
    }
(*| p=pp_else e=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e] }*)
;

pp_expr_if_section_broken:
| p=pp_expr_if_group_broken
    pl=pp_expr_elif_group_broken*
    p_opt=ioption(pp_expr_else_group_broken)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionBrokenIf (p :: pl @ pl1 @ [pe])
    }
;
pp_expr_if_group_broken:
| pi=pp_ifx_e p=postfix_expression LPAREN el_opt=expression_list_opt
    { 
      let el = list_opt_to_list el_opt in
      p#add_suffix "(";
      let e_ = mknode ~pvec:[1; List.length el] $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; e_]
    }
;
pp_expr_elif_group_broken:
| pi=pp_elif p=postfix_expression LPAREN el_opt=expression_list_opt
    { 
      env#pstat#close_paren();
      let el = list_opt_to_list el_opt in
      p#add_suffix "(";
      let e_ = mknode ~pvec:[1; List.length el] $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group pi) [pi; e_]
    }
;
pp_expr_else_group_broken:
| pi=pp_else p=postfix_expression LPAREN el_opt=expression_list_opt
    { 
      env#pstat#close_paren();
      let el = list_opt_to_list el_opt in
      p#add_suffix "(";
      let e_ = mknode ~pvec:[1; List.length el] $startpos(p) $endpos L.PostfixExpressionFunCall (p::el) in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group pi) [pi; e_]
    }
;

%inline
broken_expr:
| l=lambda_introducer { l }
;


pp_cond_if_section:
| p=pp_cond_if_group pl=pp_cond_elif_group* p_opt=ioption(pp_cond_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionCondExpr (p :: pl @ pl1 @ [pe])
    }
;
pp_cond_if_group:
| p=pp_ifx_cond q=QUEST e=expression
    { 
      ignore q;
      e#add_prefix "? ";
      let e_ = mknode ~pvec:[0; 1; 0] $startpos(q) $endpos L.ConditionalExpression [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e_]
    }
| p=pp_ifx_cond q=QUEST e=expression COLON a=assignment_expression
    { 
      ignore q;
      e#add_prefix "? ";
      e#add_suffix " :";
      let e_ = mknode ~pvec:[0; 1; 1] $startpos(q) $endpos L.ConditionalExpression [e; a] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e_]
    }
;
pp_cond_elif_group:
| p=pp_elif q=QUEST e=expression
    { 
      ignore q;
      e#add_prefix "? ";
      let e_ = mknode ~pvec:[0; 1; 0] $startpos(q) $endpos L.ConditionalExpression [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e_]
    }
| p=pp_elif q=QUEST e=expression COLON a=assignment_expression
    { 
      ignore q;
      e#add_prefix "? ";
      e#add_suffix " :";
      let e_ = mknode ~pvec:[0; 1; 1] $startpos(q) $endpos L.ConditionalExpression [e; a] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e_]
    }
;
pp_cond_else_group:
| p=pp_else q=QUEST e=expression
    { 
      ignore q;
      e#add_prefix "? ";
      let e_ = mknode ~pvec:[0; 1; 0] $startpos(q) $endpos L.ConditionalExpression [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e_]
    }
| p=pp_else q=QUEST e=expression COLON a=assignment_expression
    { 
      ignore q;
      e#add_prefix "? ";
      e#add_suffix " :";
      let e_ = mknode ~pvec:[0; 1; 1] $startpos(q) $endpos L.ConditionalExpression [e; a] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e_]
    }
;

pp_cond_tl_if_section:
| p=pp_cond_tl_if_group pl=pp_cond_tl_elif_group* p_opt=ioption(pp_cond_tl_else_group)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionCondExpr (p :: pl @ pl1 @ [pe])
    }
;
pp_cond_tl_if_group:
| p=pp_ifx_cond_ c=COLON e=assignment_expression s_opt=ioption(SEMICOLON)
    { 
      ignore c;
      begin
        match s_opt with
        | Some _ -> env#set_semicolon_info()
        | _ -> ()
      end;
      e#add_prefix ": ";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      let e_ = mknode ~pvec:[0; 0; 1] $startpos(c) $endpos(e) L.ConditionalExpression [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e_]
    }
| p=pp_ifx_cond_ c=COLON l=logical_or_expression QUEST e=expression
    { 
      ignore c;
      l#add_prefix ": ";
      l#add_suffix " ?";
      let e0_ = mknode ~pvec:[1; 1; 0] $startpos(l) $endpos L.ConditionalExpression [l ;e] in
      let e1_ = mknode ~pvec:[0; 0; 1] $startpos(c) $endpos L.ConditionalExpression [e0_] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e1_]
    }
;
pp_cond_tl_elif_group:
| p=pp_elif c=COLON e=assignment_expression s_opt=ioption(SEMICOLON)
    { 
      ignore c;
      begin
        match s_opt with
        | Some _ -> env#set_semicolon_info()
        | _ -> ()
      end;
      e#add_prefix ": ";
      begin
        match s_opt with
        | Some true -> e#add_suffix ";"
        | _ -> ()
      end;
      let e_ = mknode ~pvec:[0; 0; 1] $startpos(c) $endpos(e) L.ConditionalExpression [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e_]
    }
| p=pp_elif c=COLON l=logical_or_expression QUEST e=expression
    { 
      ignore c;
      l#add_prefix ": ";
      l#add_suffix " ?";
      let e0_ = mknode ~pvec:[1; 1; 0] $startpos(l) $endpos L.ConditionalExpression [l ;e] in
      let e1_ = mknode ~pvec:[0; 0; 1] $startpos(c) $endpos L.ConditionalExpression [e0_] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e1_]
    }
;
pp_cond_tl_else_group:
| p=pp_else c=COLON e=assignment_expression s_opt=ioption(SEMICOLON)
    { 
      ignore c;
      e#add_prefix ": ";
      begin
        match s_opt with
        | Some b -> env#set_semicolon_info(); if b then e#add_suffix ";"
        | _ -> ()
      end;
      let e_ = mknode ~pvec:[0; 0; 1] $startpos(c) $endpos(e) L.ConditionalExpression [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e_]
    }
| p=pp_else c=COLON l=logical_or_expression QUEST e=expression
    { 
      ignore c;
      l#add_prefix " :";
      l#add_suffix " ?";
      let e0_ = mknode ~pvec:[1; 1; 0] $startpos(l) $endpos L.ConditionalExpression [l ;e] in
      let e1_ = mknode ~pvec:[0; 0; 1] $startpos(c) $endpos L.ConditionalExpression [e0_] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e1_]
    }
;

assignment_expression:
| c=conditional_expression { c }

| l=logical_or_expression ao=assignment_operator i=initializer_clause
    { mknode ~pvec:[1; 1] $startpos $endpos ao [l; i] }

| l=logical_or_expression ao=assignment_operator a=asm
    { mknode ~pvec:[1; 1] $startpos $endpos ao [l; a] }

| t=throw_expression { t }
| y=yield_expression { y }

| l=logical_or_expression o=OP_MACRO i=initializer_clause
    { mknode ~pvec:[1; 1] $startpos $endpos (L.OperatorMacro o) [l; i] }

| l=logical_or_expression o=OP_MACRO EQ i=initializer_clause
    { 
      i#add_prefix "= ";
      mknode ~pvec:[1; 1] $startpos $endpos (L.OperatorMacro o) [l; i]
    }
| p=pp_a_if_section i=initializer_clause
    { mknode $startpos $endpos L.AssignmentExpression [p; i] }

| p=pp_a_if_section ao=assignment_operator i=initializer_clause
    { mknode ~pvec:[1; 1] $startpos $endpos ao [p; i] }
;

%inline
asm:
| ASM LPAREN el=expression_list RPAREN { mknode $startpos $endpos L.Asm el }
;

pp_a_if_section:
| p=pp_a_if_group pl=list(pp_a_elif_group) p_opt=ioption(pp_a_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_a_if_group:
| p=pp_ifx_a e=shift_expression ao=assignment_operator_
    { 
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos ao [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; a_]
    }
| p=pp_ifx_a e=postfix_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; e] }
;
pp_a_elif_group:
| p=pp_elif e=shift_expression ao=assignment_operator_
    { 
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos ao [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; a_]
    }
| p=pp_elif e=postfix_expression
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; e]
    }
;
pp_a_else_group:
| p=pp_else e=shift_expression ao=assignment_operator_
    { 
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos ao [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; a_]
    }
| p=pp_else e=postfix_expression
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; e]
    }
;

yield_expression:
| CO_YIELD a=assignment_expression { mknode $startpos $endpos L.YieldExpression [a] }
| CO_YIELD b=braced_init_list { mknode $startpos $endpos L.YieldExpression [b] }
;

throw_expression:
| THROW_                        { mknode $startpos $endpos L.ThrowExpression [] }
| THROW a=assignment_expression { mknode $startpos $endpos L.ThrowExpression [a] }
;

constant_expression:
| c=conditional_expression { c }
;

conditional_expression:
| l=logical_or_expression { l } %prec PREC

| l=logical_or_expression QUEST e=expression COLON a=assignment_expression
    { 
      l#add_suffix " ?";
      e#add_suffix " :";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [l; e; a]
    }
| p=pp_expr_if_section QUEST e=expression COLON a=assignment_expression
    { 
      p#add_suffix " ?";
      e#add_suffix " :";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [p; e; a]
    }
| l=logical_or_expression QUEST e=expression COLON p=pp_expr_if_section
    { 
      l#add_suffix " ?";
      e#add_suffix " :";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [l; e; p]
    }
| l=logical_or_expression QUEST COLON a=assignment_expression (* GNU extension *)
    { 
      l#add_suffix " ?:";
      mknode ~pvec:[1; 0; 1] $startpos $endpos L.ConditionalExpression [l; a]
    }
| l=logical_or_expression QUEST e=expression p=pp_cond_tl_if_section
    { 
      l#add_suffix " ?";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [l; e; p]
    }
| l=logical_or_expression QUEST e=expression p=pp_cond_tl_if_section
    QUEST e1=expression COLON a=assignment_expression
    { 
      p#add_children_r [a; e1];
      p#set_pvec (p#pvec@[1; 1]);
      _reloc $startpos(p) $endpos p;
      l#add_suffix " ?";
      e1#add_prefix "? ";
      e1#add_suffix " :";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [l; e; p]
    }
| l=logical_or_expression QUEST e=expression p=pp_cond_tl_if_section COND_MARKER
    COLON a=assignment_expression 
    { 
      p#add_children_r [a];
      p#set_pvec (p#pvec@[1]);
      _reloc $startpos(p) $endpos p;
      l#add_suffix " ?";
      a#add_prefix ": ";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [l; e; p]
    }
| l=logical_or_expression p=pp_cond_if_section COND_MARKER COLON a=assignment_expression
    { 
      p#add_children_l [l];
      p#add_children_r [a];
      p#set_pvec (1::p#pvec@[1]);
      reloc $startpos $endpos p
    }
| l=logical_or_expression p=pp_cond_if_section
    { 
      p#add_children_l [l];
      p#set_pvec (1::p#pvec);
      reloc $startpos $endpos p
    }
(*| pi=pp_ifx_e l=logical_or_expression QUEST e=expression c=COLON pe=pp_endif
    a=assignment_expression
    { 
      ignore c;
      let e0 = mknode ~pvec:[1; 1; 0] $startpos(l) $endpos(c) L.ConditionalExpression [l; e] in
      let ifg = mknode ~pvec:[1; 1] $startpos $endpos(c) (pp_if_group()) [pi; e0] in
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.PpIfSectionCondExpr [ifg; pe; a]
    }*)
| p=pp_cond_hd_if_section a=assignment_expression
    { 
      p#add_children_r [a];
      p#set_pvec (p#pvec@[1]);
      reloc $startpos $endpos p
    }
| p=pp_cond_hd_if_section pe=pp_expr_if_section
    { 
      p#add_children_r [pe];
      p#set_pvec (p#pvec@[1]);
      reloc $startpos $endpos p
    }
;

cond_unit_seq:
| l=logical_or_expression QUEST e=expression COLON
    { 
      l#add_suffix " ?";
      e#add_suffix " :";
      mknode ~pvec:[1; 1; 0] $startpos $endpos L.ConditionalExpression [l; e]
    }
| p=pp_cond_hd_if_section { p }
| l=logical_or_expression QUEST e=expression COLON c=cond_unit_seq
    { 
      l#add_suffix " ?";
      e#add_suffix " :";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [l; e; c]
    }
;

pp_cond_hd_if_section:
| p=pp_cond_hd_if_group pl=pp_cond_hd_elif_group* p_opt=ioption(pp_cond_hd_else_group)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionCondExpr (p :: pl @ pl1 @ [pe])
    }
;
pp_cond_hd_if_group:
| p=pp_ifx_e c=cond_unit_seq { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c] }
;
pp_cond_hd_elif_group:
| p=pp_elif c=cond_unit_seq { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c] }
;
pp_cond_hd_else_group:
| p=pp_else c=cond_unit_seq { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c] }
;

logical_or_expression:
| a=logical_and_expression { a }

| o=logical_or_expression b=BAR_BAR a=logical_and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalOrExpression b) [o; a] }

| o=logical_or_expression b=BAR_BAR e=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalOrExpression b) [o; e] }

| o=logical_or_expression p=_pp_lor_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalOrExpression "") [o; p] }

| o=logical_or_expression b=BAR_BAR ul=lor_unit_seq MARKER
    { 
      mknode ~pvec:[List.length ul + 1; 0] $startpos $endpos (L.LogicalOrExpression b) (o :: ul)
    }
| o=logical_or_expression b=BAR_BAR ul=lor_unit_seq a=logical_and_expression
    { 
      mknode ~pvec:[List.length ul + 1; 1] $startpos $endpos (L.LogicalOrExpression b) (o :: ul @ [a])
    }
| ul=lor_unit_seq a=logical_and_expression
    { 
      mknode ~pvec:[List.length ul; 1] $startpos $endpos (L.LogicalOrExpression "") (ul @ [a])
    }
| ul=lor_unit_seq p=_pp_lor_if_section
    { 
      mknode ~pvec:[List.length ul; 1] $startpos $endpos (L.LogicalOrExpression "") (ul @ [p])
    }
| o=logical_or_expression b=BAR_BAR p=pp_control_line ul=lor_unit_seq a=logical_and_expression
    { 
      mknode ~pvec:[List.length ul + 2; 1] $startpos $endpos (L.LogicalOrExpression b) (o :: p :: ul @ [a])
    }
;

lor_unit_seq:
| p=pp_lor_if_section { [p] }
| m=lor_macro_call    { [m] }
| ul=lor_unit_seq p=pp_lor_if_section  { ul @ [p] }
| ul=lor_unit_seq p=pp_control_line    { ul @ [p] }
| ul=lor_unit_seq p=pp_expr_if_section { ul @ [p] }
| ul=lor_unit_seq m=lor_macro_call     { ul @ [m] }
;

lor_macro_call:
| i=IDENT_LOM ml=macro_args { mknode $startpos $endpos (L.LogicalOrMacroInvocation i) ml }
;

_pp_lor_if_section:
| p=_pp_lor_if_group pl=list(_pp_lor_elif_group) p_opt=ioption(_pp_lor_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionLogicalOr (p :: pl @ pl1 @ [pe])
    }
;
_pp_lor_if_group:
| p=pp_ifx_o b=BAR_BAR o=logical_or_expression
    { 
      let o_ = mknode ~pvec:[0; 1] $startpos(b) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; o_]
    }
| p=pp_ifx_o pl=_pp_lor_if_section+ { mknode ~pvec:[1; List.length pl] $startpos $endpos (pp_if_group()) (p::pl) }
| p=pp_ifx_o pl=_pp_lor_if_section+ b=BAR_BAR o=logical_or_expression
    { 
      let o_ = mknode ~pvec:[0; 1] $startpos(b) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; List.length pl] $startpos $endpos (pp_if_group()) (p::pl@[o_])
    }
;
_pp_lor_elif_group:
| p=pp_elif b=BAR_BAR o=logical_or_expression
    { 
      let o_ = mknode ~pvec:[0; 1] $startpos(b) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; o_]
    }
| p=pp_elif pl=_pp_lor_if_section+ { mknode ~pvec:[1; List.length pl] $startpos $endpos (_pp_elif_group p) (p::pl) }
| p=pp_elif pl=_pp_lor_if_section+ b=BAR_BAR o=logical_or_expression
    { 
      let o_ = mknode ~pvec:[0; 1] $startpos(b) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; List.length pl] $startpos $endpos (_pp_elif_group p) (p::pl@[o_])
    }
;
_pp_lor_else_group:
| p=pp_else b=BAR_BAR o=logical_or_expression
    { 
      let o_ = mknode ~pvec:[0; 1] $startpos(b) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; o_]
    }
| p=pp_else pl=_pp_lor_if_section+ { mknode ~pvec:[1; List.length pl] $startpos $endpos (_pp_else_group p) (p::pl) }
| p=pp_else pl=_pp_lor_if_section+ b=BAR_BAR o=logical_or_expression
    { 
      let o_ = mknode ~pvec:[0; 1] $startpos(b) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; List.length pl] $startpos $endpos (_pp_elif_group p) (p::pl@[o_])
    }
;

pp_lor_if_section:
| p=pp_lor_if_group pl=list(pp_lor_elif_group) p_opt=ioption(pp_lor_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionLogicalOr (p :: pl @ pl1 @ [pe])
    }
;
pp_lor_if_group:
| p=pp_ifx_o o=logical_or_expression b_opt=ioption(BAR_BAR_BROKEN) ioption(SEMICOLON)
    { 
      let b = string_opt_to_string b_opt in
      let o_ = mknode ~pvec:[1; 0] $startpos(o) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; o_]
    }
| p=pp_ifx_o LPAREN o=logical_or_expression b_opt=ioption(BAR_BAR_BROKEN) ioption(SEMICOLON)
    { 
      let b = string_opt_to_string b_opt in
      let o_ = mknode ~pvec:[1; 0] $startpos(o) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; o_]
    }
;
pp_lor_elif_group:
| p=pp_elif o=logical_or_expression b_opt=ioption(BAR_BAR_BROKEN) ioption(SEMICOLON)
    { 
      let b = string_opt_to_string b_opt in
      let o_ = mknode ~pvec:[1; 0] $startpos(o) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; o_]
    }
;
pp_lor_else_group:
| p=pp_else o=logical_or_expression b_opt=ioption(BAR_BAR_BROKEN) ioption(SEMICOLON)
    { 
      let b = string_opt_to_string b_opt in
      let o_ = mknode ~pvec:[1; 0] $startpos(o) $endpos (L.LogicalOrExpression b) [o] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; o_]
    }
;

logical_and_expression:
| i=inclusive_or_expression { i }

| l=logical_and_expression a=AMP_AMP i=inclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalAndExpression a) [l; i] }

| l=logical_and_expression a=AMP_AMP e=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalAndExpression a) [l; e] }

| l=logical_and_expression p=_pp_land_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalAndExpression "") [l; p] }

| l=logical_and_expression a=AMP_AMP ul=land_unit_seq MARKER
    { 
      mknode ~pvec:[List.length ul + 1; 0] $startpos $endpos (L.LogicalAndExpression a) (l :: ul)
    }
| l=logical_and_expression a=AMP_AMP ul=land_unit_seq o=inclusive_or_expression
    { 
      mknode ~pvec:[List.length ul + 1; 1] $startpos $endpos (L.LogicalAndExpression a) (l :: ul @ [o])
    }
| ul=land_unit_seq o=inclusive_or_expression
    { 
      mknode ~pvec:[List.length ul; 1] $startpos $endpos (L.LogicalAndExpression "") (ul @ [o])
    }
(*| ul=land_unit_seq p=_pp_land_if_section
    { 
      mknode ~pvec:[List.length ul; 1] $startpos $endpos (L.LogicalAndExpression "") (ul @ [p])
    }*)
| l=logical_and_expression a=AMP_AMP p=pp_control_line ul=land_unit_seq o=inclusive_or_expression
    { 
      mknode ~pvec:[List.length ul + 2; 1] $startpos $endpos (L.LogicalAndExpression a) (l :: p :: ul @ [o])
    }
| l=logical_and_expression a=AMP_AMP p=pp_lor_if_section l0=logical_and_expression RPAREN MARKER
    pi=pp_ifx_e rp=RPAREN mid_paren_open pe=pp_endif
    { (* too ad hoc *)
      ignore rp;
      let rp_ = mkleaf $startpos(rp) $endpos(rp) L.Rparen in
      let ifg = mknode ~pvec:[1; 2] $startpos $endpos(rp) (_pp_if_group pe) [pi; rp_] in
      let ifs = mknode ~pvec:[1; 0; 0; 1] $startpos $endpos(pe) (L.PpIfSection(0, get_pp_if_cond pe)) [ifg; pe] in
      mknode ~pvec:[1; 1; 1; 1] $startpos $endpos (L.LogicalAndExpression a) [l; p; l0; ifs]
    }
;

mid_paren_open:
| { env#pstat#open_paren PK_NORMAL }
;

land_unit_seq:
| p=pp_land_if_section { [p] }
| ul=land_unit_seq p=pp_land_if_section  { ul @ [p] }
| ul=land_unit_seq p=pp_control_line    { ul @ [p] }
| ul=land_unit_seq p=pp_expr_if_section { ul @ [p] }
;

_pp_land_if_section:
| p=_pp_land_if_group pl=list(_pp_land_elif_group) p_opt=ioption(_pp_land_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionLogicalAnd (p :: pl @ pl1 @ [pe])
    }
;
_pp_land_if_group:
| p=pp_ifx_a a=AMP_AMP e=logical_and_expression
    { 
      let a_ = mknode ~pvec:[0; 1] $startpos(a) $endpos (L.LogicalAndExpression a) [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; a_]
    }

| p=pp_ifx_a a=AMP_AMP c=condition RPAREN l=LBRACE pe=postfix_expression LPAREN el=expression_list
    { 
      ignore l;
      let a_ = mknode ~pvec:[0; 1] $startpos(a) $endpos (L.LogicalAndExpression a) [c] in
      let e = mknode ~pvec:[1; List.length el] $startpos(pe) $endpos L.PostfixExpressionFunCall (pe::el) in
      let s = mknode $startpos(l) $endpos L.CompoundStatement [e] in
      s#add_prefix ")";
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; a_; s]
    }

| p=pp_ifx_a a_=_pp_land_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; a_] }
;
_pp_land_elif_group:
| p=pp_elif a=AMP_AMP e=logical_and_expression
    { 
      let a_ = mknode ~pvec:[0; 1] $startpos(a) $endpos (L.LogicalAndExpression a) [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; a_]
    }
| p=pp_elif a_=_pp_land_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; a_] }
;
_pp_land_else_group:
| p=pp_else a=AMP_AMP e=logical_and_expression
    { 
      let a_ = mknode ~pvec:[0; 1] $startpos(a) $endpos (L.LogicalAndExpression a) [e] in
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; a_]
    }

| p=pp_else RPAREN l=LBRACE pe=postfix_expression LPAREN el=expression_list
    { 
      ignore l;
      env#pstat#close_brace();
      let e = mknode ~pvec:[1; List.length el] $startpos(pe) $endpos L.PostfixExpressionFunCall (pe::el) in
      let s = mknode $startpos(l) $endpos L.CompoundStatement [e] in
      s#add_prefix ")";
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; s]
    }

| p=pp_else a_=_pp_land_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; a_] }

;

pp_land_if_section:
| p=pp_land_if_group pl=list(pp_land_elif_group) p_opt=ioption(pp_land_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos L.PpIfSectionLogicalAnd (p :: pl @ pl1 @ [pe])
    }
;
pp_land_if_group:
| p=pp_ifx_a e=logical_and_expression a=AMP_AMP_BROKEN
    { 
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos (L.LogicalAndExpression a) [e] in
      mknode ~pvec:[1; 1] $startpos $endpos(e) (pp_if_group()) [p; a_]
    }
| p=pp_ifx_a r=RETURN e=logical_and_expression a=AMP_AMP_BROKEN
    { 
      ignore r;
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos (L.LogicalAndExpression a) [e] in
      let r_ = mknode $startpos(r) $endpos L.ReturnStatement [a_] in
      mknode ~pvec:[1; 1] $startpos $endpos(e) (pp_if_group()) [p; r_]
    }
| p=pp_ifx_a pp=pp_land_if_section { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; pp] }
;
pp_land_elif_group:
| p=pp_elif e=logical_and_expression a=AMP_AMP_BROKEN
    { 
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos (L.LogicalAndExpression a) [e] in
      mknode ~pvec:[1; 1] $startpos $endpos(e) (_pp_elif_group p) [p; a_]
    }
| p=pp_elif r=RETURN e=logical_and_expression a=AMP_AMP_BROKEN
    { 
      ignore r;
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos (L.LogicalAndExpression a) [e] in
      let r_ = mknode $startpos(r) $endpos L.ReturnStatement [a_] in
      mknode ~pvec:[1; 1] $startpos $endpos(e) (_pp_elif_group p) [p; r_]
    }
| p=pp_elif pp=pp_land_if_section { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; pp] }
;
pp_land_else_group:
| p=pp_else e=logical_and_expression a=AMP_AMP_BROKEN
    { 
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos (L.LogicalAndExpression a) [e] in
      mknode ~pvec:[1; 1] $startpos $endpos(e) (_pp_else_group p) [p; a_]
    }
| p=pp_else r=RETURN e=logical_and_expression a=AMP_AMP_BROKEN
    { 
      ignore r;
      let a_ = mknode ~pvec:[1; 0] $startpos(e) $endpos (L.LogicalAndExpression a) [e] in
      let r_ = mknode $startpos(r) $endpos L.ReturnStatement [a_] in
      mknode ~pvec:[1; 1] $startpos $endpos(e) (_pp_else_group p) [p; r_]
    }
| p=pp_else pp=pp_land_if_section { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; pp] }
;

inclusive_or_expression:
| e=exclusive_or_expression { e }

| i=inclusive_or_expression b=BAR e=exclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression b) [i; e] }

| p=pp_expr_if_section b=BAR e=exclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression b) [p; e] }

| i=inclusive_or_expression b=BAR p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression b) [i; p] }

| p=pp_expr_if_section b=BAR pe=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression b) [p; pe] }

| pl=pp_ior_if_section_seq e=exclusive_or_expression
    { mknode ~pvec:[List.length pl; 1] $startpos $endpos (L.InclusiveOrExpression "") (pl @ [e]) }

| e=inclusive_or_expression b=BAR
    pi=pp_ifx_e i=inclusive_or_expression bb=BAR pe=pp_endif
    e1=exclusive_or_expression
    { 
      let ifg = mknode ~pvec:[1; 1] $startpos(pi) $endpos(i) (_pp_if_group pe) [pi; i] in
      let ifs = mknode ~pvec:[1; 1] $startpos(pi) $endpos (L.PpIfSection(0, get_pp_if_cond pe)) [ifg; pe] in
      let nd = mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression b) [e; ifs] in
      mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression bb) [nd; e1]
    }
| e=inclusive_or_expression p=_pp_ior_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression "") [e; p] }
;

_pp_ior_if_section:
| p=_pp_ior_if_group pl=list(_pp_ior_elif_group) p_opt=ioption(_pp_ior_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
_pp_ior_if_group:
| p=pp_ifx_b BAR i=inclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; i] }
| p=pp_ifx_b c=pp_control_line
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c] }
;
_pp_ior_elif_group:
| p=pp_elif BAR i=inclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; i] }
| p=pp_elif c=pp_control_line
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c] }
;
_pp_ior_else_group:
| p=pp_else BAR i=inclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; i] }
| p=pp_else c=pp_control_line
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c] }
;

pp_ior_if_section_seq:
| p=pp_ior_if_section { [p] }
| pl=pp_ior_if_section_seq p=pp_ior_if_section { pl @ [p] }
;

pp_ior_if_section:
| p=pp_ior_if_group pl=list(pp_ior_elif_group) p_opt=ioption(pp_ior_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_ior_if_group:
| p=pp_ifx_e i=inclusive_or_expression BAR
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; i] }
;
pp_ior_elif_group:
| p=pp_elif i=inclusive_or_expression BAR
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; i] }
;
pp_ior_else_group:
| p=pp_else i=inclusive_or_expression BAR
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; i] }
;


exclusive_or_expression:
| a=and_expression { a }
| e=exclusive_or_expression h=HAT a=and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.ExclusiveOrExpression h) [e; a]}
| e=exclusive_or_expression p=_pp_eor_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos (L.ExclusiveOrExpression "") [e; p]}
;

_pp_eor_if_section:
| p=_pp_eor_if_group pl=list(_pp_eor_elif_group) p_opt=ioption(_pp_eor_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
_pp_eor_if_group:
| p=pp_ifx_x HAT i=and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; i] }
| p=pp_ifx_x c=pp_control_line
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c] }
;
_pp_eor_elif_group:
| p=pp_elif HAT i=and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; i] }
| p=pp_elif c=pp_control_line
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c] }
;
_pp_eor_else_group:
| p=pp_else HAT i=and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; i] }
| p=pp_else c=pp_control_line
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c] }
;

and_expression:
| e=equality_expression { e }
| a=and_expression ao=AMP e=equality_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.AndExpression ao) [a; e] }
;

equality_expression:
| r=relational_expression { r }
| e=equality_expression eo=equality_op r=relational_expression
    { mknode ~pvec:[1; 1] $startpos $endpos eo [e; r] }
| p=pp_expr_if_section eo=equality_op r=relational_expression
    { mknode ~pvec:[1; 1] $startpos $endpos eo [p; r] }
| e=equality_expression eo=equality_op p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos eo [e; p] }
;

%inline
equality_op:
| EQ_EQ       { L.EqualityExpressionEq }
| e=EXCLAM_EQ { L.EqualityExpressionNeq e }
| EQ_EQ_EQ    { L.EqualityExpressionStrictEq }
;

relational_expression:
| c=compare_expression { c }
| r=relational_expression ro=relational_op c=compare_expression
    { mknode ~pvec:[1; 1] $startpos $endpos ro [r; c] }
| r=relational_expression ro=relational_op p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos ro [r; p] }
| p=pp_expr_if_section ro=relational_op c=compare_expression
    { mknode ~pvec:[1; 1] $startpos $endpos ro [p; c] }
;

%inline
relational_op:
| LT    { L.RelationalExpressionLt }
| GT    { L.RelationalExpressionGt }
| LT_EQ { L.RelationalExpressionLe }
| GT_EQ { L.RelationalExpressionGe }
;

compare_expression:
| s=shift_expression { s }
| c=compare_expression LT_EQ_GT s=shift_expression
    { mknode ~pvec:[1; 1] $startpos $endpos L.CompareExpression [c; s] }
;

shift_expression:
| a=additive_expression { a }

| s=shift_expression so=shift_op a=additive_expression
    { mknode ~pvec:[1; 1] $startpos $endpos so [s; a] }

| s=shift_expression so=shift_op p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos so [s; p] }

| s=shift_expression pi=pp_ifx_shift so=shift_op a=shift_expression pe=pp_endif
    { 
      let ifg = mknode ~pvec:[1; 1] $startpos(pi) $endpos(a) (_pp_if_group pe) [pi; a] in
      let ifs = mknode ~pvec:[1; 1] $startpos(pi) $endpos (L.PpIfSection(0, get_pp_if_cond pe)) [ifg; pe] in
      mknode ~pvec:[1; 1] $startpos $endpos so [s; ifs]
    }
;

%inline
shift_op:
| LT_LT { L.ShiftExpressionLeft }
| GT_GT { L.ShiftExpressionRight }
| GT_GT_GT { L.ShiftExpressionRightU }
;

additive_expression:
| m=multiplicative_expression { m }
| a=additive_expression ao=additive_op m=multiplicative_expression
    { mknode ~pvec:[1; 1] $startpos $endpos ao [a; m] }
| a=additive_expression ao=additive_op p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos ao [a; p] }
| a=additive_expression p=pp_aexpr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos L.AdditiveExpression [a; p] }
(*| a=additive_expression
    pi=pp_ifx_e ao=additive_op m=multiplicative_expression pe=pp_endif
    { 
      let ifg = mknode ~pvec:[1; 1] $startpos(pi) $endpos(m) (_pp_if_group pe) [pi; m] in
      let ifs = mknode ~pvec:[1; 1] $startpos(pi) $endpos (L.PpIfSection 0) [ifg; pe] in
      mknode ~pvec:[1; 1] $startpos $endpos ao [a; ifs]
    }
| m=additive_expression
    pi=pp_ifx_e mo=multiplicative_op p=pm_expression pe=pp_endif
    { 
      let ifg = mknode ~pvec:[1; 1] $startpos(pi) $endpos(p) (_pp_if_group pe) [pi; p] in
      let ifs = mknode ~pvec:[1; 1] $startpos(pi) $endpos (L.PpIfSection 0) [ifg; pe] in
      mknode ~pvec:[1; 1] $startpos $endpos mo [m; ifs]
    }*)
| a=additive_expression i=SUFFIX_MACRO { mknode $startpos $endpos (L.SuffixMacro i) [a] }

| a=additive_expression SUFFIX_MARKER p0=pp_control_line i=SUFFIX_MACRO p1=pp_control_line
    { mknode $startpos $endpos (L.SuffixMacro i) [p0; a; p1] }

| a=additive_expression op=op_macro_call
    { 
      op#add_children_l [a];
      let pvec =
        match op#pvec with
        | _::tl -> 1::tl
        | _ -> assert false
      in
      op#set_pvec pvec;
      reloc $startpos $endpos op
    }
;

pp_aexpr_if_section:
| p=pp_aexpr_if_group pl=list(pp_aexpr_elif_group) p_opt=ioption(pp_aexpr_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_aexpr_if_group:
| p=pp_ifx_e al=additive_unit+ ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length al] $startpos $endpos(al) (pp_if_group()) (p::al) }
| p=pp_ifx_e ml=multiplicative_unit+ ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos(ml) (pp_if_group()) (p::ml) }
;
pp_aexpr_elif_group:
| p=pp_elif al=additive_unit+ ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length al] $startpos $endpos(al) (_pp_elif_group p) (p::al) }
| p=pp_elif ml=multiplicative_unit+ ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos(ml) (_pp_elif_group p) (p::ml) }
;
pp_aexpr_else_group:
| p=pp_else al=additive_unit+ ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length al] $startpos $endpos(al) (_pp_else_group p) (p::al) }
| p=pp_else ml=multiplicative_unit+ ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos(ml) (_pp_else_group p) (p::ml) }
;

pp_aexpr_if_section_closing:
| p=pp_aexpr_if_group_closing
    pl=list(pp_aexpr_elif_group_closing)
    p_opt=ioption(pp_aexpr_else_group_closing)
    pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_aexpr_if_group_closing:
| p=pp_ifx_e al=additive_unit+ RPAREN ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length al] $startpos $endpos(al) (pp_if_group()) (p::al) }
| p=pp_ifx_e ml=multiplicative_unit+ RPAREN ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos(ml) (pp_if_group()) (p::ml) }
;
pp_aexpr_elif_group_closing:
| p=pp_elif mid_paren_close al=additive_unit+ RPAREN ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length al] $startpos $endpos(al) (_pp_elif_group p) (p::al) }
| p=pp_elif mid_paren_close ml=multiplicative_unit+ RPAREN ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos(ml) (_pp_elif_group p) (p::ml) }
;
pp_aexpr_else_group_closing:
| p=pp_else mid_paren_close al=additive_unit+ RPAREN ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length al] $startpos $endpos(al) (_pp_else_group p) (p::al) }
| p=pp_else mid_paren_close ml=multiplicative_unit+ RPAREN ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length ml] $startpos $endpos(ml) (_pp_else_group p) (p::ml) }
;

%inline
additive_unit:
| ao=additive_op e=multiplicative_expression { mknode ~pvec:[0; 1] $startpos $endpos ao [e] }
;

%inline
multiplicative_unit:
| mo=multiplicative_op e=pm_expression { mknode ~pvec:[0; 1] $startpos $endpos mo [e] }
;

%inline
additive_op:
| PLUS  { L.AdditiveExpressionAdd }
| MINUS { L.AdditiveExpressionSubt }
;

multiplicative_expression:
| p=pm_expression { p }
| m=multiplicative_expression mo=multiplicative_op p=pm_expression
    { mknode ~pvec:[1; 1] $startpos $endpos mo [m; p] }
| m=multiplicative_expression mo=multiplicative_op p=pp_expr_if_section
    { mknode ~pvec:[1; 1] $startpos $endpos mo [m; p] }
;
%inline
multiplicative_op:
| STAR  { L.MultiplicativeExpressionMult }
| SLASH { L.MultiplicativeExpressionDiv }
| PERC  { L.MultiplicativeExpressionMod }
;

pm_expression:
| c=cast_expression { c }
| p=pm_expression po=pm_op c=cast_expression
    { mknode ~pvec:[1; 1] $startpos $endpos po [p; c] }
;
%inline
pm_op:
| DOT_STAR      { L.PmExpressionClass }
| MINUS_GT_STAR { L.PmExpressionPtr }
;

cast_expression:
| u=unary_expression { u }
| TY_LPAREN t=type_id RPAREN c=cast_expression
    { mknode ~pvec:[0; 1; 1] $startpos $endpos L.CastExpression [t; c] }
| TY_LPAREN al=gnu_attribute_seq t=type_id RPAREN c=cast_expression
    { mknode ~pvec:[List.length al; 1; 1] $startpos $endpos L.CastExpression (al @ [t; c]) }

| TY_LPAREN t=type_id RPAREN p=pp_expr_if_section { mknode ~pvec:[0; 1; 1] $startpos $endpos L.CastExpression [t; p] }

| i=IDENT_CHM ml=macro_args c=cast_expression
    { 
      let t = mknode ~pvec:[List.length ml; 0] $startpos $endpos(ml) (L.TypeMacroInvocation i) ml in
      mknode ~pvec:[0; 1; 1] $startpos $endpos L.CastExpression [t; c]
    }
;

unary_expression:
| p=postfix_expression { p }
| uo=unary_operator c=cast_expression { mknode $startpos $endpos uo [c] }
| uo=op_macro_call c=cast_expression
    { 
      uo#add_children_r [c];
      uo#set_pvec (uo#pvec @ [1]);
      reloc $startpos $endpos uo
    }
| PLUS_PLUS   c=cast_expression { mknode $startpos $endpos L.UnaryExpressionIncr [c] }
| MINUS_MINUS c=cast_expression { mknode $startpos $endpos L.UnaryExpressionDecr [c] }
| a=await_expression { a }
| SIZEOF u=unary_expression { mknode $startpos $endpos L.UnaryExpressionSizeof [u] }
| SIZEOF TY_LPAREN t=type_id RPAREN
    { 
      t#add_prefix "(";
      t#add_suffix ")";
      mknode $startpos $endpos L.UnaryExpressionSizeof [t]
    } %prec PREC
| SIZEOF TY_LPAREN t=class_specifier RPAREN
    { 
      t#add_prefix "(";
      t#add_suffix ")";
      mknode $startpos $endpos L.UnaryExpressionSizeof [t]
    } (* extension? *)
| SIZEOF ELLIPSIS LPAREN i=IDENT RPAREN
    { mkleaf $startpos $endpos (L.UnaryExpressionSizeofPack i) }
| ALIGNOF TY_LPAREN t=type_id RPAREN { mknode $startpos $endpos L.UnaryExpressionAlignof [t] }
| n=noexcept_expression { n }
| n=new_expression { n }
| d=delete_expression { d }
| b=block_literal_expression { b }
;

block_literal_expression:
| TY_HAT c=compound_statement
    { mknode ~pvec:[0; 1] $startpos $endpos L.BlockLiteralExpression [c] }
| TY_HAT p=parameters_and_qualifiers c=compound_statement
    { mknode ~pvec:[0; 1] $startpos $endpos L.BlockLiteralExpression [p; c] }
| TY_HAT d=defining_type_id c=compound_statement
    { mknode ~pvec:[1; 1] $startpos $endpos L.BlockLiteralExpression [d; c] }
;

await_expression:
| CO_AWAIT c=cast_expression { mknode $startpos $endpos L.AwaitExpression [c] }
;

unary_operator:
| STAR       { Ast.L.UnaryExpressionInd }
| AMP        { Ast.L.UnaryExpressionAddr }
| AMP_AMP    { Ast.L.UnaryExpressionLabelAddr }
| PLUS       { Ast.L.UnaryExpressionPlus }
| MINUS      { Ast.L.UnaryExpressionMinus }
| e=EXCLAM   { Ast.L.UnaryExpressionNeg e }
| t=TILDE    { Ast.L.UnaryExpressionCompl t }
| o=OP_MACRO { Ast.L.OperatorMacro o }
;

op_macro_call:
| i=IDENT_OM ml=macro_args { mknode ~pvec:[0; List.length ml; 0] $startpos $endpos (L.OperatorMacroInvocation i) ml }
;

noexcept_expression:
| NOEXCEPT LPAREN e=expression RPAREN { mknode $startpos $endpos L.NoexceptExpression [e] }
;

new_expression:
| c_opt=ioption(colon_colon)
    NEW n_opt=ioption(new_placement) n=new_type_id ni_opt=ioption(new_initializer)
    { 
      let cl = opt_to_list c_opt in
      let nl = opt_to_list n_opt in
      let nil = opt_to_list ni_opt in
      let pvec = [List.length cl; List.length nl; 1; List.length nil] in
      mknode ~pvec $startpos $endpos L.NewExpression (cl @ nl @ [n] @ nil)
    } %prec PREC
| c_opt=ioption(colon_colon)
    NEW n_opt=ioption(new_placement) TY_LPAREN t=type_id RPAREN ni_opt=ioption(new_initializer)
    { 
      let cl = opt_to_list c_opt in
      let nl = opt_to_list n_opt in
      let nil = opt_to_list ni_opt in
      let pvec = [List.length cl; List.length nl; 1; List.length nil] in
      mknode ~pvec $startpos $endpos L.NewExpression (cl @ nl @ [t] @ nil)
    } %prec PREC
| MS_REF c_opt=ioption(colon_colon)
    NEW n_opt=ioption(new_placement) n=new_type_id ni_opt=ioption(new_initializer)
    { 
      let cl = opt_to_list c_opt in
      let nl = opt_to_list n_opt in
      let nil = opt_to_list ni_opt in
      let pvec = [List.length cl; List.length nl; 1; List.length nil] in
      mknode ~pvec $startpos $endpos L.RefNewExpression (cl @ nl @ [n] @ nil)
    } %prec PREC
;
%inline
colon_colon:
| COLON_COLON { mkleaf $startpos $endpos L.ColonColon }
;

new_placement:
| LPAREN el=expression_list RPAREN { mknode $startpos $endpos L.NewPlacement el }
;

new_type_id:
| tl=type_specifier_seq n_opt=ioption(new_declarator)
    { 
      let nl = opt_to_list n_opt in
      mknode ~pvec:[List.length tl; List.length nl] $startpos $endpos L.NewTypeId (tl @ nl)
    }
;

new_declarator:
| p=ptr_operator n_opt=ioption(new_declarator)
    { 
      let nl = opt_to_list n_opt in
      mknode ~pvec:[1; List.length nl] $startpos $endpos L.NewDeclaratorPtr (p::nl)
    }
| n=noptr_new_declarator { n }
;

noptr_new_declarator:
| LBRACKET e=expression RBRACKET al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[0; 1; List.length al] $startpos $endpos L.NoptrNewDeclarator (e::al)
    }
| n=noptr_new_declarator LBRACKET c=constant_expression RBRACKET
    al_opt=attribute_specifier_seq_opt
    { 
      let al = list_opt_to_list al_opt in
      mknode ~pvec:[1; 1; List.length al] $startpos $endpos L.NoptrNewDeclarator (n::c::al)
    }
;

new_initializer:
| LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      mknode ~pvec:[List.length el; 0] $startpos $endpos L.NewInitializer el
    }
| LPAREN el_opt=expression_list_opt p=pp_args_if_section_closing
    { 
      let el = list_opt_to_list el_opt in
      mknode ~pvec:[List.length el; 1] $startpos $endpos L.NewInitializer (el @ [p])
    }
| b=braced_init_list { b }
| n=NEW_INIT_MACRO { mkleaf $startpos $endpos (L.NewInitializerMacro n) }
;

delete_expression:
| c_opt=ioption(colon_colon) DELETE c=cast_expression
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[List.length cl; 1] $symbolstartpos $endpos L.DeleteExpression (cl @ [c])
    }
| c_opt=ioption(colon_colon) DELETE LBRACKET RBRACKET c=cast_expression
    { 
      let cl = opt_to_list c_opt in
      mknode ~pvec:[List.length cl; 1] $symbolstartpos $endpos L.DeleteExpressionBracket (cl @ [c])
    }
;

%inline
assignment_operator_:
| a=assignment_operator { a }
| LT_LT { L.AssignmentExpressionOverloaded "<<" }
;

assignment_operator:
| EQ       { L.AssignmentExpressionEq }
| PLUS_EQ  { L.AssignmentExpressionPlus }
| MINUS_EQ { L.AssignmentExpressionMinus }
| STAR_EQ  { L.AssignmentExpressionMult }
| SLASH_EQ { L.AssignmentExpressionDiv }
| PERC_EQ  { L.AssignmentExpressionMod }
| LT_LT_EQ { L.AssignmentExpressionShiftLeft }
| GT_GT_EQ { L.AssignmentExpressionShiftRight }
| a=AMP_EQ { L.AssignmentExpressionAnd a }
| h=HAT_EQ { L.AssignmentExpressionXor h }
| b=BAR_EQ { L.AssignmentExpressionOr b }
;

primary_expression:
| DUMMY_EXPR { mkleaf $startpos $endpos L.DummyExpr }
| l=literal { l }
| THIS { mkleaf $startpos $endpos L.This }
| LPAREN e=expression RPAREN { mknode $startpos $endpos L.ParenthesizedExpression [e] }
| LPAREN e=expression pl=pp_control_line+ RPAREN
    { 
      mknode $startpos $endpos L.ParenthesizedExpression (e::pl)
    }
| LPAREN RPAREN { mknode $startpos $endpos L.ParenthesizedExpression [] }
| i=id_expression
    { 
      let qn = Ast.qn_of_id_expression i in
      env#set_obj_binding qn i;
      i
    }
| l=lambda_expression { l }
| f=fold_expression { f }
| r=requires_expression { r }
| LPAREN s=statement RPAREN { mknode $startpos $endpos L.GnuStatementExpression [s] }
| LPAREN
    pi=pp_ifx_e e0=expression RPAREN BAR_BAR LPAREN e1=expression AMP_AMP_BROKEN pe=pp_endif
    e2=expression RPAREN
    { 
      let ifg = mknode ~pvec:[1; 2] $startpos $endpos(e1) (_pp_if_group pe) [pi; e0; e1] in
      let ifs = mknode ~pvec:[1; 0; 0; 1] $startpos $endpos(pe) (L.PpIfSection(0, get_pp_if_cond pe)) [ifg; pe] in
      mknode $startpos $endpos L.ParenthesizedExpression [ifs; e2]
    }
| LPAREN e=expression MARKER p=pp_aexpr_if_section_closing
    { 
      mknode $startpos $endpos L.ParenthesizedExpression [e; p]
    }
| e=objc_message_expr { e }
| e=objc_selector_expr { e }
| e=objc_encode_expr { e }
;

objc_encode_expr:
| OBJC_ENCODE TY_LPAREN sl=decl_specifier_seq d_opt=ioption(abstract_declarator) RPAREN
    { 
      let dl = opt_to_list d_opt in
      let pvec = [List.length sl; List.length dl] in
      mknode ~pvec $startpos $endpos L.ObjcEncodeExpression (sl @ dl)
    }
;
objc_selector_expr:
| OBJC_SELECTOR TY_LPAREN i=IDENT RPAREN { mkleaf $startpos $endpos (L.ObjcSelectorExpression i) }
;
objc_message_expr:
| OBJC_LBRACKET e=logical_or_expression m=objc_message_selector RBRACKET
    { mknode $startpos $endpos L.ObjcMessageExpression [e; m] }
;
objc_message_selector:
| i=IDENT_V { mkleaf $startpos $endpos (L.ObjcSelector i) }
| al=objc_keyword_arg+ { mknode $startpos $endpos L.ObjcMessageSelector al }
;
objc_keyword_arg:
| i_opt=ioption(IDENT_V) COLON e=expression
    { 
      let s = string_opt_to_string i_opt in
      mknode $symbolstartpos $endpos (L.ObjcKeywordArgument s) [e]
    }
;

fold_expression:
| FOLD_LPAREN c=cast_expression f=fold_operator ELLIPSIS RPAREN
    { mknode ~pvec:[1; 1; 0; 0] $startpos $endpos L.FoldExpression [c; f] }
| FOLD_LPAREN ELLIPSIS f=fold_operator c=cast_expression RPAREN
    { mknode ~pvec:[0; 0; 1; 1] $startpos $endpos L.FoldExpression [f; c] }
| FOLD_LPAREN c=cast_expression f=fold_operator ELLIPSIS
    f1=fold_operator c1=cast_expression RPAREN
    { mknode ~pvec:[1; 1; 1; 1] $startpos $endpos L.FoldExpression [c; f; f1; c1] }
;

fold_operator:
| PLUS          { mkleaf $startpos $endpos L.Plus }
| MINUS         { mkleaf $startpos $endpos L.Minus }
| STAR          { mkleaf $startpos $endpos L.Star }
| SLASH         { mkleaf $startpos $endpos L.Slash }
| PERC          { mkleaf $startpos $endpos L.Perc }
| i=HAT         { mkleaf $startpos $endpos (L.Hat i) }
| i=AMP         { mkleaf $startpos $endpos (L.Amp i) }
| i=BAR         { mkleaf $startpos $endpos (L.Bar i) }
| LT_LT         { mkleaf $startpos $endpos L.LtLt }
| GT_GT         { mkleaf $startpos $endpos L.GtGt }
| PLUS_EQ       { mkleaf $startpos $endpos L.PlusEq }
| MINUS_EQ      { mkleaf $startpos $endpos L.MinusEq }
| STAR_EQ       { mkleaf $startpos $endpos L.StarEq }
| SLASH_EQ      { mkleaf $startpos $endpos L.SlashEq }
| PERC_EQ       { mkleaf $startpos $endpos L.PercEq }
| i=HAT_EQ      { mkleaf $startpos $endpos (L.HatEq i) }
| i=AMP_EQ      { mkleaf $startpos $endpos (L.AmpEq i) }
| i=BAR_EQ      { mkleaf $startpos $endpos (L.BarEq i) }
| LT_LT_EQ      { mkleaf $startpos $endpos L.LtLtEq }
| GT_GT_EQ      { mkleaf $startpos $endpos L.GtGtEq }
| EQ            { mkleaf $startpos $endpos L.Eq }
| EQ_EQ         { mkleaf $startpos $endpos L.EqEq }
| i=EXCLAM_EQ   { mkleaf $startpos $endpos (L.ExclamEq i) }
| LT            { mkleaf $startpos $endpos L.Lt }
| GT            { mkleaf $startpos $endpos L.Gt }
| LT_EQ         { mkleaf $startpos $endpos L.LtEq }
| GT_EQ         { mkleaf $startpos $endpos L.GtEq }
| LT_EQ_GT      { mkleaf $startpos $endpos L.LtEqGt }
| i=AMP_AMP     { mkleaf $startpos $endpos (L.AmpAmp i) }
| i=BAR_BAR     { mkleaf $startpos $endpos (L.BarBar i) }
| COMMA         { mkleaf $startpos $endpos L.Comma }
| DOT_STAR      { mkleaf $startpos $endpos L.DotStar }
| MINUS_GT_STAR { mkleaf $startpos $endpos L.MinusGtStar }
;

requires_expression:
| REQUIRES r_opt=ioption(requirement_parameter_list) r=requirement_body
    { 
      let rl = opt_to_list r_opt in
      mknode ~pvec:[List.length rl; 1] $startpos $endpos L.RequiresExpression (rl @ [r])
    }
;

requirement_parameter_list:
| LPAREN p=parameter_declaration_clause RPAREN
    { 
      env#register_param_decl_clause p;
      mknode $startpos $endpos L.RequirementParameterList [p]
    }
;

requirement_body:
| LBRACE rl=requirement_seq RBRACE { mknode $startpos $endpos L.RequirementBody rl }
;

requirement_seq:
| r=requirement { [r] }
| rl=requirement_seq r=requirement { rl @ [r] }
;

requirement:
| s=simple_requirement   { s }
| t=type_requirement     { t }
| c=compound_requirement { c }
| n=nested_requirement   { n }
;

simple_requirement:
| e=expression SEMICOLON { mknode $startpos $endpos L.SimpleRequirement [e] }
;

type_requirement:
| TYPENAME n_opt=ioption(nested_name_specifier) t=type_name SEMICOLON
    { 
      let nl = opt_to_list n_opt in
      mknode ~pvec:[List.length nl; 1] $startpos $endpos L.TypeRequirement (nl @ [t])
    }
;

compound_requirement:
| LBRACE e=expression RBRACE
    n_opt=ioption(noexcept) r_opt=ioption(return_type_requirement) SEMICOLON
    { 
      let nl = opt_to_list n_opt in
      let rl = opt_to_list r_opt in
      let pvec = [1; List.length nl; List.length rl] in
      mknode ~pvec $startpos $endpos L.CompoundRequirement (e :: nl @ rl)
    }
;
%inline
noexcept:
| NOEXCEPT { mkleaf $startpos $endpos L.Noexcept }
;

return_type_requirement:
| t=trailing_return_type { t }
| MINUS_GT t=type_constraint { mknode $startpos $endpos L.ReturnTypeRequirement [t] }
;

nested_requirement:
| SUB_REQUIRES c=constraint_expression SEMICOLON
    { mknode $startpos $endpos L.NestedRequirement [c] }
;

constraint_expression:
| l=logical_or_expression { l }
;

lambda_expression:
| lh=_lambda_expression c=compound_statement
    { 
      lh#add_children_r [c];
      lh#set_pvec (lh#pvec @ [1]);
      reloc $startpos $endpos lh
    }
| lh=_lambda_expression DUMMY_STMT { lh }
;

%inline
_lambda_expression:
| l=lambda_introducer
    { 
      mknode ~pvec:[1; 0; 0; 0; 0] $startpos $endpos L.LambdaExpression [l]
    }
| l=lambda_introducer ld=lambda_declarator r_opt=ioption(requires_clause)
    { 
      let rl = opt_to_list r_opt in
      let pvec = [1; 0; 0; 1; List.length rl] in
      mknode ~pvec $startpos $endpos L.LambdaExpression (l :: ld :: rl)
    }
| l=lambda_introducer TEMPL_LT tl=template_parameter_list TEMPL_GT r_opt=ioption(requires_clause)
    { 
      let rl = opt_to_list r_opt in
      let pvec = [1; List.length tl; 0; 0; List.length rl] in
      mknode ~pvec $startpos $endpos L.LambdaExpression (l :: tl @ rl)
    }
| l=lambda_introducer TEMPL_LT tl=template_parameter_list TEMPL_GT
    r0_opt=ioption(requires_clause) ld=lambda_declarator r1_opt=ioption(requires_clause)
    { 
      let rl0 = opt_to_list r0_opt in
      let rl1 = opt_to_list r1_opt in
      let pvec = [1; List.length tl; List.length rl0; 1; List.length rl1] in
      mknode ~pvec $startpos $endpos L.LambdaExpression (l :: tl @ rl0 @ [ld] @ rl1)
    }
;

template_parameter_list:
| t=template_parameter { [t] }
| t=pp_templ_param_if_section { [t] }
| tl=template_parameter_list COMMA t=template_parameter { (Xlist.last tl)#add_suffix ","; tl @ [t] }
| tl=template_parameter_list COMMA t=templ_param_macro_call { (Xlist.last tl)#add_suffix ","; tl @ [t] }
| tl=template_parameter_list t=templ_param_macro_call { tl @ [t] }
;

pp_templ_param_if_section:
| p=pp_templ_param_if_group
    pl=list(pp_templ_param_elif_group) p_opt=ioption(pp_templ_param_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_templ_param_if_group:
| pi=pp_ifx tl=template_parameter_list
    { 
      mknode ~pvec:[1; List.length tl] $startpos $endpos (pp_if_group()) (pi::tl)
    }
;
pp_templ_param_elif_group:
| pi=pp_elif tl=template_parameter_list
    { 
      mknode ~pvec:[1; List.length tl] $startpos $endpos (_pp_elif_group pi) (pi::tl)
    }
;
pp_templ_param_else_group:
| pi=pp_else tl=template_parameter_list
    { 
      mknode ~pvec:[1; List.length tl] $startpos $endpos (_pp_else_group pi) (pi::tl)
    }
;

templ_param_macro_call:
| i=IDENT_TPM ml=macro_args
    { 
      let pvec = [List.length ml; 0] in
      mknode ~pvec $startpos $endpos (L.TemplParamMacroInvocation i) ml
    }
;

template_parameter:
| t=type_parameter { env#register_type_param t; t }
| p=parameter_declaration { env#register_param p; p }
;

type_parameter:
| t=type_parameter_key            i_opt=ioption(IDENT)
    { 
      let pvec = [0; 0; 1; 0] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t]
    }
| t=type_parameter_key e=ELLIPSIS i_opt=ioption(IDENT)
    { 
      ignore e;
      let t_ = mknode $startpos $endpos(e) L.PackExpansion [t] in
      t_#add_suffix "...";
      let pvec = [0; 0; 1; 0] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t_]
    }
| t=type_parameter_key i_opt=ioption(IDENT) EQ ti=type_id
    { 
      let pvec = [0; 0; 1; 1] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t; ti]
    }
| t=type_constraint            i_opt=ioption(IDENT)
    { 
      let pvec = [1; 0; 0; 0] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t]
    }
| t=type_constraint e=ELLIPSIS i_opt=ioption(IDENT)
    { 
      ignore e;
      let t_ = mknode $startpos $endpos(e) L.PackExpansion [t] in
      t_#add_suffix "...";
      let pvec = [1; 0; 0; 0] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t_]
    }
| t=type_constraint i_opt=ioption(IDENT) EQ ti=type_id
    { 
      let pvec = [1; 0; 0; 1] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t; ti]
    }
| t=template_head mid_typaram tk=type_parameter_key            i_opt=ioption(IDENT)
    { 
      let pvec = [0; 1; 1; 0] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t; tk]
    }
| t=template_head mid_typaram tk=type_parameter_key e=ELLIPSIS i_opt=ioption(IDENT)
    { 
      ignore e;
      let tk_ = mknode $startpos $endpos(e) L.PackExpansion [tk] in
      tk_#add_suffix "...";
      let pvec = [0; 1; 1; 0] in
      mknode ~pvec $startpos $endpos (L.TypeParameter (string_opt_to_string i_opt)) [t; tk_]
    }
| t=template_head mid_typaram tk=type_parameter_key i_opt=ioption(IDENT) EQ ti=type_id
    { 
      let pvec = [0; 1; 1; 1] in
      let i = string_opt_to_string i_opt in
      mknode ~pvec $startpos $endpos (L.TypeParameter i) [t; tk; ti]
    }
;

mid_typaram:
| { env#stack#exit_template() }
;

type_constraint:
| i=IDENT_C
    { 
      let uqn = Ast.encode_ident i in
      mkleaf ~pvec:[0; 0] $startpos $endpos (L.TypeConstraint uqn)
    }
| i=IDENT_C TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { 
      let tl = list_opt_to_list tl_opt in
      let uqn = (Ast.encode_ident i)^(Ast.encode_template_arguments tl) in
      mknode ~pvec:[0; List.length tl] $startpos $endpos (L.TypeConstraint uqn) tl
    }
| n=nested_name_specifier i=IDENT_C
    { 
      let uqn = Ast.encode_ident i in
      mknode ~pvec:[1; 0] $startpos $endpos (L.TypeConstraint uqn) [n]
    }
| n=nested_name_specifier i=IDENT_C TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { 
      let tl = list_opt_to_list tl_opt in
      let uqn = (Ast.encode_ident i)^(Ast.encode_template_arguments tl) in
      mknode ~pvec:[1; List.length tl] $startpos $endpos (L.TypeConstraint uqn) (n::tl)
    }
;

template_head:
| TEMPLATE
    TEMPL_LT mid_templ_head tl=template_parameter_list TEMPL_GT r_opt=ioption(requires_clause)
    { 
      env#register_templ_params tl;
      let rl = opt_to_list r_opt in
      mknode ~pvec:[List.length tl; List.length rl] $startpos $endpos L.TemplateHead (tl @ rl)
    }
;

mid_templ_head:
| { env#stack#enter_template() }
;

%inline
type_parameter_key:
| CLASS    { mkleaf $startpos $endpos L.TypeParameterKeyClass }
| TYPENAME { mkleaf $startpos $endpos L.TypeParameterKeyTypename }
;

lambda_introducer:
| LAM_LBRACKET l_opt=ioption(lambda_capture) RBRACKET
    { mknode $startpos $endpos L.LambdaIntroducer (opt_to_list l_opt) }
;

lambda_declarator:
(*| TY_LPAREN p=parameter_declaration_clause RPAREN al_opt=attribute_specifier_seq_opt
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let al = list_opt_to_list al_opt in
      let tl = opt_to_list t_opt in
      let pvec = [1; 0; List.length al; 0; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: al @ tl)
    }
| TY_LPAREN p=parameter_declaration_clause RPAREN dl=decl_specifier_seq
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length dl; 0; 0; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: dl @ tl)
    }
| TY_LPAREN p=parameter_declaration_clause RPAREN dl_opt=decl_specifier_seq_opt
    n=noexcept_specifier al_opt=attribute_specifier_seq_opt
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let dl = list_opt_to_list dl_opt in
      let al = list_opt_to_list al_opt in
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length dl; 1; List.length al; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: dl @ [n] @ al @ tl)

    }*)
| TY_LPAREN p=parameter_declaration_clause RPAREN
    al=attribute_specifier_seq
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length al; 0; 0; 0; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: al @ tl)
    }
| TY_LPAREN p=parameter_declaration_clause RPAREN
    al=attribute_specifier_seq
    dl=decl_specifier_seq
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length al; List.length dl; 0; 0; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: al @ dl @ tl)
    }
| TY_LPAREN p=parameter_declaration_clause RPAREN
    dl_opt=decl_specifier_seq_opt
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let dl = list_opt_to_list dl_opt in
      let tl = opt_to_list t_opt in
      let pvec = [1; 0; List.length dl; 0; 0; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: dl @ tl)
    }
| TY_LPAREN p=parameter_declaration_clause RPAREN
    dl_opt=decl_specifier_seq_opt
    n=noexcept_specifier
    al_opt=attribute_specifier_seq_opt
    t_opt=ioption(trailing_return_type)
    { 
      env#register_param_decl_clause p;
      let dl = list_opt_to_list dl_opt in
      let al = list_opt_to_list al_opt in
      let tl = opt_to_list t_opt in
      let pvec = [1; 0; List.length dl; 1; List.length al; List.length tl] in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: dl @ [n] @ al @ tl)
    }
(*| TY_LPAREN p=parameter_declaration_clause RPAREN
    al0_opt=attribute_specifier_seq_opt
    dl_opt=decl_specifier_seq_opt
    n_opt=ioption(noexcept_specifier)
    al_opt=attribute_specifier_seq_opt
    t_opt=ioption(trailing_return_type)
    r_opt=ioption(requires_clause)
    { 
      env#register_param_decl_clause p;
      let al0 = list_opt_to_list al0_opt in
      let dl = list_opt_to_list dl_opt in
      let nl = opt_to_list n_opt in
      let al = list_opt_to_list al_opt in
      let tl = opt_to_list t_opt in
      let rl = opt_to_list r_opt in
      let pvec =
        [1; List.length al0; List.length dl; List.length nl; List.length al; List.length tl; List.length rl]
      in
      mknode ~pvec $startpos $endpos L.LambdaDeclarator (p :: dl @ nl @ al @ tl @ rl)
    }*)
;

lambda_capture:
| c=capture_default { mknode ~pvec:[1; 0] $startpos $endpos L.LambdaCapture [c] }
| cl=capture_list { mknode ~pvec:[0; List.length cl] $startpos $endpos L.LambdaCapture cl }
| c=capture_default COMMA cl=capture_list
    { 
      mknode ~pvec:[1; List.length cl] $startpos $endpos L.LambdaCapture (c::cl)
    }
;

capture_default:
| AMP { mkleaf $startpos $endpos L.LambdaCaptureDefaultAmp }
| EQ  { mkleaf $startpos $endpos L.LambdaCaptureDefaultEq }
;

capture_list:
| c=capture { [c] }
| p=pp_capture_if_section { [p] }
| p=pp_capture_if_section c=capture { [p; c] }
| cl=capture_list COMMA c=capture { (Xlist.last cl)#add_suffix ","; cl @ [c] }
| cl=capture_list c=capture_macro_call { cl @ [c] }
;

capture_macro_call:
| i=IDENT_EM ml=macro_args { mknode $startpos $endpos (L.LambdaCaptureMacroInvocation i) ml }
;

pp_capture_if_section:
| p=pp_capture_if_group pl=list(pp_capture_elif_group) p_opt=ioption(pp_capture_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_capture_if_group:
| p=pp_ifx c=capture ioption(COMMA) 
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c] }
;
pp_capture_elif_group:
| p=pp_elif c=capture ioption(COMMA) 
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c] }
;
pp_capture_else_group:
| p=pp_else c=capture ioption(COMMA) 
    { mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c] }
;

capture:
| s=simple_capture { s }
| s=simple_capture ELLIPSIS
    { 
      let s_ = mknode $startpos $endpos L.PackExpansion [s] in
      s_#add_suffix "...";
      s_
    }
|          i=init_capture { i }
| ELLIPSIS i=init_capture
    { 
      let i_ = mknode $startpos $endpos L.PackExpansion [i] in
      i_#add_prefix "...";
      i_
    }
;

simple_capture:
|     i=IDENT_V { mkleaf $startpos $endpos (L.SimpleCapture i) }
| AMP i=IDENT_V { mkleaf $startpos $endpos (L.SimpleCaptureAmp i) }
|      THIS { mkleaf $startpos $endpos L.SimpleCaptureThis }
| STAR THIS { mkleaf $startpos $endpos L.SimpleCaptureStarThis }
;

init_capture:
|     i=IDENT_V ini=initializer_ { mknode $startpos $endpos (L.InitCapture i) [ini] }
| AMP i=IDENT_V ini=initializer_ { mknode $startpos $endpos (L.InitCaptureAmp i) [ini] }
;

initializer_:
| b=brace_or_equal_initializer { b }
| LPAREN el=expression_list RPAREN { mknode $startpos $endpos L.ParenthesizedInitList el }
| LPAREN RPAREN { mkleaf $startpos $endpos L.ParenthesizedInitList } (* ??? *)
;

%inline
equal_initializer:
| EQ i=initializer_clause { mknode ~pvec:[0; 1] $startpos $endpos L.EqualInitializer [i] }
| EQ pl=pp_control_line+ MARKER { mknode ~pvec:[0; List.length pl] $startpos $endpos L.EqualInitializer pl }
| EQ pl=pp_control_line+ i=initializer_clause
    { 
      mknode ~pvec:[List.length pl; 1] $startpos $endpos L.EqualInitializer (pl@[i])
    }
| EQ p=pp_expr_if_section { mknode ~pvec:[0; 1] $startpos $endpos L.EqualInitializer [p] }
| EQ p=pp_expr_if_section o=bin_op e=multiplicative_expression
    { 
      let e_ = mknode ~pvec:[1; 1] $startpos(p) $endpos o (p::[e]) in
      mknode ~pvec:[0; 1] $startpos $endpos L.EqualInitializer [e_]
    }
;

brace_or_equal_initializer:
| e=equal_initializer { e }
| b=braced_init_list { b }
;

%inline
bin_op:
| o=additive_op { o }
;

literal:
| i=INT_LITERAL        { mkleaf $startpos $endpos (L.IntegerLiteral i) }
| i=INT_MACRO DOT      { mkleaf $startpos $endpos (L.LiteralMacro (i^".")) }
| c=CHAR_LITERAL       { mkleaf $startpos $endpos (L.CharacterLiteral c) }
| f=FLOAT_LITERAL      { mkleaf $startpos $endpos (L.FloatingLiteral f) }
| s=string_literal_    { s }
| b=BOOL_LITERAL       { mkleaf $startpos $endpos (L.BooleanLiteral b) }
| NULLPTR              { mkleaf $startpos $endpos L.Nullptr }
| u=USER_INT_LITERAL   { mkleaf $startpos $endpos (L.UserDefinedIntegerLiteral u) }
| u=USER_FLOAT_LITERAL { mkleaf $startpos $endpos (L.UserDefinedFloatingLiteral u) }
(*| u=USER_STR_LITERAL   { mkleaf $startpos $endpos (L.UserDefinedStringLiteral u) }*)
| u=USER_CHAR_LITERAL  { mkleaf $startpos $endpos (L.UserDefinedCharacterLiteral u) }
;

string_literal_:
| sl=string_literal_list
    { 
      match sl with
      | [s] -> s
      | _ -> mknode $startpos $endpos L.ConcatenatedString sl
    }
;

string_literal_list:
| s=string_literal { [s] }
| STR_MARKER { [] }
| s=string_literal sl=string_literal_list { s::sl }
| STR_MARKER sl=string_literal_list { sl }
;

string_literal:
| s=STR_LITERAL      { mkleaf $startpos $endpos (L.StringLiteral s) }
| u=USER_STR_LITERAL { mkleaf $startpos $endpos (L.UserDefinedStringLiteral u) }
| s=STR_MACRO        { mkleaf $startpos $endpos (L.StringMacro s) }
| s=PP_STRINGIZED    { mkleaf $startpos $endpos (L.PpStringized s) }
| e=literal_macro_call { e }
| p=pp_str_if_section { p }
;

str_:
| s=string_literal { s }
| p=pp_control_line { p }
;

pp_str_if_section:
| p=pp_str_if_group
    pl=list(pp_str_elif_group) p_opt=ioption(pp_str_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_str_if_group:
| pi=pp_ifx_s sl=str_* ioption(STR_MARKER) ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length sl; 0] $startpos $endpos (pp_if_group()) (pi::sl) }
| pi=pp_ifx_s l=STR_LITERAL p=PLUS e=expression
    { 
      ignore p;
      let l_ = mkleaf $startpos(l) $endpos(l) (L.StringLiteral l) in
      let e_ = mknode ~pvec:[0; 1] $startpos(p) $endpos L.AdditiveExpressionAdd [e] in
      mknode ~pvec:[1; 0; 1] $startpos $endpos (pp_if_group()) (pi::l_::[e_])
    }
;
pp_str_elif_group:
| pi=pp_elif sl=str_* ioption(STR_MARKER) ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length sl; 0] $startpos $endpos (pp_if_group()) (pi::sl) }
| pi=pp_elif l=STR_LITERAL p=PLUS e=expression
    { 
      ignore p;
      let l_ = mkleaf $startpos(l) $endpos(l) (L.StringLiteral l) in
      let e_ = mknode ~pvec:[0; 1] $startpos(p) $endpos L.AdditiveExpressionAdd [e] in
      mknode ~pvec:[1; 0; 1] $startpos $endpos (_pp_elif_group pi) (pi::l_::[e_])
    }
;
pp_str_else_group:
| pi=pp_else sl=str_* ioption(STR_MARKER) ioption(SEMICOLON)
    { mknode ~pvec:[1; List.length sl; 0] $startpos $endpos (pp_if_group()) (pi::sl) }
| pi=pp_else l=STR_LITERAL p=PLUS e=expression
    { 
      ignore p;
      let l_ = mkleaf $startpos(l) $endpos(l) (L.StringLiteral l) in
      let e_ = mknode ~pvec:[0; 1] $startpos(p) $endpos L.AdditiveExpressionAdd [e] in
      mknode ~pvec:[1; 1; 0] $startpos $endpos (pp_if_group()) (pi::l_::[e_])
    }
;

literal_macro_call:
| i=IDENT_LM ml=macro_args { mknode $startpos $endpos (L.LiteralMacroInvocation i) ml }
;

unqualified_id:
| i=IDENT_V { mkleaf $startpos $endpos (L.Identifier i) }
| o=operator_function_id { o }
| c=conversion_function_id { c }
| l=literal_operator_id { l }
| TY_TILDE t=type_name { mknode $startpos $endpos L.Destructor [t] }
| TY_TILDE d=decltype_specifier { mknode $startpos $endpos L.Destructor [d] }
| t=template_id { t }
| i=id_macro_call { i }
| p=pp_concat { p }
;

id_macro_call:
| i=IDENT_IM ml=macro_args { mknode $startpos $endpos (L.IdentifierMacroInvocation i) ml }
| i=IDENT_IM a=args_macro { mknode $startpos $endpos (L.IdentifierMacroInvocation i) [a] }
;

pp_concat:
| i0=id_macro_call SHARP_SHARP i1=id_macro_call
    { mknode $startpos $endpos L.PpConcatenatedIdentifier [i0; i1] }
;

qualified_id:
| n=nested_name_specifier ioption(TEMPLATE) u=unqualified_id
    { 
      (*let qn = (Ast.encode_nested_name_spec n)^(Ast.uqn_of_unqualified_id u) in*)
      mknode ~pvec:[1; 1] $startpos $endpos L.QualifiedId [n; u]
    }
;

simple_template_id:
| t=template_name TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { mknode $startpos $endpos (L.SimpleTemplateId t) (list_opt_to_list tl_opt) }
| i=id_macro_call TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { 
      let tl = list_opt_to_list tl_opt in
      let pvec = [1; List.length tl] in
      mknode ~pvec $startpos $endpos L.SimpleTemplateIdM (i::tl)
    }
;

simple_template_id_:
| t=template_name TEMPL_LT tl_opt=template_argument_list_opt TY_TEMPL_GT
    { mknode $startpos $endpos (L.SimpleTemplateId t) (list_opt_to_list tl_opt) }
| i=id_macro_call TEMPL_LT tl_opt=template_argument_list_opt TY_TEMPL_GT
    { 
      let tl = list_opt_to_list tl_opt in
      let pvec = [1; List.length tl] in
      mknode ~pvec $startpos $endpos L.SimpleTemplateIdM (i::tl)
    }
;

template_id:
| s=simple_template_id { s }
| o=operator_function_id TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { 
      let tl = list_opt_to_list tl_opt in
      mknode ~pvec:[1; List.length tl] $startpos $endpos L.TemplateIdOp (o::tl)
    }
| l=literal_operator_id  TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { 
      let tl = list_opt_to_list tl_opt in
      mknode ~pvec:[1; List.length tl] $startpos $endpos L.TemplateIdLit (l::tl)
    }
;

conversion_function_id:
| OPERATOR c=conversion_type_id { mknode $startpos $endpos L.ConversionFunctionId [c] }
;

conversion_type_id:
| tl=type_specifier_seq
    { mknode ~pvec:[List.length tl; 0] $startpos $endpos L.ConversionTypeId tl } %prec PREC
| tl=type_specifier_seq c=conversion_declarator
    { mknode ~pvec:[List.length tl; 1] $startpos $endpos L.ConversionTypeId (tl @ [c]) }
;

conversion_declarator:
| p=ptr_operator
    { mknode ~pvec:[1; 0] $startpos $endpos L.ConversionDeclarator [p] } %prec PREC
| p=ptr_operator c=conversion_declarator
    { mknode ~pvec:[1; 1] $startpos $endpos L.ConversionDeclarator [p; c] }
;

%inline
type_specifier_seq:
| tl=_type_specifier_seq al_opt=attribute_specifier_seq_opt
    { 
      match al_opt with
      | Some al -> begin
          let pvec = [List.length tl; List.length al] in
          [mknode ~pvec $startpos $endpos L.TypeSpecifierSeq (tl @ al)]
      end
      | None -> tl
    }
;
_type_specifier_seq:
| t=type_specifier { [t] }
| tl=_type_specifier_seq t=type_specifier { tl@[t] }
;

type_specifier:
| s=simple_type_specifier { s }
| e=elaborated_type_specifier { e }
| t=typename_specifier { t }
| c=cv_qualifier { c }
| t=ty_macro_call { t }
;

simple_type_specifier:
| i=IDENT
    { 
      let uqn = Ast.encode_ident i in
      let nd = mkleaf ~pvec:[0; 0; 0; 0] $startpos $endpos (L.SimpleTypeSpecifier uqn) in
      env#set_type_binding uqn nd;
      nd
    }
| n=nested_name_specifier i=IDENT
    { 
      let uqn = Ast.encode_ident i in
      let p = (Ast.encode_nested_name_spec n) in
      let nd = mknode ~pvec:[1; 0; 0; 0] $startpos $endpos (L.SimpleTypeSpecifier uqn) [n] in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
(*| ioption(nested_name_specifier) template_name { }*)(*contained in the above*)
| s=simple_template_id_
    { 
      let uqn = Ast.uqn_of_simple_template_id s in
      let nd = mknode ~pvec:[0; 0; 1; 0] $startpos $endpos (L.SimpleTypeSpecifier uqn) [s] in
      env#set_type_binding uqn nd;
      nd
    }
| n=nested_name_specifier t_opt=ioption(template) s=simple_template_id_
    { 
      let uqn = Ast.uqn_of_simple_template_id s in
      let p = (Ast.encode_nested_name_spec n) in
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1; 0] in
      let nd = mknode ~pvec $startpos $endpos (L.SimpleTypeSpecifier uqn) (n :: tl @ [s]) in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| d=decltype_specifier { d }
| p=placeholder_type_specifier { p }
(*| ioption(nested_name_specifier) type_name { }*)
(*| ioption(nested_name_specifier) template_name { }*)
| b=basic_type { b }
| n=nested_name_specifier t=ty_macro_call
    { 
      mknode ~pvec:[1; 0; 0; 1; 0] $startpos $endpos (L.SimpleTypeSpecifier "") [n; t]
    }
| t=TYPE_MACRO { mkleaf $startpos $endpos (L.TypeMacro t) }
;

%inline
basic_type:
| CHAR     { mkleaf ~pvec:[0] $startpos $endpos L.Char }
| CHAR8_T  { mkleaf ~pvec:[0] $startpos $endpos L.Char8_t }
| CHAR16_T { mkleaf ~pvec:[0] $startpos $endpos L.Char16_t }
| CHAR32_T { mkleaf ~pvec:[0] $startpos $endpos L.Char32_t }
| WCHAR_T  { mkleaf ~pvec:[0] $startpos $endpos L.Wchar_t }
| BOOL     { mkleaf ~pvec:[0] $startpos $endpos L.Bool }
| SHORT    { mkleaf ~pvec:[0] $startpos $endpos L.Short }
| INT      { mkleaf ~pvec:[0] $startpos $endpos L.Int }
| LONG     { mkleaf ~pvec:[0] $startpos $endpos L.Long }
| SIGNED   { mkleaf ~pvec:[0] $startpos $endpos L.Signed }
| UNSIGNED { mkleaf ~pvec:[0] $startpos $endpos L.Unsigned }
| FLOAT    { mkleaf ~pvec:[0] $startpos $endpos L.Float }
| DOUBLE   { mkleaf ~pvec:[0] $startpos $endpos L.Double }
| VOID     { mkleaf ~pvec:[0] $startpos $endpos L.Void }
| UNSIGNED INT  { mkleaf ~pvec:[0] $startpos $endpos L.UnsignedInt }
| UNSIGNED LONG { mkleaf ~pvec:[0] $startpos $endpos L.UnsignedLong }
;

%inline
template_name:
| i=IDENT { i }
;

placeholder_type_specifier:
| t_opt=ioption(type_constraint) AUTO
    { 
      let tl = opt_to_list t_opt in
      let pvec = [List.length tl; 0] in
      mknode ~pvec $symbolstartpos $endpos L.PlaceholderTypeSpecifierAuto tl
    }
| t_opt=ioption(type_constraint) DECLTYPE LPAREN AUTO RPAREN
    { 
      let tl = opt_to_list t_opt in
      let pvec = [List.length tl; 0] in
      mknode ~pvec $symbolstartpos $endpos L.PlaceholderTypeSpecifierDecltype tl
    }
;

elaborated_type_specifier:
| c=class_key al=class_attr_spec_seq n_opt=ioption(nested_name_specifier) i=IDENT
    { 
      let uqn = Ast.encode_ident i in
      let lab = L.ClassKey.to_elaborated_type_specifier uqn c in
      let p =
        match n_opt with
        | Some n -> (Ast.encode_nested_name_spec n)
        | None -> ""
      in
      let nl = opt_to_list n_opt in
      let nd =
        mknode ~pvec:[List.length al; List.length nl; 0] $startpos $endpos lab (al @ nl)
      in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| c=class_key al=class_attr_spec_seq n_opt=ioption(nested_name_specifier) i=id_macro_call
    { 
      let uqn = Ast.encode_ident i#get_name in
      let lab = L.ClassKey.to_elaborated_type_specifier uqn c in
      let p =
        match n_opt with
        | Some n -> (Ast.encode_nested_name_spec n)
        | None -> ""
      in
      let nl = opt_to_list n_opt in
      let nd =
        mknode ~pvec:[List.length al; List.length nl; 1] $startpos $endpos lab (al @ nl @ [i])
      in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
(*| c=class_key el_opt=ioption(elaborated_type_specifier_part) s=simple_template_id_
    { 
      let uqn = Ast.uqn_of_simple_template_id s in
      let p =
        match el_opt with
        | Some [n] -> (Ast.encode_nested_name_spec n)
        | _ -> ""
      in
      let lab = L.ClassKey.to_elaborated_type_specifier uqn c in
      let el = list_opt_to_list el_opt in
      let nd = mknode ~pvec:[0; List.length el; 1] $startpos $endpos lab (el @ [s]) in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }*)
| c=class_key al=class_attr_spec_seq el_opt=ioption(elaborated_type_specifier_part) s=simple_template_id_
    {                (*???*)
      let uqn = Ast.uqn_of_simple_template_id s in
      let p =
        match el_opt with
        | Some [n] -> (Ast.encode_nested_name_spec n)
        | _ -> ""
      in
      let lab = L.ClassKey.to_elaborated_type_specifier uqn c in
      let el = list_opt_to_list el_opt in
      s#add_prefix " ";
      let nd = mknode ~pvec:[List.length al; List.length el; 1] $startpos $endpos lab (el @ [s]) in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| ELAB_ENUM n_opt=ioption(nested_name_specifier) i=IDENT
    { 
      let p =
        match n_opt with
        | Some n -> (Ast.encode_nested_name_spec n)
        | None -> ""
      in
      let uqn = Ast.encode_ident i in
      let nd =
        mknode $startpos $endpos (L.ElaboratedTypeSpecifierEnum uqn) (opt_to_list n_opt)
      in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
;
%inline
elaborated_type_specifier_part:
| n=nested_name_specifier ioption(TEMPLATE) { [n] }
;

member_specification:
| ml=mem_decl_seq { mknode $startpos $endpos L.MemberSpecification ml }
;

mem_access_spec:
| a=mem_access_spec_head COLON { a#add_suffix ":"; a }
;
mem_access_spec_head:
| a=access_specifier a_opt=ioption(acc_annot)
    { 
      env#set_access_spec (Ast.access_spec_of_node a);
      let al = opt_to_list a_opt in
      a#set_children al;
      a
    }
;
acc_annot:
| i=IDENT { mkleaf $startpos $endpos (L.AccessSpecAnnot i) }
;

(*mem_decl_seq:
| m=member_declaration ml_opt=ioption(mem_decl_seq) { m::(list_opt_to_list ml_opt) }
| a=mem_access_spec ml_opt=ioption(mem_decl_seq) { a::(list_opt_to_list ml_opt) }
;*)
mem_decl_seq:
| m=member_declaration { [m] }
| a=mem_access_spec { [a] }
| ml=mem_decl_seq m=member_declaration { ml @ [m] }
| ml=mem_decl_seq a=mem_access_spec { ml @ [a] }
;

pp_mdecl_if_section:
| p=pp_mdecl_if_group
    pl=list(pp_mdecl_elif_group) p_opt=ioption(pp_mdecl_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_mdecl_if_group:
| p=pp_ifx { mknode ~pvec:[1; 0] $startpos $endpos (pp_if_group()) [p] }
| p=pp_ifx ml=mem_decl_seq
    { mknode ~pvec:[1; List.length ml] $startpos $endpos (pp_if_group()) (p::ml) }
;
pp_mdecl_elif_group:
| p=pp_elif { mknode ~pvec:[1; 0] $startpos $endpos (_pp_elif_group p) [p] }
| p=pp_elif ml=mem_decl_seq
    { mknode ~pvec:[1; List.length ml] $startpos $endpos (_pp_elif_group p) (p::ml) }
;
pp_mdecl_else_group:
| p=pp_else { mknode ~pvec:[1; 0] $startpos $endpos (_pp_else_group p) [p] }
| p=pp_else ml=mem_decl_seq
    { mknode ~pvec:[1; List.length ml] $startpos $endpos (_pp_else_group p) (p::ml) }
;

pp_mdecl_if_section_broken:
| p=pp_mdecl_if_group_broken
    pl=list(pp_mdecl_elif_group_broken) p_opt=ioption(pp_mdecl_else_group_broken) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_mdecl_if_group_broken:
| p=pp_ifx ml=mem_decl_seq0 t=template_head f=func_head
    { 
      env#stack#exit_template();
      let d_ = mknode ~pvec:[1] $startpos(f) $endpos(f) L.FunctionDefinition [f] in
      let t_ = mknode ~pvec:[1; 1] $startpos(t) $endpos(f) L.TemplateDeclaration [t; d_] in
      mknode ~pvec:[1; List.length ml] $startpos $endpos (pp_if_group()) (p::ml@[t_])
    }
| p=pp_ifx f=func_head c_opt=ioption(ctor_initializer)
    { 
      let n_ =
        match c_opt with
        | Some c -> begin
            let b_ = mknode ~pvec:[1; 0] $startpos(c_opt) $endpos L.FunctionBody [c] in
            f#relab L.FunctionDefinition;
            f#add_children_r [b_];
            f#set_pvec (f#pvec @ [1]);
            reloc $startpos(f) $endpos f
        end
        | _ -> f
      in
      mknode $startpos $endpos (pp_if_group()) [p; n_]
    }
;
pp_mdecl_elif_group_broken:
| p=pp_elif ml=mem_decl_seq0 t=template_head f=func_head
    { 
      env#stack#exit_template();
      let d_ = mknode ~pvec:[1] $startpos(f) $endpos(f) L.FunctionDefinition [f] in
      let t_ = mknode ~pvec:[1; 1] $startpos(t) $endpos(f) L.TemplateDeclaration [t; d_] in
      mknode ~pvec:[1; List.length ml] $startpos $endpos (_pp_elif_group p) (p::ml@[t_])
    }
| p=pp_elif f=func_head c_opt=ioption(ctor_initializer)
    { 
      let n_ =
        match c_opt with
        | Some c -> begin
            let b_ = mknode ~pvec:[1; 0] $startpos(c_opt) $endpos L.FunctionBody [c] in
            f#relab L.FunctionDefinition;
            f#add_children_r [b_];
            f#set_pvec (f#pvec @ [1]);
            reloc $startpos(f) $endpos f
        end
        | _ -> f
      in
      mknode $startpos $endpos (_pp_elif_group p) [p; n_]
    }
;
pp_mdecl_else_group_broken:
| p=pp_else ml=mem_decl_seq0 t=template_head f=func_head
    { 
      env#stack#exit_template();
      let d_ = mknode ~pvec:[1] $startpos(f) $endpos(f) L.FunctionDefinition [f] in
      let t_ = mknode ~pvec:[1; 1] $startpos(t) $endpos(f) L.TemplateDeclaration [t; d_] in
      mknode ~pvec:[1; List.length ml] $startpos $endpos (_pp_else_group p) (p::ml@[t_])
    }
| p=pp_else f=func_head c_opt=ioption(ctor_initializer)
    { 
      let n_ =
        match c_opt with
        | Some c -> begin
            let b_ = mknode ~pvec:[1; 0] $startpos(c_opt) $endpos L.FunctionBody [c] in
            f#relab L.FunctionDefinition;
            f#add_children_r [b_];
            f#set_pvec (f#pvec @ [1]);
            reloc $startpos(f) $endpos f
        end
        | _ -> f
      in
      mknode $startpos $endpos (_pp_else_group p) [p; n_]
    }
;

%inline
mem_decl_seq0:
| ml_opt=ioption(mem_decl_seq) { list_opt_to_list ml_opt }
;

%inline
_member_declaration:
| al_opt=attribute_specifier_seq_opt ml_opt=ioption(member_declarator_list)
    { 
      let al = list_opt_to_list al_opt in
      let ml = list_opt_to_list ml_opt in
      let pvec = [List.length al; 0; List.length ml] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.MemberDeclarationDecl (al @ ml) in
      env#register_members nd;
      nd
    }
| al_opt=attribute_specifier_seq_opt dl=decl_specifier_seq ml_opt=ioption(member_declarator_list)
    { 
      let al = list_opt_to_list al_opt in
      let ml = list_opt_to_list ml_opt in
      let pvec = [List.length al; List.length dl; List.length ml] in
      let nd = mknode ~pvec $symbolstartpos $endpos L.MemberDeclarationDecl (al @ dl @ ml) in
      env#register_members nd;
      nd
    }
;

member_declaration:
| m=_member_declaration sc=SEMICOLON { if sc then m#add_suffix ";"; reloc $startpos $endpos m }
| f=function_definition { f }
| u=using_declaration { u }
| s=static_assert_declaration { s }
| t=template_declaration { t }
| d=deduction_guide { d }
| a=alias_declaration { a }
| o=opaque_enum_declaration { o }
(*| empty_declaration { }(*contained in the first line*)*)
| m=ms_pragma { m }
| p=pp_control_line { p }
| p=pp_mdecl_if_section { p }
| p=pp_mdecl_if_section_broken b=function_body
    { 
      env#clear_in_body_brace_flag();
      p#relab L.PpIfSectionAltFuncDef;
      p#add_children_r [b];
      p#set_pvec (p#pvec @ [1]);
      reloc $startpos $endpos p
    }
| p=pp_func_head_if_section_broken sl_opt=statement_seq_opt RBRACE
    { 
      let sl = list_opt_to_list sl_opt in
      p#add_children_r sl;
      p#set_pvec (p#pvec @ [List.length sl]);
      reloc $startpos $endpos p
    }
| d=decl_macro_call_ { d }
| d=decl_OR_stmt_macro_call_ { d }
| al=class_attr_spec_seq d=DECL_MACRO { mknode $startpos $endpos (L.DeclarationMacro d) al }
| al=class_attr_spec_seq d=DECL_MACRO s=compound_statement { mknode $startpos $endpos (L.DeclarationMacro d) (al @ [s]) }
| al=class_attr_spec_seq d=DECL_MACRO i=braced_init_list { mknode $startpos $endpos (L.DeclarationMacro d) (al @ [i]) }
| i=IDENT_EM ml=macro_args EQ b=braced_init_list
    { 
      let pvec = [0; List.length ml; 0; 0; 1] in
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) (ml @ [b])
    }
| i=IDENT_DSM SS_LPAREN m=member_declaration RPAREN
    { 
      let pvec = [0; 1; 0; 0; 0] in
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) [m]
    }
| ELLIPSIS { mkleaf $startpos $endpos L.Ellipsis }
| MS_PROPERTY t=type_id i=IDENT_V SEMICOLON { mknode ~pvec:[1; 0] $startpos $endpos (L.MsProperty i) [t] }
| MS_PROPERTY t=type_id i=IDENT_V INI_LBRACE m=member_specification RBRACE
    { mknode ~pvec:[1; 1] $startpos $endpos (L.MsProperty i) [t; m] }
;

decl_macro_call_:
| i=IDENT_V ml=macro_args
    { 
      let pvec = [0; List.length ml; 0; 0; 0] in
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) ml
    } %prec PREC
| i=IDENT_V ml=macro_args SEMICOLON
    { 
      let pvec = [0; List.length ml; 0; 0; 0] in
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) ml
    }
| d=decl_macro_call_ ml=macro_args
    { 
      let pvec = [1; List.length ml; 0; 0] in
      mknode ~pvec $startpos $endpos L.DeclarationMacroInvocationInvocation (d::ml)
    }
| d=decl_macro_call_ MINUS_GT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.DeclarationMacroInvocationArrow (d::tl@[i])
    }
| i=IDENT_V ml=macro_args vl_opt=virt_specifier_seq_opt c=compound_statement
    { 
      let vl = list_opt_to_list vl_opt in
      let pvec = [0; List.length ml; 0; List.length vl; 1] in
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) (ml @ vl @ [c])
    }
| d=decl_macro_call_ ioption(EQ) b=braced_init_list
    { 
      d#add_children_r [b];
      d#set_pvec (d#pvec @ [1]);
      reloc $startpos $endpos d
    }
| d=decl_macro_call_ EQ e=conditional_expression SEMICOLON
    { 
      d#add_children_r [e];
      d#set_pvec (d#pvec @ [1]);
      reloc $startpos $endpos d
    }
(*| i=IDENT_V ml=macro_args
    cl_opt=cv_qualifier_seq_opt vl_opt=virt_specifier_seq_opt c=compound_statement
    { 
      let cl = list_opt_to_list cl_opt in
      let vl = list_opt_to_list vl_opt in
      let pvec = [0; List.length ml; List.length cl; List.length vl; 1] in
      mknode ~pvec $startpos $endpos (L.DeclarationMacroInvocation i) (ml @ cl @ vl @ [c])
    }*)
;

%inline
macro_args:
| lp=LPAREN ml=macro_arg_list RPAREN
    { 
      ignore lp;
      begin
        match ml with
        | hd::_ when hd#label == L.EMPTY -> _reloc $startpos(lp) $endpos(lp) hd
        | _ -> ()
      end;
      ml
    }
;

macro_arg_list:
| m=macro_arg { [m] }
| a=arg_macro m=macro_arg { [a; m] }
| ml=macro_arg_list c=COMMA m=macro_arg
    { 
      ignore c;
      begin
        match m#label with
        | L.EMPTY -> _reloc $startpos(c) $endpos(c) m
        | _ -> ()
      end;
      ml @ [m]
    }
| ml=macro_arg_list COMMA s=selection_statement { ml @ [s] }
| ml=macro_arg_list COMMA MARKER sl=statement_seq { ml @ sl }
| ml=macro_arg_list COMMA LPAREN COMMA RPAREN
    { 
      let lp = mkleaf $startpos $endpos L.Lparen in
      let rp = mkleaf $startpos $endpos L.Rparen in
      ml @ [lp; rp]
    }
| ml=macro_arg_list i=DELIM_MACRO m=macro_arg
    { [mknode $startpos $endpos (L.DELIM_MACRO i) (ml @ [m])] }
;

swift_arg:
| i=IDENT_V COLON { mkleaf $startpos $endpos (L.SwiftArg i) }
;

macro_arg:
| r=restricted_postfix_expr i=identifier
    { mknode $startpos $endpos L.AMBIGUOUS_CONSTRUCT [r; i] }

| r=restricted_postfix_expr MINUS_GT
    { mknode ~pvec:[1; 0; 0] $startpos $endpos L.PostfixExpressionArrow [r] }
| r=restricted_postfix_expr DOT
    { mknode ~pvec:[1; 0; 0] $startpos $endpos L.PostfixExpressionDot [r] }

| r=restricted_postfix_expr LPAREN al=nonempty_list(swift_arg) RPAREN (* Swift function-call *)
    { mknode ~pvec:[1; 1] $startpos $endpos L.SwiftFunCall (r::al) }

| r=restricted_lor_expr ao=assignment_operator { mknode ~pvec:[1; 0] $startpos $endpos ao [r] }
| i0=int_literal i1=int_literal { mknode $startpos $endpos L.MacroArgument [i0; i1] }
(*| al=arg_macro+ { mknode $startpos $endpos L.MacroArgument al }*)
| p=parameters_and_qualifiers { p }
| i=identifier p=parameters_and_qualifiers
    { mknode ~pvec:[1; 1] $startpos $endpos L.NoptrDeclaratorFunc [i; p] }
| n=nested_name_specifier i=identifier p=parameters_and_qualifiers
    { 
      let q = mknode ~pvec:[1; 1] $startpos $endpos L.QualifiedId [n; i] in
      let n = mknode ~pvec:[1; 0] $startpos $endpos L.NoptrDeclaratorId [q] in
      mknode ~pvec:[1; 1] $startpos $endpos L.NoptrDeclaratorFunc [n; p]
    }
| i=IDENT_V p=parameters_and_qualifiers b=function_body
    { 
      let i_ = mkleaf $startpos $endpos(i) (L.Identifier i) in
      let d = mknode ~pvec:[1; 1] $startpos $endpos L.NoptrDeclaratorFunc [i_; p] in
      mknode ~pvec:[0; 1; 0; 1] $startpos $endpos L.FunctionDefinition [d; b]
    }
| o=operator { o }
| EQ i=initializer_clause { mknode $startpos $endpos L.EqualInitializer [i] }
| LPAREN o=operator RPAREN
    { 
      let n = mknode $startpos $endpos L.MacroArgument [o] in
      n#add_prefix "(";
      n#add_suffix ")";
      n
    }
| LPAREN o=shift_op e=additive_expression RPAREN
    { 
      let o_ = mkleaf $startpos(o) $endpos(o) o in
      o_#add_prefix "(";
      e#add_suffix ")";
      mknode $startpos $endpos L.MacroArgument [o_; e]
    }
| DOT { mkleaf $startpos $endpos L.Dot }
| t=type_parameter { t }
| t=template_head mid_typaram f=func_head { mknode ~pvec:[1; 1] $startpos $endpos L.TemplateDeclaration [t; f] }
| DEFAULT { mkleaf $startpos $endpos L.Default }
| ELLIPSIS { mkleaf $startpos $endpos L.Ellipsis }
| GOTO i=IDENT { mkleaf $startpos $endpos (L.GotoStatement i) }
| (* empty *) { Ast.empty_node }
| s=DOXYGEN_CMD { mkleaf $startpos $endpos (L.DoxygenLine s) }
| s=DOXYGEN_LINE { mkleaf $startpos $endpos (L.DoxygenLine s) }
| t=top_stmts { t }
| c=compound_statement { c }
| BEGIN_QPROP ql=q_prop_token+ END_QPROP { mknode $startpos(ql) $endpos(ql) L.Q_PROPERTY ql }
| rl=restricted_decls { mknode $startpos $endpos L.DECLS rl }
| BASE_COLON b=base_specifier { mknode $symbolstartpos $endpos L.BaseClause [b] }
| m=ms_pragma { m }
| PERC LBRACKET i=IDENT_V RBRACKET { mkleaf $startpos $endpos (L.AsmName i) }
| n=nested_name_specifier { n }
| a=asm_block { a }
| b=braced_init_list { b }
| TY_LPAREN p=ptr_declarator RPAREN { mknode $startpos $endpos L.NoptrDeclaratorParen [p] }
| l=label { l }
| n=noexcept { n }
| a=gnu_attribute { a }
| TEMPL_LT tl_opt=template_argument_list_opt TEMPL_GT
    { mknode $startpos $endpos L.TemplateArguments (list_opt_to_list tl_opt) }
| c=cast_key { mknode ~pvec:[0; 0] $startpos $endpos c [] }

| SEMICOLON { mkleaf $startpos $endpos L.EmptyDeclaration }
| sc=SEMICOLON i=identifier op=fold_operator
    { 
      ignore sc;
      let s_ = mkleaf $startpos $endpos(sc) L.Semicolon in
      mknode $startpos $endpos L.MacroArgument [s_; i; op]
    }
| sl=stmts_macro_arg s_opt=ioption(SEMICOLON)
    { 
      let last = Xlist.last sl in
      begin
        match s_opt with
        | Some true -> begin
            last#add_suffix ";";
            _reloc_end $endpos last
        end
        | _ -> ()
      end;
      match sl with
      | [s] -> s
      | _ -> mknode $startpos $endpos L.MacroArgument sl
    }
| p=pp_expr_if_section { p }
;

stmts_macro_arg:
| s=stmt_macro_arg { [s] }
| sl=stmts_macro_arg sc=SEMICOLON s=stmt_macro_arg
    { 
      ignore sc;
      let last = Xlist.last sl in
      last#add_suffix ";";
      _reloc_end $endpos(sc) last;
      sl @ [s]
    }
| sl=stmts_macro_arg sc=SEMICOLON p=pp_stmt_if_section s=stmt_macro_arg
    { 
      ignore sc;
      let last = Xlist.last sl in
      last#add_suffix ";";
      _reloc_end $endpos(sc) last;
      sl @ [p;s]
    }
;
stmt_macro_arg:
| r=restricted_expr { r }
| p=parameter_declaration { p }
| RETURN e_opt=ioption(constant_expression) { mknode $startpos $endpos L.ReturnStatement (opt_to_list e_opt) }
;

%inline
ms_pragma:
| p=MS_PRAGMA LPAREN wl=ms_warn_spec_list RPAREN { mknode $startpos $endpos (L.MsPragma p) wl }
;

ms_warn_spec_list:
| w=ms_warn_spec { [w] }
| wl=ms_warn_spec_list w=ms_warn_spec { wl @ [w] }
;
ms_warn_spec:
| i=IDENT_V COLON il=nonempty_list(int_literal) { mknode $startpos $endpos (L.MsWarningSpecifier i) il }
| i=IDENT_V COLON s=identifier { mknode $startpos $endpos (L.MsWarningSpecifier i) [s] }
;
%inline
int_literal:
| i=INT_LITERAL { mkleaf $startpos $endpos (L.IntegerLiteral i) }
;

%inline
named_namespace_definition_head_:
| h=named_namespace_definition_head
    { 
      let il, al, i, ml = h in
      let pvec = [List.length il; List.length al; List.length ml] in
      mknode ~pvec $startpos(h) $endpos (L.NamedNamespaceDefinitionHead i) (il @ al @ ml)
    }
;

restricted_decls:
| r=restricted_decl { [r] }
| h=named_namespace_definition_head_ { [h] }
| rl=restricted_decls r=restricted_decl { rl @ [r] }
| rl=restricted_decls h=named_namespace_definition_head_ { rl @ [h] }
;

restricted_decl:
| u=_using_directive SEMICOLON { u }
| n=namespace_definition SEMICOLON { n }
| e=explicit_instantiation { e }
| e=explicit_specialization { e }
| i=IDENT_DSM ml=macro_args { mknode $startpos $endpos (L.DeclarationMacroInvocation i) ml }
| d=DECL_MACRO { mkleaf $startpos $endpos (L.DeclarationMacro d) }
;

%inline
q_prop_token:
| i=IDENT        { mkleaf $startpos $endpos (L.Identifier i) }
| i=IDENT_V      { mkleaf $startpos $endpos (L.Identifier i) }
| b=BOOL_LITERAL { mkleaf $startpos $endpos (L.BooleanLiteral b) }
| i=INT_LITERAL  { mkleaf $startpos $endpos (L.IntegerLiteral i) }
;

restricted_prim_expr:
| l=literal { l }
| THIS { mkleaf $startpos $endpos L.This }
| i=id_expression { i }
| l=lambda_expression { l }
| f=fold_expression { f }
| r=requires_expression { r }
;
restricted_postfix_expr:
| r=restricted_prim_expr { r }
| r=restricted_postfix_expr LBRACKET e=expr_or_braced_init_list RBRACKET
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionSubscr [r; e] }
| r=restricted_postfix_expr LBRACKET RBRACKET
    { mknode ~pvec:[1; 0] $startpos $endpos L.PostfixExpressionSubscr [r] }
| LBRACKET e=expr_or_braced_init_list RBRACKET
    { mknode ~pvec:[0; 1] $startpos $endpos L.PostfixExpressionSubscr [e] }
| r=restricted_postfix_expr LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      if el_opt <> None then begin
        r#add_suffix "(";
        (Xlist.last el)#add_suffix ")"
      end
      else
        r#add_suffix "()";
      mknode ~pvec:[1; List.length el] $startpos $endpos L.PostfixExpressionFunCall (r::el)
    }
| s=simple_type_specifier LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el] in
      let lab =
        match s#label with
        | L.SimpleTypeSpecifier x when env#scanner_keep_flag && s#nchildren = 0 -> begin
            s#relab (L.Identifier s#get_name);
            L.PostfixExpressionFunCall
        end
        | _ -> L.PostfixExpressionExplicitTypeConvExpr
      in
      mknode ~pvec $startpos $endpos lab (s::el)
    }
| t=typename_specifier LPAREN el_opt=expression_list_opt RPAREN
    { 
      let el = list_opt_to_list el_opt in
      let pvec = [1; List.length el] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionExplicitTypeConvExpr (t::el)
    }
| s=simple_type_specifier b=braced_init_list
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionExplicitTypeConvBraced [s; b] }
| t=typename_specifier b=braced_init_list
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionExplicitTypeConvBraced [t; b] }
| r=restricted_postfix_expr DOT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionDot (r :: tl @ [i])
    }
| r=restricted_postfix_expr DOT t_opt=ioption(template) op=OP_MACRO i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 0] in
      let e = mknode ~pvec $startpos $endpos L.PostfixExpressionDot (r :: tl) in
      mknode $startpos $endpos (L.OperatorMacro op) [e; i]
    }
| r=restricted_postfix_expr MINUS_GT t_opt=ioption(template) MARKER
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 0] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (r :: tl)
    }
| r=restricted_postfix_expr MINUS_GT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (r :: tl @ [i])
    }
| r=restricted_postfix_expr MINUS_GT t_opt=ioption(template) op=OP_MACRO i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [1; List.length tl; 0] in
      let e = mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (r :: tl) in
      mknode $startpos $endpos (L.OperatorMacro op) [e; i]
    }

| DOT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [0; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionDot (tl @ [i])
    }
| MINUS_GT t_opt=ioption(template) i=id_expression
    { 
      let tl = opt_to_list t_opt in
      let pvec = [0; List.length tl; 1] in
      mknode ~pvec $startpos $endpos L.PostfixExpressionArrow (tl @ [i])
    }

| r=restricted_postfix_expr PLUS_PLUS
    { mknode $startpos $endpos L.PostfixExpressionIncr [r] }
| r=restricted_postfix_expr MINUS_MINUS
    { mknode $startpos $endpos L.PostfixExpressionDecr [r] }
| c=cast_key TEMPL_LT t=type_id TEMPL_GT LPAREN e=expression RPAREN
    { 
      e#add_prefix "(";
      e#add_suffix ")";
      mknode ~pvec:[1; 1] $startpos $endpos c [t; e]
    }
| TYPEID LPAREN e=expression RPAREN
    { mknode $startpos $endpos L.PostfixExpressionTypeidExpr [e] }
| TYPEID TY_LPAREN t=type_id RPAREN
    { mknode $startpos $endpos L.PostfixExpressionTypeidTy [t] }
| e=expr_macro_call { e }
| p=restricted_postfix_expr a=args_macro
    { mknode ~pvec:[1; 1] $startpos $endpos L.PostfixExpressionFunCall [p; a] }
| LPAREN ioption(COMMA) el=expression_list RPAREN
    { mknode ~pvec:[List.length el; 0] $startpos $endpos L.ExpressionList el }
| LPAREN ioption(COMMA) el=expression_list RPAREN s=string_literal_
    { mknode ~pvec:[List.length el; 1] $startpos $endpos L.ExpressionList (el@[s]) }
| LPAREN RPAREN { mknode ~pvec:[0; 0] $startpos $endpos L.ExpressionList [] }
;

restricted_unary_expr:
| r=restricted_postfix_expr { r }
| uo=unary_operator c=cast_expression { mknode $startpos $endpos uo [c] }
| uo=op_macro_call c=cast_expression
    { 
      uo#add_children_r [c];
      uo#set_pvec (uo#pvec @ [1]);
      reloc $startpos $endpos uo
    }
| PLUS_PLUS c=cast_expression { mknode $startpos $endpos L.UnaryExpressionIncr [c] }
| MINUS_MINUS c=cast_expression { mknode $startpos $endpos L.UnaryExpressionDecr [c] }
| a=await_expression { a }
| e=sizeof_expr { e }
| ALIGNOF TY_LPAREN t=type_id RPAREN { mknode $startpos $endpos L.UnaryExpressionAlignof [t] }
| n=noexcept_expression { n }
| n=new_expression { n }
| d=delete_expression { d }
;
%inline
sizeof_expr:
| SIZEOF u=unary_expression { mknode $startpos $endpos L.UnaryExpressionSizeof [u] }
| SIZEOF TY_LPAREN t=type_id RPAREN
    { 
      t#add_prefix "(";
      t#add_suffix ")";
      mknode $startpos $endpos L.UnaryExpressionSizeof [t]
    }
| SIZEOF ELLIPSIS LPAREN i=IDENT RPAREN { mkleaf $startpos $endpos (L.UnaryExpressionSizeofPack i) }
;
restricted_cast_expr:
| r=restricted_unary_expr { r }
| p=parameters_and_qualifiers e=restricted_postfix_expr
    { 
      p#relab L.TypeId;
      mknode ~pvec:[0; 1; 1] $symbolstartpos $endpos L.CastExpression [p; e]
    } (* contains cast_expr *)
| p=parameters_and_qualifiers p0=parameters_and_qualifiers e=restricted_postfix_expr
    { 
      p#relab L.TypeId;
      p0#relab L.TypeId;
      let e0 = mknode ~pvec:[0; 1; 1] $startpos(p0) $endpos L.CastExpression [p0; e] in
      mknode ~pvec:[0; 1; 1] $symbolstartpos $endpos L.CastExpression [p; e0]
    } (* contains cast_expr *)
| p=parameters_and_qualifiers e=sizeof_expr
    { 
      p#relab L.TypeId;
      mknode ~pvec:[0; 1; 1] $symbolstartpos $endpos L.CastExpression [p; e]
    } (* contains cast_expr *)
| p=parameters_and_qualifiers uo=restricted_unary_op c=cast_expression
    { 
      let e = mknode $startpos(uo) $endpos uo [c] in
      p#relab L.TypeId;
      mknode ~pvec:[0; 1; 1] $symbolstartpos $endpos L.CastExpression [p; e]
    }
;
%inline
restricted_unary_op:
| STAR       { Ast.L.UnaryExpressionInd }
(*| AMP        { Ast.L.UnaryExpressionAddr }*)
(*| AMP_AMP    { Ast.L.UnaryExpressionLabelAddr }*)
| PLUS       { Ast.L.UnaryExpressionPlus }
| MINUS      { Ast.L.UnaryExpressionMinus }
| e=EXCLAM   { Ast.L.UnaryExpressionNeg e }
| t=TILDE    { Ast.L.UnaryExpressionCompl t }
| o=OP_MACRO { Ast.L.OperatorMacro o }
;
restricted_pm_expr:
| r=restricted_cast_expr { r }
| r=restricted_pm_expr po=pm_op c=cast_expression
    { mknode ~pvec:[1; 1] $startpos $endpos po [r; c] }
;
restricted_mult_expr:
| r=restricted_pm_expr { r }
| r=restricted_mult_expr mo=multiplicative_op p=pm_expression
    { mknode ~pvec:[1; 1] $startpos $endpos mo [r; p] }
;
restricted_add_expr:
| r=restricted_mult_expr { r }
| r=restricted_add_expr ao=additive_op m=multiplicative_expression
    { mknode ~pvec:[1; 1] $startpos $endpos ao [r; m] } 
| r=restricted_add_expr i=SUFFIX_MACRO { mknode $startpos $endpos (L.SuffixMacro i) [r] }
| r=restricted_add_expr op=op_macro_call
    { 
      op#add_children_l [r];
      let pvec =
        match op#pvec with
        | _::tl -> 1::tl
        | _ -> assert false
      in
      op#set_pvec pvec;
      reloc $startpos $endpos op
    }
;
restricted_shift_expr:
| r=restricted_add_expr { r }
| r=restricted_shift_expr so=shift_op a=additive_expression
    { mknode ~pvec:[1; 1] $startpos $endpos so [r; a] }
;
restricted_comp_expr:
| r=restricted_shift_expr { r }
| r=restricted_comp_expr LT_EQ_GT s=shift_expression
    { mknode ~pvec:[1; 1] $startpos $endpos L.CompareExpression [r; s] }
;
restricted_rel_expr:
| r=restricted_comp_expr { r }
| r=restricted_rel_expr ro=relational_op c=compare_expression
    { mknode ~pvec:[1; 1] $startpos $endpos ro [r; c] }
;
restricted_eq_expr:
| r=restricted_rel_expr { r }
| r=restricted_eq_expr eo=equality_op re=relational_expression
    { mknode ~pvec:[1; 1] $startpos $endpos eo [r; re] }
;
restricted_and_expr:
| r=restricted_eq_expr { r }
| r=restricted_and_expr a=AMP e=equality_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.AndExpression a) [r; e] }
;
restricted_xor_expr:
| r=restricted_and_expr { r }
| r=restricted_xor_expr h=HAT a=and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.ExclusiveOrExpression h) [r; a] }
;
restricted_ior_expr:
| r=restricted_xor_expr { r }
| r=restricted_ior_expr b=BAR e=exclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.InclusiveOrExpression b) [r; e] }
;
restricted_land_expr:
| r=restricted_ior_expr { r }
| r=restricted_land_expr a=AMP_AMP i=inclusive_or_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalAndExpression a) [r; i] }
;
restricted_lor_expr:
| r=restricted_land_expr { r }
| r=restricted_lor_expr b=BAR_BAR l=logical_and_expression
    { mknode ~pvec:[1; 1] $startpos $endpos (L.LogicalOrExpression b) [r; l] }
;
restricted_cond_expr:
| r=restricted_lor_expr { r }
| r=restricted_lor_expr QUEST e=expression COLON a=assignment_expression
    { 
      r#add_suffix " ?";
      e#add_suffix " :";
      mknode ~pvec:[1; 1; 1] $startpos $endpos L.ConditionalExpression [r; e; a]
    }
;
(*restricted_const_expr:
| restricted_cond_expr { }
;*)
restricted_assign_expr:
| r=restricted_cond_expr { r }
| r=restricted_lor_expr ao=assignment_operator i=initializer_clause
    { mknode ~pvec:[1; 1] $startpos $endpos ao [r; i] }
| t=throw_expression { t }
| y=yield_expression { y }
;
restricted_expr:
| r=restricted_assign_expr { r }
;

member_declarator_list:
| m=member_declarator { [m] }
| ml=member_declarator_list COMMA m=member_declarator { ml @ [m] }
| ml=member_declarator_list COMMA p=pp_control_line m=member_declarator { ml @ [p; m] }
;

member_declarator:
| d=declarator vl_opt=ioption(virt_specifier_seq) p_opt=ioption(pure_specifier)
    { 
      (*env#stack#exit_function_if_any();*)
      let vl = list_opt_to_list vl_opt in
      let pl = opt_to_list p_opt in
      let pvec = [1; List.length vl; List.length pl; 0; 0] in
      mknode ~pvec $startpos $endpos L.MemberDeclaratorDecl (d :: vl @ pl)
    }
| d=declarator r=requires_clause
    { 
      (*env#stack#exit_function_if_any();*)
      mknode ~pvec:[1; 0; 0; 1; 0] $startpos $endpos L.MemberDeclaratorDecl [d; r]
    }
(*| declarator (*ioption(brace_or_equal_initializer)*) { }(*contained in the first case*)*)
| d=declarator b=brace_or_equal_initializer
    { mknode ~pvec:[1; 0; 0; 0; 1] $startpos $endpos L.MemberDeclaratorDecl [d; b] }

| (*attribute_specifier_seq_opt*) COLON
    c=constant_expression b_opt=ioption(brace_or_equal_initializer)
    { 
      let bl = opt_to_list b_opt in
      let pvec = [0; 1; List.length bl] in
      mknode ~pvec $startpos $endpos (L.MemberDeclaratorBitField "") (c::bl)
    }
| i=IDENT_B al_opt=attribute_specifier_seq_opt COLON
    c=constant_expression b_opt=ioption(brace_or_equal_initializer)
    { 
      let al = list_opt_to_list al_opt in
      let bl = opt_to_list b_opt in
      let pvec = [List.length al; 1; List.length bl] in
      mknode ~pvec $startpos $endpos (L.MemberDeclaratorBitField i) (al @ c :: bl)
    }
;

pure_specifier:
| EQ PURE_ZERO { mkleaf $startpos $endpos L.PureSpecifier }
;

deduction_guide:
| t=template_name LPAREN p=parameter_declaration_clause RPAREN
    MINUS_GT s=simple_template_id SEMICOLON
    { mknode ~pvec:[0; 1; 1] $startpos $endpos (L.DeductionGuide t) [p; s] }
(*| e=explicit_specifier t=template_name LPAREN p=parameter_declaration_clause RPAREN
    MINUS_GT s=simple_template_id SEMICOLON
    { mknode ~pvec:[1; 1; 1] $startpos $endpos (L.DeductionGuide t) [e; p; s] }*)(*contained in declarator?*)
;
(*%inline
explicit:
| EXPLICIT { mkleaf $startpos $endpos L.Explicit }
;*)

empty_declaration:
| sc=SEMICOLON
    { 
      let lab =
        if sc then
          L.EmptyDeclaration
        else
          L.DummyDecl
      in
      mkleaf $startpos $endpos lab
    }
;

opaque_enum_declaration:
| o=_opaque_enum_declaration sc=SEMICOLON { if sc then o#add_suffix ";"; reloc $startpos $endpos o }
;

alias_declaration:
| a=_alias_declaration sc=SEMICOLON { if sc then a#add_suffix ";"; reloc $startpos $endpos a }
;

static_assert_declaration:
| s=_static_assert_declaration sc=SEMICOLON { if sc then s#add_suffix ";"; reloc $startpos $endpos s }
;

using_declaration:
| u=_using_declaration sc=SEMICOLON { if sc then u#add_suffix ";"; reloc $startpos $endpos u }
;

_using_declaration:
| USING ul=using_declarator_list
    { 
      let nd = mknode $startpos $endpos L.UsingDeclaration ul in
      env#register_using_decls nd;
      nd
    }
;

using_declarator_list:
| u=using_declarator { [u] }
| u=using_declarator ELLIPSIS
    { 
      let u_ = mknode $startpos $endpos L.PackExpansion [u] in
      u_#add_suffix "...";
      [u_]
    }
| ul=using_declarator_list COMMA u=using_declarator { ul @ [u] }
| ul=using_declarator_list COMMA u=using_declarator ELLIPSIS
    { 
      let u_ = mknode $startpos(u) $endpos L.PackExpansion [u] in
      u_#add_suffix "...";
      ul @ [u_]
    }
;

using_declarator:
| t_opt=ioption(typename) n=nested_name_specifier u=unqualified_id
    { 
      (*let pi = (Ast.encode_of_nested_name_spec n)^(Ast.uqn_of_unqualified_id u) in*)
      let tl = opt_to_list t_opt in
      mknode ~pvec:[List.length tl; 1; 1] $symbolstartpos $endpos L.UsingDeclarator (tl @ [n; u])
    }
;
%inline
typename:
| TYPENAME { mkleaf $startpos $endpos L.Typename }
;

ms_attr:
| MS_ATTR_LBRACKET al=attribute_list RBRACKET { mknode $startpos $endpos L.MsAttributeSpecifier al }
;
%inline
ms_attrs:
| { [] }
| m=ms_attr                    { [m] }
| m=ms_attr a=access_specifier { [m; a] }
|           a=access_specifier { [a] }
;

class_specifier:
| al=ms_attrs c=class_head CLASS_LBRACE m_opt=ioption(member_specification) RBRACE
    { 
      let ml = opt_to_list m_opt in
      mknode ~pvec:[List.length al; 1; List.length ml] $symbolstartpos $endpos L.ClassSpecifier (al@c::ml)
    }
| p=pp_class_head_if_section CLASS_LBRACE m_opt=ioption(member_specification) RBRACE
    { 
      let ml = opt_to_list m_opt in
      mknode ~pvec:[0; 1; List.length ml] $startpos $endpos L.ClassSpecifier (p::ml)
    }
| p=pp_class_head_if_section_broken m_opt=ioption(member_specification) RBRACE
    { 
      let ml = opt_to_list m_opt in
      mknode ~pvec:[0; 1; List.length ml] $startpos $endpos L.ClassSpecifier (p::ml)
    }
| c=class_head p=pp_class_body_if_section
    { 
      mknode ~pvec:[0; 1; 1] $startpos $endpos L.ClassSpecifier [c; p]
    }
;

%inline
class_body:
| CLASS_LBRACE m_opt=ioption(member_specification) RBRACE
    { mknode $startpos $endpos L.ClassBody (opt_to_list m_opt) }
;

pp_class_head_if_section:
| p=pp_class_head_if_group
    pl=list(pp_class_head_elif_group) p_opt=ioption(pp_class_head_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_class_head_if_group:
| p=pp_ifx_c c=class_head { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c] }
;
pp_class_head_elif_group:
| p=pp_elif c=class_head
    { 
      env#stack#exit_class();
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c]
    }
;
pp_class_head_else_group:
| p=pp_else c=class_head
    { 
      env#stack#exit_class();
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c]
    }
;

pp_class_head_if_section_broken:
| p=pp_class_head_if_group_broken
    pl=list(pp_class_head_elif_group_broken) p_opt=ioption(pp_class_head_else_group_broken) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_class_head_if_group_broken:
| p=pp_ifx_c c=class_head CLASS_LBRACE ml=mem_decl_seq0
    { mknode ~pvec:[1; 1; List.length ml] $startpos $endpos (pp_if_group()) (p::c::ml) }
;
pp_class_head_elif_group_broken:
| p=pp_elif c=class_head CLASS_LBRACE ml=mem_decl_seq0
    { 
      env#pstat#close_brace();
      env#stack#exit_class();
      mknode ~pvec:[1; 1; List.length ml] $startpos $endpos (_pp_elif_group p) (p::c::ml)
    }
;
pp_class_head_else_group_broken:
| p=pp_else c=class_head CLASS_LBRACE ml=mem_decl_seq0
    { 
      env#pstat#close_brace();
      env#stack#exit_class();
      mknode ~pvec:[1; 1; List.length ml] $startpos $endpos (_pp_else_group p) (p::c::ml)
    }
;

pp_class_body_if_section:
| p=pp_class_body_if_group
    pl=list(pp_class_body_elif_group) p_opt=ioption(pp_class_body_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_class_body_if_group:
| p=pp_ifx_cb c=class_body
    { mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; c] }
;
pp_class_body_elif_group:
| p=pp_elif c=class_body
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; c]
    }
;
pp_class_body_else_group:
| p=pp_else c=class_body
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; c]
    }
;

class_head:
| c=class_key al=class_attr_spec_seq ch=class_head_name
    c_opt=ioption(class_virt_specifier) b_opt=ioption(base_clause)
    { 
      let lab = L.ClassKey.to_class_head c in
      let cl = opt_to_list c_opt in
      let bl = opt_to_list b_opt in
      let pvec = [List.length al; 1; List.length cl; List.length bl] in
      let nd = mknode ~pvec $startpos $endpos lab (al @ [ch] @ cl @ bl) in
      let qn = env#register_class_head nd in
      env#stack#enter_class qn;
      nd
    }
| c=class_key al=class_attr_spec_seq b_opt=ioption(base_clause)
    { 
      let lab = L.ClassKey.to_class_head c in
      let bl = opt_to_list b_opt in
      let pvec = [List.length al; 0; 0; List.length bl] in
      let nd = mknode ~pvec $startpos $endpos lab (al @ bl) in
      let qn = env#register_class_head nd in
      env#stack#enter_class qn;
      nd
    }
| c=class_head_macro al=class_attr_spec_seq ch=class_head_name b_opt=ioption(base_clause)
    { 
      let bl = opt_to_list b_opt in
      c#add_children_r (al@ch::bl);
      c#set_pvec (c#pvec@[List.length al; 1; List.length bl]);
      let x = env#register_class_head c in
      env#stack#enter_class x;
      reloc $startpos $endpos c
    }
| c=class_head_macro
    { 
      c#set_pvec (c#pvec@[0; 0; 0]);
      let x = env#register_class_head c in
      env#stack#enter_class x;
      c
    }
;

%inline
class_head_macro:
| i=IDENT_CHM ml=macro_args
    { 
      let pvec = [List.length ml] in
      mknode ~pvec $startpos $endpos (L.ClassHeadMacroInvocation i) ml
    }
| i=CLASS_HEAD_MACRO { mkleaf $startpos $endpos (L.ClassHeadMacro i) }
;

class_head_name:
|                         c=class_name { c }
| n=nested_name_specifier c=class_name
    { 
      let p = Ast.encode_nested_name_spec n in
      let uqn = Ast.uqn_of_class_name c in
      let qn = p^uqn in
      mknode ~pvec:[1; 1] $startpos $endpos (L.ClassHeadName qn) [n; c]
    }
| i=id_macro_call { i }
;

class_name:
| i=IDENT
    { 
      let uqn = Ast.encode_ident i in
      mkleaf $startpos $endpos (L.ClassName uqn)
    }
| s=simple_template_id
    { 
      let uqn = Ast.uqn_of_simple_template_id s in
      mknode $startpos $endpos (L.ClassName uqn) [s]
    }
;

class_virt_specifier:
| FINAL { mkleaf $startpos $endpos L.ClassVirtSpecifierFinal }
| MS_SEALED { mkleaf $startpos $endpos L.ClassVirtSpecifierMsSealed }
| i=VIRT_SPEC_MACRO { mkleaf $startpos $endpos (L.VirtSpecifierMacro i) }
;


base_clause:
| BASE_COLON b=_base_clause { b#add_prefix ": "; reloc $startpos $endpos b }
| b_opt=ioption(BASE_COLON) p=pp_base_clause_if_section bl_opt=ioption(base_specifier_list)
    { 
      let bl = list_opt_to_list bl_opt in
      let n = mknode ~pvec:[1; List.length bl] $symbolstartpos $endpos L.BaseClause (p::bl) in
      if b_opt <> None then
        n#add_prefix ":";
      n
    }
;
%inline
_base_clause:
| (*BASE_COLON*) bl=base_specifier_list { mknode $startpos $endpos L.BaseClause bl }
;

pp_base_clause_if_section:
| p=pp_base_clause_if_group
    pl=list(pp_base_clause_elif_group) p_opt=ioption(pp_base_clause_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_base_clause_if_group:
| p=pp_ifx c_opt=ioption(BASE_COLON) ioption(COMMA) b=_base_clause ioption(COMMA)
    { 
      ignore c_opt;
      _reloc $startpos(c_opt) $endpos(b) b;
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [p; b]
    }
;
pp_base_clause_elif_group:
| p=pp_elif c_opt=ioption(BASE_COLON) ioption(COMMA) b=_base_clause ioption(COMMA)
    { 
      ignore c_opt;
      _reloc $startpos(c_opt) $endpos(b) b;
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_elif_group p) [p; b]
    }
;
pp_base_clause_else_group:
| p=pp_else c_opt=ioption(BASE_COLON) ioption(COMMA) b=_base_clause ioption(COMMA)
    { 
      ignore c_opt;
      _reloc $startpos(c_opt) $endpos(b) b;
      mknode ~pvec:[1; 1] $startpos $endpos (_pp_else_group p) [p; b]
    }
;


base_specifier_list:
| b=base_specifier { [b] }
| b=base_specifier ELLIPSIS
    { 
      let b_ = mknode $startpos $endpos L.PackExpansion [b] in
      b_#add_suffix "...";
      [b_]
    }
| bl=base_specifier_list COMMA b=base_specifier
    { 
      (Xlist.last bl)#add_suffix ",";
      bl @ [b]
    }
| bl=base_specifier_list COMMA p=pp_base_clause_if_section
    { 
      (Xlist.last bl)#add_suffix ",";
      bl @ [p]
    }
| bl=base_specifier_list COMMA p=pp_base_clause_if_section b=base_specifier
    { 
      (Xlist.last bl)#add_suffix ",";
      bl @ [p; b]
    }
| bl=base_specifier_list COMMA b=base_specifier ELLIPSIS
    { 
      let b_ = mknode $startpos(b) $endpos L.PackExpansion [b] in
      b_#add_suffix "...";
      (Xlist.last bl)#add_suffix ",";
      bl @ [b_]
    }
| bl=base_specifier_list p=pp_base_clause_if_section { bl @ [p] }
| bl=base_specifier_list b=base_spec_macro { bl @ [b] }
| bl=base_specifier_list b0=base_spec_macro b1=base_specifier { bl @ [b0; b1] }
| bl=base_specifier_list p=pp_base_clause_if_section b=base_specifier { bl @ [p; b] }
;

%inline
base_spec_macro:
| i=BASE_SPEC_MACRO { mkleaf $startpos $endpos (L.BaseSpecMacro i) }
;

base_specifier:
| al_opt=attribute_specifier_seq_opt c=class_or_decltype
    { 
      let al = list_opt_to_list al_opt in
      let pvec = [List.length al; 0; 0; 0; 1] in
      mknode ~pvec $symbolstartpos $endpos L.BaseSpecifier (al @ [c])
    }
| al_opt=attribute_specifier_seq_opt
    v=virtual_ a_opt=ioption(access_specifier) c=class_or_decltype
    { 
      let al = list_opt_to_list al_opt in
      let al1 = opt_to_list a_opt in
      let pvec = [List.length al; 0; 1; List.length al1; 1] in
      mknode ~pvec $symbolstartpos $endpos L.BaseSpecifier (al @ v :: al1 @ [c])
    }
| al_opt=attribute_specifier_seq_opt
    a=access_specifier v_opt=ioption(virtual_) c=class_or_decltype
    { 
      let al = list_opt_to_list al_opt in
      let vl = opt_to_list v_opt in
      let pvec = [List.length al; 1; List.length vl; 0; 1] in
      mknode ~pvec $symbolstartpos $endpos L.BaseSpecifier (al @ a :: vl @ [c])
    }
;
%inline
virtual_:
| VIRTUAL { mkleaf $startpos $endpos L.Virtual }
;

access_specifier:
| PRIVATE   { mkleaf $startpos $endpos L.Private }
| PROTECTED { mkleaf $startpos $endpos L.Protected }
| PUBLIC    { mkleaf $startpos $endpos L.Public }
| a=ACC_SPEC_MACRO { mkleaf $startpos $endpos (L.AccessSpecMacro a) }
;

class_key:
| CLASS       { L.ClassKey.Class }
| ELAB_CLASS  { L.ClassKey.Class }
| STRUCT      { L.ClassKey.Struct }
| UNION       { L.ClassKey.Union }
| MS_REF CLASS { L.ClassKey.MsRefClass }
;

typename_specifier:
| TYPENAME n=nested_name_specifier i=IDENT
    { 
      let i_ = mkleaf $startpos(i) $endpos (L.Identifier i) in
      let uqn = Ast.encode_ident i in
      let p = (Ast.encode_nested_name_spec n) in
      let nd = mknode ~pvec:[1; 0; 1] $startpos $endpos (L.TypenameSpecifier uqn) [n; i_] in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| TYPENAME i=id_macro_call
    { 
      let uqn = Ast.encode_ident i#get_name in
      let nd = mknode ~pvec:[0; 0; 1] $startpos $endpos (L.TypenameSpecifier uqn) [i] in
      env#set_type_binding uqn nd;
      nd
    }
| TYPENAME n=nested_name_specifier i=id_macro_call
    { 
      let uqn = Ast.encode_ident i#get_name in
      let p = (Ast.encode_nested_name_spec n) in
      let nd = mknode ~pvec:[1; 0; 1] $startpos $endpos (L.TypenameSpecifier uqn) [n; i] in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| TYPENAME n=nested_name_specifier t_opt=ioption(template) s=simple_template_id_
    { 
      let tl = opt_to_list t_opt in
      let uqn = Ast.uqn_of_simple_template_id s in
      let p = (Ast.encode_nested_name_spec n) in
      let nd = mknode ~pvec:[1; List.length tl; 1] $startpos $endpos (L.TypenameSpecifier uqn) [n; s] in
      env#set_type_binding ~prefix:p uqn nd;
      nd
    }
| TYPENAME t_opt=ioption(template) s=simple_template_id_
    { 
      let tl = opt_to_list t_opt in
      let uqn = Ast.uqn_of_simple_template_id s in
      if t_opt <> None then
        s#add_prefix "template ";
      let nd = mknode ~pvec:[0; List.length tl; 1] $startpos $endpos (L.TypenameSpecifier uqn) [s] in
      env#set_type_binding uqn nd;
      nd
    } (* ??? *)
;

template_argument_list_opt:
| tl_opt=ioption(template_argument_list) { tl_opt }
;

template_argument_list:
| t=template_argument       { [t] }
| p=pp_templ_arg_if_section { [p] }
| t=template_argument ELLIPSIS
    { 
      let t_ = mknode $startpos $endpos L.PackExpansion [t] in
      t_#add_suffix "...";
      [t_]
    }
| tl=template_argument_list COMMA t=template_argument
    { 
      (Xlist.last tl)#add_suffix ",";
      tl @ [t]
    }
| tl=template_argument_list COMMA t=template_argument ELLIPSIS
    { 
      (Xlist.last tl)#add_suffix ",";
      let t_ = mknode $startpos(t) $endpos L.PackExpansion [t] in
      t_#add_suffix "...";
      tl @ [t_]
    }
| tl=template_argument_list i=DELIM_MACRO t=template_argument
    { [mknode $startpos $endpos (L.DELIM_MACRO i) (tl @ [t])] }
| tl=template_argument_list COMMA p=pp_templ_arg_if_section
    { 
      (Xlist.last tl)#add_suffix ",";
      tl @ [p]
    }
| tl=template_argument_list p=pp_templ_arg_if_section { tl @ [p] }
| tl=template_argument_list a=arg_macro { tl @ [a] }
;

template_argument:
| c=constant_expression { c }
| t=type_id { t }
(*| id_expression { }*)
;

pp_templ_arg_if_section:
| p=pp_templ_arg_if_group
    pl=list(pp_templ_arg_elif_group) p_opt=ioption(pp_templ_arg_else_group) pe=pp_endif
    { 
      let pl1 = opt_to_list p_opt in
      let pvec = [1; List.length pl; List.length pl1; 1] in
      let pp_if_cond = get_pp_if_cond pe in
      relab_if_group p pp_if_cond;
      mknode ~pvec $startpos $endpos (L.PpIfSection(0, pp_if_cond)) (p :: pl @ pl1 @ [pe])
    }
;
pp_templ_arg_if_group:
| pi=pp_ifx ioption(COMMA) tl=template_argument_list ioption(COMMA)
    { 
      mknode ~pvec:[1; List.length tl] $startpos $endpos (pp_if_group()) (pi::tl)
    }
| pi=pp_ifx p=pp_control_line MARKER
    { 
      mknode ~pvec:[1; 1] $startpos $endpos (pp_if_group()) [pi; p]
    }
;
pp_templ_arg_elif_group:
| pi=pp_elif ioption(COMMA) tl=template_argument_list ioption(COMMA)
    { 
      mknode ~pvec:[1; List.length tl] $startpos $endpos (_pp_elif_group pi) (pi::tl)
    }
;
pp_templ_arg_else_group:
| pi=pp_else ioption(COMMA) tl=template_argument_list ioption(COMMA)
    { 
      mknode ~pvec:[1; List.length tl] $startpos $endpos (_pp_else_group pi) (pi::tl)
    }
;

type_name:
| i=IDENT { mkleaf $startpos $endpos (L.TypeName i) }
| s=simple_template_id { s }
| i=id_macro_call { i }
;

(*type_name:
| class_name { }
| enum_name { }
| typedef_name { }
;

class_name:
| identifier { }
| simple_template_id { }
;

enum_name:
| identifier { }
;

typedef_name:
| identifier { }
| simple_template_id { }
;*)

literal_operator_id:
| OPERATOR s=STR_LITERAL i=IDENT
    { 
      let s_ = mkleaf $startpos(s) $endpos(s) (L.StringLiteral s) in
      let i_ = mkleaf $startpos(i) $endpos(i) (L.StringLiteral i) in
      mknode ~pvec:[1; 1] $startpos $endpos (L.LiteralOperatorId i) [s_; i_]
    }
| OPERATOR u=USER_STR_LITERAL
    { 
      let u_ = mkleaf $startpos(u) $endpos(u) (L.UserDefinedStringLiteral u) in
      mknode ~pvec:[1; 0] $startpos $endpos (L.LiteralOperatorId u) [u_]
    }
;

operator_function_id:
| OPERATOR o=operator { mknode $startpos $endpos L.OperatorFunctionId [o] }
| OPERATOR l=LPAREN RPAREN
    { 
      ignore l;
      let o = mkleaf $startpos(l) $endpos L.Parentheses in
      mknode $startpos $endpos L.OperatorFunctionId [o]
    }
| OPERATOR c=COMMA
    { 
      ignore c;
      let c_ = mkleaf $startpos(c) $endpos L.Comma in
      mknode $startpos $endpos L.OperatorFunctionId [c_]
    }
;

operator:
| NEW                      { mkleaf $startpos $endpos L.New } %prec PREC
| DELETE                   { mkleaf $startpos $endpos L.Delete } %prec PREC
| NEW LBRACKET RBRACKET    { mkleaf $startpos $endpos L.NewBracket }
| DELETE LBRACKET RBRACKET { mkleaf $startpos $endpos L.DeleteBracket }
(*| LPAREN RPAREN            { mkleaf $startpos $endpos L.Parentheses }*)
| LBRACKET RBRACKET        { mkleaf $startpos $endpos L.Brackets }
| MINUS_GT                 { mkleaf $startpos $endpos L.MinusGt }
| MINUS_GT_STAR            { mkleaf $startpos $endpos L.MinusGtStar }
| i=TILDE                  { mkleaf $startpos $endpos (L.Tilde i) }
| i=EXCLAM                 { mkleaf $startpos $endpos (L.Exclam i) }
| PLUS                     { mkleaf $startpos $endpos L.Plus }
| MINUS                    { mkleaf $startpos $endpos L.Minus }
| STAR                     { mkleaf $startpos $endpos L.Star }
| SLASH                    { mkleaf $startpos $endpos L.Slash }
| PERC                     { mkleaf $startpos $endpos L.Perc }
| i=HAT                    { mkleaf $startpos $endpos (L.Hat i) }
| i=AMP                    { mkleaf $startpos $endpos (L.Amp i) }
| i=BAR                    { mkleaf $startpos $endpos (L.Bar i) }
| EQ                       { mkleaf $startpos $endpos L.Eq }
| PLUS_EQ                  { mkleaf $startpos $endpos L.PlusEq }
| MINUS_EQ                 { mkleaf $startpos $endpos L.MinusEq }
| STAR_EQ                  { mkleaf $startpos $endpos L.StarEq }
| SLASH_EQ                 { mkleaf $startpos $endpos L.SlashEq }
| PERC_EQ                  { mkleaf $startpos $endpos L.PercEq }
| i=HAT_EQ                 { mkleaf $startpos $endpos (L.HatEq i) }
| i=AMP_EQ                 { mkleaf $startpos $endpos (L.AmpEq i) }
| i=BAR_EQ                 { mkleaf $startpos $endpos (L.BarEq i) }
| EQ_EQ                    { mkleaf $startpos $endpos L.EqEq }
| i=EXCLAM_EQ              { mkleaf $startpos $endpos (L.ExclamEq i) }
| LT                       { mkleaf $startpos $endpos L.Lt }
| GT                       { mkleaf $startpos $endpos L.Gt }
| LT_EQ                    { mkleaf $startpos $endpos L.LtEq }
| GT_EQ                    { mkleaf $startpos $endpos L.GtEq }
| LT_EQ_GT                 { mkleaf $startpos $endpos L.LtEqGt }
| i=AMP_AMP                { mkleaf $startpos $endpos (L.AmpAmp i) }
| i=BAR_BAR                { mkleaf $startpos $endpos (L.BarBar i) }
| LT_LT                    { mkleaf $startpos $endpos L.LtLt }
| GT_GT                    { mkleaf $startpos $endpos L.GtGt }
| LT_LT_EQ                 { mkleaf $startpos $endpos L.LtLtEq }
| GT_GT_EQ                 { mkleaf $startpos $endpos L.GtGtEq }
| PLUS_PLUS                { mkleaf $startpos $endpos L.PlusPlus }
| MINUS_MINUS              { mkleaf $startpos $endpos L.MinusMinus }
| CO_AWAIT                 { mkleaf $startpos $endpos L.Co_await }
| o=OP_MACRO               { mkleaf $startpos $endpos (L.OperatorMacro o) }
;


(* *)

_pp_param_list:
| i=IDENT { [i] }
| l=_pp_param_list COMMA i=IDENT { l @ [i] }
;
pp_param_list:
|          { Ast.L.FunctionLike([], "") }
| ELLIPSIS { Ast.L.FunctionLike([], "__VA_ARGS__") }
| il=_pp_param_list                { Ast.L.FunctionLike(il, "") }
| il=_pp_param_list COMMA ELLIPSIS { Ast.L.FunctionLike(il, "__VA_ARGS__") }
| il=_pp_param_list ELLIPSIS
    { 
      let il_, va = Xlist.partition_at_last il in
      Ast.L.FunctionLike(il_, va)
    }
;

_pp_define:
| PP_DEFINE i=IDENT tl=token_seq
    { 
      let tl_obj = Obj.repr tl in
      let tndl, pending = check_macro_body ~name:i $startpos(tl) $endpos(tl) tl (*tl_obj*)(Token.seq_to_repr tl) in
      let mnd = mknode $startpos(tl) $endpos(tl) L.ObjectLikeMacro tndl in
      let nd = mknode $startpos $endpos (L.PpDefine i) [mnd] in
      if pending then
        env#register_pending_macro i nd Ast.L.ObjectLike tl_obj
      else
        env#register_resolved_macro i mnd;
      env#register_macro_obj (Ast.mk_macro_id i) mnd;
      nd
    }
;

%inline
pp_define:
| PP_DEFINE i=IDENT NEWLINE
    { 
      let nd = mkleaf $startpos $endpos (L.PpDefine i) in
      env#register_macro_obj (Ast.mk_macro_id i) nd;
      nd
    }
| p=_pp_define NEWLINE { reloc $startpos $endpos p }
| (*PP_DEFINE i=IDENT PP_LPAREN*)i=macro_fun_head mk=pp_param_list RPAREN tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl_ = list_opt_to_list tl_opt in
      let tl__obj = Obj.repr tl_ in
      (*let tnd = mkleaf $startpos(tl_opt) $endpos(tl_opt) (L.TokenSeq tl__obj) in*)
      let tndl, pending = check_macro_body $startpos(tl_opt) $endpos(tl_opt) tl_ (*tl__obj*)(Token.seq_to_repr tl_) in
      let mnd = mknode $startpos(tl_opt) $endpos(tl_opt) (L.FunctionLikeMacro mk) tndl in
      let nd = mknode $startpos $endpos (L.PpDefine i) [mnd] in
      if pending then
        env#register_pending_macro i nd mk tl__obj;
      env#register_macro_fun (Ast.mk_macro_call_id i) nd;
      nd
    }
;

macro_fun_head:
| PP_DEFINE i=IDENT PP_LPAREN
    { 
      let nd = mknode $startpos $endpos (L.PpDefine i) [] in
      env#register_macro_fun (Ast.mk_macro_call_id i) nd;
      i
    }
;

%inline
pp_undef:
| PP_UNDEF i=IDENT NEWLINE
    { 
      env#undef_macro i;
      mkleaf $startpos $endpos (L.PpUndef i)
    }
| PP_UNDEF i=IDENT tl=token_seq NEWLINE
    { 
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl) $endpos(tl) "extra tokens at end of #undef: %s" s
      end;
      env#undef_macro i;
      mkleaf $startpos $endpos (L.PpUndef i)
    }
;

pp_control_line:
| PP_INCLUDE tl=token_seq NEWLINE
    { mkleaf $startpos $endpos (L.PpInclude (Token.seq_to_repr tl)) }

| PP_IMPORT tl=token_seq NEWLINE (* Objective-C *)
    { mkleaf $startpos $endpos (L.PpImport (Token.seq_to_repr tl)) }

| p=pp_define { p }
| p=pp_undef { p }

| PP_LINE i=INT_LITERAL s_opt=ioption(STR_LITERAL) NEWLINE
    { 
      mkleaf $startpos $endpos
        (L.PpLine (int_of_string i, string_opt_to_string s_opt))
    }
| PP_ERROR               NEWLINE { mkleaf $startpos $endpos (L.PpError "") }
| PP_ERROR  tl=token_seq NEWLINE
    { mkleaf $startpos $endpos (L.PpError (Token.seq_to_repr tl)) }
| PP_PRAGMA tl=token_seq NEWLINE
    { 
      let lab =
        match tl with
        | (T.IDENT "omp",_,_)::rest -> L.OmpDirective (Token.seq_to_repr rest)
        | (T.IDENT "acc",_,_)::rest -> L.AccDirective (Token.seq_to_repr rest)
        | _ -> L.PpPragma (Token.seq_to_repr tl)
      in
      mkleaf $startpos $endpos lab
    }
| PP_ NEWLINE { mkleaf $startpos $endpos L.PpNull }
| PP_ i=INT_LITERAL s=STR_LITERAL il=INT_LITERAL* NEWLINE
    { 
      mkleaf $startpos $endpos (L.PpMarker(int_of_string i, s, List.map int_of_string il))
    }
| PP_UNKNOWN tl=token_seq NEWLINE
    { mkleaf $startpos $endpos (L.PpUnknown (Token.seq_to_repr tl)) }
| PP_UNKNOWN NEWLINE { mkleaf $startpos $endpos (L.PpUnknown "") }

| PP_ODD_IF c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_ODD_IFDEF  i=IDENT NEWLINE { mkleaf $startpos $endpos (L.PpIfdef i) }
| PP_ODD_IFNDEF i=IDENT NEWLINE { mkleaf $startpos $endpos (L.PpIfndef i) }
| x=PP_ODD_ELIF c=constant_expression NEWLINE { mknode $startpos $endpos (L.PpElif x) [c] }
| x=PP_ODD_ELSE NEWLINE { mkleaf $startpos $endpos (L.PpElse x) }
| x=PP_ODD_ENDIF NEWLINE { mkleaf $startpos $endpos (L.PpEndif x) }
;

pp_ifx:
| PP_IF c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF  i=IDENT tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFDEF  i=INT_LITERAL NEWLINE
    { 
      warning $startpos(i) $endpos(i) "macro names must be identifiers: %s" i;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF i=IDENT tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_a:
| PP_IF_A c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_A  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_A i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_b:
| PP_IF_B c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_B  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_B i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_x:
| PP_IF_X c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_X  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_X i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_c:
| PP_IF_C c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_C  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_C i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_cb:
| PP_IF_CB c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_CB  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_CB i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_d:
| PP_IF_D c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_D  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_D i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_e:
| PP_IF_E c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_E  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_E i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_eh:
| PP_IF_EH c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_EH  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_EH i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_h:
| PP_IF_H c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_H  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_H i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_i:
| PP_IF_I c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_I  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_I i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_o:
| PP_IF_O c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_O  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_O i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_p:
| PP_IF_P c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_P  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_P i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_s:
| PP_IF_S c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_S  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_S i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_shift:
| PP_IF_SHIFT c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_SHIFT  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_SHIFT i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_cond:
| PP_IF_COND c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_COND  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_COND i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_cond_:
| PP_IF_COND_ c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_COND_  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_COND_ i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_closing:
| PP_IF_CLOSING c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_CLOSING  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_CLOSING i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_close_open:
| PP_IF_CLOSE_OPEN c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_CLOSE_OPEN  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_CLOSE_OPEN i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_ifx_attr:
| PP_IF_ATTR c=constant_expression NEWLINE { mknode $startpos $endpos (pp_if()) [c] }
| PP_IFDEF_ATTR  i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifdef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfdef i)
    }
| PP_IFNDEF_ATTR i=IDENT_V tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #ifndef directive: %s" s
      end;
      mkleaf $startpos $endpos (L.PpIfndef i)
    }
;
pp_elif:
| x=PP_ELIF c=constant_expression NEWLINE { mknode $startpos $endpos (L.PpElif (!x)) [c] }
| x=PP_ELIF NEWLINE { mknode $startpos $endpos (L.PpElif (!x)) [] }
;
pp_else:
| x=PP_ELSE tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #else: %s" s
      end;
      mkleaf $startpos $endpos (L.PpElse (!x))
    }
;
pp_endif:
| x=PP_ENDIF tl_opt=ioption(token_seq) NEWLINE
    { 
      let tl = list_opt_to_list tl_opt in
      if tl <> [] then begin
        let s = Token.seq_to_repr tl in
        warning $startpos(tl_opt) $endpos(tl_opt) "extra tokens at end of #endif: %s" s
      end;
      mkleaf $startpos $endpos (L.PpEndif (!x))
    }
;

token_seq:
| tl=nonempty_list(token) { tl }
;

token:
| LPAREN        { mktok $startpos $endpos T.LPAREN }
| TY_LPAREN     { mktok $startpos $endpos T.TY_LPAREN }
| RPAREN        { mktok $startpos $endpos T.RPAREN }
| LBRACE        { mktok $startpos $endpos T.LBRACE }
| INI_LBRACE    { mktok $startpos $endpos T.INI_LBRACE }
| RBRACE        { mktok $startpos $endpos T.RBRACE }
| LBRACKET      { mktok $startpos $endpos T.LBRACKET }
| OBJC_LBRACKET { mktok $startpos $endpos T.OBJC_LBRACKET }
| LAM_LBRACKET  { mktok $startpos $endpos T.LAM_LBRACKET }
| ATTR_LBRACKET { mktok $startpos $endpos T.ATTR_LBRACKET }
| RBRACKET      { mktok $startpos $endpos T.RBRACKET }
| TEMPL_LT      { mktok $startpos $endpos T.TEMPL_LT }
| TEMPL_GT      { mktok $startpos $endpos T.TEMPL_GT }
| TY_TEMPL_GT   { mktok $startpos $endpos T.TY_TEMPL_GT }
| END_ASM       { mktok $startpos $endpos T.END_ASM }
| PP_DEFINE     { mktok $startpos $endpos T.PP_DEFINE }
| PP_UNDEF      { mktok $startpos $endpos T.PP_UNDEF }
| PP_ERROR      { mktok $startpos $endpos T.PP_ERROR }
| PP_LINE       { mktok $startpos $endpos T.PP_LINE }
| PP_INCLUDE    { mktok $startpos $endpos T.PP_INCLUDE }
| OBJC_PLUS     { mktok $startpos $endpos T.PLUS }
| OBJC_MINUS    { mktok $startpos $endpos T.MINUS }
| OBJC_ENCODE   { mktok $startpos $endpos T.OBJC_ENCODE }
| OBJC_SELECTOR { mktok $startpos $endpos T.OBJC_SELECTOR }
| OBJC_TRY      { mktok $startpos $endpos T.OBJC_TRY }
| OBJC_CATCH    { mktok $startpos $endpos T.OBJC_CATCH }
| OBJC_FINALLY  { mktok $startpos $endpos T.OBJC_FINALLY }
| t=OBJC_UNKNOWN { mktok $startpos $endpos (T.OBJC_UNKNOWN t) }
| t=DOXYGEN_CMD  { mktok $startpos $endpos (T.DOXYGEN_CMD t) }
| t=BS_IDENT     { mktok $startpos $endpos (T.BS_IDENT t) }
| t=token_no_paren { t }
| t=quasi_keyword  { t }
| t=extra_keyword  { t }
;

quasi_keyword:
| AUDIT    { mktok $startpos $endpos T.AUDIT }
| AXIOM    { mktok $startpos $endpos T.AXIOM }
| FINAL    { mktok $startpos $endpos T.FINAL }
| OVERRIDE { mktok $startpos $endpos T.OVERRIDE }

| ASSERT   { mktok $startpos $endpos T.ASSERT }
| ENSURES  { mktok $startpos $endpos T.ENSURES }
| EXPECTS  { mktok $startpos $endpos T.EXPECTS }
;

extra_keyword:
| t=MS_ASM     { mktok $startpos $endpos (T.MS_ASM t) }
| t=MS_CDECL   { mktok $startpos $endpos (T.MS_CDECL t) }
| t=MS_STDCALL { mktok $startpos $endpos (T.MS_STDCALL t) }
| t=RESTRICT   { mktok $startpos $endpos (T.RESTRICT t) }
| t=GNU_ASM    { mktok $startpos $endpos (T.GNU_ASM t) }
| t=GNU_ATTR   { mktok $startpos $endpos (T.GNU_ATTR t) }
;

token_no_paren:
| t=IDENT   { mktok $startpos $endpos (T.IDENT t) }
| t=IDENT_B { mktok $startpos $endpos (T.IDENT_B t) }
| t=IDENT_C { mktok $startpos $endpos (T.IDENT_C t) }
| t=IDENT_V { mktok $startpos $endpos (T.IDENT_V t) }
| t=IDENT_E { mktok $startpos $endpos (T.IDENT_E t) }
| t=IDENT_EM { mktok $startpos $endpos (T.IDENT_EM t) }
| t=IDENT_SM { mktok $startpos $endpos (T.IDENT_SM t) }
| t=IDENT_TM { mktok $startpos $endpos (T.IDENT_TM t) }
| t=IDENT_IM { mktok $startpos $endpos (T.IDENT_IM t) }
| t=IDENT_PM { mktok $startpos $endpos (T.IDENT_PM t) }

| t=INT_LITERAL        { mktok $startpos $endpos (T.INT_LITERAL t) }
| t=CHAR_LITERAL       { mktok $startpos $endpos (T.CHAR_LITERAL t) }
| t=FLOAT_LITERAL      { mktok $startpos $endpos (T.FLOAT_LITERAL t) }
| t=STR_LITERAL        { mktok $startpos $endpos (T.STR_LITERAL t) }
| t=BOOL_LITERAL       { mktok $startpos $endpos (T.BOOL_LITERAL t) }
| t=USER_INT_LITERAL   { mktok $startpos $endpos (T.USER_INT_LITERAL t) }
| t=USER_FLOAT_LITERAL { mktok $startpos $endpos (T.USER_FLOAT_LITERAL t) }
| t=USER_STR_LITERAL   { mktok $startpos $endpos (T.USER_STR_LITERAL t) }
| t=USER_CHAR_LITERAL  { mktok $startpos $endpos (T.USER_CHAR_LITERAL t) }

| t=PP_STRINGIZED { mktok $startpos $endpos (T.PP_STRINGIZED t) }

| t=STR_MACRO     { mktok $startpos $endpos (T.STR_MACRO t) }

| ALIGNAS          { mktok $startpos $endpos T.ALIGNAS }
| ALIGNOF          { mktok $startpos $endpos T.ALIGNOF }
| ASM              { mktok $startpos $endpos T.ASM }
| AUTO             { mktok $startpos $endpos T.AUTO }
| BOOL             { mktok $startpos $endpos T.BOOL }
| BREAK            { mktok $startpos $endpos T.BREAK }
| CASE             { mktok $startpos $endpos T.CASE }
| CATCH            { mktok $startpos $endpos T.CATCH }
| CHAR             { mktok $startpos $endpos T.CHAR }
| CHAR8_T          { mktok $startpos $endpos T.CHAR8_T }
| CHAR16_T         { mktok $startpos $endpos T.CHAR16_T }
| CHAR32_T         { mktok $startpos $endpos T.CHAR32_T }
| CLASS            { mktok $startpos $endpos T.CLASS }
| CONCEPT          { mktok $startpos $endpos T.CONCEPT }
| CONST            { mktok $startpos $endpos T.CONST }
| CONSTEVAL        { mktok $startpos $endpos T.CONSTEVAL }
| CONSTEXPR        { mktok $startpos $endpos T.CONSTEXPR }
| CONSTINIT        { mktok $startpos $endpos T.CONSTINIT }
| CONST_CAST       { mktok $startpos $endpos T.CONST_CAST }
| CONTINUE         { mktok $startpos $endpos T.CONTINUE }
| DECLTYPE         { mktok $startpos $endpos T.DECLTYPE }
| DEFAULT          { mktok $startpos $endpos T.DEFAULT }
| DEFINED          { mktok $startpos $endpos T.DEFINED }
| DELETE           { mktok $startpos $endpos T.DELETE }
| DOUBLE           { mktok $startpos $endpos T.DOUBLE }
| DO               { mktok $startpos $endpos T.DO }
| DYNAMIC_CAST     { mktok $startpos $endpos T.DYNAMIC_CAST }
| ELSE             { mktok $startpos $endpos T.ELSE }
| ENUM             { mktok $startpos $endpos T.ENUM }
| ELAB_ENUM        { mktok $startpos $endpos T.ELAB_ENUM }
| EXPLICIT         { mktok $startpos $endpos T.EXPLICIT }
| EXPORT           { mktok $startpos $endpos T.EXPORT }
| EXTERN           { mktok $startpos $endpos T.EXTERN }
| FALSE            { mktok $startpos $endpos T.FALSE }
| FLOAT            { mktok $startpos $endpos T.FLOAT }
| FOR              { mktok $startpos $endpos T.FOR }
| FRIEND           { mktok $startpos $endpos T.FRIEND }
| GOTO             { mktok $startpos $endpos T.GOTO }
| HAS_CPP_ATTRIBUTE { mktok $startpos $endpos T.HAS_CPP_ATTRIBUTE }
| HAS_INCLUDE      { mktok $startpos $endpos T.HAS_INCLUDE }
| IF               { mktok $startpos $endpos T.IF }
| INLINE           { mktok $startpos $endpos T.INLINE }
| INT              { mktok $startpos $endpos T.INT }
| LONG             { mktok $startpos $endpos T.LONG }
| MUTABLE          { mktok $startpos $endpos T.MUTABLE }
| NAMESPACE        { mktok $startpos $endpos T.NAMESPACE }
| NEW              { mktok $startpos $endpos T.NEW }
| NOEXCEPT         { mktok $startpos $endpos T.NOEXCEPT }
| NULLPTR          { mktok $startpos $endpos T.NULLPTR }
| OPERATOR         { mktok $startpos $endpos T.OPERATOR }
| PRIVATE          { mktok $startpos $endpos T.PRIVATE }
| PROTECTED        { mktok $startpos $endpos T.PROTECTED }
| PUBLIC           { mktok $startpos $endpos T.PUBLIC }
| REGISTER         { mktok $startpos $endpos T.REGISTER }
| REINTERPRET_CAST { mktok $startpos $endpos T.REINTERPRET_CAST }
| REQUIRES         { mktok $startpos $endpos T.REQUIRES }
| RETURN           { mktok $startpos $endpos T.RETURN }
| SHORT            { mktok $startpos $endpos T.SHORT }
| SIGNED           { mktok $startpos $endpos T.SIGNED }
| SIZEOF           { mktok $startpos $endpos T.SIZEOF }
| STATIC           { mktok $startpos $endpos T.STATIC }
| STATIC_ASSERT    { mktok $startpos $endpos T.STATIC_ASSERT }
| STATIC_CAST      { mktok $startpos $endpos T.STATIC_CAST }
| STRUCT           { mktok $startpos $endpos T.STRUCT }
| SWITCH           { mktok $startpos $endpos T.SWITCH }
| TEMPLATE         { mktok $startpos $endpos T.TEMPLATE }
| THIS             { mktok $startpos $endpos T.THIS }
| THREAD_LOCAL     { mktok $startpos $endpos T.THREAD_LOCAL }
| THROW            { mktok $startpos $endpos T.THROW }
| TRUE             { mktok $startpos $endpos T.TRUE }
| TRY              { mktok $startpos $endpos T.TRY }
| TYPEDEF          { mktok $startpos $endpos T.TYPEDEF }
| TYPEID           { mktok $startpos $endpos T.TYPEID }
| TYPENAME         { mktok $startpos $endpos T.TYPENAME }
| UNION            { mktok $startpos $endpos T.UNION }
| UNSIGNED         { mktok $startpos $endpos T.UNSIGNED }
| USING            { mktok $startpos $endpos T.USING }
| VIRTUAL          { mktok $startpos $endpos T.VIRTUAL }
| VOID             { mktok $startpos $endpos T.VOID }
| VOLATILE         { mktok $startpos $endpos T.VOLATILE }
| WCHAR_T          { mktok $startpos $endpos T.WCHAR_T }
| WHILE            { mktok $startpos $endpos T.WHILE }

| SHARP                 { mktok $startpos $endpos T.SHARP }
| SHARP_SHARP           { mktok $startpos $endpos T.SHARP_SHARP }
| LT_COLON              { mktok $startpos $endpos T.LT_COLON }
| COLON_GT              { mktok $startpos $endpos T.COLON_GT }
| LT_PERC               { mktok $startpos $endpos T.LT_PERC }
| PERC_GT               { mktok $startpos $endpos T.PERC_GT }
| PERC_COLON            { mktok $startpos $endpos T.PERC_COLON }
| PERC_COLON_PERC_COLON { mktok $startpos $endpos T.PERC_COLON_PERC_COLON }
| b=SEMICOLON           { mktok $startpos $endpos (T.SEMICOLON b) }
| COLON                 { mktok $startpos $endpos T.COLON }
| BASE_COLON            { mktok $startpos $endpos T.BASE_COLON }
| ELLIPSIS              { mktok $startpos $endpos T.ELLIPSIS }
| ELLIPSIS_             { mktok $startpos $endpos T.ELLIPSIS_ }
| QUEST                 { mktok $startpos $endpos T.QUEST }
| COLON_COLON           { mktok $startpos $endpos T.COLON_COLON }
| HEAD_COLON_COLON      { mktok $startpos $endpos T.HEAD_COLON_COLON }
| DOT                   { mktok $startpos $endpos T.DOT }
| DOT_STAR              { mktok $startpos $endpos T.DOT_STAR }
| MINUS_GT              { mktok $startpos $endpos T.MINUS_GT }
| MINUS_GT_STAR         { mktok $startpos $endpos T.MINUS_GT_STAR }
| i=TILDE               { mktok $startpos $endpos (T.TILDE i) }
| TY_TILDE              { mktok $startpos $endpos T.TY_TILDE }
| i=EXCLAM              { mktok $startpos $endpos (T.EXCLAM i) }
| PLUS                  { mktok $startpos $endpos T.PLUS }
| MINUS                 { mktok $startpos $endpos T.MINUS }
| STAR                  { mktok $startpos $endpos T.STAR }
| PTR_STAR              { mktok $startpos $endpos T.PTR_STAR }
| SLASH                 { mktok $startpos $endpos T.SLASH }
| PERC                  { mktok $startpos $endpos T.PERC }
| i=HAT                 { mktok $startpos $endpos (T.HAT i) }
| TY_HAT                { mktok $startpos $endpos T.TY_HAT }
| i=AMP                 { mktok $startpos $endpos (T.AMP i) }
| PTR_AMP               { mktok $startpos $endpos T.PTR_AMP }
| i=BAR                 { mktok $startpos $endpos (T.BAR i) }
| EQ                    { mktok $startpos $endpos T.EQ }
| PLUS_EQ               { mktok $startpos $endpos T.PLUS_EQ }
| MINUS_EQ              { mktok $startpos $endpos T.MINUS_EQ }
| STAR_EQ               { mktok $startpos $endpos T.STAR_EQ }
| SLASH_EQ              { mktok $startpos $endpos T.SLASH_EQ }
| PERC_EQ               { mktok $startpos $endpos T.PERC_EQ }
| i=HAT_EQ              { mktok $startpos $endpos (T.HAT_EQ i) }
| i=AMP_EQ              { mktok $startpos $endpos (T.AMP_EQ i) }
| i=BAR_EQ              { mktok $startpos $endpos (T.BAR_EQ i) }
| EQ_EQ                 { mktok $startpos $endpos T.EQ_EQ }
| EQ_EQ_EQ              { mktok $startpos $endpos T.EQ_EQ_EQ }
| i=EXCLAM_EQ           { mktok $startpos $endpos (T.EXCLAM_EQ i) }
| LT                    { mktok $startpos $endpos T.LT }
| GT                    { mktok $startpos $endpos T.GT }
| LT_EQ                 { mktok $startpos $endpos T.LT_EQ }
| GT_EQ                 { mktok $startpos $endpos T.GT_EQ }
| LT_EQ_GT              { mktok $startpos $endpos T.LT_EQ_GT }
| i=AMP_AMP             { mktok $startpos $endpos (T.AMP_AMP i) }
| PTR_AMP_AMP           { mktok $startpos $endpos T.PTR_AMP_AMP }
| i=BAR_BAR             { mktok $startpos $endpos (T.BAR_BAR i) }
| CUDA_LT_LT_LT         { mktok $startpos $endpos T.CUDA_LT_LT_LT }
| CUDA_GT_GT_GT         { mktok $startpos $endpos T.CUDA_GT_GT_GT }
| LT_LT                 { mktok $startpos $endpos T.LT_LT }
| GT_GT                 { mktok $startpos $endpos T.GT_GT }
| GT_GT_GT              { mktok $startpos $endpos T.GT_GT_GT }
| LT_LT_EQ              { mktok $startpos $endpos T.LT_LT_EQ }
| GT_GT_EQ              { mktok $startpos $endpos T.GT_GT_EQ }
| PLUS_PLUS             { mktok $startpos $endpos T.PLUS_PLUS }
| MINUS_MINUS           { mktok $startpos $endpos T.MINUS_MINUS }
| COMMA                 { mktok $startpos $endpos T.COMMA }
| AND                   { mktok $startpos $endpos T.AND }
| OR                    { mktok $startpos $endpos T.OR }
| XOR                   { mktok $startpos $endpos T.XOR }
| NOT                   { mktok $startpos $endpos T.NOT }
| BITAND                { mktok $startpos $endpos T.BITAND }
| BITOR                 { mktok $startpos $endpos T.BITOR }
| COMPL                 { mktok $startpos $endpos T.COMPL }
| AND_EQ                { mktok $startpos $endpos T.AND_EQ }
| OR_EQ                 { mktok $startpos $endpos T.OR_EQ }
| XOR_EQ                { mktok $startpos $endpos T.XOR_EQ }
| NOT_EQ                { mktok $startpos $endpos T.NOT_EQ }
| AT                    { mktok $startpos $endpos T.AT }
;


%%
