(*
   Copyright 2012-2020 Codinuum Software Lab <https://codinuum.com>

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
(* 
 * A parser for the Java Language (based on the JLS 3rd ed.) 
 * 
 * parser.mly
 *
 *)

%{
module P = Printer
open Printf
open Ast

open Parser_aux
module A = Parser_aux.F (Stat)
open A
open Stat
%}

%parameter <Stat : Parser_aux.STATE_T>

%token EOF

%token <Ast.Loc.t * string> IDENTIFIER
%token <string> INTEGER_LITERAL
%token <string> FLOATING_POINT_LITERAL
%token <string> CHARACTER_LITERAL
%token <string> STRING_LITERAL
%token TRUE FALSE NULL

(* Separators *)
%token <Ast.Loc.t> AT AT__INTERFACE LPAREN RPAREN LPAREN__LAMBDA
%token LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT 
%token ELLIPSIS COLON_COLON

(* Operators *)
%token <Ast.Loc.t> LT
%token EQ GT EXCLAM TILDE QUESTION COLON
%token EQ_EQ LT_EQ GT_EQ EXCLAM_EQ AND_AND OR_OR PLUS_PLUS MINUS_MINUS
%token PLUS MINUS STAR SLASH AND OR HAT PERCENT LT_LT GT_GT GT_GT_GT
%token PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ AND_EQ OR_EQ HAT_EQ PERCENT_EQ 
%token LT_LT_EQ GT_GT_EQ GT_GT_GT_EQ MINUS_GT

(* Keywords *)
%token <Ast.Loc.t> ABSTRACT ASSERT BOOLEAN BREAK BYTE 
%token <Ast.Loc.t> CASE CATCH CHAR CLASS CONST CONTINUE
%token <Ast.Loc.t> DEFAULT DEFAULT__COLON DO DOUBLE ELSE ENUM EXTENDS
%token <Ast.Loc.t> FINAL FINALLY FLOAT FOR GOTO
%token <Ast.Loc.t> IF IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG 
%token <Ast.Loc.t> NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN
%token <Ast.Loc.t> SHORT STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
%token <Ast.Loc.t> THIS THROW THROWS TRANSIENT TRY VOLATILE VOID WHILE

%token <Ast.statement> STMT
%token <Ast.block_statement> BLOCK_STMT
%token <string> MARKER ERROR ERROR_STMT
%token GT_7
%token EOP

%start main
%start partial_assert_statement
%start partial_block_statement

%type <Ast.compilation_unit> main
%type <Ast.statement> partial_assert_statement
%type <Ast.block_statement> partial_block_statement


// to avoid warnings
%start reserved
%type <unit> reserved

%%
(********** Rules **********)


%inline
clist(X):
| l=separated_nonempty_list(COMMA, X) { l }
;

%inline
clist0(X):
| l_opt=ioption(separated_nonempty_list(COMMA, X)) { Xoption.list_opt_to_list l_opt }
;

reserved:
| GOTO  { }
| CONST { }
| GT_7 { }
;

partial_block_statement:
| p=block_statement EOP { p }
;

partial_assert_statement:
| p=assert_statement EOP { p }
;


main:
| c=compilation_unit EOF { end_scope(); c }
;

literal:
| i=INTEGER_LITERAL        { Linteger i }
| f=FLOATING_POINT_LITERAL { LfloatingPoint f }
| TRUE                     { Ltrue }
| FALSE                    { Lfalse }
| c=CHARACTER_LITERAL      { Lcharacter(String.sub c 1 ((String.length c) - 2)) }
| s=STRING_LITERAL         { Lstring(String.sub s 1 ((String.length s) - 2)) }
| NULL                     { Lnull }
;

unann_type:
| p=unann_primitive_type { p }
| r=unann_reference_type { r }
;

unann_primitive_type:
| n=numeric_type { n }
| BOOLEAN        { mktype $startofs $endofs (Tprimitive([], PTboolean)) }
;

%inline
primitive_type:
|               u=unann_primitive_type { u }
| a=annotations u=unann_primitive_type 
    { 
      let loc = get_loc $startofs $endofs in
      add_ann loc a u
    }
;

numeric_type:
| i=integral_type       { mktype $startofs $endofs (Tprimitive([], i)) }
| f=floating_point_type { mktype $startofs $endofs (Tprimitive([], f)) }
;

integral_type:
| BYTE  { PTbyte }
| SHORT { PTshort }
| INT   { PTint }
| LONG  { PTlong }
| CHAR  { PTchar }
;

floating_point_type:
| FLOAT  { PTfloat }
| DOUBLE { PTdouble }
;


unann_reference_type:
| c=unann_class_or_interface_type { c }
| a=unann_array_type              { a }
;

%inline
reference_type:
|               u=unann_reference_type { u }
| a=annotations u=unann_reference_type 
    { 
      let loc = get_loc $startofs $endofs in
      add_ann loc a u
    }
;

%inline
class_or_interface_type:
|               u=unann_class_or_interface_type { u }
| a=annotations u=unann_class_or_interface_type 
    { 
      let loc = get_loc $startofs $endofs in
      add_ann loc a u
    }
;

%inline
class_or_interface_type_spec:
|               u=unann_class_or_interface_type_spec { u }
| a=annotations u=unann_class_or_interface_type_spec 
    { 
      let head, a0, n0 = u in
      head, a @ a0, n0
    }
;


unann_class_or_interface_type_spec:
| n=name 
    {
     let rr =
       try
	 mkresolved (get_type_fqn n)
       with
       | _ -> env#resolve n
     in
     set_attribute_PT_T rr n;
     register_qname_as_typename n;
     [], [], n 
   }

| c=unann_class_or_interface_type_spec ts=type_arguments DOT a=annotations0 n=name
    { 
      let head, a0, n0 = c in
      head @ [TSapply(a0, n0, ts)], a, n
    }
;

unann_class_or_interface_type:
| c=unann_class_or_interface_type_spec 
    { 
      let head, a, n = c in
      mktype $startofs $endofs (TclassOrInterface(head @ [TSname(a, n)])) 
    }

| c=unann_class_or_interface_type_spec ts=type_arguments
    { 
      let head, a, n = c in
      mktype $startofs $endofs (TclassOrInterface(head @ [TSapply(a, n, ts)])) 
    }
;

%inline
unann_class_type:
| c=unann_class_or_interface_type { c }
;

%inline
class_type:
| c=class_or_interface_type { c }
;

%inline
interface_type:
| c=class_or_interface_type { c }
;

unann_array_type:
| n=name d=ann_dims 
    { 
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mktype $startofs $endofs (Tarray(name_to_ty [] n, List.length d)) 
    }

| p=unann_primitive_type d=ann_dims { mktype $startofs $endofs (Tarray(p, List.length d)) }

| c=unann_class_or_interface_type_spec ts=type_arguments DOT a=annotations0 n=name d=ann_dims 
    { 
      let head, a0, n0 = c in
      let ty =
	_mktype (Loc.merge (get_loc $startofs $endofs) n.n_loc) 
	  (TclassOrInterface(head @ [TSapply(a0, n0, ts); TSname(a, n)]))
      in
      mktype $startofs $endofs (Tarray(ty, List.length d))
    }

| c=unann_class_or_interface_type_spec ts=type_arguments d=ann_dims 
    { 
      let head, a, n = c in
      let ty =
	_mktype (Loc.merge (get_loc $startofs $endofs) ts.tas_loc) 
	  (TclassOrInterface(head @ [TSapply(a, n, ts)])) 
      in
      mktype $startofs $endofs (Tarray(ty, List.length d))
    }
;


%inline
type_arguments_opt:
| ts_opt=ioption(type_arguments) { ts_opt }
;

type_arguments:
| LT GT { mktyargs $startofs $endofs [] }
| LT tas=type_argument_list_1 
    { 
      mktyargs $startofs $endofs tas 
    }
;

%inline
wildcard_head:
| al=annotations0 QUESTION { al, $endofs }
;

wildcard:
| alo=wildcard_head                            { let al, _ = alo in al, None }
| alo=wildcard_head e=EXTENDS r=reference_type
    { 
      let _ = e in
      let al, _ = alo in
      al, Some(mkwb $startofs(e) $endofs (WBextends r))
    }
| alo=wildcard_head s=SUPER   r=reference_type
    { 
      let _ = s in
      let al, _ = alo in
      al, Some(mkwb $startofs(s) $endofs (WBsuper r))
    }
;
wildcard_1:
| alo=wildcard_head GT                           { let al, o = alo in (al, None), o }
| alo=wildcard_head e=EXTENDS r=reference_type_1
    { 
      let _ = e in
      let eo = r.Ast.ty_loc.Loc.end_offset in
      let al, _ = alo in
      (al, Some(mkwb $startofs(e) eo (WBextends r))), eo
    }
| alo=wildcard_head s=SUPER   r=reference_type_1
    { 
      let _ = s in
      let eo = r.Ast.ty_loc.Loc.end_offset in
      let al, _ = alo in
      (al, Some(mkwb $startofs(s) eo (WBsuper r))), eo
    }
;
wildcard_2:
| alo=wildcard_head GT_GT                        { let al, o = alo in (al, None), o }
| alo=wildcard_head e=EXTENDS r=reference_type_2
    { 
      let _ = e in
      let eo = r.Ast.ty_loc.Loc.end_offset in
      let al, _ = alo in
      (al, Some(mkwb $startofs(e) eo (WBextends r))), eo
    }
| alo=wildcard_head s=SUPER   r=reference_type_2
    { 
      let _ = s in
      let eo = r.Ast.ty_loc.Loc.end_offset in
      let al, _ = alo in
      (al, Some(mkwb $startofs(s) eo (WBsuper r))), eo
    }
;
wildcard_3:
| alo=wildcard_head GT_GT_GT                     { let al, o = alo in (al, None), o }
| alo=wildcard_head e=EXTENDS r=reference_type_3
    { 
      let _ = e in
      let eo = r.Ast.ty_loc.Loc.end_offset in
      let al, _ = alo in
      (al, Some(mkwb $startofs(e) eo (WBextends r))), eo
    }
| alo=wildcard_head s=SUPER   r=reference_type_3
    { 
      let _ = s in
      let eo = r.Ast.ty_loc.Loc.end_offset in
      let al, _ = alo in
      (al, Some(mkwb $startofs(s) eo (WBsuper r))), eo
    }
;

reference_type_1:
| r=reference_type GT { r }
| c=class_or_interface_type_spec lt=LT tas=type_argument_list_2 
    {
      let head, a, n = c in
      let edloc = $endofs - 1 in
      let _ = lt in
      let tyargs = mktyargs $startofs(lt) edloc tas in
      mktype $startofs edloc (TclassOrInterface(head @ [TSapply(a, n, tyargs)]))
    }
;
reference_type_2:
| r=reference_type GT_GT { r }
| c=class_or_interface_type_spec lt=LT tas=type_argument_list_3 
    {  
      let head, a, n = c in
      let edloc = $endofs - 2 in
      let _ = lt in
      let tyargs = mktyargs $startofs(lt) edloc tas in
      mktype $startofs edloc (TclassOrInterface(head @ [TSapply(a, n, tyargs)]))
    }
;
reference_type_3:
| r=reference_type GT_GT_GT { r }
;

type_argument_list:
|                            t=type_argument { [t] }
| l=type_argument_list COMMA t=type_argument { l @ [t] }
;
type_argument_list_1:
|                            t=type_argument_1 { [t] }
| l=type_argument_list COMMA t=type_argument_1 { l @ [t] }
;
type_argument_list_2:
|                            t=type_argument_2 { [t] }
| l=type_argument_list COMMA t=type_argument_2 { l @ [t] }
;
type_argument_list_3:
|                            t=type_argument_3 { [t] }
| l=type_argument_list COMMA t=type_argument_3 { l @ [t] }
;

type_argument:
| r=reference_type { mktyarg $startofs $endofs (TAreferenceType r) }
| aw=wildcard      { mktyarg $symbolstartofs $endofs (TAwildcard aw) }
;
type_argument_1:
| r=reference_type_1 { _mktyarg r.ty_loc (TAreferenceType r) }
| awo=wildcard_1     { let aw, o = awo in mktyarg $symbolstartofs o (TAwildcard aw) }
;
type_argument_2:
| r=reference_type_2 { _mktyarg r.ty_loc (TAreferenceType r) }
| awo=wildcard_2     { let aw, o = awo in mktyarg $symbolstartofs o (TAwildcard aw) }
;
type_argument_3:
| r=reference_type_3 { _mktyarg r.ty_loc (TAreferenceType r) }
| awo=wildcard_3     { let aw, o = awo in mktyarg $symbolstartofs o (TAwildcard aw) }
;


name:
| s=simple_name    { mkname $startofs $endofs (Nsimple(ref NAunknown, s)) }
| q=qualified_name { q }
;

simple_name:
| i=identifier { let _, id = i in id }
;

%inline
qualified_name:
| n=name DOT i=identifier 
    { 
      let _, id = i in
      mkname $startofs $endofs (Nqualified(ref NAunknown, n, id)) 
    }
| s=ERROR DOT i=identifier
    { 
      let n = mkerrname $startofs $endofs(s) s in
      let _, id = i in
      mkname $startofs $endofs (Nqualified(ref NAunknown, n, id)) 
    }
;


identifier:
| i=IDENTIFIER { i }
;


(***** *****)


compilation_unit:
|                                                                   { mkcu None [] [] }
|                                              t=type_declarations  { mkcu None [] t }
|                       i=import_declarations  t=type_declarations0 { mkcu None i t }
| p=package_declaration i=import_declarations0 t=type_declarations0 { mkcu (Some p) i t }
;

%inline
import_declarations0: 
| (* *)                 { [] }
| i=import_declarations { i }
;

%inline
import_declarations:
| l=nonempty_list(import_declaration) { l }
; 

%inline
type_declarations0: 
| (* *)               { [] }
| t=type_declarations { t }
;

%inline
type_declarations:
| l=nonempty_list(type_declaration) { l }
; 

package_declaration:
| a=annotations0 p=PACKAGE n=name SEMICOLON 
    { 
      let pname = P.name_to_simple_string n in
      env#set_current_package_name pname;
      let d = env#current_source#file#dirname in
      env#classtbl#add_package ~dir:(env#current_source#tree#get_entry d) pname;

      set_attribute_P_P n;
      let loc =
	match a with
	| [] -> Loc.merge p (get_loc $startofs $endofs)
	| _ -> get_loc $startofs $endofs
      in
      mkpkgdecl loc a n 
    }
;

import_declaration: 
| s=single_type_import_declaration           { s }
| t=type_import_on_demand_declaration        { t }
| s=static_single_type_import_declaration    { s }
| s=static_type_import_on_demand_declaration { s }
;

single_type_import_declaration:
| IMPORT n=name SEMICOLON 
    { 
      begin
        try
          let fqn =
            try
              env#resolve_qualified_type_name n
            with _ ->
              P.name_to_simple_string n
          in
	  register_identifier_as_typename fqn (rightmost_identifier n);
	  set_attribute_PT_T (mkresolved fqn) n;
          register_qname_as_typename n;
        with
	  _ ->
            ()(*let sn = P.name_to_simple_string n in
            parse_warning $startofs $endofs "failed to resolve %s" sn*)
      end;
      mkimpdecl $startofs $endofs (IDsingle n)
    }
;

static_single_type_import_declaration:
| IMPORT STATIC n=name DOT i=identifier SEMICOLON 
    { 
      let fqn_opt = ref None in
      begin
        try
          let fqn =
            try
              env#resolve_qualified_type_name n
            with
              _ -> P.name_to_simple_string n
          in
          fqn_opt := Some fqn;
          register_identifier_as_typename fqn (rightmost_identifier n);
          set_attribute_PT_T (mkresolved fqn) n;
          register_qname_as_typename n;
        with
          _ ->
            ()(*let sn = P.name_to_simple_string n in
            parse_warning $startofs $endofs "failed to resolve %s" sn*)
      end;
      let _, id = i in
      let sfqn =
        match !fqn_opt with
        | Some x -> x^"."^id
        | _ -> id
      in
      register_identifier_as_static_member sfqn id;
      mkimpdecl $startofs $endofs (IDsingleStatic(n, id)) 
    }
;

type_import_on_demand_declaration:
| IMPORT n=name DOT STAR SEMICOLON 
    { 
      begin
        try
          ignore (env#resolve_qualified_type_name n)
        with
          _ ->
            let sn = P.name_to_simple_string n in
            (*parse_warning $startofs $endofs "failed to resolve %s" sn;*)
            try
              let p =
                Filename.concat env#classtbl#get_source_dir#path (Common.pkg_to_path sn)
              in
              env#classtbl#add_package ~dir:(env#current_source#tree#get_entry p) sn
            with
              _ -> env#classtbl#add_api_package sn
      end;
      set_attribute_PT_PT n;
      mkimpdecl $startofs $endofs (IDtypeOnDemand n)
    }
;

static_type_import_on_demand_declaration:
| IMPORT STATIC n=name DOT STAR SEMICOLON 
    { 
      begin
        try
          let fqn =
            try
              env#resolve_qualified_type_name n
            with
              _ -> P.name_to_simple_string n
          in
          register_identifier_as_typename fqn (rightmost_identifier n);
          set_attribute_PT_T (mkresolved fqn) n;
          register_qname_as_typename n;
        with
          _ ->
            ()(*let sn = P.name_to_simple_string n in
            parse_warning $startofs $endofs "failed to resolve %s" sn*)
      end;
      mkimpdecl $startofs $endofs (IDstaticOnDemand n)
    }
;

type_declaration: 
| c=class_declaration     { mktd $startofs $endofs (TDclass c) }
| e=enum_declaration      { mktd $startofs $endofs (TDclass e) }
| i=interface_declaration { mktd $startofs $endofs (TDinterface i) }
| SEMICOLON               { mktd $startofs $endofs TDempty }
;

%inline
modifiers_opt:
| o=ioption(modifiers) { o }
;

modifiers:
| l=nonempty_list(annotation_or_modifier) { mkmods $startofs $endofs l }
;

%inline
annotation_or_modifier:
| a=annotation     { annot_to_mod a }
| m=adhoc_modifier { mkmod $startofs $endofs m }
;

adhoc_modifier:
| PUBLIC       { Mpublic }
| PROTECTED    { Mprotected }
| PRIVATE      { Mprivate }
| STATIC       { Mstatic }
| ABSTRACT     { Mabstract }
| FINAL        { Mfinal }
| NATIVE       { Mnative }
| SYNCHRONIZED { Msynchronized }
| TRANSIENT    { Mtransient }
| VOLATILE     { Mvolatile }
| STRICTFP     { Mstrictfp }
| DEFAULT      { Mdefault }
;

%inline
annotations0:
| (* *)         { [] }
| a=annotations { a }
;

annotations:
| l=nonempty_list(annotation) { l }
(*|               a=annotation { [a] }
| l=annotations a=annotation { l @ [a] }*)
;

annotation:
| AT a=annotation_body { mkannot $startofs $endofs a }
;

annotation_body:
| n=normal_annotation_body         { n }
| m=marker_annotation_body         { m }
| s=single_element_annotation_body { s }
;

normal_annotation_body:
| n=name LPAREN e=element_value_pairs0 RPAREN { Anormal(n, e) }
;

marker_annotation_body:
| n=name { Amarker n }
;

single_element_annotation_body:
| n=name LPAREN e=element_value RPAREN { AsingleElement(n, e) }
;

%inline
element_value_pairs0:
| l=clist0(element_value_pair) { l }
;

element_value_pair:
| i=identifier EQ e=element_value { let _, id = i in mkevp $startofs $endofs (id, e) }
;

element_value:
| a=annotation                      { mkev $startofs $endofs (EVannotation a) }
| e=element_value_array_initializer { mkev $startofs $endofs (EVarrayInit e) }
| c=conditional_expression          { mkev $startofs $endofs (EVconditional c) }
;

element_value_array_initializer:
| LBRACE COMMA                   RBRACE { [] }
| LBRACE e=element_values0       RBRACE { e }
| LBRACE e=element_values0_comma RBRACE { e }
;

%inline
element_values0:
| l=clist0(element_value) { l }
;

%inline
element_values0_comma:
| l=nonempty_list(element_value_comma) { l }
;

%inline
element_value_comma:
| e=element_value COMMA { e }
;

class_declaration_head0:
| m_opt=modifiers_opt CLASS i=identifier 
    { 
      let _, id = i in
      register_identifier_as_class (mkfqn_cls id) id;
      begin_scope ~kind:(FKclass id) ();
      m_opt, id
    }
;
class_declaration_head1:
| ch=class_declaration_head0 ts_opt=type_parameters_opt { ch, ts_opt }
;
class_declaration_head:
| ch1=class_declaration_head1 s_opt=super_opt i_opt=interfaces_opt
    { 
      let ch0, ts_opt = ch1 in
      end_typeparameter_scope ts_opt;
      let ms, id = ch0 in
      mkch $startofs $endofs ms id ts_opt s_opt i_opt
    }

class_declaration:
| ch=class_declaration_head b=class_body
    { 
      mkcd $startofs $endofs (CDclass(ch, b))
    }
;

super_ext:
| EXTENDS ct=class_type { mkexc $startofs $endofs ct }
;

%inline
super_opt:
| o=ioption(super_ext) { o }
;

interfaces:
| IMPLEMENTS i=interface_type_list { mkim $startofs $endofs i }
;

%inline
interfaces_opt:
| o=ioption(interfaces) { o }
;

%inline
interface_type_list:
| l=clist(interface_type) { l }
;

class_body: 
| LBRACE c=class_body_declarations0 RBRACE { end_scope(); mkcb $startofs $endofs c }
;

%inline
class_body_opt:
| o=ioption(class_body) { o }
;

%inline
class_body_declarations0:
| l=list(class_body_declaration) { l }
;

class_body_declaration:
| c=class_member_declaration { c }
| s=static_initializer       { s }
| i=instance_initializer     { i }
| c=constructor_declaration  { mkcbd $startofs $endofs (CBDconstructor c) }
(*| error                      { mkerrcbd $startofs $endofs "" }*)
(*| s=ERROR                    { mkerrcbd $startofs $endofs s }*)
;

class_member_declaration:
| f=field_declaration     { mkcbd $startofs $endofs (CBDfield f) }
| m=method_declaration    { m }
| c=class_declaration     { mkcbd $startofs $endofs (CBDclass c) }
| e=enum_declaration      { mkcbd $startofs $endofs (CBDclass e) }
| i=interface_declaration { mkcbd $startofs $endofs (CBDinterface i) }
| SEMICOLON               { mkcbd $startofs $endofs CBDempty }
| s=MARKER                { mkerrcbd $startofs $endofs s }
;

enum_declaration_head0:
| m_opt=modifiers_opt ENUM i=IDENTIFIER
    { 
      let _, id = i in
      match env#java_lang_spec with
      | Common.JLS3 | Common.JLSx ->
	  env#set_java_lang_spec_JLS3;
	  register_identifier_as_class (mkfqn_cls id) id;
	  begin_scope ~kind:(FKclass id) ();
	  m_opt, id
      | Common.JLS2 ->
	  parse_error $symbolstartofs $endofs "'enum' declaration is not available in JLS2"
    }
;
enum_declaration_head:
| eh0=enum_declaration_head0 i_opt=interfaces_opt
    { 
      let ms, id = eh0 in
      mkch $startofs $endofs ms id None None i_opt
    }
;
enum_declaration:
| eh=enum_declaration_head b_opt=enum_body
    { 
      mkcd $startofs $endofs (CDenum(eh, b_opt))
    }
;

enum_body:
| LBRACE                  comma_opt b=enum_body_declarations0 RBRACE
    { end_scope(); mkeb $startofs $endofs [] b }
| LBRACE e=enum_constants comma_opt b=enum_body_declarations0 RBRACE
    { end_scope(); mkeb $startofs $endofs e b } 
;

%inline
comma_opt:
| ioption(COMMA) {}
;


enum_constants:
|                         e=enum_constant { [e] }
| es=enum_constants COMMA e=enum_constant { es @ [e] }
;

enum_constant_head:
| a=annotations0 i=identifier e=enum_arguments_opt          
    { 
      let loc0, id = i in
      register_identifier_as_enumconst (mkfqn id) id;
      let loc =
	match a with
	| [] -> Loc.merge loc0 (get_loc $startofs $endofs)
	| _ -> get_loc $startofs $endofs
      in
      begin_scope();
      (loc, a, id, e)
    }
;

enum_constant:
| x=enum_constant_head
    { 
      let loc, a, id, e = x in
      end_scope();
      mkec loc a id e None
    }
| x=enum_constant_head c=class_body
    { 
      let loc, a, id, e = x in
      mkec loc a id e (Some c)
    }
;

%inline 
enum_arguments_opt:
| (* *)                          { None }
| LPAREN a=argument_list0 RPAREN { Some(mkargs $startofs $endofs a) }
;

enum_body_declarations0:
| (* *)                                { [] }
| s=SEMICOLON c=class_body_declarations0
    { 
      ignore s;
      (mkcbd $startofs $endofs(s) CBDempty) :: c
    }
;

field_declaration:
| m_opt=modifiers_opt t=unann_type v=variable_declarators SEMICOLON 
    { 
      let loc = 
	match m_opt with
	| None -> Loc.merge t.ty_loc (get_loc $symbolstartofs $endofs)
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      List.iter 
	(fun vd -> 
	  register_identifier_as_field (fst vd.vd_variable_declarator_id) t;
	  vd.vd_is_local := false;
	) v;
      Ast.proc_type (register_qname_as_typename ~skip:1) t;
      mkfd loc m_opt t v
    }
;

%inline
variable_declarators:
| l=clist(variable_declarator) { l }
;

variable_declarator: 
| v=variable_declarator_id                            { mkvd $startofs $endofs v None }
| v=variable_declarator_id EQ vi=variable_initializer { mkvd $startofs $endofs v (Some vi) }
(*| v=variable_declarator_id EQ s=ERROR
    { 
      let vi = mkerrvi $startofs(s) $endofs s in
      mkvd $startofs $endofs v (Some vi)
    }*)
;

variable_declarator_id: 
| i=identifier                               { let _, id = i in id, 0 }
| v=variable_declarator_id LBRACKET RBRACKET { let id, dim = v in id, dim + 1 }
;

variable_initializer: 
(*| e=expression        { mkvi $startofs $endofs (VIexpression e) }*)
| e=expr_or_err        { mkvi $startofs $endofs (VIexpression e) }
| a=array_initializer { a }
;

method_declaration:
| mh=method_header b=method_body 
    { 
      if mh_is_generic mh then end_scope();
      mkcbd $startofs $endofs (CBDmethod(mh, b)) 
    }
;

void:
| VOID { mktype $startofs $endofs (Tvoid) }
;

method_header:
| m_opt=modifiers_opt ts_opt=type_parameters_opt tv=type_or_void md=method_declarator t_opt=throws_opt 
    { 
      let (id, params_loc, params), dim = md in
      let loc =
	match m_opt with
	| None -> begin
            match ts_opt with
            | Some ts -> Loc.merge ts.tps_loc (get_loc $symbolstartofs $endofs)
            | None    -> Loc.merge tv.ty_loc (get_loc $symbolstartofs $endofs)
        end
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      Ast.proc_type (register_qname_as_typename ~skip:1) tv;
      mkmh loc m_opt ts_opt tv id params_loc params dim t_opt
    }
;

%inline
type_or_void:
| j=unann_type { j }
| v=void       { v }
;


method_declarator_head: 
| i=identifier lp=LPAREN 
    { 
      let _, id = i in
      register_identifier_as_method id; 
      begin_scope(); 
      id, lp
    }
;

method_declarator:
| m=method_declarator_head f=formal_parameter_list0 rp=RPAREN 
       { 
	 let id, lparen_loc = m in
	 let params_loc = Loc.merge lparen_loc rp in
	 (id, params_loc, f), 0 
       }
| m=method_declarator LBRACKET RBRACKET { let md, dim = m in md, dim + 1 }
;

%inline
formal_parameter_list0:
| l=clist0(formal_parameter) { l }
;

formal_parameter:
| v=variable_modifiers_opt t=unann_type d=variable_declarator_id 
    { 
      let loc = 
	match v with
	| None -> Loc.merge t.ty_loc (get_loc $symbolstartofs $endofs)
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      register_identifier_as_parameter (fst d) t;
      Ast.proc_type (register_qname_as_typename ~skip:2) t;
      mkfp loc v t d false 
    }

| v=variable_modifiers_opt t=unann_type ELLIPSIS d=variable_declarator_id 
    { 
      let loc = 
	match v with
	| None -> Loc.merge t.ty_loc (get_loc $symbolstartofs $endofs)
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      register_identifier_as_parameter (fst d) t;
      Ast.proc_type (register_qname_as_typename ~skip:2) t;
      mkfp loc v t d true 
    }
;

%inline
variable_modifiers_opt:
| (* *)                { None }
| v=variable_modifiers { Some (mkmods $startofs $endofs v) }
;

%inline
variable_modifiers:
| l=nonempty_list(variable_modifier) { l }
;

variable_modifier:
| FINAL        { mkmod $startofs $endofs Mfinal }
| a=annotation { mkmod $startofs $endofs (Mannotation a) }
;

%inline 
throws_opt:
| o=ioption(throws) { o }
;

throws:
| THROWS c=class_type_list { mkth $startofs $endofs c }
;

%inline
class_type_list:
| l=clist(class_type) { l }
;

method_body:
| b=block   { end_scope(); Some b }
| SEMICOLON { end_scope(); None }
;

static_initializer:
| STATIC b=block { mkcbd $startofs $endofs (CBDstaticInitializer b) }
;

instance_initializer:
| b=block { mkcbd $startofs $endofs (CBDinstanceInitializer b) }
;

constructor_declaration:
| m_opt=modifiers_opt ts_opt=type_parameters_opt c=constructor_declarator t_opt=throws_opt cb=constructor_body
    { 
      end_typeparameter_scope ts_opt;
      let cloc, sn, params_loc, params = c in
      let loc = 
	match m_opt with
	| None -> begin
            match ts_opt with
            | Some ts -> Loc.merge ts.tps_loc (get_loc $symbolstartofs $endofs)
            | None ->  Loc.merge cloc (get_loc $symbolstartofs $endofs)
        end
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      mkcnd loc m_opt ts_opt sn params_loc params t_opt cb
    }
;

constructor_declarator_head:
| s=simple_name lp=LPAREN 
    { 
      register_identifier_as_constructor s; 
      begin_scope(); 
      s, lp 
    }
;

constructor_declarator:
| c=constructor_declarator_head f=formal_parameter_list0 rp=RPAREN 
    { 
      let n, lparen_loc = c in
      let params_loc = Loc.merge lparen_loc rp in
      get_loc $startofs $endofs, n, params_loc, f 
    }
;

constructor_body:
| LBRACE e_opt=ioption(explicit_constructor_invocation) b=loption(block_statements) RBRACE 
    { 
      end_scope(); 
      mkcnb $startofs $endofs e_opt b 
    }
;

this:
| THIS { get_loc $startofs $endofs }
;

super:
| SUPER { get_loc $startofs $endofs }
;

explicit_constructor_invocation:
| this a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIthis(None, a)) }

| t=type_arguments this a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIthis(Some t, a)) }

| super a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIsuper(None, a)) }

| t=type_arguments super a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIsuper(Some t, a)) }

| p=primary DOT super a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIprimary(p, None, a)) }

| p=primary DOT t=type_arguments super a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIprimary(p, Some t, a)) }

| n=name DOT super a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIname(n, None, a)) }

| n=name DOT t=type_arguments super a=arguments SEMICOLON 
    { mkeci $startofs $endofs (ECIname(n, Some t, a)) }
;

%inline
arguments:
| lp=LPAREN a=argument_list0 rp=RPAREN { _mkargs (Loc.merge lp rp) a }
;

interface_declaration:
| n=normal_interface_declaration { n }
| a=annotation_type_declaration  { a }
;

normal_interface_declaration_head0:
| m_opt=modifiers_opt INTERFACE i=identifier 
    { 
      let _, id = i in
      register_identifier_as_interface (mkfqn_cls id) id; 
      begin_scope ~kind:(FKclass id) ();
      m_opt, id
    }
;
normal_interface_declaration_head1:
| h0=normal_interface_declaration_head0 ts_opt=type_parameters_opt { h0, ts_opt }
;
normal_interface_declaration_head:
| h1=normal_interface_declaration_head1 e_opt=extends_interfaces_opt
    { 
      let h0, ts_opt = h1 in
      end_typeparameter_scope ts_opt;
      let ms, id = h0 in
      mkifh $startofs $endofs ms id ts_opt e_opt
    }
;
normal_interface_declaration:
| h=normal_interface_declaration_head b=interface_body
    { 
      mkifd $startofs $endofs (IFDnormal(h, b))
    }
;

annotation_type_declaration_head:
| m_opt=modifiers_opt AT__INTERFACE INTERFACE i=identifier
    { 
      let _, id = i in
      register_identifier_as_interface (mkfqn_cls id) id;
      begin_scope ~kind:(FKclass id) ();
      mkifh $startofs $endofs m_opt id None None
    }
;

annotation_type_declaration:
| h=annotation_type_declaration_head b=annotation_type_body
    { 
      mkifd $startofs $endofs (IFDannotation(h, b))
    }
;

annotation_type_body:
| LBRACE a=annotation_type_member_declarations0 RBRACE { end_scope(); mkatb $startofs $endofs a }
;

%inline
annotation_type_member_declarations0:
| l=list(annotation_type_member_declaration) { l }
;

annotation_type_member_declaration:
| c=constant_declaration { mkatmd $startofs $endofs (ATMDconstant c) }
| m_opt=modifiers_opt j=unann_type i=identifier LPAREN RPAREN a=ann_dims0 d=default_value_opt SEMICOLON 
    { 
      let loc = 
	match m_opt with
	| None -> Loc.merge j.ty_loc (get_loc $symbolstartofs $endofs)
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      let _, id = i in
      _mkatmd loc (ATMDelement(m_opt, j, id, a, d))
    }
| c=class_declaration     { _mkatmd c.cd_loc (ATMDclass c)  }
| e=enum_declaration      { _mkatmd e.cd_loc (ATMDclass e) }
| i=interface_declaration { _mkatmd i.ifd_loc (ATMDinterface i) }
| SEMICOLON               { mkatmd $startofs $endofs ATMDempty }
;

default_value_opt:
| d_opt=ioption(default_value) { d_opt }
;

default_value:
| DEFAULT e=element_value { e }
;

%inline
ann_dims0:
| (* *)      { [] }
| a=ann_dims { a }
;

ann_dims:
|            a=ann_dim { [a] }
| d=ann_dims a=ann_dim { d @ [a] }
;

ann_dim:
| a=annotations0 LBRACKET RBRACKET { mkad $startofs $endofs a }
;

extends_interfaces_opt:
| (* *)                { None }
| e=extends_interfaces { Some(mkexi $startofs $endofs e) }
;

%inline
extends_interfaces:
| EXTENDS l=clist(interface_type) { l }
;

interface_body:
| LBRACE i=interface_member_declarations0 RBRACE { end_scope(); mkib $startofs $endofs i }
;

%inline
interface_member_declarations0:
| l=list(interface_member_declaration) { l }
;

interface_member_declaration:
| c=constant_declaration         { IMDconstant c }
| a=interface_method_declaration { IMDinterfaceMethod a }
| c=class_declaration            { IMDclass c }
| e=enum_declaration             { IMDclass e }
| i=interface_declaration        { IMDinterface i }
| SEMICOLON                      { IMDempty }
;

%inline
constant_declaration:
| f=field_declaration { f }
;

interface_method_declaration:
| mh=method_header b=method_body
    { 
      if mh_is_generic mh then end_scope();
      let loc = Loc.merge mh.mh_loc (get_loc $startofs $endofs) in
      mkimd loc mh b
    }
;

array_initializer:
| LBRACE                         ioption(COMMA) RBRACE { mkvi $startofs $endofs (VIarray []) }
| LBRACE v=variable_initializers ioption(COMMA) RBRACE
    { mkvi $startofs $endofs (VIarray (List.rev v)) }
;

variable_initializers:
|                                v=variable_initializer { [v] }
| vs=variable_initializers COMMA v=variable_initializer { v::vs }
;

block: 
| LBRACE b=block_statements0 RBRACE { mkb $startofs $endofs b }
;

%inline
block_statements0: 
| (* *)              { [] }
| b=block_statements { b }
;

%inline
block_statements: 
| l=nonempty_list(block_statement) { l }
;

block_statement: 
| l=local_variable_declaration_statement { l }
| c=class_declaration                    { mkbs $startofs $endofs (BSclass c) }
| s=statement                            { mkbs $startofs $endofs (BSstatement s) }
| e=enum_declaration                     { mkbs $startofs $endofs (BSclass e) }
(*| error                                  { mkerrbs $symbolstartofs $endofs "" }*)
| s=ERROR                                { mkerrbs $startofs $endofs s }
| s=MARKER                               { mkerrbs $startofs $endofs s }
| s=BLOCK_STMT                           { s }
;

local_variable_declaration_statement:
| l=local_variable_declaration SEMICOLON { mkbs $startofs $endofs (BSlocal l) }
;

local_variable_declaration:
| m_opt=modifiers_opt t=unann_type v=variable_declarators 
    { 
       List.iter
       (fun vd ->
	 register_identifier_as_variable (fst vd.vd_variable_declarator_id) t;
	 vd.vd_is_local := true;
       ) v;
      mklvd $symbolstartofs $endofs m_opt t v
    }
;

statement:
| s=statement_without_trailing_substatement { s }
| l=labeled_statement                       { l }
| i=if_then_statement                       { i }
| i=if_then_else_statement                  { i }
| w=while_statement                         { w }
| f=for_statement                           { f }
| e=enhanced_for_statement                  { e }
;

statement_no_short_if:
| s=statement_without_trailing_substatement { s }
| l=labeled_statement_no_short_if           { l }
| i=if_then_else_statement_no_short_if      { i }
| w=while_statement_no_short_if             { w }
| f=for_statement_no_short_if               { f }
| e=enhanced_for_statement_no_short_if      { e }
;

statement_without_trailing_substatement:
| b=block                  { mkstmt $startofs $endofs (Sblock b) }
| e=empty_statement        { e }
| e=expression_statement   { e }
| s=switch_statement       { s }
| d=do_statement           { d }
| b=break_statement        { b }
| c=continue_statement     { c }
| r=return_statement       { r }
| s=synchronized_statement { s }
| t=throw_statement        { t }
| t=try_statement          { t }
| a=assert_statement       { a }
| s=STMT                   { s }
| s=ERROR_STMT             { mkerrstmt $startofs $endofs s }
;

empty_statement:
| SEMICOLON { mkstmt $startofs $endofs Sempty }
;

labeled_statement_head:
| i=identifier COLON 
    { 
      let _, id = i in
      register_identifier_as_label id; 
      begin_scope(); 
      id
    }
;

labeled_statement:
| l=labeled_statement_head s=statement { end_scope(); mkstmt $startofs $endofs (Slabeled(l, s)) }
;

labeled_statement_no_short_if:
| l=labeled_statement_head s=statement_no_short_if 
    { end_scope(); mkstmt $startofs $endofs (Slabeled(l, s)) }
;

expression_statement:
| s=statement_expression SEMICOLON { mkstmt $startofs $endofs (Sexpression s) }
(*| error                SEMICOLON { mkerrstmt $startofs $endofs } *)
;

statement_expression:
| a=assignment                         { mkstmtexpr $startofs $endofs (SEassignment a) }
| p=pre_increment_expression           { mkstmtexpr $startofs $endofs (SEpreIncrement p) }
| p=pre_decrement_expression           { mkstmtexpr $startofs $endofs (SEpreDecrement p) }
| p=post_increment_expression          { mkstmtexpr $startofs $endofs (SEpostIncrement p) }
| p=post_decrement_expression          { mkstmtexpr $startofs $endofs (SEpostDecrement p) }
| m=method_invocation                  { mkstmtexpr $startofs $endofs (SEmethodInvocation m) }
| c=class_instance_creation_expression { mkstmtexpr $startofs $endofs (SEclassInstanceCreation c) }
;

if_then_statement:
| IF LPAREN e=expr_or_err RPAREN s=statement { mkstmt $startofs $endofs (SifThen(e, s)) }
;

if_then_else_statement:
| IF LPAREN e=expr_or_err RPAREN s0=statement_no_short_if ELSE s1=statement 
     { mkstmt $startofs $endofs (SifThenElse(e, s0, s1)) }
;

if_then_else_statement_no_short_if:
| IF LPAREN e=expr_or_err RPAREN s0=statement_no_short_if ELSE s1=statement_no_short_if 
    { mkstmt $startofs $endofs (SifThenElse(e, s0, s1)) }
(*
| IF LPAREN e=expression RPAREN er=error                 ELSE s=statement_no_short_if 
    { 
      let err = mkerrstmt $startofs(er) $endofs(er) in
      let _ = er in
      mkstmt $startofs $endofs (SifThenElse(e, err, s))
    }
*)
;

switch_statement:
| SWITCH LPAREN e=expr_or_err RPAREN s=switch_block { mkstmt $startofs $endofs (Sswitch(e, s)) }
;

switch_block:
| LBRACE                                                 RBRACE { mksb $startofs $endofs [] }
| LBRACE                                 s=switch_labels RBRACE { mksb $startofs $endofs [(s, [])] }
| LBRACE g=switch_block_statement_groups                 RBRACE { mksb $startofs $endofs (List.rev g) }
| LBRACE g=switch_block_statement_groups s=switch_labels RBRACE { mksb $startofs $endofs (List.rev ((s, [])::g)) }
;

switch_block_statement_groups:
|                                  s=switch_block_statement_group { [s] }
| ss=switch_block_statement_groups s=switch_block_statement_group { s :: ss }
;

switch_block_statement_group: 
| s=switch_labels b=block_statements { s, b }
;

%inline
switch_labels:
| l=nonempty_list(switch_label) { l }
;

switch_label: 
| CASE c=constant_expression COLON { mksl $startofs $endofs (SLconstant c) }
| DEFAULT__COLON             COLON { mksl $startofs $endofs SLdefault }
;

while_statement:
| WHILE LPAREN e=expr_or_err RPAREN s=statement { mkstmt $startofs $endofs (Swhile(e, s)) }
;  

while_statement_no_short_if:
| WHILE LPAREN e=expr_or_err RPAREN s=statement_no_short_if { mkstmt $startofs $endofs (Swhile(e, s)) }
;

do_statement:
| DO s=statement WHILE LPAREN e=expression RPAREN SEMICOLON { mkstmt $startofs $endofs (Sdo(s, e)) }
(*
| DO er=error    WHILE LPAREN e=expression RPAREN SEMICOLON 
     { 
       let err = mkerrstmt $startofs(er) $endofs(er) in
       let _ = er in
       mkstmt $startofs $endofs (Sdo(err, e))
     }
*)
;

for_statement_head:
| FOR LPAREN { begin_scope() }
;

javatype_vdid:
| j=unann_type d=variable_declarator_id
    { 
      register_identifier_as_variable (fst d) j;
      mkfp (get_loc $startofs $endofs) None j d false
    }
;

enhanced_for_statement:
| for_statement_head j=javatype_vdid COLON e=expr_or_err RPAREN s=statement
    { end_scope(); mkstmt $startofs $endofs (SforEnhanced(j, e, s)) }

| for_statement_head m=modifiers j=javatype_vdid COLON e=expr_or_err RPAREN s=statement
    { end_scope();
      let fp0 = j in
      let fp =
	mkfp (Loc.merge m.Ast.ms_loc fp0.Ast.fp_loc) (Some m)
	  fp0.Ast.fp_type fp0.Ast.fp_variable_declarator_id false
      in
      mkstmt $startofs $endofs (SforEnhanced(fp, e, s))
    }
;

enhanced_for_statement_no_short_if:
| for_statement_head j=javatype_vdid COLON e=expr_or_err RPAREN s=statement_no_short_if
    { end_scope(); mkstmt $startofs $endofs (SforEnhanced(j, e, s)) }

| for_statement_head m=modifiers j=javatype_vdid COLON e=expr_or_err RPAREN s=statement_no_short_if
    { end_scope();
      let fp0 = j in
      let fp =
	mkfp (Loc.merge m.Ast.ms_loc fp0.Ast.fp_loc) (Some m)
	  fp0.Ast.fp_type fp0.Ast.fp_variable_declarator_id false
      in
      mkstmt $startofs $endofs (SforEnhanced(fp, e, s))
    }
;


for_statement:
| for_statement_head i_opt=for_init_opt SEMICOLON e_opt=expression_opt SEMICOLON u=for_update0 RPAREN s=statement 
    { end_scope(); mkstmt $startofs $endofs (Sfor(i_opt, e_opt, u, s)) }
;

for_statement_no_short_if:
| for_statement_head i_opt=for_init_opt SEMICOLON e_opt=expression_opt SEMICOLON u=for_update0 RPAREN s=statement_no_short_if
    { end_scope(); mkstmt $startofs $endofs (Sfor(i_opt, e_opt, u, s)) }
;

for_init_opt:
| o=ioption(for_init) { o }
;

for_init: 
| s=statement_expression_list   { mkfi $startofs $endofs (FIstatement s)}
| l=local_variable_declaration  { mkfi $startofs $endofs (FIlocal l) }
;

for_update0:
| (* *)        { [] }
| f=for_update { f }
;

for_update:
| s=statement_expression_list { s }
;

%inline
statement_expression_list:
| l=clist(statement_expression) { l }
;

%inline
identifier_opt:
| (* *)        { None }
| i=identifier { let _, id = i in Some id }
;

break_statement:
| BREAK i=identifier_opt SEMICOLON { mkstmt $startofs $endofs (Sbreak i) }
(*| BREAK error SEMICOLON { mkerrstmt $startofs $endofs } *)
;

continue_statement:
| CONTINUE i=identifier_opt SEMICOLON { mkstmt $startofs $endofs (Scontinue i) }
(*| CONTINUE error SEMICOLON { mkerrstmt $startofs $endofs } *)
;

return_statement:
| RETURN e_opt=expression_opt SEMICOLON { mkstmt $startofs $endofs (Sreturn e_opt) }
;

throw_statement:
| THROW e=expr_or_err SEMICOLON { mkstmt $startofs $endofs (Sthrow e) }
;

synchronized_statement:
| SYNCHRONIZED LPAREN e=expr_or_err RPAREN b=block { mkstmt $startofs $endofs (Ssynchronized(e, b)) }
;

try_statement:
| TRY r_opt=resource_spec_opt b=block c=catches           { mkstmt $startofs $endofs (Stry(r_opt, b, Some c, None)) }
| TRY r_opt=resource_spec_opt b=block           f=finally { mkstmt $startofs $endofs (Stry(r_opt, b, None, Some f)) }
| TRY r_opt=resource_spec_opt b=block c=catches f=finally { mkstmt $startofs $endofs (Stry(r_opt, b, Some c, Some f)) }
| TRY r_opt=resource_spec_opt b=block                     { mkstmt $startofs $endofs (Stry(r_opt, b, None, None)) }
;

%inline
resource_spec_opt:
| r_opt=ioption(resource_spec) { r_opt }
;

resource_spec:
| LPAREN rl=resource_list ioption(SEMICOLON) RPAREN { mkresspec $symbolstartofs $endofs rl }
;

resource_list:
|                           r=resource { [r] }
| l=resource_list SEMICOLON r=resource { l @ [r] }
;

resource:
| m_opt=variable_modifiers_opt t=unann_type v=variable_declarator_id EQ e=expression 
    { 
      let loc = 
	match m_opt with
	| None -> Loc.merge t.ty_loc (get_loc $symbolstartofs $endofs)
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      register_identifier_as_variable (fst v) t;
      mkres loc m_opt t v e
    }
;

%inline
catches:
| l=nonempty_list(catch_clause) { l }
;

catch_clause_header:
| CATCH { begin_scope() }

catch_formal_parameter:
| ms_opt=variable_modifiers_opt tl=catch_type d=variable_declarator_id
    {
      let loc = 
	match ms_opt with
	| None -> Loc.merge (List.hd tl).ty_loc (get_loc $symbolstartofs $endofs)
	| Some _ -> get_loc $symbolstartofs $endofs
      in
      List.iter (register_identifier_as_parameter (fst d)) tl;
      mkcfp loc ms_opt tl d
    }
;

catch_type:
|                 t=unann_class_type { [t] }
| l=catch_type OR t=class_type       { l @ [t] }
;

catch_clause: 
| catch_clause_header LPAREN f=catch_formal_parameter RPAREN b=block 
    { end_scope(); mkcatch $startofs $endofs f b }
;

finally:
| FINALLY b=block { mkfinally $startofs $endofs b }
;


%inline
expr_or_err:
| e=expression { e }
(*| error        { mkerrexpr $startofs $endofs }*)
| s=ERROR { mkerrexpr $startofs $endofs s }
;

%inline
colon__expr:
| COLON e=expr_or_err { e }
;

assert_statement:
| ASSERT b=expression e_opt=ioption(colon__expr) SEMICOLON
    { 
      let lab =
        match e_opt with
        | Some e -> Sassert2(b, e)
        | None   -> Sassert1 b
      in
      match env#java_lang_spec with
      | Common.JLS3 | Common.JLSx -> 
	  env#set_java_lang_spec_JLS3;
	  mkstmt $startofs $endofs lab
      | Common.JLS2 -> 
          parse_error $startofs $endofs "assert statement is not available in JLS2"
    }
;


primary:
| p=primary_no_new_array  { p }
| a=array_creation_init   { mkprim $startofs $endofs (ParrayCreationExpression a) }
| a=array_creation_noinit { mkprim $startofs $endofs (ParrayCreationExpression a) }
(*| error                 { mkerrprim $startofs $endofs } *)
;

primary_no_new_array:
| l=literal                            { mkprim $startofs $endofs (Pliteral l) }
| this                                 { mkprim $startofs $endofs Pthis }
| LPAREN n=name          RPAREN        { mkprim $startofs $endofs (Pparen(_name_to_expr n.n_loc n)) }
| LPAREN e=expression_nn RPAREN        { mkprim $startofs $endofs (Pparen e) }

(*| LPAREN error           RPAREN        { mkerrprim $startofs $endofs }*)

| c=class_instance_creation_expression { mkprim $startofs $endofs (PclassInstanceCreation c) }
| f=field_access       { mkprim $startofs $endofs (PfieldAccess f) }
| m=method_invocation  { mkprim $startofs $endofs (PmethodInvocation m) }
| a=array_access       { mkprim $startofs $endofs (ParrayAccess a) }
| n=name DOT this      { register_qname_as_typename n; mkprim $startofs $endofs (PqualifiedThis n) }
| n=name DOT CLASS     { register_qname_as_typename n; mkprim $startofs $endofs (PclassLiteral (name_to_ty [] n)) }
| n=name d=ann_dims DOT CLASS 
    { 
      register_qname_as_typename n;
      let ty = _mkty (Loc.merge n.n_loc (Xlist.last d).Ast.ad_loc) (Tarray(name_to_ty [] n, List.length d)) in
      mkprim $startofs $endofs (PclassLiteral ty) 
    }
| p=unann_primitive_type        DOT CLASS { mkprim $startofs $endofs (PclassLiteral p) }

| p=unann_primitive_type d=ann_dims DOT CLASS 
    { 
      let ty = _mkty (Loc.merge p.ty_loc (Xlist.last d).Ast.ad_loc) (Tarray(p, List.length d)) in
      mkprim $startofs $endofs (PclassLiteral ty) 
    }

| void DOT CLASS { mkprim $startofs $endofs PclassLiteralVoid }

| r=method_reference { mkprim $startofs $endofs (PmethodReference r) }
;

method_reference:
| n=name           COLON_COLON tas_opt=type_arguments_opt i=identifier
    { 
      env#reclassify_identifier(leftmost_of_name n);
      let _, id = i in
      mkmr $startofs $endofs (MRname(n, tas_opt, id))
    }
| p=primary        COLON_COLON tas_opt=type_arguments_opt i=identifier
    { 
      let _, id = i in
      mkmr $startofs $endofs (MRprimary(p, tas_opt, id))
    }
| SUPER            COLON_COLON tas_opt=type_arguments_opt i=identifier
    { 
      let _, id = i in
      mkmr $startofs $endofs (MRsuper(tas_opt, id))
    }
| n=name DOT SUPER COLON_COLON tas_opt=type_arguments_opt i=identifier
    { 
      register_qname_as_typename n;
      let _, id = i in
      mkmr $startofs $endofs (MRtypeSuper(n, tas_opt, id))
    }
(*| n=name           COLON_COLON tas_opt=type_arguments_opt NEW
    { 
      register_qname_as_typename n;
      mkmr $startofs $endofs (MRtypeNew(n, tas_opt))
    }*)
| p=unann_primitive_type d=ann_dims0 COLON_COLON tas_opt=type_arguments_opt NEW
    { 
      let ty =
	match d with
	| [] -> p
	| l -> _mkty (Loc.merge p.ty_loc (Xlist.last l).Ast.ad_loc) (Tarray(p, List.length d))
      in
      mkmr $startofs $endofs (MRtypeNew(ty, tas_opt))
    }
| n=name d=ann_dims0 COLON_COLON tas_opt=type_arguments_opt NEW
    { 
      let ty =
        match d with
        | [] -> name_to_ty [] n
        | l -> _mkty (Loc.merge n.n_loc (Xlist.last l).Ast.ad_loc) (Tarray(name_to_ty [] n, List.length d))
      in
      register_qname_as_typename n;
      mkmr $startofs $endofs (MRtypeNew(ty, tas_opt))
    }
;

class_instance_creation_head:
| NEW                  c=class_or_interface_type { begin_scope(); None, c }
| NEW t=type_arguments c=class_or_interface_type { begin_scope(); Some t, c }
;

class_instance_creation_head_qualified:
| p=primary DOT NEW t=type_arguments_opt { begin_scope(); Some p, None, t }
| n=name    DOT NEW t=type_arguments_opt { begin_scope(); None, Some n, t }
;

class_instance_creation_expression: 
| c=class_instance_creation_head a=arguments cb=class_body_opt
    { 
      let tyargs, ty = c in
      begin 
	match cb with 
	| None -> end_scope() 
	| _ -> () 
      end;
      mkcic $startofs $endofs (CICunqualified(tyargs, ty, a, cb))
    }

| c=class_instance_creation_head_qualified i=identifier 
    t=type_arguments_opt a=arguments cb=class_body_opt
    { 
      let _, id = i in
      let po, no, tyargs = c in
      let cic =
	match po, no with
	| Some p, None -> 
	    CICqualified(p, tyargs, id, t, a, cb)
	| None, Some n ->
	    CICnameQualified(n, tyargs, id, t, a, cb)
	| _ -> assert false
      in
      begin 
	match cb with 
	| None -> end_scope() 
	| _ -> () 
      end;
      mkcic $startofs $endofs cic
    }
;

%inline
argument_list0:
(*| l=clist0(expression) { l }*)
| l=clist0(expr_or_err) { l }
;

array_creation_noinit:
| NEW p=primitive_type          d=dim_exprs      { ACEtype(p, List.rev d, 0) }
| NEW c=class_or_interface_type d=dim_exprs      { ACEtype(c, List.rev d, 0) }
| NEW p=primitive_type          d=dim_exprs dm=ann_dims
     { 
       ACEtype(p, List.rev d, List.length dm)
     }
| NEW c=class_or_interface_type d=dim_exprs dm=ann_dims
     { 
       ACEtype(c, List.rev d, List.length dm)
     }
;

array_creation_init:
| NEW p=primitive_type          d=ann_dims a=array_initializer { ACEtypeInit(p, (List.length d), [a]) }
| NEW c=class_or_interface_type d=ann_dims a=array_initializer { ACEtypeInit(c, (List.length d), [a]) }
;

dim_exprs:
|              d=dim_expr { [d] }
| ds=dim_exprs d=dim_expr { d :: ds }
;

dim_expr:
| LBRACKET e=expression RBRACKET { mkde $startofs $endofs e }
;

(*%inline
dims_opt:
| o=ioption(dims) { o }
;

dims:
|        LBRACKET RBRACKET { 1, get_loc $startofs $endofs }
| d=dims LBRACKET RBRACKET { (fst d) + 1, get_loc $startofs $endofs }
;*)

field_access:
| p=primary DOT           i=identifier 
     { 
       let _, id = i in 
       FAprimary(p, id) 
     }
(*
| e=error   DOT           i=identifier
     {
      let err = mkerrprim $startofs(e) $endofs(e) in
      let _ = e in
      let _, id = i in FAprimary(err, id)
     }
*)
| super     DOT           i=identifier { let _, id = i in FAsuper id }
| n=name    DOT super DOT i=identifier 
     { 
       register_qname_as_typename n;
       let _, id = i in
       set_attribute_PT_T (env#resolve n) n;
       register_qname_as_typename n;
       FAclassSuper(n, id) 
     }
;

method_invocation:
| n=name a=arguments
    { 
      set_attribute_A_M (env#resolve n) n;
      register_qname_as_method n;
      if is_local_name n then begin
	mkmi $startofs $endofs (MImethodName(n, a)) 
      end
      else begin
	try
	  let q = get_qualifier n in
	  let id = rightmost_identifier n in
	  if
            is_local_name q ||
            is_implicit_field_name q ||
            is_field_access q ||
            is_expr_name q
          then begin
            set_name_attribute NAexpression q;
            register_qname_as_expression q;
            env#reclassify_identifier(leftmost_of_name q);
	    mkmi $startofs $endofs (MIprimary(_name_to_prim q.n_loc q, None, id, a))
          end
          else begin
            if is_type_name q then begin
              try
                let fqn = get_type_fqn q in
                set_attribute_PT_T (mkresolved fqn) q;
                register_qname_as_typename q;
                mkmi $startofs $endofs (MItypeName(q, None, id, a))
              with
              | Unknown _ ->
                  set_attribute_PT_T (env#resolve q) q;
                  register_qname_as_typename q;
                  mkmi $startofs $endofs (MItypeName(q, None, id, a))
            end
            else begin
              env#reclassify_identifier(leftmost_of_name q);
              mkmi $startofs $endofs (MIprimary(_name_to_prim q.n_loc q, None, id, a))
              (*raise (Unknown "")*)
            end
          end
	with
	| Not_found -> mkmi $startofs $endofs (MImethodName(n, a))
      end
    }

| p=primary DOT i=identifier a=arguments
    { 
      let _, id = i in
      mkmi $startofs $endofs (MIprimary(p, None, id, a)) 
    }

| p=primary DOT t=type_arguments i=identifier a=arguments
    { 
      let _, id = i in
      mkmi $startofs $endofs (MIprimary(p, Some t, id, a)) 
    }

| q=name DOT t=type_arguments i=identifier a=arguments
    { 
      let _, id = i in
      if
        is_local_name q ||
        is_implicit_field_name q ||
        is_field_access q ||
        is_expr_name q
      then begin
        set_name_attribute NAexpression q;
        register_qname_as_expression q;
        env#reclassify_identifier(leftmost_of_name q);
	mkmi $startofs $endofs (MIprimary(_name_to_prim q.n_loc q, Some t, id, a))
      end
      else begin
        if is_type_name q then begin
          try
            let fqn = get_type_fqn q in
            set_attribute_PT_T (mkresolved fqn) q;
            register_qname_as_typename q;
            mkmi $startofs $endofs (MItypeName(q, Some t, id, a))
          with
          | Unknown _ ->
              set_attribute_PT_T (env#resolve q) q;
              register_qname_as_typename q;
              mkmi $startofs $endofs (MItypeName(q, Some t, id, a))
        end
        else begin
          env#reclassify_identifier(leftmost_of_name q);
          mkmi $startofs $endofs (MIprimary(_name_to_prim q.n_loc q, Some t, id, a))
        end
      end
    }

| s=super DOT i=identifier a=arguments
    { 
      let _, id = i in
      mkmi $startofs $endofs (MIsuper(s, None, id, a)) 
    }

| s=super DOT t=type_arguments i=identifier a=arguments
    { 
      let _, id = i in
      mkmi $startofs $endofs (MIsuper(s, Some t, id, a)) 
    }

| n=name DOT s=super DOT i=identifier a=arguments
    { 
      let _, id = i in
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mkmi $startofs $endofs (MIclassSuper(n.n_loc, s, n, None, id, a)) 
    }

| n=name DOT s=super DOT t=type_arguments i=identifier a=arguments
    { 
      let _, id = i in
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mkmi $startofs $endofs (MIclassSuper(n.n_loc, s, n, Some t, id, a)) 
    }
;

array_access:
| n=name LBRACKET e=expression RBRACKET               
    { 
      set_attribute_A_E (env#resolve n) n;
      register_qname_as_array n;
      if is_local_name n then
	mkaa $startofs $endofs (AAname(n, e))
      else
	mkaa $startofs $endofs (AAprimary(_name_to_prim n.n_loc n, e))
    }
| p=primary_no_new_array LBRACKET e=expression RBRACKET 
     { 
       mkaa $startofs $endofs (AAprimary(p, e)) 
     }
;

postfix_expression:
| p=primary                 { mkexpr $startofs $endofs (Eprimary p) }
| n=name                      
    { 
      set_attribute_A_E (env#resolve n) n;
      register_qname_as_expression n;
      env#reclassify_identifier(leftmost_of_name n);
      name_to_expr $startofs $endofs n
    }
| p=post_increment_expression { p }
| p=post_decrement_expression { p }
;

post_increment_expression:
| p=postfix_expression PLUS_PLUS { mkexpr $startofs $endofs (Eunary(UOpostIncrement, p)) }
;

post_decrement_expression:
| p=postfix_expression MINUS_MINUS { mkexpr $startofs $endofs (Eunary(UOpostDecrement, p)) }
;

unary_expression:
| p=pre_increment_expression        { p }
| p=pre_decrement_expression        { p }
| PLUS  u=unary_expression          { mkexpr $startofs $endofs (Eunary(UOpositive, u)) }
| MINUS u=unary_expression          { mkexpr $startofs $endofs (Eunary(UOnegative, u)) }
| u=unary_expression_not_plus_minus { u }
;

pre_increment_expression:
| PLUS_PLUS u=unary_expression { mkexpr $startofs $endofs (Eunary(UOpreIncrement, u)) }
;

pre_decrement_expression:
| MINUS_MINUS u=unary_expression { mkexpr $startofs $endofs (Eunary(UOpreDecrement, u)) }
;

unary_expression_not_plus_minus:
| p=postfix_expression      { p }
| TILDE  u=unary_expression { mkexpr $startofs $endofs (Eunary(UOcomplement, u)) }
| EXCLAM u=unary_expression { mkexpr $startofs $endofs (Eunary(UOnot, u)) }
| c=cast_expression { c }
;

unary_expression_not_plus_minus_or_lambda_expression:
| u=unary_expression_not_plus_minus { u }
| l=lambda_e                        { l }
;

lambda_e:
| p=lambda_parameters MINUS_GT b=lambda_b { mkexpr $startofs $endofs (Elambda(p, b)) }
;
lambda_b:
| e=unary_expression_not_plus_minus { LBexpr e }
| b=block                           { LBblock b }
;

cast_expression:
| LPAREN p=primitive_type d=ann_dims0 RPAREN u=unary_expression 
    { 
      let ty =
	match d with
	| [] -> p
	| l -> _mkty (Loc.merge p.ty_loc (Xlist.last l).Ast.ad_loc) (Tarray(p, List.length d))
      in
      mkexpr $startofs $endofs (Ecast(ty, u)) 
    }
| LPAREN a=annotations0 n=name RPAREN u=unary_expression_not_plus_minus_or_lambda_expression
    { 
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mkexpr $startofs $endofs (Ecast(name_to_ty a n, u))
    }
| LPAREN a=annotations0 n=name d=ann_dims RPAREN u=unary_expression_not_plus_minus_or_lambda_expression
    { 
      let ty = _mkty (Loc.merge n.n_loc (Xlist.last d).Ast.ad_loc) (Tarray(name_to_ty a n, List.length d)) in
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mkexpr $startofs $endofs (Ecast(ty, u)) 
    }
| LPAREN a=annotations0 n=name ts=type_arguments d=ann_dims0 RPAREN u=unary_expression_not_plus_minus_or_lambda_expression
    { 
      let ty = name_to_ty_args (Loc.merge n.n_loc ts.tas_loc) a n ts in
      let ty' = 
	match d with
	| [] -> ty
	| l -> _mkty (Loc.merge ty.ty_loc (Xlist.last l).Ast.ad_loc) (Tarray(ty, List.length l))
      in
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mkexpr $startofs $endofs (Ecast(ty', u)) 
    }
| LPAREN a=annotations0 n=name ts0=type_arguments DOT c=class_or_interface_type d=ann_dims0 RPAREN
       u=unary_expression_not_plus_minus_or_lambda_expression
    { 
      let tspecs =
	match c.ty_desc with
	| TclassOrInterface ts | Tclass ts | Tinterface ts -> ts
	| _ -> parse_error $startofs $endofs "invalid type"
      in
      let ty = 
	_mkty (Loc.merge n.n_loc c.ty_loc) 
	  (TclassOrInterface((TSapply(a, n, ts0)) :: tspecs))
      in
      let ty' = 
	match d with 
	| [] -> ty
	| l -> _mkty (Loc.merge ty.ty_loc (Xlist.last l).Ast.ad_loc) (Tarray(ty, List.length l))
      in
      set_attribute_PT_T (env#resolve n) n;
      register_qname_as_typename n;
      mkexpr $startofs $endofs (Ecast(ty', u)) 
    }
;

multiplicative_expression:
|                                        u=unary_expression { u }
| m=multiplicative_expression op=mult_op u=unary_expression { mkexpr $startofs $endofs (Ebinary(op, m, u)) }
;

%inline
mult_op:
| STAR    { BOmul }
| SLASH   { BOdiv }
| PERCENT { BOmod }
;

additive_expression:
|                             m=multiplicative_expression { m }
| a=additive_expression PLUS  m=multiplicative_expression { mkexpr $startofs $endofs (Ebinary(BOadd, a, m)) }
| a=additive_expression MINUS m=multiplicative_expression { mkexpr $startofs $endofs (Ebinary(BOsub, a, m)) }
;

shift_expression:
|                                a=additive_expression { a }
| s=shift_expression op=shift_op a=additive_expression { mkexpr $startofs $endofs (Ebinary(op, s, a)) }
;

%inline
shift_op:
| LT_LT    { BOshiftL }
| GT_GT    { BOshiftR }
| GT_GT_GT { BOshiftRU }
;

relational_expression:
|                                   s=shift_expression { s }
| r=relational_expression op=rel_op s=shift_expression { mkexpr $startofs $endofs (Ebinary(op, r, s)) }
;

%inline
rel_op:
| LT    { BOlt }
| GT    { BOgt }
| LT_EQ { BOle }
| GT_EQ { BOge }
;

instanceof_expression:
| r=relational_expression                             { r }
| i=instanceof_expression INSTANCEOF r=reference_type { mkexpr $startofs $endofs (Einstanceof(i, r)) }
| s=ERROR INSTANCEOF r=reference_type
    { 
      let e = mkerrexpr $startofs $endofs(s) s in
      mkexpr $startofs $endofs (Einstanceof(e, r))
    }
;

equality_expression:
|                                 i=instanceof_expression { i }
| e=equality_expression EQ_EQ     i=instanceof_expression { mkexpr $startofs $endofs (Ebinary(BOeq, e, i)) }
| e=equality_expression EXCLAM_EQ i=instanceof_expression { mkexpr $startofs $endofs (Ebinary(BOneq, e, i)) }
;

and_expression:
|                      e=equality_expression { e }
| a=and_expression AND e=equality_expression { mkexpr $startofs $endofs (Ebinary(BObitAnd, a, e)) }
;

exclusive_or_expression:
|                               a=and_expression { a }
| e=exclusive_or_expression HAT a=and_expression { mkexpr $startofs $endofs (Ebinary(BObitXor, e, a)) }
;

inclusive_or_expression:
|                              e=exclusive_or_expression { e }
| i=inclusive_or_expression OR e=exclusive_or_expression { mkexpr $startofs $endofs (Ebinary(BObitOr, i, e)) }
;

conditional_and_expression:
|                                      i=inclusive_or_expression { i }
| c=conditional_and_expression AND_AND i=inclusive_or_expression { mkexpr $startofs $endofs (Ebinary(BOand, c, i)) }
;

conditional_or_expression:
|                                   a=conditional_and_expression { a }
| c=conditional_or_expression OR_OR a=conditional_and_expression { mkexpr $startofs $endofs (Ebinary(BOor, c, a)) }
;

conditional_expression:
| c=conditional_or_expression { c }
| c=conditional_or_expression QUESTION e=expression COLON ce=conditional_expression { mkexpr $startofs $endofs (Econd(c, e, ce)) }
;

assignment_expression:
| c=conditional_expression { c }
| a=assignment             { mkexpr $startofs $endofs (Eassignment a) }
;

assignment: (* set_attribute_A_E name *)
| p=postfix_expression op=assign_op e=expr_or_err { p, mkaop $startofs $endofs op, e }
;

%inline
assign_op:
| EQ          { AOeq }
| STAR_EQ     { AOmulEq }
| SLASH_EQ    { AOdivEq }
| PERCENT_EQ  { AOmodEq }
| PLUS_EQ     { AOaddEq }
| MINUS_EQ    { AOsubEq }
| LT_LT_EQ    { AOshiftLEq }
| GT_GT_EQ    { AOshiftREq }
| GT_GT_GT_EQ { AOshiftRUEq }
| AND_EQ      { AOandEq }
| HAT_EQ      { AOxorEq }
| OR_EQ	      { AOorEq }
;

expression_opt:
| o=ioption(expression) { o }
;

expression:
| a=assignment_expression { a }
| l=lambda_expression     { l }
(*| s=ERROR { mkerrexpr $startofs $endofs s }*)
;

lambda_expression:
| p=lambda_parameters MINUS_GT b=lambda_body
    { 
      mkexpr $startofs $endofs (Elambda(p, b))
    }
;

lambda_parameters:
| i=identifier                                   { let _, id = i in mklp $startofs $endofs (LPident id) }
| LPAREN__LAMBDA l=formal_parameter_list0 RPAREN { mklp $startofs $endofs (LPformal l) }
| LPAREN__LAMBDA l=clist(identifier)      RPAREN { mklp $startofs $endofs (LPinferred l) }
;

lambda_body:
| e=expression { LBexpr e }
| b=block      { LBblock b }
;

%inline
constant_expression:
| e=expression { e }
;

%inline
type_parameters_opt:
| o=ioption(type_parameters) { o }
;

type_parameters:
| LT tps=type_parameter_list_1 { mktyparams $startofs $endofs tps }
;

type_parameter_list:
|                               tp=type_parameter { [tp] }
| tps=type_parameter_list COMMA tp=type_parameter { tps @ [tp] }
;

type_parameter_list_1:
|                               tp=type_parameter_1 { [tp] }
| tps=type_parameter_list COMMA tp=type_parameter_1 { tps @ [tp] }
;

type_variable:
| a=annotations0 i=identifier 
    { 
      begin_scope ~kind:FKtypeparameter ();
      let _, id = i in 
      register_identifier_as_typeparameter id;
      a, id 
    }
;

type_parameter:
| tv=type_variable tb=type_bound_opt { mktyparam $symbolstartofs $endofs tv tb }
;

type_parameter_1:
| tv=type_variable GT              { mktyparam $symbolstartofs $endofs(tv) tv None }
| tv=type_variable tb=type_bound_1 { mktyparam $symbolstartofs (tb.Ast.tb_loc.Loc.end_offset) tv (Some tb) }
;

%inline
type_bound_opt:
| o=ioption(type_bound) { o }
;

type_bound:
| EXTENDS r=reference_type a=additional_bound_list0 { mktb $startofs $endofs r a }
;

type_bound_1:
| EXTENDS r=reference_type_1                           { mktb $startofs r.Ast.ty_loc.Loc.end_offset r [] }
| EXTENDS r=reference_type   a=additional_bound_list_1
    { 
      let edofs = (Xlist.last a).Ast.ab_loc.Loc.end_offset in
      mktb $startofs edofs r a
    }
;

%inline
additional_bound_list0:
| (* *)                   { [] }
| a=additional_bound_list { a }
;

additional_bound_list:
| l=nonempty_list(additional_bound) { l }
;

additional_bound_list_1:
| a=additional_bound_1                           { [a] }
| a=additional_bound   l=additional_bound_list_1 { a :: l }
;

additional_bound:
| AND i=interface_type { mkab $startofs $endofs i }
;

additional_bound_1:
| AND r=reference_type_1 { mkab $startofs r.Ast.ty_loc.Loc.end_offset r }
;

postfix_expression_nn:
| p=primary                   { mkexpr $startofs $endofs (Eprimary p) }
| p=post_increment_expression { p }
| p=post_decrement_expression { p }
;

unary_expression_nn:
| p=pre_increment_expression           { p }
| p=pre_decrement_expression           { p }
| PLUS u=unary_expression              { mkexpr $startofs $endofs (Eunary(UOpositive, u)) }
| MINUS u=unary_expression             { mkexpr $startofs $endofs (Eunary(UOnegative, u)) }
| u=unary_expression_not_plus_minus_nn { u }
;

unary_expression_not_plus_minus_nn:
| p=postfix_expression_nn   { p }
| TILDE u=unary_expression  { mkexpr $startofs $endofs (Eunary(UOcomplement, u)) }
| EXCLAM u=unary_expression { mkexpr $startofs $endofs (Eunary(UOnot, u)) }
| c=cast_expression         { c }
;

multiplicative_expression_nn:
|                                           u=unary_expression_nn { u }
| m=multiplicative_expression_nn op=mult_op u=unary_expression    { mkexpr $startofs $endofs (Ebinary(op, m, u)) }
| n=name                         op=mult_op u=unary_expression    { mkexpr $startofs $endofs (Ebinary(op, _name_to_expr n.n_loc n, u)) }
;

additive_expression_nn:
|                                m=multiplicative_expression_nn { m }
| a=additive_expression_nn PLUS  m=multiplicative_expression    { mkexpr $startofs $endofs (Ebinary(BOadd, a, m)) }
| a=additive_expression_nn MINUS m=multiplicative_expression    { mkexpr $startofs $endofs (Ebinary(BOsub, a, m)) }
| n=name                   PLUS  m=multiplicative_expression    { mkexpr $startofs $endofs (Ebinary(BOadd, _name_to_expr n.n_loc n, m)) }
| n=name                   MINUS m=multiplicative_expression    { mkexpr $startofs $endofs (Ebinary(BOsub, _name_to_expr n.n_loc n, m)) }
;

shift_expression_nn:
|                                   a=additive_expression_nn { a }
| s=shift_expression_nn op=shift_op a=additive_expression    { mkexpr $startofs $endofs (Ebinary(op, s, a)) }
| n=name                op=shift_op a=additive_expression    { mkexpr $startofs $endofs (Ebinary(op, _name_to_expr n.n_loc n, a)) }
;

relational_expression_nn:
|                                      s=shift_expression_nn { s }

| r=shift_expression_nn LT s=shift_expression    { mkexpr $startofs $endofs (Ebinary(BOlt, r, s)) } (* to avoid conflicts *)
| r=shift_expression_nn GT s=shift_expression    { mkexpr $startofs $endofs (Ebinary(BOgt, r, s)) }

| r=relational_expression_nn LT_EQ s=shift_expression    { mkexpr $startofs $endofs (Ebinary(BOle, r, s)) }
| r=relational_expression_nn GT_EQ s=shift_expression    { mkexpr $startofs $endofs (Ebinary(BOge, r, s)) }

| n=name                     op=rel_op s=shift_expression { mkexpr $startofs $endofs (Ebinary(op, _name_to_expr n.n_loc n, s)) }
;

instanceof_expression_nn:
| r=relational_expression_nn { r }
| n=name INSTANCEOF r=reference_type { mkexpr $startofs $endofs (Einstanceof(_name_to_expr n.n_loc n, r)) }
| i=instanceof_expression_nn INSTANCEOF r=reference_type { mkexpr $startofs $endofs (Einstanceof(i, r)) }
;

equality_expression_nn:
|                                    i=instanceof_expression_nn { i }

| e=equality_expression_nn EQ_EQ     i=instanceof_expression { mkexpr $startofs $endofs (Ebinary(BOeq, e, i)) }
| e=equality_expression_nn EXCLAM_EQ i=instanceof_expression { mkexpr $startofs $endofs (Ebinary(BOneq, e, i)) }

| n=name EQ_EQ     i=instanceof_expression { mkexpr $startofs $endofs (Ebinary(BOeq, _name_to_expr n.n_loc n, i)) }
| n=name EXCLAM_EQ i=instanceof_expression { mkexpr $startofs $endofs (Ebinary(BOneq, _name_to_expr n.n_loc n, i)) }
;

and_expression_nn:
|                         e=equality_expression_nn { e }
| a=and_expression_nn AND e=equality_expression    { mkexpr $startofs $endofs (Ebinary(BObitAnd, a, e)) }
| n=name              AND e=equality_expression    { mkexpr $startofs $endofs (Ebinary(BObitAnd, _name_to_expr n.n_loc n, e)) }
;

exclusive_or_expression_nn:
|                                  a=and_expression_nn { a }
| e=exclusive_or_expression_nn HAT a=and_expression    { mkexpr $startofs $endofs (Ebinary(BObitXor, e, a)) }
| n=name                       HAT a=and_expression    { mkexpr $startofs $endofs (Ebinary(BObitXor, _name_to_expr n.n_loc n, a)) }
;

inclusive_or_expression_nn:
|                                 e=exclusive_or_expression_nn { e }
| i=inclusive_or_expression_nn OR e=exclusive_or_expression    { mkexpr $startofs $endofs (Ebinary(BObitOr, i, e)) }
| n=name                       OR e=exclusive_or_expression    { mkexpr $startofs $endofs (Ebinary(BObitOr, _name_to_expr n.n_loc n, e)) }
;

conditional_and_expression_nn:
|                                         i=inclusive_or_expression_nn { i }
| c=conditional_and_expression_nn AND_AND i=inclusive_or_expression    { mkexpr $startofs $endofs (Ebinary(BOand, c, i)) }
| n=name                          AND_AND i=inclusive_or_expression    { mkexpr $startofs $endofs (Ebinary(BOand, _name_to_expr n.n_loc n, i)) }
;

conditional_or_expression_nn:
|                                      c=conditional_and_expression_nn { c }
| o=conditional_or_expression_nn OR_OR c=conditional_and_expression    { mkexpr $startofs $endofs (Ebinary(BOor, o, c)) }
| n=name                         OR_OR c=conditional_and_expression    { mkexpr $startofs $endofs (Ebinary(BOor, _name_to_expr n.n_loc n, c)) }
;

conditional_expression_nn:
|                                                            ce=conditional_or_expression_nn { ce }
| c=conditional_or_expression_nn QUESTION e=expression COLON ce=conditional_expression       { mkexpr $startofs $endofs (Econd(c, e, ce)) }
| n=name                         QUESTION e=expression COLON ce=conditional_expression       { mkexpr $startofs $endofs (Econd(_name_to_expr n.n_loc n, e, ce)) }
;

assignment_expression_nn:
| c=conditional_expression_nn { c }
| a=assignment { mkexpr $startofs $endofs (Eassignment a) }
;

expression_nn:
| a=assignment_expression_nn { a }
| l=lambda_expression        { l }
;


%%
