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
 * A parser for SystemVerilog (IEEE-1800-2009) 
 * based on Verilog-Perl <http://www.veripool.org/systemperl>
 *
 * parser.mly
 *
 *)

%{
module P = Printer
open Printf

module L = Label
module Ls = Labels
module LE = Ls.Expression
module LEE = Ls.EventExpression
module LSE = Ls.SequenceExpression
module LPE = Ls.PropertyExpression
module LS = Ls.Statement
module LCD = Ls.CompilerDirective
module LNT = Ls.NetType
module LPD = Ls.PortDirection
module LDT = Ls.DataType
module LQ = Ls.Qualifier
module LAO = Ls.AssignmentOperator
module LBO = Ls.BinaryOperator
module LUO = Ls.UnaryOperator
module LOO = Ls.OverloadOperator
module LST = Ls.SystemTask
module LTC = Ls.TimingCheck
module LCA = Ls.ConcurrentAssertion
module LSIA = Ls.SimpleImmediateAssertion
module LDIA = Ls.DeferredImmediateAssertion

open Ast

module Aux = Parser_aux.F (Stat)
open Aux
open Stat
%}

%parameter <Stat : Parser_aux.STATE_T>

%token EOF

%token EOP

%token <Ast.node> DESCRIPTION
%token <Ast.node> MODULE_ITEM
%token <Ast.node> GENERATE_ITEM
%token <Ast.node> BLOCK_ITEM_DECLARATION
%token <Ast.node> STMT
%token <Ast.node> CASE_ITEM
%token <Ast.node> CASE_INSIDE_ITEM
%token <Ast.node> CELLPIN_ITEM
%token <Ast.node> PORT
%token <Ast.node> EXPR


%token <string> NB_ASSIGN_POSTFIX


%token EOL

%token <string> IDENTIFIER

%token <string> PP_IDENTIFIER

%token <string> PP_MACRO_NAME
%token <string> PP_MACRO_CONST
%token <string> PP_MACRO_CONST_STR
%token <string> PP_MACRO_CONST_INT
%token <string> PP_MACRO_EXPR

%token <string> PP_MACRO_ID
%token <string * string list> PP_MACRO_APPL

%token PP_CONCAT

%token <string> PACKAGE_IDENTIFIER
%token <string> TYPE_IDENTIFIER
%token <string> CLASS_IDENTIFIER
(*%token <string> INTERFACE_IDENTIFIER*)
%token <string> COVERGROUP_IDENTIFIER
(*%token <string> CLOCKING_IDENTIFIER*)
(*%token <string> PROPERTY_IDENTIFIER*)

%token <string> PATHPULSE_IDENTIFIER

%token <string> SYMBOL_xX
%token <string> SYMBOL_bB
%token <string> SYMBOL_rRfFpPnN

%token <string> INTEGRAL_NUMBER
%token <string> REAL_NUMBER
%token <string> TIME_NUMBER
%token <string> STRING_LITERAL

%token <string> SYSCALL
%token <Labels.gate> GATE
%token <Labels.Strength.t> STRENGTH

%token TC_SETUP TC_HOLD TC_SETUPHOLD TC_RECOVERY TC_REMOVAL TC_RECREM 
%token TC_SKEW TC_TIMESKEW TC_FULLSKEW TC_PERIOD TC_WIDTH TC_NOCHANGE

%token UNDERSCORE


(* Compiler directives *)
%token <string * Macro.body> PP_DEFINE__IDENT__BODY
%token <string> PP_UNDEF__IDENT
%token <string> PP_INCLUDE PP_SYS_INCLUDE
%token PP_LINE PP_ELSE PP_ELSIF PP_ENDIF PP_IFDEF PP_IFNDEF 
%token PP_UNDEF PP_UNDEFINEALL PP_ERROR PP_TIMESCALE PP_RESETALL PP_DEFAULT_NETTYPE
%token PP_PRAGMA PP_BEGIN_KEYWORDS PP_END_KEYWORDS
%token PP_DEFAULT_DECAY_TIME PP_DEFAULT_TRIREG_STRENGTH 
%token PP_DELAY_MODE_DISTRIBUTED PP_DELAY_MODE_PATH PP_DELAY_MODE_UNIT PP_DELAY_MODE_ZERO
%token PP_CELLDEFINE PP_ENDCELLDEFINE PP_UNCONNECTED_DRIVE PP_NOUNCONNECTED_DRIVE
(* %token PP_PROTECTED PP_ENDPROTECTED *)


%token BEGIN_ END_ FORK_ JOIN_ ENDMODULE_ MATCHES_ INSIDE_ GENERATE_


(* Operators *)
%token LBRACE RBRACE 
%token EXCLAM SHARP DOLLAR PERCENT AMP 
%token LPAREN RPAREN
%token STAR PLUS COMMA MINUS DOT SLASH COLON SEMICOLON LT EQ GT QUESTION
%token AT LBRACKET RBRACKET HAT PIPE TILDE
(* Verilog 1995 *)
%token AMP_AMP PIPE_PIPE LT_EQ GT_EQ LT_LT GT_GT EQ_EQ EXCLAM_EQ EQ_EQ_EQ
%token EXCLAM_EQ_EQ HAT_TILDE (* TILDE_HAT *) TILDE_AMP TILDE_PIPE MINUS_GT EQ_GT
%token STAR_GT AMP_AMP_AMP
(* Verilog 2001 *)
%token (* LT_LT_LT *) GT_GT_GT STAR_STAR PLUS_COLON MINUS_COLON DOT_STAR
(* System Verilog 2005 *)
%token TICK TICK_LBRACE EQ_EQ_QUESTION EXCLAM_EQ_QUESTION PLUS_PLUS MINUS_MINUS
%token PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ AMP_EQ PIPE_EQ HAT_EQ 
%token LT_LT_EQ GT_GT_EQ (* LT_LT_LT_EQ *) GT_GT_GT_EQ MINUS_GT_GT SHARP_SHARP AT_AT
%token COLON_COLON COLON_EQ COLON_SLASH PIPE_MINUS_GT PIPE_EQ_GT LBRACKET_STAR
%token LBRACKET_EQ LBRACKET_MINUS_GT LBRACKET_PLUS_RBRACKET
(* System Verilog 2009 *)
%token SHARP_MINUS_SHARP SHARP_EQ_SHARP LT_MINUS_GT

%token LPAREN_STAR STAR_RPAREN

(* Keywords *)
(* Verilog 1995 *)
%token <Labels.AlwaysSpec.t> ALWAYS 
%token AND ASSIGN BEGIN BUF (* BUFIF0 BUFIF1 *) CASE CASEX CASEZ
%token (* CMOS *) DEASSIGN DEFAULT DEFPARAM DISABLE EDGE ELSE END ENDCASE
%token ENDFUNCTION ENDMODULE ENDPRIMITIVE ENDSPECIFY ENDTABLE
%token ENDTASK EVENT FOR FORCE FOREVER FORK FUNCTION (* HIGHZ0 HIGHZ1 *)
%token IF INITIAL INOUT INPUT INTEGER
%token <Labels.JoinSpec.t> JOIN
%token <Labels.ModuleSpec.t> MODULE 
%token (* LARGE MACROMODULE MEDIUM *) NAND NEGEDGE (* NMOS *) NOR NOT (* NOTIF0 NOTIF1 *) OR
%token OUTPUT PARAMETER (* PMOS *) POSEDGE PRIMITIVE (* PULL0 PULL1 *)
%token (* PULLDOWN PULLUP RCMOS RNMOS RPMOS *) REAL REALTIME REG RELEASE 
%token REPEAT (* RTRAN RTRANIF0 RTRANIF1 *) SCALARED (* SMALL *) SPECIFY
%token SPECPARAM (* STRONG0 STRONG1 *) SUPPLY0 SUPPLY1 TABLE TASK TIME
%token (* TRAN TRANIF0 TRANIF1 *) TRI TRI0 TRI1 TRIAND TRIOR TRIREG
%token VECTORED WAIT WAND (* WEAK0 WEAK1 *) WHILE 
%token <Labels.wirespec> WIRE
%token WOR XNOR XOR
(* Verilog 2001 *)
%token AUTOMATIC CELL CONFIG DESIGN ENDCONFIG ENDGENERATE GENERATE
%token GENVAR IFNONE INCDIR INCLUDE INSTANCE LIBLIST LIBRARY
%token LOCALPARAM NOSHOWCANCELLED PULSESTYLE_ONDETECT
%token PULSESTYLE_ONEVENT SHOWCANCELLED SIGNED UNSIGNED USE
(* Verilog 2005 *)
(* %token UWIRE *)
(* System Verilog 2005 *)
(*%token <Ast.system_task> SYSTASK*)
%token ALIAS (* ALWAYS_COMB ALWAYS_FF ALWAYS_LATCH *) ASSERT ASSUME
%token BEFORE BIND BINS BINSOF BIT BREAK BYTE CHANDLE CLASS 
%token CLOCKING CONST CONSTRAINT CONTEXT CONTINUE COVER COVERGROUP
%token COVERPOINT CROSS DIST DO ENDCLASS ENDCLOCKING ENDGROUP
%token ENDINTERFACE ENDPACKAGE ENDPROGRAM ENDPROPERTY ENDSEQUENCE
%token ENUM EXPECT EXPORT EXTENDS EXTERN FINAL FIRST_MATCH FOREACH 
%token FORKJOIN IFF IGNORE_BINS ILLEGAL_BINS IMPORT INSIDE INT
%token INTERFACE INTERSECT (* JOIN_ANY JOIN_NONE *) LOCAL LOGIC LONGINT
%token MATCHES MODPORT NEW NULL PACKAGE PACKED PRIORITY PROGRAM 
%token PROPERTY PROTECTED PURE RAND RANDC RANDCASE RANDSEQUENCE REF 
%token RETURN SEQUENCE SHORTINT SHORTREAL SOLVE STATIC STRING STRUCT
%token ST_ERROR ST_FATAL ST_INFO ST_ROOT ST_UNIT ST_WARNING
%token SUPER TAGGED THIS THROUGHOUT TIMEPRECISION TIMEUNIT TYPE
%token TYPEDEF UNION UNIQUE VAR VIRTUAL VOID WAIT_ORDER WILDCARD
%token WITHx WITHIN
(* System Verilog 2009 *)
%token ACCEPT_ON CHECKER ENDCHECKER EVENTUALLY GLOBAL IMPLIES
%token LET NEXTTIME REJECT_ON RESTRICT S_ALWAYS S_EVENTUALLY
%token S_NEXTTIME S_UNTIL S_UNTIL_WITH STRONG SYNC_ACCEPT_ON
%token SYNC_REJECT_ON UNIQUE0 UNTIL UNTIL_WITH UNTYPED WEAK


%token CONST__R FUNCTION__PV LOCAL__CC NEW__P STATIC__C TASK__PV
%token VIRTUAL__I VIRTUAL__ID VIRTUAL__C WITH__P WITH__B WITH__C
%token LPAREN__IGN LPAREN__S LT_EQ__IGN COMMA__I

%token P_UNARY_ARITH
%token P_REDUCTION
%token P_NEGATION
%token P_TAGGED

%left AMP_AMP_AMP
%left MATCHES MATCHES_
%left P_TAGGED
%left P_SEQ_CLOCKING

%nonassoc ALWAYS S_ALWAYS EVENTUALLY S_EVENTUALLY ACCEPT_ON REJECT_ON SYNC_ACCEPT_ON SYNC_REJECT_ON

%right PIPE_MINUS_GT PIPE_EQ_GT SHARP_MINUS_SHARP SHARP_EQ_SHARP
%right UNTIL S_UNTIL UNTIL_WITH S_UNTIL_WITH IMPLIES
%right IFF
%left OR
%left AND
%nonassoc (* NOT *) NEXTTIME S_NEXTTIME
%left INTERSECT
%left WITHIN
%right THROUGHOUT
%left P_SHARP_SHARP_MULTI
%left SHARP_SHARP
%left LBRACKET_STAR LBRACKET_EQ LBRACKET_MINUS_GT LBRACKET_PLUS_RBRACKET

%left POSEDGE NEGEDGE EDGE
%left LBRACE (* RBRACE *)
%right MINUS_GT LT_MINUS_GT
%right QUESTION COLON
%left PIPE_PIPE
%left AMP_AMP
%left PIPE TILDE_PIPE
%left HAT HAT_TILDE
%left AMP TILDE_AMP
%left EQ_EQ EXCLAM_EQ EQ_EQ_EQ EXCLAM_EQ_EQ EQ_EQ_QUESTION EXCLAM_EQ_QUESTION
%left GT LT GT_EQ LT_EQ LT_EQ__IGN INSIDE DIST
%left LT_LT GT_GT GT_GT_GT
%left PLUS MINUS
%left STAR SLASH PERCENT
%left STAR_STAR
%left P_UNARY_ARITH MINUS_MINUS PLUS_PLUS P_REDUCTION P_NEGATION
%left DOT
%left TICK

%nonassoc P_LOWER_THAN_ELSE
%nonassoc ELSE


%start main
%start library_map
%start partial_description_list
%start partial_module_item_list
%start partial_gen_item_list
%start partial_block_decl_stmt_list 
%start partial_case_item_list
%start partial_case_inside_item_list
%start partial_cellpin_list 
%start partial_list_of_ports
%start partial_pev_expr
%start partial_ev_expr
%start partial_expr

%type <Ast.node> main library_map
%type <Ast.partial> partial_description_list
%type <Ast.partial> partial_module_item_list
%type <Ast.partial> partial_gen_item_list
%type <Ast.partial> partial_block_decl_stmt_list
%type <Ast.partial> partial_case_item_list
%type <Ast.partial> partial_case_inside_item_list
%type <Ast.partial> partial_cellpin_list
%type <Ast.partial> partial_list_of_ports
%type <Ast.partial> partial_pev_expr
%type <Ast.partial> partial_ev_expr
%type <Ast.partial> partial_expr

(* to avoid warnings *)
%start pp_token
%type <unit> pp_token

%%
(********** Rules **********)

%inline
clist(X):
   | l=separated_nonempty_list(COMMA, X) { l }
;

pp_token:
   | PP_UNDEF      { }
   | PP_CONCAT     { }
   | PP_MACRO_ID   { }
   | PP_MACRO_APPL { }
;


state_push:
   | (* empty *) { }
;
state_pop:
   | (* empty *) { }
;


main:
   | EOF           { empty_node }
   | s=source_text { end_scope(); s }

   | error { parse_error $startpos $endpos "syntax error" }
;


library_map:
   | EOF            { empty_node }
   | l=library_text { end_scope(); l }

   | error { parse_error $startpos $endpos "syntax error" }
;

library_text:
   | l=library_description_list EOF { mknode $startpos $endpos(l) L.LibraryText l }
;

library_description_list:
   |                             x=library_description { [x] }
   | xs=library_description_list x=library_description { xs @ [x] }
;

library_description:
   | l=library_declaration { l }
   | i=include_statement   { i }
   | c=config_declaration  { c }
   | SEMICOLON             { empty_node }
;

library_declaration:
   | LIBRARY i=id f=file_path_spec_list SEMICOLON
       { 
         mknode $startpos $endpos (L.LibraryDeclaration i) f
       }
   | LIBRARY i=id f0=file_path_spec_list d=INCDIR f1=file_path_spec_list SEMICOLON 
       { 
         ignore d;
	 mknode $startpos $endpos (L.LibraryDeclaration i) (f0 @ [mknode $startpos(d) $endpos(f1) L.Incdir f1])
       }
;

file_path_spec_list:
   |                              x=file_path_spec { [x] }
   | xs=file_path_spec_list COMMA x=file_path_spec { xs @ [x] }
;

file_path_spec:
   | f=file_path { mkleaf $startpos $endpos (L.FilePathSpec f) }
;

file_path:
   | s=STRING_LITERAL { s } (* ? *)
;

string_literal:
   | s=STRING_LITERAL     { s }
   | s=PP_MACRO_CONST_STR { s }
;

include_statement:
   | INCLUDE f=file_path SEMICOLON { mkleaf $startpos $endpos (L.IncludeStatement f) }
;

config_declaration:
   | CONFIG i=id SEMICOLON d=design_statement c=config_rule_statement_list_opt ENDCONFIG e=end_label_opt 
       { 
	 mknode $startpos $endpos (L.ConfigDeclaration i) (d :: c @ e)
       }
;

design_statement:
   | DESIGN                   SEMICOLON { mkleaf $startpos $endpos L.DesignStatement }
   | DESIGN cl=cell_name_list SEMICOLON { mknode $startpos $endpos L.DesignStatement cl }
;

cell_name_list:
   |                   x=cell_name { [x] }
   | xs=cell_name_list x=cell_name { xs @ [x] }
;

cell_name:
   |            i=id { mkleaf $startpos $endpos (L.CellId i) }
   | i0=id DOT i1=id { mknode $startpos $endpos (L.CellId i1) [mkleaf $startpos $endpos(i1) (L.LibraryIdentifier i0)] }
;

config_rule_statement_list_opt:
   | (* empty *)                  { [] }
   | c=config_rule_statement_list { c }
;

config_rule_statement_list:
   |                               x=config_rule_statement { [x] }
   | xs=config_rule_statement_list x=config_rule_statement { xs @ [x] }
;

config_rule_statement:
   | DEFAULT       l=liblist_clause SEMICOLON { mknode $startpos $endpos L.ConfigRuleStatementDefault [l] }
   | i=inst_clause l=liblist_clause SEMICOLON { mknode $startpos $endpos L.ConfigRuleStatement [i; l] }
   | i=inst_clause u=use_clause     SEMICOLON { mknode $startpos $endpos L.ConfigRuleStatement [i; u] }
   | c=cell_clause l=liblist_clause SEMICOLON { mknode $startpos $endpos L.ConfigRuleStatement [c; l] }
   | c=cell_clause u=use_clause     SEMICOLON { mknode $startpos $endpos L.ConfigRuleStatement [c; u] }
;

liblist_clause:
   | LIBLIST                            { mkleaf $startpos $endpos L.LiblistClause }
   | LIBLIST ll=library_identifier_list { mknode $startpos $endpos L.LiblistClause ll }
;

library_identifier_list:
   |                           i=id { [mkleaf $startpos $endpos (L.LibraryIdentifier i)] }
   | l=library_identifier_list i=id { l @ [mkleaf $startpos $endpos (L.LibraryIdentifier i)] }
;

inst_clause:
   | INSTANCE i=instance_name { mknode $startpos $endpos L.InstClause [i] }
;

instance_name:
   |                     i=id { mknode $startpos $endpos L.InstanceName [mkleaf $startpos $endpos (L.TopModuleIdentifier i)] }
   | n=instance_name DOT i=id { reloc $startpos $endpos n; n#add_children_r [mkleaf $startpos(i) $endpos (L.InstanceIdentifier i)]; n }
;

cell_clause:
   | CELL            i=id { mkleaf $startpos $endpos (L.CellClause i) }
   | CELL i0=id DOT i1=id { mknode $startpos $endpos (L.CellClause i1) [mkleaf $startpos(i0) $endpos(i0) (L.LibraryIdentifier i0)] }
;

use_clause:
   | USE n=cell_name                { mknode $startpos $endpos L.UseClause [n] }
   | USE n=cell_name c=colon_config { mknode $startpos $endpos L.UseClause [n; c] }
;

colon_config:
   | COLON CONFIG { mkleaf $startpos $endpos L.ColonConfig }
;

(* * * * * *)

source_text:
   | d=description_list EOF { mknode $startpos $endpos(d) L.SourceText d }
;

partial_description_list:
   | d=description_list EOP { Ast.Pdescription_list d }
   |                    EOP { Ast.Pdescription_list [] }
;

description_list:
   |                     x=description { context_stack#activate_top; [x] }
   | xs=description_list x=description { context_stack#activate_top; xs @ [x] }
;  

description:
   | m=module_declaration    { m }
   | u=udp_declaration       { u }
   | i=interface_declaration { i }
   | p=program_declaration   { p }
   | p=package_declaration   { p }
   | p=package_item          { p }
   | b=bind_directive        { b }
   | c=config_declaration    { c }

   | a=attribute_instances m=module_declaration    { reloc $startpos $endpos m; m#add_children_l [a]; m }
   | a=attribute_instances i=interface_declaration { reloc $startpos $endpos i; i#add_children_l [a]; i }
   | a=attribute_instances p=program_declaration   { reloc $startpos $endpos p; p#add_children_l [a]; p }
   | a=attribute_instances p=package_declaration   { reloc $startpos $endpos p; p#add_children_l [a]; p }
   | a=attribute_instances p=package_item          { reloc $startpos $endpos p; p#add_children_l [a]; p }
   | a=attribute_instances b=bind_directive        { reloc $startpos $endpos b; b#add_children_l [a]; b }

   | c=compiler_directive_full { c }
   | d=DESCRIPTION             { check_error d; d }
;

udp_declaration:
   | un=udp_nonansi_declaration up=udp_port_declaration_list ub=udp_body ENDPRIMITIVE e=end_label_opt 
       { 
	 reloc $startpos $endpos un; un#add_children_r ([up; ub] @ e);
	 un
       }
   | ua=udp_ansi_declaration                                 ub=udp_body ENDPRIMITIVE e=end_label_opt 
       { 
	 reloc $startpos $endpos ua; ua#add_children_r ([ub] @ e);
	 ua
       }
   | e=extern u=udp_nonansi_declaration { reloc $startpos $endpos u; u#add_children_l [e]; u }
   | e=extern u=udp_ansi_declaration    { reloc $startpos $endpos u; u#add_children_l [e]; u }
;

udp_ports_star:
   | LPAREN   DOT_STAR      RPAREN { mkleaf $startpos $endpos L.UdpPortsStar }
   | LPAREN u=udp_port_list RPAREN { mknode $startpos $endpos L.UdpPorts u }
;

udp_nonansi_declaration:
   | a=attribute_instance_list_opt PRIMITIVE i=id_any u=udp_ports_star SEMICOLON
       { 
         mknode $symbolstartpos $endpos (L.UdpDeclaration i) (a @ [u])
       }
;

udp_ansi_declaration:
   | a=attribute_instance_list_opt PRIMITIVE i=id_any u=udp_declaration_ports SEMICOLON
       { 
         mknode $symbolstartpos $endpos (L.UdpDeclaration i) (a @ [u])
       }
;

attribute_instance_list_opt:
   | (* empty *)           { [] }
   | a=attribute_instances { [a] }
;

attribute_instances:
   | a=attribute_instance_list { mknode $startpos $endpos L.AttributeInstances a }
;

attribute_instance_list:
   |                            x=attribute_instance { [x] }
   | xs=attribute_instance_list x=attribute_instance { xs @ [x] }
;

attribute_instance:
   | LPAREN_STAR a=attr_spec_list STAR_RPAREN { mknode $startpos $endpos L.AttributeInstance a }
;

attr_spec_list:
   |                            x=attr_spec { [x] }
   | xs=attr_spec_list COMMA__I x=attr_spec { xs @ [x] }
;

attr_spec:
   | i=id                 { mkleaf $startpos $endpos (L.AttrSpec i) }
   | i=id EQ c=const_expr { mknode $startpos $endpos (L.AttrSpec i) [c] }
;

udp_port_list:
   | i0=id           COMMA__I i1=id { [mkleaf $startpos $endpos(i0) (L.UdpPort i0); mkleaf $startpos(i1) $endpos(i1) (L.UdpPort i1)] }
   | u=udp_port_list COMMA__I i=id  { u @ [mkleaf $startpos(i) $endpos(i) (L.UdpPort i)] }
;

udp_declaration_ports:
   | LPAREN u=udp_declaration_port_list RPAREN { mknode $startpos $endpos L.UdpDeclarationPorts u }
;

udp_declaration_port_list:
   | uo=udp_output_declaration   COMMA ui=udp_input_declaration { [uo; ui] }
   | u=udp_declaration_port_list COMMA ui=udp_input_declaration { u @ [ui] }
;

udp_port_declaration:
   | u=udp_output_declaration SEMICOLON { reloc $startpos $endpos u; u }
   | u=udp_input_declaration  SEMICOLON { reloc $startpos $endpos u; u }
   | u=udp_reg_declaration    SEMICOLON { reloc $startpos $endpos u; u }
;


udp_port_declaration_list:
   | u=udp_port_declaration_list_ { mknode $startpos $endpos L.UdpPortDecls u }
;

udp_port_declaration_list_:
   |                               u=udp_port_declaration { [u] }
   | ul=udp_port_declaration_list_ u=udp_port_declaration { ul @ [u] }
;

udp_output_declaration:
   | OUTPUT     i=id                 { mkleaf $startpos $endpos (L.UdpOutputDeclaration i) }
   | OUTPUT REG i=id                 { mkleaf $startpos $endpos (L.UdpOutputDeclarationReg i) }
   | OUTPUT REG i=id EQ c=const_expr { mknode $startpos $endpos (L.UdpOutputDeclarationReg i) [c] }
;

udp_input_declaration:
   | INPUT p=port_id_list { mknode $startpos $endpos L.UdpInputDeclaration p }
;

port_id_list:
   |                         i=id { [mkleaf $startpos $endpos (L.UdpPort i)] }
   | p=port_id_list COMMA__I i=id { p @ [mkleaf $startpos(i) $endpos(i) (L.UdpPort i)] }
;

udp_reg_declaration:
   | REG i=id { mkleaf $startpos $endpos (L.UdpRegDeclaration i) }
;

udp_body:
   | c=combinational_body { c }
   | s=sequential_body    { s }
;

sequential_body:
   |                    TABLE s=sequential_entry_list ENDTABLE { mknode $startpos $endpos L.SequentialBody s }
   | u=udp_initial_stmt TABLE s=sequential_entry_list ENDTABLE { mknode $startpos $endpos L.SequentialBody (u::s) }
;

udp_initial_stmt:
   | INITIAL i=id EQ n=INTEGRAL_NUMBER SEMICOLON { mkleaf $startpos $endpos (L.UdpInitialStmt(i, n)) }
;

sequential_entry_list:
   |                          s=sequential_entry { [s] }
   | sl=sequential_entry_list s=sequential_entry { sl @ [s] }
;

sequential_entry:
   | level_input_list COLON current_state COLON next_state SEMICOLON { mknode $startpos $endpos L.SequentialEntry ($1 @ [$3; $5]) }
   | edge_input_list  COLON current_state COLON next_state SEMICOLON { mknode $symbolstartpos $endpos L.SequentialEntry ($1 @ [$3; $5]) }
;

%inline
level_input_list_opt:
   | (* empty *)        { [] }
   | l=level_input_list { l }
;

edge_input_list:
   | l0=level_input_list_opt e=edge_indicator l1=level_input_list_opt { l0 @ [e] @ l1 }
;

edge_indicator:
   | LPAREN level_symbol level_symbol RPAREN { mknode $startpos $endpos L.EdgeIndicator [$2; $3] }
   | edge_symbol                             { mknode $startpos $endpos L.EdgeIndicator [$1] }
;

edge_symbol: (* r R f F p P n N * *)
   | STAR              { mkleaf $startpos $endpos (L.EdgeSymbol "*") }
   | s=SYMBOL_rRfFpPnN { mkleaf $startpos $endpos (L.EdgeSymbol s) }
;

current_state: 
   | l=level_symbol { l }
;

next_state:
   | o=output_symbol { o }
   | MINUS           { mkleaf $startpos $endpos L.NextStateMinus }
;

integral_number:
   |                      i=INTEGRAL_NUMBER { i }
   | m=PP_IDENTIFIER      i=INTEGRAL_NUMBER { m^i }
   | m=PP_MACRO_CONST_INT                   { m }
   | m=PP_MACRO_CONST_INT i=INTEGRAL_NUMBER { m^i }
;


compiler_directive_full:
   | compiler_directive { $1 }
   | PP_BEGIN_KEYWORDS                { mknode $startpos $endpos (L.compiler_directive (LCD.Begin_keywords "")) [] }
   | PP_BEGIN_KEYWORDS string_literal { mknode $startpos $endpos (L.compiler_directive (LCD.Begin_keywords $2)) [] }
   | PP_END_KEYWORDS                  { mknode $startpos $endpos (L.compiler_directive LCD.End_keywords) [] }
;

compiler_directive:
   | p=PP_DEFINE__IDENT__BODY { let id, _ = p in mknode $startpos $endpos (L.compiler_directive (LCD.Define id)) [] }
   | i=PP_UNDEF__IDENT        { mknode $startpos $endpos (L.compiler_directive (LCD.Undef i)) [] }
   | PP_UNDEFINEALL           { mknode $startpos $endpos (L.compiler_directive LCD.Undefineall) []}

   | PP_INCLUDE               { mknode $startpos $endpos (L.compiler_directive (LCD.Include $1)) [] }
   | PP_SYS_INCLUDE           { mknode $startpos $endpos (L.compiler_directive (LCD.SysInclude $1)) [] }

   | PP_TIMESCALE TIME_NUMBER SLASH TIME_NUMBER { mknode $startpos $endpos (L.compiler_directive (LCD.Timescale($2, $4))) []}

   | PP_DEFAULT_DECAY_TIME INTEGRAL_NUMBER { mknode $startpos $endpos (L.compiler_directive (LCD.Default_decay_time $2)) [] }
   | PP_DEFAULT_DECAY_TIME REAL_NUMBER     { mknode $startpos $endpos (L.compiler_directive (LCD.Default_decay_time $2)) [] }
   | PP_DEFAULT_DECAY_TIME id              { mknode $startpos $endpos (L.compiler_directive (LCD.Default_decay_time $2)) [] }

   | PP_DEFAULT_TRIREG_STRENGTH INTEGRAL_NUMBER { mknode $startpos $endpos (L.compiler_directive (LCD.Default_trireg_strength $2)) [] }

   | PP_DELAY_MODE_DISTRIBUTED { mknode $startpos $endpos (L.compiler_directive LCD.Delay_mode_distributed) [] }
   | PP_DELAY_MODE_PATH        { mknode $startpos $endpos (L.compiler_directive LCD.Delay_mode_path) [] }
   | PP_DELAY_MODE_UNIT        { mknode $startpos $endpos (L.compiler_directive LCD.Delay_mode_unit) [] }
   | PP_DELAY_MODE_ZERO        { mknode $startpos $endpos (L.compiler_directive LCD.Delay_mode_zero) [] }


   | PP_ERROR string_literal { mknode $startpos $endpos (L.compiler_directive (LCD.Error $2)) [] }

   | PP_LINE integral_number string_literal INTEGRAL_NUMBER { mknode $startpos $endpos (L.compiler_directive (LCD.Line($2, $3, $4))) [] }

   | PP_RESETALL             { mknode $startpos $endpos (L.compiler_directive LCD.Resetall) [] }

   | PP_DEFAULT_NETTYPE id (* none *) { mknode $startpos $endpos (L.compiler_directive LCD.Default_nettypeNone) [] }
   | PP_DEFAULT_NETTYPE net_type      { mknode $startpos $endpos (L.compiler_directive LCD.Default_nettype) [$2] }

   | PP_CELLDEFINE    { mkleaf $startpos $endpos (L.compiler_directive LCD.Celldefine) }
   | PP_ENDCELLDEFINE { mkleaf $startpos $endpos (L.compiler_directive LCD.Endcelldefine) }

   | PP_UNCONNECTED_DRIVE   { mkleaf $startpos $endpos (L.compiler_directive LCD.Unconnected_drive) }
   | PP_NOUNCONNECTED_DRIVE { mkleaf $startpos $endpos (L.compiler_directive LCD.Nounconnected_drive) }

   | PP_PRAGMA i=id                          EOL { mknode $startpos $endpos(i) (L.compiler_directive (LCD.Pragma i)) [] }
   | PP_PRAGMA i=id p=pragma_expression_list EOL { mknode $startpos $endpos(p) (L.compiler_directive (LCD.Pragma i)) p }

   | pp_branch { dummy_node }
;

(*
%inline
pragma_expression_list_opt:
   | (* empty *)              { [] }
   | p=pragma_expression_list { p }
;
*)
pragma_expression_list:
   |                              pragma_expression { [$1] }
   | pragma_expression_list COMMA pragma_expression { $1 @ [$3] }
;

pragma_expression:
   | id                 { mkleaf $startpos $endpos (L.PragmaExpression $1) }
   | id EQ pragma_value { mknode $startpos $endpos (L.PragmaExpression $1) [$3] }
   | id EQ id           { mknode $startpos $endpos (L.PragmaExpression $1) [mkleaf $startpos($3) $endpos($3) (L.PragmaValueId $3)] }
   |       pragma_value { mknode $startpos $endpos (L.PragmaExpression "") [$1] }
;

pragma_value:
   | LPAREN l=pragma_expression_list RPAREN { mknode $startpos $endpos L.PragmaValueTuple l }
   | i=INTEGRAL_NUMBER                      { mkleaf $startpos $endpos (L.PragmaValueNum i) }
   | r=REAL_NUMBER                          { mkleaf $startpos $endpos (L.PragmaValueNum r) }
   | s=string_literal                       { mkleaf $startpos $endpos (L.PragmaValueStr s) }
;

pp_branch_:
   |              PP_IFDEF  i=id c=pp_content_opt { ignore (i, c) }
   |              PP_IFNDEF i=id c=pp_content_opt { ignore (i, c) }
   | b=pp_branch_ PP_ELSE        c=pp_content_opt { ignore (b, c) }
   | b=pp_branch_ PP_ELSIF  i=id c=pp_content_opt { ignore (b, i, c) }
;

pp_branch:
   | pp_branch_ PP_ENDIF { }
;

%inline
pp_content_opt:
   | (* empty *)   { }
   | p=pp_content  { ignore p }

pp_content:
   |            pp_fragment { }
   | pp_content pp_fragment { }
;

pp_fragment:
   | compiler_directive { }
;


timeunits_declaration:
   | TIMEUNIT      TIME_NUMBER                   SEMICOLON 
       { 
	 mknode $startpos $endpos L.TimeUnitsDeclaration [mkleaf $startpos($2) $endpos($2) (L.TimeUnit $2)] 
       }
   | TIMEUNIT      TIME_NUMBER SLASH TIME_NUMBER SEMICOLON 
       { 
	 let u = mkleaf $startpos($2) $endpos($2) (L.TimeUnit $2) in
	 let p = mkleaf $startpos($4) $endpos($4) (L.Timeprecision $4) in
	 mknode $startpos $endpos L.TimeUnitsDeclaration [u; p]
       }
   | TIMEPRECISION TIME_NUMBER                   SEMICOLON 
       { 
	 mknode $startpos $endpos L.TimeUnitsDeclaration [mkleaf $startpos($2) $endpos($2) (L.Timeprecision $2)] 
       }
;

package_declaration:
   | pd=package_declaration_head pi=package_item_list_opt ENDPACKAGE e=end_label_opt 
       { 
	 end_scope();
	 let l, id = pd in
	 mknode $startpos $endpos (L.PackageDeclaration id) (pi @ e)
       }
;

package_declaration_head:
   | PACKAGE l=lifetime_opt i=id_any SEMICOLON { register_package i; begin_package_scope i; l, i }
;

package_item_list_opt:
   | (* empty *)       { [] }
   | package_item_list { $1 }
;

package_item_list:
   |                   package_item { [$1] }
   | package_item_list package_item { $1 @ [$2] }
;

package_item:
   | package_or_generate_item_declaration { $1 }
   | anonymous_program                    { $1 }
   | package_export_declaration           { $1 }
   | timeunits_declaration                { $1 }
;

package_or_generate_item_declaration:
   | net_declaration                       { $1 }
   | data_declaration                      { $1 }
   | task_declaration                      { $1 }
   | function_declaration                  { $1 }
   | checker_declaration                   { $1 }
   | dpi_import_export                     { $1 }
   | extern_constraint_declaration         { $1 }
   | class_declaration                     { $1 }
   | local_parameter_declaration SEMICOLON { reloc $startpos $endpos $1; $1 }
   | parameter_declaration       SEMICOLON { reloc $startpos $endpos $1; $1 }
   | covergroup_declaration                { $1 }
   | overload_declaration                  { $1 }
   | assertion_item_declaration            { $1 }
   | SEMICOLON                             { mkleaf $startpos $endpos L.PackageOrGenerateItemEmpty }

   | initial_construct                     { $1 } (* ex. "OpenSPARCT2.1.3/design/fpga/rtl/dump.v" *)
   | a=always_construct                    { a }
   | continuous_assign                     { $1 } (* ex. "OpenSPARCT2.1.3/verif/env/common/verilog/ldst_sync/ldst_dma.v" *)
   | etc_inst                              { $1 } (* ex. "OpenSPARCT2.1.3/verif/env/fc/fc_pcie_stuff.v" *)
   | parameter_override                    { $1 } (* ex. "OpenSPARCT2.1.3/verif/env/fc/fc_pcie_stuff.v" *)
;

always_construct:
(*   | ALWAYS stmt_block         { mknode $startpos $endpos (L.Always_construct $1) [$2] } (* ex. "OpenSPARCT2.1.3/verif/env/common/verilog/misc/errorCountTasks.v"*)*)
   | a=ALWAYS s=stmt_non_block { mknode $startpos $endpos (L.AlwaysConstruct a) [s] } (* ex. "OpenSPARCT2.1.3/verif/env/common/verilog/misc/errorCountTasks.v"*)
   | a=ALWAYS s=stmt_block_act { mknode $startpos $endpos (L.AlwaysConstruct a) [s] } (* ex. "OpenSPARCT2.1.3/verif/env/common/verilog/misc/errorCountTasks.v"*)
;

package_import_declaration_list:
   | package_import_declaration_list_ { [mknode $startpos $endpos L.PackageImportDecls $1] }
;

package_import_declaration_list_:
   |                                        package_import_declaration { [$1] }
   | package_import_declaration_list_ COMMA package_import_declaration { $1 @ [$3] }
;

package_import_declaration:
   | package_import_declaration_head SEMICOLON { reloc $startpos $endpos $1; $1 }

package_import_declaration_head:
   | IMPORT package_import_item_list
       { 
	 List.iter
	   (fun pkg_imp_item ->
	     match pkg_imp_item#label with
	     | L.PackageImportItem pkg -> begin
		 match pkg_imp_item#children with
		 | [imp_item_obj] -> begin
		     match imp_item_obj#label with
		     | L.PackageImport id -> import_one pkg id
		     | L.PackageImportAny -> import_any pkg
		     | _ -> assert false
		 end
		 | _ -> assert false
	     end
	     | _ -> assert false
	   ) $2;
	 mknode $startpos $endpos L.PackageImportDeclaration $2 
       }
;

package_import_item_list:
   |                                package_import_item { [$1] }
   | package_import_item_list COMMA package_import_item { $1 @ [$3] }
;

package_import_item:
   | PACKAGE_IDENTIFIER COLON_COLON package_import_item_obj { mknode $startpos $endpos (L.PackageImportItem $1) [$3] }
;

package_import_item_obj:
   | i=id_any { mkleaf $startpos $endpos (L.PackageImport i) }
   | STAR     { mkleaf $startpos $endpos L.PackageImportAny }
;

package_export_declaration:
   | EXPORT STAR COLON_COLON STAR    SEMICOLON { mkleaf $startpos $endpos L.PackageExportDeclarationStar }
   | EXPORT package_import_item_list SEMICOLON { mknode $startpos $endpos L.PackageExportDeclaration $2 }
;

extern:
   | EXTERN { mkleaf $startpos $endpos L.Extern }
;

module_declaration:
   |           m=module_declaration_ ENDMODULE_ e=end_label_opt { reloc $startpos $endpos m; m#add_children_r e; m }
   | ex=extern m=module_declaration_head1 p=ports_star_opt SEMICOLON                                              
       { 
	 end_scope();
	 let (ms, lt, id), ips = m in
	 mknode $startpos $endpos (L.ModuleDeclaration(ms, id)) (ex::(lt @ ips @ p))
       }
;

module_declaration_:
   | module_declaration_head2 SEMICOLON module_item_list_opt ENDMODULE
       { 
	 end_scope(); context_stack#pop_and_activate;
	 let ms, lt, id, params, ports = $1 in
	 mknode $startpos $endpos (L.ModuleDeclaration(ms, id)) (lt @ params @ ports @ $3)
       }
;

module_declaration_head2:
   | m=module_declaration_head1 p=ports_star_opt 
       { 
	 context_stack#push (Context.module_item_list());
	 let (ms, lt, id), ips = m in
	 ms, lt, id, ips, p
       }
;

module_declaration_head1:
   | m=module_declaration_head i=imports_parameters_opt { context_stack#push (Context.list_of_ports()); m, i }
;

module_declaration_head:
   | m=MODULE l=lifetime_opt i=id_any { begin_scope(); m, l, i }
;


%inline
imports_parameters_opt:
   |                                    pp=parameter_port_list_opt { pp }
   | pi=package_import_declaration_list pp=parameter_port_list_opt { pi @ pp }
;



%inline
parameter_value_assignment_opt:
   | (* empty *)                         { [] }
   | parameter_value_assignment_opt_cellpin_list_head c=cellpin_list RPAREN 
       { 
	 context_stack#pop;
	 [mknode $startpos $endpos L.ParameterValueAssignment c] 
       }
   | SHARP dv=delay_value                { [mknode $startpos $endpos L.ParameterValueAssignment [dv]] }
;

parameter_value_assignment_opt_cellpin_list_head:
   | parameter_value_assignment_opt_cellpin_list_head0 LPAREN { }
;

%inline
parameter_value_assignment_opt_cellpin_list_head0:
   | SHARP { context_stack#push (Context.cellpin_list()) }
;

%inline
parameter_port_list_opt:
   | (* empty *)                                       { [] }
   | SHARP LPAREN                               RPAREN { [mknode $startpos $endpos L.ParamPorts []] }
   | SHARP LPAREN p=param_port_decl_or_arg_list RPAREN { [mknode $startpos $endpos L.ParamPorts p] }
;

param_port_decl_or_arg_list:
   |                                   param_port_decl_or_arg { [$1] }
   | param_port_decl_or_arg_list COMMA param_port_decl_or_arg { $1 @ [$3] }
;

param_port_decl_or_arg:
   |                                 param_assignment { $1 }
   | parameter_port_declaration_head param_assignment { mknode $startpos $endpos L.ParameterPortDeclaration ($1 @ [$2]) }
;

%inline
ports_star_opt:
   | (* empty *)                       { context_stack#pop; [] }
   | LPAREN DOT_STAR            RPAREN { context_stack#pop; [mkleaf $startpos $endpos L.PortsStar] }
   | LPAREN l=list_of_ports_opt RPAREN { context_stack#pop; [mknode $startpos $endpos L.Ports l] }
;

partial_list_of_ports:
   | list_of_ports_opt EOP { Ast.Plist_of_ports $1 }
;

list_of_ports_opt:
   |                           p=port_opt { context_stack#activate_top; p }
   | l=list_of_ports_opt COMMA p=port_opt { context_stack#activate_top; l @ p }
;

%inline
port_opt:
   | (* empty *) { [] }
   | p=port      { [p] }
;

interface_port_head:
   | i=id                   { mkleaf $startpos $endpos (L.InterfacePort i) }
   | i=id      DOT m=id_any { mknode $startpos $endpos (L.InterfacePort i) [mkleaf $startpos(m) $endpos(m) (L.ModportIdentifier m)] }
   | INTERFACE              { mkleaf $startpos $endpos L.InterfacePortInterface }
   | INTERFACE DOT m=id_any { mknode $startpos $endpos L.InterfacePortInterface [mkleaf $startpos(m) $endpos(m) (L.ModportIdentifier m)] }
;

port: (* ansi_port_declaration + port *)
   | p=port_dir_net_opt i=interface_port_head    ia=id_any r=range_list_opt s=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ia) (p @ [i] @ r @ s) }

   | p=port_dir_net_opt v=var_data_type          DOT ps=port_sig LPAREN pa=port_assign_expr_opt RPAREN s=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ [v] @ pa @ s) }

   | p=port_dir_net_opt s=signing_opt r=range_list DOT ps=port_sig LPAREN pa=port_assign_expr_opt RPAREN sa=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ s @ r @ pa @ sa) }

   | p=port_dir_net_opt                        DOT ps=port_sig LPAREN pa=port_assign_expr_opt RPAREN s=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ pa @ s) }

   | p=port_dir_net_opt v=var_data_type          ps=port_sig vd=variable_dimension_list_opt s=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ [v] @ vd @ s) }

   | p=port_dir_net_opt s=signing_opt r=range_list ps=port_sig v=variable_dimension_list_opt sa=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ s @ r @ v @ sa) }

   | p=port_dir_net_opt                        ps=port_sig v=variable_dimension_list_opt s=sig_attr_list_opt 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ v @ s) }

   | p=port_dir_net_opt v=var_data_type          ps=port_sig vd=variable_dimension_list_opt s=sig_attr_list_opt EQ c=const_expr 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ [v] @ vd @ s @ [c]) }

   | p=port_dir_net_opt s=signing_opt r=range_list ps=port_sig v=variable_dimension_list_opt sa=sig_attr_list_opt EQ c=const_expr 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ s @ r @ v @ sa @ [c]) }

   | p=port_dir_net_opt                        ps=port_sig v=variable_dimension_list_opt s=sig_attr_list_opt EQ c=const_expr 
       { mknode $symbolstartpos $endpos (L.Port ps) (p @ v @ s @ [c]) }

   | LBRACE list_of_ports_opt RBRACE { mknode $startpos $endpos L.PortMulti $2 }
   | PORT { check_error $1; $1 }

   | e=error port_tail { ignore e; parse_error $startpos(e) $endpos(e) "syntax error" }
;

%inline
port_tail:
(*   | id_any   { }*)
   | RPAREN   { }
   | RBRACKET { }
(*
   | RBRACE   { }
*)
;

port_dir_net_opt:
   | (* empty *)             { [] }
   | port_direction          { [$1] }
   | port_direction net_type { [$1; $2] }
   | net_type                { [$1] }
;

port_decl_net_opt:
   | (* empty *) { [] }
   | net_type    { [$1] }
;

port_assign_expr_opt:
   | (* empty *) { [] }
   | expr        { [$1] }
;

port_sig:
   | id            { $1 }
   | id_SV_keyword { $1 }
;


interface_declaration:
   |        i=interface_declaration_head2 p=ports_star_opt SEMICOLON ii=interface_item_list_opt ENDINTERFACE e=end_label_opt 
       { 
	 end_scope();
	 let (l, id), ips = i in
	 mknode $startpos $endpos (L.InterfaceDeclaration id) (l @ ips @ p @ ii @ e)
       }
   | EXTERN i=interface_declaration_head2 p=ports_star_opt SEMICOLON                                                    
       { 
	 end_scope();
	 let (l, id), ips = i in
	 mknode $startpos $endpos (L.InterfaceDeclarationExtern id) (l @ ips @ p)
       }
;

interface_declaration_head2:
   | i=interface_declaration_head ip=imports_parameters_opt { context_stack#push (Context.list_of_ports()); i, ip }
;

interface_declaration_head:
   | INTERFACE l=lifetime_opt i=id_any { register_interface i; begin_scope(); l, i }
;

interface_item_list_opt:
   | (* empty *)         { [] }
   | interface_item_list { $1 }
;

interface_item_list:
   |                     interface_item { [$1] }
   | interface_item_list interface_item { $1 @ [$2] }
;

interface_item:
   | port_declaration SEMICOLON { reloc $startpos $endpos $1; $1 }
   | generate_region            { $1 }
   | interface_or_generate_item { $1 }
   | program_declaration        { $1 }
   | interface_declaration      { $1 }
   | timeunits_declaration      { $1 }
   | module_common_item         { $1 }
;

interface_or_generate_item:
   | modport_declaration   { $1 }
   | extern_tf_declaration { $1 }
;


anonymous_program:
   | PROGRAM SEMICOLON anonymous_program_item_list_opt ENDPROGRAM { mknode $startpos $endpos L.AnonymousProgram $3 }
;

anonymous_program_item_list_opt:
   | (* empty *)                 { [] }
   | anonymous_program_item_list { $1 }
;

anonymous_program_item_list:
   |                             anonymous_program_item { [$1] }
   | anonymous_program_item_list anonymous_program_item { $1 @ [$2] }
;

anonymous_program_item:
   | task_declaration       { $1 }
   | function_declaration   { $1 }
   | class_declaration      { $1 }
   | covergroup_declaration { $1 }
   | SEMICOLON              { mkleaf $startpos $endpos L.AnonymousProgramItemEmpty }
;


program_declaration:
   |           pd=program_declaration_head2 ps=ports_star_opt SEMICOLON pi=program_item_list_opt ENDPROGRAM e=end_label_opt 
       { 
	 end_scope();
	 let (l, id), ips = pd in
	 mknode $startpos $endpos (L.ProgramDeclaration id) (l @ ips @ ps @ pi @ e)
       }
   | ex=extern pd=program_declaration_head2 ps=ports_star_opt SEMICOLON                                                
       { 
	 end_scope();
	 let (l, id), ips = pd in
	 mknode $startpos $endpos (L.ProgramDeclaration id) (ex :: l @ ips @ ps)
       }
;

program_declaration_head2:
   | p=program_declaration_head i=imports_parameters_opt { context_stack#push (Context.list_of_ports()); p, i }
;

program_declaration_head:
   | PROGRAM l=lifetime_opt i=id_any { begin_scope(); l, i }
;

program_item_list_opt:
   | (* empty *)       { [] }
   | program_item_list { $1 }
;

program_item_list:
   |                   program_item { [$1] }
   | program_item_list program_item { $1 @ [$2] }
;

program_item:
   | port_declaration SEMICOLON { reloc $startpos $endpos $1; $1 }
   | non_port_program_item      { $1 }
;

non_port_program_item:
(*   | continuous_assign                   { $1 } --> package_or_generate_item_declaration*)
   | module_or_generate_item_declaration { $1 }
(*   | initial_construct                   { $1 } --> package_or_generate_item_declaration*)
   | final_construct                     { $1 }
   | concurrent_assertion_item           { $1 }
   | timeunits_declaration               { $1 }
   | program_generate_item               { $1 }
;

program_generate_item:
   | loop_generate_construct        { $1 }
   | conditional_generate_construct { $1 }
   | generate_region                { $1 }
   | elaboration_system_task        { $1 }
;

extern_tf_declaration:
   | EXTERN            t=task_prototype     SEMICOLON { mknode $startpos $endpos (L.ExternTfDeclaration t#get_identifier) [t] }
   | EXTERN            f=function_prototype SEMICOLON { mknode $startpos $endpos (L.ExternTfDeclaration f#get_identifier) [f] }
   | EXTERN f=FORKJOIN t=task_prototype     SEMICOLON
       { 
         ignore f;
	 let fj = mkleaf $startpos(f) $endpos(f) L.Forkjoin in
	 mknode $startpos $endpos (L.ExternTfDeclaration t#get_identifier) [fj; t] 
       }
;

modport_declaration:
   | MODPORT ml=modport_item_list SEMICOLON 
       { 
	 let ids = List.map (fun m -> m#get_identifier) ml in
	 mknode $startpos $endpos (L.ModportDeclaration ids) ml
       }
;

modport_item_list:
   |                         modport_item { [$1] }
   | modport_item_list COMMA modport_item { $1 @ [$3] }
;

modport_item:
   | modport_id_head LPAREN modport_ports_decl_list RPAREN { mknode $startpos $endpos (L.ModportItem $1) $3 }
 ;

modport_id_head:
   | id { $1 }
;

modport_ports_decl_list:
   |                               modport_ports_decl { [$1] }
   | modport_ports_decl_list COMMA modport_ports_decl { $1 @ [$3] }
;

modport_ports_decl:
   | port_direction modport_simple_port { mknode $startpos $endpos L.ModportSimplePortsDecl [$1; $2] }
   | CLOCKING id_any                    { mkleaf $startpos $endpos (L.ModportClockingDecl $2) }
   | IMPORT modport_tf_port             { mknode $startpos $endpos L.ModportTfPortsDeclImport [$2] }
   | EXPORT modport_tf_port             { mknode $startpos $endpos L.ModportTfPortsDeclExport [$2] }
   | modport_simple_port                { $1 }
;

modport_simple_port:
   | id                            { mkleaf $startpos $endpos (L.ModportSimplePort $1) }
   | DOT id_any LPAREN RPAREN      { mkleaf $startpos $endpos (L.ModportSimplePortDot $2) }
   | DOT id_any LPAREN expr RPAREN { mknode $startpos $endpos (L.ModportSimplePortDot $2) [$4] }
;

modport_tf_port:
   | id               { mkleaf $startpos $endpos (L.ModportTfPort $1) }
   | method_prototype { $1 }
;

genvar_declaration:
   | GENVAR gl=genvar_identifier_list SEMICOLON 
       { 
	 let ids = List.map (fun g -> g#get_identifier) gl in
	 mknode $startpos $endpos (L.GenvarDeclaration ids) gl
       }
;

genvar_identifier_list:
   |                              genvar_identifier_decl { [$1] }
   | genvar_identifier_list COMMA genvar_identifier_decl { $1 @ [$3] }
;

genvar_identifier_decl:
   | i=id s=sig_attr_list_opt { mknode $startpos $endpos (L.GenvarIdDecl i) s }
;

local_parameter_declaration:
   | l=local_parameter_declaration_head p=param_assignments
       { 
	 let ids = p#get_identifiers in
	 mknode $startpos $endpos (L.LocalParameterDeclaration ids) (l @ [p]) 
       }
;

parameter_declaration:
   | h=parameter_declaration_head p=param_assignments 
       { 
	 let ids = p#get_identifiers in
	 mknode $startpos $endpos (L.ParameterDeclaration ids) (h @ [p])
       }
;

local_parameter_declaration_head:
   | var_local_param_reset i=implicit_data_type { reset_vartype(); i }
   | var_local_param_reset data_type            { reset_vartype(); [$2] }
   | var_local_param_reset TYPE                 { set_vartype_type(); [] }
;

parameter_declaration_head:
   | var_param_reset i=implicit_data_type { reset_vartype(); i }
   | var_param_reset data_type            { reset_vartype(); [$2] }
   | var_param_reset TYPE                 { set_vartype_type(); [] }
;

parameter_port_declaration_head:
   | parameter_declaration_head       { $1 }
   | local_parameter_declaration_head { $1 }
   | data_type                        { reset_vartype(); [$1] }
   | TYPE                             { set_vartype_type(); [] }
;

net_declaration:
   | h=net_declaration_head n=net_decl_assignments SEMICOLON 
       { 
	 let stp, nt, ss, ns, ndt = h in
	 let ids = n#get_identifiers in
	 mknode stp $endpos (L.NetDeclaration ids) ([nt] @ ss @ ns @ ndt @ [n])
       }
;

net_declaration_head:
   | net_decl_reset net_type strength_spec_opt net_scalared_opt net_data_type { $symbolstartpos, $2, $3, $4, $5 }
;

net_decl_reset:
   | (* empty *) { }
;

net_scalared_opt:
   | (* empty *) { [] }
   | SCALARED    { [mkleaf $startpos $endpos L.Scalared] }
   | VECTORED    { [mkleaf $startpos $endpos L.Vectored] }
;

net_data_type:
   | v=var_data_type                        { [v] }
   | s=signing_opt r=range_list d=delay_opt { s @ r @ d }
   | s=signing                  d=delay_opt { s::d }
   |                            d=delay_opt { d }
;

net_type:
   | net_type_ { mkleaf $startpos $endpos (L.net_type $1) }
;

net_type_:
   | SUPPLY0 { LNT.Supply0 }
   | SUPPLY1 { LNT.Supply1 }
   | TRI     { LNT.Tri }
   | TRI0    { LNT.Tri0 }
   | TRI1    { LNT.Tri1 }
   | TRIAND  { LNT.Triand }
   | TRIOR   { LNT.Trior }
   | TRIREG  { LNT.Trireg }
   | WAND    { LNT.Wand }
   | w=WIRE
       { 
         match w with
         | Ls.WS_NORMAL     -> Ls.NetType.Wire 
         | Ls.WS_UNRESOLVED -> Ls.NetType.Uwire
       }
   | WOR     { LNT.Wor }
;

var_param_reset:
   | PARAMETER { }
;

var_local_param_reset:
   | LOCALPARAM { }
 ;

port_direction:
   | port_direction_ { mkleaf $startpos $endpos (L.PortDirection $1) }
;

port_direction_:
   | INPUT        { LPD.Input }
   | OUTPUT       { LPD.Output }
   | INOUT        { LPD.Inout }
   | REF          { LPD.Ref }
   | CONST__R REF { LPD.ConstRef }
;

port_direction_reset:
   | port_direction_reset_ { mkleaf $startpos $endpos (L.PortDirection $1) }
;

port_direction_reset_:
   | INPUT        { LPD.Input }
   | OUTPUT       { LPD.Output }
   | INOUT        { LPD.Inout }
   | REF          { LPD.Ref }
   | CONST__R REF { LPD.ConstRef }
;

port_declaration:
   | port_declaration_ { mknode $startpos $endpos L.PortDeclaration $1 }
;

port_declaration_:
   | pr=port_direction_reset pn=port_decl_net_opt vt=var_data_type           v=variable_decl_assignment_list { [pr] @ pn @ [vt] @ v }
   | pr=port_direction_reset pn=port_decl_net_opt s=signing_opt r=range_list v=variable_decl_assignment_list { [pr] @ pn @ s @ r @ v }
   | pr=port_direction_reset pn=port_decl_net_opt s=signing                  v=variable_decl_assignment_list { [pr] @ pn @ (s::v) }
   | pr=port_direction_reset pn=port_decl_net_opt                            v=variable_decl_assignment_list { [pr] @ pn @ v }
;

tf_port_declaration:
   | p=port_direction_reset v=var_data_type      t=tf_variable_identifier_list SEMICOLON { mknode $startpos $endpos L.TfPortDeclaration ([p; v] @ t) }
   | p=port_direction_reset i=implicit_data_type t=tf_variable_identifier_list SEMICOLON { mknode $startpos $endpos L.TfPortDeclaration (p::(i @ t)) }
;

integer_atom_type:
   | BYTE     { LDT.Byte }
   | SHORTINT { LDT.Shortint }
   | INT      { LDT.Int }
   | LONGINT  { LDT.Longint }
   | INTEGER  { LDT.Integer }
   | TIME     { LDT.Time }
;

integer_vector_type:
   | BIT   { LDT.Bit }
   | LOGIC { LDT.Logic }
   | REG   { LDT.Reg }
;

non_integer_type:
   | SHORTREAL { LDT.Shortreal }
   | REAL      { LDT.Real }
   | REALTIME  { LDT.Realtime }
;

%inline
signing_opt:
   | (* empty *) { [] }
   | s=signing   { [s] }
;

(*
signing_opt:
   | (* empty *) { printf "!!! %d-%d\n" $startpos $endpos; [] }
   | signing     { [$1] }
;
*)

signing:
   | SIGNED   { mkleaf $startpos $endpos L.Signed }
   | UNSIGNED { mkleaf $startpos $endpos L.Unsigned }
;

casting_type:
   | simple_type { mknode $startpos $endpos L.CastingTypeSimple [$1] }
   | SIGNED      { mkleaf $startpos $endpos L.CastingTypeSigned }
   | UNSIGNED    { mkleaf $startpos $endpos L.CastingTypeUnsigned }
   | STRING      { mkleaf $startpos $endpos L.CastingTypeString }
   | CONST       { mkleaf $startpos $endpos L.CastingTypeConst }
;

simple_type:
   | integer_atom_type   { mkleaf $startpos $endpos (L.DataType $1) }
   | integer_vector_type { mkleaf $startpos $endpos (L.DataType $1) }
   | non_integer_type    { mkleaf $startpos $endpos (L.DataType $1) }
   | ps_type             { let psif, id = $1 in mknode $startpos $endpos (L.data_type (LDT.PsType id)) psif }
;

data_type_var:
   | data_type                                                     { $1 }
   | VIRTUAL__I INTERFACE i0=id p=parameter_value_assignment_opt DOT i1=id 
       { 
	 mknode $startpos $endpos (L.VirtualInterfaceDeclaration i0) (p @ [mkleaf $startpos(i1) $endpos (L.Port i1)])
       }
;

data_type:
   | i=integer_vector_type s=signing_opt r=range_list_opt { mknode $startpos $endpos (L.data_type i) (s @ r) }
   | i=integer_atom_type   s=signing_opt                  { mknode $startpos $endpos (L.data_type i) s }
   | non_integer_type                               { mkleaf $startpos $endpos (L.data_type $1) }
   | STRUCT p=packed_signing_opt           l=LBRACE s=struct_union_member_list r=RBRACE pd=packed_dimension_list_opt 
       { 
	 ignore (l, r);
	 let packed, signing = p in
         let b = mknode $startpos(l) $endpos(r) L.StructUnionBody s in
	 mknode $startpos $endpos (L.data_type LDT.Struct) (packed @ signing @ [b] @ pd)
       }
   | UNION t=tagged_opt p=packed_signing_opt l=LBRACE s=struct_union_member_list r=RBRACE pd=packed_dimension_list_opt 
       { 
	 ignore (l, r);
	 let packed, signing = p in
         let b = mknode $startpos(l) $endpos(r) L.StructUnionBody s in
	 mknode $startpos $endpos (L.data_type LDT.Union) (t @ packed @ signing @ [b] @ pd)
       }
   | enum_declaration { $1 }
   | STRING  { mkleaf $startpos $endpos (L.data_type LDT.String) }
   | CHANDLE { mkleaf $startpos $endpos (L.data_type LDT.Chandle) }
   | VIRTUAL__I INTERFACE i=id p=parameter_value_assignment_opt { mknode $startpos $endpos (L.data_type (LDT.VirtualInterface i)) p }
   | VIRTUAL__ID          i=id p=parameter_value_assignment_opt { mknode $startpos $endpos (L.data_type (LDT.VirtualInterface i)) p }
   | EVENT { mkleaf $startpos $endpos (L.data_type LDT.Event) }
   | type_reference                             { $1 }
   | p=ps_type          pd=packed_dimension_list_opt { let psif, id = p in mknode $startpos $endpos (L.data_type (LDT.PsType id)) (psif @ pd) }
   | c=class_scope_type pd=packed_dimension_list_opt { let csif, id = c in mknode $startpos $endpos (L.data_type (LDT.ClassScopeType id)) (csif @ pd) }
   | class_type_without_id                      { mknode $startpos $endpos (L.data_type LDT.ClassType) $1 }
   | ps_covergroup_identifier                   { $1 }
;

data_type_or_void:
   | data_type { $1 }
   | VOID      { mkleaf $startpos $endpos L.Void }
;

var_data_type:
   |     d=data_type          { d }
   | VAR d=data_type          { mknode $startpos $endpos L.VarDataType [d] }
   | VAR i=implicit_data_type { mknode $startpos $endpos L.VarDataType i }
;

type_reference:
   | TYPE LPAREN expr_or_data_type RPAREN { mknode $startpos $endpos (L.data_type LDT.TypeReference) [$3] }
;

struct_union_member_list:
   |                          struct_union_member { [$1] }
   | struct_union_member_list struct_union_member { $1 @ [$2] }
;

struct_union_member:
   | random_qualifier_opt data_type_or_void variable_decl_assignment_list SEMICOLON 
       { 
	 mknode $symbolstartpos $endpos L.StructUnionMember ($1 @ [$2] @ $3)
       }
;

variable_decl_assignment_list:
   |                                     variable_decl_assignment { [$1] }
   | variable_decl_assignment_list COMMA variable_decl_assignment { $1 @ [$3] }
;

variable_decl_assignment:
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt                       
       { mknode $startpos $endpos (L.VariableDeclAssignment i) (v @ s) }

   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt EQ vd=variable_decl_expr 
       { mknode $startpos $endpos (L.VariableDeclAssignment i) (v @ s @ [vd]) }

   | i=id_SV_keyword v=variable_dimension_list_opt s=sig_attr_list_opt                       
       { mknode $startpos $endpos (L.VariableDeclAssignment i) (v @ s) }

   | i=id_SV_keyword v=variable_dimension_list_opt s=sig_attr_list_opt EQ vd=variable_decl_expr 
       { mknode $startpos $endpos (L.VariableDeclAssignment i) (v @ s @ [vd]) }

   | EQ class_new                                                           
       { mknode $startpos $endpos (L.VariableDeclAssignment "") [$2] }
;

tf_variable_identifier_list:
   |                                   tf_variable_identifier { [$1] }
   | tf_variable_identifier_list COMMA tf_variable_identifier { $1 @ [$3] }
;

tf_variable_identifier:
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt           { mknode $startpos $endpos (L.TfVariableIdentifier i) (v @ s) }
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt EQ e=expr { mknode $startpos $endpos (L.TfVariableIdentifier i) (v @ s @ [e]) }
   | i=id_SV_keyword v=variable_dimension_list_opt s=sig_attr_list_opt           { mknode $startpos $endpos (L.TfVariableIdentifier i) (v @ s) }
   | i=id_SV_keyword v=variable_dimension_list_opt s=sig_attr_list_opt EQ e=expr { mknode $startpos $endpos (L.TfVariableIdentifier i) (v @ s @ [e]) }
;

variable_decl_expr:
   | e=expr              { e }
   | d=dynamic_array_new { d }
   | c=class_new         { c }
;

%inline
variable_dimension_list_opt:
   | (* empty *)               { [] }
   | v=variable_dimension_list { v }
;

variable_dimension_list:
   | v=variable_dimension_list_ { [mknode $startpos $endpos L.VariableDimensions v] }
;

variable_dimension_list_:
   |                             v=variable_dimension { [v] }
   | vl=variable_dimension_list_ v=variable_dimension { vl @ [v] }
;

variable_dimension:
   | LBRACKET RBRACKET                             { mkleaf $startpos $endpos L.VariableDimension }
   | LBRACKET const_expr COLON const_expr RBRACKET { mknode $startpos $endpos L.VariableDimension [$2; $4] }
   | LBRACKET const_expr RBRACKET { mknode $startpos $endpos L.VariableDimension [$2] }
   | LBRACKET data_type RBRACKET  { mknode $startpos $endpos L.VariableDimension [$2] }
   | LBRACKET_STAR RBRACKET       { mkleaf $startpos $endpos L.VariableDimensionStar }
   | LBRACKET STAR RBRACKET       { mkleaf $startpos $endpos L.VariableDimensionStar }
;

random_qualifier_opt:
   | (* empty *)        { [] }
   | r=random_qualifier { [r] }
;

random_qualifier:
   | RAND  { mkleaf $startpos $endpos (L.qualifier LQ.Rand) }
   | RANDC { mkleaf $startpos $endpos (L.qualifier LQ.Randc) }
;

tagged_opt:
   | (* empty *) { [] }
   | TAGGED      { [mkleaf $startpos $endpos L.Tagged] }
;

packed_signing_opt:
   | (* empty *)            { [], [] }
   | p=PACKED s=signing_opt { ignore p; [mkleaf $startpos $endpos(p) L.Packed], s }
;

enum_declaration:
   | ENUM e=enum_base_type_opt l=LBRACE en=enum_name_list r=RBRACE rl=range_list_opt 
       { 
	 ignore (l, r);
	 mknode $startpos $endpos (L.data_type LDT.Enum) (e @ [mknode $startpos(l) $endpos(r) L.EnumBody en] @ rl)
       }
;

enum_base_type_opt:
   | (* empty *)                                         { [] }
   |                       s=signing_opt r=range_list    { [mknode $startpos $endpos (L.data_type LDT.Implicit) (s @ r)] }
   |                       s=signing                     { [mknode $startpos $endpos (L.data_type LDT.Implicit) [s]] }
   | i=integer_atom_type   s=signing_opt                 { [mknode $startpos $endpos (L.data_type i) s] }
   | i=integer_vector_type s=signing_opt r=reg_range_opt { [mknode $startpos $endpos (L.data_type i) (s @ r)] }
   | i=TYPE_IDENTIFIER                   r=reg_range_opt { [mknode $startpos $endpos (L.data_type (LDT.Named i)) r] }
;

enum_name_list:
   |                      enum_name_declaration { [$1] }
   | enum_name_list COMMA enum_name_declaration { $1 @ [$3] }
;

enum_name_declaration:
   | i=id_any e=enum_name_range_opt en=enum_name_start_opt { mknode $startpos $endpos (L.EnumNameDeclaration i) (e @ en) }
;

enum_name_range_opt:
   | (* empty *)                                             { [] }
   | LBRACKET integral_number RBRACKET                       
       { 
	 [mknode $startpos $endpos L.Range [mkleaf $startpos($2) $endpos($2) (L.expr_of_integral_number $2)]] 
       }
   | LBRACKET integral_number COLON integral_number RBRACKET 
       { 
	 [mknode $startpos $endpos L.Range [mkleaf $startpos($2) $endpos($3) (L.expr_of_integral_number $2); 
					    mkleaf $startpos($4) $endpos($4) (L.expr_of_integral_number $4)]]
       }
;

%inline
enum_name_start_opt:
   | (* empty *)     { [] }
   | EQ c=const_expr { [c] }
;

data_declaration:
   | d=data_declaration_var       { d }
   | t=type_declaration           { t }
   | p=package_import_declaration { p }
;

class_property:
   |       member_qual_reset_list_opt data_declaration_var_class { mknode $symbolstartpos $endpos L.ClassProperty ($1 @ [$2]) }
   | const member_qual_reset_list_opt data_declaration_var_class { mknode $startpos $endpos L.ClassProperty ($1 :: $2 @ [$3]) }
   |       member_qual_reset_list_opt type_declaration           { mknode $symbolstartpos $endpos L.ClassProperty ($1 @ [$2]) }
   |       member_qual_reset_list_opt package_import_declaration { mknode $symbolstartpos $endpos L.ClassProperty ($1 @ [$2]) }
;

data_declaration_var:
   | h=data_declaration_var_head l=variable_decl_assignment_list SEMICOLON
       { 
         let v = mknode $startpos(l) $endpos(l) L.VarDeclAssignments l in
         mknode $symbolstartpos $endpos L.DataDeclarationVar (h @ [v])
       }
;

data_declaration_var_class:
   | h=data_declaration_var_head_class l=variable_decl_assignment_list SEMICOLON
       { 
         let v = mknode $startpos(l) $endpos(l) L.VarDeclAssignments l in
         mknode $symbolstartpos $endpos L.DataDeclarationVarClass (h @ [v])
       }
;

const:
   | CONST { mkleaf $startpos $endpos L.Const }
;

%inline
var:
   | VAR { mkleaf $startpos $endpos L.Var }
;

data_declaration_var_head:
   | c=const_opt v=var l=lifetime_opt d=data_type           { c @ (v::l) @ [d] }
   | c=const_opt v=var l=lifetime_opt                       { c @ (v::l) }
   | c=const_opt v=var l=lifetime_opt i=_implicit_data_type { c @ (v::l) @ [i] }
   |                                  d=data_type_var       { [d] }
   |                   l=lifetime     d=data_type_var       { [l; d] }
   | c=const           l=lifetime_opt d=data_type_var       { c :: l @ [d] }
;

data_declaration_var_head_class:
   | v=var l=lifetime_opt d=data_type           { v :: l @ [d] }
   | v=var l=lifetime_opt                       { v :: l }
   | v=var l=lifetime_opt i=_implicit_data_type { (v :: l) @ [i] }
   |                      d=data_type           { [d] }
;

const_opt:
   | (* empty *) { [] }
   | c=const     { [c] }
;

%inline
_implicit_data_type:
   | s=signing_opt p=packed_dimension_list { mknode $symbolstartpos $endpos L.ImplicitDataType (s @ p) }
   | s=signing                             { mknode $symbolstartpos $endpos L.ImplicitDataType [s] }
;

%inline
implicit_data_type:
   | (* empty *)                           { [] }
   | i=_implicit_data_type                 { [i] }
;

assertion_variable_declaration:
   | var_data_type variable_decl_assignment_list SEMICOLON { mknode $startpos $endpos L.AssertionVariableDeclaration ($1::$2) }
;

type_declaration:
   | type_declaration_ SEMICOLON { reloc $startpos $endpos $1; $1 }
;

type_declaration_:
   | TYPEDEF d=data_type                    i=id_any v=variable_dimension_list_opt
       { 
	 register_type i;
	 mknode $startpos $endpos (L.TypeDeclaration i) (d::v) 
       }
   | TYPEDEF id bit_select_opt DOT id_any id_any                            
       { 
	 register_type $6;
	 let sty = mknode $startpos($2) $endpos($5) (L.ScopedType $5) ((mkleaf $startpos($2) $endpos($2) (L.InterfaceIdentifier $2))::$3) in
	 mknode $startpos $endpos (L.TypeDeclaration $6) [sty] 
       }
   | TYPEDEF        id     { register_type $2; mkleaf $startpos $endpos (L.TypeDeclaration $2) }
   | TYPEDEF ENUM   id_any { register_type $3; mknode $startpos $endpos (L.TypeDeclaration $3) [mkleaf $startpos($2) $endpos($2) L.TypeDeclEnum] }
   | TYPEDEF STRUCT id_any { register_type $3; mknode $startpos $endpos (L.TypeDeclaration $3) [mkleaf $startpos($2) $endpos($2) L.TypeDeclStruct] }
   | TYPEDEF UNION  id_any { register_type $3; mknode $startpos $endpos (L.TypeDeclaration $3) [mkleaf $startpos($2) $endpos($2) L.TypeDeclUnion] }
   | TYPEDEF CLASS  id_any { register_class $3; mknode $startpos $endpos (L.TypeDeclaration $3) [mkleaf $startpos($2) $endpos($2) L.TypeDeclClass] }
;

module_item_list_opt:
   | (* empty *)        { [] }
   | l=module_item_list { [mknode $startpos $endpos L.ModuleBody l] }
;


partial_module_item_list:
   | l=module_item_list EOP { Ast.Pmodule_item_list l }
   |                    EOP { Ast.Pmodule_item_list [] }
;

module_item_list:
   |                     m=module_item { context_stack#activate_top; [m] }
   | ml=module_item_list m=module_item { context_stack#activate_top; ml @ [m] }
;

module_item:
   | p=port_declaration SEMICOLON { reloc $startpos $endpos p; p }
   | n=non_port_module_item       { n }
   | c=compiler_directive         { c }
   | m=MODULE_ITEM                { check_error m; m }

   | e=error module_item_tail { ignore e; parse_error $startpos(e) $endpos(e) "syntax error" }
;


module_item_tail:
   | SEMICOLON    { }
   | ENDTASK      { }
   | ENDFUNCTION  { }
   | ENDMODULE_   { }
   | END_         { }
   | ENDGENERATE  { }
   | end_label    { }
   | ENDCHECKER   { }
   | RBRACE       { }
   | ENDCLASS     { }
   | ENDGROUP     { }
   | ENDPROPERTY  { }
   | ENDCLOCKING  { }
   | ENDSPECIFY   { }
   | ENDPROGRAM   { }
   | ENDINTERFACE { }
   | ENDCASE      { }
   | ENDSEQUENCE  { }
;


non_port_module_item:
   | g=generate_region         { g }
   | m=module_or_generate_item { m }
   | s=specify_block           { s }
   | s=specparam_declaration   { s }
   | p=program_declaration     { p }
   | m=module_declaration      { m }
   | i=interface_declaration   { i }
   | t=timeunits_declaration   { t }

   | a=attribute_instances s=specparam_declaration { reloc $startpos $endpos s; s#add_children_l [a]; s }
;

module_or_generate_item:
(*   | parameter_override { $1 } --> package_or_generate_item_declaration*)
   | m=module_common_item { m }

(*   | attribute_instances parameter_override { reloc $startpos $endpos $2; add_children_l [$1] $2; $2 }*)
   | a=attribute_instances m=module_common_item { reloc $startpos $endpos m; m#add_children_l [a]; m }
;

parameter_override:
   | DEFPARAM defparam_assignment_list SEMICOLON { mknode $startpos $endpos L.ParameterOverride $2 }
;

module_common_item:
   | m=module_or_generate_item_declaration         { m } 
(*   | etc_inst                                      { $1 } --> package_or_generate_item_declaration*)
   | a=assertion_item                              { a }
   | b=bind_directive                              { b }
(*   | continuous_assign                             { $1 } --> package_or_generate_item_declaration*)
   | ALIAS v=variable_lvalue a=alias_eq_list SEMICOLON { mknode $startpos $endpos L.NetAlias (v::a) }
(*   | initial_construct                             { $1 } --> package_or_generate_item_declaration*)
   | f=final_construct                             { f }
(*   | ALWAYS stmt_block                             { mknode $startpos $endpos (L.Always_construct $1) [$2] } --> package_or_generate_item_declaration*)
   | l=loop_generate_construct                     { l } 
   | c=conditional_generate_construct              { c }
   | e=elaboration_system_task                     { e }
;

continuous_assign:
   | ASSIGN s=strength_spec_opt d=delay_opt a=assign_list SEMICOLON { mknode $startpos $endpos L.ContinuousAssign (s @ d @ a) }
;

(*
initial_construct:
   | INITIAL stmt_block { mknode $startpos $endpos L.Linitial_construct [$2] }
;
*)
initial_construct:
   | INITIAL s=stmt_non_block { mknode $startpos $endpos L.InitialConstruct [s] }
   | INITIAL s=stmt_block_act { mknode $startpos $endpos L.InitialConstruct [s] }
;


(*
final_construct:
   | FINAL stmt_block { mknode $startpos $endpos L.Final_construct [$2] }
;
*)
final_construct:
   | FINAL s=stmt_non_block { mknode $startpos $endpos L.FinalConstruct [s] }
   | FINAL s=stmt_block_act { mknode $startpos $endpos L.FinalConstruct [s] }
;


module_or_generate_item_declaration:
   | p=package_or_generate_item_declaration { p }
   | g=genvar_declaration                   { g }
   | c=clocking_declaration                 { c }
   | d=default CLOCKING i=id_any  SEMICOLON { mknode $startpos $endpos (L.ClockingDeclaration i) [d] }
   | d=default DISABLE IFF e=expr SEMICOLON { mknode $startpos $endpos L.DisableIff [d; e] }
;

alias_eq_list:
   |               EQ variable_lvalue { [$2] }
   | alias_eq_list EQ variable_lvalue { $1 @ [$3] }
;

bind_directive:
   | BIND id bit_select_opt                  bind_instantiation { mknode $startpos $endpos (L.BindDirective $2) ($3 @ [$4]) }
   | BIND id COLON bind_target_instance_list bind_instantiation { mknode $startpos $endpos (L.BindDirective $2) ($4 @ [$5]) }
;

bind_target_instance_list:
   |                                 bind_target_instance { [$1] }
   | bind_target_instance_list COMMA bind_target_instance { $1 @ [$3] }
;

bind_target_instance:
   | h=hierarchical_identifier_bit { h }
;

bind_instantiation:
   | e=etc_inst { e }
;

generate_region:
   | generate0 GENERATE_ b=gen_top_block ENDGENERATE
       { 
         context_stack#pop_and_activate;
         mknode $startpos $endpos L.GenerateRegion b
       }
 ;

c_generate_region:
   | generate0 GENERATE_ b=c_gen_top_block ENDGENERATE
       { 
         context_stack#pop_and_activate;
         mknode $startpos $endpos L.GenerateRegion b
       }
;

generate0:
   | GENERATE { context_stack#push (Context.generate_item_list()) }
;

generate_block:
   | g=generate_item  { g }
   | g=gen_item_begin { g }
;

c_generate_block:
   | c=c_generate_item  { c }
   | c=c_gen_item_begin { c }
;

gen_top_block:
   | l=gen_item_list  { l }
   | g=gen_item_begin { [g] }
;

c_gen_top_block:
   | l=c_gen_item_list  { l }
   | c=c_gen_item_begin { [c] }
;

%inline
gen_block_id0_opt:
   | (* empty *) { [] }
   | i=id COLON  { [mkleaf $startpos $endpos (L.GenBlockId i)] }
;
%inline
gen_block_id1_opt:
   | (* empty *)    { [] }
   | COLON i=id_any { [mkleaf $startpos $endpos (L.GenBlockId i)] }
;
%inline
gen_item_list_opt:
   | (* empty *)     { [] }
   | g=gen_item_list { g }
;

begin_kw:
   | BEGIN BEGIN_ { }
;

end_kw:
   | END END_ { }
;

fork_kw:
   | FORK FORK_ { }
;

join_kw:
   | JOIN JOIN_ { }
;

gen_item_begin:
   | i0=gen_block_id0_opt begin_kw i1=gen_block_id1_opt g=gen_item_list_opt end_kw i2=gen_block_id1_opt 
       { 
	 mknode $symbolstartpos $endpos L.GenItemBegin (i0 @ i1 @ g @ i2) 
       }
;

%inline
c_gen_item_list_opt:
   | (* empty *)       { [] }
   | g=c_gen_item_list { g }
;

c_gen_item_begin:
   | i0=gen_block_id0_opt begin_kw i1=gen_block_id1_opt g=c_gen_item_list_opt end_kw i2=gen_block_id1_opt 
       { 
	 mknode $symbolstartpos $endpos L.GenItemBegin (i0 @ i1 @ g @ i2) 
       }
;

partial_gen_item_list:
   | l=gen_item_list EOP { Ast.Pgenerate_item_list l }
   |                 EOP { Ast.Pgenerate_item_list [] }
;

gen_item_list:
   |               generate_item { [$1] }
   | gen_item_list generate_item { $1 @ [$2] }
;

c_gen_item_list:
   |                 c_generate_item { [$1] }
   | c_gen_item_list c_generate_item { $1 @ [$2] }
;

generate_item:
   | m=module_or_generate_item    { m }
   | i=interface_or_generate_item { i }
   | g=GENERATE_ITEM              { check_error g; g }
;

c_generate_item:
   | c=checker_or_generate_item { c }
;

conditional_generate_construct:
   | CASE LPAREN expr RPAREN                         ENDCASE { mknode $startpos $endpos L.ConditionalGenerateConstructCase [$3] }
   | CASE LPAREN expr RPAREN case_generate_item_list ENDCASE { mknode $startpos $endpos L.ConditionalGenerateConstructCase ($3::$5) }
   | IF LPAREN expr RPAREN generate_block %prec P_LOWER_THAN_ELSE { mknode $startpos $endpos L.ConditionalGenerateConstructIf [$3; $5] }
   | IF LPAREN expr RPAREN generate_block ELSE generate_block     { mknode $startpos $endpos L.ConditionalGenerateConstructIf [$3; $5; $7] }
;

c_conditional_generate_construct:
   | CASE LPAREN expr RPAREN                           ENDCASE { mknode $startpos $endpos L.ConditionalGenerateConstructCase [$3] }
   | CASE LPAREN expr RPAREN c_case_generate_item_list ENDCASE { mknode $startpos $endpos L.ConditionalGenerateConstructCase ($3::$5) }
   | IF LPAREN expr RPAREN c_generate_block %prec P_LOWER_THAN_ELSE { mknode $startpos $endpos L.ConditionalGenerateConstructIf [$3; $5] }
   | IF LPAREN expr RPAREN c_generate_block ELSE c_generate_block   { mknode $startpos $endpos L.ConditionalGenerateConstructIf [$3; $5; $7] }
;

loop_generate_construct:
   | FOR LPAREN genvar_initialization SEMICOLON expr SEMICOLON genvar_iteration RPAREN generate_block 
       { 
	 mknode $startpos $endpos L.LoopGenerateConstruct [$3; $5; $7; $9]
       }
;

c_loop_generate_construct:
   | FOR LPAREN genvar_initialization SEMICOLON expr SEMICOLON genvar_iteration RPAREN c_generate_block 
       { 
	 mknode $startpos $endpos L.LoopGenerateConstruct [$3; $5; $7; $9]
       }
;

genvar_initialization:
   | id                            EQ const_expr { mknode $startpos $endpos (L.GenvarInitId $1) [$3] }
   | GENVAR genvar_identifier_decl EQ const_expr { mknode $startpos $endpos L.GenvarInit [$2; $4] }
;

genvar_iteration:
   | i=id a=_aop e=expr { mknode $startpos $endpos (L.GenvarIterationAssign(a, i)) [e] }
   | PLUS_PLUS   i=id   { mkleaf $startpos $endpos (L.GenvarIterationIncOrDec(Ls.IncOrDecOperator.PreIncr, i)) }
   | MINUS_MINUS i=id   { mkleaf $startpos $endpos (L.GenvarIterationIncOrDec(Ls.IncOrDecOperator.PreDecr, i)) }
   | i=id PLUS_PLUS     { mkleaf $startpos $endpos (L.GenvarIterationIncOrDec(Ls.IncOrDecOperator.PostIncr, i)) }
   | i=id MINUS_MINUS   { mkleaf $startpos $endpos (L.GenvarIterationIncOrDec(Ls.IncOrDecOperator.PostDecr, i)) }
;

case_generate_item_list:
   |                         case_generate_item { [$1] }
   | case_generate_item_list case_generate_item { $1 @ [$2] }
;

c_case_generate_item_list:
   |                           c_case_generate_item { [$1] }
   | c_case_generate_item_list c_case_generate_item { $1 @ [$2] }
;

case_generate_item:
   | case_cond_list COLON generate_block { mknode $startpos $endpos L.CaseGenerateItem ($1 @ [$3]) }
   | DEFAULT        COLON generate_block { mknode $startpos $endpos L.CaseGenerateItemDefault [$3] }
   | DEFAULT              generate_block { mknode $startpos $endpos L.CaseGenerateItemDefault [$2] }
;

c_case_generate_item:
   | case_cond_list COLON c_generate_block { mknode $startpos $endpos L.CaseGenerateItem ($1 @ [$3]) }
   | DEFAULT        COLON c_generate_block { mknode $startpos $endpos L.CaseGenerateItemDefault [$3] }
   | DEFAULT              c_generate_block { mknode $startpos $endpos L.CaseGenerateItemDefault [$2] }
;

assign_list:
   |                   assign { [$1] }
   | assign_list COMMA assign { $1 @ [$3] }
;

assign:
   | variable_lvalue EQ expr         { mknode $startpos $endpos L.Assign [$1; $3] }
   | id_SV_keyword   EQ expr 
       { 
	 let idnd0 = mkleaf $startpos($1) $endpos($1) (L.IdSelect $1) in
	 let idnd = mknode $startpos($1) $endpos($1) (L.IdClassSel $1) [idnd0] in
	 mknode $startpos $endpos L.Assign [idnd; $3] 
       }
;

delay_or_event_control_opt:
   | (* empty *)   { [] }
   | delay_control { [$1] }
   | event_control { [$1] }
   | REPEAT LPAREN expr RPAREN event_control { [mknode $startpos $endpos L.EventControlRepeat [$3; $5]] }
;

%inline
delay_opt:
   | (* empty *)     { [] }
   | d=delay_control { [d] }
;

delay_control:
   | SHARP delay_value { mknode $startpos $endpos L.DelayControl [$2] }
   | SHARP LPAREN min_type_max                                       RPAREN { mknode $startpos $endpos L.DelayControl [$3] }
   | SHARP LPAREN min_type_max COMMA min_type_max                    RPAREN { mknode $startpos $endpos L.DelayControl [$3; $5] }
   | SHARP LPAREN min_type_max COMMA min_type_max COMMA min_type_max RPAREN { mknode $startpos $endpos L.DelayControl [$3; $5; $7] }
;

delay_value:
   | p=ps_id_etc       { let ps, id = p in mknode $startpos $endpos (L.DelayValue id) ps }
   | i=INTEGRAL_NUMBER { mkleaf $startpos $endpos (L.DelayValue i) }
   | r=REAL_NUMBER     { mkleaf $startpos $endpos (L.DelayValue r) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.DelayValue $1) }
   | c=PP_MACRO_CONST     { mkleaf $startpos $endpos (L.DelayValue c) }
   | c=PP_MACRO_CONST_INT { mkleaf $startpos $endpos (L.DelayValue c) }
;

delay_expr:
   | e=expr { e }
;

min_type_max:
   | delay_expr                                   { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$1] }
   | delay_expr COLON delay_expr COLON delay_expr { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$1; $3; $5] }
;

net_decl_assignments:
   | nl=net_sig_list 
       { 
	 let ids = List.map (fun n -> n#get_identifier) nl in
	 mknode $startpos $endpos (L.NetDeclAssignments ids) nl
       }
;

net_sig_list:
   |                    net_sig { [$1] }
   | net_sig_list COMMA net_sig { $1 @ [$3] }
;

net_sig:
   | n=net_id              s=sig_attr_list_opt           { mknode $startpos $endpos (L.NetSig n) s }
   | n=net_id              s=sig_attr_list_opt EQ e=expr { mknode $startpos $endpos (L.NetSig n) (s @ [e]) }
   | n=net_id r=range_list s=sig_attr_list_opt           { mknode $startpos $endpos (L.NetSig n) (r @ s) }
;

net_id:
   | id            { $1 }
   | id_SV_keyword { $1 }
;

%inline
sig_attr_list_opt:
   | (* empty *) { [] }
;

%inline
range_list_opt:
   | (* empty *)   { [] }
   | r=range_list  { r }
;

range_list:
   | range_list_ { [mknode $startpos $endpos L.Ranges $1] }
;

range_list_:
   |             any_range { [$1] }
   | range_list_ any_range { $1 @ [$2] }
;

%inline
reg_range_opt:
   | (* empty *) { [] }
   | a=any_range { [a] }
;

bit_select_opt:
   | (* empty *)                  { [] }
   | LBRACKET const_expr RBRACKET { [mknode $startpos $endpos L.BitSelect [$2]] }
;

any_range:
   | LBRACKET const_expr COLON const_expr RBRACKET { mknode $startpos $endpos L.Range [$2; $4] }
   | LBRACKET const_expr                  RBRACKET { mknode $startpos $endpos L.Range [$2] }
(*   | LBRACKET PP_IDENTIFIER               RBRACKET { mknode $startpos $endpos L.Range [mkleaf $startpos($2) $endpos (L.Pp_identifier $2)] }*)
;

%inline
packed_dimension_list_opt:
   | (* empty *)             { [] }
   | p=packed_dimension_list { p }
;

packed_dimension_list:
   |                       packed_dimension { [$1] }
   | packed_dimension_list packed_dimension { $1 @ [$2] }
;

packed_dimension:
   | any_range         { mknode $startpos $endpos L.PackedDimension [$1] }
   | LBRACKET RBRACKET { mknode $startpos $endpos L.PackedDimension [] }
;

param_assignment:
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt EQ e=expr_or_data_type 
       { 
	 register_vartype i;
	 mknode $startpos $endpos (L.ParamAssignment i) (v @ s @ [e])
       }
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt EQ e=error
       { 
	 register_vartype i;
         ignore e;
	 mknode $startpos $endpos (L.ParamAssignment i) (v @ s @ [parse_error $startpos(e) $endpos(e) "syntax error"])
       }
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt                      
       { 
	 register_vartype i;
	 mknode $startpos $endpos (L.ParamAssignment i) (v @ s)
       }
;

param_assignments:
   | pl=param_assignment_list { mknode $startpos $endpos (L.ParamAssignments (List.map (fun p -> p#get_identifier) pl)) pl }
;

param_assignment_list:
   |                             param_assignment { [$1] }
   | param_assignment_list COMMA param_assignment { $1 @ [$3] }
;

defparam_assignment_list:
   |                                defparam_assignment { [$1] }
   | defparam_assignment_list COMMA defparam_assignment { $1 @ [$3] }
;

defparam_assignment:
   | hierarchical_identifier EQ expr { mknode $startpos $endpos L.DefparamAssignment [$1; $3] }
;

etc_inst:
   | etc_inst0 SEMICOLON { reloc $startpos $endpos $1; $1 }
;

etc_inst0:
   | i=inst_name s=strength_spec_opt p=parameter_value_assignment_opt il=instname_list 
       { 
	 context_stack#activate_top_no_delay; 
	 mknode $startpos $endpos i (s @ p @ il) 
       }
;

inst_name:
   | gate_keyword { L.GateInstantiation $1 }
   | id           { L.Instantiation $1 }
;

instname_list:
   |                     instname_paren { [$1] }
   | instname_list COMMA instname_paren { $1 @ [$3] }
;

instname_paren:
   | i=instname c=cellpin_list RPAREN { context_stack#pop; let l, ir = i in mknode $symbolstartpos $endpos l (ir @ c) }
;

instname:
   | i=instname0 LPAREN { i }
;

instname0:
   | i=id ir=inst_range_opt { context_stack#push (Context.cellpin_list()); L.InstName i, ir }
   |      ir=inst_range_opt { context_stack#push (Context.cellpin_list()); L.InstName "", ir }
;

%inline
inst_range_opt:
   | (* empty *)                                         { [] }
   | LBRACKET c=const_expr                      RBRACKET { [mknode $startpos $endpos L.InstRange [c]] }
   | LBRACKET c0=const_expr COLON c1=const_expr RBRACKET { [mknode $startpos $endpos L.InstRange [c0; c1]] }
;

partial_cellpin_list:
   | cellpin_list EOP { Ast.Pcellpin_list $1 }
;

cellpin_list:
   | cellpin_item_list { $1 }
;

cellpin_item_list:
   |                           c=cellpin_item_opt { context_stack#activate_top; c }
   | l=cellpin_item_list COMMA c=cellpin_item_opt { context_stack#activate_top; l @ c }
;

%inline
cellpin_item_opt:
   | (* empty *)    { [] }
   | c=cellpin_item { [c] }
;

cellpin_item:
   | DOT_STAR                                                          { mkleaf $startpos $endpos L.CellpinStar }
   | DOT i=id_any                                                      { mkleaf $startpos $endpos (L.Cellpin i) }
   | DOT i=id_any LPAREN                                        RPAREN { mkleaf $startpos $endpos (L.Cellpin i) }
   | DOT i=id_any LPAREN p=pev_expr                             RPAREN { mknode $startpos $endpos (L.Cellpin i) [p] }
   | DOT i=id_any LPAREN p=pev_expr COLON e=expr                RPAREN { mknode $startpos $endpos (L.Cellpin i) [p; e] }
   | DOT i=id_any LPAREN p=pev_expr COLON e1=expr COLON e2=expr RPAREN { mknode $startpos $endpos (L.Cellpin i) [p; e1; e2] }
   | DOT i=id_any LPAREN d=data_type                            RPAREN { mknode $startpos $endpos (L.Cellpin i) [d] }
   |                     d=data_type                                   { mknode $startpos $endpos L.CellpinAnon [d] }

   | e =expr                             { mknode $startpos $endpos L.CellpinAnon [e] }
   | e1=expr COLON e2=expr               { mknode $startpos $endpos L.CellpinAnon [e1; e2] }
   | e1=expr COLON e2=expr COLON e3=expr { mknode $startpos $endpos L.CellpinAnon [e1; e2; e3] }

   | CELLPIN_ITEM { check_error $1; $1 }

   | e=error cellpin_item_tail { ignore e; parse_error $startpos(e) $endpos(e) "syntax error" }
;

%inline
cellpin_item_tail:
(*   | id_any          { }*)
   | RPAREN          { }
(*
   | RBRACE          { }
   | RBRACKET        { }
   | INTEGRAL_NUMBER { }
   | REAL_NUMBER     { }
   | TIME_NUMBER     { }
   | DOLLAR          { }
   | DOT_STAR        { }
   | PLUS_PLUS       { }
   | MINUS_MINUS     { }
*)
;

event_control:
   | AT LPAREN el=ev_expr_list RPAREN { mknode $startpos $endpos L.EventControl el }
   | AT STAR                          { mkleaf $startpos $endpos L.EventControlStar }
   | AT LPAREN STAR            RPAREN { mkleaf $startpos $endpos L.EventControlParenStar }
   | AT LPAREN_STAR            RPAREN { mkleaf $startpos $endpos L.EventControlParenStar }
   | AT i=id_class_sel                { mknode $startpos $endpos L.EventControl [i] }

   | AT LPAREN el=ev_expr_list e=error RPAREN
       { 
         ignore e;
         parse_warning ~out:stdout $startpos(e) $endpos(e) "syntax error";
         mknode $startpos $endpos L.EventControl el
       }
   | AT LPAREN e=error                 RPAREN
       { 
         ignore e;
         mknode $startpos $endpos L.EventControl [parse_error $startpos(e) $endpos(e) "syntax error"]
       }
;

ev_expr_list:
   |                       e=ev_expr { [e] }
   | el=ev_expr_list COMMA e=ev_expr { el @ [e] }
   | el=ev_expr_list COMMA e=error
       { 
         ignore e;
         parse_warning ~out:stdout $startpos(e) $endpos(e) "syntax error";
         el
       }

;

%inline
edge_ev:
   | POSEDGE { LEE.Posedge }
   | NEGEDGE { LEE.Negedge }
   | EDGE    { LEE.Edge }
;

%inline
senitem_edge:
   | ev=edge_ev e=expr { mknode $startpos $endpos (L.ev_expr ev) [e] }
;

stmt_block:
   | s=stmt { s }
;

seq_block:
   | s=seq_block0 END_ e=end_label_opt { reloc $startpos $endpos s; s#add_children_r e; s }
;

seq_block0:
   | s=seq_block_head bl=block_decl_stmt_list END { context_stack#pop; mknode $startpos $endpos (L.stmt s) bl }
   | s=seq_block_head                         END { context_stack#pop; mknode $startpos $endpos (L.stmt s) [] }
;

seq_block_act:
   | b=seq_block0_act END_ e=end_label_opt { reloc $startpos $endpos b; b#add_children_r e; b }
;

seq_block0_act:
   | h=seq_block_head b=block_decl_stmt_list END { context_stack#pop_and_activate; mknode $startpos $endpos (L.stmt h) b }
   | h=seq_block_head                        END { context_stack#pop_and_activate; mknode $startpos $endpos (L.stmt h) [] }
;

par_block:
   | p=par_block0 JOIN_ e=end_label_opt { reloc $startpos $endpos p; p#add_children_r e; p }
;

par_block0:
   | p=par_block_head bl=block_decl_stmt_list j=JOIN { context_stack#pop; mknode $startpos $endpos (L.stmt (LS.ParBlock(p, j))) bl }
   | p=par_block_head                         j=JOIN { context_stack#pop; mknode $startpos $endpos (L.stmt (LS.ParBlock(p, j))) [] }
;

par_block_act:
   | b=par_block0_act JOIN_ e=end_label_opt { reloc $startpos $endpos b; b#add_children_r e; b }
;

par_block0_act:
   | h=par_block_head b=block_decl_stmt_list j=JOIN { context_stack#pop_and_activate; mknode $startpos $endpos (L.stmt (LS.ParBlock(h, j))) b }
   | h=par_block_head                        j=JOIN { context_stack#pop_and_activate; mknode $startpos $endpos (L.stmt (LS.ParBlock(h, j))) [] }
;


begin0:
   | BEGIN { context_stack#push (Context.block_decl_stmt_list()) }
;

seq_block_head:
   | begin0 BEGIN_                { LS.SeqBlock "" }
   | begin0 BEGIN_ COLON i=id_any { LS.SeqBlock i }
;

fork0:
   | FORK { context_stack#push (Context.block_decl_stmt_list()) }
;

par_block_head:
   | fork0 FORK_                { "" }
   | fork0 FORK_ COLON i=id_any { i }
;

partial_block_decl_stmt_list:
   | b=block_decl_stmt_list EOP { Ast.Pblock_decl_stmt_list b }
   |                        EOP { Ast.Pblock_decl_stmt_list [] }
;

block_decl_stmt_list:
   | bl=block_item_declaration_list              { bl }
   | bl=block_item_declaration_list sl=stmt_list { bl @ sl }
   |                                sl=stmt_list { sl }
;

block_item_declaration_list:
   |                                b=block_item_declaration { context_stack#activate_top; [b] }
   | bl=block_item_declaration_list b=block_item_declaration { context_stack#activate_top; bl @ [b] }
;

block_item_declaration:
   | d=data_declaration                      { d }
   | l=local_parameter_declaration SEMICOLON { reloc $startpos $endpos l; l }
   | p=parameter_declaration       SEMICOLON { reloc $startpos $endpos p; p }
   | o=overload_declaration                  { o }
   | l=let_declaration                       { l }
   | b=BLOCK_ITEM_DECLARATION                { check_error b; b }
;

stmt_list:
   |              s=stmt_block { context_stack#activate_top; [s] }
   | sl=stmt_list s=stmt_block { context_stack#activate_top; sl @ [s] }
;

stmt:
   | s=stmt_non_block  { s }
   | s=stmt_block_only { s }
   | SEMICOLON         { mkleaf $startpos $endpos (L.stmt LS.Empty) }
   | s=STMT            { check_error s; s }

   | c=compiler_directive { c }

   | e=error SEMICOLON   { ignore e; parse_error $startpos(e) $endpos(e) "syntax error" }
   | e=error ENDCASE     { ignore e; parse_error $startpos(e) $endpos(e) "syntax error" }
   | e=error ENDSEQUENCE { ignore e; parse_error $startpos(e) $endpos(e) "syntax error" }
;

stmt_non_block:
   |            s=statement_item_non_block { s }
   | i=id COLON s=statement_item_non_block { mknode $startpos $endpos (L.stmt (LS.Labeled i)) [s] }
;

stmt_block_only:
   |            s=statement_item_block { s }
   | i=id COLON s=statement_item_block { mknode $startpos $endpos (L.stmt (LS.Labeled i)) [s] }
;

stmt_block_act:
   |            s=statement_item_block_act { s }
   | i=id COLON s=statement_item_block_act { mknode $startpos $endpos (L.stmt (LS.Labeled i)) [s] }
;



nb_assign_postfix_opt:
   | (* empty *)         { [] }
   | n=NB_ASSIGN_POSTFIX { [mkleaf $startpos $endpos (L.PpIdentifier n)] }
;

inside_kw:
   | INSIDE INSIDE_ { }
;

statement_item_non_block:
   | m=PP_MACRO_EXPR SEMICOLON { mkleaf $startpos $endpos (L.MacroStmt m) }
   | f=foperator_assignment                             SEMICOLON { reloc $startpos $endpos f; f }
   | f=fexpr_lvalue EQ c=class_new                      SEMICOLON { mknode $startpos $endpos (L.stmt LS.BlockingAssignment) [f; c] }
   | f=fexpr_lvalue EQ d=dynamic_array_new              SEMICOLON { mknode $startpos $endpos (L.stmt LS.BlockingAssignment) [f; d] }
   | fexpr_lvalue LT_EQ nb_assign_postfix_opt delay_or_event_control_opt expr SEMICOLON { mknode $startpos $endpos (L.stmt LS.NonBlockingAssignment) ($1::($3 @ $4 @ [$5])) }

   | ASSIGN expr EQ     delay_or_event_control_opt expr SEMICOLON { mknode $startpos $endpos (L.stmt LS.Assign) ($2::($4 @ [$5])) }
   | DEASSIGN variable_lvalue                           SEMICOLON { mknode $startpos $endpos (L.stmt LS.Deassign) [$2] }
   | DEASSIGN id_SV_keyword                             SEMICOLON 
       { 
	 let idnd0 = mkleaf $startpos($2) $endpos($2) (L.IdSelect $2) in
	 let idnd = mknode $startpos($2) $endpos($2) (L.IdClassSel $2) [idnd0] in
	 mknode $startpos $endpos (L.stmt LS.Deassign) [idnd] 
       }

   | FORCE variable_lvalue EQ expr                                 SEMICOLON { mknode $startpos $endpos (L.stmt LS.Force) [$2; $4] }
   | RELEASE variable_lvalue                            SEMICOLON { mknode $startpos $endpos (L.stmt LS.Release) [$2] }
   | unique_priority_opt IF LPAREN expr RPAREN stmt_block %prec P_LOWER_THAN_ELSE { mknode $symbolstartpos $endpos (L.stmt LS.Conditional) ($1 @ [$4; $6]) }
   | unique_priority_opt IF LPAREN expr RPAREN stmt_block ELSE stmt_block         { mknode $symbolstartpos $endpos (L.stmt LS.Conditional) ($1 @ [$4; $6; $8]) }
   | finc_or_dec_expression                                              SEMICOLON { mknode $startpos $endpos (L.stmt LS.IncOrDec) [$1] }
   | VOID TICK LPAREN function_subroutine_call_no_method          RPAREN SEMICOLON { mknode $startpos $endpos (L.stmt LS.SubroutineCallVoid) [$4] }
   | VOID TICK LPAREN e=expr DOT f=function_subroutine_call_no_method RPAREN SEMICOLON 
       { 
         ignore (e, f);
	 reloc $startpos(e) $endpos(f) f; f#add_children_l [e]; f#relab (L.expr (LE.MethodCall f#get_identifier));
	 mknode $startpos $endpos (L.stmt LS.SubroutineCallVoid) [f]
       }
   | t=task_subroutine_call_no_method             SEMICOLON { mknode $startpos $endpos (L.stmt LS.SubroutineCall) [t] }
   | f=fexpr DOT a=array_method_no_root           SEMICOLON 
       {
	reloc $startpos $endpos(a) a; a#add_children_l [f];
	mknode $startpos $endpos (L.stmt LS.SubroutineCall) [a]
       }
   | f=fexpr DOT t=task_subroutine_call_no_method SEMICOLON 
       {
	reloc $startpos $endpos(t) t; t#add_children_l [f];
	mknode $startpos $endpos (L.stmt LS.SubroutineCall) [t]
       }
   | fexpr_scope                              SEMICOLON { mknode $startpos $endpos (L.stmt LS.SubroutineCall) [$1] }
   | f=fexpr DOT c=class_new                  SEMICOLON { reloc $startpos $endpos c; c#add_children_l [f]; c#expr_to_stmt; c }
   | DISABLE hierarchical_identifier          SEMICOLON { mknode $startpos $endpos (L.stmt LS.Disable) [$2] }
   | DISABLE fork_kw                          SEMICOLON { mkleaf $startpos $endpos (L.stmt LS.DisableFork) }
   | MINUS_GT                               hierarchical_identifier SEMICOLON { mknode $startpos $endpos (L.stmt LS.EventTrigger) [$2] }
   | MINUS_GT_GT delay_or_event_control_opt hierarchical_identifier SEMICOLON { mknode $startpos $endpos (L.stmt LS.EventTriggerNonBlocking) ($2 @ [$3]) }
   | FOREVER                   stmt_block { mknode $startpos $endpos (L.stmt LS.Forever) [$2] }
   | REPEAT LPAREN expr RPAREN stmt_block { mknode $startpos $endpos (L.stmt LS.Repeat) [$3; $5] }
   | WHILE  LPAREN expr RPAREN stmt_block { mknode $startpos $endpos (L.stmt LS.While) [$3; $5] }
   | FOR LPAREN for_initialization expr SEMICOLON for_step_opt RPAREN stmt_block { mknode $startpos $endpos (L.stmt LS.For) ([$3; $4] @ $6 @ [$8]) }
   | DO stmt_block WHILE LPAREN expr RPAREN SEMICOLON { mknode $startpos $endpos (L.stmt LS.Do) [$2; $5] }
   | FOREACH LPAREN id_class_foreach RPAREN stmt { mknode $startpos $endpos (L.stmt LS.Foreach) [$3; $5] }
   | RETURN      SEMICOLON { mkleaf $startpos $endpos (L.stmt LS.Return) }
   | RETURN expr SEMICOLON { mknode $startpos $endpos (L.stmt LS.Return) [$2] }
   | BREAK    SEMICOLON { mkleaf $startpos $endpos (L.stmt LS.Break) }
   | CONTINUE SEMICOLON { mkleaf $startpos $endpos (L.stmt LS.Continue) }
   | delay_control stmt_block { mknode $startpos $endpos (L.stmt LS.ProceduralTimingControl) [$1; $2] }
   | event_control stmt_block { mknode $startpos $endpos (L.stmt LS.ProceduralTimingControl) [$1; $2] }
   | cycle_delay stmt_block   { mknode $startpos $endpos (L.stmt LS.ProceduralTimingControl) [$1; $2] }
   | WAIT LPAREN expr RPAREN stmt_block { mknode $startpos $endpos (L.stmt LS.Wait) [$3; $5] } 
   | WAIT fork_kw SEMICOLON             { mkleaf $startpos $endpos (L.stmt LS.WaitFork) }
   | WAIT_ORDER LPAREN hierarchical_identifier_list RPAREN action_block { mknode $startpos $endpos (L.stmt LS.WaitOrder) ($3 @ [$5]) }
   | procedural_assertion_statement { $1 }
   | fexpr_lvalue LT_EQ cycle_delay expr SEMICOLON { mknode $startpos $endpos (L.stmt LS.ClockingDrive) [$1; $3; $4] }
   | randsequence_statement { $1 }
   | RANDCASE case_item_list ENDCASE { mknode $startpos $endpos (L.stmt LS.Randcase) $2 }
   | expect_property_statement { $1 }
;

statement_item_block:
   | u=unique_priority_opt cs=case_start ca=case_attr_opt         ci=case_item_list_opt    ENDCASE 
       { 
	 context_stack#pop;
	 let cc = mknode $startpos(ci) $endpos(ci) L.CaseItems ci in
	 reloc $symbolstartpos $endpos cs;
         cs#add_children_l u;
         cs#add_children_r (ca @ [cc]); 
	 cs
       }
   | case_pattern_head                                    case_pattern_list_opt  ENDCASE 
       { 
	 context_stack#pop;
	 let nd, st_pattern = $1 in
	 let cc = mknode st_pattern $endpos($2) L.CaseItemsMatches $2 in
	 reloc $startpos $endpos nd;
         nd#add_children_r [cc];
	 nd
       }
   | case_inside_head                                     case_inside_list_opt   ENDCASE 
       { 
	 context_stack#pop;
	 let nd, st_inside = $1 in
	 let cc = mknode st_inside $endpos($2) L.CaseItemsInside $2 in
	 reloc $startpos $endpos nd;
         nd#add_children_r [cc];
	 nd
       }

   | par_block                { $1 }
   | seq_block                { $1 }
;

statement_item_block_act:
   | s=statement_item_block_act_case ENDCASE { reloc $startpos $endpos s; s }
   | p=par_block_act                         { p }
   | s=seq_block_act                         { s }
;

statement_item_block_act_case:
   | u=unique_priority_opt c=case_start ca=case_attr_opt
       { 
	 context_stack#pop;
	 context_stack#activate_top_no_delay;
	 reloc $symbolstartpos $endpos c;
         c#add_children_l u;
         c#add_children_r ca;
	 c
       }
   | u=unique_priority_opt c=case_start ca=case_attr_opt         ci=case_item_list
       { 
	 context_stack#pop;
	 context_stack#activate_top_no_delay;
	 let cc = mknode $startpos(ci) $endpos(ci) L.CaseItems ci in
	 reloc $symbolstartpos $endpos c;
         c#add_children_l u;
         c#add_children_r (ca @ [cc]);
	 c
       }
   | c=case_pattern_head
       { 
	 context_stack#pop;
	 context_stack#activate_top_no_delay;
	 let nd, st_pattern = c in
	 reloc $startpos $endpos nd;
	 nd
       }
   | c=case_pattern_head                                    cp=case_item_list (* case_pattern_list *)
       { 
	 context_stack#pop;
	 context_stack#activate_top_no_delay;
	 let nd, st_pattern = c in
	 let cc = mknode st_pattern $endpos(cp) L.CaseItemsMatches cp in
	 reloc $startpos $endpos nd;
         nd#add_children_r [cc];
	 nd
       }
   | c=case_inside_head
       { 
	 context_stack#pop;
	 context_stack#activate_top_no_delay;
	 let nd, st_inside = c in
	 reloc $startpos $endpos nd;
	 nd
       }
   | c=case_inside_head                                     ci=case_inside_item_list (* case_inside_list *)
       { 
	 context_stack#pop;
	 context_stack#activate_top_no_delay;
	 let nd, st_inside = c in
	 let cc = mknode st_inside $endpos(ci) L.CaseItemsInside ci in
	 reloc $startpos $endpos nd;
         nd#add_children_r [cc];
	 nd
       }
;


case_pattern_head0:
   | u=unique_priority_opt c=case_start ca=case_attr_opt m=MATCHES
       { 
	 context_stack#pop;
	 context_stack#push (Context.case_item_list());
	 reloc $symbolstartpos $endpos c;
	 c#add_children_l u;
	 c#add_children_r ca;
         ignore m;
	 c, $startpos(m)
       }
;

case_pattern_head:
   | c=case_pattern_head0 MATCHES_ { let nd, st_pattern = c in reloc $startpos $endpos nd; nd, st_pattern }
;

case_inside_head0:
   | u=unique_priority_opt c=case_start ca=case_attr_opt i=INSIDE
       { 
         context_stack#pop;
         context_stack#push (Context.case_inside_item_list());
         reloc $symbolstartpos $endpos c;
         c#add_children_l u;
         c#add_children_r ca;
         ignore i;
         c, $startpos(i)
       }
;

case_inside_head:
   | c=case_inside_head0 INSIDE_ { let nd, st_inside = c in reloc $startpos $endpos nd; nd, st_inside }
;


operator_assignment:
   | e0=expr_lvalue EQ el=delay_or_event_control_opt e2=expr { mknode $startpos $endpos (L.stmt_ao LAO.Eq) ((e0::el) @ [e2]) }
   | e0=expr_lvalue a=__aop e1=expr { mknode $startpos $endpos (L.stmt_ao a) [e0; e1] }
;

%inline
fexpr_lvalue_svkw:
   | l=fexpr_lvalue  { l }
   | i=id_SV_keyword { mknode $startpos $endpos (L.IdClassSel i) [mkleaf $startpos $endpos (L.IdSelect i)] }
;

foperator_assignment:
   | l=fexpr_lvalue_svkw EQ d=delay_or_event_control_opt e=expr { mknode $startpos $endpos (L.stmt_ao LAO.Eq) ((l::d) @ [e]) }
   | l=fexpr_lvalue_svkw a=__aop e=expr { mknode $startpos $endpos (L.stmt_ao a) [l; e] }
;

inc_or_dec_expression_non_mintypmax:
   | expr_non_mintypmax_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | expr_non_mintypmax_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostDecr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

inc_or_dec_expression:
   | expr_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | expr_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

finc_or_dec_expression:
   | fexpr_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | fexpr_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostDecr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

sinc_or_dec_expression:
   | sexpr_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | sexpr_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostDecr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

pinc_or_dec_expression:
   | pexpr_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | pexpr_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostDecr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

ev_inc_or_dec_expression:
   | ev_expr_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | ev_expr_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostDecr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

pev_inc_or_dec_expression:
   | pev_expr_scope PLUS_PLUS   { mknode $startpos $endpos (L.expr LE.PostIncr) [$1] }
   | pev_expr_scope MINUS_MINUS { mknode $startpos $endpos (L.expr LE.PostDecr) [$1] }
   | PLUS_PLUS expr   { mknode $startpos $endpos (L.expr LE.PreIncr) [$2] }
   | MINUS_MINUS expr { mknode $startpos $endpos (L.expr LE.PreDecr) [$2] }
;

class_new:
   | NEW                                    { mkleaf $startpos $endpos (L.expr LE.ClassNew) }
   | NEW expr                               { mknode $startpos $endpos (L.expr LE.ClassNew) [$2] }
   | NEW__P LPAREN argument_list_opt RPAREN { mknode $startpos $endpos (L.expr LE.ClassNewA) $3 }
 ;

dynamic_array_new:
   | NEW LBRACKET expr RBRACKET                    { mknode $startpos $endpos L.DynamicArrayNew [$3] }
   | NEW LBRACKET expr RBRACKET LPAREN expr RPAREN { mknode $startpos $endpos L.DynamicArrayNew [$3; $6] }
;

unique_priority_opt:
   | (* empty *) { [] }
   | PRIORITY    { [mkleaf $startpos $endpos L.Priority] }
   | UNIQUE      { [mkleaf $startpos $endpos L.Unique] }
   | UNIQUE0     { [mkleaf $startpos $endpos L.Unique0] }
;

action_block:
   | stmt %prec P_LOWER_THAN_ELSE { $1 }
   | stmt ELSE stmt               { mknode $startpos $endpos L.ActionBlock [$1; $3] }
   |      ELSE stmt               { mknode $startpos $endpos L.ActionBlock [$2] }
;

case_start:
   | c=case_start0 RPAREN { reloc $startpos $endpos c; c }
;

case_start0:
   | CASE  LPAREN expr { context_stack#push (Context.case_item_list()); mknode $startpos $endpos (L.stmt LS.Case) [$3] }
   | CASEX LPAREN expr { context_stack#push (Context.case_item_list()); mknode $startpos $endpos (L.stmt LS.Casex) [$3] }
   | CASEZ LPAREN expr { context_stack#push (Context.case_item_list()); mknode $startpos $endpos (L.stmt LS.Casez) [$3] }
;

case_attr_opt:
   | (* empty *) { [] }
;

case_pattern_list_opt:
   | c=case_item_list_opt { c }
;

%inline
case_item_list_opt:
   | (* empty *)      { [] }
   | c=case_item_list { c }
;

case_inside_list_opt:
   | (* empty *)           { [] }
   | case_inside_item_list { $1 }
;

partial_case_item_list:
   | case_item_list EOP { Ast.Pcase_item_list $1 }
   |                EOP { Ast.Pcase_item_list [] }

case_item_list:
   |                case_item { context_stack#activate_top; [$1] }
   | case_item_list case_item { context_stack#activate_top; $1 @ [$2] }
;

case_item:
   | case_cond_list COLON stmt_block { mknode $startpos $endpos L.CaseItem ($1 @ [$3]) }
   | DEFAULT        COLON stmt_block { mknode $startpos $endpos L.CaseItemDefault [$3] }
   | DEFAULT              stmt_block { mknode $startpos $endpos L.CaseItemDefault [$2] }
   | CASE_ITEM                       { check_error $1; $1 }
   | compiler_directive { $1 }
;

partial_case_inside_item_list:
   | case_inside_item_list EOP { Ast.Pcase_inside_item_list $1 }
   |                       EOP { Ast.Pcase_inside_item_list [] }
;

case_inside_item_list:
   |                       case_inside_item { context_stack#activate_top; [$1] }
   | case_inside_item_list case_inside_item { context_stack#activate_top; $1 @ [$2] }
;

case_inside_item:
   | open_range_list COLON stmt_block { mknode $startpos $endpos L.CaseInsideItem ($1 @ [$3]) }
   | DEFAULT         COLON stmt_block { mknode $startpos $endpos L.CaseInsideItemDefault [$3] }
   | DEFAULT               stmt_block { mknode $startpos $endpos L.CaseInsideItemDefault [$2] }
   | CASE_INSIDE_ITEM                 { check_error $1; $1 }
   | compiler_directive { $1 }
;


open_range_list:
   |                       open_value_range { [$1] }
   | open_range_list COMMA open_value_range { $1 @ [$3] }
;

open_value_range:
   | value_range { $1 }
;

value_range:
   | expr                              { mknode $startpos $endpos L.ValueRange [$1] }
   | LBRACKET expr COLON expr RBRACKET { mknode $startpos $endpos L.ValueRange [$2; $4] }
;

case_cond_list:
   | case_cond_list_ { [mknode $startpos $endpos L.CaseConds $1] }
;

case_cond_list_:
   |                       expr { [$1] }
   | case_cond_list_ COMMA expr { $1 @ [$3] }
;

pattern_no_expr:
   | DOT id                    { mkleaf $startpos $endpos (L.PatternId $2) }
   | DOT_STAR                  { mkleaf $startpos $endpos L.PatternStar }
   | TAGGED id pattern_no_expr { mknode $startpos $endpos (L.PatternTagged $2) [$3] }
;

pattern_list:
   |                    pattern { [$1] }
   | pattern_list COMMA pattern { $1 @ [$3] }
;

pattern:
   | expr                              { mknode $startpos $endpos L.Pattern [$1] }
   | expr LBRACE args_expr_list RBRACE { mknode $startpos $endpos L.Pattern ($1::$3) }
   | pattern_no_expr                   { $1 }
;

pattern_member_list:
   |                           pattern_key COLON expr            { [mknode $startpos $endpos L.PatternMember [$1; $3]] }
   |                           pattern_key COLON pattern_no_expr { [mknode $startpos $endpos L.PatternMember [$1; $3]] }
   | pattern_member_list COMMA pattern_key COLON expr            { $1 @ [mknode $startpos($3) $endpos($5) L.PatternMember [$3; $5]] }
   | pattern_member_list COMMA pattern_key COLON pattern_no_expr { $1 @ [mknode $startpos($3) $endpos($5) L.PatternMember [$3; $5]] }
;

pattern_key:
   | const_expr  { mknode $startpos $endpos L.PatternKey [$1] }
   | DEFAULT     { mkleaf $startpos $endpos L.PatternKeyDefault }
   | simple_type { mknode $startpos $endpos L.PatternKey [$1] }
;

assignment_pattern:
   | TICK_LBRACE pattern_list        RBRACE { mknode $startpos $endpos L.AssignmentPattern $2 }
   | TICK_LBRACE pattern_member_list RBRACE { mknode $startpos $endpos L.AssignmentPattern $2 }
   | TICK_LBRACE                     RBRACE { mkleaf $startpos $endpos L.AssignmentPattern }
;

for_initialization:
   | for_initialization_item_list SEMICOLON { mknode $startpos $endpos L.ForInit $1 }
;

for_initialization_item_list:
   |                                    for_initialization_item { [$1] }
   | for_initialization_item_list COMMA for_initialization_item { $1 @ [$3] }
;

for_initialization_item:
   | data_type id_any EQ expr { mknode $startpos $endpos (L.ForInitItemDT $2) [$1; $4] }
   | variable_lvalue  EQ expr { mknode $startpos $endpos L.ForInitItemLval [$1; $3] }
;

for_step_opt:
   | (* empty *) { [] }
   | for_step    { $1 }
;

for_step:
   | for_step_assignment                { [$1] }
   | for_step COMMA for_step_assignment { $1 @ [$3] }
;

for_step_assignment:
   | o=operator_assignment                { o }
   | i=inc_or_dec_expression              { i }
   | f=function_subroutine_call_no_method { f }
   | e=expr DOT a=array_method_no_root    { reloc $startpos $endpos a; a#add_children_l [e]; a }
   | e=expr_scope                         { e }
;

loop_variable_list:
   |                          id { [mkleaf $startpos $endpos (L.Variable $1)] }
   | loop_variable_list COMMA id { $1 @ [mkleaf $startpos $endpos (L.Variable $3)] }
;

func_ref:
   | id                          LPAREN pev_argument_list_opt RPAREN 
       { 
	 mknode $startpos $endpos (L.expr (LE.TfCall $1)) [mknode $startpos($2) $endpos($4) L.Args $3] 
       }
   | package_scope_id_follows id LPAREN pev_argument_list_opt RPAREN 
       { 
	 mknode $startpos $endpos (L.expr (LE.TfCall $2)) [$1; mknode $startpos($3) $endpos($5) L.Args $4]
       }
   | class_scope_id_follows   id LPAREN pev_argument_list_opt RPAREN 
       { 
	 mknode $symbolstartpos $endpos (L.expr (LE.TfCall $2)) ($1 @ [mknode $startpos($3) $endpos($5) L.Args $4])
       }
;

task_subroutine_call_no_method:
   | f=func_ref                                  { f }
   | f=func_ref w=WITH__P LPAREN e=expr r=RPAREN { ignore (w, r); f#add_children_r [mknode $startpos(w) $endpos(r) L.With [e]]; f }
   | s=system_t_call                             { s }
   | f=func_ref w=WITH__C c=constraint_block     { ignore w; f#add_children_r [mknode $startpos(w) $endpos(c) L.With [c]]; f }
;

function_subroutine_call_no_method:
   | f=func_ref                                  { f }
   | f=func_ref w=WITH__P LPAREN e=expr r=RPAREN { ignore (w, r); f#add_children_r [mknode $startpos(w) $endpos(r) L.With [e]]; f }
   | s=system_f_call                             { s }
   | f=func_ref w=WITH__C c=constraint_block     { ignore w; f#add_children_r [mknode $startpos(w) $endpos(c) L.With [c]]; f }
;

system_t_call:
   | system_f_call { $1 }
;

systask:
  | ST_ERROR     { LST.Error }
  | ST_FATAL     { LST.Fatal }
  | ST_INFO      { LST.Info }
  | ST_ROOT      { LST.Root }
  | ST_UNIT      { LST.Unit }
  | ST_WARNING   { LST.Warning }
;

system_f_call:
   | s=SYSCALL p=paren_opt                            { mknode $startpos $endpos (L.expr (LE.SystemFCall s)) p }
   | s=SYSCALL l=LPAREN e=expr_or_data_type_list r=RPAREN 
       { ignore (l, r); mknode $startpos $endpos (L.expr (LE.SystemFCall s)) [mknode $startpos(l) $endpos(r) L.Args e] }

   | s=systask p=paren_opt                            { mknode $startpos $endpos (L.expr (LE.SystemTCall s)) p }
   | s=systask l=LPAREN e=expr_or_data_type_list r=RPAREN 
       { ignore (l, r); mknode $startpos $endpos (L.expr (LE.SystemTCall s)) [mknode $startpos(l) $endpos(r) L.Args e] }
;

elaboration_system_task:
   | s=systask p=paren_opt                            SEMICOLON { mknode $startpos $endpos (L.ElaborationSystemTask s) p }
   | s=systask l=LPAREN e=expr_or_data_type_list r=RPAREN SEMICOLON 
       { ignore (l, r); mknode $startpos $endpos (L.ElaborationSystemTask s) [mknode $startpos(l) $endpos(r) L.Args e] }
;

property_actual_arg:
   | p=pev_expr { p }
;

task_kw:
   | TASK     { }
   | TASK__PV { }
;

task_declaration:
   | TASK     l=lifetime_opt t=task_id tf=tf_part ENDTASK e=end_label_opt 
       { mknode $startpos $endpos (L.TaskDeclaration t#get_identifier) (l @ (t::tf) @ e) }
   | TASK__PV l=lifetime_opt t=task_id tf=tf_part_pure_v
       { mknode $startpos $endpos (L.TaskDeclaration t#get_identifier) (l @ (t::tf)) }
;

task_prototype:
   | task_kw t=task_id LPAREN tl=tf_port_list_opt RPAREN { mknode $startpos $endpos (L.TaskPrototype t#get_identifier) (t::tl) }
   | task_kw t=task_id                                   { mknode $startpos $endpos (L.TaskPrototype t#get_identifier) [t] }
;

function_kw:
   | FUNCTION     { }
   | FUNCTION__PV { }
;

function_head:
  | FUNCTION l=lifetime_opt f=func_id 
      { 
	let id = f#get_identifier in
	begin_function_scope id; 
	import_pkg_cls_scope (get_scope_of_func_id f);
	l, f, id 
      }
;

function_head_new:
  | FUNCTION l=lifetime_opt f=func_id_new 
      { 
	begin_function_scope "new";
	import_pkg_cls_scope (get_scope_of_func_id f);
	l, f
      }
;

function_declaration:
   | f=function_head                         t=tf_part ENDFUNCTION e=end_label_opt
       { 
	 end_scope();
	 let l, fid, id = f in
         mknode $startpos $endpos (L.FunctionDeclaration id) (l @ (fid::t) @ e)
       }
   | f=function_head_new                     t=tf_part ENDFUNCTION e=end_label_opt
       { 
	 end_scope();
	 let l, fid = f in
         mknode $startpos $endpos (L.FunctionDeclaration "new") (l @ (fid::t) @ e)
       }
   | FUNCTION__PV l=lifetime_opt f=func_id     t=tf_part_pure_v { mknode $startpos $endpos (L.FunctionDeclaration f#get_identifier) (l @ (f::t)) }
   | FUNCTION__PV l=lifetime_opt f=func_id_new t=tf_part_pure_v { mknode $startpos $endpos (L.FunctionDeclaration "new") (l @ (f::t)) }
;

function_prototype:
   | function_kw f=func_id LPAREN tl=tf_port_list_opt RPAREN
       { 
         mknode $startpos $endpos (L.FunctionPrototype f#get_identifier) (f::tl)
       }
   | function_kw f=func_id
       { 
         mknode $startpos $endpos (L.FunctionPrototype f#get_identifier) [f]
       }
;

class_constructor_prototype:
   | function_kw func_id_new LPAREN tf_port_list_opt RPAREN SEMICOLON { mknode $startpos $endpos (L.ClassCtorPrototype) ($2::$4) }
   | function_kw func_id_new                                SEMICOLON { mknode $startpos $endpos (L.ClassCtorPrototype) [$2] }
;

method_prototype:
   | t=task_prototype     { t }
   | f=function_prototype { f }
;

%inline
lifetime_opt:
   | (* empty *) { [] }
   | l=lifetime  { [l] }
;

lifetime:
   | STATIC    { mkleaf $startpos $endpos L.LifetimeStatic }
   | AUTOMATIC { mkleaf $startpos $endpos L.LifetimeAutomatic }
;

task_id:
   | t=tf_id_scoped { t }
;

func_id:
   |                            t=tf_id_scoped { mknode $startpos $endpos (L.FuncId t#get_identifier) [t] }
   | s=signing_opt r=range_list t=tf_id_scoped 
       { 
         let id = t#get_identifier in
         mknode $symbolstartpos $endpos (L.FuncId id) (s @ r @ [t])
       }
   | s=signing   t=tf_id_scoped { mknode $startpos $endpos (L.FuncId t#get_identifier) [s; t] }
   | VOID        t=tf_id_scoped { mknode $startpos $endpos (L.FuncIdVoid t#get_identifier) [t] }
   | d=data_type t=tf_id_scoped { mknode $startpos $endpos (L.FuncId t#get_identifier) [d; t] }
;

func_id_new:
   | NEW                           { mkleaf $startpos $endpos L.FuncIdNew }
   | NEW__P                        { mkleaf $startpos $endpos L.FuncIdNew }
   | class_scope_without_id NEW__P { mknode $startpos $endpos L.FuncIdNew $1 }
;

tf_id_scoped:
   |        id      { mkleaf $startpos $endpos (L.TfIdScoped $1) }
   | id DOT id      { mknode $startpos $endpos (L.TfIdScoped $3) [mkleaf $startpos $endpos($1) (L.InterfaceIdentifier $1)] }
   | class_scope_id { $1 }
;

tf_part:
   | t=tf_port_list_part tb=tf_body_opt { t::tb }
;

tf_port_list_part:
   | LPAREN tl=tf_port_list_opt RPAREN SEMICOLON { mknode $startpos $endpos L.TfPortListPart tl }
   |                                   SEMICOLON { mkleaf $startpos $endpos L.TfPortListPart }
;

tf_part_pure_v:
   | LPAREN tl=tf_port_list_opt RPAREN SEMICOLON { [mknode $startpos $endpos L.TfPortListPart tl] }
   |                                   SEMICOLON { [mkleaf $startpos $endpos L.TfPortListPart] }
;

%inline
tf_body_opt:
   | (* empty *)                              { [] }
   | tl=tf_item_declaration_list              { [mknode $startpos $endpos L.TfBody tl] }
   | tl=tf_item_declaration_list sl=stmt_list { [mknode $startpos $endpos L.TfBody (tl@sl)] }
   |                             sl=stmt_list { [mknode $startpos $endpos L.TfBody sl] }
;

function_data_type:
   | VOID      { mkleaf $startpos $endpos L.Void }
   | data_type { $1 }
;

tf_item_declaration_list:
   |                          tf_item_declaration { [$1] }
   | tf_item_declaration_list tf_item_declaration { $1 @ [$2] }
;

tf_item_declaration:
   | block_item_declaration { $1 }
   | tf_port_declaration    { $1 }
;

tf_port_list_opt:
   | tf_port_list_list { $1 }
;

tf_port_list_list:
   |                         tf_port_item { $1 }
   | tf_port_list_list COMMA tf_port_item { $1 @ $3 }
;

tf_port_item:
   | (* empty *)                                   { [] }
   | h=tf_port_item_head t=tf_port_item_assignment { [mknode $startpos $endpos L.TfPortItem (h @ [t])] }
   |                     t=tf_port_item_assignment { [mknode $startpos $endpos L.TfPortItem [t]] }
;

tf_port_item_head:
   |                          d=data_type           { [d] }
   |                          i=_implicit_data_type { [i] }
   | t=tf_port_item_dir                             { [t] }
   | t=tf_port_item_dir       d=data_type           { [t; d] }
   | t=tf_port_item_dir       i=_implicit_data_type { [t; i] }
   |                    v=var d=data_type           { [v; d] }
   |                    v=var i=implicit_data_type  { (v::i) }
   | t=tf_port_item_dir v=var d=data_type           { [t; v; d] }
   | t=tf_port_item_dir v=var i=implicit_data_type  { (t::v::i) }
;

tf_port_item_dir:
   | p=port_direction { p }
;

tf_port_item_assignment:
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt           { mknode $startpos $endpos (L.TfPortItemAssignment i) (v @ s) }
   | i=id v=variable_dimension_list_opt s=sig_attr_list_opt EQ e=expr { mknode $startpos $endpos (L.TfPortItemAssignment i) (v @ s @ [e])}
;

%inline
paren_opt:
   | (* empty *)   { [] }
   | LPAREN RPAREN { [mkleaf $startpos $endpos L.Args] }
;

array_method_no_root:
   | a=array_method_name_no_id                                        m=method_call_with_opt { mknode $startpos $endpos (L.Expr a) m }
   | a=array_method_name_no_id l=LPAREN al=argument_list_opt r=RPAREN m=method_call_with_opt 
       { 
	 ignore (l, r);
	 mknode $startpos $endpos (L.Expr a) ((mknode $startpos(l) $endpos(r) L.Args al)::m) 
       }
;

%inline
method_call_with_opt:
   | (* empty *)                  { [] }
   | WITH__P LPAREN e=expr RPAREN { [mknode $startpos $endpos L.With [e]] }
;

array_method_name_no_id:
   | UNIQUE { LE.ArrayMethodCallUnique }
   | AND    { LE.ArrayMethodCallAnd }
   | OR     { LE.ArrayMethodCallOr }
   | XOR    { LE.ArrayMethodCallXor }
;

dpi_import_export:
   | IMPORT string_literal dpi_tf_import_property_opt dpi_import_label_opt function_prototype SEMICOLON { mknode $startpos $endpos (L.DpiImport $2) ($3 @ $4 @ [$5]) }
   | IMPORT string_literal dpi_tf_import_property_opt dpi_import_label_opt task_prototype     SEMICOLON { mknode $startpos $endpos (L.DpiImport $2) ($3 @ $4 @ [$5]) }
   | EXPORT string_literal                            dpi_import_label_opt function_kw id_any SEMICOLON { mknode $startpos $endpos (L.DpiExportFunc($2, $5)) $3 }
   | EXPORT string_literal                            dpi_import_label_opt task_kw     id_any SEMICOLON { mknode $startpos $endpos (L.DpiExportTask($2, $5)) $3 }
;

dpi_import_label_opt:
   | (* empty *) { [] }
   | id_any EQ   { [mkleaf $startpos $endpos (L.DpiImportLabel $1)] }
;

dpi_tf_import_property_opt:
   | (* empty *) { [] }
   | CONTEXT     { [mkleaf $startpos $endpos L.DpiTfImportPropertyContext] }
   | PURE        { [mkleaf $startpos $endpos L.DpiTfImportPropertyPure] }
;

overload_declaration:
   | BIND overload_operator function_kw data_type id_any LPAREN overload_proto_formal_list RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos (L.OverloadDeclaration($2, $5)) [$4; mknode $startpos($6) $endpos($8) L.Params $7]
       }
;

overload_operator:
   | PLUS        { LOO.Add }
   | PLUS_PLUS   { LOO.Incr }
   | MINUS       { LOO.Subt }
   | MINUS_MINUS { LOO.Decr }
   | STAR        { LOO.Mult }
   | STAR_STAR   { LOO.Pow }
   | SLASH       { LOO.Div }
   | PERCENT     { LOO.Mod }
   | EQ_EQ       { LOO.Eq }
   | EXCLAM_EQ   { LOO.Neq }
   | LT          { LOO.Lt }
   | LT_EQ       { LOO.Le }
   | GT          { LOO.Gt }
   | GT_EQ       { LOO.Ge }
   | EQ          { LOO.Assign }
;

overload_proto_formal_list:
   |                                  data_type { [$1] }
   | overload_proto_formal_list COMMA data_type { $1 @ [$3] }
;

partial_expr:
   | e=_expr EOP { Ast.Pexpr e }
;

partial_pev_expr:
   | e=_pev_expr EOP { Ast.Pexpr e }
;

partial_ev_expr:
   | e=_ev_expr EOP { Ast.Pexpr e }
;

const_expr:
   | e=expr { e }
;

%inline
__aop:
   | PLUS_EQ     { LAO.AddEq }
   | MINUS_EQ    { LAO.SubtEq }
   | STAR_EQ     { LAO.MultEq }
   | SLASH_EQ    { LAO.DivEq }
   | PERCENT_EQ  { LAO.ModEq }
   | AMP_EQ      { LAO.AndEq }
   | PIPE_EQ     { LAO.OrEq }
   | HAT_EQ      { LAO.XorEq }
   | LT_LT_EQ    { LAO.ShiftLEq }
   | GT_GT_EQ    { LAO.ShiftREq }
   | GT_GT_GT_EQ { LAO.SShiftREq }
;

%inline
_aop:
   | EQ      { LAO.Eq }
   | a=__aop { a }
;

%inline
__bop:
   | PLUS               { L.expr_bo LBO.Add }
   | MINUS              { L.expr_bo LBO.Subt }
   | STAR               { L.expr_bo LBO.Mult }
   | SLASH              { L.expr_bo LBO.Div }
   | PERCENT            { L.expr_bo LBO.Mod }
   | EQ_EQ              { L.expr_bo LBO.Eq }
   | EXCLAM_EQ          { L.expr_bo LBO.Neq }
   | EQ_EQ_EQ           { L.expr_bo LBO.CaseEq }
   | EXCLAM_EQ_EQ       { L.expr_bo LBO.CaseNeq }
   | EQ_EQ_QUESTION     { L.expr_bo LBO.WildEq }
   | EXCLAM_EQ_QUESTION { L.expr_bo LBO.WildNeq }
   | AMP_AMP            { L.expr_bo LBO.LogAnd }
   | PIPE_PIPE          { L.expr_bo LBO.LogOr }
   | STAR_STAR          { L.expr_bo LBO.Pow }
   | LT                 { L.expr_bo LBO.Lt }
   | GT                 { L.expr_bo LBO.Gt }
   | GT_EQ              { L.expr_bo LBO.Ge }
   | AMP                { L.expr_bo LBO.And }
   | PIPE               { L.expr_bo LBO.Or }
   | HAT                { L.expr_bo LBO.Xor }
   | HAT_TILDE          { L.expr_bo LBO.Xnor }
   | TILDE_PIPE         { L.expr_bo LBO.Nor }
   | TILDE_AMP          { L.expr_bo LBO.Nand }
   | LT_LT              { L.expr_bo LBO.ShiftL }
   | GT_GT              { L.expr_bo LBO.ShiftR }
   | GT_GT_GT           { L.expr_bo LBO.SShiftR }
   | LT_MINUS_GT        { L.expr_bo LBO.LtMinusGt }
;

%inline
_bop:
   | b=__bop { b }
   | LT_EQ   { L.expr_bo LBO.Le }
;

expr_non_mintypmax:
   | PLUS       expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [$2] }
   | MINUS      expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [$2] }
   | EXCLAM     expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [$2] }
   | AMP        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [$2] }
   | TILDE      expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [$2] }
   | PIPE       expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [$2] }
   | HAT        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [$2] }
   | TILDE_AMP  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [$2] }
   | TILDE_PIPE expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [$2] }
   | HAT_TILDE  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [$2] }
   | inc_or_dec_expression_non_mintypmax               { $1 }
   | e0=expr_non_mintypmax b=_bop e1=expr { mknode $startpos $endpos b [e0; e1] }
   | expr_non_mintypmax MINUS_GT constraint_set { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [$1; $3] }
   | expr_non_mintypmax QUESTION           expr COLON expr   { mknode $startpos $endpos (L.expr LE.Cond) [$1; $3; $5] }
   | expr_non_mintypmax inside_kw LBRACE open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) ($1::$4) }
   | TAGGED id %prec P_TAGGED { mkleaf $startpos $endpos (L.expr (LE.Tagged $2)) }
   | TAGGED id expr %prec P_TAGGED { mknode $startpos $endpos (L.expr (LE.Tagged $2)) [$3] }
   | integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber $1)) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber $1)) }
   | str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) ($2::$4) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr RBRACKET                  
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($9) L.ArrayRange [$8]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRange [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangeMinus [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangePlus [$8; $10]]) 
       }
   | function_subroutine_call_no_method          { $1 }
   | e=expr_non_mintypmax DOT f=function_subroutine_call_no_method
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [e];
	 f
       }
   | e=expr_non_mintypmax DOT a=array_method_no_root               { reloc $startpos $endpos a; a#add_children_l [e]; a }
   | UNDERSCORE LPAREN state_push expr state_pop RPAREN { dummy_node }
   | casting_type TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | expr_non_mintypmax         TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | expr_non_mintypmax_ok_lvalue { $1 }
   | expr_non_mintypmax AMP_AMP_AMP expr        { mknode $startpos $endpos L.CondPredicate [$1; $3] }
   | expr_non_mintypmax MATCHES MATCHES_ pattern_no_expr { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | expr_non_mintypmax MATCHES MATCHES_ expr            { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | expr_non_mintypmax DIST LBRACE dist_list RBRACE { mknode $startpos $endpos L.Dist $4 }
;

expr:
   | e=_expr         { e }
   | e=EXPR          { check_error e; e }
   | e=PP_MACRO_EXPR { mkleaf $startpos $endpos (L.MacroExpr e) }
;

_expr:
   | PLUS       expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [$2] }
   | MINUS      expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [$2] }
   | EXCLAM     expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [$2] }
   | AMP        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [$2] }
   | TILDE      expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [$2] }
   | PIPE       expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [$2] }
   | HAT        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [$2] }
   | TILDE_AMP  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [$2] }
   | TILDE_PIPE expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [$2] }
   | HAT_TILDE  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [$2] }
   | inc_or_dec_expression               { $1 }
   | LPAREN e0=expr_scope a=_aop e1=expr RPAREN { mknode $startpos $endpos (L.expr_ao a) [e0; e1] }
   | e0=expr b=_bop e1=expr { mknode $startpos $endpos b [e0; e1] }
   | expr MINUS_GT constraint_set { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [$1; $3] }
   | expr QUESTION           expr COLON expr   { mknode $startpos $endpos (L.expr LE.Cond) [$1; $3; $5] }
   | expr inside_kw LBRACE open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) ($1::$4) }
   | TAGGED id %prec P_TAGGED { mkleaf $startpos $endpos (L.expr (LE.Tagged $2)) }
   | TAGGED id expr %prec P_TAGGED { mknode $startpos $endpos (L.expr (LE.Tagged $2)) [$3] }
   | integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber $1)) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber $1)) }
   | str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) ($2::$4) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr RBRACKET                  
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($9) L.ArrayRange [$8]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRange [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangeMinus [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangePlus [$8; $10]]) 
       }
   | function_subroutine_call_no_method          { $1 }
   | e=expr DOT f=function_subroutine_call_no_method 
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [e];
	 f
       }
   | e=expr DOT a=array_method_no_root               { reloc $startpos $endpos a; a#add_children_l [e]; a }
   | LPAREN expr                       RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$2] }
   | LPAREN expr COLON expr COLON expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$2; $4; $6] }
   | UNDERSCORE LPAREN state_push expr state_pop RPAREN { dummy_node }
   | casting_type TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | expr         TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | expr_ok_lvalue { $1 }
   | expr AMP_AMP_AMP expr        { mknode $startpos $endpos L.CondPredicate [$1; $3] }
   | expr MATCHES MATCHES_ pattern_no_expr { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | expr MATCHES MATCHES_ expr            { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | expr DIST LBRACE dist_list RBRACE { mknode $startpos $endpos L.Dist $4 }
;

fexpr: (* for use as leading part of statement *)
(* from expr *)
   | PLUS       fexpr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [$2] }
   | MINUS      fexpr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [$2] }
   | EXCLAM     fexpr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [$2] }
   | AMP        fexpr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [$2] }
   | TILDE      fexpr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [$2] }
   | PIPE       fexpr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [$2] }
   | HAT        fexpr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [$2] }
   | TILDE_AMP  fexpr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [$2] }
   | TILDE_PIPE fexpr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [$2] }
   | HAT_TILDE  fexpr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [$2] }
   | finc_or_dec_expression { $1 }
   | LPAREN e0=expr_scope a=_aop e1=expr RPAREN { mknode $startpos $endpos (L.expr_ao a) [e0; e1] }
   | f=fexpr b=__bop e=expr { mknode $startpos $endpos b [f; e] }
   | fexpr LT_EQ__IGN         fexpr { mknode $startpos $endpos (L.expr_bo LBO.Le) [$1; $3] }
   | fexpr MINUS_GT constraint_set { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [$1; $3] }
   | fexpr QUESTION fexpr COLON fexpr { mknode $startpos $endpos (L.expr LE.Cond) [$1; $3; $5] }
   | fexpr inside_kw LBRACE open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) ($1::$4) }
   | TAGGED id %prec P_TAGGED { mkleaf $startpos $endpos (L.expr (LE.Tagged $2)) }
   | TAGGED id expr %prec P_TAGGED { mknode $startpos $endpos (L.expr (LE.Tagged $2)) [$3] }
   | integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber $1)) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber $1)) }
   | str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) ($2::$4) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($9) L.ArrayRange [$8]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRange [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangeMinus [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangePlus [$8; $10]]) 
       }
   | function_subroutine_call_no_method           { $1 }
   | e=fexpr DOT f=function_subroutine_call_no_method 
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [e];
	 f
       }
   | f=fexpr DOT a=array_method_no_root               { reloc $startpos $endpos a; a#add_children_l [f]; a }
   | LPAREN expr                       RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$2] }
   | LPAREN expr COLON expr COLON expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$2; $4; $6] }
   | UNDERSCORE LPAREN state_push expr state_pop RPAREN { dummy_node }
   | casting_type TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | fexpr        TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | fexpr_ok_lvalue { $1 }
   | fexpr AMP_AMP_AMP fexpr            { mknode $startpos $endpos L.CondPredicate [$1; $3] }
   | fexpr MATCHES MATCHES_     pattern_no_expr  { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | fexpr MATCHES MATCHES_     fexpr            { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | fexpr DIST LBRACE dist_list RBRACE { mknode $startpos $endpos L.Dist $4 }
   | e=PP_MACRO_EXPR { mkleaf $startpos $endpos (L.MacroExpr e) }
;

ev_expr:
   | e=_ev_expr      { e }
   | e=EXPR          { check_error e; e }
   | e=PP_MACRO_EXPR { mkleaf $startpos $endpos (L.MacroExpr e) }
;

_ev_expr: (* event_expression *)
   | s=senitem_edge { s }
   | e0=ev_expr IFF e1=expr   { mknode $startpos $endpos (L.ev_expr LEE.Iff) [e0; e1] }
   | e0=ev_expr OR e1=ev_expr { mknode $startpos $endpos (L.ev_expr LEE.Or) [e0; e1] }
(* from expr *)
   | PLUS       e=ev_expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [e] }
   | MINUS      e=ev_expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [e] }
   | EXCLAM     e=ev_expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [e] }
   | AMP        e=ev_expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [e] }
   | TILDE      e=ev_expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [e] }
   | PIPE       e=ev_expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [e] }
   | HAT        e=ev_expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [e] }
   | TILDE_AMP  e=ev_expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [e] }
   | TILDE_PIPE e=ev_expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [e] }
   | HAT_TILDE  e=ev_expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [e] }
   | e=ev_inc_or_dec_expression { e }
   | LPAREN e0=ev_expr_scope a=_aop e1=expr RPAREN { mknode $startpos $endpos (L.expr_ao a) [e0; e1] }
   | e0=ev_expr b=_bop e1=ev_expr { mknode $startpos $endpos b [e0; e1] }
   | e=ev_expr  MINUS_GT c=constraint_set                 { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [e; c] }
   | e0=ev_expr QUESTION e1=ev_expr COLON e2=ev_expr      { mknode $startpos $endpos (L.expr LE.Cond) [e0; e1; e2] }
   | e=ev_expr inside_kw LBRACE ol=open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) (e::ol) }
   | TAGGED i=id        %prec P_TAGGED                    { mkleaf $startpos $endpos (L.expr (LE.Tagged i)) }
   | TAGGED i=id e=expr %prec P_TAGGED                    { mknode $startpos $endpos (L.expr (LE.Tagged i)) [e] }
   | i=integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber i)) }
   | r=REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber r)) }
   | t=TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber t)) }
   | s=str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber s)) }
   | LBRACE                                  RBRACE        { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE c=const_expr LBRACE cl=cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) (c::cl) }
   | LBRACE c=const_expr LBRACE cl=cate_list RBRACE RBRACE l=LBRACKET e=expr                  r=RBRACKET
       { 
         ignore (l, r);
	 mknode $startpos $endpos (L.expr LE.Concat) ((c::cl) @ [mknode $startpos(l) $endpos(r) L.ArrayRange [e]]) 
       }
   | LBRACE c=const_expr LBRACE cl=cate_list RBRACE RBRACE l=LBRACKET e0=expr COLON       e1=expr r=RBRACKET
       { 
         ignore (l, r);
	 mknode $startpos $endpos (L.expr LE.Concat) ((c::cl) @ [mknode $startpos(l) $endpos(r) L.ArrayRange [e0; e1]]) 
       }
   | LBRACE c=const_expr LBRACE cl=cate_list RBRACE RBRACE l=LBRACKET e0=expr PLUS_COLON  e1=expr r=RBRACKET
       { 
         ignore (l, r);
	 mknode $startpos $endpos (L.expr LE.Concat) ((c::cl) @ [mknode $startpos(l) $endpos(r) L.ArrayRangeMinus [e0; e1]]) 
       }
   | LBRACE c=const_expr LBRACE cl=cate_list RBRACE RBRACE l=LBRACKET e0=expr MINUS_COLON e1=expr r=RBRACKET
       { 
         ignore (l, r);
	 mknode $startpos $endpos (L.expr LE.Concat) ((c::cl) @ [mknode $startpos(l) $endpos(r) L.ArrayRangePlus [e0; e1]]) 
       }
   |               f=function_subroutine_call_no_method { f }
   | e=ev_expr DOT f=function_subroutine_call_no_method 
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [e];
	 f
       }
   | e=ev_expr DOT a=array_method_no_root                          { reloc $startpos $endpos a; a#add_children_l [e]; a }
   | LPAREN__IGN LPAREN e=expr                              RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [e] }
   | LPAREN__IGN LPAREN e0=expr COLON e1=expr COLON e2=expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [e0; e1; e2] }
   | UNDERSCORE  LPAREN state_push expr state_pop           RPAREN { dummy_node }
   | c=casting_type TICK LPAREN e=expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [c; e] }
   | e0=ev_expr     TICK LPAREN e=expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [e0; e] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | e=ev_expr_ok_lvalue { e }
   | e0=ev_expr AMP_AMP_AMP      e1=ev_expr        { mknode $startpos $endpos L.CondPredicate [e0; e1] }
   | e=ev_expr  MATCHES MATCHES_ p=pattern_no_expr { mknode $startpos $endpos L.CondPattern [e; p] }
   | e0=ev_expr MATCHES MATCHES_ e1=ev_expr        { mknode $startpos $endpos L.CondPattern [e0; e1] }
   | e=ev_expr DIST LBRACE dl=dist_list RBRACE     { mknode $startpos $endpos L.Dist (e::dl) }

   | LPAREN el=ev_expr_list                             RPAREN { mknode $startpos $endpos (L.ev_expr LEE.Multi) el }
   | LPAREN el=ev_expr_list COLON e0=expr COLON e1=expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) (el @ [e0; e1]) }
;

expr_lvalue:
   | e=expr_ok_lvalue { e }
;

fexpr_lvalue:
   | f=fexpr_ok_lvalue { f }
;

expr_non_mintypmax_ok_lvalue:
   | expr_non_mintypmax_scope                                                      { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr                  RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | expr_non_mintypmax_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type  assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   |            assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation       { $1 }
;

expr_ok_lvalue:
   | expr_scope                                                      { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr                  RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | expr_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type  assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   |            assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation       { $1 }
;

fexpr_ok_lvalue:
   | fexpr_scope                                                     { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr RBRACKET                  
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]])  
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | fexpr_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type   assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   |             assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation        { $1 }
;

sexpr_ok_lvalue:
   | sexpr_scope                                                     { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr                  RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]])  
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]])  
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | sexpr_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type   assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   |             assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation        { $1 }
;

pexpr_ok_lvalue:
   | pexpr_scope                                                     { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr                  RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]])  
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]])  
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | pexpr_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type   assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   |             assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation        { $1 }
;

ev_expr_ok_lvalue:
   | ev_expr_scope                                                   { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr                  RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | ev_expr_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type     assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2 ] }
   |               assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation          { $1 }
;

pev_expr_ok_lvalue:
   | pev_expr_scope                                                  { $1 }
   | LBRACE cate_list RBRACE                                         { mknode $startpos $endpos (L.expr LE.Concat) $2 }
   | LBRACE cate_list RBRACE LBRACKET expr                  RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($6) L.ArrayRange [$5]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRange [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangePlus [$5; $7]]) 
       }
   | LBRACE cate_list RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) ($2 @ [mknode $startpos($4) $endpos($8) L.ArrayRangeMinus [$5; $7]]) 
       }
   | pev_expr_scope assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   | data_type      assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1; $2] }
   |                assignment_pattern { mknode $startpos $endpos L.AssignmentPatternExpr [$1] }
   | streaming_concatenation           { $1 }
;


expr_non_mintypmax_scope:
   | THIS                                { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_arrayed { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_arrayed { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | expr_non_mintypmax DOT   id_arrayed { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | expr_non_mintypmax DOT SUPER        { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                               { mkleaf $startpos $endpos L.ExprScopeSuper }
;

expr_scope:
   | THIS                                     { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_svkw_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_arrayed      { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_arrayed      { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | expr DOT                 id_arrayed      { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | expr DOT SUPER                           { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                                    { mkleaf $startpos $endpos L.ExprScopeSuper }
;

fexpr_scope:
   | THIS                                { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_arrayed { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_arrayed { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | fexpr DOT                id_arrayed { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | fexpr DOT SUPER                     { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                               { mkleaf $startpos $endpos L.ExprScopeSuper }
;

sexpr_scope:
   | THIS                                { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_arrayed { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_arrayed { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | sexpr DOT                id_arrayed { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | sexpr DOT SUPER                     { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                               { mkleaf $startpos $endpos L.ExprScopeSuper }
;

pexpr_scope:
   | THIS                                { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_arrayed { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_arrayed { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | pexpr DOT                id_arrayed { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | pexpr DOT SUPER                     { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                               { mkleaf $startpos $endpos L.ExprScopeSuper }
;

ev_expr_scope:
   | THIS                                { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_arrayed { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_arrayed { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | ev_expr DOT              id_arrayed { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | ev_expr DOT SUPER                   { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                               { mkleaf $startpos $endpos L.ExprScopeSuper }
;

pev_expr_scope:
   | THIS                                     { mkleaf $startpos $endpos L.ExprScopeThis }
   |                          id_svkw_arrayed { mknode $startpos $endpos L.ExprScope [$1] }
   | package_scope_id_follows id_svkw_arrayed { mknode $startpos $endpos L.ExprScope [$1; $2] }
   | class_scope_id_follows   id_svkw_arrayed { mknode $startpos $endpos L.ExprScope ($1 @ [$2]) }
   | pev_expr DOT             id_svkw_arrayed { mknode $startpos $endpos L.ExprScopeDot [$1; $3] }
   | pev_expr DOT SUPER                       { mknode $startpos $endpos L.ExprScopeDotSuper [$1] }
   | SUPER                                    { mkleaf $startpos $endpos L.ExprScopeSuper }
;

expr_or_data_type:
   | expr      { $1 }
   | data_type { $1 }
;

cate_list:
   |                 stream_expression { [$1] }
   | cate_list COMMA stream_expression { $1 @ [$3] }
;

expr_or_data_type_list:
   |                              expr_or_data_type { [$1] }
   | expr_or_data_type_list COMMA expr_or_data_type { $1 @ [$3] }
   | expr_or_data_type_list COMMA                   { $1 }
;

argument_list_opt:
   |                          args_dotted_list { $1 }
   | args_expr_list_opt                        { $1 }
   | args_expr_list_opt COMMA args_dotted_list { $1 @ $3 }
;

pev_argument_list_opt:
   |                              pev_args_dotted_list { $1 }
   | pev_args_expr_list_opt                            { $1 }
   | pev_args_expr_list_opt COMMA pev_args_dotted_list { $1 @ $3 }
;

args_expr_list:
   |                      expr { [$1] }
   | args_expr_list COMMA expr { $1 @ [$3] }
;

args_expr_list_opt:
   |                          args_expr_opt { $1 }
   | args_expr_list_opt COMMA args_expr_opt { $1 @ $3 }
;

pev_args_expr_list_opt:
   |                              pev_args_expr_opt { $1 }
   | pev_args_expr_list_opt COMMA pev_args_expr_opt { $1 @ $3 }
;

args_expr_opt:
   | (* empty *) { [] }
   | e=expr      { [e] }
;

pev_args_expr_opt:
   | (* empty *) { [] }
   | p=pev_expr  { [p] }
;

args_dotted_list:
   |                           a=args_dotted { [a] }
   | al=args_dotted_list COMMA a=args_dotted { al @ [a] }
;

pev_args_dotted_list:
   |                               p=pev_args_dotted { [p] }
   | pl=pev_args_dotted_list COMMA p=pev_args_dotted { pl @ [p] }
;

args_dotted:
   | DOT i=id_any LPAREN e=expr RPAREN { mknode $startpos $endpos (L.ArgsDotted i) [e] }
;

pev_args_dotted:
   | DOT i=id_any LPAREN e=pev_expr RPAREN { mknode $startpos $endpos (L.ArgsDotted i) [e] }
;

streaming_concatenation:
   | LBRACE l=LT_LT s=stream_conc_or_expr_or_type RBRACE
       { 
         ignore l;
         mknode $startpos $endpos L.StreamingConcat ((mkleaf $startpos(l) $endpos(l) L.OrderRL)::s)
       }
   | LBRACE l=LT_LT s=stream_conc_or_expr_or_type sc=stream_concatenation RBRACE 
       { 
         ignore l;
	 mknode $startpos $endpos L.StreamingConcat ((mkleaf $startpos(l) $endpos(l) L.OrderRL)::s @ [sc])
       }
   | LBRACE g=GT_GT s=stream_conc_or_expr_or_type RBRACE
       { 
         ignore g;
         mknode $startpos $endpos L.StreamingConcat ((mkleaf $startpos(g) $endpos(g) L.OrderLR)::s)
       }
   | LBRACE g=GT_GT s=stream_conc_or_expr_or_type sc=stream_concatenation RBRACE 
       { 
         ignore g;
	 mknode $startpos $endpos L.StreamingConcat ((mkleaf $startpos(g) $endpos(g) L.OrderLR)::s @ [sc])
       }
;

stream_conc_or_expr_or_type:
   | cl=cate_list  { cl }
   | s=simple_type { [s] }
;

stream_concatenation:
   | LBRACE s=stream_expression_list RBRACE { mknode $startpos $endpos L.StreamConcat s }
;

stream_expression_list:
   |                                 s=stream_expression { [s] }
   | sl=stream_expression_list COMMA s=stream_expression { sl @ [s] }
;

stream_expression:
   | expr                                                 { mknode $startpos $endpos (L.expr LE.Stream) [$1] }
   | expr WITH__B LBRACKET expr                  RBRACKET { mknode $startpos $endpos (L.expr LE.Stream) [$1; mknode $startpos($3) $endpos($5) L.ArrayRange [$4]] }
   | expr WITH__B LBRACKET expr COLON expr       RBRACKET { mknode $startpos $endpos (L.expr LE.Stream) [$1; mknode $startpos($3) $endpos($7) L.ArrayRange [$4; $6]] }
   | expr WITH__B LBRACKET expr PLUS_COLON expr  RBRACKET { mknode $startpos $endpos (L.expr LE.Stream) [$1; mknode $startpos($3) $endpos($7) L.ArrayRangePlus [$4; $6]] }
   | expr WITH__B LBRACKET expr MINUS_COLON expr RBRACKET { mknode $startpos $endpos (L.expr LE.Stream) [$1; mknode $startpos($3) $endpos($7) L.ArrayRangeMinus [$4; $6]] }
;

gate_keyword:
   | g=GATE { Ls.Gate.Gate g }
   | AND    { Ls.Gate.And }
   | BUF    { Ls.Gate.Buf }
   | NAND   { Ls.Gate.Nand }
   | NOR    { Ls.Gate.Nor }
   | NOT    { Ls.Gate.Not }
   | OR     { Ls.Gate.Or }
   | XNOR   { Ls.Gate.Xnor }
   | XOR    { Ls.Gate.Xor }
;

strength:
   | s=STRENGTH { mkleaf $startpos $endpos (L.Strength s) }
   | SUPPLY0    { mkleaf $startpos $endpos L.StrengthSupply0 }
   | SUPPLY1    { mkleaf $startpos $endpos L.StrengthSupply1 }
;

strength_spec_opt:
   | (* empty *)   { [] }
   | strength_spec { [$1] }
;

strength_spec:
   | LPAREN__S strength RPAREN                { mknode $startpos $endpos L.StrengthSpec [$2] }
   | LPAREN__S strength COMMA strength RPAREN { mknode $startpos $endpos L.StrengthSpec [$2; $4]}
;

combinational_body:
   | TABLE combinational_entry_list ENDTABLE { mknode $startpos $endpos L.CombinationalBody $2 }
;

combinational_entry_list:
   |                          combinational_entry { [$1] }
   | combinational_entry_list combinational_entry { $1 @ [$2] }
;

combinational_entry:
   | level_input_list COLON output_symbol SEMICOLON { mknode $startpos $endpos L.CombinationalEntry ($1 @ [$3]) }
;

level_input_list:
   |                  level_symbol { [$1] }
   | level_input_list level_symbol { $1 @ [$2] }
;

level_symbol: (* 0 1 x X ? b B *)
   | INTEGRAL_NUMBER { mkleaf $startpos $endpos (L.LevelSymbol $1) }
   | QUESTION        { mkleaf $startpos $endpos (L.LevelSymbol "?") }
   | SYMBOL_xX       { mkleaf $startpos $endpos (L.LevelSymbol $1) }
   | SYMBOL_bB       { mkleaf $startpos $endpos (L.LevelSymbol $1) }
;

output_symbol: (* 0 1 x X *)
   | INTEGRAL_NUMBER { mkleaf $startpos $endpos (L.OutputSymbol $1) }
   | SYMBOL_xX       { mkleaf $startpos $endpos (L.OutputSymbol $1) }
;

specify_block:
   | SPECIFY specify_item_list ENDSPECIFY { mknode $startpos $endpos L.SpecifyBlock $2 }
   | SPECIFY                   ENDSPECIFY { mkleaf $startpos $endpos L.SpecifyBlock }
;

specify_item_list:
   |                   specify_item { [$1] }
   | specify_item_list specify_item { $1 @ [$2] }
;

specify_item:
   | specparam_declaration     { $1 }
   | pulsestyle_declaration    { $1 }
   | showcancelled_declaration { $1 }
   | path_declaration          { $1 }
   | system_timing_check       { $1 }
;

system_timing_check:
   | timing_check0 LPAREN timing_check_event COMMA timing_check_event COMMA expr comma_notifier_opt        RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos L.SystemTimingCheck ([$1; $3; $5; $7] @ $8) 
       }
   | timing_check1 LPAREN timing_check_event COMMA timing_check_event COMMA expr COMMA expr setuphold_rest RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos L.SystemTimingCheck ([$1; $3; $5; $7; $9] @ $10) 
       }
   | timing_check2 LPAREN timing_check_event COMMA timing_check_event COMMA expr timeskew_rest             RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos L.SystemTimingCheck ([$1; $3; $5; $7] @ $8) 
       }
   | TC_PERIOD     LPAREN timing_check_event COMMA expr comma_notifier_opt                                 RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos L.SystemTimingCheck ([mkleaf $startpos $endpos (L.timing_check LTC.Period); $3; $5] @ $6) 
       }
   | TC_WIDTH      LPAREN timing_check_event COMMA expr COMMA const_expr comma_notifier_opt                RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos L.SystemTimingCheck ([mkleaf $startpos $endpos (L.timing_check LTC.Width); $3; $5; $7] @ $8) 
       }
   | TC_NOCHANGE   LPAREN timing_check_event COMMA timing_check_event COMMA min_type_max COMMA min_type_max comma_notifier_opt RPAREN SEMICOLON 
       { 
	 mknode $startpos $endpos L.SystemTimingCheck ([mkleaf $startpos $endpos (L.timing_check LTC.Nochange); $3; $5; $7; $9] @ $10) 
       }
;

timing_check0:
   | timing_check0_ { mkleaf $startpos $endpos (L.timing_check $1) }

timing_check0_:
   | TC_SETUP    { LTC.Setup }
   | TC_HOLD     { LTC.Hold }
   | TC_RECOVERY { LTC.Recovery }
   | TC_REMOVAL  { LTC.Removal }
   | TC_SKEW     { LTC.Skew }
;

timing_check1:
   | timing_check1_ { mkleaf $startpos $endpos (L.timing_check $1) }
;

timing_check1_:
   | TC_SETUPHOLD { LTC.Setuphold }
   | TC_RECREM    { LTC.Recrem }
;

timing_check2:
   | timing_check2_ { mkleaf $startpos $endpos (L.timing_check $1) }
;

timing_check2_:
   | TC_TIMESKEW { LTC.Timeskew }
   | TC_FULLSKEW { LTC.Fullskew }
;
     
comma_notifier_opt:
   | (* empty *) { [] }
   | COMMA       { [] }
   | COMMA id    { [mkleaf $startpos($2) $endpos (L.Notifier $2)] }
;

notifier_opt:
   | (* empty *) { [] }
   | id          { [mkleaf $startpos $endpos (L.Notifier $1)] }
;

setuphold_rest:
   | (* empty *)                                { [] }
   | COMMA n=notifier_opt c=comma_stamptime_opt { n @ c }
;

%inline
comma_stamptime_opt:
   | (* empty *)                                    { [] }
   | COMMA m=min_type_max_opt c=comma_checktime_opt { m @ c }
;

min_type_max_opt:
   | (* empty *)  { [] }
   | min_type_max { [$1] }
;

%inline
comma_checktime_opt:
   | (* empty *)                                            { [] }
   | COMMA m=min_type_max_opt c=comma_delayed_reference_opt { m @ c }
;

%inline
comma_delayed_reference_opt:
   | (* empty *)                                  { [] }
   | COMMA d=delayed_opt c=comma_delayed_data_opt { d @ c }
;

%inline
delayed_opt:
   | (* empty *) { [] }
   | d=delayed   { [d] }
;

%inline
comma_delayed_data_opt:
   | (* empty *)         { [] }
   | COMMA d=delayed_opt { d }
;

delayed:
   | id                                { mkleaf $startpos $endpos (L.Delayed $1) }
   | id LBRACKET min_type_max RBRACKET { mknode $startpos $endpos (L.Delayed $1) [$3] }
;

timeskew_rest:
   | (* empty *)                                       { [] }
   | COMMA n=notifier_opt c=comma_event_based_flag_opt { n @ c }
;

%inline
comma_event_based_flag_opt:
   | (* empty *)                                                { [] }
   | COMMA e=event_base_flag_opt c=comma_remain_active_flag_opt { e @ c }
;

event_base_flag_opt:
   | (* empty *) { [] }
   | const_expr  { [$1] }
;

%inline
comma_remain_active_flag_opt:
   | (* empty *)                    { [] }
   | COMMA r=remain_active_flag_opt { r }
;

%inline
remain_active_flag_opt:
   | (* empty *)    { [] }
   | m=min_type_max { [m] }
;

timing_check_event:
   | timing_check_event_control specify_terminal_descriptor AMP_AMP_AMP timing_check_condition { mknode $startpos $endpos L.TimingCheckEvent [$1; $2; $4] }
   |                            specify_terminal_descriptor AMP_AMP_AMP timing_check_condition { mknode $startpos $endpos L.TimingCheckEvent [$1; $3] }
   | timing_check_event_control specify_terminal_descriptor                                    { mknode $startpos $endpos L.TimingCheckEvent [$1; $2] }
   |                            specify_terminal_descriptor                                    { mknode $startpos $endpos L.TimingCheckEvent [$1] }
;

timing_check_event_control:
   | POSEDGE                { mkleaf $startpos $endpos L.TimingCheckEventControlPosedge }
   | NEGEDGE                { mkleaf $startpos $endpos L.TimingCheckEventControlNegedge }
   | edge_control_specifier { $1 }
;

edge_control_specifier:
   | EDGE LBRACKET edge_descriptor_list RBRACKET { mknode $startpos $endpos L.TimingCheckEventControl $3 }
;

edge_descriptor_list:
   |                            edge_descriptor { [$1] }
   | edge_descriptor_list COMMA edge_descriptor { $1 @ [$3] }
;

edge_descriptor: (* [01xXzX][01xXzZ] *)
   | INTEGRAL_NUMBER { mkleaf $startpos $endpos (L.EdgeDescriptor $1) }
;

timing_check_condition:
   | s=scalar_timing_check_condition { s }
;

scalar_timing_check_condition:
   | e=expr { e }
(*
   | TILDE expr { }
   | expr EQ_EQ        INTEGRAL_NUMBER { }
   | expr EQ_EQ_EQ     INTEGRAL_NUMBER { }
   | expr EXCLAM_EQ    INTEGRAL_NUMBER { }
   | expr EXCLAM_EQ_EQ INTEGRAL_NUMBER { }
*)
;

specparam_declaration:
   | SPECPARAM                  specparam_assignment_list SEMICOLON { mknode $startpos $endpos L.SpecparamDeclaration $2 }
   | SPECPARAM packed_dimension specparam_assignment_list SEMICOLON { mknode $startpos $endpos L.SpecparamDeclaration ($2::$3) }
;

specparam_assignment_list:
   |                                 specparam_assignment { [$1] }
   | specparam_assignment_list COMMA specparam_assignment { $1 @ [$3] }
;

specparam_assignment:
   | id                   EQ        min_type_max                           { mknode $startpos $endpos (L.SpecparamAssignmentId $1) [$3] }
   | PATHPULSE_IDENTIFIER EQ LPAREN min_type_max                    RPAREN { mknode $startpos $endpos (L.SpecparamAssignmentPulseControl $1) [$4] }
   | PATHPULSE_IDENTIFIER EQ LPAREN min_type_max COMMA min_type_max RPAREN { mknode $startpos $endpos (L.SpecparamAssignmentPulseControl $1) [$4; $6] }
;

pulsestyle_declaration:
   | PULSESTYLE_ONEVENT  path_list SEMICOLON { mknode $startpos $endpos L.PulsestyleDeclarationOnevent $2 }
   | PULSESTYLE_ONDETECT path_list SEMICOLON { mknode $startpos $endpos L.PulsestyleDeclarationOndetect $2 }
;

showcancelled_declaration:
   | SHOWCANCELLED   path_list SEMICOLON { mknode $startpos $endpos L.ShowcancelledDeclaration $2 }
   | NOSHOWCANCELLED path_list SEMICOLON { mknode $startpos $endpos L.NoshowcancelledDeclaration $2 }
;


path_list:
   |                 specify_terminal_descriptor { [$1] }
   | path_list COMMA specify_terminal_descriptor { $1 @ [$3] }
;

specify_terminal_descriptor:
   | input_or_output_identifier                                  { mknode $startpos $endpos L.SpecifyTerminalDescriptor [$1] }
   | input_or_output_identifier LBRACKET constant_range RBRACKET { mknode $startpos $endpos L.SpecifyTerminalDescriptor [$1; $3] }
;

input_or_output_identifier:
   |        id { mkleaf $startpos $endpos (L.InputOrOutputId $1) }
   | id DOT id { mknode $startpos $endpos (L.InputOrOutputId $3) [mkleaf $startpos $endpos($1) (L.InterfaceIdentifier $1)] }
;

path_declaration:
   | s=simple_path_declaration          SEMICOLON { reloc $startpos $endpos s; s }
   | e=edge_sensitive_path_declaration  SEMICOLON { reloc $startpos $endpos e; e }
   | s=state_dependent_path_declaration SEMICOLON { reloc $startpos $endpos s; s }
;

simple_path_declaration:
   | parallel_path_description EQ path_delay_value { mknode $startpos $endpos L.SimplePathDeclaration [$1; $3] }
   | full_path_description     EQ path_delay_value { mknode $startpos $endpos L.SimplePathDeclaration [$1; $3] }
;

parallel_path_description:
   | LPAREN specify_terminal_descriptor                   EQ_GT specify_terminal_descriptor RPAREN { mknode $startpos $endpos L.ParallelPathDescription [$2; $4] }
   | LPAREN specify_terminal_descriptor polarity_operator EQ_GT specify_terminal_descriptor RPAREN { mknode $startpos $endpos L.ParallelPathDescription [$2; $3; $5] }
;

full_path_description:
   | LPAREN path_list STAR_GT path_list RPAREN 
       { 
	 mknode $startpos $endpos L.FullPathDescription [mknode $startpos($2) $endpos($2) L.PathInputs $2; mknode $startpos($4) $endpos($4) L.PathOutputs $4] 
       }
;

path_delay_value:
   |        path_delay_expression_list        { mknode $startpos $endpos L.PathDelayValue $1 }
   | LPAREN path_delay_expression_list RPAREN { mknode $startpos $endpos L.PathDelayValue $2 }
;

mintypmax_without_paren:
   | expr_non_mintypmax                                                   { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$1] }
   | expr_non_mintypmax COLON expr_non_mintypmax COLON expr_non_mintypmax { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$1; $3; $5] }
;

path_delay_expression_list:
   |                                  mintypmax_without_paren { [$1] }
   | path_delay_expression_list COMMA mintypmax_without_paren { $1 @ [$3] }
;


edge_sensitive_path_declaration:
   | parallel_edge_sensitive_path_description EQ path_delay_value { mknode $startpos $endpos L.EdgeSensitivePathDeclaration [$1; $3] }
   | full_edge_sensitive_path_description     EQ path_delay_value { mknode $startpos $endpos L.EdgeSensitivePathDeclaration [$1; $3] }
;

%inline
edge_identifier_opt:
   | (* empty *)       { [] }
   | e=edge_identifier { [e] }
;

%inline
polarity_operator_opt:
   | (* empty *)         { [] }
   | p=polarity_operator { [p] }
;


%inline
parallel_edge_sensitive_path_description_sub:
   | LPAREN s=specify_terminal_descriptor p=polarity_operator_opt COLON e=expr RPAREN 
       { 
	 mknode $startpos $endpos L.ParallelEdgeSensitivePathDescriptionSub (s :: p @ [e])
       }
;

parallel_edge_sensitive_path_description:
   | LPAREN e=edge_identifier_opt s=specify_terminal_descriptor EQ_GT p=parallel_edge_sensitive_path_description_sub RPAREN 
       { 
	 mknode $startpos $endpos L.ParallelEdgeSensitivePathDescription (e @ [s; p])
       }
;

%inline
full_edge_sensitive_path_description_sub:
   | LPAREN pl=path_list p=polarity_operator_opt COLON e=expr RPAREN { mknode $startpos $endpos L.FullEdgeSensitivePathDescriptionSub (pl @ p @ [e]) }
;

full_edge_sensitive_path_description:
   | LPAREN e=edge_identifier_opt pl=path_list STAR_GT f=full_edge_sensitive_path_description_sub RPAREN 
       { 
	 mknode $startpos $endpos L.FullEdgeSensitivePathDescription (e @ pl @ [f])
       }
;

state_dependent_path_declaration:
   | IF LPAREN expr RPAREN simple_path_declaration         { mknode $startpos $endpos L.StateDependentPathDeclarationIf [$3; $5] }
   | IF LPAREN expr RPAREN edge_sensitive_path_declaration { mknode $startpos $endpos L.StateDependentPathDeclarationIf [$3; $5] }
   | IFNONE simple_path_declaration                        { mknode $startpos $endpos L.StateDependentPathDeclarationIfnone [$2] }
;


polarity_operator:
   | PLUS  { mkleaf $startpos $endpos L.PolarityPlus }
   | MINUS { mkleaf $startpos $endpos L.PolarityMinus }
;

edge_identifier:
   | POSEDGE { mkleaf $startpos $endpos L.EdgePosedge }
   | NEGEDGE { mkleaf $startpos $endpos L.EdgeNegedge }
;



id:
   | i=IDENTIFIER    { i }
   | p=PP_IDENTIFIER { p }
   | n=PP_MACRO_NAME { n }
;

id_any:
   | c=CLASS_IDENTIFIER      { c }
   | c=COVERGROUP_IDENTIFIER { c }
   | p=PACKAGE_IDENTIFIER    { p }
   | t=TYPE_IDENTIFIER       { t }
   | i=IDENTIFIER            { i }
   | i=id_SV_keyword         { i }
(*
   | c=CLOCKING_IDENTIFIER  { c }
   | p=PROPERTY_IDENTIFIER  { p }
   | p=PP_IDENTIFIER        { p }
   | i=INTERFACE_IDENTIFIER { i }
*)
;

id_SV_keyword:
   | _id_SV_keyword 
       { 
	 env#current_source#set_lang_spec_v2005;
	 Common.parse_warning $startpos $endpos "System Velilog keyword \"%s\" used as identifier: assuming \"%s\"" 
	   $1 (Source.lang_spec_to_string env#current_source#lang_spec); 
	 $1 
       }

_id_SV_keyword:
   | DO      { "do" }
   | FINAL   { "final" }
   | CLASS   { "class" }
   | RETURN  { "return" }
   | RAND    { "rand" }
;

%inline
assignment_pattern_:
   | TICK_LBRACE vs=variable_lvalue_list RBRACE { mknode $startpos $endpos L.AssignmentPattern vs }
;

variable_lvalue:
   | i=id_class_sel                             { i }
   | LBRACE vs=variable_lvalue_conc_list RBRACE { mknode $startpos $endpos L.VariableLvalue vs }
   | d=data_type    a=assignment_pattern_       { mknode $startpos $endpos L.AssignmentPatternExpr [d; a] }
   | i=id_class_sel a=assignment_pattern_       { mknode $startpos $endpos L.AssignmentPatternExpr [i; a] }
   |                a=assignment_pattern_       { mknode $startpos $endpos L.AssignmentPatternExpr [a] }
   | s=streaming_concatenation                  { s }
;

variable_lvalue_conc_list:
   |                                 variable_lvalue { [$1] }
   | variable_lvalue_conc_list COMMA variable_lvalue { $1 @ [$3] }
;

variable_lvalue_list:
   |                            variable_lvalue { [$1] }
   | variable_lvalue_list COMMA variable_lvalue { $1 @ [$3] }
;

id_class_sel:
   |                          id_dotted { mknode $startpos $endpos (L.IdClassSel (get_last_id $1)) $1 }
   | THIS DOT                 id_dotted { mknode $startpos $endpos (L.IdClassSel (get_last_id $3)) ((mkleaf $startpos $endpos($2) L.This)::$3) }
   | SUPER DOT                id_dotted { mknode $startpos $endpos (L.IdClassSel (get_last_id $3)) ((mkleaf $startpos $endpos($2) L.Super)::$3) }
   | THIS DOT SUPER DOT       id_dotted 
       { 
	 mknode $startpos $endpos (L.IdClassSel (get_last_id $5)) ([mkleaf $startpos $endpos($2) L.This; mkleaf $startpos($3) $endpos($4) L.Super] @ $5) 
       }
   | class_scope_id_follows   id_dotted { mknode $startpos $endpos (L.IdClassSel (get_last_id $2)) ($1 @ $2) }
   | package_scope_id_follows id_dotted { mknode $startpos $endpos (L.IdClassSel (get_last_id $2)) ($1::$2) }
;

id_class_foreach:
   |                          id_dotted_foreach { mknode $startpos $endpos (L.IdClassSel (get_last_id $1)) $1 }
   | THIS DOT                 id_dotted_foreach { mknode $startpos $endpos (L.IdClassSel (get_last_id $3)) ((mkleaf $startpos $endpos($2) L.This)::$3) }
   | SUPER DOT                id_dotted_foreach { mknode $startpos $endpos (L.IdClassSel (get_last_id $3)) ((mkleaf $startpos $endpos($2) L.Super)::$3) } 
   | THIS DOT SUPER DOT       id_dotted_foreach 
       { 
	 mknode $startpos $endpos (L.IdClassSel (get_last_id $5)) ([mkleaf $startpos $endpos($2) L.This; mkleaf $startpos($3) $endpos($4) L.Super] @ $5) 
       }
   | class_scope_id_follows   id_dotted_foreach { mknode $startpos $endpos (L.IdClassSel (get_last_id $2)) ($1 @ $2) }
   | package_scope_id_follows id_dotted_foreach { mknode $startpos $endpos (L.IdClassSel (get_last_id $2)) ($1::$2) }
;

hierarchical_identifier_list:
   |                                    hierarchical_identifier { [$1] }
   | hierarchical_identifier_list COMMA hierarchical_identifier { $1 @ [$3] }
;

hierarchical_identifier_bit:
   | id_class_sel { $1 }
;

hierarchical_identifier:
   | id_class_sel { $1 }
;

id_dotted:
   | ST_ROOT DOT id_dotted_rest { (mkleaf $startpos $endpos($2) L.Root)::$3 }
   |             id_dotted_rest { $1 }
;

id_dotted_foreach:
   | ST_ROOT DOT id_dotted_foreach_rest { (mkleaf $startpos $endpos($2) L.Root)::$3 }
   |             id_dotted_foreach_rest { $1 }
;

id_dotted_rest:
   |                    id_arrayed { [$1] }
   | id_dotted_rest DOT id_arrayed { $1 @ [$3] }
;

id_dotted_foreach_rest:
   | id_foreach                            { [$1] }
   | id_dotted_foreach_rest DOT id_foreach { $1 @ [$3] }
;

id_arrayed:
   | id_arrayed_ { let id, sel = $1 in mknode $startpos $endpos (L.IdSelect id) sel }
;

id_arrayed_:
   | id                                                        { $1, [] }
   | id_arrayed_ LBRACKET expr                        RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($4) L.Select [$3]] }
   | id_arrayed_ LBRACKET const_expr COLON const_expr RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.Range [$3; $5]] }
   | id_arrayed_ LBRACKET expr PLUS_COLON  const_expr RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangePlus [$3; $5]] }
   | id_arrayed_ LBRACKET expr MINUS_COLON const_expr RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangeMinus [$3; $5]] }
;

id_svkw_arrayed:
   | id_arrayed   { $1 }
   | svkw_arrayed { $1 }
;

svkw_arrayed:
   | svkw_arrayed_ { let id, sel = $1 in mknode $startpos $endpos (L.IdSelect id) sel }
;

svkw_arrayed_:
   | id_SV_keyword                                                 { $1, [] }
   | svkw_arrayed_ LBRACKET expr                        RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($4) L.Select [$3]] }
   | svkw_arrayed_ LBRACKET const_expr COLON const_expr RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.Range [$3; $5]] }
   | svkw_arrayed_ LBRACKET expr PLUS_COLON  const_expr RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangePlus [$3; $5]] }
   | svkw_arrayed_ LBRACKET expr MINUS_COLON const_expr RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangeMinus [$3; $5]] }
;

id_foreach:
   | id_foreach_ { let id, sel = $1 in mknode $startpos $endpos (L.IdSelect id) sel }
;

id_foreach_:
   | id                                                          { $1, [] }
   | id_foreach_ LBRACKET expr                          RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($4) L.Select [$3]] }
   | id_foreach_ LBRACKET const_expr COLON const_expr   RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.Range [$3; $5]] }
   | id_foreach_ LBRACKET expr PLUS_COLON const_expr    RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangePlus [$3; $5]] }
   | id_foreach_ LBRACKET expr MINUS_COLON const_expr   RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangeMinus [$3; $5]] }
   | id_foreach_ LBRACKET expr COMMA loop_variable_list RBRACKET { let id, sel = $1 in id, sel @ [mknode $startpos($2) $endpos($6) L.RangeForeach ($3::$5)] }
;

str_as_int:
   | s=string_literal { s }
;

%inline
end_label_opt:
   | (* empty *)  { [] }
   | e=end_label { [e] }
;

%inline
end_label:
   | end_label0 lab=id_any { mkleaf $startpos $endpos (L.EndLabel lab) }
   | end_label0 NEW        { mkleaf $startpos $endpos L.EndLabelNew }
;

%inline
end_label0:
   | COLON { context_stack#activate_top_no_delay }
;

clocking_declaration:
   | c=clocking_head ce=clocking_event SEMICOLON ci=clocking_item_list_opt ENDCLOCKING e=end_label_opt 
       { 
	 let p, id = c in
	 mknode $startpos $endpos 
	   (L.ClockingDeclaration id) 
	   (p @ [ce; mknode $startpos(ci) $endpos(ci) L.ClockingBody ci] @ e)
       }
;

global:
| GLOBAL { mkleaf $startpos $endpos L.Global }
;

default:
| DEFAULT { mkleaf $startpos $endpos L.Default }
;

clocking_head:
   |         CLOCKING        { [], "" }
   |         CLOCKING id_any { register_clocking $2; [], $2 }
   | default CLOCKING        { [$1], "" }
   | default CLOCKING id_any { register_clocking $3; [$1], $3 }
   | global  CLOCKING        { [$1], "" }
   | global  CLOCKING id_any { register_clocking $3; [$1], $3 }
;

clocking_event:
   | AT i=id                          { mkleaf $startpos $endpos (L.ClockingEvent i) }
   | AT LPAREN el=ev_expr_list RPAREN { mknode $startpos $endpos L.ClockingEventParen el }
;

clocking_item_list_opt:
   | (* empty *)        { [] }
   | clocking_item_list { $1 }
;

clocking_item_list:
   |                    clocking_item { [$1] }
   | clocking_item_list clocking_item { $1 @ [$2] }
;

clocking_item:
   | DEFAULT default_skew SEMICOLON                         { mknode $startpos $endpos L.ClockingItemDefault [$2] }
   | clocking_direction clocking_decl_assign_list SEMICOLON { mknode $startpos $endpos L.ClockingItem ($1::$2) }
   | assertion_item_declaration                             { $1 }
;

default_skew:
   | INPUT  clocking_skew                      { mknode $startpos $endpos L.DefaultSkewInput [$2] }
   | OUTPUT clocking_skew                      { mknode $startpos $endpos L.DefaultSkewOutput [$2] }
   | INPUT  clocking_skew OUTPUT clocking_skew { mknode $startpos $endpos L.DefaultSkewInputOutput [$2; $4] }
;

clocking_direction:
   | INPUT c0=clocking_skew_opt                             { mknode $startpos $endpos L.ClockingDirectionInput c0 }
   | INPUT c0=clocking_skew_opt OUTPUT c1=clocking_skew_opt { mknode $startpos $endpos L.ClockingDirectionInputOutput (c0 @ c1) }
   | INOUT                                                  { mkleaf $startpos $endpos L.ClockingDirectionInout }
;

clocking_decl_assign_list:
   |                                 clocking_decl_assign { [$1] }
   | clocking_decl_assign_list COMMA clocking_decl_assign { $1 @ [$3] }
;

clocking_decl_assign:
   | id_any         { mkleaf $startpos $endpos (L.ClockingDeclAssign $1) }
   | id_any EQ expr { mknode $startpos $endpos (L.ClockingDeclAssign $1) [$3] }
;

%inline
clocking_skew_opt:
   | (* empty *)     { [] }
   | c=clocking_skew { [c] }
;

clocking_skew:
   | POSEDGE               { mkleaf $startpos $endpos L.ClockingSkewPosedge }
   | POSEDGE delay_control { mknode $startpos $endpos L.ClockingSkewPosedge [$2] }
   | NEGEDGE               { mkleaf $startpos $endpos L.ClockingSkewNegedge }
   | NEGEDGE delay_control { mknode $startpos $endpos L.ClockingSkewNegedge [$2] }
   | EDGE                  { mkleaf $startpos $endpos L.ClockingSkewEdge }
   | EDGE    delay_control { mknode $startpos $endpos L.ClockingSkewEdge [$2] }
   |         delay_control { mknode $startpos $endpos L.ClockingSkew [$1] }
;

cycle_delay:
   | SHARP_SHARP INTEGRAL_NUMBER    { mkleaf $startpos $endpos (L.CycleDelay $2) }
   | SHARP_SHARP id                 { mkleaf $startpos $endpos (L.CycleDelayId $2) }
   | SHARP_SHARP LPAREN expr RPAREN { mknode $startpos $endpos L.CycleDelayParen [$3] }
;

assertion_item_declaration:
   | p=property_declaration { p }
   | s=sequence_declaration { s }
   | l=let_declaration      { l }
;

assertion_item:
   | c=concurrent_assertion_item         { c }
   | d=deffered_immediate_assertion_item { d }
;

deffered_immediate_assertion_item:
   |            d=deferred_immediate_assertion_statement { d }
   | i=id COLON d=deferred_immediate_assertion_statement { mknode $startpos $endpos (L.DeferredImmediateAssertionItemLabeled i) [d] }
;

procedural_assertion_statement:
   | c=concurrent_assertion_statement { c }
   | i=immediate_assertion_statement  { i }
   | c=checker_instantiation          { c }
;

immediate_assertion_statement:
   | s=simple_immediate_assertion_statement   { s }
   | d=deferred_immediate_assertion_statement { d }
;

simple_immediate_assertion_statement:
   | ASSERT LPAREN e=expr RPAREN a=action_block { mknode $startpos $endpos (L.sia_stmt LSIA.Assert) [e; a] }
   | ASSUME LPAREN e=expr RPAREN a=action_block { mknode $startpos $endpos (L.sia_stmt LSIA.Assume) [e; a] }
   | COVER  LPAREN e=expr RPAREN s=stmt         { mknode $startpos $endpos (L.sia_stmt LSIA.Cover) [e; s] }
;

deferred_immediate_assertion_statement:
   | ASSERT SHARP i=INTEGRAL_NUMBER LPAREN e=expr RPAREN a=action_block { mknode $startpos $endpos (L.dia_stmt (LDIA.Assert i)) [e; a] }
   | ASSUME SHARP i=INTEGRAL_NUMBER LPAREN e=expr RPAREN a=action_block { mknode $startpos $endpos (L.dia_stmt (LDIA.Assume i)) [e; a] }
   | COVER  SHARP i=INTEGRAL_NUMBER LPAREN e=expr RPAREN s=stmt         { mknode $startpos $endpos (L.dia_stmt (LDIA.Cover i)) [e; s] }
;

expect_property_statement:
   | EXPECT LPAREN p=property_spec RPAREN a=action_block { mknode $startpos $endpos (L.stmt LS.ExpectProperty) [p; a] }
;

concurrent_assertion_item:
   |            c=concurrent_assertion_statement { c }
   | i=id COLON c=concurrent_assertion_statement { mknode $startpos $endpos (L.ConcurrentAssertionItemLabeled i) [c] }
;

concurrent_assertion_statement:
   | ASSERT   PROPERTY LPAREN property_spec RPAREN action_block { mknode $startpos $endpos (L.ca_stmt LCA.AssertProp) [$4; $6] }
   | ASSUME   PROPERTY LPAREN property_spec RPAREN action_block { mknode $startpos $endpos (L.ca_stmt LCA.AssumeProp) [$4; $6] }
   | COVER    PROPERTY LPAREN property_spec RPAREN stmt_block   { mknode $startpos $endpos (L.ca_stmt LCA.CoverProp) [$4; $6] }
   | COVER    SEQUENCE LPAREN expr          RPAREN stmt         { mknode $startpos $endpos (L.ca_stmt LCA.CoverSeq) [$4; $6] }
   | COVER    SEQUENCE LPAREN clocking_event DISABLE IFF LPAREN expr RPAREN sexpr RPAREN stmt 
       { 
	 let di = mknode $startpos($5) $endpos($10) L.DisableIff [$8; $10] in
	 mknode $startpos $endpos (L.ca_stmt LCA.CoverSeq) [mknode $startpos($4) $endpos($10) (L.pexpr LPE.Spec) [$4; di]; $12]
       }
   | COVER    SEQUENCE LPAREN                DISABLE IFF LPAREN expr RPAREN sexpr RPAREN stmt 
       { 
	 let di = mknode $startpos($4) $endpos($9) L.DisableIff [$7; $9] in
	 mknode $startpos $endpos (L.ca_stmt LCA.CoverSeq) [mknode $startpos($4) $endpos($9) (L.pexpr LPE.Spec) [di]; $11]
       }
   | RESTRICT PROPERTY LPAREN property_spec RPAREN SEMICOLON { mknode $startpos $endpos (L.ca_stmt LCA.RestrictProp) [$4] }
;

property_declaration:
   | pd=property_declaration_head pp=property_port_list_opt SEMICOLON pb=property_declaration_body ENDPROPERTY e=end_label_opt 
       { 
	 mknode $startpos $endpos (L.PropertyDeclaration pd) (pp @ [pb] @ e)
       }
;

property_declaration_head:
   | PROPERTY id_any { register_property $2; $2 }
;

%inline
property_port_list_opt:
   | (* empty *)                        { [] }
   | LPAREN p=property_port_list RPAREN { p }
;

property_port_list:
   |                          property_port_item { [$1] }
   | property_port_list COMMA property_port_item { $1 @ [$3] }
;

property_port_item:
   | h=property_port_item_head p=property_port_item_assignment { mknode $symbolstartpos $endpos L.PropertyPortItem (h @ [p]) }
;

property_port_item_head:
   | p=property_port_item_dir_opt pf=property_formal_type_no_data_type { p @ [pf] }
   | p=property_port_item_dir_opt       d=data_type                    { p @ [d] }
   | p=property_port_item_dir_opt v=var d=data_type                    { p @ [v; d] }
   | p=property_port_item_dir_opt v=var i=implicit_data_type           { p @ (v::i) }
   | p=property_port_item_dir_opt       i=_implicit_data_type          { p @ [i] }
   | p=property_port_item_dir_opt                                      { p }
;

property_port_item_assignment:
   | p=port_sig v=variable_dimension_list_opt                           { mknode $startpos $endpos (L.PropertyPortItemAssignment p) v }
   | p=port_sig v=variable_dimension_list_opt EQ pa=property_actual_arg { mknode $startpos $endpos (L.PropertyPortItemAssignment p) (v @ [pa]) }
;

%inline
property_port_item_dir_opt:
   | (* empty *)            { [] }
   | LOCAL                  { [mkleaf $startpos $endpos L.PropertyPortItemDir] }
   | LOCAL p=port_direction { [mknode $startpos $endpos L.PropertyPortItemDir [p]] }
;

property_declaration_body:
   | assertion_variable_declaration_list property_statement_spec { mknode $startpos $endpos L.PropertyDeclBody ($1 @ [$2]) }
   |                                     property_statement_spec { mknode $startpos $endpos L.PropertyDeclBody [$1] }
;

assertion_variable_declaration_list:
   |                                     assertion_variable_declaration { [$1] }
   | assertion_variable_declaration_list assertion_variable_declaration { $1 @ [$2] }
;

sequence_declaration:
   | s=sequence_declaration_head sp=sequence_port_list_opt SEMICOLON sd=sequence_declaration_body ENDSEQUENCE e=end_label_opt 
       { 
	 mknode $startpos $endpos (L.SequenceDeclaration s) (sp @ [sd] @ e)
       }
;

sequence_declaration_head:
   | SEQUENCE i=id_any { i }
;

sequence_port_list_opt:
   | p=property_port_list_opt { p }
;

property_formal_type_no_data_type:
   | s=sequence_formal_type_no_data_type { s }
;

sequence_formal_type_no_data_type:
   | SEQUENCE { mkleaf $startpos $endpos L.SequenceFormalTypeSequence }
   | UNTYPED  { mkleaf $startpos $endpos L.SequenceFormalTypeUntyped }
;

sequence_declaration_body:
   | assertion_variable_declaration_list sexpr SEMICOLON { mknode $startpos $endpos L.SequenceDeclBody ($1 @ [$2]) }
   |                                     sexpr SEMICOLON { mknode $startpos $endpos L.SequenceDeclBody [$1] }
;

property_spec:
   | DISABLE IFF LPAREN expr RPAREN pexpr { mknode $startpos $endpos (L.pexpr LPE.Spec) [mknode $startpos $endpos($5) L.DisableIff [$4]; $6] }
   |                                pexpr { mknode $startpos $endpos (L.pexpr LPE.Spec) [$1] }
;

property_statement_spec:
   |                                property_statement { mknode $startpos $endpos L.PropertyStatementSpec [$1] }
   | DISABLE IFF LPAREN expr RPAREN property_statement 
       { 
	 mknode $startpos $endpos L.PropertyStatementSpec [mknode $startpos $endpos($5) L.DisableIff [$4]; $6] 
       }
   | clocking_event                                property_statement_case_if { mknode $startpos $endpos L.PropertyStatementSpec [$1; $2] }
   | clocking_event DISABLE IFF LPAREN expr RPAREN property_statement_case_if 
       { 
       	 mknode $startpos $endpos L.PropertyStatementSpec [mknode $startpos($2) $endpos($6) L.DisableIff [$5]; $1; $7] 
       }
;

property_statement:
   | p=pexpr SEMICOLON            { reloc $startpos $endpos p; p#pexpr_to_stmt; p }
   | p=property_statement_case_if { p#pexpr_to_stmt; p }
;

property_statement_case_if:
   | CASE LPAREN expr RPAREN property_case_item_list ENDCASE { mknode $startpos $endpos (L.pexpr LPE.Case) ($3::$5) }
   | CASE LPAREN expr RPAREN                         ENDCASE { mknode $startpos $endpos (L.pexpr LPE.Case) [$3] }
   | IF LPAREN expr RPAREN pexpr %prec P_LOWER_THAN_ELSE { mknode $startpos $endpos (L.pexpr LPE.If) [$3; $5] }
   | IF LPAREN expr RPAREN pexpr ELSE pexpr              { mknode $startpos $endpos (L.pexpr LPE.If) [$3; $5; $7] }
;

property_case_item_list:
   |                               property_case_item { [$1] }
   | property_case_item_list COMMA property_case_item { $1 @ [$3] }
;

property_case_item:
   | case_cond_list COLON property_statement { mknode $startpos $endpos L.PropertyCase ($1 @ [$3]) }
   | DEFAULT              property_statement { mknode $startpos $endpos L.PropertyCaseDefault [$2] }
   | DEFAULT        COLON property_statement { mknode $startpos $endpos L.PropertyCaseDefault [$3] }
;

pev_expr:
   | e=_pev_expr     { e }
   | e=EXPR          { check_error e; e }
   | e=PP_MACRO_EXPR { mkleaf $startpos $endpos (L.MacroExpr e) }
;

_pev_expr:
   | s=senitem_edge { s }
(* from pexpr *)
   | NOT p=pexpr %prec P_NEGATION { mknode $startpos $endpos (L.pexpr LPE.Not) [p] }
   | STRONG LPAREN s=sexpr RPAREN { mknode $startpos $endpos (L.pexpr LPE.Strong) [s] }
   | WEAK   LPAREN s=sexpr RPAREN { mknode $startpos $endpos (L.pexpr LPE.Weak) [s] }
   | p0=pev_expr PIPE_MINUS_GT     p1=pexpr { mknode $startpos $endpos (L.pexpr LPE.ImplicationOverlapped) [p0; p1] }
   | p0=pev_expr PIPE_EQ_GT        p1=pexpr { mknode $startpos $endpos (L.pexpr LPE.ImplicationNonOverlapped) [p0; p1] }
   | p0=pev_expr SHARP_MINUS_SHARP p1=pexpr { mknode $startpos $endpos (L.pexpr LPE.SharpMinusSharp) [p0; p1] }
   | p0=pev_expr SHARP_EQ_SHARP    p1=pexpr { mknode $startpos $endpos (L.pexpr LPE.SharpEqSharp) [p0; p1] }
   | p=property_statement_case_if { p }
   | NEXTTIME   p=pexpr { mknode $startpos $endpos (L.pexpr LPE.Nexttime) [p] }
   | S_NEXTTIME p=pexpr { mknode $startpos $endpos (L.pexpr LPE.S_nexttime) [p] }
   | NEXTTIME   LBRACKET e=expr RBRACKET p=pexpr %prec NEXTTIME   { mknode $startpos $endpos (L.pexpr LPE.Nexttime) [e; p] }
   | S_NEXTTIME LBRACKET e=expr RBRACKET p=pexpr %prec S_NEXTTIME { mknode $startpos $endpos (L.pexpr LPE.S_nexttime) [e; p] }
   | ALWAYS p=pexpr                                                                             { mknode $startpos $endpos (L.pexpr LPE.Always) [p] }
   | ALWAYS   LBRACKET c=cycle_delay_const_range_expression RBRACKET p=pexpr %prec ALWAYS       { mknode $startpos $endpos (L.pexpr LPE.Always) [c; p] }
   | S_ALWAYS LBRACKET c=constant_range                     RBRACKET p=pexpr %prec S_ALWAYS     { mknode $startpos $endpos (L.pexpr LPE.S_always) [c; p] }
   | S_EVENTUALLY p=pexpr                                                                       { mknode $startpos $endpos (L.pexpr LPE.S_eventually) [p] }
   | EVENTUALLY LBRACKET c=cycle_delay_const_range_expression RBRACKET p=pexpr %prec EVENTUALLY { mknode $startpos $endpos (L.pexpr LPE.Eventually) [c; p] }
   | p0=pev_expr UNTIL        p1=pexpr    { mknode $startpos $endpos (L.pexpr LPE.Until) [p0; p1] }
   | p0=pev_expr S_UNTIL      p1=pexpr    { mknode $startpos $endpos (L.pexpr LPE.S_until) [p0; p1] }
   | p0=pev_expr UNTIL_WITH   p1=pexpr    { mknode $startpos $endpos (L.pexpr LPE.Until_with) [p0; p1] }
   | p0=pev_expr S_UNTIL_WITH p1=pexpr    { mknode $startpos $endpos (L.pexpr LPE.S_until_with) [p0; p1] }
   | p0=pev_expr IMPLIES      p1=pexpr    { mknode $startpos $endpos (L.pexpr LPE.Implies) [p0; p1] }
   | p0=pev_expr IFF          p1=pev_expr { mknode $startpos $endpos (L.pexpr LPE.Iff) [p0; p1] }
   | ACCEPT_ON LPAREN e=expr RPAREN p=pexpr %prec ACCEPT_ON { mknode $startpos $endpos (L.pexpr LPE.Accept_on) [e; p] }
   | REJECT_ON LPAREN e=expr RPAREN p=pexpr %prec REJECT_ON { mknode $startpos $endpos (L.pexpr LPE.Reject_on) [e; p] }
   | SYNC_ACCEPT_ON LPAREN e=expr RPAREN p=pexpr %prec SYNC_ACCEPT_ON { mknode $startpos $endpos (L.pexpr LPE.Sync_accept_on) [e; p] }
   | SYNC_REJECT_ON LPAREN e=expr RPAREN p=pexpr %prec SYNC_REJECT_ON { mknode $startpos $endpos (L.pexpr LPE.Sync_reject_on) [e; p] }
   | c=clocking_event d=DISABLE IFF LPAREN e=expr r=RPAREN p=pexpr %prec P_SEQ_CLOCKING 
       { 
         ignore (d, r);
	 mknode $startpos $endpos (L.pexpr LPE.Spec) [c; mknode $startpos(d) $endpos(r) L.DisableIff [e]; p] 
       }
(* from sexpr *)
   |            c=cycle_delay_range s=sexpr %prec SHARP_SHARP         { mknode $startpos $endpos (L.sexpr LSE.Concat) [c; s] }
   | p=pev_expr c=cycle_delay_range s=sexpr %prec P_SHARP_SHARP_MULTI { mknode $startpos $endpos (L.sexpr LSE.Concat) [p; c; s] }
   | p=pev_expr b=boolean_abbrev                               { mknode $startpos $endpos (L.sexpr LSE.Repetition) [p; b] }
   | LPAREN p=pev_expr                                   RPAREN { mknode $startpos $endpos (L.sexpr LSE.OnMatch) [p] }
   | LPAREN p=pev_expr COMMA sl=sequence_match_item_list RPAREN { mknode $startpos $endpos (L.sexpr LSE.OnMatch) (p::sl) }
   | p0=pev_expr AND p1=pev_expr { mknode $startpos $endpos (L.sexpr LSE.And) [p0; p1] }
   | p0=pev_expr OR  p1=pev_expr { mknode $startpos $endpos (L.sexpr LSE.Or) [p0; p1] }
   | p=pev_expr INTERSECT s=sexpr                                        { mknode $startpos $endpos (L.sexpr LSE.Intersect) [p; s] }
   | FIRST_MATCH LPAREN s=sexpr                                   RPAREN { mknode $startpos $endpos (L.sexpr LSE.First_match) [s] }
   | FIRST_MATCH LPAREN s=sexpr COMMA sl=sequence_match_item_list RPAREN { mknode $startpos $endpos (L.sexpr LSE.First_match) (s::sl) }
   | p=pev_expr THROUGHOUT s=sexpr                                       { mknode $startpos $endpos (L.sexpr LSE.Throughout) [p; s] }
   | p=pev_expr WITHIN     s=sexpr                                       { mknode $startpos $endpos (L.sexpr LSE.Within) [p; s] }
   | c=clocking_event p=pev_expr %prec P_SEQ_CLOCKING                    { mknode $startpos $endpos (L.sexpr LSE.Clocking) [c; p] }
(* from expr *)
   | PLUS       e=expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [e] }
   | MINUS      e=expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [e] }
   | EXCLAM     e=expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [e] }
   | AMP        e=expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [e] }
   | TILDE      e=expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [e] }
   | PIPE       e=expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [e] }
   | HAT        e=expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [e] }
   | TILDE_AMP  e=expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [e] }
   | TILDE_PIPE e=expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [e] }
   | HAT_TILDE  e=expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [e] }
   | p=pev_inc_or_dec_expression { p }
   | LPAREN p=pev_expr_scope a=_aop e=expr RPAREN { mknode $startpos $endpos (L.expr_ao a) [p; e] }
   | e0=pev_expr b=_bop e1=expr { mknode $startpos $endpos b [e0; e1] }
   | pev_expr MINUS_GT constraint_set { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [$1; $3] }
   | pev_expr QUESTION           expr COLON expr   { mknode $startpos $endpos (L.expr LE.Cond) [$1; $3; $5] }
   | pev_expr inside_kw LBRACE open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) ($1::$4) }
   | TAGGED id %prec P_TAGGED { mkleaf $startpos $endpos (L.expr (LE.Tagged $2)) }
   | TAGGED id expr %prec P_TAGGED { mknode $startpos $endpos (L.expr (LE.Tagged $2)) [$3] }
   | integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber $1)) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber $1)) }
   | str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) ($2::$4) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr RBRACKET                  
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($9) L.ArrayRange [$8]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRange [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangeMinus [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangePlus [$8; $10]]) 
       }
   | function_subroutine_call_no_method          { $1 }
   | p=pev_expr DOT f=function_subroutine_call_no_method
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [p];
	 f
       }
   | p=pev_expr DOT a=array_method_no_root               { reloc $startpos $endpos a; a#add_children_l [p]; a }
   | LPAREN__IGN LPAREN expr                       RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$3] }
   | LPAREN__IGN LPAREN expr COLON expr COLON expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$3; $5; $7] }
   | UNDERSCORE LPAREN state_push expr state_pop RPAREN { dummy_node }
   | casting_type TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | pev_expr     TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | pev_expr_ok_lvalue { $1 }
   | pev_expr AMP_AMP_AMP expr        { mknode $startpos $endpos L.CondPredicate [$1; $3] }
   | pev_expr MATCHES MATCHES_ pattern_no_expr { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | pev_expr MATCHES MATCHES_ expr            { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | pev_expr DIST LBRACE dist_list RBRACE { mknode $startpos $endpos L.Dist $4 }
;

pexpr: (* property_expr *)
   | NOT pexpr %prec P_NEGATION { mknode $startpos $endpos (L.pexpr LPE.Not) [$2] }
   | STRONG LPAREN sexpr RPAREN { mknode $startpos $endpos (L.pexpr LPE.Strong) [$3] }
   | WEAK   LPAREN sexpr RPAREN { mknode $startpos $endpos (L.pexpr LPE.Weak) [$3] }
   | pexpr PIPE_MINUS_GT pexpr { mknode $startpos $endpos (L.pexpr LPE.ImplicationOverlapped) [$1; $3] }
   | pexpr PIPE_EQ_GT    pexpr { mknode $startpos $endpos (L.pexpr LPE.ImplicationNonOverlapped) [$1; $3] }
   | property_statement_case_if { $1 }
   | pexpr SHARP_MINUS_SHARP pexpr { mknode $startpos $endpos (L.pexpr LPE.SharpMinusSharp) [$1; $3] }
   | pexpr SHARP_EQ_SHARP    pexpr { mknode $startpos $endpos (L.pexpr LPE.SharpEqSharp) [$1; $3] }
   | NEXTTIME   pexpr { mknode $startpos $endpos (L.pexpr LPE.Nexttime) [$2] }
   | S_NEXTTIME pexpr { mknode $startpos $endpos (L.pexpr LPE.S_nexttime) [$2] }
   | NEXTTIME   LBRACKET expr RBRACKET pexpr %prec NEXTTIME   { mknode $startpos $endpos (L.pexpr LPE.Nexttime) [$3; $5] }
   | S_NEXTTIME LBRACKET expr RBRACKET pexpr %prec S_NEXTTIME { mknode $startpos $endpos (L.pexpr LPE.S_nexttime) [$3; $5] }
   | ALWAYS pexpr { mknode $startpos $endpos (L.pexpr LPE.Always) [$2] }
   | ALWAYS   LBRACKET cycle_delay_const_range_expression RBRACKET pexpr %prec ALWAYS   { mknode $startpos $endpos (L.pexpr LPE.Always) [$3; $5] }
   | S_ALWAYS LBRACKET constant_range                     RBRACKET pexpr %prec S_ALWAYS { mknode $startpos $endpos (L.pexpr LPE.S_always) [$3; $5] }
   | S_EVENTUALLY pexpr { mknode $startpos $endpos (L.pexpr LPE.S_eventually) [$2] }
   | EVENTUALLY LBRACKET cycle_delay_const_range_expression RBRACKET pexpr %prec EVENTUALLY { mknode $startpos $endpos (L.pexpr LPE.Eventually) [$3; $5] }
   | pexpr UNTIL        pexpr { mknode $startpos $endpos (L.pexpr LPE.Until) [$1; $3] }
   | pexpr S_UNTIL      pexpr { mknode $startpos $endpos (L.pexpr LPE.S_until) [$1; $3] }
   | pexpr UNTIL_WITH   pexpr { mknode $startpos $endpos (L.pexpr LPE.Until_with) [$1; $3] }
   | pexpr S_UNTIL_WITH pexpr { mknode $startpos $endpos (L.pexpr LPE.S_until_with) [$1; $3] }
   | pexpr IMPLIES      pexpr { mknode $startpos $endpos (L.pexpr LPE.Implies) [$1; $3] }
   | pexpr IFF          pexpr { mknode $startpos $endpos (L.pexpr LPE.Iff) [$1; $3] }
   | ACCEPT_ON LPAREN expr RPAREN pexpr %prec ACCEPT_ON { mknode $startpos $endpos (L.pexpr LPE.Accept_on) [$3; $5] }
   | REJECT_ON LPAREN expr RPAREN pexpr %prec REJECT_ON { mknode $startpos $endpos (L.pexpr LPE.Reject_on) [$3; $5] }
   | SYNC_ACCEPT_ON LPAREN expr RPAREN pexpr %prec SYNC_ACCEPT_ON { mknode $startpos $endpos (L.pexpr LPE.Sync_accept_on) [$3; $5] }
   | SYNC_REJECT_ON LPAREN expr RPAREN pexpr %prec SYNC_REJECT_ON { mknode $startpos $endpos (L.pexpr LPE.Sync_reject_on) [$3; $5] }
   | clocking_event DISABLE IFF LPAREN expr RPAREN pexpr %prec P_SEQ_CLOCKING 
       { 
	 mknode $startpos $endpos (L.pexpr LPE.Spec) [$1; mknode $startpos($2) $endpos($6) L.DisableIff [$5]; $7] 
       }
(* from sexpr *)
   |       cycle_delay_range sexpr %prec SHARP_SHARP         { mknode $startpos $endpos (L.sexpr LSE.Concat) [$1; $2] }
   | pexpr cycle_delay_range sexpr %prec P_SHARP_SHARP_MULTI { mknode $startpos $endpos (L.sexpr LSE.Concat) [$1; $2; $3] }
   | pexpr boolean_abbrev { mknode $startpos $endpos (L.sexpr LSE.Repetition) [$1; $2] }
   | LPAREN pexpr                                RPAREN { mknode $startpos $endpos (L.sexpr LSE.OnMatch) [$2] }
   | LPAREN pexpr COMMA sequence_match_item_list RPAREN { mknode $startpos $endpos (L.sexpr LSE.OnMatch) ($2::$4) }
   | pexpr AND       pexpr { mknode $startpos $endpos (L.sexpr LSE.And) [$1; $3] }
   | pexpr OR        pexpr { mknode $startpos $endpos (L.sexpr LSE.Or) [$1; $3] }
   | pexpr INTERSECT sexpr { mknode $startpos $endpos (L.sexpr LSE.Intersect) [$1; $3] }
   | FIRST_MATCH LPAREN sexpr                                RPAREN { mknode $startpos $endpos (L.sexpr LSE.First_match) [$3] }
   | FIRST_MATCH LPAREN sexpr COMMA sequence_match_item_list RPAREN { mknode $startpos $endpos (L.sexpr LSE.First_match) ($3::$5) }
   | pexpr THROUGHOUT sexpr { mknode $startpos $endpos (L.sexpr LSE.Throughout) [$1; $3] }
   | pexpr WITHIN     sexpr { mknode $startpos $endpos (L.sexpr LSE.Within) [$1; $3] }
   | clocking_event pexpr %prec P_SEQ_CLOCKING { mknode $startpos $endpos (L.sexpr LSE.Clocking) [$1; $2] }
(* from expr *)
   | PLUS       expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [$2] }
   | MINUS      expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [$2] }
   | EXCLAM     expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [$2] }
   | AMP        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [$2] }
   | TILDE      expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [$2] }
   | PIPE       expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [$2] }
   | HAT        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [$2] }
   | TILDE_AMP  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [$2] }
   | TILDE_PIPE expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [$2] }
   | HAT_TILDE  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [$2] }
   | pinc_or_dec_expression { $1 }
   | LPAREN p=pexpr_scope a=_aop e=expr RPAREN { mknode $startpos $endpos (L.expr_ao a) [p; e] }
   | p=pexpr b=_bop e=expr { mknode $startpos $endpos b [p; e] }
   | pexpr MINUS_GT constraint_set { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [$1; $3] }
   | pexpr QUESTION expr COLON expr { mknode $startpos $endpos (L.expr LE.Cond) [$1; $3; $5] }
   | pexpr inside_kw LBRACE open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) ($1::$4) }
   | TAGGED id %prec P_TAGGED { mkleaf $startpos $endpos (L.expr (LE.Tagged $2)) }
   | TAGGED id expr %prec P_TAGGED { mknode $startpos $endpos (L.expr (LE.Tagged $2)) [$3] }
   | integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber $1)) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber $1)) }
   | str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) ($2::$4) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr RBRACKET                  
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($9) L.ArrayRange [$8]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRange [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangeMinus [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangePlus [$8; $10]]) 
       }
   | function_subroutine_call_no_method           { $1 }
   | p=pexpr DOT f=function_subroutine_call_no_method 
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [p];
	 f
       }
   | p=pexpr DOT a=array_method_no_root { reloc $startpos $endpos a; a#add_children_l [p]; a }
   | LPAREN__IGN LPAREN expr                       RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$3] }
   | LPAREN__IGN LPAREN expr COLON expr COLON expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$3; $5; $7] }
   | UNDERSCORE LPAREN state_push expr state_pop RPAREN { dummy_node }
   | casting_type TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | pexpr TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | pexpr_ok_lvalue { $1 }
   | pexpr AMP_AMP_AMP expr        { mknode $startpos $endpos L.CondPredicate [$1; $3] }
   | pexpr MATCHES MATCHES_ pattern_no_expr { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | pexpr MATCHES MATCHES_ expr            { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | pexpr DIST LBRACE dist_list RBRACE { mknode $startpos $endpos L.Dist $4 }
;

sexpr: (* sequence_expr *)
   |       cycle_delay_range sexpr %prec SHARP_SHARP         { mknode $startpos $endpos (L.sexpr LSE.Concat) [$1; $2] }
   | sexpr cycle_delay_range sexpr %prec P_SHARP_SHARP_MULTI { mknode $startpos $endpos (L.sexpr LSE.Concat) [$1; $2; $3] }
   | sexpr boolean_abbrev { mknode $startpos $endpos (L.sexpr LSE.Repetition) [$1; $2] }
   | LPAREN sexpr                                RPAREN { mknode $startpos $endpos (L.sexpr LSE.OnMatch) [$2] }
   | LPAREN sexpr COMMA sequence_match_item_list RPAREN { mknode $startpos $endpos (L.sexpr LSE.OnMatch) ($2::$4) }
   | sexpr AND       sexpr { mknode $startpos $endpos (L.sexpr LSE.And) [$1; $3] }
   | sexpr OR        sexpr { mknode $startpos $endpos (L.sexpr LSE.Or) [$1; $3] }
   | sexpr INTERSECT sexpr { mknode $startpos $endpos (L.sexpr LSE.Intersect) [$1; $3] }
   | FIRST_MATCH LPAREN sexpr                                RPAREN { mknode $startpos $endpos (L.sexpr LSE.First_match) [$3] }
   | FIRST_MATCH LPAREN sexpr COMMA sequence_match_item_list RPAREN { mknode $startpos $endpos (L.sexpr LSE.First_match) ($3::$5) }
   | sexpr THROUGHOUT sexpr { mknode $startpos $endpos (L.sexpr LSE.Throughout) [$1; $3] }
   | sexpr WITHIN     sexpr { mknode $startpos $endpos (L.sexpr LSE.Within) [$1; $3] }
   | clocking_event sexpr %prec P_SEQ_CLOCKING { mknode $startpos $endpos (L.sexpr LSE.Clocking) [$1; $2] }
(* from expr *)
   | PLUS       expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Plus) [$2] }
   | MINUS      expr %prec P_UNARY_ARITH { mknode $startpos $endpos (L.expr_uo LUO.Minus) [$2] }
   | EXCLAM     expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Not) [$2] }
   | AMP        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.And) [$2] }
   | TILDE      expr %prec P_NEGATION    { mknode $startpos $endpos (L.expr_uo LUO.Neg) [$2] }
   | PIPE       expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Or) [$2] }
   | HAT        expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xor) [$2] }
   | TILDE_AMP  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nand) [$2] }
   | TILDE_PIPE expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Nor) [$2] }
   | HAT_TILDE  expr %prec P_REDUCTION   { mknode $startpos $endpos (L.expr_uo LUO.Xnor) [$2] }
   | sinc_or_dec_expression { $1 }
   | LPAREN s=sexpr_scope a=_aop e=expr RPAREN { mknode $startpos $endpos (L.expr_ao a) [s; e] }
   | s=sexpr b=_bop e=expr { mknode $startpos $endpos b [s; e] }
   | sexpr MINUS_GT constraint_set { mknode $startpos $endpos (L.expr_bo LBO.Constraint) [$1; $3] }
   | sexpr QUESTION           expr COLON expr   { mknode $startpos $endpos (L.expr LE.Cond) [$1; $3; $5] }
   | sexpr inside_kw LBRACE open_range_list RBRACE { mknode $startpos $endpos (L.expr LE.Inside) ($1::$4) }
   | TAGGED id %prec P_TAGGED { mkleaf $startpos $endpos (L.expr (LE.Tagged $2)) }
   | TAGGED id expr %prec P_TAGGED { mknode $startpos $endpos (L.expr (LE.Tagged $2)) [$3] }
   | integral_number { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | REAL_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.RealNumber $1)) }
   | TIME_NUMBER     { mkleaf $startpos $endpos (L.expr (LE.TimeNumber $1)) }
   | str_as_int      { mkleaf $startpos $endpos (L.expr (LE.IntegralNumber $1)) }
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos (L.expr LE.EmptyQueue) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE { mknode $startpos $endpos (L.expr LE.Concat) ($2::$4) }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr RBRACKET                  
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($9) L.ArrayRange [$8]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr COLON       expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRange [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr PLUS_COLON  expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangePlus [$8; $10]]) 
       }
   | LBRACE const_expr LBRACE cate_list RBRACE RBRACE LBRACKET expr MINUS_COLON expr RBRACKET 
       { 
	 mknode $startpos $endpos (L.expr LE.Concat) (($2::$4) @ [mknode $startpos($7) $endpos($11) L.ArrayRangeMinus [$8; $10]]) 
       }
   | function_subroutine_call_no_method          { $1 }
   | s=sexpr DOT f=function_subroutine_call_no_method 
       { 
	 reloc $startpos $endpos f; f#relab (L.expr (LE.MethodCall f#get_identifier)); f#add_children_l [s];
	 f
       }
   | s=sexpr DOT a=array_method_no_root               { reloc $startpos $endpos a; a#add_children_l [s]; a }
   | LPAREN__IGN LPAREN expr                       RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$3] }
   | LPAREN__IGN LPAREN expr COLON expr COLON expr RPAREN { mknode $startpos $endpos (L.expr LE.MinTypeMax) [$3; $5; $7] }
   | UNDERSCORE LPAREN state_push expr state_pop RPAREN { dummy_node }
   | casting_type TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | sexpr         TICK LPAREN expr RPAREN { mknode $startpos $endpos (L.expr LE.Cast) [$1; $4] }
   | DOLLAR { mkleaf $startpos $endpos (L.expr LE.Last) }
   | NULL   { mkleaf $startpos $endpos (L.expr LE.Null) }
   | sexpr_ok_lvalue { $1 }
   | sexpr AMP_AMP_AMP expr        { mknode $startpos $endpos L.CondPredicate [$1; $3] }
   | sexpr MATCHES MATCHES_ pattern_no_expr { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | sexpr MATCHES MATCHES_ expr            { mknode $startpos $endpos L.CondPattern [$1; $4] }
   | sexpr DIST LBRACE dist_list RBRACE { mknode $startpos $endpos L.Dist $4 }
;

cycle_delay_range:
   | SHARP_SHARP INTEGRAL_NUMBER          { mkleaf $startpos $endpos (L.CycleDelayRange $2) }
   | SHARP_SHARP id                       { mkleaf $startpos $endpos (L.CycleDelayRangeId $2) }
   | SHARP_SHARP LPAREN const_expr RPAREN { mknode $startpos $endpos L.CycleDelayRangeParen [$3] }
   | SHARP_SHARP LBRACKET cycle_delay_const_range_expression RBRACKET { mknode $startpos $endpos L.CycleDelayRangeBracket [$3] }
   | SHARP_SHARP LBRACKET_STAR                               RBRACKET { mkleaf $startpos $endpos L.CycleDelayRangeBracketStar }
   | SHARP_SHARP LBRACKET_PLUS_RBRACKET                               { mkleaf $startpos $endpos L.CycleDelayRangeBracketPlus }
;

sequence_match_item_list:
   |                                sequence_match_item { [$1] }
   | sequence_match_item_list COMMA sequence_match_item { $1 @ [$3] }
;

sequence_match_item:
   | for_step_assignment { $1 }
;

boolean_abbrev:
   | LBRACKET_STAR const_or_range_expression     RBRACKET { mknode $startpos $endpos (L.ConsecutiveRepetition) [$2] }
   | LBRACKET_STAR                               RBRACKET { mkleaf $startpos $endpos (L.ConsecutiveRepetition) }
   | LBRACKET_PLUS_RBRACKET                               { mkleaf $startpos $endpos (L.ConsecutiveRepetition) }
   | LBRACKET_EQ       const_or_range_expression RBRACKET { mknode $startpos $endpos (L.NonconsecutiveRepetition) [$2] }
   | LBRACKET_MINUS_GT const_or_range_expression RBRACKET { mknode $startpos $endpos (L.GotoRepetition) [$2] }
;

const_or_range_expression:
   | const_expr                         { $1 }
   | cycle_delay_const_range_expression { $1 }
;

constant_range:
   | const_expr                  { mknode $startpos $endpos (L.expr LE.ConstantRange) [$1] }
   | const_expr COLON const_expr { mknode $startpos $endpos (L.expr LE.ConstantRange) [$1; $3] }
;

cycle_delay_const_range_expression:
   | const_expr COLON const_expr { mknode $startpos $endpos (L.expr LE.CycleDelayConstRange) [$1; $3] }
;

let_declaration:
   | let_declaration_head let_port_list_opt EQ expr SEMICOLON { mknode $startpos $endpos (L.LetDeclaration $1) ($2 @ [$4]) }
;

let_declaration_head:
   | LET id_any { $2 }
;

let_port_list_opt:
   | (* empty *)                      { [] }
   | LPAREN t=tf_port_list_opt RPAREN { t }
;

covergroup_declaration:
   | c=covergroup_declaration_head                                      ce=coverage_event_opt SEMICOLON cs=coverage_spec_or_option_list_opt ENDGROUP e=end_label_opt 
       { 
	 mknode $startpos $endpos (L.CovergroupDeclaration c) (ce @ cs @ e)
       }
   | c=covergroup_declaration_head l=LPAREN t=tf_port_list_opt r=RPAREN ce=coverage_event_opt SEMICOLON cs=coverage_spec_or_option_list_opt ENDGROUP e=end_label_opt 
       { 
	 ignore (l, r);
	 mknode $startpos $endpos (L.CovergroupDeclaration c) ((mknode $startpos(l) $endpos(r) L.Paren t)::(ce @ cs @ e))
       }
;

covergroup_declaration_head:
   | COVERGROUP i=id_any { register_covergroup i; i }
;

coverage_spec_or_option_list_opt:
   | (* empty *)                     { [] }
   | cl=coverage_spec_or_option_list { cl }
;

coverage_spec_or_option_list:
   |                                 c=coverage_spec_or_option { [c] }
   | cl=coverage_spec_or_option_list c=coverage_spec_or_option { cl @ [c] }
;

coverage_spec_or_option:
   | cover_point               { $1 }
   | cover_cross               { $1 }
   | coverage_option SEMICOLON { reloc $startpos $endpos $1; $1 }
;

coverage_option:
   | id DOT id_any EQ expr { mknode $startpos $endpos (L.CoverageOption($1, $3)) [$5] }
;

cover_point:
   | i=id COLON COVERPOINT e=expr io=iff_opt b=bins_or_empty { mknode $startpos $endpos (L.CoverPointLabeled i) (e :: io @ [b]) }
   |            COVERPOINT e=expr io=iff_opt b=bins_or_empty { mknode $startpos $endpos L.CoverPoint (e :: io @ [b]) }
;

%inline
iff_opt:
   | (* empty *)              { [] }
   | IFF LPAREN e=expr RPAREN { [mknode $startpos $endpos L.Iff [e]] }
;

bins_or_empty:
   | LBRACE bins_or_options_list RBRACE { mknode $startpos $endpos L.BinsList $2 }
   | LBRACE                      RBRACE { mkleaf $startpos $endpos L.BinsList }
   | SEMICOLON                          { mkleaf $startpos $endpos L.BinsEmpty }
;

bins_or_options_list:
   |                      bins_or_options SEMICOLON { [$1] }
   | bins_or_options_list bins_or_options SEMICOLON { $1 @ [$2] }
;

%inline
wildcard_opt:
   | (* empty *) { [] }
   | WILDCARD    { [mkleaf $startpos $endpos L.Wildcard] }
;

%inline
nbins_opt:
   | (* empty *)            { [] }
   | LBRACKET e=expr RBRACKET { [mknode $startpos $endpos L.NBins [e]] }
   | LBRACKET          RBRACKET { [mkleaf $startpos $endpos L.NBins] }
;

%inline
brackets_opt:
   | (* empty *)       { [] }
   | LBRACKET RBRACKET { [mkleaf $startpos $endpos L.NBins] }
;

%inline
brace_open_range_list:
   | LBRACE o=open_range_list RBRACE { mknode $startpos $endpos L.OpenRangeList o }
;

%inline
default_sequence:
   | DEFAULT SEQUENCE { mkleaf $startpos $endpos L.DefaultSequence }
;

bins_or_options:
   | coverage_option { $1 }
   | w=wildcard_opt b=bins_keyword id=id n=nbins_opt     EQ o=brace_open_range_list i=iff_opt { mknode $symbolstartpos $endpos (L.Bins(b, id)) (w @ n @ [o] @ i) }
   | w=wildcard_opt b=bins_keyword id=id br=brackets_opt EQ t=trans_list            i=iff_opt { mknode $symbolstartpos $endpos (L.Bins(b, id)) (w @ br @ t @ i) }
   |                b=bins_keyword id=id n=nbins_opt     EQ d=default               i=iff_opt { mknode $startpos $endpos (L.Bins(b, id)) (n @ [d] @ i) }
   |                b=bins_keyword id=id                 EQ d=default_sequence      i=iff_opt { mknode $startpos $endpos (L.Bins(b, id)) (d::i) }
;

bins_keyword:
   | BINS         { Ls.BinsSpec.Normal }
   | ILLEGAL_BINS { Ls.BinsSpec.Illegal }
   | IGNORE_BINS  { Ls.BinsSpec.Ignore }
;

value_range_list:
   |                        value_range { [$1] }
   | value_range_list COMMA value_range { $1 @ [$3] }
;

trans_list:
   |                  LPAREN trans_set RPAREN { reloc $startpos $endpos $2; [$2] }
   | trans_list COMMA LPAREN trans_set RPAREN { reloc $startpos($3) $endpos($5) $4; $1 @ [$4] }
;

trans_set:
   |                 trans_range_list { mknode $startpos $endpos L.TransSet [$1] }
   | trans_set EQ_GT trans_range_list { mknode $startpos $endpos L.TransSet [$1; $3] }
;

%inline
trans_repetition:
   | LBRACKET_STAR     r=repeat_range RBRACKET { mknode $startpos $endpos L.TransRepetitionConsecutive [r] }
   | LBRACKET_MINUS_GT r=repeat_range RBRACKET { mknode $startpos $endpos L.TransRepetitionGoto [r] }
   | LBRACKET_EQ       r=repeat_range RBRACKET { mknode $startpos $endpos L.TransRepetitionNonconsecutive [r] }
;

trans_range_list:
   |    trans_item                     { mknode $startpos $endpos L.TransRangeList [$1] }
   | ti=trans_item tr=trans_repetition { mknode $startpos $endpos L.TransRangeList [ti; tr] }
;

trans_item:
   | value_range_list { mknode $startpos $endpos L.TransItem $1 }
;

repeat_range:
   | expr            { mknode $startpos $endpos L.RepeatRange [$1] }
   | expr COLON expr { mknode $startpos $endpos L.RepeatRange [$1; $3] }
;

cover_cross:
   | i=id COLON CROSS c=coverpoints_list io=iff_opt { mknode $startpos $endpos (L.CoverCrossLabeled i) (c @ io) }
   |            CROSS c=coverpoints_list io=iff_opt { mknode $startpos $endpos L.CoverCross (c @ io) }
   | select_bins_or_empty { $1 }
;

coverpoints_list:
   | cross_item COMMA cross_item                       { [$1; $3] }
   | cross_item COMMA cross_item COMMA cross_item_list { [$1; $3] @ $5 }
;

cross_item_list:
   |                       cross_item { [$1] }
   | cross_item_list COMMA cross_item { $1 @ [$3] }
;

cross_item:
   | id_any { mkleaf $startpos $endpos (L.CrossItem $1) }
;

select_bins_or_empty:
   | LBRACE                                    RBRACE { mkleaf $startpos $endpos L.SelectBins }
   | LBRACE bins_selection_or_option_semi_list RBRACE { mknode $startpos $endpos L.SelectBins $2 }
   | SEMICOLON                                        { mkleaf $startpos $endpos L.SelectBinsEmpty  }
;

bins_selection_or_option_semi_list:
   |                                    bins_selection_or_option SEMICOLON { reloc $startpos $endpos $1; [$1] }
   | bins_selection_or_option_semi_list bins_selection_or_option SEMICOLON { reloc $startpos($2) $endpos $2; $1 @ [$2] }
;

bins_selection_or_option:
   | coverage_option { $1 }
   | bins_selection  { $1 }
;

bins_selection:
   | b=bins_keyword i=id_any EQ s=select_expression io=iff_opt { mknode $startpos $endpos (L.BinsSelection(b, i)) (s::io) }
;

select_expression:
   |        select_condition { $1 }
   | EXCLAM select_condition { mknode $startpos $endpos L.SelExprNot [$2] }
   | select_expression AMP_AMP   select_expression { mknode $startpos $endpos L.SelExprAnd [$1; $3] }
   | select_expression PIPE_PIPE select_expression { mknode $startpos $endpos L.SelExprOr [$1; $3] }
   | LPAREN select_expression RPAREN { mknode $startpos $endpos L.SelExprParen [$2] }
;

select_condition:
   | BINSOF LPAREN bins_expression RPAREN                                         { mknode $startpos $endpos L.SelCondBinsof [$3] }
   | BINSOF LPAREN bins_expression RPAREN INTERSECT LBRACE open_range_list RBRACE 
       { 
	 mknode $startpos $endpos L.SelCondBinsof [$3; mknode $startpos($5) $endpos($8) L.Intersect $7] 
       }
;

bins_expression:
   | id            { mkleaf $startpos $endpos (L.BinsExpressionVar $1) }
   | id DOT id_any { mkleaf $startpos $endpos (L.BinsExpression($1, $3)) }
;

coverage_event_opt:
   | (* empty *)                                              { [] }
   | clocking_event                                           { [$1] }
   | WITHx function_kw id_any LPAREN tf_port_list_opt RPAREN { [mknode $startpos $endpos (L.CoverageEventWith $3) $5] }
   | AT_AT LPAREN block_event_expression RPAREN               { [mknode $startpos $endpos L.CoverageEventBlockEvent [$3]] }
;

block_event_expression:
   | block_event_expression_term                                { $1 }
   | block_event_expression_term OR block_event_expression_term { mknode $startpos $endpos L.BlockEventExpression [$1; $3] }
;

block_event_expression_term:
   | begin_kw hierarchical_btf_identifier { mknode $startpos $endpos L.BlockEventExpressionBegin [$2] }
   | end_kw   hierarchical_btf_identifier { mknode $startpos $endpos L.BlockEventExpressionEnd [$2] }
;

hierarchical_btf_identifier:
   | hierarchical_identifier                { $1 }
   | hierarchical_identifier class_scope_id { mknode $startpos $endpos (L.HierarchicalBtfIdentifier "") [$1; $2] }
   | hierarchical_identifier id             { mknode $startpos $endpos (L.HierarchicalBtfIdentifier $2) [$1] }
;

randsequence_statement:
   | RANDSEQUENCE LPAREN    RPAREN production_list ENDSEQUENCE { mknode $startpos $endpos (L.stmt (LS.Randsequence "")) $4 }
   | RANDSEQUENCE LPAREN id RPAREN production_list ENDSEQUENCE { mknode $startpos $endpos (L.stmt (LS.Randsequence $3)) $5 }
;

production_list:
   |                 production { [$1] }
   | production_list production { $1 @ [$2] }
;

production:
   | production_head COLON rs_rule_list SEMICOLON 
       { 
	 let fdt, id, ps = $1 in
	 mknode $startpos $endpos (L.Production id) (fdt @ ps @ $3)
       }
;

production_head:
   | function_data_type id                                { [$1], $2, [] }
   |                    id                                { [], $1, [] }
   | function_data_type id LPAREN tf_port_list_opt RPAREN { [$1], $2, [mknode $startpos($3) $endpos($5) L.Ports $4] }
   |                    id LPAREN tf_port_list_opt RPAREN { [], $1, [mknode $startpos($2) $endpos($4) L.Ports $3] }
;

rs_rule_list:
   |              rs_rule { [$1] }
   | rs_rule PIPE rs_rule { [$1; $3] }
;

rs_rule:
   | rs_production_list                                             { mknode $startpos $endpos L.RsRule [$1] }
   | rs_production_list COLON_EQ weight_specification               { mknode $startpos $endpos L.RsRule [$1; $3] }
   | rs_production_list COLON_EQ weight_specification rs_code_block { mknode $startpos $endpos L.RsRule [$1; $3; $4] }
;

rs_production_list:
   | rs_prod_list                                                         { mknode $startpos $endpos L.RsProductionList $1 }
   | RAND join_kw                    production_item production_item_list { mknode $startpos $endpos L.RsProductionListRandJoin ($3::$4) }
   | RAND join_kw LPAREN expr RPAREN production_item production_item_list { mknode $startpos $endpos L.RsProductionListRandJoin ($4::$6::$7) }
;

weight_specification:
   | integral_number    { mkleaf $startpos $endpos (L.WeightSpecInt $1) }
   | id_class_sel       { mknode $startpos $endpos L.WeightSpecId [$1] }
   | LPAREN expr RPAREN { mknode $startpos $endpos L.WeightSpec [$2] }
;

rs_code_block:
   | LBRACE                         RBRACE { mkleaf $startpos $endpos L.RsCodeBlock }
   | LBRACE rs_code_block_item_list RBRACE { mknode $startpos $endpos L.RsCodeBlock $2 }
;

rs_code_block_item_list:
   |                         rs_code_block_item { [$1] }
   | rs_code_block_item_list rs_code_block_item { $1 @ [$2] }
;

rs_code_block_item:
   | d=data_declaration { d }
   | s=stmt             { s }
;

rs_prod_list:
   |              rs_prod { [$1] }
   | rs_prod_list rs_prod { $1 @ [$2] }
;

rs_prod:
   | production_item { $1 }
   | rs_code_block   { $1 }
   | IF LPAREN expr RPAREN production_item (* %prec P_LOWER_THAN_ELSE *) { mknode $startpos $endpos L.RsProdIf [$3; $5] }
   | IF LPAREN expr RPAREN production_item ELSE production_item          { mknode $startpos $endpos L.RsProdIf [$3; $5; $7] }
   | REPEAT LPAREN expr RPAREN production_item                           { mknode $startpos $endpos L.RsProdRepeat [$3; $5] }
   | CASE   LPAREN expr RPAREN rs_case_item_list ENDCASE                 { mknode $startpos $endpos L.RsProdCase ($3::$5) }
;

production_item_list:
   |                      production_item { [$1] }
   | production_item_list production_item { $1 @ [$2] }
;

production_item:
   | id                                 { mkleaf $startpos $endpos (L.ProductionItem $1) }
   | id LPAREN argument_list_opt RPAREN { mknode $startpos $endpos (L.ProductionItem $1) $3 }
;

rs_case_item_list:
   |                   rs_case_item { [$1] }
   | rs_case_item_list rs_case_item { $1 @ [$2] }
;

rs_case_item:
   | case_cond_list COLON production_item SEMICOLON { mknode $startpos $endpos L.RsCaseItem ($1 @ [$3]) }
   | DEFAULT              production_item SEMICOLON { mknode $startpos $endpos L.RsCaseItemDefault [$2] }
   | DEFAULT        COLON production_item SEMICOLON { mknode $startpos $endpos L.RsCaseItemDefault [$3] }
;

checker_declaration:
   | c=checker_head cp=checker_port_list_opt SEMICOLON cg=checker_or_generate_item_list_opt ENDCHECKER e=end_label_opt 
       { 
	 mknode $startpos $endpos (L.CheckerDeclaration c) (cp @ cg @ e)
       }
;

checker_head:
   | CHECKER i=id_any { i }
;

checker_port_list_opt:
   | p=property_port_list_opt { p }
;

checker_or_generate_item_list_opt:
   | (* empty *)                   { [] }
   | checker_or_generate_item_list { $1 }
;

checker_or_generate_item_list:
   |                               checker_or_generate_item { [$1] }
   | checker_or_generate_item_list checker_or_generate_item { $1 @ [$2] }
;

checker_or_generate_item:
   | checker_or_generate_item_declaration { $1 }
   | initial_construct                    { $1 }
   | ALWAYS stmt                          { mknode $startpos $endpos (L.AlwaysConstruct $1) [$2] }
   | final_construct                      { $1 }
   | assertion_item                       { $1 }
   | checker_generate_item                { $1 }
;

checker_or_generate_item_declaration:
   |      data_declaration      { $1 }
   | r=RAND d=data_declaration  { ignore r; reloc $startpos $endpos d; d#add_children_l [mkleaf $startpos $endpos(r) (L.qualifier LQ.Rand)]; d }
   | function_declaration       { $1 }
   | assertion_item_declaration { $1 }
   | covergroup_declaration     { $1 }
   | overload_declaration       { $1 }
   | genvar_declaration         { $1 }
   | clocking_declaration       { $1 }
   | default CLOCKING id      SEMICOLON { mknode $startpos $endpos (L.ClockingDeclaration $3) [$1] }
   | default DISABLE IFF expr SEMICOLON { mknode $startpos $endpos L.DisableIff [$1; $4] }
   | SEMICOLON { mkleaf $startpos $endpos L.CheckerOrGenerateItemEmpty }
;

checker_generate_item:
   | c=c_loop_generate_construct        { c }
   | c=c_conditional_generate_construct { c }
   | c=c_generate_region                { c }
   | e=elaboration_system_task          { e }
;

checker_instantiation:
   | cinst=checker_instantiation_ SEMICOLON { reloc $startpos $endpos cinst; cinst }
;

checker_instantiation_:
   | cinst_head=checker_instantiation_head cellpins=cellpin_list RPAREN
       { 
	 context_stack#pop;
	 let inst_name, instname_nd = cinst_head in
	 mknode $startpos $endpos (L.CheckerInstantiation inst_name) (instname_nd::cellpins) 
       }
;

checker_instantiation_head:
   | cinst_head=checker_instantiation_head0 LPAREN { cinst_head }
;

%inline
checker_instantiation_head0:
   | inst_name=id instname=id { context_stack#push (Context.cellpin_list()); inst_name, mkleaf $startpos(instname) $endpos(instname) (L.InstName instname) }
;

class_declaration:
   | c=class_declaration_head p=parameter_port_list_opt ce=class_extends_opt SEMICOLON ci=class_item_list_opt ENDCLASS e=end_label_opt 
       { 
	 end_scope();
	 let v, l, id = c in
	 mknode $symbolstartpos $endpos (L.ClassDeclaration id) (v @ l @ p @ ce @ [mknode $startpos(ci) $endpos(ci) L.ClassBody ci] @ e)
       }
;

class_declaration_head:
   | c=class_virtual_opt CLASS l=lifetime_opt i=id_any { register_class i; begin_class_scope i; c, l, i }
;

class_virtual_opt:
   | (* empty *) { [] }
   | VIRTUAL__C  { [mkleaf $startpos $endpos L.Virtual] }
;

class_extends_opt:
   | (* empty *)                                                   { [] }
   | EXTENDS class_type_without_id                                 
       { 
	 import_pkg_cls_scope (get_scope_of_pkg_ctys $2);
	 [mknode $startpos $endpos L.ClassExtends [mknode $startpos($2) $endpos (L.data_type LDT.ClassType) $2]] 
       }
   | EXTENDS class_type_without_id LPAREN argument_list_opt RPAREN 
       { 
	 import_pkg_cls_scope (get_scope_of_pkg_ctys $2);
	 [mknode $startpos $endpos L.ClassExtends ([mknode $startpos($2) $endpos (L.data_type LDT.ClassType) $2] @ $4)] 
       }
;

ps_id_etc:
   | package_scope_id_follows_opt id { $1, $2 }
;

ps_type:
   | package_scope_id_follows_opt TYPE_IDENTIFIER { $1, $2 }
;

ps_covergroup_identifier:
   | package_scope_id_follows_opt COVERGROUP_IDENTIFIER { mknode $startpos $endpos (L.data_type (LDT.PsCovergroup $2)) $1 }
;

class_scope_type:
   | class_scope_id_follows TYPE_IDENTIFIER { $1, $2 }
;

class_scope_id:
   | class_scope_id_follows id { mknode $startpos $endpos (L.ClassScopeId $2) $1 }
;

class_type_without_id:
   | package_scope_id_follows_opt class_type_list { $1 @ $2 }
;

class_scope_without_id:
   | c=class_scope_id_follows { c }
;

class_scope_id_follows:
   | p=package_scope_id_follows_opt c=class_type_list_colon_id_follows { p @ c }
;

class_type_list_colon_id_follows:
   | c=class_type_list COLON_COLON { c }
;

class_type_list:
   |                                  class_type { [$1] }
   | class_type_list_colon_id_follows class_type { $1 @ [$2] }
;

class_type:
   | c=CLASS_IDENTIFIER p=parameter_value_assignment_opt { mknode $startpos $endpos (L.ClassType c) p }
;

package_scope_id_follows_opt:
   | (* empty *)              { [] }
   | package_scope_id_follows { [$1] }
;

package_scope_id_follows:
   | ST_UNIT            COLON_COLON { mkleaf $startpos $endpos L.PackageScopeUnit }
   | PACKAGE_IDENTIFIER COLON_COLON { mkleaf $startpos $endpos (L.PackageScope $1) }
   | LOCAL__CC          COLON_COLON { mkleaf $startpos $endpos L.PackageScopeLocal }
;

class_item_list_opt:
   | (* empty *)     { [] }
   | class_item_list { $1 }
;

class_item_list:
   |                 class_item { [$1] }
   | class_item_list class_item { $1 @ [$2] }
;

class_item:
   | class_property                        { $1 }
   | class_method                          { $1 }
   | class_constraint                      { $1 }
   | class_declaration                     { $1 }
   | timeunits_declaration                 { $1 }
   | covergroup_declaration                { $1 }
   | local_parameter_declaration SEMICOLON { reloc $startpos $endpos $1; $1 }
   | parameter_declaration SEMICOLON       { reloc $startpos $endpos $1; $1 }
   | SEMICOLON                             { mkleaf $startpos $endpos L.ClassItemEmpty }
;

class_method:
   |        member_qual_reset_list_opt task_declaration            { mknode $symbolstartpos $endpos L.ClassMethod ($1 @ [$2]) }
   |        member_qual_reset_list_opt function_declaration        { mknode $symbolstartpos $endpos L.ClassMethod ($1 @ [$2]) }
   | e=extern ml=member_qual_reset_list_opt m=method_prototype SEMICOLON  { register_extern_method m#get_identifier; mknode $startpos $endpos L.ClassMethod ([e] @ ml @ [m]) }
   | extern member_qual_reset_list_opt class_constructor_prototype { register_extern_method "new";        mknode $startpos $endpos L.ClassMethod ([$1] @ $2 @ [$3]) }
;

class_item_qualifier:
   | PROTECTED { mkleaf $startpos $endpos (L.qualifier LQ.Protected) }
   | LOCAL     { mkleaf $startpos $endpos (L.qualifier LQ.Local) }
   | STATIC    { mkleaf $startpos $endpos (L.qualifier LQ.Static) }
;

member_qual_reset_list_opt:
   | (* empty *)      { [] }
   | member_qual_list { $1 }
;

member_qual_list:
   |                  member_qual { [$1] }
   | member_qual_list member_qual { $1 @ [$2] }
;

member_qual:
   | class_item_qualifier { $1 }
   | VIRTUAL              { mkleaf $startpos $endpos (L.qualifier LQ.Virtual) }
   | PURE VIRTUAL         { mkleaf $startpos $endpos (L.qualifier LQ.PureVirtual) }
   | random_qualifier     { $1 }
   | AUTOMATIC            { mkleaf $startpos $endpos (L.qualifier LQ.Automatic) }
;

pure:
   | PURE { mkleaf $startpos $endpos L.Pure }
;

class_constraint:
   |        constraint_static_opt CONSTRAINT id_any constraint_block { mknode $symbolstartpos $endpos (L.ClassConstraint $3) ($1 @ [$4]) }
   |        constraint_static_opt CONSTRAINT id_any SEMICOLON        { mknode $symbolstartpos $endpos (L.ClassConstraint $3) $1 }
   | extern constraint_static_opt CONSTRAINT id_any SEMICOLON        { mknode $startpos $endpos (L.ClassConstraint $4) ($1::$2) }
   | pure   constraint_static_opt CONSTRAINT id_any SEMICOLON        { mknode $startpos $endpos (L.ClassConstraint $4) ($1::$2) }
;

constraint_block:
   | LBRACE constraint_block_item_list RBRACE { mknode $startpos $endpos L.ConstraintBlock $2 }
;

constraint_block_item_list:
   |                            constraint_block_item { [$1] }
   | constraint_block_item_list constraint_block_item { $1 @ [$2] }
;

constraint_block_item:
   | SOLVE solve_before BEFORE solve_before SEMICOLON { mknode $startpos $endpos L.Solve [$2; $4] }
   | constraint_expression { $1 }
;

solve_before:
   | solve_before_list { mknode $startpos $endpos L.SolveBefore $1 }
;

solve_before_list:
   |                         solve_before_primary { [$1] }
   | solve_before_list COMMA solve_before_primary { $1 @ [$3] }
;

solve_before_primary:
   | e=expr_scope { e }
;

constraint_expression_list:
   |                            constraint_expression { [$1] }
   | constraint_expression_list constraint_expression { $1 @ [$2] }
;

constraint_expression:
   | expr SEMICOLON                                               { mknode $startpos $endpos (L.expr LE.Constraint) [$1] }
   | IF LPAREN expr RPAREN constraint_set %prec P_LOWER_THAN_ELSE { mknode $startpos $endpos (L.expr LE.ConstraintIf) [$3; $5] }
   | IF LPAREN expr RPAREN constraint_set ELSE constraint_set     { mknode $startpos $endpos (L.expr LE.ConstraintIf) [$3; $5; $7] }
   | FOREACH LPAREN id_class_foreach RPAREN constraint_set        { mknode $startpos $endpos (L.expr LE.ConstraintForeach) [$3; $5] }
;

constraint_set:
   | constraint_expression                    { $1 }
   | LBRACE constraint_expression_list RBRACE { mknode $startpos $endpos (L.expr LE.ConstraintSet) $2 }
;

dist_list:
   |                 dist_item { [$1] }
   | dist_list COMMA dist_item { $1 @ [$3] }
;

dist_item:
   | value_range                  { mknode $startpos $endpos L.DistItem [$1] }
   | value_range COLON_EQ    expr { mknode $startpos $endpos L.DistItem [$1; mknode $startpos($2) $endpos L.DistWeight [$3]] }
   | value_range COLON_SLASH expr { mknode $startpos $endpos L.DistItem [$1; mknode $startpos($2) $endpos L.DistWeightRange [$3]] }
;

extern_constraint_declaration:
   | constraint_static_opt CONSTRAINT class_scope_id constraint_block { mknode $symbolstartpos $endpos L.ExternConstraintDeclaration ($1 @ [$3; $4]) }
;

constraint_static_opt:
   | (* empty *) { [] }
   | STATIC__C   { [mkleaf $startpos $endpos L.Static] }
;


%%
