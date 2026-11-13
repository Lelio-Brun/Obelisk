(*
   Copyright 2013-2018 RIKEN
   Copyright 2018-2020 Chiba Institude of Technology

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

(* 
 * A parser for Fortran (based on Fortran95)
 * 
 *
 * parser.mly
 *
 *)

%{
module P = Printer
open Printf
open Common
open Ast
open Labels
module L = Label
module C = Context

module Aux = Parser_aux.F (Stat)
module D = Disambg.F (Stat)
open Aux
open Stat
open D

let reloc = reloc Stat.env
let mknode = mknode Stat.env
let mkleaf = mkleaf Stat.env
let mkstmtnode = mkstmtnode Stat.env
let mkstmtleaf = mkstmtleaf Stat.env

let ibm()      = env#current_source#add_ext_IBM
let intel()    = env#current_source#add_ext_Intel
let pgi()      = env#current_source#add_ext_PGI
let pgi_cuda() = env#current_source#add_ext_PGI_CUDA
let apollo()   = env#current_source#add_ext_Apollo
let f90()      = env#current_source#set_spec_F90
let f2003()    = env#current_source#set_spec_F2003
let f2008()    = env#current_source#set_spec_F2008

let get_nd(s, n) = n

let name_sep_pat = Str.regexp_string ";"

%}

%parameter <Stat : Parser_aux.STATE_T>

%token NOTHING

%token<Obj.t option> EOF

%token EOP

%token <Ast.Partial.spec * Ast.node> PROGRAM_UNIT
%token <Ast.Partial.spec * Ast.node> SPEC_PART_CONSTRUCT
%token <Ast.Partial.spec * Ast.node> EXEC_PART_CONSTRUCT
%token <Ast.Partial.spec * Ast.node> SUBPROGRAM
%token <Ast.Partial.spec * Ast.node> INTERFACE_SPEC
%token <Ast.Partial.spec * Ast.node> CASE_BLOCK
%token <Ast.Partial.spec * Ast.node> DATA_STMT_SET
%token <Ast.Partial.spec * Ast.node> VARIABLE
%token <Ast.Partial.spec * Ast.node> EXPR
%token <Ast.Partial.spec * Ast.node> ACTION_STMT
%token <Ast.Partial.spec * Ast.node> TYPE_SPEC
%token <Ast.Partial.spec * Ast.node> STMT
%token <Ast.Partial.spec * Ast.node> DERIVED_TYPE_DEF_PART
%token <Ast.Partial.spec * Ast.node> ONLY_

%token <Ast.Partial.spec * Ast.node> IF_THEN_STMT
%token <Ast.Partial.spec * Ast.node> DO_STMT
%token <Ast.Partial.spec * Ast.node> FORALL_CONSTRUCT_STMT
%token <Ast.Partial.spec * Ast.node> WHERE_CONSTRUCT_STMT
%token <Ast.Partial.spec * Ast.node> SELECT_CASE_STMT
%token <Ast.Partial.spec * Ast.node> DERIVED_TYPE_STMT

%token <Ast.Partial.spec * Ast.node> END_IF_STMT
%token <Ast.Partial.spec * Ast.node> END_DO_STMT
%token <Ast.Partial.spec * Ast.node> END_FORALL_STMT
%token <Ast.Partial.spec * Ast.node> END_WHERE_STMT
%token <Ast.Partial.spec * Ast.node> END_SELECT_STMT
%token <Ast.Partial.spec * Ast.node> END_TYPE_STMT

%token <Ast.Partial.spec * Ast.node> FUNCTION_HEAD
%token <Ast.Partial.spec * Ast.node> FUNCTION_STMT_HEAD
%token <Ast.Partial.spec * Ast.node> SUBROUTINE_HEAD
%token <Ast.Partial.spec * Ast.node> SUBROUTINE_STMT_HEAD

%token <Ast.Partial.spec * Ast.node> PU_TAIL



%token EOL


%token <string> IDENTIFIER

%token <bool * string * Obj.t list> COMPOSITE_IDENTIFIER

%token <string> CONTINUED_IDENTIFIER


%token <string> LETTER

%token <string> LABEL

%token <string> CONSTRUCT_NAME

%token <string> INT_LITERAL
%token <string> REAL_LITERAL
%token <string> BOZ_LITERAL
%token <string> LOGICAL_LITERAL
%token <string> CHAR_LITERAL
(*%token <string*string> COMPLEX_LITERAL*)


%token <string> DEFINED_OP

%token <string> DATA_EDIT_DESC
%token <string> KP_DESC
%token <string> POSITION_EDIT_DESC
%token <string * bool> HOLLERITH


%token <string> PP_IDENTIFIER
%token <string> PP_UNDERSCORE
%token PP_AND PP_OR PP_CONCAT

%token <string * string> PP_MACRO_NAME
%token <string> PP_MACRO_CONST
%token <string> PP_MACRO_CONST_CHAR
%token <string> PP_MACRO_CONST_INT
%token <string> PP_MACRO_VARIABLE
%token <string> PP_MACRO_EXPR
%token <string> PP_MACRO_STMT
%token <string> PP_MACRO_TYPE_SPEC
%token <string> PP_MACRO_WRITE
%token <string> PP_MACRO_READ_WRITE
%token <string> PP_MACRO_READ_PRINT

%token <Macro.kind * string> PP_MACRO_ID
%token <Macro.kind * string> PP_MACRO_ID_RW
%token <string * string list> PP_MACRO_APPL

(* Preprocessor directives *)
%token <string * Macro.body> PP_DEFINE__IDENT__BODY

%token <string> PP_UNDEF__IDENT 

(*PP_IF__COND PP_ELIF__COND PP_IFDEF__IDENT PP_IFNDEF__IDENT PP_ELSE PP_ENDIF*)
%token <F_pp_directive.branch> PP_BRANCH

(*PP_ERROR__MESG PP_WARNING__MESG*)
%token <F_pp_directive.message> PP_ISSUE__MESG

(*PP_INCLUDE__FILE PP_SYS_INCLUDE__FILE PP_INCLUDE__MACRO*)
%token <F_header_file.t> PP_INCLUDE__FILE

%token <string * string> PP_UNKNOWN__REST
%token PP_IF PP_ELIF PP_IFDEF PP_IFNDEF PP_ELSE PP_ENDIF
%token PP_DEFINE PP_UNDEF PP_INCLUDE PP_ERROR PP_WARNING PP_UNKNOWN


%token <Common.DirectiveLine.raw> RAW


(* OCL directives and keywords *)
%token OCL_ARRAY_FUSION OCL_END_ARRAY_FUSION OCL_ARRAY_MERGE OCL_ARRAY_SUBSCRIPT
%token OCL_EVAL OCL_NOEVAL OCL_FLTLD OCL_NOFLTLD OCL_FP_RELAXED OCL_NOFP_RELAXED
%token OCL_LOOP_INTERCHANGE OCL_LOOP_NOINTERCHANGE OCL_MFUNC OCL_NOMFUNC
%token OCL_NOARRAYPAD OCL_LOOP_NOFUSION OCL_PREEX OCL_NOPREEX 
%token OCL_PREFETCH OCL_NOPREFETCH OCL_PREFETCH_CACHE_LEVEL OCL_PREFETCH_INFER
%token OCL_PREFETCH_NOINFER OCL_PREFETCH_ITERATION OCL_PREFETCH_ITERATION_L2
%token OCL_PREFETCH_READ OCL_PREFETCH_WRITE OCL_STRIPING OCL_NOSTRIPING
%token OCL_SWP OCL_NOSWP OCL_LOOP_BLOCKING OCL_UNROLL
%token OCL_NOUNROLL OCL_NOVREC OCL_SIMD OCL_NOSIMD OCL_CACHE_SECTOR_SIZE
%token OCL_END_CACHE_SECTOR_SIZE OCL_CACHE_SUBSECTOR_ASSIGN OCL_END_CACHE_SUBSECTOR
%token OCL_FISSION_POINT OCL_LOOP_NOFISSION OCL_XFILL OCL_NOXFILL 
%token OCL_PREFETCH_SEQUENTIAL OCL_PREFETCH_STRONG OCL_PREFETCH_NOSTRONG
%token OCL_PREFETCH_STRONG_L2 OCL_PREFETCH_NOSTRONG_L2 OCL_FP_CONTRACT OCL_NOFP_CONTRACT
%token OCL_LOOP_NOBLOCKING OCL_NORECURRENCE OCL_UXSIMD OCL_NOUXSIMD
%token OCL_ARRAY_PRIVATE OCL_NOARRAY_PRIVATE OCL_INDEPENDENT OCL_NOALIAS OCL_SERIAL
%token OCL_PARALLEL OCL_PARALLEL_STRONG OCL_REDUCTION OCL_NOREDUCTION OCL_TEMP

%token OCL_LOOP_PART_SIMD OCL_LOOP_NOPART_SIMD OCL_SHORTLOOP OCL_NOSHORTLOOP
%token OCL_SIMD_LISTV OCL_UNSWITCHING OCL_LOOP_PART_PARALLEL OCL_LOOP_NOPART_PARALLEL
%token OCL_FIRST_PRIVATE OCL_LAST_PRIVATE OCL_TEMP_PRIVATE OCL_PARALLEL_CYCLIC


%token OCL_ALIGNED OCL_UNALIGNED OCL_LEVEL OCL_STRONG OCL_AUTO OCL_SOFT
%token OCL_ALL OCL_THEN OCL_ELSE

%token <Ast.node> OCL


(* OMP directives and keywords *)
%token OMP_AUTO OMP_BARRIER OMP_CAPTURE OMP_COLLAPSE OMP_COPYIN OMP_COPYPRIVATE 
%token OMP_DEFAULT OMP_DYNAMIC OMP_FINAL OMP_FIRSTPRIVATE OMP_FLUSH OMP_GUIDED 
%token OMP_IF OMP_LASTPRIVATE OMP_MERGEABLE OMP_NONE OMP_NOWAIT OMP_NUM_THREADS 
%token OMP_PRIVATE OMP_READ OMP_RUNTIME OMP_SCHEDULE OMP_SECTION OMP_SHARED 
%token OMP_STATIC OMP_TASKWAIT OMP_TASKYIELD OMP_THREADPRIVATE OMP_UNTIED 
%token OMP_WRITE OMP_END_ATOMIC OMP_END_CRITICAL OMP_END_MASTER OMP_END_ORDERED
%token OMP_END_SECTIONS OMP_END_SINGLE OMP_END_TASK OMP_END_WORKSHARE
%token OMP_PARALLEL_SECTIONS OMP_PARALLEL_WORKSHARE OMP_END_PARALLEL_SECTIONS
%token OMP_END_PARALLEL_WORKSHARE

(* may be contained in composite keywords *)
%token OMP_END OMP_REDUCTION OMP_UPDATE OMP_PARALLEL OMP_DO OMP_SECTIONS
%token OMP_SINGLE OMP_WORKSHARE OMP_TASK OMP_MASTER OMP_CRITICAL OMP_ATOMIC
%token OMP_ORDERED OMP_END_DO OMP_END_PARALLEL OMP_PARALLEL_DO
%token OMP_END_PARALLEL_DO

(* OMP 4.0 *)
%token OMP_END_SIMD OMP_SEQ_CST OMP_PROC_BIND OMP_CLOSE OMP_SPREAD OMP_DECLARE_SIMD 
%token OMP_DO_SIMD OMP_END_DO_SIMD OMP_TARGET_DATA OMP_END_TARGET_DATA OMP_TO
%token OMP_TARGET_UPDATE OMP_PARALLEL_DO_SIMD OMP_TEAMS_DISTRIBUTE_SIMD OMP_FROM
%token OMP_DECLARE_TARGET OMP_DISTRIBUTE_SIMD OMP_END_DISTRIBUTE_SIMD OMP_TOFROM
%token OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD OMP_END_PARALLEL_DO_SIMD OMP_IN OMP_OUT
%token OMP_END_TEAMS_DISTRIBUTE_SIMD OMP_TARGET_TEAMS_DISTRIBUTE_SIMD OMP_LINEAR
%token OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD OMP_MAP OMP_ALLOC OMP_ALIGNED OMP_INOUT
%token OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD OMP_DEPEND OMP_TASKGROUP
%token OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD OMP_SAFELEN OMP_SIMDLEN
%token OMP_END_TASKGROUP OMP_CANCEL OMP_CANCELLATION_POINT OMP_DECLARE_REDUCTION
%token OMP_UNIFORM OMP_INBRANCH OMP_NOTINBRANCH OMP_DEVICE OMP_DIST_SCHEDULE
%token OMP_INITIALIZER OMP_NUM_TEAMS OMP_THREAD_LIMIT

(* may be contained in composite keywords (OMP 4.0) *)
%token OMP_DISTRIBUTE OMP_DISTRIBUTE_PARALLEL_DO OMP_END_DISTRIBUTE_PARALLEL_DO
%token OMP_END_DISTRIBUTE OMP_END_TARGET OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD
%token OMP_END_TARGET_TEAMS_DISTRIBUTE OMP_TEAMS_DISTRIBUTE OMP_TEAMS OMP_SIMD
%token OMP_END_TEAMS OMP_END_TEAMS_DISTRIBUTE OMP_TEAMS_DISTRIBUTE_PARALLEL_DO
%token OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO OMP_TARGET_TEAMS_DISTRIBUTE
%token OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO OMP_DISTRIBUTE_PARALLEL_DO_SIMD
%token OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO OMP_TARGET OMP_END_TARGET_TEAMS
%token OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD OMP_TARGET_TEAMS

%token <Ast.node> OMP

(* ACC (2.0) directives *)
%token ACC_PARALLEL ACC_KERNELS ACC_DATA ACC_ENTER ACC_EXIT ACC_HOST_DATA ACC_LOOP
%token ACC_CACHE ACC_ATOMIC ACC_UPDATE ACC_WAIT ACC_ROUTINE ACC_DECLARE ACC_END

(* ACC (2.0) keywords *)
%token ACC_IF ACC_REDUCTION ACC_PRIVATE ACC_FIRSTPRIVATE ACC_DEFAULT ACC_NONE
%token ACC_DEVICE_TYPE ACC_DTYPE ACC_ASYNC ACC_NUM_GANGS ACC_NUM_WORKERS
%token ACC_VECTOR_LENGTH ACC_COPYIN ACC_CREATE ACC_PRESENT_OR_COPY ACC_PCOPY
%token ACC_PRESENT_OR_COPYIN ACC_PCOPYIN ACC_PRESENT_OR_COPYOUT ACC_PCOPYOUT ACC_USE_DEVICE
%token ACC_PRESENT_OR_CREATE ACC_PCREATE ACC_COPYOUT ACC_DELETE ACC_COPY ACC_PRESENT
%token ACC_DEVICEPTR ACC_COLLAPSE ACC_SEQ ACC_AUTO ACC_TILE ACC_GANG ACC_WORKER
%token ACC_VECTOR ACC_INDEPENDENT ACC_READ ACC_WRITE ACC_CAPTURE ACC_SELF ACC_HOST
%token ACC_BIND ACC_NOHOST ACC_DEVICE_RESIDENT ACC_LINK ACC_DEVICE

%token <Ast.node> ACC

(* XLF directives *)
%token XLF_ALIGN XLF_ASSERT XLF_BLOCK_LOOP XLF_CNCALL XLF_COLLAPSE XLF_EJECT
%token XLF_EXECUTION_FREQUENCY  XLF_EXPECTED_VALUE XLF_FUNCTRACE_XLF_CATCH
%token XLF_FUNCTRACE_XLF_ENTER XLF_FUNCTRACE_XLF_EXIT XLF_IGNORE_TKR XLF_INDEPENDENT
%token XLF_LOOPID XLF_MEM_DELAY XLF_NEW XLF_NOFUNCTRACE XLF_NOSIMD XLF_NOVECTOR
%token XLF_PERMUTATION XLF_PROCESS XLF_SNAPSHOT XLF_SOURCEFORM XLF_STREAM_UNROLL
%token XLF_SUBSCRIPTORDER XLF_UNROLL XLF_UNROLL_AND_FUSE

%token XLF_ITERCNT XLF_MINITERCNT XLF_MAXITERCNT XLF_NODEPS
%token XLF_REDUCTION
%token XLF_FIXED XLF_FREE XLF_F90 XLF_IBM
%token XLF_VERY_HIGH XLF_VERY_LOW

%token <Ast.node> XLF

(* XLF Keywords *)


(* Fortran-Linda *)
%token <string> LINDA_TYPEOF




(* Dotted identifiers *)
%token D_EQ D_NE D_GT D_GE D_LT D_LE D_NOT D_AND D_OR D_EQV D_NEQV (* D_TRUE D_FALSE *)

(* Symbols *)
%token DOLLAR QUESTION EXCLAM BACKSLASH
%token PERCENT COMMA COLON SEMICOLON COLON_COLON AMP DOT 

(* Operators *)
%token STAR PLUS MINUS SLASH STAR_STAR SLASH_SLASH 
%token UPLUS UMINUS
%token LT EQ GT LT_EQ GT_EQ EQ_EQ SLASH_EQ EQ_GT 

(* Delimiters *)
%token LPAREN RPAREN 
%token LPAREN_SLASH SLASH_RPAREN

%token LPAREN__IMPLICIT LPAREN__GO_TO
%token LPAREN__io_control_spec
%token LPAREN__position_spec
%token LPAREN__flush_spec

%token COMMA__SLASH

%token LBRACKET RBRACKET (* F2003 *)

%token <string> VFE_BEGIN VFE_END (* variable format expression (Compaq/Intel) *)

(* Keywords *)
%token <string> ALLOCATABLE ALLOCATE ASSIGNMENT 
%token <string> BACKSPACE BLOCK_DATA BYTE
%token <string> CALL CASE CHARACTER CLOSE COMMON CONTAINS CONTINUE CYCLE 
%token <string> DOUBLE DATA DEALLOCATE DEFAULT DIMENSION DO DOUBLE_PRECISION
%token <string> DOUBLE_COMPLEX
%token <string> (*ELEMENTAL*) ELSE ELSE_IF ELSEWHERE END END_BLOCK_DATA END_DO END_FILE 
%token <string> END_FORALL END_FUNCTION  END_IF END_INTERFACE END_MODULE END_PROGRAM 
%token <string> END_SELECT END_SUBROUTINE END_TYPE END_WHERE ENTRY EQUIVALENCE EXIT 
(*%token <string> EXTERNAL *)
%token <string> FORALL FORMAT FUNCTION GO_TO 
%token <string> IF IMPLICIT INQUIRE INTENT
%token <string> INTERFACE INTRINSIC KIND LEN MODULE 
%token <string> NAMELIST NONE NULL NULLIFY ONLY OPEN OPERATOR OPTIONAL
%token <string> PARAMETER POINTER PRINT PRIVATE PROCEDURE PROGRAM PUBLIC (*PURE*)
%token <string> PRECISION
%token <string> READ (*RECURSIVE*) RESULT RETURN REWIND 
%token <string> SAVE SEQUENCE SELECT_CASE (*STAT*) STOP SUBROUTINE 
%token <string> TARGET THEN TYPE USE WHERE WHILE WRITE 

%token <string> PREFIX_SPEC      (* RECURSIVE PURE ELEMENTAL IMPURE *)
%token <string> INTENT_SPEC      (* IN OUT IN_OUT *)
%token <string> KINDED_TYPE_SPEC (* COMPLEX INTEGER LOGICAL REAL *)
%token <string> SIMPLE_ATTR      (* EXTERNAL PROTECTED VALUE VOLATILE CONTIGUOUS *)

%token <string> INCLUDE__FILE

(* Keywords (F90) *)
%token <string> ASSIGN PAUSE TO

(* Keywords (F2003) *)
%token <string> ABSTRACT ASSOCIATE ASYNCHRONOUS BIND CLASS CLASS_DEFAULT CLASS_IS
%token <string> DEFERRED END_ASSOCIATE END_ENUM ENUM ENUMERATOR (*ERRMSG*) EXTENDS
%token <string> FINAL FLUSH GENERIC ID IMPORT NON_INTRINSIC NON_OVERRIDABLE NOPASS
%token <string> PASS (*PROTECTED*) SELECT_TYPE (*SOURCE*) TYPE_IS (*VALUE VOLATILE*) WAIT

(* Keywords (F2008) *)
%token <string> BLOCK CODIMENSION CONCURRENT (*CONTIGUOUS*) CRITICAL END_BLOCK
%token <string> END_CRITICAL ERROR (*IMPURE*) (*MOLD*) LOCK SYNC SUBMODULE END_SUBMODULE
%token <string> ALLOC_OPT_EXPR (* MOLD SOURCE *)

(* Keywords (Compaq/Intel) *)
%token <string> (*OPTIONS*) ACCEPT REWRITE UNLOCK DELETE (*DEFINE_FILE*) ENCODE DECODE
%token <string> FIND INTEL_CLOSE_CONNECT_SPEC (* DISPOSE DISP *)
%token <string> OPTIONS__OPTS
%token DEFINE_FILE

(* DEC directives (Intel) *)
%token DEC_ALIAS DEC_ASSUME DEC_ASSUME_ALIGNED DEC_ATTRIBUTES DEC_DECLARE DEC_NODECLARE
%token DEC_DEFINE DEC_UNDEFINE DEC_DISTRIBUTE DEC_POINT DEC_FIXEDFORMLINESIZE
%token DEC_FMA DEC_NOFMA DEC_FREEFORM DEC_NOFREEFORM DEC_IDENT DEC_IF DEC_DEFINED
%token DEC_INLINE DEC_FORCEINLINE DEC_NOINLINE DEC_INTEGER DEC_IVDEP DEC_LOOP DEC_COUNT
%token DEC_MESSAGE DEC_NOFUSION DEC_OBJCOMMENT DEC_OPTIMIZE DEC_NOOPTIMIZE DEC_OPTIONS
%token DEC_PACK DEC_PARALLEL DEC_NOPARALLEL DEC_PREFETCH DEC_NOPREFETCH DEC_PSECT DEC_REAL
%token DEC_SIMD DEC_STRICT DEC_NOSTRICT DEC_UNROLL DEC_NOUNROLL DEC_UNROLL_AND_JAM
%token DEC_NOUNROLL_AND_JAM DEC_VECTOR DEC_NOVECTOR DEC_INIT_DEP_FWD DEC_RECURSIVE
%token DEC_DISTRIBUTEPOINT DEC_LOOPCOUNT DEC_IFDEFINED DEC_ELSEIF DEC_ELSE DEC_ENDIF

%token DEC_ALWAYS DEC_ASSERT DEC_ALIGNED DEC_UNALIGNED DEC_TEMPORAL DEC_NONTEMPORAL
%token DEC_VECREMAINDER DEC_NOVECREMAINDER DEC_NOASSERT DEC_FIRSTPRIVATE DEC_LASTPRIVATE
%token DEC_LINEAR DEC_PRIVATE DEC_REDUCTION DEC_VECTORLENGTH DEC_VECTORLENGTHFOR
%token DEC_ALIGN DEC_WRT DEC_NOWRT DEC_NUM_THREADS DEC_END DEC_ENDOPTIONS
%token DEC_NOALIGN DEC_OFFLOAD_ATTRIBUTE_TARGET DEC_WARN DEC_ALIGNMENT DEC_NOALIGNMENT

%token DEC_MASK DEC_NOMASK DEC_PROCESSOR DEC_UNIFORM DEC_BLOCK_LOOP DEC_NOBLOCK_LOOP
%token DEC_CODE_ALIGN DEC_FACTOR DEC_LEVEL

(*%token DEC_OFFLOAD DEC_BEGIN DEC_OFFLOAD_TRANSFER DEC_OFFLOAD_WAIT*)(* not yet *)

%token <Ast.node> DEC


(* connect-spec *)
    (* ASSOCIATEVARIABLE BLOCKSIZE BUFFERCOUNT *)
    (* MAXREC READONLY SHARED TITLE USEROPEN *)
(* connect or inquire spec *)
    (* BUFFERED CARRIAGECONTROL CONVERT *)
    (* DEFAULTFILE IOFOCUS MODE *)
    (* ORGANIZATION RECORDSIZE RECORDTYPE *)
(* inquire spec *)
    (* BINARY *)

(* Keywords (Compaq/Intel and IBM) *)
%token <string> (*AUTOMATIC STATIC*) VIRTUAL STRUCTURE END_STRUCTURE 
%token <string> RECORD UNION END_UNION MAP END_MAP


(* Keywords (io-control-spec) *)
%token <string> FMT NML (*REC ADVANCE*) EOR SIZE

%token <string> IOCTL_SPEC (* ADVANCE REC *)

%token <string> CONNECT_INQUIRE_IOCTL_SPEC (* BLANK DECIMAL DELIM PAD ROUND SIGN *)

%token <string> CONNECT_INQUIRE_SPEC (* ACCESS ACTION ENCODING FORM POSITION RECL *)

%token <string> CONNECT_SPEC (* NEWUNIT *)

%token <string> INQUIRE_IOCTL_SPEC (* ID POS SIZE *)

%token <string> INQUIRE_SPEC (* DIRECT EXIST FORMATTED NAMED NEXTREC NUMBER *)
                             (* OPENED PENDING READWRITE SEQUENTIAL UNFORMATTED *)

%token <string> ERR FILE IOSTAT STATUS UNIT IOMSG NAME_ IOLENGTH



%token MARKER
%token PP_MARKER

%token END_FRAGMENT


(*
%left DEFINED_OP 
%left D_EQV D_NEQV
%left D_OR
%left D_AND
%nonassoc D_NOT
%left D_EQ D_NE D_LT D_LE D_GT D_GE EQ_EQ SLASH_EQ LT LE GT GE
*)
(*%left SLASH_SLASH*)
(*%left PLUS MINUS*)
(*%left STAR SLASH*)
(*%right STAR_STAR*)


%start main 
%start partial_program
%start partial_program_unit
%start partial_spec__exec
%start partial_specification_part
%start partial_execution_part
%start partial_subprograms
%start partial_interface_spec
%start partial_case_block
%start partial_assignment_stmt
%start partial_type_declaration_stmt
%start partial_function_stmt
%start partial_variable
%start partial_expr
%start partial_stmts
%start partial_data_stmt_sets
%start partial_type_spec
%start partial_action_stmt
%start partial_derived_type_def_part
%start partial_onlys
%start partial_type_bound_proc_part
%start partial_function_head
%start partial_function_stmt_head
%start partial_subroutine_head
%start partial_subroutine_stmt_head
%start partial_pu_tail

%start ocl
%start omp
%start acc
%start xlf
%start dec

%type <Ast.node> main
%type <Ast.Partial.t> partial_program
%type <Ast.Partial.t> partial_program_unit
%type <Ast.Partial.t> partial_spec__exec
%type <Ast.Partial.t> partial_specification_part
%type <Ast.Partial.t> partial_execution_part
%type <Ast.Partial.t> partial_subprograms
%type <Ast.Partial.t> partial_interface_spec
%type <Ast.Partial.t> partial_case_block
%type <Ast.Partial.t> partial_assignment_stmt
%type <Ast.Partial.t> partial_type_declaration_stmt
%type <Ast.Partial.t> partial_function_stmt
%type <Ast.Partial.t> partial_variable
%type <Ast.Partial.t> partial_expr
%type <Ast.Partial.t> partial_stmts
%type <Ast.Partial.t> partial_data_stmt_sets
%type <Ast.Partial.t> partial_type_spec
%type <Ast.Partial.t> partial_action_stmt
%type <Ast.Partial.t> partial_derived_type_def_part
%type <Ast.Partial.t> partial_onlys
%type <Ast.Partial.t> partial_type_bound_proc_part
%type <Ast.Partial.t> partial_function_head
%type <Ast.Partial.t> partial_function_stmt_head
%type <Ast.Partial.t> partial_subroutine_head
%type <Ast.Partial.t> partial_subroutine_stmt_head
%type <Ast.Partial.t> partial_pu_tail

%type <Ast.node> ocl
%type <Ast.node> omp
%type <Ast.node> acc
%type <Ast.node> xlf
%type <Ast.node> dec

(* to avoid warnings *)
%start pp_branch
%type <unit> pp_branch
%start special_token special_symbol omp_token pp_token
%type <unit> special_token special_symbol omp_token pp_token


(*
%start not_yet
%type <unit> not_yet
*)

%%
(********** Rules **********)

(*
not_yet:
   | DEFINE_FILE   { }
   | OPTIONS       { }
;
*)

(*
listL(X):
   |            x=X { [x] }
   | l=listL(X) x=X { l @ [x] }
;

listL0(X):
   |                 { [] }
   | l=listL0(X) x=X { l @ [x] }
;
*)

%inline
clist0(X):
   | l_opt=ioption(separated_nonempty_list(COMMA, X)) { list_opt_to_list l_opt }
;

opclist(X):
   | l=separated_nonempty_list(COMMA, ioption(X)) { opt_list_to_list l }
;


%inline
clist(X):
   | l=separated_nonempty_list(COMMA, X) { l }
;


%inline
coplist(X):
   | l=separated_nonempty_list(ioption(COMMA), X) { l }
;

%inline
coplist0(X):
   | l_opt=ioption(separated_nonempty_list(ioption(COMMA), X)) { list_opt_to_list l_opt }
;


main:
   |           EOF { empty_node }
   | p=program EOF { disambiguate_deferred(); end_scope(); p }
;

(*
include_line:
   | INCLUDE c=char_literal EOL { (* mkleaf $startpos $endpos (L.Linclude_statement $2) *) }
;
*)

name:
   | i=IDENTIFIER    { mkleaf $startpos $endpos (mkn i) }
   | n=PP_MACRO_NAME { let r, e = n in mkleaf $startpos $endpos (L.Name r) }
;

stmt_end:
   | EOL            { (* let loc = pos_mgr#offsets_to_loc $startpos $endpos in Printf.printf "%dL\n%!" loc.Loc.start_line *) }
   | SEMICOLON+     { }
   | SEMICOLON+ EOL { }
   | DOLLAR         { } (* some compilers use '$' to separate two statements on the same line *)
   | error          { ignore (parse_error $startpos $endpos "syntax error") }
;
(*
pp_macro_stmt:
   | s=_pp_macro_stmt stmt_end { s }
;
*)
_pp_macro_stmt:
   | m=PP_MACRO_STMT { mkstmtleaf $startpos $endpos (Stmt.PpMacroStmt m) }
   | m=pp_macro_id as_opt=ioption(pp_macro_args)
       { 
         if not (env#macro_defined m) then begin
           let body =
             Macro.mk_obj_body ~stat:(Macro.Resolved (Obj.repr (Tokens_.PP_MACRO_STMT m))) "<constrained>"
           in
           env#define_macro m body
         end;
         mkstmtnode $startpos $endpos (Stmt.PpMacroStmt m) (list_opt_to_list as_opt)
       }
;

%inline
pp_macro_args:
   | LPAREN es=opclist(expr) RPAREN { es }
;

pp_macro_const:
   | c=PP_MACRO_CONST       { c }
   | c=PP_MACRO_CONST_CHAR  { c }
   | c=PP_MACRO_CONST_INT   { c }
;

partial_program:
   | p=program EOP { Partial.mk_program p#children }
;

partial_program_unit:
   | p=program_unit EOP { Partial.mk_program_unit p }
;

partial_spec__exec:
   | p=specification_part__execution_part EOP { let sp_opt, ep_opt = p in Partial.mk_spec_exec sp_opt ep_opt }
;

partial_specification_part:   
   | p=specification_part EOP { Partial.mk_specification_part p }
;

partial_execution_part:
   | p=execution_part EOP { Partial.mk_execution_part p }
;

partial_subprogram:
   | s=subprogram { s }
   | sps=STMT     { let _, s = sps in s }
;
partial_subprograms:
   | s_opt=ioption(separated_nonempty_list(ioption(END_FRAGMENT), partial_subprogram)) EOP
       { 
         Partial.mk_subprograms (list_opt_to_list s_opt)
       }

partial_interface_spec:
   | p=interface_specification* EOP { Partial.mk_interface_spec p }
;

partial_case_block:
   | c=case_stmt__block+ EOP { Partial.mk_case_block c }
;

partial_assignment_stmt:
   | a=assignment_stmt EOP { Partial.mk_assignment_stmt a }
;

partial_variable:
   | v=var_or_ref EOP { disambiguate_variable v; Partial.mk_variable v }
;

partial_expr:
   | e=expr EOP { Partial.mk_expr e }
;

partial_stmts:
   | ss=stmt+ EOP { Partial.mk_stmts ss }
;

partial_data_stmt_sets:
   | ds=nonempty_list(data_stmt_set__comma_opt) EOP { Partial.mk_data_stmt_sets ds }
;

data_stmt_set__comma_opt:
   | d=data_stmt_set ioption(COMMA) { d }
;

partial_type_spec:
   | t=decl_type_spec ioption(EOL) EOP { Partial.mk_type_spec t }
;

partial_action_stmt:
   | a=action_stmt EOP { Partial.mk_action_stmt a }
;

partial_type_declaration_stmt:
   | t=type_declaration_stmt EOP { Partial.mk_type_declaration_stmt t }
;

partial_function_stmt:
   | f=function_stmt EOP { Partial.mk_function_stmt f }
;

partial_function_head:
   | fr_opt=ioption(fragment) f=function_stmt se=specification_part__execution_part EOP
       {
        let fr =
          match fr_opt with
          | Some fr -> if fr#children = [] then [] else [fr]
          | None -> []
         in
         Partial.mk_function_head (fr @ f :: (Ast.spec_opt_exec_opt_to_list se))
       }
   | h=function_head EOP
       { 
         let sp, f, fn = h in
         Partial.mk_function_head [f]
       }         
;

partial_function_stmt_head:
   | fshd=function_stmt_head0 EOP
       { 
         let pnds, n_str = fshd in
         let nd = mknode $startpos(fshd) $endpos(fshd) (L.FunctionStmtHead n_str) pnds in
         Partial.mk_function_stmt_head nd
       }
;

partial_subroutine_head:
   | fr_opt=ioption(fragment) s=subroutine_stmt se=specification_part__execution_part EOP
       { 
        let fr =
          match fr_opt with
          | Some fr -> if fr#children = [] then [] else [fr]
          | None -> []
         in
         Partial.mk_subroutine_head (fr @ s :: (Ast.spec_opt_exec_opt_to_list se))
       }
   | h=subroutine_head EOP
       { 
         let sp, s, sn = h in
         Partial.mk_subroutine_head [s]
       }
;

partial_subroutine_stmt_head:
   | sshd=subroutine_stmt_head EOP
       { 
         let pnds, n_str = sshd in
         let nd = mknode $startpos(sshd) $endpos(sshd) (L.SubroutineStmtHead n_str) pnds in
         Partial.mk_subroutine_stmt_head nd
       }
;

partial_pu_tail:
   | se=specification_part__execution_part e=end_stmt_p pus_=program_unit* EOP
       { 
         let pus = 
           List.flatten
             (List.map 
                (fun pu -> 
                  match pu#label with
                  | L.Fragment -> finalize_fragment C.Tprogram_unit pu
                  | _ -> [pu]
                ) pus_
             )
         in
         Partial.mk_pu_tail ((Ast.spec_opt_exec_opt_to_list se) @ (e :: pus))
       }
;

end_stmt_p:
   | e=_end_stmt_p stmt_end { e }
;

_end_stmt_p:
   | END { mark_EOPU ~ending_scope:false (); mkstmtleaf $startpos $endpos (Stmt.EndStmt) }
;

partial_derived_type_def_part:
   | pl=private_sequence_stmt* cs=component_def_stmt* EOP { Partial.mk_derived_type_def_part (pl @ cs) }
;

partial_onlys:
   | ioption(COMMA) os=onlys EOP { Partial.mk_onlys os }
;

onlys:
   | os=_onlys       { List.rev os }
   | os=_onlys COMMA { List.rev os }
;

_onlys:
   |                 o=only { [o] }
   | os=_onlys COMMA o=only { o :: os }
;

partial_type_bound_proc_part:
   | b_opt=binding_private_stmt_opt ts=type_bound_proc_binding* EOP 
       { 
         let c =
           match b_opt with
           | Some b -> b :: ts
           | None -> ts
         in
         Partial.mk_type_bound_proc_part c
       }
;


stmt:
(*
   | s=program_stmt                      { s }
   | s=interface_stmt                    { s }
   | s=module_stmt                       { s }
   | s=function_stmt                     { s }
   | s=subroutine_stmt                   { s }
   | s=block_data_stmt                   { s }
*)
   | s=function_stmt                     { s }

   | s=use_stmt                          { s }
   | s=import_stmt                       { s }
   | s=implicit_stmt                     { s }
   | s=derived_type_stmt                 { s }
   | s=specification_stmt_no_access_stmt { s }
   | s=access_stmt_public                { s }
   | s=procedure_stmt                    { s }

   | s=parameter_format_entry_stmt       { s }
   | s=action_stmt                       { s }
   | s=select_case_stmt                  { s }
   | s=case_stmt                         { s }
   | s=forall_construct_stmt             { s }
   | s=where_construct_stmt              { s }
   | s=if_then_stmt                      { s }
   | s=else_stmt                         { s }
   | s=else_if_stmt                      { s }
   | s=do_stmt                           { s }
   | s=masked_or_unmasked_elsewhere_stmt { s }

   | s=type_declaration_stmt             { s } (* should be disambiguated (may be component_def_stmt) *)
(*
   | s=component_def_stmt                { s }
   | s=private_sequence_stmt             { s }
*)
   | s=access_stmt_private               { s } (* should be disambiguated (may be private_stmt) *)

   | s=sequence_stmt                     { s }

   | s=end_if_stmt                       { s }
   | s=end_interface_stmt                { s }
   | s=end_type_stmt                     { s }
   | s=end_do_stmt                       { s }
   | s=end_select_stmt                   { s }
   | s=end_forall_stmt                   { s }
   | s=end_where_stmt                    { s }

   | s=structure_stmt                    { s }
   | s=union_stmt                        { s }
   | s=map_stmt                          { s }
   | s=end_structure_stmt                { s }
   | s=end_union_stmt                    { s }
   | s=end_map_stmt                      { s }

(*   | s=pp_macro_stmt                     { s }*)
   | sps=STMT                            { let sp, s = sps in check_error s; s }
   | sps=DO_STMT                         { let sp, s = sps in check_error s; s }
   | sps=FORALL_CONSTRUCT_STMT           { let sp, s = sps in check_error s; s }
   | sps=IF_THEN_STMT                    { let sp, s = sps in check_error s; s }
   | sps=SELECT_CASE_STMT                { let sp, s = sps in check_error s; s }
   | sps=WHERE_CONSTRUCT_STMT            { let sp, s = sps in check_error s; s }
   | sps=DERIVED_TYPE_STMT               { let sp, s = sps in check_error s; s }

   | sps=SPEC_PART_CONSTRUCT             { let sp, s = sps in check_error s; s }
   | sps=EXEC_PART_CONSTRUCT             { let sp, s = sps in check_error s; s }

   | l=linda_call                        { l }
;


(* * * * * *)

pp_ident:
   | name          { }
   | PP_IDENTIFIER { }
   | PP_UNDERSCORE { }

pp_branch:
   | PP_BRANCH { }
(*
   | PP_IF__COND        { }
   | PP_ELIF__COND      { }
   | PP_IFDEF__IDENT    { }
   | PP_IFNDEF__IDENT   { }
*)
   | PP_ELSE            { }
   | PP_ENDIF           { }
   | PP_IF pp_cond      { }
   | PP_ELIF pp_cond    { }
   | PP_IFDEF  pp_ident { }
   | PP_IFNDEF pp_ident { }
;

pp_cond:
   | PP_OR  { }
   | PP_AND { }
;

pp_directive:
   | p=PP_DEFINE__IDENT__BODY 
       { 
         let n, b = p in 
         mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Define(n, Macro.body_to_rep b)))) 
       }
   | h=PP_INCLUDE__FILE       { mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Include h))) }
   | i=PP_UNDEF__IDENT        { mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Undef i))) }
   | m=PP_ISSUE__MESG         { mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Message m))) }
(*
   | e=PP_ERROR__MESG         { mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Error e))) }
   | w=PP_WARNING__MESG       { mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Warning w))) }
*)
   | u=PP_UNKNOWN__REST       { let d, r = u in mkleaf $startpos $endpos (L.PpDirective (PpDirective.mk (PpDirective.Unknown(d, r)))) }
;

pp_macro_id:
   | km=PP_MACRO_ID  { let _, m = km in m }
   | i=PP_IDENTIFIER { i }
;

pp_token:
   | PP_DEFINE  { }
   | PP_INCLUDE { }
   | PP_UNDEF   { }
   | PP_ERROR   { }
   | PP_WARNING { }
   | PP_UNKNOWN { }

   | PP_MACRO_APPL { }

   | PP_CONCAT { }
;

(* * * * * *)

ocl_directive:
   | OCL_ARRAY_FUSION                      { mkleaf $startpos $endpos (L.mkocl OclDirective.ArrayFusion) }
   | OCL_END_ARRAY_FUSION                  { mkleaf $startpos $endpos (L.mkocl OclDirective.EndArrayFusion) }
   | OCL_ARRAY_MERGE t=ocl_tuple
       { 
         let n_opt, ns = ocl_tuple_to_n_opt_names t in
         mkleaf $startpos $endpos (L.mkocl (OclDirective.ArrayMerge(n_opt, ns))) 
       }
   | OCL_ARRAY_SUBSCRIPT t=ocl_tuple       { mkleaf $startpos $endpos (L.mkocl (OclDirective.ArraySubscript (ocl_tuple_to_names t))) }
   | OCL_EVAL                              { mkleaf $startpos $endpos (L.mkocl OclDirective.Eval) }
   | OCL_NOEVAL                            { mkleaf $startpos $endpos (L.mkocl OclDirective.Noeval) }
   | OCL_FLTLD                             { mkleaf $startpos $endpos (L.mkocl OclDirective.Fltld) }
   | OCL_NOFLTLD                           { mkleaf $startpos $endpos (L.mkocl OclDirective.Nofltld) }
   | OCL_FP_RELAXED                        { mkleaf $startpos $endpos (L.mkocl OclDirective.FpRelaxed) }
   | OCL_NOFP_RELAXED                      { mkleaf $startpos $endpos (L.mkocl OclDirective.NofpRelaxed) }
   | OCL_LOOP_INTERCHANGE t=ocl_tuple      { mkleaf $startpos $endpos (L.mkocl (OclDirective.LoopInterchange (ocl_tuple_to_names t))) }
   | OCL_LOOP_NOINTERCHANGE                { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopNointerchange) }
   | OCL_MFUNC t_opt=ioption(ocl_tuple)    { mkleaf $startpos $endpos (L.mkocl (OclDirective.Mfunc (ocl_tuple_opt_to_num_opt t_opt))) }
   | OCL_NOMFUNC                           { mkleaf $startpos $endpos (L.mkocl OclDirective.Nomfunc) }
   | OCL_NOARRAYPAD t=ocl_tuple            { mkleaf $startpos $endpos (L.mkocl (OclDirective.Noarraypad (ocl_tuple_to_name t))) }
   | OCL_LOOP_NOFUSION                     { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopNofusion) }
   | OCL_PREEX                             { mkleaf $startpos $endpos (L.mkocl OclDirective.Preex) }
   | OCL_NOPREEX                           { mkleaf $startpos $endpos (L.mkocl OclDirective.Nopreex) }
   | OCL_PREFETCH                          { mkleaf $startpos $endpos (L.mkocl OclDirective.Prefetch) }
   | OCL_NOPREFETCH                        { mkleaf $startpos $endpos (L.mkocl OclDirective.Noprefetch) }
   | OCL_PREFETCH_CACHE_LEVEL t=ocl_tuple  { mkleaf $startpos $endpos (L.mkocl (OclDirective.PrefetchCacheLevel (ocl_tuple_to_nn t))) }
   | OCL_PREFETCH_INFER                    { mkleaf $startpos $endpos (L.mkocl OclDirective.PrefetchInfer) }
   | OCL_PREFETCH_NOINFER                  { mkleaf $startpos $endpos (L.mkocl OclDirective.PrefetchNoinfer) }
   | OCL_PREFETCH_ITERATION t=ocl_tuple    { mkleaf $startpos $endpos (L.mkocl (OclDirective.PrefetchIteration (ocl_tuple_to_num t))) }
   | OCL_PREFETCH_ITERATION_L2 t=ocl_tuple { mkleaf $startpos $endpos (L.mkocl (OclDirective.PrefetchIterationL2 (ocl_tuple_to_num t))) }

   | OCL_PREFETCH_READ LPAREN r=var_or_ref l_opt=ioption(comma__level) s_opt=ioption(comma__strong) RPAREN 
       { 
         disambiguate_array_element r;
         mknode $startpos $endpos (L.mkocl(OclDirective.PrefetchRead(l_opt, s_opt))) [r]
       }
   | OCL_PREFETCH_WRITE LPAREN r=var_or_ref l_opt=ioption(comma__level) s_opt=ioption(comma__strong) RPAREN 
       { 
         disambiguate_array_element r;
         mknode $startpos $endpos (L.mkocl(OclDirective.PrefetchWrite(l_opt, s_opt))) [r]
       }
   | OCL_STRIPING t_opt=ioption(ocl_tuple)      { mkleaf $startpos $endpos (L.mkocl (OclDirective.Striping (ocl_tuple_opt_to_num_opt t_opt))) }
   | OCL_NOSTRIPING                             { mkleaf $startpos $endpos (L.mkocl OclDirective.Nostriping) }
   | OCL_SWP                                    { mkleaf $startpos $endpos (L.mkocl OclDirective.Swp) }
   | OCL_NOSWP                                  { mkleaf $startpos $endpos (L.mkocl OclDirective.Noswp) }
   | OCL_LOOP_BLOCKING t=ocl_tuple              { mkleaf $startpos $endpos (L.mkocl (OclDirective.LoopBlocking (ocl_tuple_to_num t))) }
   | OCL_UNROLL t=ocl_tuple                     { mkleaf $startpos $endpos (L.mkocl (OclDirective.Unroll (ocl_tuple_to_num t))) }
   | OCL_UNROLL LPAREN CHAR_LITERAL RPAREN      { mkleaf $startpos $endpos (L.mkocl (OclDirective.UnrollFull)) }
   | OCL_NOUNROLL                               { mkleaf $startpos $endpos (L.mkocl OclDirective.Nounroll) }
   | OCL_NOVREC t_opt=ioption(ocl_tuple)        { mkleaf $startpos $endpos (L.mkocl (OclDirective.Novrec (ocl_tuple_opt_to_names t_opt))) }
   | OCL_SIMD a_opt=ioption(ocl_align)          { mkleaf $startpos $endpos (L.mkocl (OclDirective.Simd a_opt)) }
   | OCL_NOSIMD                                 { mkleaf $startpos $endpos (L.mkocl OclDirective.Nosimd) }
   | OCL_CACHE_SECTOR_SIZE t=ocl_tuple          { mkleaf $startpos $endpos (L.mkocl (OclDirective.CacheSectorSize (ocl_tuple_to_nums t))) }
   | OCL_END_CACHE_SECTOR_SIZE                  { mkleaf $startpos $endpos (L.mkocl OclDirective.EndCacheSectorSize) }
   | OCL_CACHE_SUBSECTOR_ASSIGN t=ocl_tuple     { mkleaf $startpos $endpos (L.mkocl (OclDirective.CacheSubsectorAssign (ocl_tuple_to_names t))) }
   | OCL_END_CACHE_SUBSECTOR                    { mkleaf $startpos $endpos (L.mkocl OclDirective.EndCacheSubsector) }
   | OCL_FISSION_POINT t_opt=ioption(ocl_tuple) { mkleaf $startpos $endpos (L.mkocl (OclDirective.FissionPoint (ocl_tuple_opt_to_num_opt t_opt))) }
   | OCL_LOOP_NOFISSION                         { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopNofission) }
   | OCL_XFILL t_opt=ioption(ocl_tuple)         { mkleaf $startpos $endpos (L.mkocl (OclDirective.Xfill (ocl_tuple_opt_to_num_opt t_opt))) }
   | OCL_NOXFILL                                { mkleaf $startpos $endpos (L.mkocl OclDirective.Noxfill) }

   | OCL_PREFETCH_SEQUENTIAL p_opt=ioption(ocl_prefetch_spec) 
       { 
         mkleaf $startpos $endpos (L.mkocl (OclDirective.PrefetchSequential p_opt)) 
       }
   | OCL_PREFETCH_STRONG                        { mkleaf $startpos $endpos (L.mkocl OclDirective.PrefetchStrong) }
   | OCL_PREFETCH_NOSTRONG                      { mkleaf $startpos $endpos (L.mkocl OclDirective.PrefetchNostrong) }
   | OCL_PREFETCH_STRONG_L2                     { mkleaf $startpos $endpos (L.mkocl OclDirective.PrefetchStrongL2) }
   | OCL_PREFETCH_NOSTRONG_L2                   { mkleaf $startpos $endpos (L.mkocl OclDirective.PrefetchNostrongL2) }
   | OCL_FP_CONTRACT                            { mkleaf $startpos $endpos (L.mkocl OclDirective.FpContract) }
   | OCL_NOFP_CONTRACT                          { mkleaf $startpos $endpos (L.mkocl OclDirective.NofpContract) }
   | OCL_LOOP_NOBLOCKING                        { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopNoblocking) }
   | OCL_NORECURRENCE t_opt=ioption(ocl_tuple)  { mkleaf $startpos $endpos (L.mkocl (OclDirective.Norecurrence (ocl_tuple_opt_to_names t_opt))) }
   | OCL_UXSIMD a_opt=ioption(ocl_align)        { mkleaf $startpos $endpos (L.mkocl (OclDirective.Uxsimd a_opt)) }
   | OCL_NOUXSIMD                               { mkleaf $startpos $endpos (L.mkocl OclDirective.Nouxsimd) }
   | OCL_ARRAY_PRIVATE                          { mkleaf $startpos $endpos (L.mkocl OclDirective.ArrayPrivate) }
   | OCL_NOARRAY_PRIVATE                        { mkleaf $startpos $endpos (L.mkocl OclDirective.NoarrayPrivate) }
   | OCL_INDEPENDENT t_opt=ioption(ocl_tuple)   { mkleaf $startpos $endpos (L.mkocl (OclDirective.Independent (ocl_tuple_opt_to_names t_opt))) }
   | OCL_NOALIAS                                { mkleaf $startpos $endpos (L.mkocl OclDirective.Noalias) }
   | OCL_SERIAL                                 { mkleaf $startpos $endpos (L.mkocl OclDirective.Serial) }
   | OCL_PARALLEL                               { mkleaf $startpos $endpos (L.mkocl OclDirective.Parallel) }
   | OCL_PARALLEL_STRONG                        { mkleaf $startpos $endpos (L.mkocl OclDirective.ParallelStrong) }
   | OCL_REDUCTION                              { mkleaf $startpos $endpos (L.mkocl OclDirective.Reduction) }
   | OCL_NOREDUCTION                            { mkleaf $startpos $endpos (L.mkocl OclDirective.Noreduction) }
   | OCL_TEMP t_opt=ioption(ocl_tuple)          { mkleaf $startpos $endpos (L.mkocl (OclDirective.Temp (ocl_tuple_opt_to_names t_opt))) }
   | l=primary op=rel_op r=primary              { mknode $startpos $endpos (L.mkocl (OclDirective.RelOp op)) [l; r] }

   | OCL_LOOP_PART_SIMD                         { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopPartSimd) }
   | OCL_LOOP_NOPART_SIMD                       { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopNopartSimd) }
   | OCL_SHORTLOOP t=ocl_tuple                  { mkleaf $startpos $endpos (L.mkocl (OclDirective.Shortloop (ocl_tuple_to_num t))) }
   | OCL_NOSHORTLOOP                            { mkleaf $startpos $endpos (L.mkocl OclDirective.Noshortloop) }
   | OCL_SIMD_LISTV s_opt=ioption(ocl_scope)    { mkleaf $startpos $endpos (L.mkocl (OclDirective.SimdListv s_opt)) }
   | OCL_UNSWITCHING                            { mkleaf $startpos $endpos (L.mkocl OclDirective.Unswitching) }

   | OCL_LOOP_PART_PARALLEL                       { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopPartParallel) }
   | OCL_LOOP_NOPART_PARALLEL                     { mkleaf $startpos $endpos (L.mkocl OclDirective.LoopNopartParallel) }
   | OCL_FIRST_PRIVATE t=ocl_tuple                { mkleaf $startpos $endpos (L.mkocl (OclDirective.FirstPrivate (ocl_tuple_to_names t))) }
   | OCL_LAST_PRIVATE  t=ocl_tuple                { mkleaf $startpos $endpos (L.mkocl (OclDirective.LastPrivate (ocl_tuple_to_names t))) }
   | OCL_TEMP_PRIVATE  t=ocl_tuple                { mkleaf $startpos $endpos (L.mkocl (OclDirective.TempPrivate (ocl_tuple_to_names t))) }
   | OCL_PARALLEL_CYCLIC t_opt=ioption(ocl_tuple) { mkleaf $startpos $endpos (L.mkocl (OclDirective.ParallelCyclic (ocl_tuple_opt_to_num_opt t_opt))) }

   | error                                        { mkleaf $startpos $endpos (L.mkocl OclDirective.ERROR) }
;

ocl_scope:
   | LPAREN s=_ocl_scope RPAREN { s }
;

_ocl_scope:
   | OCL_ALL  { OclDirective.Sall }
   | OCL_THEN { OclDirective.Sthen }
   | OCL_ELSE { OclDirective.Selse }
;

ocl_tuple:
   | LPAREN nn_opt=ioption(ocl_nn__colon) nns=clist(ocl_nn) RPAREN { nn_opt, nns }
;

ocl_nn__colon:
   | nn=ocl_nn COLON { nn }
;

%inline
comma__level:
   | COMMA OCL_LEVEL EQ l=ocl_nn { l }
;

ocl_nn:
   | i=INT_LITERAL { OclDirective.NNnum (int_of_string i) }
   | n=IDENTIFIER  { OclDirective.NNname n }
;

%inline
comma__strong:
   | COMMA OCL_STRONG EQ i=INT_LITERAL { int_of_string i }
;

%inline
ocl_align:
   | LPAREN OCL_ALIGNED   RPAREN { OclDirective.Aaligned }
   | LPAREN OCL_UNALIGNED RPAREN { OclDirective.Aunaligned }
;

%inline
ocl_prefetch_spec:
   | LPAREN OCL_AUTO RPAREN { OclDirective.Pauto }
   | LPAREN OCL_SOFT RPAREN { OclDirective.Psoft }
;

ocl:
   | o=_ocl EOL { o }
;

_ocl:
   | ds=clist(ocl_directive) { mknode $startpos $endpos L.OCL ds }
;

(* * * * * *)

xlf_directive:
  | XLF_ALIGN LPAREN i=int_literal COMMA nl=clist(name) RPAREN             { mknode $startpos $endpos (L.mkxlfd (Xlf.Directive.Align i)) nl }
  | XLF_ASSERT LPAREN al=clist(xlf_assertion) RPAREN                       { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Assert) al }
  | XLF_BLOCK_LOOP LPAREN e=expr nl=clist0(name) RPAREN                    { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.BlockLoop) (e :: nl) }
  | XLF_CNCALL                                                             { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.Cncall) }
  | XLF_COLLAPSE LPAREN cal=clist(collapse_array) RPAREN                   { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Collapse) cal }
  | XLF_EJECT                                                              { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.Eject) }
  | XLF_EXECUTION_FREQUENCY LPAREN hl=high_or_low RPAREN                   { mkleaf $startpos $endpos (L.mkxlfd (Xlf.Directive.ExecutionFrequency hl)) }
  | XLF_EXPECTED_VALUE LPAREN n=name COMMA e=expr RPAREN                   { mknode $startpos $endpos (L.mkxlfd (Xlf.Directive.ExpectedValue n#get_name)) [e] }
  | XLF_FUNCTRACE_XLF_CATCH                                                { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.FunctraceXlfCatch) }
  | XLF_FUNCTRACE_XLF_ENTER                                                { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.FunctraceXlfEnter) }
  | XLF_FUNCTRACE_XLF_EXIT                                                 { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.FunctraceXlfExit) }
  | XLF_IGNORE_TKR                                                         { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.IgnoreTkr) }
  | XLF_INDEPENDENT                                                        { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Independent) [] }
  | XLF_INDEPENDENT COMMA cl=clist(independent_clause)                     { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Independent) cl }
  | XLF_LOOPID LPAREN n=name RPAREN                                        { mkleaf $startpos $endpos (L.mkxlfd (Xlf.Directive.Loopid n#get_name)) }
  | XLF_MEM_DELAY LPAREN v=var_or_ref COMMA c=xlf_cycle RPAREN          
      { 
        disambiguate_variable v;
        mknode $startpos $endpos (L.mkxlfd (Xlf.Directive.MemDelay c)) [v]
      }
  | XLF_NEW nl=clist(name)                                                { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.New) nl }
  | XLF_NOFUNCTRACE                                                       { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.Nofunctrace) }
  | XLF_NOSIMD                                                            { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.Nosimd) }
  | XLF_NOVECTOR                                                          { mkleaf $startpos $endpos (L.mkxlfd Xlf.Directive.Novector) }
  | XLF_PERMUTATION LPAREN nl=clist(name) RPAREN                          { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Permutation) nl }
  | XLF_SNAPSHOT LPAREN nl=clist(name) RPAREN                             { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Snapshot) nl }
  | XLF_SOURCEFORM LPAREN s=xlf_source RPAREN                             { mkleaf $startpos $endpos (L.mkxlfd (Xlf.Directive.Sourceform s)) }
  | XLF_STREAM_UNROLL e_opt=ioption(xlf_factor)                           { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.StreamUnroll) (opt_to_list e_opt) }
  | XLF_SUBSCRIPTORDER LPAREN sal=clist(subscriptorder_array) RPAREN      { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Subscriptorder) sal }
  | XLF_UNROLL e_opt=ioption(xlf_factor)                                  { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Unroll) (opt_to_list e_opt) }
  | XLF_UNROLL_AND_FUSE e_opt=ioption(xlf_factor)                         { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.UnrollAndFuse) (opt_to_list e_opt) }

  | XLF_PROCESS ol=clist(xlf_option)                                      { mknode $startpos $endpos (L.mkxlfd Xlf.Directive.Process) ol }
;

high_or_low:
  | XLF_VERY_HIGH { Xlf.Directive.VeryHigh }
  | XLF_VERY_LOW  { Xlf.Directive.VeryLow }
;

xlf_option:
  | n=name                              { mkleaf $startpos $endpos (L.mkxlf (Xlf.Option n#get_name)) }
  | n=name LPAREN el=clist(expr) RPAREN { mknode $startpos $endpos (L.mkxlf (Xlf.Option n#get_name)) el }
;

xlf_cycle:
  | i=int_literal { i }
  | n=name        { n#get_name } (* named constant *)
;

xlf_factor:
  | LPAREN e=expr RPAREN { e }
;

xlf_source:
  | XLF_FIXED LPAREN i=int_literal RPAREN { Xlf.Directive.Fixed (Some (int_of_string i)) }
  | XLF_FIXED                             { Xlf.Directive.Fixed None }
  | XLF_FREE LPAREN XLF_F90 RPAREN        { Xlf.Directive.FreeF90 }
  | XLF_FREE LPAREN XLF_IBM RPAREN        { Xlf.Directive.FreeIBM }
  | XLF_FREE                              { Xlf.Directive.Free }
;

independent_clause:
  | XLF_NEW       LPAREN nl=clist(name) RPAREN { mknode $startpos $endpos (L.mkxlf Xlf.NewClause) nl }
  | XLF_REDUCTION LPAREN nl=clist(name) RPAREN { mknode $startpos $endpos (L.mkxlf Xlf.ReductionClause) nl }
;

collapse_array:
  | n=name LPAREN el=clist(expr) RPAREN { mknode $startpos $endpos (L.mkxlf (Xlf.CollapseArray n#get_name)) el }
;

subscriptorder_array:
  | n=name LPAREN il=clist(int_literal) RPAREN { mkleaf $startpos $endpos (L.mkxlf (Xlf.SubscriptorderArray(n#get_name, il))) }
;

xlf_assertion:
  | XLF_ITERCNT    LPAREN e=expr RPAREN { mknode $startpos $endpos (L.mkxlfa Xlf.Assertion.Itercnt) [e] }
  | XLF_MINITERCNT LPAREN e=expr RPAREN { mknode $startpos $endpos (L.mkxlfa Xlf.Assertion.Minitercnt) [e] }
  | XLF_MAXITERCNT LPAREN e=expr RPAREN { mknode $startpos $endpos (L.mkxlfa Xlf.Assertion.Maxitercnt) [e] }
  | XLF_NODEPS                          { mkleaf $startpos $endpos (L.mkxlfa Xlf.Assertion.Nodeps) }
;

xlf:
   | x=_xlf EOL { x }
;

_xlf:
   | d=xlf_directive { mknode $startpos $endpos L.XLF [d] }
;


(* * * * * *)

dec_directive:
  | DEC_ALIAS i=name COMMA e=name           { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Alias(i#get_name, e#get_name))) }
  | DEC_ASSUME LPAREN e=expr RPAREN         { mknode $startpos $endpos (L.mkdecd Dec.Directive.Assume) [e] }
  | DEC_ASSUME_ALIGNED l=clist(dec_var_expr) { mknode $startpos $endpos (L.mkdecd Dec.Directive.Assume_aligned) l }
  | DEC_ATTRIBUTES al=clist(dec_att) COLON_COLON nl=clist(name)
      { 
        mknode $startpos $endpos (L.mkdecd Dec.Directive.Attributes) (al@nl)
      }
  | DEC_BLOCK_LOOP cs=coplist0(dec_clause)  { mknode $startpos $endpos (L.mkdecd Dec.Directive.Block_loop) cs }
  | DEC_NOBLOCK_LOOP                        { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Noblock_loop) }
  | DEC_CODE_ALIGN COLON i=int_literal      { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Code_align (int_of_string i))) }
  | DEC_CODE_ALIGN                          { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Code_align 0)) }
  | DEC_DECLARE                             { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Declare) }
  | DEC_NODECLARE                           { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nodeclare) }
  | DEC_DEFINE n=name                       { mknode $startpos $endpos (L.mkdecd (Dec.Directive.Define n#get_name)) [] }
  | DEC_DEFINE n=name EQ e=expr             { mknode $startpos $endpos (L.mkdecd (Dec.Directive.Define n#get_name)) [e] }
  | DEC_UNDEFINE n=name                     { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Undefine n#get_name)) }
  | DEC_DISTRIBUTE DEC_POINT                { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.DistributePoint) }
  | DEC_DISTRIBUTEPOINT                     { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.DistributePoint) }
  | DEC_FIXEDFORMLINESIZE COLON i=int_literal { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Fixedformlinesize i)) }
  | DEC_FMA                                 { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Fma) }
  | DEC_NOFMA                               { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nofma) }
  | DEC_FREEFORM                            { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Freeform) }
  | DEC_NOFREEFORM                          { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nofreeform) }
  | DEC_IDENT c=CHAR_LITERAL                { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Ident c)) }
  | DEC_IF LPAREN e=expr RPAREN             { mknode $startpos $endpos (L.mkdecd Dec.Directive.If) [e] }
  | DEC_IF DEC_DEFINED LPAREN n=name RPAREN { mknode $startpos $endpos (L.mkdecd (Dec.Directive.IfDefined n#get_name)) [] }
  | DEC_IFDEFINED LPAREN n=name RPAREN      { mknode $startpos $endpos (L.mkdecd (Dec.Directive.IfDefined n#get_name)) [] }
  | DEC_ELSEIF LPAREN e=expr RPAREN         { mknode $startpos $endpos (L.mkdecd Dec.Directive.Elseif) [e] }
  | DEC_ELSE                                { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Else) }
  | DEC_ENDIF                               { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Endif) }
  | DEC_INLINE                              { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Inline false)) }
  | DEC_INLINE DEC_RECURSIVE                { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Inline true)) }
  | DEC_FORCEINLINE                         { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Forceinline false)) }
  | DEC_FORCEINLINE DEC_RECURSIVE           { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Forceinline true)) }
  | DEC_NOINLINE                            { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Noinline) }
  | DEC_INTEGER COLON i=INT_LITERAL         { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Integer i)) }
  | DEC_IVDEP                               { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Ivdep "")) }
  | DEC_IVDEP COLON o=name                  { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Ivdep o#get_name)) }
  | DEC_INIT_DEP_FWD                        { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Init_dep_fwd) }
  | DEC_LOOP DEC_COUNT lc=dec_loop_count    { let l, c = lc in mknode $startpos $endpos (L.mkdecd (Dec.Directive.LoopCount l)) c }
  | DEC_LOOPCOUNT lc=dec_loop_count         { let l, c = lc in mknode $startpos $endpos (L.mkdecd (Dec.Directive.LoopCount l)) c }
  | DEC_MESSAGE COLON c=CHAR_LITERAL        { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Message c)) }
  | DEC_NOFUSION                            { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nofusion) }
  | DEC_OBJCOMMENT name COLON c=CHAR_LITERAL { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Objcomment c)) }
  | DEC_OPTIMIZE                            { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Optimize "")) }
  | DEC_OPTIMIZE COLON i=INT_LITERAL        { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Optimize i)) }
  | DEC_NOOPTIMIZE                          { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nooptimize) }
  | DEC_OPTIONS os=dec_option+              { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Options os)) }
  | DEC_END DEC_OPTIONS                     { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.EndOptions) }
  | DEC_ENDOPTIONS                          { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.EndOptions) }
  | DEC_PACK                                { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Pack "")) }
  | DEC_PACK COLON i=INT_LITERAL            { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Pack i)) }
  | DEC_PARALLEL cs=coplist0(dec_clause)    { mknode $startpos $endpos (L.mkdecd Dec.Directive.Parallel) cs }
  | DEC_NOPARALLEL                          { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Noparallel) }
  | DEC_PREFETCH h=dec_prefetch_hint        { mknode $startpos $endpos (L.mkdecd Dec.Directive.Prefetch) h }
  | DEC_NOPREFETCH vs=clist0(var_or_ref)
      { 
        List.iter disambiguate_variable vs;
        mknode $startpos $endpos (L.mkdecd Dec.Directive.Noprefetch) vs
      }
  | DEC_PSECT SLASH n=name SLASH al=clist(dec_a)
      { 
        mknode $startpos $endpos (L.mkdecd (Dec.Directive.Psect n#get_name)) al
      }
  | DEC_REAL COLON i=INT_LITERAL            { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Real i)) }
  | DEC_SIMD cs=coplist0(dec_clause)        { mknode $startpos $endpos (L.mkdecd Dec.Directive.Simd) cs }
  | DEC_STRICT                              { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Strict) }
  | DEC_NOSTRICT                            { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nostrict) }
  | DEC_UNROLL i_opt=ioption(dec_int_spec)         { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Unroll i_opt)) }
  | DEC_NOUNROLL                                   { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nounroll) }
  | DEC_UNROLL_AND_JAM i_opt=ioption(dec_int_spec) { mkleaf $startpos $endpos (L.mkdecd (Dec.Directive.Unroll_and_jam i_opt)) }
  | DEC_NOUNROLL_AND_JAM                           { mkleaf $startpos $endpos (L.mkdecd Dec.Directive.Nounroll_and_jam) }
  | DEC_VECTOR   cs=coplist0(dec_clause)    { mknode $startpos $endpos (L.mkdecd Dec.Directive.Vector) cs }
  | DEC_NOVECTOR cs=coplist0(dec_clause)    { mknode $startpos $endpos (L.mkdecd Dec.Directive.Novector) cs }
;

dec_att:
  | DEC_VECTOR COLON LPAREN l=clist(dec_clause) RPAREN { mknode $startpos $endpos (L.mkdeca Dec.Attribute.Vector) l }

  | DEC_ALIAS COLON s=char_literal { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.Alias s)) }
  | a=name    COLON s=char_literal { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.of_keyword_string a#get_name s)) }

  | DEC_ALIGN      COLON i=int_literal  { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.Align (int_of_string i))) }
  | DEC_CODE_ALIGN COLON i=int_literal  { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.Code_align (int_of_string i))) }
  | a=name         COLON i=int_literal  { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.of_keyword_int a#get_name (int_of_string i))) }

  | a=name COLON n=name { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.of_keyword_name a#get_name n#get_name)) }

  | DEC_VECTOR COLON c=dec_clause { mknode $startpos $endpos (L.mkdeca Dec.Attribute.Vector) [c] }
  | a=name     COLON c=dec_clause { mknode $startpos $endpos (L.mkdeca (Dec.Attribute.of_keyword a#get_name)) [c] }

  | DEC_INLINE   { mkleaf $startpos $endpos (L.mkdeca Dec.Attribute.Inline) }
  | DEC_NOINLINE { mkleaf $startpos $endpos (L.mkdeca Dec.Attribute.Noinline) }
  | DEC_VECTOR   { mkleaf $startpos $endpos (L.mkdeca Dec.Attribute.Vector) }
  | a=name       { mkleaf $startpos $endpos (L.mkdeca (Dec.Attribute.of_keyword a#get_name)) }
;

dec_option:
  | SLASH DEC_OFFLOAD_ATTRIBUTE_TARGET EQ n=name { Printf.sprintf "/OFFLOAD_ATTRIBUTE_TARGET=%s" n#get_name }
  | SLASH DEC_WARN EQ DEC_ALIGNMENT   { "/WARN=ALIGNMENT" }
  | SLASH DEC_WARN EQ DEC_NOALIGNMENT { "/WARN=NOALIGNMENT" }
  | SLASH DEC_ALIGN   p_opt=ioption(dec_p) { Printf.sprintf "/ALIGN=%s" (string_opt_to_string p_opt) }
  | SLASH DEC_NOALIGN p_opt=ioption(dec_p) { Printf.sprintf "/NOALIGN=%s" (string_opt_to_string p_opt) }
;

dec_p:
  | n=name                                { n#get_name }
  | c=dec_class_rule                      { c }
  | LPAREN l=clist(dec_class_rule) RPAREN { Printf.sprintf "(%s)" (Xlist.to_string (fun x -> x) "," l) }
;

dec_class_rule:
  | c=name EQ r=name { Printf.sprintf "%s=%s" c#get_name r#get_name }
;


dec_hint:
  | v=var_or_ref                           { disambiguate_variable v; mknode $startpos $endpos (L.mkdec Dec.PrefetchHint) [v] }
  | v=var_or_ref COLON h=expr              { disambiguate_variable v; mknode $startpos $endpos (L.mkdec Dec.PrefetchHint) [v;h] }
  | v=var_or_ref COLON h=expr COLON d=expr { disambiguate_variable v; mknode $startpos $endpos (L.mkdec Dec.PrefetchHint) [v;h;d] }
;

dec_prefetch_hint:
  | l=clist0(dec_hint) { l }
  | STAR COLON h=expr d_opt=ioption(colon__expr) { [mknode $startpos $endpos (L.mkdec Dec.PrefetchHintAll) (h::(opt_to_list d_opt))] }
;

dec_a:
  | DEC_ALIGN EQ kw=IDENTIFIER { mkleaf $startpos $endpos (L.mkdec (Dec.Align kw)) }
  | DEC_ALIGN EQ v=INT_LITERAL { mkleaf $startpos $endpos (L.mkdec (Dec.Align v)) }
  | DEC_WRT                    { mkleaf $startpos $endpos (L.mkdec Dec.Wrt) }
  | DEC_NOWRT                  { mkleaf $startpos $endpos (L.mkdec Dec.Nowrt) }
;

dec_clause:
  | DEC_ALWAYS                                     { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Always) }
  | DEC_ASSERT                                     { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Assert) }
  | DEC_NOASSERT                                   { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Noassert) }
  | DEC_ALIGNED                                    { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Aligned) }
  | DEC_UNALIGNED                                  { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Unaligned) }
  | DEC_TEMPORAL    ns_opt=ioption(dec_list_p)     { mknode $startpos $endpos (L.mkdecc Dec.Clause.Temporal) (list_opt_to_list ns_opt) }
  | DEC_NONTEMPORAL ns_opt=ioption(dec_list_p)     { mknode $startpos $endpos (L.mkdecc Dec.Clause.Nontemporal) (list_opt_to_list ns_opt) }
  | DEC_VECREMAINDER                               { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Vecremainder) }
  | DEC_NOVECREMAINDER                             { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Novecremainder) }
  | DEC_FIRSTPRIVATE ns=dec_list_p                 { mknode $startpos $endpos (L.mkdecc Dec.Clause.Firstprivate) ns }
  | DEC_LASTPRIVATE  ns=dec_list_p                 { mknode $startpos $endpos (L.mkdecc Dec.Clause.Lastprivate) ns }
  | DEC_LINEAR LPAREN l=clist(dec_var_expr) RPAREN { mknode $startpos $endpos (L.mkdecc Dec.Clause.Linear) l }
  | DEC_PRIVATE ns=dec_list_p                      { mknode $startpos $endpos (L.mkdecc Dec.Clause.Private) ns }
  | DEC_REDUCTION LPAREN op_vs=dec_op_var RPAREN
      { 
        let op, vs = op_vs in
        mknode $startpos $endpos (L.mkdecc Dec.Clause.Reduction) (op::vs)
      }
  | DEC_VECTORLENGTH LPAREN l=clist(dec_int) RPAREN   { mkleaf $startpos $endpos (L.mkdecc (Dec.Clause.Vectorlength l)) }
  | DEC_VECTORLENGTHFOR LPAREN t=dec_data_type RPAREN { mknode $startpos $endpos (L.mkdecc Dec.Clause.Vectorlengthfor) [t] }
  | DEC_NUM_THREADS LPAREN e=expr RPAREN              { mknode $startpos $endpos (L.mkdecc Dec.Clause.Num_threads) [e] }

  | DEC_MASK                           { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Mask) }
  | DEC_NOMASK                         { mkleaf $startpos $endpos (L.mkdecc Dec.Clause.Nomask) }
  | DEC_PROCESSOR LPAREN n=name RPAREN { mkleaf $startpos $endpos (L.mkdecc (Dec.Clause.Processor n#get_name)) }
  | DEC_UNIFORM ns=dec_list_p          { mknode $startpos $endpos (L.mkdecc Dec.Clause.Uniform) ns }
  | DEC_FACTOR LPAREN e=expr RPAREN    { mknode $startpos $endpos (L.mkdecc Dec.Clause.Factor) [e] }
  | DEC_LEVEL LPAREN ls=clist(dec_level) RPAREN { mkleaf $startpos $endpos (L.mkdecc (Dec.Clause.Level ls)) }
;

dec_data_type:
  | t=type_spec_no_character { t }
;

%inline
dec_op_var:
  | op=dec_op COLON vs=clist(var_or_ref)
      { 
        List.iter disambiguate_variable vs;
        op, vs
      }
;

dec_iop:
   | PLUS         { IntrinsicOperator.Add }
   | STAR         { IntrinsicOperator.Mult }
   | MINUS        { IntrinsicOperator.Subt }
   | D_AND        { IntrinsicOperator.AND }
   | D_OR         { IntrinsicOperator.OR }
   | D_EQV        { IntrinsicOperator.EQV }
   | D_NEQV       { IntrinsicOperator.NEQV }
;

dec_op:
   | op=dec_iop { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
;

dec_var_expr:
  | v=var_or_ref COLON e=expr
      { 
        disambiguate_variable v;
        mknode $startpos $endpos (L.mkdec Dec.VarExpr) [v; e]
      }
;

dec_list_p:
  | LPAREN l=dec_list RPAREN { l }
;

dec_list:
  | l=clist(dec_list_item) { l }
;

dec_list_item:
  | n=name             { mkleaf $startpos $endpos (L.Name n#get_name) }
  | SLASH n=name SLASH { mkleaf $startpos $endpos (L.CommonBlockName n#get_name) }
;

dec_int:
  | i=INT_LITERAL { int_of_string i }
;

dec_level:
  | i=dec_int                   { (i, 0) }
  | i0=dec_int COLON i1=dec_int { (i0, i1) }
;

dec_int_spec:
  | LPAREN i=dec_int RPAREN { i }
  | EQ i=dec_int            { i }
;

dec_loop_count:
  | LPAREN il=clist(dec_int) RPAREN { il, [] }
  | EQ il=clist(dec_int)            { il, [] }
  | max=name i0=dec_int_spec COMMA min=name i1=dec_int_spec COMMA avg=name i2=dec_int_spec
      { 
        let max_str, min_str, avg_str = max#get_name, min#get_name, avg#get_name in
        ignore (max_str, min_str, avg_str);
        let max_nd = mkleaf $startpos(max) $endpos(i0) (L.mkdec (Dec.Max i0)) in
        let min_nd = mkleaf $startpos(min) $endpos(i1) (L.mkdec (Dec.Min i1)) in
        let avg_nd = mkleaf $startpos(avg) $endpos(i2) (L.mkdec (Dec.Avg i2)) in
        [], [max_nd; min_nd; avg_nd]
      }
;

dec:
  | d=_dec EOL { d }
;

_dec:
  | d=dec_directive { mknode $startpos $endpos L.DEC [d] }
;


(* * * * * *)

acc_directive:
   | ACC_PARALLEL cl=coplist0(acc_clause)                          { mknode $startpos $endpos (L.mkacc AccDirective.Parallel) cl }
   | ACC_END ACC_PARALLEL                                          { mkleaf $startpos $endpos (L.mkacc AccDirective.EndParallel) }
   | ACC_PARALLEL ACC_LOOP cl=coplist0(acc_clause)                 { mknode $startpos $endpos (L.mkacc AccDirective.ParallelLoop) cl }
   | ACC_END ACC_PARALLEL ACC_LOOP                                 { mkleaf $startpos $endpos (L.mkacc AccDirective.EndParallelLoop) }
   | ACC_KERNELS cl=coplist0(acc_clause)                           { mknode $startpos $endpos (L.mkacc AccDirective.Kernels) cl }
   | ACC_END ACC_KERNELS                                           { mkleaf $startpos $endpos (L.mkacc AccDirective.EndKernels) }
   | ACC_KERNELS ACC_LOOP cl=coplist0(acc_clause)                  { mknode $startpos $endpos (L.mkacc AccDirective.KernelsLoop) cl }
   | ACC_END ACC_KERNELS ACC_LOOP                                  { mkleaf $startpos $endpos (L.mkacc AccDirective.EndKernelsLoop) }
   | ACC_DATA cl=coplist0(acc_clause)                              { mknode $startpos $endpos (L.mkacc AccDirective.Data) cl }
   | ACC_END ACC_DATA                                              { mkleaf $startpos $endpos (L.mkacc AccDirective.EndData) }
   | ACC_ENTER ACC_DATA cl=coplist0(acc_clause)                    { mknode $startpos $endpos (L.mkacc AccDirective.EnterData) cl }
   | ACC_EXIT  ACC_DATA cl=coplist0(acc_clause)                    { mknode $startpos $endpos (L.mkacc AccDirective.ExitData) cl }
   | ACC_HOST_DATA cl=coplist0(acc_clause)                         { mknode $startpos $endpos (L.mkacc AccDirective.Host_data) cl }
   | ACC_END ACC_HOST_DATA                                         { mkleaf $startpos $endpos (L.mkacc AccDirective.EndHost_data) }
   | ACC_LOOP cl=coplist0(acc_clause)                              { mknode $startpos $endpos (L.mkacc AccDirective.Loop) cl }
   | ACC_CACHE LPAREN l=acc_list RPAREN                            { mknode $startpos $endpos (L.mkacc AccDirective.Cache) l }
   | ACC_ATOMIC                                                    { mkleaf $startpos $endpos (L.mkacc (AccDirective.Atomic None)) }
   | ACC_ATOMIC a=acc_atomic                                       { mkleaf $startpos $endpos (L.mkacc (AccDirective.Atomic (Some a))) }
   | ACC_END ACC_ATOMIC                                            { mkleaf $startpos $endpos (L.mkacc AccDirective.EndAtomic) }
   | ACC_UPDATE cl=coplist0(acc_clause)                            { mknode $startpos $endpos (L.mkacc AccDirective.Update) cl }
   | ACC_WAIT                              cl=coplist0(acc_clause) { mknode $startpos $endpos (L.mkacc AccDirective.Wait) cl }
   | ACC_WAIT LPAREN el=clist(expr) RPAREN cl=coplist0(acc_clause) { mknode $startpos $endpos (L.mkacc AccDirective.Wait) (el @ cl) }
   | ACC_ROUTINE                      cl=coplist0(acc_clause)      { mknode $startpos $endpos (L.mkacc (AccDirective.Routine None)) cl }
   | ACC_ROUTINE LPAREN n=name RPAREN cl=coplist0(acc_clause)      { mknode $startpos $endpos (L.mkacc (AccDirective.Routine (Some n#get_name))) cl }
   | ACC_DECLARE cl=coplist0(acc_clause)                           { mknode $startpos $endpos (L.mkacc AccDirective.Declare) cl }
;

acc_atomic:
   | ACC_READ    { AccDirective.Read }
   | ACC_WRITE   { AccDirective.Write }
   | ACC_CAPTURE { AccDirective.Capture }
   | ACC_UPDATE  { AccDirective.Update }
;

acc_clause:
   | ACC_ASYNC                                              { mkleaf $startpos $endpos (L.mkaccc AccClause.Async) }
   | ACC_ASYNC LPAREN e=expr RPAREN                         { mknode $startpos $endpos (L.mkaccc AccClause.Async) [e] }
   | ACC_AUTO                                               { mkleaf $startpos $endpos (L.mkaccc AccClause.Auto) }
   | ACC_BIND LPAREN n=name RPAREN                          { mknode $startpos $endpos (L.mkaccc AccClause.Bind) [n] }
   | ACC_BIND LPAREN s=char_literal RPAREN                  { mknode $startpos $endpos (L.mkaccc AccClause.Bind) [ mkleaf $startpos(s) $endpos(s) (L.Constant (Constant.mkchar s))] }
   | ACC_COLLAPSE LPAREN i=int_literal RPAREN               { mknode $startpos $endpos (L.mkaccc AccClause.Collapse) [ mkleaf $startpos(i) $endpos(i) (L.Constant (Constant.mkint i))] }
   | ACC_COPY LPAREN l=acc_list RPAREN                      { mknode $startpos $endpos (L.mkaccc AccClause.Copy) l }
   | ACC_COPYIN LPAREN l=acc_list RPAREN                    { mknode $startpos $endpos (L.mkaccc AccClause.Copyin) l }
   | ACC_COPYOUT LPAREN l=acc_list RPAREN                   { mknode $startpos $endpos (L.mkaccc AccClause.Copyout) l }
   | ACC_CREATE LPAREN l=acc_list RPAREN                    { mknode $startpos $endpos (L.mkaccc AccClause.Create) l }
   | ACC_DEFAULT LPAREN ACC_NONE RPAREN                     { mkleaf $startpos $endpos (L.mkaccc AccClause.DefaultNone) }
   | ACC_DEFAULT LPAREN ACC_PRESENT RPAREN                  { mkleaf $startpos $endpos (L.mkaccc AccClause.DefaultPresent) }
   | ACC_DELETE LPAREN l=acc_list RPAREN                    { mknode $startpos $endpos (L.mkaccc AccClause.Delete) l }
   | ACC_DEVICE LPAREN l=acc_list RPAREN                    { mknode $startpos $endpos (L.mkaccc AccClause.Device) l }
   | ACC_DEVICEPTR LPAREN l=acc_list RPAREN                 { mknode $startpos $endpos (L.mkaccc AccClause.Deviceptr) l }
   | ACC_DEVICE_RESIDENT LPAREN l=acc_list RPAREN           { mknode $startpos $endpos (L.mkaccc AccClause.Device_resident) l }
   | ACC_DEVICE_TYPE LPAREN STAR RPAREN                     { mkleaf $startpos $endpos (L.mkaccc AccClause.Device_typeAny) }
   | ACC_DEVICE_TYPE LPAREN al=dtype_args RPAREN            { mknode $startpos $endpos (L.mkaccc AccClause.Device_type) al }
   | ACC_DTYPE LPAREN STAR RPAREN                           { mkleaf $startpos $endpos (L.mkaccc AccClause.DtypeAny) }
   | ACC_DTYPE LPAREN al=dtype_args RPAREN                  { mknode $startpos $endpos (L.mkaccc AccClause.Dtype) al }
   | ACC_FIRSTPRIVATE LPAREN l=acc_list RPAREN              { mknode $startpos $endpos (L.mkaccc AccClause.Firstprivate) l }
   | ACC_GANG                                               { mkleaf $startpos $endpos (L.mkaccc AccClause.Gang) }
   | ACC_GANG LPAREN el=clist(expr) RPAREN                  { mknode $startpos $endpos (L.mkaccc AccClause.Gang) el }
   | ACC_HOST LPAREN l=acc_list RPAREN                      { mknode $startpos $endpos (L.mkaccc AccClause.Host) l }
   | ACC_IF LPAREN e=expr RPAREN                            { mknode $startpos $endpos (L.mkaccc AccClause.If) [e] }
   | ACC_INDEPENDENT                                        { mkleaf $startpos $endpos (L.mkaccc AccClause.Independent) }
   | ACC_LINK LPAREN l=acc_list RPAREN                      { mknode $startpos $endpos (L.mkaccc AccClause.Link) l }
   | ACC_NOHOST                                             { mkleaf $startpos $endpos (L.mkaccc AccClause.Nohost) }
   | ACC_NUM_GANGS LPAREN el=clist(expr) RPAREN             { mknode $startpos $endpos (L.mkaccc AccClause.Num_gangs) el }
   | ACC_NUM_WORKERS LPAREN el=clist(expr) RPAREN           { mknode $startpos $endpos (L.mkaccc AccClause.Num_workers) el }
   | ACC_PCOPY LPAREN l=acc_list RPAREN                     { mknode $startpos $endpos (L.mkaccc AccClause.Pcopy) l }
   | ACC_PCOPYIN LPAREN l=acc_list RPAREN                   { mknode $startpos $endpos (L.mkaccc AccClause.Pcopyin) l }
   | ACC_PCOPYOUT LPAREN l=acc_list RPAREN                  { mknode $startpos $endpos (L.mkaccc AccClause.Pcopyout) l }
   | ACC_PCREATE LPAREN l=acc_list RPAREN                   { mknode $startpos $endpos (L.mkaccc AccClause.Pcreate) l }
   | ACC_PRESENT LPAREN l=acc_list RPAREN                   { mknode $startpos $endpos (L.mkaccc AccClause.Present) l }
   | ACC_PRESENT_OR_COPY LPAREN l=acc_list RPAREN           { mknode $startpos $endpos (L.mkaccc AccClause.Present_or_copy) l }
   | ACC_PRESENT_OR_COPYIN LPAREN l=acc_list RPAREN         { mknode $startpos $endpos (L.mkaccc AccClause.Present_or_copyin) l }
   | ACC_PRESENT_OR_COPYOUT LPAREN l=acc_list RPAREN        { mknode $startpos $endpos (L.mkaccc AccClause.Present_or_copyout) l }
   | ACC_PRESENT_OR_CREATE LPAREN l=acc_list RPAREN         { mknode $startpos $endpos (L.mkaccc AccClause.Present_or_create) l }
   | ACC_PRIVATE LPAREN l=acc_list RPAREN                   { mknode $startpos $endpos (L.mkaccc AccClause.Private) l }
   | ACC_REDUCTION LPAREN op=acc_op COLON l=acc_list RPAREN { mknode $startpos $endpos (L.mkaccc AccClause.Reduction) (op::l) }
   | ACC_SELF LPAREN l=acc_list RPAREN                      { mknode $startpos $endpos (L.mkaccc AccClause.Self) l }
   | ACC_SEQ                                                { mkleaf $startpos $endpos (L.mkaccc AccClause.Seq) }
   | ACC_TILE LPAREN el=clist(expr) RPAREN                  { mknode $startpos $endpos (L.mkaccc AccClause.Tile) el }
   | ACC_USE_DEVICE LPAREN l=acc_list RPAREN                { mknode $startpos $endpos (L.mkaccc AccClause.Use_device) l }
   | ACC_VECTOR                                             { mkleaf $startpos $endpos (L.mkaccc AccClause.Vector) }
   | ACC_VECTOR LPAREN e=expr RPAREN                        { mknode $startpos $endpos (L.mkaccc AccClause.Vector) [e] }
   | ACC_VECTOR_LENGTH LPAREN el=clist(expr) RPAREN         { mknode $startpos $endpos (L.mkaccc AccClause.Vector_length) el }
   | ACC_WAIT LPAREN el=clist(expr) RPAREN                  { mknode $startpos $endpos (L.mkaccc AccClause.Wait) el }
   | ACC_WORKER                                             { mkleaf $startpos $endpos (L.mkaccc AccClause.Worker) }
   | ACC_WORKER LPAREN e=expr RPAREN                        { mknode $startpos $endpos (L.mkaccc AccClause.Worker) [e] }
;

acc_iop:
   | PLUS         { IntrinsicOperator.Add }
   | STAR         { IntrinsicOperator.Mult }
   | D_AND        { IntrinsicOperator.AND }
   | D_OR         { IntrinsicOperator.OR }
   | D_EQV        { IntrinsicOperator.EQV }
   | D_NEQV       { IntrinsicOperator.NEQV }
;

acc_op:
   | op=acc_iop   { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
   | d=DEFINED_OP { mkleaf $startpos $endpos (L.DefinedOperator (DefinedOperator.mkb d)) }


acc_list:
   | l=clist(acc_list_item) { l }
;

acc_list_item:
   | v=var_or_ref
       { 
         disambiguate_variable v;
         v
       }
;

dtype_args:
   | nl=clist0(name) { nl }
;

acc:
   | o=_acc EOL { o }
   | error
       { 
         let d = mkleaf $startpos $endpos (L.mkacc AccDirective.ERROR) in
         mknode $startpos $endpos L.ACC [d]
       }
;

_acc:
   | d=acc_directive { mknode $startpos $endpos L.ACC [d] }
;


(* * * * * *)

omp_directive:
   | OMP_PARALLEL cs=coplist0(omp_clause)                      { mknode $startpos $endpos (L.mkomp OmpDirective.Parallel) cs }
   | OMP_END_PARALLEL                                          { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndParallel) }
   | OMP_DO cs=coplist0(omp_clause)                            { mknode $startpos $endpos (L.mkomp (OmpDirective.Do false)) cs }
   | OMP_END_DO w_opt=ioption(omp_nowait)                      { mknode $startpos $endpos (L.mkomp (OmpDirective.EndDo false)) (opt_to_list w_opt) }
   | OMP_SECTIONS cs=coplist0(omp_clause)                      { mknode $startpos $endpos (L.mkomp OmpDirective.Sections) cs }
   | OMP_SECTION                                               { mkleaf $startpos $endpos (L.mkomp OmpDirective.Section) }
   | OMP_END_SECTIONS w_opt=ioption(omp_nowait)                { mknode $startpos $endpos (L.mkomp OmpDirective.EndSections) (opt_to_list w_opt) }
   | OMP_SINGLE cs=coplist0(omp_clause)                        { mknode $startpos $endpos (L.mkomp OmpDirective.Single) cs }
   | OMP_END_SINGLE cs=coplist0(omp_clause)                    { mknode $startpos $endpos (L.mkomp OmpDirective.EndSingle) cs }
   | OMP_WORKSHARE                                             { mkleaf $startpos $endpos (L.mkomp OmpDirective.Workshare) }
   | OMP_END_WORKSHARE w_opt=ioption(omp_nowait)               { mknode $startpos $endpos (L.mkomp OmpDirective.EndWorkshare) (opt_to_list w_opt) }
   | OMP_PARALLEL_DO cs=coplist0(omp_clause)                   { mknode $startpos $endpos (L.mkomp (OmpDirective.ParallelDo false)) cs }
   | OMP_END_PARALLEL_DO                                       { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndParallelDo false)) }
   | OMP_PARALLEL_SECTIONS cs=coplist0(omp_clause)             { mknode $startpos $endpos (L.mkomp OmpDirective.ParallelSections) cs }
   | OMP_END_PARALLEL_SECTIONS                                 { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndParallelSections) }
   | OMP_PARALLEL_WORKSHARE cs=coplist0(omp_clause)            { mknode $startpos $endpos (L.mkomp OmpDirective.ParallelWorkshare) cs }
   | OMP_END_PARALLEL_WORKSHARE                                { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndParallelWorkshare) }
   | OMP_TASK cs=coplist0(omp_clause)                          { mknode $startpos $endpos (L.mkomp OmpDirective.Task) cs }
   | OMP_END_TASK                                              { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndTask) }
   | OMP_TASKYIELD                                             { mkleaf $startpos $endpos (L.mkomp OmpDirective.Taskyield) }
   | OMP_MASTER                                                { mkleaf $startpos $endpos (L.mkomp OmpDirective.Master) }
   | OMP_END_MASTER                                            { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndMaster) }
   | OMP_CRITICAL n_opt=ioption(lparen__name__rparen)          { mkleaf $startpos $endpos (L.mkomp (OmpDirective.Critical n_opt)) }
   | OMP_END_CRITICAL n_opt=ioption(lparen__name__rparen)      { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndCritical n_opt)) }
   | OMP_BARRIER                                               { mkleaf $startpos $endpos (L.mkomp OmpDirective.Barrier) }
   | OMP_TASKWAIT                                              { mkleaf $startpos $endpos (L.mkomp OmpDirective.Taskwait) }
   | OMP_ATOMIC a_opt=ioption(omp_atomic) OMP_SEQ_CST          { mkleaf $startpos $endpos (L.mkomp (OmpDirective.Atomic(a_opt, true))) }
   | OMP_ATOMIC a_opt=ioption(omp_atomic)                      { mkleaf $startpos $endpos (L.mkomp (OmpDirective.Atomic(a_opt, false))) }
   | OMP_END_ATOMIC                                            { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndAtomic) }
   | OMP_FLUSH l_opt=ioption(lparen__omp_list__rparen)         { mknode $startpos $endpos (L.mkomp OmpDirective.Flush) (list_opt_to_list l_opt) }
   | OMP_ORDERED                                               { mkleaf $startpos $endpos (L.mkomp OmpDirective.Ordered) }
   | OMP_END_ORDERED                                           { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndOrdered) }
   | OMP_THREADPRIVATE l_opt=ioption(lparen__omp_list__rparen) { mknode $startpos $endpos (L.mkomp OmpDirective.Threadprivate) (list_opt_to_list l_opt) }
   (* 4.0 *)
   | OMP_SIMD cs=coplist0(omp_clause)                              { mknode $startpos $endpos (L.mkomp OmpDirective.Simd) cs }
   | OMP_END_SIMD                                                  { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndSimd) }
   | OMP_DECLARE_SIMD LPAREN n=name RPAREN cs=coplist0(omp_clause) { mknode $startpos $endpos (L.mkomp (OmpDirective.DeclareSimd n#get_name)) cs }
   | OMP_DO_SIMD cs=coplist0(omp_clause)                           { mknode $startpos $endpos (L.mkomp (OmpDirective.Do true)) cs }
   | OMP_END_DO_SIMD w_opt=ioption(omp_nowait)                     { mknode $startpos $endpos (L.mkomp (OmpDirective.EndDo true)) (opt_to_list w_opt) }
   | OMP_TARGET_DATA cs=coplist0(omp_clause)                       { mknode $startpos $endpos (L.mkomp OmpDirective.TargetData) cs }
   | OMP_END_TARGET_DATA                                           { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndTargetData) }
   | OMP_TARGET      cs=coplist0(omp_clause)                       { mknode $startpos $endpos (L.mkomp OmpDirective.TargetData) cs }
   | OMP_END_TARGET                                                { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndTarget) }
   | OMP_TARGET_UPDATE cs=coplist(omp_clause)                      { mknode $startpos $endpos (L.mkomp OmpDirective.TargetUpdate) cs }
   | OMP_DECLARE_TARGET l_opt=ioption(lparen__omp_list__rparen)    { mknode $startpos $endpos (L.mkomp OmpDirective.DeclareTarget) (list_opt_to_list l_opt) }
   | OMP_TEAMS cs=coplist0(omp_clause)                             { mknode $startpos $endpos (L.mkomp OmpDirective.Teams) cs }
   | OMP_END_TEAMS                                                 { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndTeams) }
   | OMP_DISTRIBUTE_SIMD cs=coplist0(omp_clause)                   { mknode $startpos $endpos (L.mkomp (OmpDirective.Distribute true)) cs }
   | OMP_END_DISTRIBUTE_SIMD                                       { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndDistribute true)) }
   | OMP_DISTRIBUTE cs=coplist0(omp_clause)                        { mknode $startpos $endpos (L.mkomp (OmpDirective.Distribute false)) cs }
   | OMP_END_DISTRIBUTE                                            { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndDistribute false)) }
   | OMP_DISTRIBUTE_PARALLEL_DO_SIMD cs=coplist0(omp_clause)       { mknode $startpos $endpos (L.mkomp (OmpDirective.DistributeParallelDo true)) cs }
   | OMP_END_DISTRIBUTE_PARALLEL_DO_SIMD                           { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndDistributeParallelDo true)) }
   | OMP_DISTRIBUTE_PARALLEL_DO cs=coplist0(omp_clause)            { mknode $startpos $endpos (L.mkomp (OmpDirective.DistributeParallelDo false)) cs }
   | OMP_END_DISTRIBUTE_PARALLEL_DO                                { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndDistributeParallelDo false)) }
   | OMP_PARALLEL_DO_SIMD cs=coplist0(omp_clause)                  { mknode $startpos $endpos (L.mkomp (OmpDirective.ParallelDo true)) cs }
   | OMP_END_PARALLEL_DO_SIMD                                      { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndParallelDo true)) }
   | OMP_TARGET_TEAMS cs=coplist0(omp_clause)                      { mknode $startpos $endpos (L.mkomp OmpDirective.TargetTeams) cs }
   | OMP_END_TARGET_TEAMS                                          { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndTargetTeams) }
   | OMP_TEAMS_DISTRIBUTE_SIMD cs=coplist0(omp_clause)             { mknode $startpos $endpos (L.mkomp (OmpDirective.TeamsDistribute true)) cs }
   | OMP_END_TEAMS_DISTRIBUTE_SIMD                                 { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTeamsDistribute true)) }
   | OMP_TEAMS_DISTRIBUTE cs=coplist0(omp_clause)                  { mknode $startpos $endpos (L.mkomp (OmpDirective.TeamsDistribute false)) cs }
   | OMP_END_TEAMS_DISTRIBUTE                                      { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTeamsDistribute false)) }
   | OMP_TARGET_TEAMS_DISTRIBUTE_SIMD cs=coplist0(omp_clause)      { mknode $startpos $endpos (L.mkomp (OmpDirective.TargetTeamsDistribute true)) cs }
   | OMP_END_TARGET_TEAMS_DISTRIBUTE_SIMD                          { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTargetTeamsDistribute true)) }
   | OMP_TARGET_TEAMS_DISTRIBUTE cs=coplist0(omp_clause)           { mknode $startpos $endpos (L.mkomp (OmpDirective.TargetTeamsDistribute false)) cs }
   | OMP_END_TARGET_TEAMS_DISTRIBUTE                               { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTargetTeamsDistribute false)) }
   | OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD cs=coplist0(omp_clause) { mknode $startpos $endpos (L.mkomp (OmpDirective.TeamsDistributeParallelDo true)) cs }
   | OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD                     { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTeamsDistributeParallelDo true)) }
   | OMP_TEAMS_DISTRIBUTE_PARALLEL_DO cs=coplist0(omp_clause)      { mknode $startpos $endpos (L.mkomp (OmpDirective.TeamsDistributeParallelDo false)) cs }
   | OMP_END_TEAMS_DISTRIBUTE_PARALLEL_DO                          { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTeamsDistributeParallelDo false)) }
   | OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD cs=coplist0(omp_clause) { mknode $startpos $endpos (L.mkomp (OmpDirective.TargetTeamsDistributeParallelDo true)) cs }
   | OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD                     { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTargetTeamsDistributeParallelDo true)) }
   | OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO cs=coplist0(omp_clause)      { mknode $startpos $endpos (L.mkomp (OmpDirective.TargetTeamsDistributeParallelDo false)) cs }
   | OMP_END_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO                          { mkleaf $startpos $endpos (L.mkomp (OmpDirective.EndTargetTeamsDistributeParallelDo false)) }
   | OMP_TASKGROUP                                                 { mkleaf $startpos $endpos (L.mkomp OmpDirective.Taskgroup) }
   | OMP_END_TASKGROUP                                             { mkleaf $startpos $endpos (L.mkomp OmpDirective.EndTaskgroup) }
   | OMP_CANCEL t=omp_construct_type i_opt=ioption(omp_if_clause)  { mknode $startpos $endpos (L.mkomp (OmpDirective.Cancel t)) (opt_to_list i_opt) }
   | OMP_CANCELLATION_POINT t=omp_construct_type                   { mkleaf $startpos $endpos (L.mkomp (OmpDirective.CancellationPoint t)) }

   | OMP_DECLARE_REDUCTION LPAREN o=omp_reduction_op COLON ts=clist(omp_type_spec) COLON c=omp_combiner RPAREN i_opt=ioption(omp_initializer_clause) 
       { mknode $startpos $endpos (L.mkomp OmpDirective.DeclareReduction) (o::(ts@[c]@(opt_to_list i_opt))) }


;

%inline
omp_type_spec:
   | t=decl_type_spec { t }
   | n=name           { n }
;

omp_combiner:
   | v=var_or_ref EQ e=expr 
       { 
         disambiguate_variable v;
         mkstmtnode $startpos $endpos Stmt.AssignmentStmt [v; e] 
       }
   | v=var_or_ref { disambiguate_primary v; v }
;

omp_initializer_clause:
   | OMP_INITIALIZER LPAREN c=omp_combiner RPAREN { mknode $startpos $endpos (L.mkompc OmpClause.Initializer) [c] }
;

omp_construct_type:
   | OMP_PARALLEL  { OmpDirective.C_parallel }
   | OMP_SECTIONS  { OmpDirective.C_sections }
   | OMP_DO        { OmpDirective.C_do }
   | OMP_TASKGROUP { OmpDirective.C_taskgroup }
;

omp_atomic:
   | OMP_READ    { OmpDirective.Read }
   | OMP_WRITE   { OmpDirective.Write }
   | OMP_CAPTURE { OmpDirective.Capture }
   | OMP_UPDATE  { OmpDirective.Update }
;

lparen__name__rparen:
   | LPAREN n=name RPAREN { n#get_name }
;

lparen__omp_list__rparen:
   | LPAREN l=omp_list RPAREN { l }
;

omp_nowait:
   | OMP_NOWAIT { mkleaf $startpos $endpos (L.mkompc OmpClause.Nowait) }
;

omp_if_clause:
   | OMP_IF LPAREN e=expr RPAREN                                      { mknode $startpos $endpos (L.mkompc OmpClause.If) [e] }
;

omp_clause:
   | i=omp_if_clause                                                  { i }
   | OMP_NUM_THREADS LPAREN e=expr RPAREN                             { mknode $startpos $endpos (L.mkompc OmpClause.Num_threads) [e] }
   | OMP_DEFAULT LPAREN a=omp_attr RPAREN                             { mkleaf $startpos $endpos (L.mkompc (OmpClause.Default a)) }
   | OMP_PRIVATE LPAREN l=omp_list RPAREN                             { mknode $startpos $endpos (L.mkompc (OmpClause.DataSharingAttr OmpClause.Private)) l }
   | OMP_FIRSTPRIVATE LPAREN l=omp_list RPAREN                        { mknode $startpos $endpos (L.mkompc (OmpClause.DataSharingAttr OmpClause.Firstprivate)) l }
   | OMP_LASTPRIVATE LPAREN l=omp_list RPAREN                         { mknode $startpos $endpos (L.mkompc OmpClause.Lastprivate) l }
   | OMP_SHARED LPAREN l=omp_list RPAREN                              { mknode $startpos $endpos (L.mkompc (OmpClause.DataSharingAttr OmpClause.Shared)) l }
   | OMP_COPYIN LPAREN l=omp_list RPAREN                              { mknode $startpos $endpos (L.mkompc OmpClause.Copyin) l }
   | OMP_REDUCTION LPAREN o=omp_reduction_op COLON l=omp_list RPAREN  { mknode $startpos $endpos (L.mkompc OmpClause.Reduction) (o::l) }
   | OMP_SCHEDULE LPAREN k=omp_kind e_opt=ioption(comma__expr) RPAREN { mknode $startpos $endpos (L.mkompc (OmpClause.Schedule k)) (opt_to_list e_opt) }
   | OMP_COLLAPSE LPAREN e=expr RPAREN                                { mknode $startpos $endpos (L.mkompc OmpClause.Collapse) [e] }
   | OMP_ORDERED                                                      { mkleaf $startpos $endpos (L.mkompc OmpClause.Ordered) }
   | OMP_COPYPRIVATE LPAREN l=omp_list RPAREN                         { mknode $startpos $endpos (L.mkompc OmpClause.Copyprivate) l }
   | w=omp_nowait                                                     { w }
   | OMP_FINAL LPAREN e=expr RPAREN                                   { mknode $startpos $endpos (L.mkompc OmpClause.Final) [e] }
   | OMP_UNTIED                                                       { mkleaf $startpos $endpos (L.mkompc OmpClause.Untied) }
   | OMP_MERGEABLE                                                    { mkleaf $startpos $endpos (L.mkompc OmpClause.Mergeable) }
   (* 4.0 *)
   | OMP_LINEAR LPAREN l=clist(expr) COLON e=expr RPAREN              { mknode $startpos $endpos (L.mkompc (OmpClause.Linear true)) (l@[e]) }
   | OMP_LINEAR LPAREN l=clist(expr)              RPAREN              { mknode $startpos $endpos (L.mkompc (OmpClause.Linear false)) l }
   | OMP_MAP LPAREN t=omp_map_type COLON l=omp_va_sec_list RPAREN     { mknode $startpos $endpos (L.mkompc (OmpClause.Map (Some t))) l }
   | OMP_MAP LPAREN                      l=omp_va_sec_list RPAREN     { mknode $startpos $endpos (L.mkompc (OmpClause.Map None)) l }
   | OMP_SAFELEN LPAREN e=expr RPAREN                                 { mknode $startpos $endpos (L.mkompc OmpClause.Safelen) [e] }
   | OMP_SIMDLEN LPAREN e=expr RPAREN                                 { mknode $startpos $endpos (L.mkompc OmpClause.Simdlen) [e] }
   | OMP_ALIGNED LPAREN l=clist(expr) COLON e=expr RPAREN             { mknode $startpos $endpos (L.mkompc (OmpClause.Aligned true)) (l@[e]) }
   | OMP_ALIGNED LPAREN l=clist(expr)              RPAREN             { mknode $startpos $endpos (L.mkompc (OmpClause.Aligned false)) l }
   | OMP_UNIFORM LPAREN l=clist(expr) RPAREN                          { mknode $startpos $endpos (L.mkompc OmpClause.Uniform) l }
   | OMP_INBRANCH                                                     { mkleaf $startpos $endpos (L.mkompc OmpClause.Inbranch) }
   | OMP_NOTINBRANCH                                                  { mkleaf $startpos $endpos (L.mkompc OmpClause.Notinbranch) }
   | OMP_PROC_BIND LPAREN p=omp_policy RPAREN                         { mkleaf $startpos $endpos (L.mkompc (OmpClause.Proc_bind p)) }
   | OMP_DEVICE LPAREN e=expr RPAREN                                  { mknode $startpos $endpos (L.mkompc OmpClause.Device) [e] }
   | OMP_NUM_TEAMS LPAREN e=expr RPAREN                               { mknode $startpos $endpos (L.mkompc OmpClause.Num_teams) [e] }
   | OMP_THREAD_LIMIT LPAREN e=expr RPAREN                            { mknode $startpos $endpos (L.mkompc OmpClause.Thread_limit) [e] }
   | OMP_DIST_SCHEDULE LPAREN k=omp_kind e_opt=ioption(comma__expr) RPAREN  { mknode $startpos $endpos (L.mkompc (OmpClause.Dist_schedule k)) (opt_to_list e_opt) }
   | OMP_DEPEND LPAREN t=omp_dependence_type COLON l=omp_va_sec_list RPAREN { mknode $startpos $endpos (L.mkompc (OmpClause.Depend t)) l }
   | error { mkleaf $startpos $endpos (L.mkompc OmpClause.ERROR) }
;

%inline
omp_va_sec_list:
   | l=clist(substring_range_OR_section_subscript_OR_actual_arg_spec) 
       { 
         List.iter disambiguate_array_element l;
         l
       }

omp_dependence_type:
   | OMP_IN    { OmpClause.In }
   | OMP_OUT   { OmpClause.Out }
   | OMP_INOUT { OmpClause.Inout }
;

omp_policy:
   | OMP_MASTER { OmpClause.Master }
   | OMP_CLOSE  { OmpClause.Close }
   | OMP_SPREAD { OmpClause.Spread }
;

omp_map_type:
   | OMP_ALLOC  { OmpClause.Alloc }
   | OMP_TO     { OmpClause.To }
   | OMP_FROM   { OmpClause.From }
   | OMP_TOFROM { OmpClause.Tofrom }
;

omp_list:
   | ns=clist(omp_list_item) { ns }
;

%inline
omp_list_item:
   | n=name             { mkleaf $startpos $endpos (L.Name n#get_name) }
   | SLASH n=name SLASH { mkleaf $startpos $endpos (L.CommonBlockName n#get_name) }
;

omp_kind:
   | OMP_STATIC  { OmpClause.Static }
   | OMP_DYNAMIC { OmpClause.Dynamic }
   | OMP_GUIDED  { OmpClause.Guided }
   | OMP_AUTO    { OmpClause.Auto }
   | OMP_RUNTIME { OmpClause.Runtime }
;

omp_attr:
   | OMP_PRIVATE      { OmpClause.Private }
   | OMP_FIRSTPRIVATE { OmpClause.Firstprivate }
   | OMP_SHARED       { OmpClause.Shared }
   | OMP_NONE         { OmpClause.None_ }
;

omp_reduction_op:
   | n=name               { mkleaf $startpos $endpos (L.IntrinsicProcedureName n#get_name) }
   | op=mult_op           { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
   | op=add_op            { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
   | op=and_op            { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
   | op=or_op             { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
   | op=equiv_op          { mkleaf $startpos $endpos (L.IntrinsicOperator op) }
   | op=DEFINED_OP        { mkleaf $startpos $endpos (L.DefinedOperator (DefinedOperator.mk op)) }
;

omp_token:
   | OMP_END { }
;

omp:
   | o=_omp EOL { o }
   | error 
       { 
         let d = mkleaf $startpos $endpos (L.mkomp OmpDirective.ERROR) in
         mknode $startpos $endpos L.OMP [d]
       }
;

_omp:
   | d=omp_directive { mknode $startpos $endpos L.OMP [d] }
;

(* * * * * *)

linda_call:
   | c=var_or_ref EOL { disambiguate_linda_call c; c }
;

linda_formal:
   | QUESTION af=linda_anonymous_formal l_opt=ioption(linda_len) 
       { 
         mknode $startpos $endpos L.LindaFormal (af::(opt_to_list l_opt))
       }
   | QUESTION cbn=linda_common_block_name  { mknode $startpos $endpos L.LindaFormal [cbn] }
   | QUESTION v=var_or_ref l_opt=ioption(linda_len) 
       { 
         disambiguate_variable v;
         mknode $startpos $endpos L.LindaFormal (v::(opt_to_list l_opt))
       }
;

linda_anonymous_formal:
   | d=_linda_anonymous_formal RPAREN { mknode $startpos $endpos L.LindaTypeof [d] }

_linda_anonymous_formal:
   | linda_anonymous_formal_head d=linda_data_type { env#exit_typeof_context; d }
;

linda_anonymous_formal_head:
   | linda_typeof LPAREN { }
;

linda_typeof:
   | LINDA_TYPEOF { env#enter_typeof_context }
;

%inline
linda_data_type:
   | t=decl_type_spec          { t }
   | c=linda_common_block_name { c }
;

%inline
linda_len:
   | COLON e_opt=ioption(expr) { mknode $startpos $endpos L.LindaLength (opt_to_list e_opt) }
;

%inline
linda_common_block_name:
   | SLASH n=name SLASH { mkleaf $startpos $endpos (L.CommonBlockName n#get_name) }
;


(* * * * * *)


special_token:
   | COMPOSITE_IDENTIFIER { }
   | CONTINUED_IDENTIFIER { }
   | DOUBLE               { }
   | PRECISION            { }
   | LABEL                { }
   | MARKER               { }
   | PP_MARKER            { }
   | NOTHING              { }
   | RAW                  { }
;

special_symbol:
   | AMP    { }
   | EXCLAM { }
;

(* * * * * *)

directive:
   | d=OCL { finalize_directive d }
   | d=OMP { finalize_directive d }
   | d=XLF { finalize_directive d }
   | d=DEC { finalize_directive d }
   | d=ACC { finalize_directive d }
;

program:
   | pus_=program_unit+ 
       { 
         let pus = 
           List.flatten
             (List.map 
                (fun pu -> 
                  match pu#label with
                  | L.Fragment -> finalize_fragment C.Tprogram_unit pu
                  | _ -> [pu]
                ) pus_
             )
         in
         (* mknode $startpos $endpos L.Program pus *)
         let lloc = (* due to END_FRAGMENT *)
           if pus = [] then
             LLoc.dummy
           else
             lloc_of_nodes pus 
         in 
         new node ~lloc ~children:pus L.Program
       }
   | EOL { mknode $startpos $endpos L.Program [] }
;

fragment:
   | se=specification_part__execution_part sp_opt=ioption(subprogram_part) END_FRAGMENT
       { 
         let specs, execs = Ast.spec_opt_exec_opt_to_children_pair se in
         let spl = opt_to_list sp_opt in
         mknode $startpos $endpos L.Fragment (specs @ execs @ spl)
       }
;

program_unit:
   | m=main_program          { at_EOPU(); m }
   | f=function_subprogram   { at_EOPU(); f }
   | s=subroutine_subprogram { at_EOPU(); s }
   | m=module_               { at_EOPU(); m }
   | m=submodule             { at_EOPU(); m }
   | b=block_data            { at_EOPU(); b }
   | sp=PROGRAM_UNIT         { at_EOPU(); let s, p = sp in check_error p; p }
   | f=fragment (* incomplete code fragment *)
       { 
         let in_main_scope =
           match env#current_frame#scope with
           | Pinfo.Name.ScopingUnit.MainProgram _ -> true
           | _ -> false
         in
         if f#children <> [] && not in_main_scope then 
           at_EOPU()
         else 
           env#set_BOPU;
         f 
       } 
(*
   | sp=SPEC_PART_CONSTRUCT  { let s, p = sp in check_error p; p }
   | p=pp_directive { set_pp_context C.Tprogram_unit p; p }
   | d=directive    { d }
*)
;

main_program:
   | m=_main_program e=end_program_stmt { reloc $startpos $endpos m; m#add_children_r [e]; m }
;

_main_program:
   | m0=main_program0 sp_opt=ioption(subprogram_part)
       { 
         (*end_scope();*)
         let n_opt, pl, se = m0 in
         let spl = opt_to_list sp_opt in
         List.iter disambiguate_internal_subprogram spl;
         mknode $symbolstartpos $endpos (L.ProgramUnit (ProgramUnit.MainProgram n_opt)) (pl @ se @ spl)
       } 
;

%inline
main_program0:
   | p_opt=ioption(program_stmt) se=specification_part__execution_part 
       { 
(*
         context_stack#pop; (* spec__exec *)
*)
         let n_opt, pl =
           match p_opt with
           | Some p -> p#get_name_opt, [p]
           | _ -> None, []
         in
         n_opt, pl, (Ast.spec_opt_exec_opt_to_list se)
       }
;


%inline
specification_part__execution_part:
   |                                                              { None, None }
   | cs=spec_part_construct_OR_exec_part_constructs 
       { 
         let specs, execs = finalize_spec_exec cs in
         let sp_nd_opt = 
           if specs = [] then
             None
           else
             Some (new node ~lloc:(Ast.lloc_of_nodes specs) ~children:specs L.SpecificationPart)
         in
         let ep_nd_opt = 
           if execs = [] then
             None
           else begin
             let ep_nd = new node ~lloc:(Ast.lloc_of_nodes execs) ~children:execs L.ExecutionPart in
             (*if not env#partial_parsing_flag then*)
               elaborate_execution_part ep_nd;
             Some ep_nd
           end
         in
         sp_nd_opt, ep_nd_opt
       }
;

spec_part_construct_OR_exec_part_constructs:
   |                                               x=specification_part_construct_OR_execution_part_construct 
       { 
         if is_execution_part_construct x && not (is_specification_part_construct x) then
           [], [], [x]
         else if L.is_directive x#label then
           [], [x], []
         else
           [x], [], []
       }
   | xs=spec_part_construct_OR_exec_part_constructs x=specification_part_construct_OR_execution_part_construct 
       { 
         let spec, dctv, exec = xs in
         if exec = [] then
           if is_execution_part_construct x && not (is_specification_part_construct x) then
             spec, [], (x::dctv)
           else if L.is_directive x#label then
             spec, (x::dctv), []
           else
             (x::(dctv@spec)), [], []
         else
           spec, dctv, (x::exec)
       }
;

specification_part_construct_OR_execution_part_construct:
   | s=specification_part_construct { s }
   | e=execution_part_construct     { e }

   | a=assignment_stmt              { a } (* Occasionally, this can occur in specification part *)
   | p=pointer_assignment_stmt      { p } (* Occasionally, this can occur in specification part *)

   | d=data_stmt                    { d }
   | f=format_entry_stmt            { f }

   | i=INCLUDE__FILE                { mkleaf $startpos $endpos (L.Include i) }
   | o=OPTIONS__OPTS                { mkleaf $startpos $endpos (L.Options o) }
   | d=directive                    { d }
   | p=pp_directive                 { set_pp_context C.Tspec__exec p; p }
   | l=linda_call                   { l }

(*   | s=pp_macro_stmt                { s }*)
   | sps=STMT                       { let sp, s = sps in check_error s; s }

   | error stmt_end                 { parse_error $startpos $endpos "syntax error" }
;




program_stmt:
   | p=_program_stmt stmt_end { p }
;

_program_stmt:
   | program_stmt_head n=name 
       { 
         env#exit_pu_head_context;
         context_stack#activate_top; 
         let n_str = n#get_name in
         register_main n_str;
         mkstmtleaf $startpos $endpos (Stmt.ProgramStmt n_str) 
       }
;

program_stmt_head:
   | PROGRAM { set_headed() }
;

interface_block:
   | i=interface_stmt is=interface_specification* e=end_interface_stmt 
       { 
         context_stack#pop;
         let gname_opt = i#get_name_opt in
         let nd = mknode $startpos $endpos (L.InterfaceBlock gname_opt) ((i :: is) @ [e]) in
         begin
           match gname_opt with
           | Some gn -> finalize_object_spec gn  nd
           | _ -> ()
         end;
         nd
       }
;

interface_stmt:
   | i=_interface_stmt stmt_end { i}
;

_interface_stmt:
   | interface g_opt=ioption(generic_spec) 
       { 
         let gname_opt =
           match g_opt with
           | Some s -> begin
               try
                 let gname = L.get_generic_name s#label in
                 register_generic gname;
                 Some gname
               with
                 _ -> None
           end
           | None -> None
         in
         context_stack#push (C.interface_spec());
         mkstmtnode $startpos $endpos (Stmt.InterfaceStmt gname_opt) (opt_to_list g_opt)
       }
   | ABSTRACT interface (* F2003 *)
       { 
         f2003();
         context_stack#push (C.interface_spec());
         mkstmtleaf $startpos $endpos Stmt.AbstractInterfaceStmt 
       } 
;

interface:
   | INTERFACE { (* env#enter_interface_context *) }
;

interface_specification:
   | i=interface_body  { i }
   | m=procedure_stmt  { m }
   | si=INTERFACE_SPEC { let s, i = si in check_error i; i }
;

interface_body:
   | f=function_stmt   sp=specification_part e=end_function_stmt   
       { 
         mknode $startpos $endpos L.InterfaceBody ((f :: sp) @ [e])
       }
   | s=subroutine_stmt sp=specification_part e=end_subroutine_stmt 
       { 
         mknode $startpos $endpos L.InterfaceBody ((s :: sp) @ [e])
       }
;

procedure_stmt:
   | m=_procedure_stmt stmt_end { m }
;

_procedure_stmt: (* F2008 (optional '::') *)
   | ioption(MODULE) procedure_kw colon_colon_opt ns=clist(name)
       { 
         env#exit_procedure_context;
         List.iter (fun n -> n#relab (L.ProcName n#get_name)) ns;
         List.iter set_binding_of_subprogram_reference ns;
         mkstmtnode $symbolstartpos $endpos Stmt.ProcedureStmt ns
       }
;

procedure_kw:
   | PROCEDURE 
       { 
         env#enter_procedure_context; 
         if env#in_pu_head_context then
           env#exit_pu_head_context 
       }
;

end_interface_stmt:
   | e=_end_interface_stmt stmt_end { e }
;

_end_interface_stmt:
   | end_interface g_opt=ioption(generic_spec) { mkstmtnode $startpos $endpos Stmt.EndInterfaceStmt (opt_to_list g_opt) }
;

end_interface:
   | END_INTERFACE { (* env#exit_interface_context *) }
;

module_:
   | m=_module_ e=end_module_stmt { reloc $startpos $endpos m; m#add_children_r [e]; m }

_module_:
   | m0=module0_ sp_opt=ioption(subprogram_part) 
       { 
         (*end_scope();*)
         let m = List.hd m0 in
         let spl = opt_to_list sp_opt in
         List.iter disambiguate_module_subprogram spl;
         mknode $startpos $endpos (L.ProgramUnit (ProgramUnit.Module m#get_name)) (m0 @ spl) 
       }
;

module0_:
   | m=module_stmt sp=specification_part 
       { 
(*
         context_stack#pop; (* specification_part *)
*)
         m :: sp
       }
;

module_stmt:
   | m=_module_stmt stmt_end { m }
;

_module_stmt:
   | MODULE n=name 
       { 
         env#exit_pu_head_context;
         let n_str = n#get_name in

         if env#at_BOPU then
           env#clear_BOPU;

         begin
           cancel_main_program_scope();
           let frm = begin_module_scope n_str in
           register_module n_str frm;
           if not env#partial_parsing_flag then
             context_stack#pop; (* cancel context of main_program *)
         end;

         if not env#partial_parsing_flag then
           context_stack#push (C.specification_part()); 

         mkstmtleaf $startpos $endpos (Stmt.ModuleStmt n_str) 
       }
;

%inline
end_module_stmt:
   | e=end_stmt                  { e#relab_stmt (Stmt.EndModuleStmt None); e }
   | e=_end_module_stmt stmt_end { e }
;

_end_module_stmt:
   | end_module_stmt_head n_opt=ioption(name) { mkstmtleaf $startpos $endpos(n_opt) (Stmt.EndModuleStmt (node_opt_to_name_opt n_opt)) }
;

end_module_stmt_head:
   | END_MODULE { mark_EOPU() }
;

submodule:
   | m=submodule_head e=end_submodule_stmt { reloc $startpos $endpos m; m#add_children_r [e]; m }

submodule_head:
   | m0=submodule_head0 sp_opt=ioption(subprogram_part) 
       { 
         (*end_scope();*)
         let m = List.hd m0 in
         let spl = opt_to_list sp_opt in
         List.iter disambiguate_module_subprogram spl;
         mknode $startpos $endpos (L.ProgramUnit (ProgramUnit.Submodule m#get_name)) (m0 @ spl) 
       }
;

submodule_head0:
   | m=submodule_stmt sp=specification_part 
       { 
         m :: sp
       }
;

submodule_stmt:
   | m=_submodule_stmt stmt_end { m }
;

_submodule_stmt:
   | SUBMODULE LPAREN a=name p_opt=ioption(colon__name) RPAREN n=name 
       { 
         env#exit_pu_head_context;
         let a_str = a#get_name in
         let p_str_opt = 
           match p_opt with
           | Some p -> Some p#get_name 
           | None -> None
         in
         let n_str = n#get_name in

         if env#at_BOPU then
           env#clear_BOPU;

         begin
           cancel_main_program_scope();
           let frm = begin_submodule_scope n_str in
           register_submodule n_str frm;
           if not env#partial_parsing_flag then
             context_stack#pop; (* cancel context of main_program *)
         end;

         if not env#partial_parsing_flag then
           context_stack#push (C.specification_part()); 

         mkstmtleaf $startpos $endpos (Stmt.SubmoduleStmt(a_str, p_str_opt, n_str)) 
       }
;

colon__name:
   | COLON n=name { n }
;

%inline
end_submodule_stmt:
   | e=end_stmt                     { e#relab_stmt (Stmt.EndSubmoduleStmt None); e }
   | e=_end_submodule_stmt stmt_end { e }
;

_end_submodule_stmt:
   | end_submodule_stmt_head n_opt=ioption(name) 
       { 
         mkstmtleaf $startpos $endpos(n_opt) (Stmt.EndSubmoduleStmt (node_opt_to_name_opt n_opt)) 
       }
;

end_submodule_stmt_head:
   | END_SUBMODULE { mark_EOPU() }
;

block_data:
   | b0=block_data0 e=end_block_data_stmt 
       { 
         (*end_scope();*)
         let b = List.hd b0 in
         let n_opt = b#get_name_opt in
         mknode $startpos $endpos (L.ProgramUnit (ProgramUnit.BlockData n_opt)) (b0 @ [e])
       }
;

block_data0:
   | b=block_data_stmt sp=specification_part 
       { 
(*
         context_stack#pop; (* specification_part *)
*)
         b :: sp
       }
;

block_data_stmt:
   | b=_block_data_stmt stmt_end { b }
;

_block_data_stmt:
   | BLOCK_DATA n_opt=ioption(name) 
       { 
         env#exit_pu_head_context;

         let n_str_opt = node_opt_to_name_opt n_opt in

         if env#at_BOPU then 
           env#clear_BOPU;
         begin
           cancel_main_program_scope();
           begin
             match n_str_opt with
             | Some n_str -> register_block_data n_str
             | _ -> ()
           end;
           begin_block_data_scope n_str_opt;
           if not env#partial_parsing_flag then
             context_stack#pop; (* cancel context of main_program *)
         end;

         if not env#partial_parsing_flag then
           context_stack#push (C.specification_part());

         mkstmtleaf $startpos $endpos (Stmt.BlockDataStmt n_str_opt) 
       }
;

%inline
end_block_data_stmt:
   | e=end_stmt                      { e#relab_stmt (Stmt.EndBlockDataStmt None); e }
   | e=_end_block_data_stmt stmt_end { e }
;

_end_block_data_stmt:
   | end_block_data_stmt_head n_opt=ioption(name) { mkstmtleaf $startpos $endpos(n_opt) (Stmt.EndBlockDataStmt (node_opt_to_name_opt n_opt)) }
;

end_block_data_stmt_head:
   | END_BLOCK_DATA { mark_EOPU() }
;

(*
specification_part:
   | use_stmt* import_stmt* ioption(implicit_part) declaration_construct* { }
;
*)

%inline
specification_part:
   | se=specification_part__execution_part 
       { 
         let sp_nd_opt, ep_nd_opt = se in
         let sps = 
           match sp_nd_opt with
           | Some sp_nd -> sp_nd#children 
           | None -> []
         in
         match ep_nd_opt with
         | None -> sps
         | Some ep_nd ->
             List.iter
               (fun nd ->
                 if L.is_execution_part_construct nd#label && not (L.is_specification_part_construct nd#label) then
                   parse_warning_loc nd#loc 
                     "specification-part contains execution-part-construct: %s" (L.to_simple_string nd#label)
               ) ep_nd#children;
             sps @ ep_nd#children
       }
;

specification_part_construct:
   | u=use_stmt                                    { u }
   | i=import_stmt                                 { i }
   | i=implicit_part_stmt_OR_declaration_construct { i }
;


implicit_part_stmt_OR_declaration_construct:
   | i=implicit_part_stmt_proper    { i }
   | p=parameter_stmt               { p }
(* | p=parameter_format_entry_stmt  { p } *)
   | d=declaration_construct_proper { d }
(* | p=pointer_assignment_stmt      { p } *)
   | sps=SPEC_PART_CONSTRUCT        { let sp, s = sps in check_error s; s }
;


implicit_part_stmt_proper:
   | i=implicit_stmt  { i }
;

declaration_construct_proper:
   | d=derived_type_def      { d }
   | i=interface_block       { i }
   | t=type_declaration_stmt { t }
   | s=specification_stmt    { s }
(* | s=stmt_function_stmt    { s } contained in assignment_stmt *)
(* | a=assignment_stmt       { a } *)
   | s=structure_decl        { ibm();intel(); s } (* Compaq/Intel and IBM *)
   | r=record_stmt           { ibm();intel(); r } (* Compaq/Intel and IBM *)
   | p=procedure_declaration_stmt { p } (* F2003 *)
   | e=enum_def                   { e } (* F2003 *)
;

enum_def: (* F2003 *)
   | ed=enum_def_stmt etd=enumerator_def_stmt+ e=end_enum_stmt 
       { 
         mknode $startpos $endpos L.EnumDef (ed :: etd @ [e])
       }
;

enum_def_stmt:
   | e=_enum_def_stmt stmt_end { e }
;

_enum_def_stmt:
   | ENUM COMMA BIND LPAREN c=IDENTIFIER RPAREN 
       { 
         f2003();
         if c <> "c" && c <> "C" then
           parse_warning $startpos $endpos "invalid language-binding-spec: %s" c;

         mkstmtleaf $startpos $endpos Stmt.EnumDefStmt 
       }
;

enumerator_def_stmt:
   | e=_enumerator_def_stmt stmt_end { e }
;

_enumerator_def_stmt:
   | ENUMERATOR colon_colon_opt es=clist(enumerator) 
       { 
         mkstmtnode $startpos $endpos Stmt.EnumeratorDefStmt es
       }
;

enumerator:
   | n=name e_opt=ioption(enum_cst)
       { 
         set_attr_of_data_object 
           ~type_spec:I.TypeSpec.Integer 
           (fun attr -> attr#set_parameter) n#get_name;

         mknode $startpos $endpos (L.Enumerator n#get_name) (opt_to_list e_opt) 
       }
;

enum_cst:
   | EQ e=expr { e }
;

end_enum_stmt:
   | e=_end_enum_stmt stmt_end { e }
;

_end_enum_stmt:
   | END_ENUM { mkstmtleaf $startpos $endpos Stmt.EndEnumStmt }
;

procedure_declaration_stmt: (* F2003 *)
   | p=_procedure_declaration_stmt stmt_end { f2003(); p }
;

_procedure_declaration_stmt:
   | procedure_kw LPAREN i_opt=proc_interface_opt RPAREN a_opt=ioption(proc_attr_part) ds=proc_decls 
       { 
         env#exit_procedure_context;
         let aspec_nodes = List.flatten (opt_to_list a_opt) in
         let pi = 
           match i_opt with
           | Some i -> I.ProcInterface.of_label i#label 
           | None -> I.ProcInterface.Unknown
         in
         if I.ProcInterface.is_interface_name pi then
           register_interface_name (I.ProcInterface.get_name pi);

         List.iter (register_pdecl_node aspec_nodes pi) ds;
         let c = (opt_to_list i_opt) @ aspec_nodes @ ds in
         mkstmtnode $startpos $endpos Stmt.ProcedureDeclarationStmt c
       }
;

proc_attr_part:
   |                                COLON_COLON { [] }
   | COMMA ps=clist(proc_attr_spec) COLON_COLON { ps }
;

proc_attr_spec:
   | a=access_spec                      { a#relab (L.access_spec_to_proc_attr_spec a#label); a }
   | l=language_binding_spec            { l#relab (L.ProcAttrSpec ProcAttrSpec.Bind); l }
   | intent LPAREN i=intent_spec RPAREN { env#exit_intent_context; mkleaf $startpos $endpos (L.ProcAttrSpec (ProcAttrSpec.Intent i)) }
   | OPTIONAL                           { mkleaf $startpos $endpos (L.ProcAttrSpec ProcAttrSpec.Optional) }
   | POINTER                            { mkleaf $startpos $endpos (L.ProcAttrSpec ProcAttrSpec.Pointer) }
   | SAVE                               { mkleaf $startpos $endpos (L.ProcAttrSpec ProcAttrSpec.Save) }

   | kw=SIMPLE_ATTR (* PROTECTED *)     (* GNU *)
       { 
         mkleaf $startpos $endpos (L.ProcAttrSpec (ProcAttrSpec.of_keyword kw)) 
       }
;


import_stmt: (* F2003 *)
   | i=_import_stmt stmt_end { f2003(); i }
;

_import_stmt: (* F2003 *)
   | IMPORT                                { mkstmtleaf $startpos $endpos Stmt.ImportStmt }
   | IMPORT colon_colon_opt ns=clist(name) { mkstmtnode $startpos $endpos Stmt.ImportStmt ns }
;


use_stmt:
   | u=_use_stmt stmt_end { u }
;

_use_stmt:
   | USE m_opt_opt=ioption(module_nature_part) n=name ro_opt=ioption(rename_OR_only)
       { 
         let mod_name = n#get_name in
         let only_nds = handle_use mod_name ro_opt in
         let children =
           match m_opt_opt with
           | Some m_opt -> begin
               match m_opt with
               | Some m -> m :: only_nds
               | None -> only_nds
           end
           | None -> only_nds
         in
         mkstmtnode $startpos $endpos (Stmt.UseStmt mod_name) children
       }
;

rename_OR_only:
   | COMMA rs=clist(rename) { rs }
   | COMMA ol=only_list     { [ol] }
;         
        

module_nature_part: (* F2003 *)
   | m_opt=ioption(comma__module_nature) COLON_COLON { f2003(); m_opt }
;

comma__module_nature: (* F2003 *)
   | COMMA m=module_nature { m }
;

module_nature: (* F2003 *)
   | INTRINSIC     { env#exit_name_context; mkleaf $startpos $endpos L.ModuleNatureIntrinsic }
   | NON_INTRINSIC { mkleaf $startpos $endpos L.ModuleNatureNonIntrinsic }
;

only_head:
   | ONLY { context_stack#push (C.onlys()) }
;

%inline
only_list:
   | only_head COLON os=coplist0(only) 
       { 
         env#exit_only_context;
         context_stack#pop;
         mknode $startpos $endpos L.OnlyList os 
       }
;

rename:
   | l=name EQ_GT u=name { mknode $startpos $endpos L.Rename [l; u] }
   | l=operator_defined_op EQ_GT u=operator_defined_op
       { 
         mknode $startpos $endpos L.Rename [l; u]
       }
;

only:
   | g=generic_spec 
       { 
         begin
           match g#label with 
           | L.GenericSpec (GenericSpec.Name n) -> 
               g#relab (L.mkambiguous_generic_spec_or_use_name n) (* maybe a use-name *)
           | _ -> () 
         end;
         (*disambiguate_generic_spec_OR_use_name g;*)
         g
       }
   | r=rename       { r }
(* | n=name         { } contained in generic_spec *)
   | so=ONLY_       { let s, o = so in o }
;

generic_spec:
   | n=name                                   { mkleaf $startpos $endpos (L.GenericSpec (GenericSpec.Name n#get_name)) }
   | OPERATOR   LPAREN op=intrinsic_op RPAREN { mkleaf $startpos $endpos (L.GenericSpec op) }
   | op=operator_defined_op                   
       { 
         begin
           match op#label with
           | L.DefinedOperator o -> 
               op#relab (L.GenericSpec (GenericSpec.DefinedOperator o))
           | _ -> ()
         end;
         op 
       }
   | ASSIGNMENT LPAREN EQ              RPAREN { mkleaf $startpos $endpos (L.GenericSpec GenericSpec.Assignment) }

   | READ  LPAREN i=IDENTIFIER RPAREN 
       { 
         mkleaf $startpos $endpos (L.GenericSpec (GenericSpec.Read (GenericSpec.record_kind_of_keyword i)))
       }
   | WRITE LPAREN i=IDENTIFIER RPAREN 
       { 
         mkleaf $startpos $endpos (L.GenericSpec (GenericSpec.Write (GenericSpec.record_kind_of_keyword i)))
       }
;

operator_defined_op:
   | OPERATOR LPAREN d=DEFINED_OP RPAREN 
       { 
         mkleaf $startpos $endpos (L.DefinedOperator (DefinedOperator.mk d))
       }
;

intrinsic_op:
   | op=power_op   { GenericSpec.IntrinsicOperator op }
   | op=mult_op    { GenericSpec.IntrinsicOperator op }
   | op=add_op     { GenericSpec.IntrinsicOperator op }
   | op=concat_op  { GenericSpec.IntrinsicOperator op }
   | op=rel_op     { GenericSpec.IntrinsicOperator op }
   | op=not_op     { GenericSpec.IntrinsicOperator op }
   | op=and_op     { GenericSpec.IntrinsicOperator op }
   | op=or_op      { GenericSpec.IntrinsicOperator op }
   | op=equiv_op   { GenericSpec.IntrinsicOperator op }
;


parameter_format_entry_stmt:
   | p=parameter_stmt    { p }
   | f=format_entry_stmt { f }
;

format_entry_stmt:
   | f=format_stmt { f }
   | e=entry_stmt  { e }
;

implicit_stmt:
   | i=_implicit_stmt stmt_end { i }
;

_implicit_stmt:
   | implicit_stmt_head ss=clist(implicit_spec) 
       { 
         env#exit_implicit_context; 
         env#add_implicit_spec ss;
         mkstmtnode $startpos $endpos Stmt.ImplicitStmt ss 
       }
   | implicit_stmt_head NONE                    
       { 
         env#exit_implicit_context; 
         env#set_implicit_spec [];
         mkstmtleaf $startpos $endpos Stmt.ImplicitStmt
       }
;

implicit_stmt_head:
   | IMPLICIT { context_stack#activate_top; (* env#enter_implicit_context *) }
;

implicit_spec:
   | t=decl_type_spec LPAREN__IMPLICIT ss=clist(letter_spec) RPAREN { mknode $startpos $endpos L.ImplicitSpec (t :: ss) }
;


character:
   | CHARACTER { env#enter_character_context }
;

type_spec:
   | i=intrinsic_type_spec { i }
   | d=derived_type_spec   { d }
;


intrinsic_type_spec:
   | t=type_spec_no_character { t }
   | c=type_spec_character    { c }
;

decl_type_spec:
   | d=decl_type_spec_no_character { d }
   | c=type_spec_character         { c }
;


type_spec_node:
   | sts=TYPE_SPEC+ 
       { 
         match sts with
         | [st] ->
             let s, t = st in 
             check_error t;
             t

         | _ ->
             let children =
               List.flatten
                 (List.map
                    (fun (spec, nd) ->
                      check_error nd;
                      match nd#label with
                      | L.PpBranch -> nd#children
                      | _ -> [nd]
                    ) sts
                 )
             in
             let lloc = Ast.lloc_of_nodes children in
             let nd = new Ast.node ~lloc ~children L.PpBranch in
             nd
       }
;


%inline
type_spec_character:
   | character c_opt=ioption(char_selector) 
       { 
         env#exit_character_context; 
         mknode $startpos $endpos (L.TypeSpec TypeSpec.Character) (opt_to_list c_opt) 
       }
;

%inline
decl_type_spec_no_character:
   | t=type_spec_no_character { t }
   | d=decl_type_spec_proper  { d }
   | t=type_spec_node         { t }
;

type_spec_no_character:
   | t=type_spec_no_kind k_opt=ioption(kind_selector) { mknode $startpos $endpos (L.TypeSpec t) (opt_to_list k_opt) }
   | DOUBLE_PRECISION                                 { mkleaf $startpos $endpos (L.TypeSpec TypeSpec.DoublePrecision) }
   | DOUBLE_COMPLEX                                   { mkleaf $startpos $endpos (L.TypeSpec TypeSpec.DoubleComplex) }
   | BYTE                                             { mkleaf $startpos $endpos (L.TypeSpec TypeSpec.Byte) }
   | m=PP_MACRO_TYPE_SPEC                             { mkleaf $startpos $endpos (L.TypeSpec (TypeSpec.PpMacroTypeSpec m)) }
   | m=pp_macro_id                                    
       { 
         let body =
           Macro.mk_obj_body ~stat:(Macro.Resolved (Obj.repr (Tokens_.PP_MACRO_TYPE_SPEC m))) "<constrained>"
         in
         env#define_macro m body;
         mkleaf $startpos $endpos (L.TypeSpec (TypeSpec.PpMacroTypeSpec m)) 
       }
;

decl_type_spec_proper:
   | type_kw LPAREN i=intrinsic_type_spec RPAREN
       { 
         env#exit_type_context;
         mknode $startpos $endpos (L.TypeSpec (TypeSpec.Type "")) [i]
       }
   | type_kw LPAREN d=derived_type_spec   RPAREN
       { 
         env#exit_type_context;
         let info = I.make (env#lookup_name d#get_name) in
         mkleaf ~info $startpos $endpos (L.TypeSpec (TypeSpec.Type d#get_name))
       }
   | CLASS   LPAREN d=derived_type_spec   RPAREN { mkleaf $startpos $endpos (L.TypeSpec (TypeSpec.Class d#get_name)) }
   | CLASS   LPAREN STAR                  RPAREN { mkleaf $startpos $endpos (L.TypeSpec (TypeSpec.Class "*")) }
   | type_kw LPAREN STAR                  RPAREN
       { 
         env#exit_type_context;
         mkleaf $startpos $endpos (L.TypeSpec (TypeSpec.Type "*"))
       } (* ISO/IEC TS 29113:2012 *)
;

type_kw:
   | TYPE { env#enter_type_context }
;


%inline
derived_type_spec:
   | n=name { n }
;

%inline
type_spec_no_kind:
   | kw=KINDED_TYPE_SPEC { TypeSpec.of_keyword kw }
(*
   | INTEGER { TypeSpec.Integer }
   | REAL    { TypeSpec.Real }
   | COMPLEX { TypeSpec.Complex }
   | LOGICAL { TypeSpec.Logical }
*)
;

kind_selector:
   | LPAREN ioption(kind__eq) e=expr RPAREN { mknode $startpos $endpos L.KindSelector [e] }
   | STAR e=expr                            { mknode $startpos $endpos L.KindSelector [e] } (* non-standard *)
;

%inline
kind__eq:
   | KIND EQ { }
;

letter_spec:
   | l0=LETTER                 { mkleaf $startpos $endpos (L.LetterSpec(l0, None)) }
   | l0=LETTER MINUS l1=LETTER { mkleaf $startpos $endpos (L.LetterSpec(l0, Some l1)) }
;

format_stmt:
   | f=_format_stmt stmt_end { f }
;

_format_stmt:
   | format_stmt_head f=format_specification { env#exit_format_context; mkstmtnode $startpos $endpos Stmt.FormatStmt [f] }
;

format_stmt_head:
   | FORMAT { (* env#enter_format_context *) }
;

format_specification:
   | LPAREN                         RPAREN { mkleaf $startpos $endpos L.FormatSpecification }
   | LPAREN is=coplist(format_item) RPAREN
       { 
         mknode $startpos $endpos L.FormatSpecification (finalize_format_items is)
       }
;

format_item:
   | (* ioption(r) *) d=data_edit_desc                      { d } (* r is contained in DATA_EDIT_DESC *)
   | c=control_edit_desc                                    { c }
   | c=char_string_edit_desc                                { c }
   | r_opt=ioption(r) LPAREN is=coplist(format_item) RPAREN
       { 
         let i_opt = i_opt_of_r_opt r_opt in
         mknode $symbolstartpos $endpos (L.FormatItem (FormatItem.FormatItemList i_opt)) is
       }
   | DOLLAR (* Compaq/Intel and IBM *)
       { 
         mkleaf $startpos $endpos (L.FormatItem (FormatItem.SpecialEditDesc FormatItem.Dollar)) 
       } 

   | BACKSLASH (* Compaq/Intel *)
       { 
         mkleaf $startpos $endpos (L.FormatItem (FormatItem.SpecialEditDesc FormatItem.Backslash)) 
       } 

   | r_opt=ioption(r) vfe=vfe (* Compaq/Intel *)
       { 
         intel();
         let vb, e, ve = vfe in
         let i_opt = i_opt_of_r_opt r_opt in
         mknode $symbolstartpos $endpos (make_vfe_lab ~i_opt ~tail:ve vb) [e]
       } 

   | r_opt=ioption(r) id=IDENTIFIER DOT vfe=vfe (* Compaq/Intel *)
       { 
         intel();
         let vb, e, ve = vfe in
         let i_opt = i_opt_of_r_opt r_opt in
         let desc = id^"." in
         let lab = L.FormatItem (FormatItem.VariableFormatDesc(i_opt, desc)) in
         mknode $symbolstartpos $endpos lab [e]
       } 

   | r_opt=ioption(r) vfe0=vfe DOT vfe1=vfe (* Compaq/Intel *)
       { 
         intel();
         let vb0, e0, ve0 = vfe0 in
         let vb1, e1, ve1 = vfe1 in
         let i_opt = i_opt_of_r_opt r_opt in
         mknode $symbolstartpos $endpos (make_vfe_lab ~i_opt ~tail:"." vb0) [e0; e1]
       } 

   | r_opt=ioption(r) km=PP_MACRO_ID 
       { 
         let _, m = km in
         let i_opt = i_opt_of_r_opt r_opt in
         mkleaf $symbolstartpos $endpos (L.FormatItem (FormatItem.Macro(i_opt, m)))
       }
;

%inline
vfe:
   | vb=VFE_BEGIN e=expr ve=VFE_END { vb, e, ve } (* Compaq/Intel *)
;

%inline
r:
   | i=int_literal { i }
;

%inline
data_edit_desc:
   | d=DATA_EDIT_DESC 
       { 
         let r_opt, desc = split_data_edit_desc d in
         mkleaf $startpos $endpos (L.FormatItem (FormatItem.DataEditDesc(r_opt, desc)))
       }
;

control_edit_desc:
   | p=position_edit_desc     { mkleaf $startpos $endpos (L.FormatItem (FormatItem.ControlEditDesc p)) }
   | r_opt=ioption(r) SLASH       
       { 
         let i_opt =
           match r_opt with
           | None -> None
           | Some r -> 
               try
                 Some (int_of_string r)
               with
                 _ -> assert false
         in
         mkleaf $symbolstartpos $endpos (L.FormatItem (FormatItem.ControlEditDesc (ControlEditDesc.EndOfRecord i_opt)))
       }
   | COLON                    { mkleaf $startpos $endpos (L.FormatItem (FormatItem.ControlEditDesc ControlEditDesc.Terminate)) }
   | k=KP_DESC                
       { 
         let i_str = Xstring.lstrip ~strs:["+"] (Xstring.rstrip ~strs:["P";"p"] k) in
         let i = 
           try
             int_of_string i_str 
           with
             _ -> assert false
         in
         mkleaf $startpos $endpos (L.FormatItem (FormatItem.ControlEditDesc (ControlEditDesc.ScaleFactor i)))
       }
   | sb=sign_edit_desc_OR_blank_interp_edit_desc { mkleaf $startpos $endpos (L.FormatItem (FormatItem.ControlEditDesc sb)) }
(*
   | s=sign_edit_desc         { }
   | b=blank_interp_edit_desc { }
*)
;

%inline
position_edit_desc:
   | p=POSITION_EDIT_DESC { ControlEditDesc.PositionEditDesc p }
;

sign_edit_desc_OR_blank_interp_edit_desc:
   | n=name (* S SP SS or BN BZ *) 
     { 
       let s = n#get_name in
       let l = String.lowercase_ascii s in
       match l with
       | "bn" | "bz" -> ControlEditDesc.BlankInterpEditDesc s
       | "s" | "sp" | "ss" -> ControlEditDesc.SignEditDesc s
       | _ -> 
           parse_warning $startpos $endpos "unknown control-edit-desc: %s" s;
           ControlEditDesc.Unknown s
     }
;

%inline
hollerith_cst:
   | cp=HOLLERITH { let c, _ = cp in c }
;

%inline
char_string_edit_desc:
   | c=char_literal        { mkleaf $startpos $endpos (L.FormatItem (FormatItem.CharStringEditDesc c)) }
   | c=hollerith_cst       { mkleaf $startpos $endpos (L.FormatItem (FormatItem.CharStringEditDesc c)) }
   | c=PP_MACRO_CONST_CHAR { mkleaf $startpos $endpos (L.FormatItem (FormatItem.CharStringEditDesc c)) }
;

entry_stmt:
   | e=_entry_stmt stmt_end { e }
;

_entry_stmt:
   | ENTRY n=name                                   
       { 
         let n_str = n#get_name in
         let nd = mkstmtleaf $startpos $endpos (Stmt.EntryStmt n_str) in
         begin
           try
             register_entry n_str
           with
             Failure msg -> parse_warning_loc nd#loc "%s" msg
         end;
         nd
       }
   | x=entry_stmt_sub RPAREN s_opt=ioption(suffix) 
       { 
         env#exit_result_context;
         let n, dnds = x in
         let n_str = n#get_name in
         let nd = 
           mkstmtnode $startpos $endpos (Stmt.EntryStmt n_str) (dnds @ (opt_to_list s_opt))
         in
         begin
           try
             register_entry n_str
           with
             Failure msg -> parse_warning_loc nd#loc "%s" msg
         end;
         nd
       }
;

entry_stmt_sub:
   | ENTRY n=name LPAREN                      { env#enter_result_context; n, [] }
   | ENTRY n=name LPAREN das=clist(dummy_arg) 
       { 
         env#enter_result_context;
         let dnds =
           match das with
           | [] -> []
           | _ -> [mknode $startpos(das) $endpos(das) (L.DummyArgList n#get_name) das]
         in
         n, dnds
       }
;

dummy_arg:
   | n=name { mkleaf $startpos $endpos (L.DummyArgName n#get_name) }
   | STAR   { mkleaf $startpos $endpos L.AlternateReturnIndicator }
;

result:
   | RESULT LPAREN n=name RPAREN { mkleaf $startpos $endpos (L.Result n#get_name) }
;


level_1_expr:
   |                     p=primary { p }
   | UPLUS               p=primary { mknode $startpos $endpos (L.IntrinsicOperator IntrinsicOperator.Id) [p] }  (* GNU *)
   | UMINUS              p=primary { mknode $startpos $endpos (L.IntrinsicOperator IntrinsicOperator.Neg) [p] } (* GNU *)
   | op=defined_unary_op p=primary
       { 
         let nd = mknode $startpos $endpos (L.DefinedOperator op) [p] in
         D.set_binding_of_subprogram_reference nd;
         nd
       }
;

%inline
defined_unary_op:
   | d=DEFINED_OP { DefinedOperator.mku d }
;

level_2_expr:
   |                          a=add_operand { a }
   |                op=add_op a=add_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [a] }
   | e=level_2_expr op=add_op a=add_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [e; a] }
   | e0=level_2_expr se1=EXPR 
       { 
         let s, e1 = se1 in
         check_error e1;
         change_top_uop_into_bop e1;
         mknode $startpos $endpos L.SelectiveOp [e0; e1]
       }
;

add_operand:
   |                          m=mult_operand { m }
   | a=add_operand op=mult_op m=mult_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [a; m] }
;

mult_operand:
   | e=level_1_expr                            { e }
   | e=level_1_expr op=power_op m=mult_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [e; m] }
;

%inline
mult_op:
   | STAR  { IntrinsicOperator.Mult }
   | SLASH { IntrinsicOperator.Div }
;

%inline
add_op:
   | PLUS  { IntrinsicOperator.Add }
   | MINUS { IntrinsicOperator.Subt }
;

%inline
power_op:
   | STAR_STAR { IntrinsicOperator.Power }
;

level_3_expr:
   |                              e1=level_2_expr { e1 }
   | e0=level_3_expr op=concat_op e1=level_2_expr { mknode $startpos $endpos (L.IntrinsicOperator op) [e0; e1] }
;

%inline
concat_op:
   | SLASH_SLASH { IntrinsicOperator.Concat }
;

level_4_expr:
   |                           e1=level_3_expr { e1 }
   | e0=level_3_expr op=rel_op e1=level_3_expr { mknode $startpos $endpos (L.IntrinsicOperator op) [e0; e1] }
;

%inline
rel_op:
   | D_EQ     { IntrinsicOperator.EQ }
   | D_NE     { IntrinsicOperator.NE }
   | D_LT     { IntrinsicOperator.LT }
   | D_LE     { IntrinsicOperator.LE }
   | D_GT     { IntrinsicOperator.GT }
   | D_GE     { IntrinsicOperator.GE }
   | EQ_EQ    { IntrinsicOperator.Eq }
   | SLASH_EQ { IntrinsicOperator.Neq }
   | LT       { IntrinsicOperator.Lt }
   | LT_EQ    { IntrinsicOperator.Le }
   | GT       { IntrinsicOperator.Gt }
   | GT_EQ    { IntrinsicOperator.Ge }
;

level_5_expr:
   |                            eq=equiv_operand { eq }
   | e=level_5_expr op=equiv_op eq=equiv_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [e; eq] }
;

equiv_operand:
   |                          o=or_operand { o }
   | e=equiv_operand op=or_op o=or_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [e; o] }
;

or_operand:
   |                        a=and_operand { a }
   | o=or_operand op=and_op a=and_operand { mknode $startpos $endpos (L.IntrinsicOperator op) [o; a] }
;

and_operand:
   | op_opt=ioption(not_op) e=level_4_expr 
       { 
         match op_opt with
         | Some op -> mknode $startpos $endpos (L.IntrinsicOperator op) [e] 
         | _ -> e
       }
;

%inline
not_op:
   | D_NOT { IntrinsicOperator.NOT }
;

%inline
and_op:
   | D_AND { IntrinsicOperator.AND }
;

%inline
or_op:
   | D_OR { IntrinsicOperator.OR }
;

%inline
equiv_op:
   | D_EQV  { IntrinsicOperator.EQV }
   | D_NEQV { IntrinsicOperator.NEQV }
;

expr:
   |                              e1=level_5_expr { e1 }
   | e0=expr op=defined_binary_op e1=level_5_expr
       { 
         let nd = mknode $startpos $endpos (L.DefinedOperator op) [e0; e1] in
         D.set_binding_of_subprogram_reference nd;
         nd
       }
;

%inline
defined_binary_op:
   | d=DEFINED_OP { DefinedOperator.mkb d }
;


primary:
   | c=constant           { mkleaf $startpos $endpos (L.Constant c) }
   | a=array_constructor  { a }
   | t=tuple              { disambiguate_primary_tuple t; t } (* complex-literal or paren-expr *)
   | c=constant_substring { c }
   | v=var_or_ref         { disambiguate_primary v; v } (* variable or structure constructor or function-reference *)
(*   | r=record_field_ref   { r } conflicts with IBM's extension *)
   | m=PP_MACRO_EXPR      { mkleaf $startpos $endpos (L.PpMacroExpr m) }
   | se=EXPR              { let s, e = se in check_error e; e }

   | m=pp_macro_id t=tuple 
       { 
         let mnd = mkleaf $startpos(m) $endpos(m) (L.mkambiguous_desig m) in
         let nd = mknode $startpos $endpos (L.mkambiguous_tuple()) [mnd; t] in
         disambiguate_primary nd;
         nd
       }

   | m=pp_macro_id        
       { 
         let body =
           Macro.mk_obj_body ~stat:(Macro.Resolved (Obj.repr (Tokens_.PP_MACRO_EXPR m))) "<constrained>"
         in
         env#define_macro m body;
         mkleaf $startpos $endpos (L.PpMacroExpr m) 
       }

(* | LPAREN e=expr RPAREN { mknode $startpos $endpos L.ParenExpr [e] } *)
(* | c=complex_literal    { mkleaf $startpos $endpos (L.Constant (Constant.mkcomp c)) } *)
(* | f=function_reference    { } contained in var_or_ref *)
(* | s=structure_constructor { } contained in var_or_ref *)
;

constant:
   | i=int_literal     { Constant.mkint i }
   | r=real_literal    { Constant.mkreal r }
(* | c=complex_literal { Constant.mkcomp c } *)
   | b=boz_literal     { Constant.mkboz b }
   | l=logical_literal { Constant.mklogi l }
   | c=char_literal    { Constant.mkchar c }
   | h=hollerith_cst   { Constant.mkhollerith h }
   | c=pp_macro_const  { Constant.mkppm c }
;

int_literal:
   | i=INT_LITERAL        { i }
;

real_literal:
   | r=REAL_LITERAL    { r }
   | i=INT_LITERAL DOT { i^"." }
   | DOT i=INT_LITERAL { "."^i }
;

boz_literal:
   | b=BOZ_LITERAL { b }
;

logical_literal:
   | l=LOGICAL_LITERAL { l }
;

char_literal:
   | c=CHAR_LITERAL { c }
;


complex_literal:
   | LPAREN r=complex_part COMMA i=complex_part RPAREN { r, i }
;

%inline
complex_part:
   | s_opt=ioption(sign) i=int_literal  { (match s_opt with Some s -> s | _ -> "")^i }
   | s_opt=ioption(sign) r=real_literal { (match s_opt with Some s -> s | _ -> "")^r }
   | n=IDENTIFIER                       { n }
;



%inline
var_or_ref:
   | d=data_ref_         { d }
   | sv=VARIABLE         { let s, v = sv in check_error v; v }
   | v=PP_MACRO_VARIABLE { mkleaf $startpos $endpos (L.PpMacroVariable v) }
;

data_ref_:
   | xs=separated_nonempty_list(component_sep, part_ref_) 
       { 
         let c = List.flatten xs in
         if (List.length xs) > 1 then begin
           let prs, last = Xlist.partition_at_last c in
           if L.is_ambiguous_desig last#label then
             List.iter disambiguate_part_ref_elem c
           else
             List.iter disambiguate_part_ref_elem prs
         end;
         mknode $startpos $endpos (L.mkambiguous_tuple()) c
       }
;

component_sep:
   | PERCENT { }
   | DOT     { } (* IBM *)
;

part_ref_:
   | n=part_name t=tuple LPAREN s=substring_range RPAREN
       { 
         disambiguate_part_ref_elem n; 
         disambiguate_part_ref_elem t; 
         [n; t; s] 
       }
   | n=part_name t_opt=ioption(tuple) i_opt=ioption(image_selector) 
       { 
         n :: (opt_to_list t_opt) @ (opt_to_list i_opt)
       }
;

image_selector:
   | LBRACKET ss=clist(explicit_coshape_spec) RBRACKET 
       { (* disambiguated later *)
         mknode $startpos $endpos L.ImageSelector ss 
       }
;


;

part_name:
   | n=name { mkleaf $startpos $endpos (L.mkambiguous_desig n#get_name) }
;



tuple:
   | t=_tuple RPAREN { reloc $startpos $endpos t; t }
;

_tuple:
   | tuple_head ss=opclist(substring_range_OR_section_subscript_OR_actual_arg_spec)
       { 
         env#exit_name_context;
         let lab =
           if ss = [] then
             L.ActualArgSpecList ""
           else
             L.mkambiguous_tuple()
         in
         mknode $startpos $endpos lab ss 
       }
;

tuple_head:
   | LPAREN { env#enter_name_context }
;



substring_range_OR_section_subscript_OR_actual_arg_spec:
   |           a=actual_arg      { a }
(* |           a=hollerith_cst   { mkleaf $startpos $endpos (L.Constant (Constant.mkchar a)) } Fortran66 *)
   | n=name EQ a=actual_arg      { mknode $startpos $endpos (L.ActualArgSpec (Some n#get_name)) [a] }
   | e0_opt=ioption(expr) COLON_COLON e1=expr 
       { 
         let stride = mknode $startpos(e1) $endpos(e1) L.Stride [e1] in
         let subs = 
           match e0_opt with
           | Some e0 -> [mknode $startpos(e0_opt) $endpos(e0_opt) L.FirstSubscript [e0]]
           | _ -> []
         in
         mknode $symbolstartpos $endpos L.SubscriptTriplet (subs @ [stride])
       }
   | e0_opt=ioption(expr) COLON e1_opt=ioption(expr) e2_opt=ioption(colon__expr) 
       { 
         let stride = 
           match e2_opt with
           | Some e2 -> [mknode $startpos(e2_opt) $endpos(e2_opt) L.Stride [e2]]
           | _ -> []
         in
         let is_ambiguous = stride = [] in
         let left = 
           match e0_opt with
           | Some e0 -> 
               let lab =
                 if is_ambiguous then
                   L.mkambiguous_first()
                 else
                   L.FirstSubscript
               in
               [mknode $startpos(e0_opt) $endpos(e0_opt) lab [e0]]
           | _ -> []
         in
         let right = 
           match e1_opt with
           | Some e1 -> 
               let lab =
                 if is_ambiguous then
                   L.mkambiguous_second()
                 else
                   L.SecondSubscript
               in
               [mknode $startpos(e1_opt) $endpos(e1_opt) lab [e1]]
           | _ -> []
         in
         let lab =
           if is_ambiguous then
             L.mkambiguous_triplet_or_range()
           else
             L.SubscriptTriplet
         in
         mknode $symbolstartpos $endpos lab (left @ right @ stride)
       }
   | f=linda_formal            { pgi(); f }
   | n=linda_common_block_name { n }
;

%inline
actual_arg:
(* | v=variable        { } contained in expr *)
(* | n=name            { } contained in expr *)
   | e=expr            { e }
   | a=alt_return_spec { a }
;

substring_range:
   | e0_opt=ioption(expr) COLON e1_opt=ioption(expr) 
       { 
         let st = 
           match e0_opt with
           | Some e0 -> [mknode $startpos(e0_opt) $endpos(e0_opt) L.StartingPoint [e0]]
           | _ -> []
         in
         let ed = 
           match e1_opt with
           | Some e1 -> [mknode $startpos(e1_opt) $endpos(e1_opt) L.EndingPoint [e1]]
           | _ -> []
         in
         mknode $symbolstartpos $endpos L.SubstringRange (st @ ed)
       }
;


%inline
colon__expr:
   | COLON e=expr { e }
;


array_constructor:
   | array_constructor_head vs=ac_spec SLASH_RPAREN 
       { 
         env#exit_array_ctor_context;
         mknode $startpos $endpos L.ArrayConstructor vs
       }
   | LBRACKET vs=ac_spec RBRACKET (* F2003 *)
       { 
         f2003();
         (*env#exit_array_ctor_context;*)
         mknode $startpos $endpos L.ArrayConstructor vs
       }
;

array_constructor_head:
   | LPAREN_SLASH { }
;

ac_spec: (* F2003 *)
   | t=decl_type_spec COLON_COLON                    { f2003(); [t] }
   | t=decl_type_spec COLON_COLON vs=clist(ac_value) { f2003(); t :: vs }
   |                              vs=clist(ac_value) { vs }
;

%inline
ac_value:
   | a=expr { disambiguate_ac_value a; a }
;


alt_return_spec:
   | STAR l=label { mkleaf $startpos $endpos (L.AltReturnSpec l#get_label) }
;

char_selector:
   | c=char_selector_no_length_selector { c }
   | l=length_selector                  { l }
;

char_selector_no_length_selector:
   | LPAREN l=LEN EQ c=char_len_param_value COMMA k=KIND EQ               e=expr RPAREN 
       { 
         ignore l;
         ignore k;
         let lnd = mknode $startpos(l) $endpos(c) L.LengthSelector [c] in
         let knd = mknode $startpos(k) $endpos(e) L.KindSelector [e] in
         mknode $startpos $endpos L.CharSelector [lnd; knd]
       }
   | LPAREN          c=char_len_param_value COMMA k_opt=ioption(kind__eq) e=expr RPAREN 
       { 
         ignore k_opt;
         let lnd = mknode $startpos(c) $endpos(c) L.LengthSelector [c] in
         let knd = mknode $startpos(k_opt) $endpos(e) L.KindSelector [e] in
         mknode $startpos $endpos L.CharSelector [lnd; knd]
       }
   | LPAREN k=KIND EQ e=expr                                       RPAREN 
       { 
         ignore k;
         let knd = mknode $startpos(k) $endpos(e) L.KindSelector [e] in
         mknode $startpos $endpos L.CharSelector [knd]
       }
   | LPAREN k=KIND EQ e=expr COMMA l=LEN EQ c=char_len_param_value RPAREN 
       { 
         ignore l;
         ignore k;
         let knd = mknode $startpos(k) $endpos(e) L.KindSelector [e] in
         let lnd = mknode $startpos(l) $endpos(c) L.LengthSelector [c] in
         mknode $startpos $endpos L.CharSelector [knd; lnd]
       }
;

length_selector:
   | LPAREN LEN EQ c=char_len_param_value RPAREN { mknode $startpos $endpos L.LengthSelector [c] }
   | LPAREN        c=char_len_param_value RPAREN { mknode $startpos $endpos L.LengthSelector [c] }
   | s=char_length_part (* ioption(COMMA) *)     { s } (* trailing COMMA is context dependent *)
;

char_length_part:
   | STAR c=char_length { mknode $startpos $endpos L.LengthSelectorOverride [c] }
;

char_length:
   | LPAREN c=char_len_param_value RPAREN { c } (* IBM *)
   | i=int_literal                        { mkleaf $startpos $endpos (L.Constant (Constant.mkint i)) }
   | c=PP_MACRO_CONST_INT                 { mkleaf $startpos $endpos (L.Constant (Constant.mkppm c)) }
;

%inline
char_len_param_value:
   | t=type_param_value { t }
;

type_param_value: (* F2003: supersedes char_len_param_value *)
   | e=expr { e }
   | STAR   { mkleaf $startpos $endpos L.TypeParamValueAsterisk }
   | COLON  { mkleaf $startpos $endpos L.TypeParamValueColon }
;

structure_decl: (* Compaq/Intel and IBM *)
   | s=structure_stmt fs=field_decl+ e=end_structure_stmt 
       { 
         end_scope();
         let n_opt = s#get_name_opt in
         let nd = mknode $startpos $endpos (L.StructureDecl n_opt) ((s :: fs) @ [e]) in
         begin
           match n_opt with
           | Some n -> finalize_object_spec n nd
           | _ -> ()
         end;
         nd
       }
;

%inline
field_decl: (* Compaq/Intel and IBM *)
   | t=type_declaration_stmt { t }
   | s=structure_decl        { s }
   | r=record_stmt           { r }
   | u=union_decl            { u }
   | p=parameter_stmt        { p }
;

union_decl: (* Compaq/Intel and IBM *)
   | u=union_stmt ms=map_decl+ e=end_union_stmt { mknode $startpos $endpos L.UnionDecl ((u :: ms) @ [e]) }
;

union_stmt: (* Compaq/Intel and IBM *)
   | u=_union_stmt stmt_end { u }
;

_union_stmt: (* Compaq/Intel and IBM *)
   | UNION { mkstmtleaf $startpos $endpos Stmt.UnionStmt }
;

end_union_stmt: (* Compaq/Intel and IBM *)
   | e=_end_union_stmt stmt_end { e }
;

_end_union_stmt: (* Compaq/Intel and IBM *)
   | END_UNION { mkstmtleaf $startpos $endpos Stmt.EndUnionStmt }
;

map_decl: (* Compaq/Intel and IBM *)
   | m=map_stmt fs=field_decl+ e=end_map_stmt { mknode $startpos $endpos L.MapDecl ((m :: fs) @ [e]) }
;

map_stmt: (* Compaq/Intel and IBM *)
   | m=_map_stmt stmt_end { m }
;

_map_stmt: (* Compaq/Intel and IBM *)
   | MAP { mkstmtleaf $startpos $endpos Stmt.MapStmt }
;

end_map_stmt: (* Compaq/Intel and IBM *)
   | e=_end_map_stmt stmt_end { e }
;

_end_map_stmt: (* Compaq/Intel and IBM *)
   | END_MAP { mkstmtleaf $startpos $endpos Stmt.EndMapStmt }
;


structure_stmt: (* Compaq/Intel and IBM *)
   | s=_structure_stmt stmt_end { s }
;

_structure_stmt: (* Compaq/Intel and IBM *)
   | structure_stmt_head n_opt=ioption(structure_name) fs=clist0(component_decl_OR_entity_decl)
       { 
         env#exit_name_context;
         env#enter_structure_context;
         let n_str_opt = map_opt (fun n -> n#get_name) n_opt in
         let frm = begin_structure_decl_scope n_str_opt in
         begin
           match n_str_opt with
           | Some n_str -> register_derived_type [] n_str frm
           | _ -> ()
         end;
         let eds = List.map (disambiguate_entity_decl false) fs in
         begin
           match n_str_opt with
           | Some n_str ->
               let ty = I.TypeSpec.Structure n_str in
               List.iter (register_edecl_node ty None) eds
           | _ -> ()
         end;
         mkstmtnode $startpos $endpos (Stmt.StructureStmt n_str_opt) eds
       }
;

structure_stmt_head: (* Compaq/Intel and IBM *)
   | STRUCTURE { env#enter_name_context }
;

%inline
structure_name: (* Compaq/Intel and IBM *)
   | SLASH n=name SLASH { n }
;

end_structure_stmt: (* Compaq/Intel and IBM *)
   | e=_end_structure_stmt stmt_end { e }
;

_end_structure_stmt: (* Compaq/Intel and IBM *)
   | END_STRUCTURE 
       { 
         env#exit_structure_context;
         mkstmtleaf $startpos $endpos Stmt.EndStructureStmt 
       }
;

record_stmt: (* Compaq/Intel and IBM *)
   | r=_record_stmt stmt_end { r }
;

_record_stmt: (* Compaq/Intel and IBM *)
   | record_stmt_head rs=separated_nonempty_list(COMMA__SLASH, record_decl) 
       { 
	 env#exit_name_context; 
	 env#exit_slash_name_context; 
         mkstmtnode $startpos $endpos Stmt.RecordStmt rs 
       }
;

record_stmt_head: (* Compaq/Intel and IBM *)
   | RECORD { env#enter_name_context; env#enter_slash_name_context }
;

record_decl: (* Compaq/Intel and IBM *)
   | SLASH n=name SLASH ds=clist(component_decl_OR_entity_decl) 
       { 
         let n_str = n#get_name in
         let eds = List.map (disambiguate_entity_decl false) ds in
         let ty = I.TypeSpec.Structure n_str in
         List.iter (register_edecl_node ty None) eds;
         mknode $startpos $endpos (L.RecordDecl n_str) eds
       }
;

(* conflicts with IBM's extension
record_field_ref:
   | p=part_ref_ fas=field_accessor+ 
       { 
         let c = p @ (List.flatten fas) in
         List.iter disambiguate_part_ref_elem c;
         let n = String.concat "." (get_part_names c) in
         mknode $startpos $endpos (L.RecordFieldRef n) c
       }
;
%inline
field_accessor:
   | DOT p=part_ref_ { p }
;
*)

derived_type_def:
   | d=derived_type_stmt p=derived_type_def_part e=end_type_stmt 
       { 
         end_scope();
         let n = d#get_name in
         let nd = mknode $startpos $endpos (L.DerivedTypeDef n) ((d :: p) @ [e]) in
         finalize_object_spec n nd;
         nd
       }
   | sd=DERIVED_TYPE_STMT p=derived_type_def_part e=end_type_stmt
       { 
         let s, d = sd in
         let n_str = 
           let ns = Xset.create 0 in
           visit
             (fun nd ->
               match nd#label with
               | L.Stmt stmt -> begin
                   match Stmt.get_stmt stmt with
                   | Stmt.DerivedTypeStmt n -> Xset.add ns n
                   | _ -> ()
               end
               | _ -> ()
             ) d;
           Xlist.to_string (fun x -> x) "|" (Xset.to_list ns)
         in
         let nd = mknode $startpos $endpos (L.DerivedTypeDef n_str) ((d :: p) @ [e]) in
         finalize_object_spec n_str nd;
         nd
       }
;

derived_type_def_part:
   | cs=derived_type_def_content* t_opt=ioption(type_bound_procedure_part) { cs @ (opt_to_list t_opt) }
;

derived_type_def_content:
   | p=private_sequence_stmt     { p }
   | sp=DERIVED_TYPE_DEF_PART    { let s, p = sp in check_error p; p }
   | c=component_def_stmt        { c }
   | p=pp_directive              { p }
;

type_bound_procedure_part:
   | c=tb_contains_stmt b_opt=binding_private_stmt_opt ts=type_bound_proc_binding*
       { 
         let children = 
           match b_opt with
           | Some b -> c :: b :: ts
           | None -> c :: ts
         in
         mknode $startpos $endpos L.TypeBoundProcedurePart children
       }
;

tb_contains_stmt:
   | c=_tb_contains_stmt stmt_end { c }
;

_tb_contains_stmt:
   | CONTAINS 
       { 
         context_stack#push (C.type_bound_proc_part());
         mkstmtleaf $startpos $endpos Stmt.ContainsStmt 
       }
;

%inline
binding_private_stmt_opt:
   | b_opt=ioption(binding_private_stmt) { b_opt }
;

binding_private_stmt:
   | p=_binding_private_stmt stmt_end { p }
;

_binding_private_stmt:
   | PRIVATE  { mkstmtleaf $startpos $endpos Stmt.BindingPrivateStmt }
;

type_bound_proc_binding:
   | t=type_bound_procedure_stmt { t }
   | t=type_bound_generic_stmt   { t }
   | f=final_procedure_stmt      { f }
;

final_procedure_stmt: (* F2003 *)
   | f=_final_procedure_stmt stmt_end { f2003(); f }
;

_final_procedure_stmt:
   | FINAL colon_colon_opt ns=clist(name) { mkstmtnode $startpos $endpos Stmt.FinalProcedureStmt ns }
;

type_bound_generic_stmt: (* F2003 *)
   | t=_type_bound_generic_stmt stmt_end { f2003(); t }
;

_type_bound_generic_stmt:
   | GENERIC a_opt=ioption(comma__access_spec) COLON_COLON g=generic_spec EQ_GT ns=clist(name) 
       { 
         let c =
           match a_opt with
           | Some a -> a :: g :: ns
           | None -> g :: ns
         in
         mkstmtnode $startpos $endpos Stmt.TypeBoundGenericStmt c
       }
;
%inline
comma__access_spec:
   | COMMA a=access_spec { a }
;

type_bound_procedure_stmt:
   | t=_type_bound_procedure_stmt stmt_end { t }
;

_type_bound_procedure_stmt:
   | procedure_kw a_opt=ioption(binding_attr_part) ts=clist(type_bound_proc_decl) 
       { 
         env#exit_procedure_context;
         let children =
           match a_opt with
           | Some a -> a @ ts
           | None -> ts
         in
         mkstmtnode $startpos $endpos (Stmt.TypeBoundProcedureStmt None) children
       }
   | procedure_kw LPAREN n=name RPAREN a=binding_attr_part ns=clist(name)     
       { 
         env#exit_procedure_context;
         mkstmtnode $startpos $endpos (Stmt.TypeBoundProcedureStmt (Some n#get_name)) (a @ ns)
       }
;

type_bound_proc_decl:
   | b=name              { mkleaf $startpos $endpos (L.TypeBoundProcDecl(b#get_name, None)) }
   | b=name EQ_GT p=name { mkleaf $startpos $endpos (L.TypeBoundProcDecl(b#get_name, Some p#get_name)) }
;

binding_attr_part:
   |                              COLON_COLON { [] }
   | COMMA bs=clist(binding_attr) COLON_COLON { bs }
;

binding_attr:
   | a=_binding_attr { mkleaf $startpos $endpos (L.BindingAttr a) }
;

_binding_attr:
   | PASS                      { BindingAttr.Pass "" }
   | PASS LPAREN n=name RPAREN { BindingAttr.Pass n#get_name }
   | NOPASS                    { BindingAttr.Nopass }
   | NON_OVERRIDABLE           { BindingAttr.NonOverridable }
   | DEFERRED                  { BindingAttr.Deferred }
   | PUBLIC                    { BindingAttr.Public }
   | PRIVATE                   { BindingAttr.Private }
;

derived_type_stmt:
   | d=_derived_type_stmt stmt_end { d }
;

_derived_type_stmt:
   | type_kw a_opt=ioption(type_attr_part) n=name 
       { 
         env#exit_type_context;
         let n_str = n#get_name in
         let frm = begin_derived_type_def_scope n_str in
         let attrs =
           match a_opt with
           | None | Some [] -> []
           | Some a -> a
         in
         register_derived_type attrs n_str frm;
         context_stack#push (C.derived_type_def_part());
         mkstmtnode $startpos $endpos (Stmt.DerivedTypeStmt n_str) attrs
       }
;

%inline
type_attr_part:
   |                                COLON_COLON { [] }
   | COMMA ts=clist(type_attr_spec) COLON_COLON { ts }
;

%inline
type_attr_spec:
   | a=access_spec { a#relab (L.access_spec_to_type_attr_spec a#label); a }
   | BIND LPAREN c=IDENTIFIER RPAREN (* F2003 *)
       { 
         f2003();
         if c <> "c" && c <> "C" then
           parse_warning $startpos $endpos "invalid language-binding-spec: %s" c;

         mkleaf $startpos $endpos (L.TypeAttrSpec TypeAttrSpec.Bind)
       }
   | ABSTRACT                           (* F2003 *)
       { 
         f2003();
         mkleaf $startpos $endpos (L.TypeAttrSpec TypeAttrSpec.Abstract) 
       }
   | EXTENDS LPAREN n=IDENTIFIER RPAREN (* F2003 *)
       { 
         f2003();
         mkleaf $startpos $endpos (L.TypeAttrSpec (TypeAttrSpec.Extends n)) 
       }
;

%inline
access_spec:
   | PUBLIC  { mkleaf $startpos $endpos (L.AccessSpec AccessSpec.Public) }
   | PRIVATE { mkleaf $startpos $endpos (L.AccessSpec AccessSpec.Private) }
;

private_sequence_stmt:
   | p=private_stmt  { p }
   | s=sequence_stmt { s }
;

private_stmt:
   | p=_private_stmt stmt_end { p }
;

sequence_stmt:
   | p=_sequence_stmt stmt_end { p }
;

_private_stmt:
   | PRIVATE  { mkstmtleaf $startpos $endpos Stmt.PrivateStmt }
;

_sequence_stmt:
   | SEQUENCE { mkstmtleaf $startpos $endpos Stmt.SequenceStmt }
;

component_def_stmt:
   | c=_component_def_stmt stmt_end { c }
;

_component_def_stmt:
   | t=decl_type_spec a_opt_opt=ioption(component_attr_part) cs=clist(component_decl_OR_entity_decl) 
       { 
         let a_opt =
           match a_opt_opt with
           | Some a_opt -> a_opt
           | None -> None
         in
         env#exit_type_spec_context;
         let is_deferred = 
           match a_opt with
           | Some a -> contain_allocatable_or_pointer a#children
           | None -> false
         in
         begin
           match a_opt with
           | Some a -> disambiguate_component_attr_specs is_deferred a#children
           | _ -> ()
         end;
         let cds = List.map (disambiguate_component_decl is_deferred) cs in
         let ty = ty_of_node t in
         let attr_opt = 
           match a_opt with
           | Some a -> Some (name_attribute_of_aspec_nodes a#children)
           | None -> None
         in
         List.iter (register_edecl_node ty attr_opt) cds;
         let children =
           match a_opt with
           | None -> t :: cds
           | Some a -> t :: a :: cds
         in
         mkstmtnode $startpos $endpos Stmt.ComponentDefStmt children
       }
   | procedure_kw LPAREN i_opt=proc_interface_opt RPAREN COMMA ps=proc_compo_attr_specs COLON_COLON ds=proc_decls 
       { 
         env#exit_procedure_context;
         let c =
           match i_opt with
           | Some i -> i :: (ps @ ds)
           | None -> ps @ ds
         in
         mkstmtnode $startpos $endpos Stmt.ProcComponentDefStmt c
       }
;

component_attr_part:
   |                           COLON_COLON { None }
   | COMMA ss=clist(attr_spec) COLON_COLON { Some (mknode $startpos $endpos L.ComponentAttrSpecs ss) }
;

%inline
proc_interface_opt:
   | i_opt=ioption(proc_interface) { i_opt }
;

proc_interface:
   | n=name           { n }
   | t=decl_type_spec { t }
;

%inline
proc_compo_attr_specs:
   | ps=clist(proc_compo_attr_spec) { ps }
;

proc_compo_attr_spec:
   | a=_proc_compo_attr_spec { mkleaf $startpos $endpos (L.ProcComponentAttrSpec a) }
;

_proc_compo_attr_spec:
   | POINTER                   { ProcComponentAttrSpec.Pointer }
   | PASS                      { ProcComponentAttrSpec.Pass "" }
   | PASS LPAREN n=name RPAREN { ProcComponentAttrSpec.Pass n#get_name }
   | NOPASS                    { ProcComponentAttrSpec.Nopass }
   | PUBLIC                    { ProcComponentAttrSpec.Public }
   | PRIVATE                   { ProcComponentAttrSpec.Private }
;

%inline
proc_decls:
   | ds=clist(proc_decl) { ds }
;

proc_decl:
   | n=name                       { mkleaf $startpos $endpos (L.ProcDecl n#get_name) }
   | n=name EQ_GT i=proc_ptr_init { mknode $startpos $endpos (L.ProcDecl n#get_name) [i] }
;

%inline
proc_ptr_init:
   | t=target { t }
(*
   | n=null_ref { n }
   | n=name     { mkleaf $startpos $endpos (L.InitialProcTarget n#get_name) }
*)
;


null_ref:
   | NULL LPAREN e_opt=ioption(expr) RPAREN 
       { 
         let es = opt_to_list e_opt in
         mknode $startpos $endpos (L.FunctionReference "null") es
       }
;

%inline
array_spec_part:
   | LPAREN a=array_spec RPAREN { a }
;

array_spec:
   | ss=clist(shape_spec) { mknode $startpos $endpos (L.mkambiguous_tuple()) ss }
   | DOT DOT              { mkleaf $startpos $endpos L.AssumedRankArray } (* ISO/IEC TS 29113:2012 *)
;

shape_spec:
   | e=explicit_shape_spec { e }
   | e=expr COLON          { mknode $startpos $endpos (L.mkambiguous_assumed()) [e] }
   |        COLON          { mkleaf $startpos $endpos (L.mkambiguous_deferred()) }
   | STAR                  { mkleaf $startpos $endpos (L.mkambiguous_assumedsize()) }
   | e=expr COLON STAR     { mknode $startpos $endpos (L.mkambiguous_assumedsize()) [e] }
;

explicit_shape_spec:
   | e0=expr COLON e1=expr { mknode $startpos $endpos L.ExplicitShapeSpec [e0; e1] }
   |               e1=expr { mknode $startpos $endpos L.ExplicitShapeSpec [e1] }
;


coarray_spec_part:
   | LBRACKET c=coarray_spec RBRACKET { (*env#exit_array_ctor_context;*) c }
;

coarray_spec:
   | ss=clist(deferred_coshape_spec) { mknode $startpos $endpos L.DeferredCoshapeCoarray ss }
   | ss=clist(explicit_coshape_spec) { mknode $startpos $endpos L.ExplicitCoshapeCoarray ss }
;

%inline
deferred_coshape_spec:
   | COLON { mkleaf $startpos $endpos L.DeferredCoshapeSpec }
;

explicit_coshape_spec:
   | l=expr COLON u=expr { mknode $startpos $endpos L.ExplicitCoshapeSpec [l; u] }
   | l=expr COLON STAR   { mknode $startpos $endpos L.ExplicitCoshapeSpec [l] }
   |              u=expr { u }
   |              STAR   { mknode $startpos $endpos L.ExplicitCoshapeSpec [] }
;

%inline
array_spec_part_opt:
   | a_opt=ioption(array_spec_part) { a_opt }
;

%inline
coarray_spec_part_opt:
   | c_opt=ioption(coarray_spec_part) { c_opt }
;

%inline
char_length_part_opt:
   | c_opt=ioption(char_length_part) { c_opt }
;

%inline
init_opt:
   | i_opt=ioption(initialization) { i_opt }
;

component_decl_OR_entity_decl:
   | n=entity_name a_opt=array_spec_part_opt c_opt=coarray_spec_part_opt l_opt=char_length_part_opt i_opt=init_opt
       { 
         let lab = L.mkambiguous_namedtuple n#get_name in
         let children = (opt_to_list c_opt) @ (opt_to_list l_opt) @ (opt_to_list i_opt) in
         let nd = mknode $startpos $endpos lab children in
         a_opt, c_opt, nd
       }
   | v=PP_MACRO_VARIABLE { None, None, mkleaf $startpos $endpos (L.PpMacroEntityDecl v) } (* ??? *)
;

entity_name:
   | ns=name+ 
       { 
         let lab = mkn (Xlist.to_string (fun n -> n#get_name) "" ns) in
         mkleaf $startpos $endpos lab
       }
;

initialization:
   | EQ e=expr        { mknode $startpos $endpos L.InitializationExpr [e] }
   | EQ_GT n=null_ref { mknode $startpos $endpos L.InitializationNull [n] }
   | EQ_GT v=var_or_ref       (* F2008 *)
       { 
         disambiguate_variable v; mknode $startpos $endpos L.InitializationTarget [v] 
       }
   | SLASH vs=clist(data_stmt_value) SLASH { mknode $startpos $endpos L.InitializationOldStyle vs } (* GNU *)
;

end_type_stmt:
   | e=_end_type_stmt stmt_end { e }
   | e=END_TYPE_STMT           { get_nd e }
;

_end_type_stmt:
   | END_TYPE n_opt=ioption(name) 
       { 
         context_stack#pop;
         mkstmtleaf $startpos $endpos (Stmt.EndTypeStmt (node_opt_to_name_opt n_opt)) 
       }
;

specification_stmt:
   | s=specification_stmt_no_access_stmt { s }
   | a=access_stmt                       { a }
;

specification_stmt_no_access_stmt:
   | a=allocatable_stmt  { a }
   | c=common_stmt       { c }
(* | d=data_stmt         { d } *)
   | d=dimension_stmt    { d }
   | e=equivalence_stmt  { e }
(*   | e=external_stmt     { e }*)
   | i=intent_stmt       { i }
   | i=intrinsic_stmt    { i }
   | n=namelist_stmt     { n }
   | o=optional_stmt     { o }
   | p=pointer_stmt      { p }
   | s=save_stmt         { s }
   | t=target_stmt       { t }

   | a=asynchronous_stmt { f2003(); a } (* F2003 *)
   | b=bind_stmt         { f2003(); b } (* F2003 *)
(*
   | p=protected_stmt    { f2003(); p } (* F2003 *)
   | v=value_stmt        { f2003(); v } (* F2003 *)
   | v=volatile_stmt     { f2003(); v } (* F2003 *)
*)

   | c=codimension_stmt  { f2008(); c } (* F2008 *)
(*   | c=contiguous_stmt   { f2008(); c } (* F2008 *)*)

   | s=simple_attr_stmt { s }
;

common_stmt:
   | c=_common_stmt stmt_end { c }
;

_common_stmt:
   | common_stmt_head css=separated_nonempty_list(ioption(COMMA__SLASH), common_spec) 
       { 
	 env#exit_name_context; 
	 env#exit_slash_name_context; 
         List.iter
           (fun cs ->
             try
               register_common_block cs#get_name
             with
               Not_found -> ()
           ) css;
	 mkstmtnode $startpos $endpos Stmt.CommonStmt css
       }
;

common_stmt_head:
   | COMMON { (* env#enter_name_context; env#enter_slash_name_context *) }
;

common_spec:
   | n_opt_opt=ioption(common_name_part) cs=clist(common_block_object) 
       { 
         let name_opt =
           match n_opt_opt with
           | Some n_opt -> node_opt_to_name_opt n_opt
           | None -> None
         in
         mknode $symbolstartpos $endpos (L.CommonSpec name_opt) cs
       }
;

common_name_part:
   | SLASH n_opt=ioption(name) SLASH { n_opt }
   | SLASH_SLASH                     { None }
;

common_block_object:
   | n=name                                             { mkleaf $startpos $endpos (L.CommonBlockObject n#get_name) }
   | n=name LPAREN es=clist(explicit_shape_spec) RPAREN 
       { 
         let dim = N.Dimension.explicit_shape (List.length es) in
         set_attr_of_data_object (fun a -> a#set_dimension dim) n#get_name;
         mknode $startpos $endpos (L.CommonBlockObject n#get_name) es 
       }
;

parameter_stmt:
   | p=_parameter_stmt stmt_end { intel(); p }
;

_parameter_stmt:
   | PARAMETER LPAREN ds=clist(named_constant_def) RPAREN { mkstmtnode $startpos $endpos Stmt.ParameterStmt ds }
   | PARAMETER        ds=clist(named_constant_def)        { mkstmtnode $startpos $endpos Stmt.ParameterStmt ds } (* Compaq/Intel *)
;


named_constant_def:
   | n=name EQ e=expr 
       { 
         set_attr_of_data_object (fun attr -> attr#set_parameter) n#get_name;
         mknode $startpos $endpos (L.NamedConstantDef n#get_name) [e] 
       }
;

type_declaration_stmt:
   | t=_type_declaration_stmt stmt_end { t }
;

_type_declaration_stmt:
   | ta=type_declaration_stmt_head es=clist(component_decl_OR_entity_decl)
       { 
         env#exit_type_spec_context;
         env#exit_character_context;
         context_stack#activate_top;
	 env#exit_name_context;
         let tspec, attr_specs_opt = ta in
         let is_deferred = 
           match attr_specs_opt with
           | Some attr_specs -> contain_allocatable_or_pointer attr_specs#children
           | None -> false
         in
         begin
           match attr_specs_opt with
           | Some attr_specs -> disambiguate_attr_specs is_deferred attr_specs#children
           | _ -> ()
         end;
         let eds = List.map (disambiguate_entity_decl is_deferred) es in
         let edns = Xlist.filter_map (fun n -> n#get_name_opt) eds in
         let ty = ty_of_node tspec in
         let attr_opt = 
           match attr_specs_opt with
           | Some an -> Some (name_attribute_of_aspec_nodes an#children)
           | None -> None
         in
         List.iter (register_edecl_node ty attr_opt) eds;
         let children = (tspec :: (opt_to_list attr_specs_opt)) @ eds in
	 mkstmtnode $startpos $endpos (Stmt.TypeDeclarationStmt edns) children
       }
;


type_declaration_stmt_head:
   | t=decl_type_spec_no_character                                   
       { 
         env#enter_name_context;
         t, None 
       }
   | t=decl_type_spec_no_character ss=comma__attr_spec* COLON_COLON 
       { 
         env#enter_name_context;
         let asnd_opt = 
           if ss = [] then
             None
           else
             Some (mknode $startpos(ss) $endpos(ss) L.AttrSpecs ss)
         in
         t, asnd_opt
       }
   | ta=type_declaration_stmt_head_character { env#enter_name_context; ta }
;

%inline
type_declaration_stmt_head_character:
   | character                                                                 { mkleaf $startpos $endpos (L.TypeSpec TypeSpec.Character), None }
   | t=type_declaration_stmt_head_character_                                   { t, None }
   | t=type_declaration_stmt_head_character_ ss=comma__attr_spec* COLON_COLON 
       { 
         let asnd = mknode $startpos(ss) $endpos(ss) L.AttrSpecs ss in
         t, Some asnd 
       }

   | character s=char_length_part ioption(COMMA)                                 { mknode $startpos $endpos (L.TypeSpec TypeSpec.Character) [s], None }
   | character s_opt=ioption(char_length_part) ss=comma__attr_spec+ COLON_COLON 
       { 
         let asnd = mknode $startpos(ss) $endpos(ss) L.AttrSpecs ss in
         mknode $startpos $endpos (L.TypeSpec TypeSpec.Character) (opt_to_list s_opt), Some asnd
       }
   | character s_opt=ioption(char_length_part)                       COLON_COLON { mknode $startpos $endpos (L.TypeSpec TypeSpec.Character) (opt_to_list s_opt), None }
;

%inline
type_declaration_stmt_head_character_:
   | character c=char_selector_no_length_selector                  
       { mknode $startpos $endpos (L.TypeSpec TypeSpec.Character) [c] }
   | character LPAREN l_opt=ioption(len__eq) c=char_len_param_value RPAREN 
       { 
         ignore l_opt;
         let lnd = mknode $startpos(l_opt) $endpos(c) L.LengthSelector [c] in
         mknode $startpos $endpos (L.TypeSpec TypeSpec.Character) [lnd] 
       }
;

%inline
len__eq:
   | LEN EQ { }
;

%inline
comma__attr_spec:
   | COMMA a=attr_spec { a }
;

attr_spec:
   | PARAMETER                          { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Parameter)}
   | a=access_spec                      { a#relab (L.access_spec_to_attr_spec a#label); a }
   | ALLOCATABLE                        { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Allocatable) }
(*   | EXTERNAL                           { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.External) }*)
   | intent LPAREN i=intent_spec RPAREN { env#exit_intent_context; mkleaf $startpos $endpos (L.AttrSpec (AttrSpec.Intent i)) }
   | INTRINSIC                          { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Intrinsic) }
   | OPTIONAL                           { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Optional) }
   | SAVE                               { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Save) }
   | TARGET                             { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Target) }
   | POINTER                            { mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Pointer) }
   | DIMENSION a=array_spec_part        { mknode $startpos $endpos (L.AttrSpec AttrSpec.Dimension) [a] }

   | ASYNCHRONOUS                       { f2003(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Asynchronous) } (* F2003 *)
   | l=language_binding_spec            { f2003(); l#relab (L.AttrSpec AttrSpec.Bind); l }                       (* F2003 *)
(*
   | PROTECTED                          { f2003(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Protected) }    (* F2003 *)
   | VALUE                              { f2003(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Value) }        (* F2003 *)
   | VOLATILE                           { f2003(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Volatile) }     (* F2003 *)
*)
   | CODIMENSION c=coarray_spec_part    { f2008(); mknode $startpos $endpos (L.AttrSpec AttrSpec.Codimension) [c] } (* F2008 *)
(*   | CONTIGUOUS                         { f2008(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Contiguous) }      (* F2008 *)*)

(*
   | AUTOMATIC                          { ibm();intel(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Automatic) } (* Compaq/Intel and IBM *)
   | STATIC                             { ibm();intel(); mkleaf $startpos $endpos (L.AttrSpec AttrSpec.Static) }    (* Compaq/Intel and IBM *)
*)
   | kw=SIMPLE_ATTR { mkleaf $startpos $endpos (L.AttrSpec (AttrSpec.of_keyword kw))}
;

language_binding_spec: (* F2003 *)
   | language_binding_spec_head LPAREN c=IDENTIFIER e_opt=ioption(comma__name_spec) RPAREN 
       { 
         env#exit_bind_context;
         if c <> "c" && c <> "C" then
           parse_warning $startpos $endpos "invalid language-binding-spec: %s" c;

         mknode $startpos $endpos L.LanguageBindingSpec (opt_to_list e_opt)
       }
;

language_binding_spec_head:
   | BIND { env#enter_bind_context}
;

%inline
comma__name_spec: (* F2003 *)
   | COMMA NAME_ EQ e=expr { e }
;

intent:
   | INTENT { env#enter_intent_context }
;

(*
entity_decl:
   | name ioption(array_spec_part) ioption(char_length_part) ioption(initialization) { }
;
*)

intent_spec:
   | kw=INTENT_SPEC { IntentSpec.of_keyword kw }
(*
   | IN     { IntentSpec.In }
   | OUT    { IntentSpec.Out }
   | IN_OUT { IntentSpec.InOut }
*)
;

%inline
execution_part:
   | se=specification_part__execution_part 
       { 
         let sp_nd_opt, ep_nd_opt = se in
         let eps = 
           match ep_nd_opt with
           | Some ep_nd -> ep_nd#children
           | None -> []
         in
         match sp_nd_opt with
         | None -> eps
         | Some sp_nd -> 
             List.iter
               (fun nd ->
                 if L.is_specification_part_construct nd#label && not (L.is_execution_part_construct nd#label) then
                   parse_warning_loc nd#loc 
                     "execution-part contains specification-part-construct: %s" (L.to_simple_string nd#label)
               ) sp_nd#children;
             sp_nd#children @ eps
       }
;

execution_part_construct:
   | e=executable_construct { e }
(* | f=format_entry_stmt    { f } *)
(* | d=data_stmt            { d } *)
;


executable_construct:
   | a=action_stmt_no_assign { a }
   | c=case_construct        { c }
(*
   | d=do_construct          { d }
   | d=label_do_stmt         { d }
   | i=if_construct          { i }
*)
   | d=do_stmt               { d } (* handle later *)
   | sd=DO_STMT              { let s, d = sd in d } (* handle later *)
   | e=end_do_stmt           { e } (* handle later *)

   | i=if_then_stmt          { i } (* handle later *)
   | e=else_if_stmt          { e } (* handle later *)
   | e=else_stmt             { e } (* handle later *)
   | si=IF_THEN_STMT         { let s, i = si in i } (* handle later *)
   | e=end_if_stmt           { e } (* handle later *)

   | f=forall_construct      { f }
   | w=where_construct       { w }
   | se=EXEC_PART_CONSTRUCT  { let s, e = se in check_error e; e }

   | s=select_type_construct { s }
   | a=associate_construct   { a }
   | b=block_construct       { b }
   | c=critical_construct    { c }
;


construct_name__colon:
   | n=CONSTRUCT_NAME COLON { n }
;


critical_construct:
   | c=critical_stmt b=block e=end_critical_stmt 
       { 
         mknode $startpos $endpos L.CriticalConstruct (c :: b @ [e])
       }
;

critical_stmt:
   | c=_critical_stmt stmt_end { c }
;

_critical_stmt:
   | n_opt=ioption(construct_name__colon) CRITICAL
       { 
         mkstmtleaf $symbolstartpos $endpos (Stmt.CriticalStmt n_opt)
       }
;

end_critical_stmt:
   | e=_end_critical_stmt stmt_end { e }
;

_end_critical_stmt:
   | END_CRITICAL n_opt=ioption(name) 
       { 
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtleaf $startpos $endpos (Stmt.EndCriticalStmt nm_opt) 
       }
;


block_construct:
   | b=block_stmt se=specification_part__execution_part e=end_block_stmt 
       { 
         let sp_opt, ep_opt = se in
         let c0 =
           match ep_opt with
           | Some ep_nd -> 
               let st, ed = node_to_lexposs ep_nd in
               [mknode st ed L.Block ep_nd#children; e]
           | None -> [e]
         in
         let c =
           match sp_opt with
           | Some sp_nd -> b :: sp_nd :: c0
           | None -> b :: c0
         in
         mknode $startpos $endpos L.BlockConstruct c
       }
;

block_stmt:
   | b=_block_stmt stmt_end { b }
;

_block_stmt:
   | n_opt=ioption(construct_name__colon) BLOCK 
       { 
         begin_block_scope n_opt;
         mkstmtleaf $symbolstartpos $endpos (Stmt.BlockStmt n_opt)
       }
;

end_block_stmt:
   | e=_end_block_stmt stmt_end { e }
;

_end_block_stmt:
   | END_BLOCK n_opt=ioption(name) 
       { 
         end_scope();
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtleaf $startpos $endpos (Stmt.EndBlockStmt nm_opt) 
       }
;


associate_construct:
   | a=associate_stmt b=block e=end_associate_stmt 
       { 
         mknode $startpos $endpos L.AssociateConstruct (a :: (b @ [e])) 
       }
;

associate_stmt:
   | a=_associate_stmt stmt_end { a }
;

_associate_stmt:
   | n_opt=ioption(construct_name__colon) ASSOCIATE LPAREN al=clist(association) RPAREN 
       { 
         mkstmtnode $symbolstartpos $endpos (Stmt.AssociateStmt n_opt) al
       }
;

association:
   | n=name EQ_GT e=expr 
       { 
         register_associate_name n#get_name;
         mknode $startpos $endpos (L.Association n#get_name) [e] 
       }
;

end_associate_stmt:
   | e=_end_associate_stmt stmt_end { e }
;

_end_associate_stmt:
   | END_ASSOCIATE n_opt=ioption(name) 
       { 
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtleaf $startpos $endpos (Stmt.EndAssociateStmt nm_opt) 
       }
;

select_type_construct:
   | s=select_type_stmt gs=type_guard_block* e=end_select_type_stmt 
       { 
         mknode $startpos $endpos L.SelectTypeConstruct (s :: (gs @ [e]))
       }
;

select_type_stmt:
   | s=_select_type_stmt stmt_end { s }
;

_select_type_stmt:
   | n_opt=ioption(construct_name__colon) SELECT_TYPE LPAREN s=selector_part RPAREN 
       { 
         env#enter_select_type_context;
         mkstmtnode $symbolstartpos $endpos (Stmt.SelectTypeStmt n_opt) [s]
       }
;

selector_part:
   |              e=expr { e }
   | n=name EQ_GT e=expr 
       { 
         register_associate_name n#get_name;
         mknode $startpos $endpos (L.Association n#get_name) [e] 
       }
;

type_guard_block:
   | t=type_guard_stmt b=block { mknode $startpos $endpos L.TypeGuardBlock (t::b) }
;

type_guard_stmt:
   | t=_type_guard_stmt stmt_end { t }
;

_type_guard_stmt:
   | type_is  LPAREN t=type_spec         RPAREN n_opt=ioption(name) 
       { 
         env#exit_type_guard_context;
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtnode $startpos $endpos (Stmt.TypeIsTypeGuardStmt nm_opt) [t] 
       }
   | class_is LPAREN d=derived_type_spec RPAREN n_opt=ioption(name) 
       { 
         env#exit_type_guard_context;
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtnode $startpos $endpos (Stmt.ClassIsTypeGuardStmt nm_opt) [d] 
       }
   | CLASS_DEFAULT                              n_opt=ioption(name) 
       { 
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtleaf $startpos $endpos (Stmt.ClassDefaultTypeGuardStmt nm_opt)
       }
;

type_is:
   | TYPE_IS { env#enter_type_guard_context }
;
class_is:
   | CLASS_IS { env#enter_type_guard_context }
;

end_select_type_stmt:
   | e=_end_select_type_stmt stmt_end { e }
;

_end_select_type_stmt:
   | END_SELECT n_opt=ioption(name) 
       { 
         env#exit_select_type_context;
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtleaf $startpos $endpos (Stmt.EndSelectTypeStmt nm_opt)
       }
;

(*
action_stmt:
   | a=action_stmt_no_assign   { a }
   | a=assignment_stmt         { a }
   | p=pointer_assignment_stmt { p }
   | sa=ACTION_STMT            { let s, a = sa in a }
;
*)

action_stmt:
   | a=_action_stmt stmt_end { a }
;

_action_stmt:
   | a=_action_stmt_no_assign   { a }
   | a=_assignment_stmt         { a }
   | p=_pointer_assignment_stmt { p }
   | sa=ACTION_STMT             { let s, a = sa in a }
;

action_stmt_no_assign:
   | a=_action_stmt_no_assign stmt_end { a }
;

_action_stmt_no_assign:
   | a=_allocate_stmt           { a }
   | a=_arithmetic_if_stmt      { a }
   | a=_assign_stmt             { a } (* deleted in F95 *)
   | a=_assigned_goto_stmt      { a } (* deleted in F95 *)
(* | a=_assignment_stmt         { a } *)
   | b=_backspace_stmt          { b }
   | c=_call_stmt               { disambiguate_call c; c }
   | c=_close_stmt              { c }
   | c=_computed_goto_stmt      { c }
   | c=_continue_stmt           { c }
   | c=_cycle_stmt              { c }
   | d=_deallocate_stmt         { d }
   | e=_endfile_stmt            { e }
(*
   | _end_function_stmt       { }
   | _end_program_stmt        { }
   | _end_subroutine_stmt     { }
*)
   | e=_exit_stmt               { e }
   | f=_forall_stmt             { f }
   | g=_goto_stmt               { g }
   | i=_if_stmt                 { i }
   | i=_inquire_stmt            { i }
   | n=_nullify_stmt            { n }
   | o=_open_stmt               { o }
   | d=_define_file_stmt        { d } (* Compaq/Intel *)
   | p=_pause_stmt              { p } (* deleted in F95 *)
(* | p=_pointer_assignment_stmt { p } *)
   | p=_print_stmt              { p }
   | t=_type_stmt               { t }
   | r=_read_stmt               { r }
   | r=_return_stmt             { r }
   | r=_rewind_stmt             { r }
   | s=_stop_stmt               { s }
   | e=_error_stop_stmt         { e }
   | w=_where_stmt              { w }
   | w=_write_stmt              { w }
   | rw=_read_OR_write_stmt     { rw }
   | rp=_read_OR_print_stmt     { rp }
   | m=_pp_macro_stmt           { m }

   | r=_rewrite_stmt            { r } (* Compaq/Intel *)
   | d=_delete_stmt             { d } (* Compaq/Intel *)
   | u=_unlock_stmt             { u } (* Compaq/Intel *)
   | f=_find_stmt               { f } (* Compaq/Intel *)
   | e=_encode_decode_stmt      { e } (* Compaq/Intel *)
   | a=_accept_stmt             { a } (* Compaq/Intel *)
   | w=_wait_stmt               { w }
   | f=_flush_stmt              { f }
   | l=_lock_stmt               { l } (* F2008 *)
   | s=_sync_xxx_stmt           { s } (* F2008: xxx = all, images, or memory *)
;

_sync_xxx_stmt:
   | SYNC kw=IDENTIFIER a_opt=ioption(sync_stmt_part) 
       { 
         let slab =
           match (String.lowercase_ascii kw) with
           | "all" -> Stmt.SyncAllStmt
           | "images" -> Stmt.SyncImagesStmt
           | "memory" -> Stmt.SyncMemoryStmt
           | _ -> assert false
         in
         let c = 
           match a_opt with
           | Some l -> l
           | None -> []
         in
         mkstmtnode $startpos $endpos slab c
       }
;

sync_stat_OR_image_set:
   | e=expr      { e }
   | STAR        { mkleaf $startpos $endpos L.AllImages }
   | s=sync_stat { s }
;

sync_stmt_part:
   | LPAREN                                  RPAREN { [] }
   | LPAREN ss=clist(sync_stat_OR_image_set) RPAREN { ss }
;

_wait_stmt:
   | wait_stmt_head LPAREN ss=clist(wait_spec) RPAREN 
       { 
         env#exit_wait_context;
         mkstmtnode $startpos $endpos Stmt.WaitStmt ss 
       }
;

wait_stmt_head:
   | WAIT { env#enter_wait_context }
;

wait_spec:
   | p=position_spec { position_spec_to_wait_spec p }
   | END EQ l=label  { mkleaf $startpos $endpos (L.WaitSpec (WaitSpec.End l#get_label)) }
   | EOR EQ l=label  { mkleaf $startpos $endpos (L.WaitSpec (WaitSpec.Eor l#get_label)) }
   | ID  EQ e=expr   { mknode $startpos $endpos (L.WaitSpec WaitSpec.Id) [e] }
;

(*
assign_stmt:
   | a=_assign_stmt stmt_end { a }
;
*)
_assign_stmt:
   | ASSIGN l=label TO v=var_or_ref
       { 
         f90(); 
         disambiguate_variable v;
         mkstmtnode $startpos $endpos (Stmt.AssignStmt l#get_label) [v] 
       }
;
(*
assigned_goto_stmt:
   | a=_assigned_goto_stmt stmt_end { a }
;
*)
_assigned_goto_stmt:
   | GO_TO v=var_or_ref
       { 
	 f90(); 
         disambiguate_variable v;
	 mkstmtnode $startpos $endpos Stmt.AssignedGotoStmt [v] 
       }
   | GO_TO v=var_or_ref         COMMA  LPAREN ls=clist(label) RPAREN 
       { 
	 f90(); 
         disambiguate_variable v;
	 mkstmtnode $startpos $endpos Stmt.AssignedGotoStmt (v :: ls) 
       }
(* | GO_TO v=var_or_ref ioption(COMMA) LPAREN clist(label) RPAREN { f90() } GO_TO v LPAREN clist(label) LPAREN is contained in GO_TO v *)
;
(*
pause_stmt:
   | p=_pause_stmt stmt_end { p }
;
*)
_pause_stmt:
   | PAUSE s_opt=ioption(stop_code) 
       { 
         f90(); 
         mkstmtnode $startpos $endpos Stmt.PauseStmt (opt_to_list s_opt)  
       }
;
(*
exit_stmt:
   | e=_exit_stmt stmt_end { e }
;
*)
_exit_stmt:
   | EXIT n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.ExitStmt (node_opt_to_name_opt n_opt)) }
;

(*
deallocate_stmt:
   | d=_deallocate_stmt stmt_end { d }
;
*)
_deallocate_stmt:
 | DEALLOCATE LPAREN os=clist(deallocate_stmt_part) RPAREN
       { 
	 mkstmtnode $startpos $endpos Stmt.DeallocateStmt os
       }
;

%inline
deallocate_stmt_part:
   | o=object_              { disambiguate_allocation o; o }
(*   | STAT   EQ v=var_or_ref { disambiguate_variable v; mknode $startpos $endpos L.StatVariable [v] }*)
(*   | ERRMSG EQ v=var_or_ref { disambiguate_variable v; mknode $startpos $endpos L.ErrmsgVariable [v] }*)
   | kw=IDENTIFIER EQ v=var_or_ref
       { 
         disambiguate_variable v;
         let lab =
           match (String.lowercase_ascii kw) with
           | "errmsg" -> L.ErrmsgVariable
           | "stat" -> L.StatVariable
           | _ -> L.WEIRD kw
         in
         mknode $startpos $endpos lab [v]
       }
;

(*
inquire_stmt:
   | i=_inquire_stmt stmt_end { i }
*)
_inquire_stmt:
   | inquire_stmt_head LPAREN ss=clist(inquire_spec)  RPAREN                   
       { 
	 env#exit_inquire_context;
	 mkstmtnode $startpos $endpos Stmt.InquireStmt ss
       }
   | inquire_stmt_head LPAREN i=IOLENGTH EQ v=var_or_ref RPAREN os=clist(io_item) 
       { 
         ignore i;
	 env#exit_inquire_context;
         disambiguate_variable v;
         let ind = mknode $startpos(i) $endpos(v) L.IoLength [v] in
         let ond = mknode $startpos(os) $endpos(os) L.OutputItemList os in
	 mkstmtnode $startpos $endpos Stmt.InquireStmt [ind; ond] 
       }
;

inquire_stmt_head:
   | INQUIRE { env#enter_inquire_context }
;

inquire_spec:
   | p=position_spec { position_spec_to_inquire_spec p }

   | i=inquire_spec_kw_no_file EQ e=expr (*v=var_or_ref*)
       { 
         (*disambiguate_variable v;*)
         mknode $startpos $endpos (L.InquireSpec i) [e]
       }
   | FILE EQ e=expr         { mknode $startpos $endpos (L.InquireSpec (InquireSpec.File)) [e] }
(*| POS  EQ e=expr { mknode $startpos $endpos (L.InquireSpec (InquireSpec.Pos)) [e] }*)
;

%inline
inquire_spec_kw_no_file:
(*| c=connect_spec_kw_no_file { ConnectSpec.to_inquire_spec c }*)
   | NAME_        { InquireSpec.Name }
   | READ         { InquireSpec.Read }
   | WRITE        { InquireSpec.Write }
   | ASYNCHRONOUS { InquireSpec.Asynchronous }

   | kw=CONNECT_INQUIRE_IOCTL_SPEC { InquireSpec.of_keyword kw }
   | kw=CONNECT_INQUIRE_SPEC       { InquireSpec.of_keyword kw }
   | kw=INQUIRE_SPEC               { InquireSpec.of_keyword kw }
   | kw=INQUIRE_IOCTL_SPEC         { InquireSpec.of_keyword kw }
;
(*     
nullify_stmt:
   | n=_nullify_stmt stmt_end { n }
;
*)
_nullify_stmt:
   | NULLIFY LPAREN os=clist(object_) RPAREN 
       { 
         List.iter disambiguate_pointer_object os;
         mkstmtnode $startpos $endpos Stmt.NullifyStmt os
       }
;
(*
arithmetic_if_stmt:
   | a=_arithmetic_if_stmt stmt_end { a }
;
*)
_arithmetic_if_stmt:
   | e=if__lparen__expr__rparen l0=label COMMA l1=label COMMA l2=label 
         { 
           context_stack#pop; (* action_stmt *)
           mkstmtnode $startpos $endpos Stmt.ArithmeticIfStmt [e; l0; l1; l2] 
         }
;
(*
computed_goto_stmt:
   | c=_computed_goto_stmt stmt_end { c }
;
*)
_computed_goto_stmt:
   | computed_goto_stmt_head LPAREN__GO_TO ls=clist(label) RPAREN ioption(COMMA) e=expr 
       { 
         env#exit_name_context;
         mkstmtnode $startpos $endpos Stmt.ComputedGotoStmt (ls @ [e])
       }
;

computed_goto_stmt_head:
   | GO_TO { env#enter_name_context }
;


(*
allocate_stmt:
   | a=_allocate_stmt stmt_end { a }
;
*)
_allocate_stmt: (* type-spec :: from F2003 *)
   | allocate_ LPAREN t_opt=ioption(type_spec__colon_colon) os=clist(allocate_stmt_part) RPAREN
       { 
         env#exit_allocate_context;
	 mkstmtnode $startpos $endpos Stmt.AllocateStmt ((opt_to_list t_opt) @ os)
       }
;

allocate_:
   | ALLOCATE { env#enter_allocate_context }
;

%inline
type_spec__colon_colon:
   | t=decl_type_spec COLON_COLON { t }
   | dt=part_ref_ COLON_COLON 
       { 
         let t = mknode $startpos(dt) $endpos(dt) (L.mkambiguous_tuple()) dt in
         disambiguate_derived_type_spec t; t 
       }
;

%inline
allocate_stmt_part:
   | o=object_              { disambiguate_allocation o; o }
(*   | STAT   EQ v=var_or_ref { disambiguate_variable v; mknode $startpos $endpos L.StatVariable [v] }*)
(*   | ERRMSG EQ v=var_or_ref { disambiguate_variable v; mknode $startpos $endpos L.ErrmsgVariable [v] } (* F2003 *)*)
(*   | SOURCE EQ e=expr       { mknode $startpos $endpos L.SourceExpr [e] }  (* F2003 *)*)
(*   | MOLD   EQ e=expr       { mknode $startpos $endpos L.MoldExpr [e] }    (* F2008 *)*)
   | kw=ALLOC_OPT_EXPR EQ e=expr
       {
        let lab =
          match (String.lowercase_ascii kw) with
          | "mold" -> L.MoldExpr
          | "source" -> L.SourceExpr
          | _ -> L.WEIRD kw
        in
         mknode $startpos $endpos lab [e]
       }

   | kw=IDENTIFIER EQ v=var_or_ref
       { 
         disambiguate_variable v;
         let lab =
           match (String.lowercase_ascii kw) with
           | "errmsg" -> L.ErrmsgVariable
           | "stat" -> L.StatVariable
           | _ -> L.WEIRD kw
         in
         mknode $startpos $endpos lab [v]
       }

(* | o=object_ LPAREN clist(allocate_shape_spec) RPAREN { } contained in object_ *)
;


object_:
(* | name                  { } contained in structure_component *)
   | s=structure_component { s }
   | v=PP_MACRO_VARIABLE   { mkleaf $startpos $endpos (L.PpMacroObject v) }
;

%inline
structure_component:
   | d=data_ref_ { d }
;

assignment_stmt: (* contains stmt-function-stmt *)
  | a=_assignment_stmt stmt_end { a }
;

_assignment_stmt: (* may be stmt_function_stmt *)
   | v=assignment_stmt_head e=expr
       { 
         context_stack#pop; (* expr *)
         let nd = mkstmtnode $startpos $endpos Stmt.AssignmentStmt [v; e] in
         propagate_binding nd;
         nd
       }
;

assignment_stmt_head:
   | v=var_or_ref EQ 
       { 
         disambiguate_variable v; 
         context_stack#push (C.expr());
         v 
       }
;

do_stmt:
   | d=_do_stmt stmt_end { d }
;

_do_stmt:
   | n_opt=do_stmt_head l_opt=ioption(label) lc_opt=ioption(loop_control)
       { 
         env#exit_do_context;
         let lab =
           Stmt.DoStmt(n_opt, map_opt (fun l -> l#get_label) l_opt, map_opt (fun lc -> lc#get_var) lc_opt)
         in
         mkstmtnode $symbolstartpos $endpos lab (opt_to_list lc_opt)
       }
;

do_stmt_head:
   | n_opt=ioption(construct_name__colon) DO { env#enter_do_context; n_opt }
;

%inline
label:
   | i=int_literal        { mkleaf $startpos $endpos (L.Label (normalize_label i)) }
;

loop_control:
   | ioption(COMMA) v=var_or_ref EQ e0=expr COMMA e1=expr e2_opt=ioption(comma__expr) 
       { 
         disambiguate_variable ~mklab:(fun n -> L.VariableName n) v;
         let vn =
           try
             v#get_name
           with
             Not_found -> "?"
         in
         mknode $symbolstartpos $endpos (L.LoopControl vn) ([v; e0; e1] @ (opt_to_list e2_opt))
       }
   | ioption(COMMA) WHILE LPAREN e=expr RPAREN { mknode $symbolstartpos $endpos L.LoopControlWhile [e] }
   | ioption(COMMA) CONCURRENT f=forall_header { mknode $symbolstartpos $endpos L.LoopControlConcurrent [f] }
;

block:
   | se=specification_part__execution_part 
       { 
         let sp_nd_opt, ep_nd_opt = se in
         let eps = 
           match ep_nd_opt with
           | Some ep_nd -> ep_nd#children
           | None -> []
         in
         let children =
           match sp_nd_opt with
           | None -> eps
           | Some sp_nd ->
             List.iter
               (fun nd ->
                 if L.is_specification_part_construct nd#label && not (L.is_execution_part_construct nd#label) then
                   parse_warning_loc nd#loc 
                     "block contains specification-part-construct: %s" (L.to_simple_string nd#label)
               ) sp_nd#children;
             sp_nd#children @ eps
         in
         if children = [] then
           []
         else
           [mknode $startpos $endpos L.Block children]
       }
;

end_do_stmt:
   | e=_end_do_stmt stmt_end { e }
   | e=END_DO_STMT           { get_nd e }
;

_end_do_stmt:
   | END_DO n_opt=ioption(name) 
       { 
         mkstmtleaf $startpos $endpos (Stmt.EndDoStmt (node_opt_to_name_opt n_opt)) 
       }
;
(*
continue_stmt:
   | c=_continue_stmt stmt_end { c }
;
*)
_continue_stmt:
   | CONTINUE { mkstmtleaf $startpos $endpos Stmt.ContinueStmt }
;


_define_file_stmt:
   | DEFINE_FILE ss=clist(define_file_spec)
       { 
         mkstmtnode $startpos $endpos Stmt.DefineFileStmt ss
       }
;

define_file_spec:
   | i=INT_LITERAL LPAREN m=expr COMMA n=expr COMMA expr COMMA asv=var_or_ref RPAREN
       { 
         let u = mkleaf $startpos(i) $endpos(i) (L.Constant (Constant.mkint i)) in
         mknode $startpos $endpos L.DefineFileSpec [u;m;n;asv]
       }
;

(*
open_stmt:
   | o=_open_stmt stmt_end { o }
;
*)
_open_stmt:
   | open_stmt_head LPAREN ss=clist(connect_spec) RPAREN 
       { 
         env#exit_open_context;
         mkstmtnode $startpos $endpos Stmt.OpenStmt ss
       }
;

open_stmt_head:
   | OPEN { env#enter_open_context }
;

connect_spec:
   | c=close_spec                { close_spec_to_connect_spec c }
   | c=connect_spec_kw EQ e=expr { mknode $startpos $endpos (L.ConnectSpec c) [e] }
;


connect_spec_kw:
(*| c=connect_spec_kw_no_file { c }*)
   | FILE                      { ConnectSpec.File }
   | NAME_                     { ConnectSpec.Name }         (* Compaq/Intel *)
   | ASYNCHRONOUS              { ConnectSpec.Asynchronous } (* F2003 *)

   | kw=CONNECT_SPEC               { ConnectSpec.of_keyword kw }
   | kw=CONNECT_INQUIRE_SPEC       { ConnectSpec.of_keyword kw }
   | kw=CONNECT_INQUIRE_IOCTL_SPEC { ConnectSpec.of_keyword kw }
;
(*
%inline
connect_spec_kw_no_file:
   | kw=CONNECT_SPEC               { ConnectSpec.of_keyword kw }
   | kw=CONNECT_INQUIRE_SPEC       { ConnectSpec.of_keyword kw }
   | kw=CONNECT_INQUIRE_IOCTL_SPEC { ConnectSpec.of_keyword kw }
;
*)

(*
call_stmt:
   | c=_call_stmt stmt_end { c }
;
*)
_call_stmt:
   | CALL m=PP_MACRO_EXPR (* same as function-reference *)
       { 
         env#exit_name_context;
         mkstmtnode $startpos $endpos (Stmt.CallStmt m) []
       }
   | CALL n=PP_MACRO_VARIABLE t_opt=ioption(tuple) 
       { 
         env#exit_name_context;
         let args =
           match t_opt with
           | Some t -> t#children
           | None -> []
         in
         mkstmtnode $startpos $endpos (Stmt.CallStmt n) args
       }
   | CALL d=data_ref_ 
       { 
         env#exit_name_context;
         let n, desig, args = disambiguate_proc_desig d in
         mkstmtnode $startpos $endpos (Stmt.CallStmt n) (desig @ args)
       }
;

(*
close_stmt:
   | c=_close_stmt stmt_end { c }
;
*)
_close_stmt:
   | close_stmt_head LPAREN ss=clist(close_spec) RPAREN 
       { 
         env#exit_close_context;
         mkstmtnode $startpos $endpos Stmt.CloseStmt ss
       }
;

close_stmt_head:
   | CLOSE { env#enter_close_context }
;

close_spec:
   | p=position_spec { position_spec_to_close_spec p }
   | c=close_spec1   { c }
   | kw=INTEL_CLOSE_CONNECT_SPEC EQ e=expr (* Compaq/Intel *)
       { 
         intel(); 
         mknode $startpos $endpos (L.CloseSpec (CloseSpec.of_keyword kw)) [e]
       }
;

%inline
position_spec_no_expr:
   | UNIT     EQ e=expr       { mknode $startpos $endpos (L.PositionSpec PositionSpec.Unit) [e] }
   | IOSTAT   EQ v=var_or_ref 
       { 
         disambiguate_variable v;
         mknode $startpos $endpos (L.PositionSpec PositionSpec.Iostat) [v]
       }
   | IOMSG   EQ v=var_or_ref (* F2003 *)
       { 
         f2003();
         disambiguate_variable v;
         mknode $startpos $endpos (L.PositionSpec PositionSpec.Iomsg) [v]
       }
   | ERR      EQ l=label      { mknode $startpos $endpos (L.PositionSpec (PositionSpec.Err l#get_label)) [] }
;

position_spec:
   | p=position_spec_no_expr { p }
   | e=expr                  { mknode $startpos $endpos (L.PositionSpec PositionSpec.Unit) [e] }
;

close_spec1:
   | STATUS EQ e=expr { mknode $startpos $endpos (L.CloseSpec CloseSpec.Status) [e] }
;

io_control_spec:
(* | p=position_spec      { position_spec_to_io_control_spec p } *)
(* |        f=format      { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Fmt) [f] } *)
(* |        name          { } contained in expr *)

   | pf=position_spec_OR_format { pf }

   | UNIT EQ STAR         { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.PreconnectedUnit) [] }
   | FMT EQ f=format      { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Fmt) [f] }
   | NML EQ n=name        { mknode $startpos $endpos (L.IoControlSpec (IoControlSpec.Nml n#get_name)) [] }
   | END EQ l=label       { mknode $startpos $endpos (L.IoControlSpec (IoControlSpec.End l#get_label)) [] }
   | SIZE EQ v=var_or_ref 
       { 
         disambiguate_variable v;
         mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Size) [v] 
       }
   | EOR EQ l=label       { mknode $startpos $endpos (L.IoControlSpec (IoControlSpec.Eor l#get_label)) [] }
(*
   | ADVANCE EQ e=expr    { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Advance) [e] }
   | REC EQ e=expr        { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Rec) [e] }
   | POS EQ e=expr        { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Pos) [e] }
*)

   | s=ioctl_spec_kw EQ e=expr { mknode $startpos $endpos (L.IoControlSpec s) [e] }

   | ASYNCHRONOUS EQ e=expr { mknode $startpos $endpos (L.IoControlSpec IoControlSpec.Asynchronous) [e] }
;

ioctl_spec_kw:
   | kw=CONNECT_INQUIRE_IOCTL_SPEC { IoControlSpec.of_keyword kw }
   | kw=INQUIRE_IOCTL_SPEC         { IoControlSpec.of_keyword kw }
   | kw=IOCTL_SPEC                 { IoControlSpec.of_keyword kw }
;

%inline
position_spec_OR_format:
   | p=position_spec_no_expr { position_spec_to_io_control_spec p }
   | STAR    { mkleaf $startpos $endpos (L.Format Format.ListDirected) }
   | e=expr  { mknode $startpos $endpos (L.DUMMY) [e] } (* classified later *)
;

%inline
io_control_spec_list:
   | ss=clist(io_control_spec) 
       { 
         begin
           match ss with
           | fst::rest -> begin
               begin
                 match fst#label with (* first dummy must be UNIT *)
                 | L.DUMMY -> fst#relab (L.IoControlSpec IoControlSpec.Unit)
                 | L.Format Format.ListDirected -> fst#relab (L.IoControlSpec IoControlSpec.PreconnectedUnit)
                 | _ -> ()
               end;
               match rest with
               | snd::rest0 -> begin
                   List.iter 
                     (fun nd ->
                       match nd#label with
                       | L.DUMMY -> nd#relab (L.ERROR "")
                       | _ -> ()
                     ) rest0;
                   match snd#label with (* second dummy must be FMT or NML *)
                   | L.DUMMY -> begin
                       match snd#children with
                       | [nd] -> begin
                           match nd#label with
                           | L.Constant (Constant.IntLiteralConstant i) -> begin
                               let lab = normalize_label i in
                               snd#relab (L.Format (Format.Label lab));
                               snd#set_children []
                           end
                           | L.Name name -> begin
                               snd#relab (L.IoControlSpec (IoControlSpec.Nml name));
                               snd#set_children []
                           end
                           | _ -> snd#relab (L.Format Format.Expr)
                       end
                       | _ -> assert false
                   end
                   | _ -> ()
               end
               | _ -> ()
           end
           | _ -> ()
         end;
         ss
       } 
;

format:
   | e=expr  
       { 
         match e#label with
         | L.Constant (Constant.IntLiteralConstant i) ->
             let lab = normalize_label i in
             mkleaf $startpos $endpos (L.Format (Format.Label lab))
         | _ ->
             mknode $startpos $endpos (L.Format Format.Expr) [e] 
       }
(* | l=label { } contained in expr *)
   | STAR    { mkleaf $startpos $endpos (L.Format Format.ListDirected) }
;


%inline
io_item:
   | i=expr { disambiguate_io_item i; i }
;

_read_OR_write_stmt: (* gfortran accepts extra comma *)
   | kis=read_OR_write_stmt_head2 ioption(COMMA) os=clist0(io_item) (* GNU accepts optional comma *)
       { 
         env#exit_name_context;
         let kind, id, ss = kis in
         match kind with
           | Macro.K_WRITE      -> 
               let item_nd = mknode $startpos(os) $endpos(os) L.OutputItemList os in
               mkstmtnode $startpos $endpos (Stmt.PpMacroStmt id) (ss @ [item_nd])               

           | Macro.K_READ_WRITE -> 
               let item_nd = mknode $startpos(os) $endpos(os) L.IoItemList os in
               mkstmtnode $startpos $endpos (Stmt.PpMacroStmt id) (ss @ [item_nd])               

           | _ -> parse_error $startpos $endpos "syntax error"
       }
;

read_OR_write_stmt_head2:
   | k_m=read_OR_write_stmt_head LPAREN__io_control_spec ss=io_control_spec_list RPAREN
       { 
         env#exit_io_control_context;
         let kind, id = k_m in
         kind, id, ss
       }

   | id=PP_MACRO_READ_WRITE LPAREN__io_control_spec ss=io_control_spec_list RPAREN
       { 
         env#exit_io_control_context;
         Macro.K_READ_WRITE, id, ss
       }
;

read_OR_write_stmt_head:
   | km=PP_MACRO_ID_RW
       { 
         let kind, id = km in
         begin
           match kind with
           | Macro.K_WRITE      -> 
               let body = 
                 Macro.mk_obj_body ~stat:(Macro.Resolved (Obj.repr (Tokens_.PP_MACRO_WRITE id))) "<constrained>" 
               in
               env#lex_define_macro id body

           | Macro.K_READ_WRITE -> 
               let body = 
                 Macro.mk_obj_body ~stat:(Macro.Resolved (Obj.repr (Tokens_.PP_MACRO_READ_WRITE id))) "<constrained>" 
               in
               env#lex_define_macro id body

           | _ -> ()
         end;
         km 
       }
;

(*
write_stmt:
   | w=_write_stmt stmt_end { w }
;
*)

_write_stmt: (* gfortran accepts extra comma *)
   | ss=write_stmt_head2 ioption(COMMA) os=clist0(io_item) (* GNU accepts optional comma *)
       { 
         env#exit_name_context;
         let ond = mknode $startpos(os) $endpos(os) L.OutputItemList os in
         mkstmtnode $startpos $endpos Stmt.WriteStmt (ss @ [ond])
       }
;

write_stmt_head2:
   | write_stmt_head LPAREN__io_control_spec ss=io_control_spec_list RPAREN
       { 
         env#exit_io_control_context;
         ss
       }
;

write_stmt_head:
   | WRITE          { env#enter_name_context; env#enter_io_control_context }
   | PP_MACRO_WRITE {  }
;

_rewrite_stmt: (* Compaq/Intel *)
   | ss=rewrite_stmt_head2 ioption(COMMA) os=clist0(io_item)
       { 
         env#exit_name_context;
         let ond = mknode $startpos(os) $endpos(os) L.OutputItemList os in
         mkstmtnode $startpos $endpos Stmt.RewriteStmt (ss @ [ond])
       }
;

rewrite_stmt_head2:
   | rewrite_stmt_head LPAREN__io_control_spec ss=io_control_spec_list RPAREN
       { 
         env#exit_io_control_context;
         ss
       }
;

rewrite_stmt_head:
   | REWRITE { env#enter_name_context; env#enter_io_control_context }
;

_encode_decode_stmt: (* Compaq/Intel *)
   | slab=encode_decode_stmt_head LPAREN__position_spec ss=encode_decode_arg RPAREN is=clist0(io_item)
       { 
         env#exit_position_context;
         let ind = mknode $startpos(is) $endpos(is) L.IoItemList is in
         mkstmtnode $startpos $endpos slab (ss @ [ind])
       }
;
encode_decode_stmt_head:
   | ENCODE { env#enter_position_context; Stmt.EncodeStmt }
   | DECODE { env#enter_position_context; Stmt.DecodeStmt }
;

%inline
encode_decode_arg:
   | f=expr COMMA c=expr COMMA b=var_or_ref ss=clist0(encode_decode_spec) 
       { 
         disambiguate_variable b; 
         [f; c; b] @ ss 
       }
;
%inline
encode_decode_spec:
   | IOSTAT EQ v=var_or_ref 
       { 
         disambiguate_variable v; 
         mknode $startpos $endpos (L.PositionSpec PositionSpec.Iostat) [v]
       }
   | ERR    EQ l=label      
       { 
         mkleaf $startpos $endpos (L.PositionSpec (PositionSpec.Err l#get_label))
       }
;

pointer_assignment_stmt:
   | p=_pointer_assignment_stmt stmt_end { p }
;

_pointer_assignment_stmt:
   | o=object_ EQ_GT t=target 
       { 
         disambiguate_data_pointer_object o;
         let nd = mkstmtnode $startpos $endpos Stmt.PointerAssignmentStmt [o; t] in
         propagate_binding nd;
         nd
       }
;

%inline
target:
(* | v=var_or_ref { } contained in expr *)
   | e=expr     { e }
   | n=null_ref { n }
;

(*
print_stmt:
   | p=_print_stmt stmt_end { p }
;
*)
_print_stmt:
   | PRINT f=format                             
       { 
         env#exit_io_control_context; 
         env#exit_name_context;
         mkstmtnode $startpos $endpos Stmt.PrintStmt [f] 
       }
   | PRINT f=format COMMA os=clist(io_item) 
       { 
         env#exit_io_control_context;
         env#exit_name_context;
         let ond = mknode $startpos(os) $endpos(os) L.OutputItemList os in
         mkstmtnode $startpos $endpos Stmt.PrintStmt [f; ond] 
       }
;

_read_OR_print_stmt: (* e.g. DCOUP "v1_Gen4: Cloop(1) =", Cloop(1) ENDL of superpy *)
   | id=read_OR_print_stmt_head es=clist(expr) n_opt=ioption(name)
       { 
         env#exit_name_context;
         mkstmtnode $startpos $endpos (Stmt.PpMacroStmt id) (es @ (opt_to_list n_opt))
       }
;

read_OR_print_stmt_head:
   | id=PP_MACRO_READ_PRINT
       { 
         let body = 
           Macro.mk_obj_body ~stat:(Macro.Resolved (Obj.repr (Tokens_.PP_MACRO_READ_PRINT id))) "<constrained>" 
         in
         env#lex_define_macro id body;
         id 
       }
;

_type_stmt:
   | type_kw f=format__type                         { (*env#exit_io_control_context;*) mkstmtnode $startpos $endpos Stmt.TypeStmt [f] }
   | type_kw f=format__type COMMA os=clist(io_item) 
       { 
         env#exit_type_context;
         (*env#exit_io_control_context;*)
         let ond = mknode $startpos(os) $endpos(os) L.OutputItemList os in
         mkstmtnode $startpos $endpos Stmt.TypeStmt [f; ond] 
       }
;
%inline
format__type:
   | l=label { l }
   | STAR    { mkleaf $startpos $endpos (L.Format Format.ListDirected) }
;


(*
read_stmt:
   | r=_read_stmt stmt_end { r }
;
*)
_read_stmt:  (* GNU accepts optional comma *)
   | ss=read_stmt_head2 ioption(COMMA) is=clist0(io_item)
       { 
         env#exit_name_context;
         let ind = mknode $startpos(is) $endpos(is) L.InputItemList is in
         mkstmtnode $startpos $endpos Stmt.ReadStmt (ss @ [ind])
       }
   | read_stmt_head f=format                                                  
       { 
         env#exit_io_control_context;
         env#exit_name_context;
         mkstmtnode $startpos $endpos Stmt.ReadStmt [f] 
       }
   | read_stmt_head f=format COMMA is=clist(io_item)                          
       { 
         env#exit_io_control_context;
         env#exit_name_context;
         let ind = mknode $startpos(is) $endpos(is) L.InputItemList is in
         mkstmtnode $startpos $endpos Stmt.ReadStmt [f; ind]
       }
;

read_stmt_head2:
   | read_stmt_head LPAREN__io_control_spec ss=io_control_spec_list RPAREN
       { 
         env#exit_io_control_context; 
         ss
       }

read_stmt_head:
   | READ { (* env#enter_io_control_context *) }
;

_accept_stmt: (* Compaq/Intel *)
   | accept_stmt_head f=format                                                  
       { 
         env#exit_io_control_context;
         mkstmtnode $startpos $endpos Stmt.AcceptStmt [f] 
       }
   | accept_stmt_head f=format COMMA is=clist(io_item)                          
       { 
         env#exit_io_control_context;
         let ind = mknode $startpos(is) $endpos(is) L.InputItemList is in
         mkstmtnode $startpos $endpos Stmt.AcceptStmt [f; ind]
       }
;
accept_stmt_head:
   | ACCEPT { env#enter_io_control_context }
;

_find_stmt:
   | find_stmt_head LPAREN__io_control_spec ss=io_control_spec_list RPAREN ioption(COMMA)
       { 
         env#exit_io_control_context; 
         mkstmtnode $startpos $endpos Stmt.FindStmt ss
       }
;
find_stmt_head:
   | FIND { env#enter_io_control_context }
;

(*
return_stmt:
   | r=_return_stmt stmt_end { r }
;
*)
_return_stmt:
   | RETURN        { mkstmtleaf $startpos $endpos Stmt.ReturnStmt }
   | RETURN e=expr { mkstmtnode $startpos $endpos Stmt.ReturnStmt [e] }
;


_delete_stmt: (* Compaq/Intel *)
   | delete_stmt_head LPAREN__position_spec ss=clist(position_spec) RPAREN 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.DeleteStmt ss
       }
;
delete_stmt_head:
   | DELETE { env#enter_position_context }
;


_lock_stmt: (* F2008 *)
   | LOCK LPAREN ls=clist(lock_variable_OR_lock_stat) RPAREN 
       { 
         mkstmtnode $startpos $endpos Stmt.LockStmt ls
       }
;

lock_variable_OR_lock_stat:
   | v=var_or_ref { disambiguate_variable v; v }
   | l=lock_stat  { l }
;

%inline
lock_stat:
   | s=sync_stat { s }
;

_unlock_stmt: 
   | unlock_stmt_head e=file_unit_number 
       { (* Compaq/Intel *)
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.UnlockStmt [e] 
       }
   | unlock_stmt_head LPAREN__position_spec ss=clist(position_spec_OR_sync_stat) RPAREN 
       { (* Compaq/Intel *) (* F2008 *)
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.UnlockStmt ss 
       }
;

%inline
position_spec_OR_sync_stat:
   | p=position_spec { p }
   | s=sync_stat     { s }
;

unlock_stmt_head:
   | UNLOCK { env#enter_position_context }
;

%inline
sync_stat:
(*   | STAT   EQ v=var_or_ref { disambiguate_variable v; mknode $startpos $endpos L.StatVariable [v] }*)
(*   | ERRMSG EQ v=var_or_ref { disambiguate_variable v; mknode $startpos $endpos L.ErrmsgVariable [v] }*)
   | kw=IDENTIFIER EQ v=var_or_ref
       { 
         disambiguate_variable v;
         let lab =
           match (String.lowercase_ascii kw) with
           | "acquired_lock" -> L.AcquiredLock
           | "errmsg" -> L.ErrmsgVariable
           | "stat" -> L.StatVariable
           | _ -> L.WEIRD kw
         in
         mknode $startpos $endpos lab [v]
       }
;

(*
rewind_stmt:
   | r=_rewind_stmt stmt_end { r }
;
*)
_rewind_stmt:
   | rewind_stmt_head e=file_unit_number                 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.RewindStmt [e] 
       }
   | rewind_stmt_head LPAREN__position_spec ss=clist(position_spec) RPAREN 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.RewindStmt ss 
       }
;
rewind_stmt_head:
   | REWIND { env#enter_position_context }
;

(*
endfile_stmt:
   | e=_endfile_stmt stmt_end { e }
;
*)
_endfile_stmt:
   | endfile_stmt_head e=file_unit_number                 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.EndfileStmt [e] 
       }
   | endfile_stmt_head LPAREN__position_spec ss=clist(position_spec) RPAREN 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.EndfileStmt ss
       }
;
endfile_stmt_head:
   | END_FILE { env#enter_position_context }
;

(*
backspace_stmt:
   | b=_backspace_stmt stmt_end { b }
;
*)
_backspace_stmt:
   | backspace_stmt_head e=file_unit_number                 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.BackspaceStmt [e] 
       }
   | backspace_stmt_head LPAREN__position_spec ss=clist(position_spec) RPAREN 
       { 
         env#exit_position_context; 
         mkstmtnode $startpos $endpos Stmt.BackspaceStmt ss
       }
;

backspace_stmt_head:
   | BACKSPACE { env#enter_position_context }
;

_flush_stmt:
   | flush_stmt_head e=file_unit_number
       { 
         env#exit_flush_context;
         mkstmtnode $startpos $endpos Stmt.FlushStmt [e]
       }
   | flush_stmt_head LPAREN__flush_spec ss=clist(flush_spec) RPAREN
       { 
         env#exit_flush_context;
         mkstmtnode $startpos $endpos Stmt.FlushStmt ss 
       }
;

flush_stmt_head:
   | FLUSH { env#enter_flush_context }
;

flush_spec:
   | p=position_spec { position_spec_to_flush_spec p }
;


%inline
file_unit_number:
   | e=expr { e }
;

(*
where_stmt:
   | WHERE LPAREN e=expr RPAREN a=assignment_stmt { mkstmtnode $startpos $endpos Stmt.WhereStmt [e; a] }
;
*)

where_stmt:
   | w=_where_stmt stmt_end { w }
;

_where_stmt:
   | WHERE LPAREN e=expr RPAREN a=_assignment_stmt { mkstmtnode $startpos $endpos Stmt.WhereStmt [e; a] }
;
(*
stop_stmt:
   | s=_stop_stmt stmt_end { s }
;
*)
_stop_stmt:
   | STOP s_opt=ioption(stop_code) { mkstmtnode $startpos $endpos Stmt.StopStmt (opt_to_list s_opt) }
;

_error_stop_stmt:
   | ERROR STOP s_opt=ioption(stop_code) { mkstmtnode $startpos $endpos Stmt.ErrorStopStmt (opt_to_list s_opt) }
;

%inline
stop_code:
   | e=expr      { e } (* F2008 *)
(* | c=constant  { mkleaf $startpos $endpos (L.StopCode c) } *)
(* | int_literal { } contained in constant *)
;
(*
cycle_stmt:
   | c=_cycle_stmt stmt_end { c }
;
*)
_cycle_stmt:
   | CYCLE n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.CycleStmt (node_opt_to_name_opt n_opt)) }
;
(*
goto_stmt:
   | g=_goto_stmt stmt_end { g }
;
*)
_goto_stmt:
   | GO_TO l=label { mkstmtnode $startpos $endpos Stmt.GotoStmt [l] }
;

access_stmt:
   | a=access_stmt_private { a }
   | a=access_stmt_public  { a }
;

access_stmt_private:
   | a=_access_stmt_private stmt_end { a }
;
access_stmt_public:
   | a=_access_stmt_public stmt_end { a }
;

_access_stmt_private:
   | PRIVATE                                             
       { 
         env#set_default_accessibility_private;
         mkstmtnode $startpos $endpos (Stmt.AccessStmt AccessSpec.Private) [] 
       }
   | PRIVATE colon_colon_opt ss=clist(generic_spec) 
       { 
         env#exit_access_context;
         List.iter 
           (fun g ->
             match g#label with 
             | L.GenericSpec (GenericSpec.Name n) -> (* maybe a use-name *)
                 g#relab (L.mkambiguous_generic_spec_or_use_name n)
             | _ -> () 
           ) ss;
         List.iter disambiguate_generic_spec_OR_use_name ss;
         List.iter (set_access_spec_attr N.AccessSpec.Private) ss;
         mkstmtnode $startpos $endpos (Stmt.AccessStmt AccessSpec.Private) ss 
       }
;

_access_stmt_public:
   | PUBLIC                                              
       { 
         env#set_default_accessibility_public;
         mkstmtnode $startpos $endpos (Stmt.AccessStmt AccessSpec.Public) [] 
       }
   | PUBLIC colon_colon_opt ss=clist(generic_spec) 
       { 
         env#exit_access_context;
         List.iter 
           (fun g ->
             match g#label with 
             | L.GenericSpec (GenericSpec.Name n) -> (* maybe a use-name *)
                 g#relab (L.mkambiguous_generic_spec_or_use_name n)
             | _ -> () 
           ) ss;
         List.iter disambiguate_generic_spec_OR_use_name ss;
         List.iter (set_access_spec_attr N.AccessSpec.Public) ss;
         mkstmtnode $startpos $endpos (Stmt.AccessStmt AccessSpec.Public) ss 
       }
;

%inline
colon_colon_opt:
   | ioption(COLON_COLON) { }
;

allocatable_stmt:
   | a=_allocatable_stmt stmt_end { a }
;

_allocatable_stmt:
   | ALLOCATABLE colon_colon_opt aas=clist(allocatable_array) { mkstmtnode $startpos $endpos Stmt.AllocatableStmt aas }
;

%inline
allocatable_array:
   | n=name ss_opt=ioption(lparen__deferred_shape_spec_list__rparen) 
       { 
         let snds = list_opt_to_list ss_opt in
         let r = List.length snds in
         let setter attr =
           attr#set_allocatable;
           if r > 0 then
             attr#set_dimension (N.Dimension.DeferredShape r)
         in
         set_attr_of_data_object setter n#get_name;
         mknode $startpos $endpos (L.Array n#get_name) snds
       }
;

%inline
lparen__deferred_shape_spec_list__rparen:
   | LPAREN ss=clist(deferred_shape_spec) RPAREN { ss }
;

%inline
deferred_shape_spec:
   | COLON { mkleaf $startpos $endpos L.DeferredShapeSpec }
;



codimension_stmt:
   | c=_codimension_stmt stmt_end { c }
;

_codimension_stmt:
   | codimension_stmt_head ds=clist(codimension_decl) 
       { 
         env#exit_name_context;
         mkstmtnode $startpos $endpos Stmt.CodimensionStmt ds
       }
;

codimension_stmt_head:
   | CODIMENSION colon_colon_opt { env#enter_name_context }
;

codimension_decl:
   | n=name s=coarray_spec_part { mknode $startpos $endpos (L.CodimensionDecl n#get_name) [s] }
;

dimension_stmt:
   | d=_dimension_stmt stmt_end { d }
;

_dimension_stmt:
   | slab=dimension_stmt_head colon_colon_opt ds=clist(dimension_array) 
       { 
         env#exit_name_context; 
         mkstmtnode $startpos $endpos slab ds
       }
;

dimension_stmt_head:
   | DIMENSION { env#enter_name_context; Stmt.DimensionStmt }
   | VIRTUAL   { ibm();intel(); env#enter_name_context; Stmt.VirtualStmt }
;

dimension_array:
   | n=name s=array_spec_part 
       { 
         let name = n#get_name in
         let is_deferred =
           match env#lookup_name ~afilt:N.Spec.has_data_object_attr name with
           | [] -> false
           | specs -> 
               List.exists 
                 (fun spec -> 
                   try
                     let a = N.Spec.get_data_object_attr spec in
                     a#is_pointer || a#is_allocatable
                   with
                     Not_found -> false
                 ) specs
         in
         disambiguate_array_spec is_deferred s;
         set_attr_of_data_object (fun attr -> attr#set_dimension (N.Dimension.of_label s#label)) name;
         mknode $startpos $endpos (L.Array n#get_name) [s] 
       }
;

equivalence_stmt:
   | e=_equivalence_stmt stmt_end { e }
;

_equivalence_stmt:
   | EQUIVALENCE es=clist(equivalence_set) { mkstmtnode $startpos $endpos Stmt.EquivalenceStmt es }
;

equivalence_set:
   | LPAREN es=clist(equivalence_object) RPAREN { mknode $startpos $endpos L.EquivalenceSet es }
;

equivalence_object:
(* | n=name               { } contained in var_or_ref *)
   | v=var_or_ref         { disambiguate_equivalence_object v; v }
   | c=constant_substring { c }
;


simple_attr_stmt: (* external protected value volatile contiguous *)
   | s=_simple_attr_stmt stmt_end { s }
;
_simple_attr_stmt:
   | kw=simple_attr_stmt_head ns=clist(name) 
       { 
         env#exit_name_context;
         let handler =
           match String.lowercase_ascii kw with
           | "external" -> begin
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_external) n;
                 nd#relab (L.ExternalName n)
           end
           | "protected" -> begin
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_protected) n;
                 nd#relab (L.EntityName n)
           end
           | "value" -> begin
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_value) n;
                 nd#relab (L.DummyArgName n)
           end
           | "volatile" -> begin
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_volatile) n;
                 nd#relab (L.ObjectName n)
           end
           | "contiguous" -> begin
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_contiguous) n;
                 nd#relab (L.ObjectName n)
           end
           | "automatic" -> begin
               ibm();intel();
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_automatic) n;
                 nd#relab (L.ObjectName n)
           end
           | "static" -> begin
               ibm();intel();
               fun nd ->
                 let n = nd#get_name in
                 set_attr_of_data_object (fun attr -> attr#set_automatic) n;
                 nd#relab (L.ObjectName n)
           end
           | _ -> failwith ("unknown attribute: "^kw)
         in
         List.iter handler ns;
         mkstmtnode $startpos $endpos (Stmt.of_keyword kw) ns 
       }
;
simple_attr_stmt_head:
   | kw=SIMPLE_ATTR colon_colon_opt { env#enter_name_context; kw }
;


intent_stmt:
   | i=_intent_stmt stmt_end { i }
;

_intent_stmt:
   | intent LPAREN s=intent_spec RPAREN colon_colon_opt ns=clist(name) 
       { 
         env#exit_intent_context;
         List.iter
           (set_attr_of_data_object
              (fun attr ->
                attr#set_intent_spec (N.IntentSpec.of_ispec_label s)
              )
           ) (node_list_to_name_list ns);
         List.iter (fun n -> n#relab (L.DummyArgName n#get_name)) ns;
         let snd = mkleaf $startpos $endpos (L.IntentSpec s) in
         mkstmtnode $startpos $endpos Stmt.IntentStmt (snd :: ns)
       }
;

intrinsic_stmt:
   | i=_intrinsic_stmt stmt_end { i }

_intrinsic_stmt:
   | INTRINSIC colon_colon_opt ns=clist(name) 
       { 
         env#exit_name_context; 
         List.iter 
           (fun n -> set_attr_of_data_object (fun attr -> attr#set_intrinsic) n)
           (node_list_to_name_list ns);
         mkstmtnode $startpos $endpos Stmt.IntrinsicStmt ns
       }
;

namelist_stmt:
   | n=_namelist_stmt stmt_end { n }
;

_namelist_stmt:
   | namelist_stmt_head gs=separated_nonempty_list(ioption(COMMA__SLASH), namelist_group) 
       { 
         env#exit_name_context; 
         env#exit_slash_name_context; 
         mkstmtnode $startpos $endpos Stmt.NamelistStmt gs 
       }
;

namelist_group:
   | SLASH n=name SLASH ns=clist(name) 
       { 
         let gn = n#get_name in
         List.iter (fun nm -> nm#relab (L.VariableName nm#get_name)) ns;
         let nd = mknode $startpos $endpos (L.NamelistGroup gn) ns in
         register_namelist_group ~node:nd gn;
         nd
       }
;

namelist_stmt_head:
   | NAMELIST { env#enter_name_context; env#enter_slash_name_context }
;

optional_stmt:
   | o=_optional_stmt stmt_end { o }
;

_optional_stmt:
   | optional_stmt_head ns=clist(name) 
       { 
         env#exit_name_context;
         List.iter
           (set_attr_of_data_object 
              (fun attr -> attr#set_optional)
           ) (node_list_to_name_list ns);
         List.iter (fun n -> n#relab (L.DummyArgName n#get_name)) ns;
         mkstmtnode $startpos $endpos Stmt.OptionalStmt ns 
       }
;

optional_stmt_head:
   | OPTIONAL colon_colon_opt { env#enter_name_context }
;

pointer_stmt:
   | p=_pointer_stmt stmt_end { p }
;

_pointer_stmt:
   | POINTER colon_colon_opt os=clist(pointer_object) { mkstmtnode $startpos $endpos Stmt.PointerStmt os }
   | POINTER l=clist(cray_pointer_spec)               { mkstmtnode $startpos $endpos Stmt.PointerStmt l }
   | POINTER a=apollo_pointer_spec                    { mkstmtnode $startpos $endpos Stmt.PointerStmt [a] }
;

pointee:
   | v=name            { v }
   | v=dimension_array { v }
;

apollo_pointer_spec: 
   | SLASH p=name SLASH vs=clist(pointee) (* Apollo/Domain Fortran *)
       { 
         apollo();
         mknode $startpos $endpos L.ApolloPointerSpec (p::vs) 
       }
;

cray_pointer_spec:
   | LPAREN p=name COMMA v=pointee RPAREN { mknode $startpos $endpos L.CrayPointerSpec [p; v] }
;

%inline
pointer_object:
   | n=name ss_opt=ioption(lparen__deferred_shape_spec_list__rparen) 
       { 
         let snds = list_opt_to_list ss_opt in
         let r = List.length snds in
         let setter attr =
           attr#set_pointer;
           if r > 0 then
             attr#set_dimension (N.Dimension.DeferredShape r)
         in
         set_attr_of_data_object setter n#get_name;
         mknode $startpos $endpos (L.ObjectName n#get_name) snds
       }
;


save_stmt:
   | s=_save_stmt stmt_end { s }
;

_save_stmt:
   | save_stmt_head                        { env#exit_name_context; mkstmtleaf $startpos $endpos Stmt.SaveStmt }
   | save_stmt_head es=clist(saved_entity) 
       { 
         env#exit_name_context;
         List.iter
           (fun nd ->
             match nd#label with
             | L.ObjectName name ->
                 set_attr_of_data_object (fun attr -> attr#set_save) name
             | L.CommonBlockName name ->
                 () (* ??? *)
             | _ -> ()
           ) es;
         mkstmtnode $startpos $endpos Stmt.SaveStmt es 
       }
;

save_stmt_head:
   | SAVE colon_colon_opt { env#enter_name_context }
;

%inline
saved_entity:
   |       n=name       { mkleaf $startpos $endpos (L.ObjectName n#get_name) }
   | SLASH n=name SLASH { mkleaf $startpos $endpos (L.CommonBlockName n#get_name) }
;

target_stmt:
   | t=_target_stmt stmt_end { t }
;

_target_stmt:
   | TARGET colon_colon_opt os=clist(target_object) { mkstmtnode $startpos $endpos Stmt.TargetStmt os }
;

%inline
target_object:
   | n=name s_opt=ioption(array_spec_part) 
       { 
         let setter attr =
           attr#set_target;
           match s_opt with
           | Some a -> attr#set_dimension (N.Dimension.of_label a#label)
           | None -> ()
         in
         set_attr_of_data_object setter n#get_name;
         mknode $startpos $endpos (L.ObjectName n#get_name) (opt_to_list s_opt)
       }
;

asynchronous_stmt: (* F2003 *)
   | a=_asynchronous_stmt stmt_end { f2003(); a }
;

_asynchronous_stmt: (* F2003 *)
   | asynchronous_stmt_head ns=clist(name) 
       { 
         env#exit_name_context;
         List.iter
           (set_attr_of_data_object 
              (fun attr -> attr#set_asynchronous)
           ) (node_list_to_name_list ns);
         List.iter (fun n -> n#relab (L.ObjectName n#get_name)) ns;
         mkstmtnode $startpos $endpos Stmt.AsynchronousStmt ns
       }
;

asynchronous_stmt_head: (* F2003 *)
   | ASYNCHRONOUS colon_colon_opt { env#enter_name_context }
;

bind_stmt: (* F2003 *)
   | b=_bind_stmt stmt_end { f2003(); b }
;

_bind_stmt: (* F2003 *)
   | l=language_binding_spec colon_colon_opt bs=clist(bind_entity) { mkstmtnode $startpos $endpos Stmt.BindStmt (l#children @ bs) }
;

%inline
bind_entity: (* F2003 *)
   | n=name             
       { 
         let n_str = n#get_name in
         set_attr_of_data_object (fun attr -> attr#set_bind) n_str;
         mkleaf $startpos $endpos (L.EntityName n_str) 
       }
   | SLASH n=name SLASH { mkleaf $startpos $endpos (L.CommonBlockName n#get_name) }
;

(* contained in assignment_stmt
stmt_function_stmt: 
   | n=name LPAREN ns=clist0(name) RPAREN EQ e=expr 
       { 
         let n_str = n#get_name in
         let dnds = 
           match ns with
           | [] -> []
           | _ -> 
               List.iter (fun n -> n#relab (L.DummyArgName n#get_name)) ns;
               [mknode $startpos(ns) $endpos(ns) (L.DummyArgNameList n_str) ns]
         in
         mkstmtnode $startpos $endpos (Stmt.StmtFunctionStmt n_str) (dnds @ [e])
       }
;
 *)

data_stmt:
   | d=_data_stmt stmt_end { d }
;

_data_stmt:
   | data_stmt_head ds=coplist(data_stmt_set) 
       { 
         env#exit_data_context;
         env#exit_name_context; 
         context_stack#pop; 
         mkstmtnode $startpos $endpos Stmt.DataStmt ds 
       }
;


data_stmt_head:
   | DATA 
       { 
         env#enter_data_context;
         env#enter_name_context; 
         context_stack#push (C.data_stmt_sets()) 
       }
;

data_stmt_set:
   | ond=data_stmt_object_list SLASH vnd=data_stmt_value_list SLASH
       { 
         mknode $startpos $endpos L.DataStmtSet [ond; vnd]
       }
   | sd=DATA_STMT_SET { let s, d = sd in check_error d; d }
;

%inline
data_stmt_object_list:
   | os=clist(data_stmt_object) { mknode $startpos $endpos L.DataStmtObjectList os }
;

%inline
data_stmt_value_list:
   | vs=clist(data_stmt_value) { mknode $startpos $endpos L.DataStmtValueList vs }
;

data_stmt_object:
   | v=var_or_ref      { disambiguate_variable v; v }
   | d=data_implied_do { d }
;

data_implied_do:
   | LPAREN os=clist(data_i_do_object_OR_variable) EQ e0=expr COMMA e1=expr e2_opt=ioption(comma__expr) RPAREN 
       { 
         let ds, v = Xlist.partition_at_last os in
         List.iter disambiguate_data_i_do_object ds;
         disambiguate_variable v;
         let dnds = 
           try
             let _, ed = node_to_lexposs (Xlist.last ds) in
             [mknode $startpos(os) ed L.DataIDoObjectList ds] 
           with
             Failure _ -> []
         in
         mknode $startpos $endpos L.DataImpliedDo (dnds @ [v; e0; e1] @ (opt_to_list e2_opt))
       }
;


%inline
comma__expr:
   | COMMA e=expr { e }
;

data_i_do_object_OR_variable:
(* | data_ref          { } contained in var_or_ref *)
   | d=data_implied_do { d }
   | v=var_or_ref      { v }
;

data_stmt_value:
   | r_opt=ioption(data_stmt_repeat__star) c=data_stmt_constant
       { 
         mknode $symbolstartpos $endpos L.DataStmtValue ((opt_to_list r_opt) @ [c])
       }
;

data_stmt_repeat__star:
   | r=data_stmt_repeat STAR { disambiguate_named_constant r; r }
;

data_stmt_repeat:
   | v=var_or_ref         { v#relab (L.mkambiguous_data_stmt_constant()); v }
   | c=constant           { mkleaf $startpos $endpos (L.Constant c) }
   | c=constant_substring { c }
;

data_stmt_constant:
   | d=data_stmt_repeat    { disambiguate_data_stmt_constant d; d } (* constant or named constant or constant_substring or structure constructur *)
   | c=complex_literal     { mkleaf $startpos $endpos (L.Constant (Constant.mkcomp c)) }
   | s=sign i=int_literal  { mkleaf $startpos $endpos (L.Constant (Constant.mksint (s^i))) }
   | s=sign r=real_literal { mkleaf $startpos $endpos (L.Constant (Constant.mksreal (s^r))) }
   | n=null_ref            { n }
   | m=PP_MACRO_EXPR       { mkleaf $startpos $endpos (L.Constant (Constant.PpMacroConstant m)) }

(* | s=hollerith_cst       { mkleaf $startpos $endpos (L.Constant (Constant.mkchar s)) } Fortran66 *)
(* | v=var_or_ref          { v#relab (L.mkambiguous_data_stmt_constant()); v } subobject or structure_constructor *)
(* | structure_constructor { } contained in var_or_ref *)
;

%inline
constant_substring:
   | c=constant LPAREN s=substring_range RPAREN { mknode $startpos $endpos L.Substring [(mkleaf $startpos(c) $endpos(c) (L.Constant c)); s] }
;

%inline
sign:
   | PLUS   { "+" }
   | MINUS  { "-" }
   | UPLUS  { "+" }
   | UMINUS { "-" }
;

subprogram_part:
   | c=contains_stmt ss=subprograms
       { 
(*
         context_stack#pop; (* subprogram *)
*)
         mknode $startpos $endpos L.SubprogramPart (c :: ss)
       }
;


contains_stmt:
   | c=_contains_stmt stmt_end { c }
;


_contains_stmt:
   | CONTAINS 
       { 
         (* env#enter_contains_context; *)
         context_stack#push (C.subprograms()); 
         mkstmtleaf $startpos $endpos Stmt.ContainsStmt 
       }
;

subprograms:
   | ss=subprogram* { ss }
;

subprogram:
   | f=function_subprogram   { f }
   | s=subroutine_subprogram { s }
   | sps=SUBPROGRAM          { let sp, s = sps in check_error s; s }
   | p=pp_directive          { p }
   | d=directive             { d }
   | error { parse_error $startpos $endpos "syntax error" }
;

function_subprogram:
   | f=_function_subprogram e=end_function_stmt 
       { 
         reloc $startpos $endpos f;
         f#add_children_r [e];
         let names = Str.split name_sep_pat f#get_name in
         let multi_bind = (List.length names) > 1 in
         List.iter
           (fun x ->
             finalize_object_spec ~multi_bind x f;
           ) names;
         f
       }
;

_function_subprogram:
   | f0=function_subprogram0 sp_opt=ioption(subprogram_part) 
       { 
         (*end_scope();*)
         let f, se, fn = f0 in
         let spl = opt_to_list sp_opt in
         List.iter disambiguate_internal_subprogram spl;
         mknode $startpos $endpos (L.ProgramUnit (ProgramUnit.FunctionSubprogram fn)) ((f :: se) @ spl)
       }
;

function_subprogram0:
   | f=function_stmt se=specification_part__execution_part 
       { 
         f, (Ast.spec_opt_exec_opt_to_list se), f#get_name
       }
   | spffn=function_head se=specification_part__execution_part 
       { 
         let sp, f, fn = spffn in
         f, (Ast.spec_opt_exec_opt_to_list se), fn
       }
;


function_head0:
   | spf=FUNCTION_HEAD ioption(END_FRAGMENT) { spf }
;

function_head:
   | spfs=function_head0+
       { 
         let fnames = Xset.create 0 in
         let len = ref 0 in

         List.iter
           (fun spf ->
             let sp, f = spf in
             len := !len + (Ast.Partial.length_of_spec sp);
             Ast.visit 
               (fun nd ->
                 match nd#label with
                 | L.Stmt stmt -> 
                     if Stmt.is_function_stmt stmt then
                       Xset.add fnames (Stmt.get_name stmt)
                 | _ -> ()
               ) f
           ) spfs;

         if env#at_BOPU then 
           env#clear_BOPU;

         if env#in_contains_context || env#in_interface_context then begin
           if not env#partial_parsing_flag then
             context_stack#push (C.spec__exec()); (* already pushed for head-less main_program *)
         end
         else begin
           cancel_main_program_scope()
         end;

         let fname_list = Xset.to_list fnames in

         (*List.iter register_function fname_list;*)(* handled elsewhere *)

         let fname = Xlist.to_string (fun x -> x) ";" fname_list in

         begin_function_subprogram_scope fname;

         let spec, nd =
           match spfs with
           | [spf] -> spf
           | _ ->
               let children = 
                 List.flatten
                   (List.map 
                      (fun (_, nd) ->
                        match nd#label with
                        | L.PpBranchFunction -> nd#children
                        | _ -> [nd]
                      ) spfs
                   )
               in
               let lloc = Ast.lloc_of_nodes children in
               let nd = new Ast.node ~lloc ~children L.PpBranchFunction in
               let sp = Ast.Partial.mkspec ~length:!len () in
               sp, nd
         in
         spec, nd, fname
       }
;

function_stmt:
   | f=_function_stmt stmt_end { f }
;

_function_stmt:
   | x=function_stmt_head RPAREN r_opt=ioption(suffix) 
       { 
         env#exit_result_context;
         env#exit_pu_head_context;
         let n_str, pnds, params = x in
         mkstmtnode $symbolstartpos $endpos (Stmt.FunctionStmt n_str) (pnds @ params @ (opt_to_list r_opt))
       }
;

suffix:
   | r=result l_opt=ioption(language_binding_spec) { mknode $startpos $endpos L.Suffix (r :: (opt_to_list l_opt)) }
   | l=language_binding_spec r_opt=ioption(result) { mknode $startpos $endpos L.Suffix (l :: (opt_to_list r_opt)) }
;
(*
%inline
name_or_macro_var:
   | n=name              { n#get_name }
   | n=PP_MACRO_VARIABLE { n }
;
*)
function_stmt_head:
   | fshd=function_stmt_head0 LPAREN ns=clist0(name)
       { 
         let pnds, n_str = fshd in

         env#enter_result_context;

         if env#at_BOPU then 
           env#clear_BOPU;

         if env#in_contains_context || env#in_interface_context then begin
           if not env#partial_parsing_flag then
             context_stack#push (C.spec__exec()); (* already pushed for head-less main_program *)
         end
         else begin
           cancel_main_program_scope()
         end;

         List.iter
           (fun x ->
             register_function x
           ) (Str.split name_sep_pat n_str);

         begin_function_subprogram_scope n_str;

         let params =
           match ns with
           | [] -> []
           | _ -> 
               List.iter (fun n -> n#relab (L.DummyArgName n#get_name)) ns;
               [mknode $startpos(ns) $endpos(ns) (L.DummyArgNameList n_str) ns]
         in
         n_str, pnds, params
       }
;

function_stmt_head0:
   | ps=prefix FUNCTION n_str=ident_or_macro_var
       { 
         let pnds =
           match ps with
           | [] -> []
           | _ -> [mknode $startpos(ps) $endpos(ps) L.Prefix ps]
         in
         pnds, n_str
       }
   | ps=prefix FUNCTION re=PP_MACRO_NAME
       { 
         let n_str, expanded = re in
         let pnds =
           match ps with
           | [] -> []
           | _ -> [mknode $startpos(ps) $endpos(ps) L.Prefix ps]
         in
         pnds, n_str^(if expanded <> "" then ";"^expanded else "")
       }
   | spf=FUNCTION_STMT_HEAD
       { 
         let sp, f = spf in
         let n =
           try
             f#get_name
           with
             Not_found ->
               let l = ref [] in
               let rec scan nd =
                 if L.is_function_stmt_head nd#label then
                   l := nd#get_name :: !l
                 else
                   List.iter scan nd#children
               in
               scan f;
               String.concat ";" (List.rev !l)
         in
         [f], n
       }
;

%inline
prefix:
   |           { [] }
   | p=_prefix { p }
;
_prefix:
   | ps0=prefix_spec_no_type_spec+ t=decl_type_spec ps1=prefix_spec_no_type_spec* { env#exit_type_spec_context; ps0 @ (t :: ps1) }
   |                               t=decl_type_spec ps1=prefix_spec_no_type_spec* { env#exit_type_spec_context; (t :: ps1) }
   | ps=prefix_spec_no_type_spec+                                                 { ps }
;

(*
prefix_spec:
   | t=decl_type_spec           { env#exit_type_spec_context; t#relab (L.type_spec_to_prefix_spec t#label); t }
   | p=prefix_spec_no_type_spec { p }
;
*)

prefix_spec_no_type_spec:
(*
   | RECURSIVE { mkleaf $startpos $endpos (L.PrefixSpec PrefixSpec.Recursive) }
   | PURE      { mkleaf $startpos $endpos (L.PrefixSpec PrefixSpec.Pure) }
   | ELEMENTAL { mkleaf $startpos $endpos (L.PrefixSpec PrefixSpec.Elemental) }
   | IMPURE    { f2008(); mkleaf $startpos $endpos (L.PrefixSpec PrefixSpec.Impure) }
*)
   | kw=PREFIX_SPEC 
       { 
         begin
           match String.lowercase_ascii kw with
           | "impure" -> f2008()
           | _ -> ()
         end;
         mkleaf $startpos $endpos (L.PrefixSpec (PrefixSpec.of_keyword kw)) 
       }
   | MODULE { f2008(); mkleaf $startpos $endpos (L.PrefixSpec PrefixSpec.Module) }

   | kw=PREFIX_SPEC LPAREN n=name RPAREN
       { 
         begin
           match String.lowercase_ascii kw with
           | "attributes" -> pgi_cuda()
           | _ -> ()
         end;
         mkleaf $startpos $endpos (L.PrefixSpec (PrefixSpec.of_keyword_name kw n#get_name))
       }
;

%inline
end_function_stmt:
   | e=end_stmt                    { e#relab_stmt (Stmt.EndFunctionStmt None); e }
   | e=_end_function_stmt stmt_end { e }
   | e=PU_TAIL                     { get_nd e }
;

_end_function_stmt:
   | end_function_stmt_head n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.EndFunctionStmt (node_opt_to_name_opt n_opt)) }
;

end_function_stmt_head:
   | END_FUNCTION { mark_EOPU() }
;

subroutine_subprogram:
   | s=_subroutine_subprogram e=end_subroutine_stmt 
       { 
         reloc $startpos $endpos s;
         s#add_children_r [e];
         let names = Str.split name_sep_pat s#get_name in
         let multi_bind = (List.length names) > 1 in
         List.iter
           (fun x ->
             finalize_object_spec ~multi_bind x s;
           ) names;
         s
       }
;

_subroutine_subprogram:
   | s0=subroutine_subprogram0 sp_opt=ioption(subprogram_part) 
       { 
         (*end_scope();*)
         let s, se, sn = s0 in
         let spl = opt_to_list sp_opt in
         List.iter disambiguate_internal_subprogram spl;
         mknode $startpos $endpos (L.ProgramUnit (ProgramUnit.SubroutineSubprogram sn)) (s :: se @ spl)
       } 
;
subroutine_subprogram0:
   | s=subroutine_stmt se=specification_part__execution_part 
       { 
         s, (Ast.spec_opt_exec_opt_to_list se), s#get_name
       }
   | spssn=subroutine_head se=specification_part__execution_part 
       { 
         let sp, s, sn = spssn in
         s, (Ast.spec_opt_exec_opt_to_list se), sn
       }
;

subroutine_head0:
   | sps=SUBROUTINE_HEAD ioption(END_FRAGMENT) { sps }
;

subroutine_head:
   | spss=subroutine_head0+ 
       { 
         let snames = Xset.create 0 in
         let len = ref 0 in

         List.iter
           (fun sps ->
             let sp, s = sps in
             len := !len + (Ast.Partial.length_of_spec sp);
             Ast.visit 
               (fun nd ->
                 match nd#label with
                 | L.Stmt stmt -> 
                     if Stmt.is_subroutine_stmt stmt then
                       Xset.add snames (Stmt.get_name stmt)
                 | _ -> ()
               ) s
           ) spss;

         if env#at_BOPU then 
           env#clear_BOPU;

         if env#in_contains_context || env#in_interface_context then begin
           context_stack#push (C.spec__exec()); (* already pushed for head-less main_program *)
         end
         else begin
           cancel_main_program_scope()
         end;

         let sname_list = Xset.to_list snames in

         (*List.iter register_subroutine sname_list;*)(* handled elsewhere *)

         let sname = Xlist.to_string (fun x -> x) ";" sname_list in

         begin_subroutine_subprogram_scope sname;

         let spec, nd =
           match spss with
           | [sps] -> sps
           | _ ->
               let children = 
                 List.flatten
                   (List.map 
                      (fun (_, nd) ->
                        match nd#label with
                        | L.PpBranchSubroutine -> nd#children
                        | _ -> [nd]
                      ) spss
                   )
               in
               let lloc = Ast.lloc_of_nodes children in
               let nd = new Ast.node ~lloc ~children L.PpBranchSubroutine in
               let sp = Ast.Partial.mkspec ~length:!len () in
               sp, nd
         in
         spec, nd, sname
       }
;


subroutine_stmt:
   | s=_subroutine_stmt stmt_end { s }
;

_subroutine_stmt:
   | sshd=subroutine_stmt_head das_opt=ioption(subroutine_das) l_opt=ioption(language_binding_spec)
       { 
         let pnds, n_str = sshd in

         env#exit_pu_head_context;

         (*let n_str = n#get_name in*)

         if env#at_BOPU then
           env#clear_BOPU;

         if env#in_contains_context || env#in_interface_context then begin
           if not env#partial_parsing_flag then
             context_stack#push (C.spec__exec()); (* already pushed for head-less main program *)
         end
         else begin
           cancel_main_program_scope()
         end;

         List.iter
           (fun x ->
             register_subroutine x
           ) (Str.split name_sep_pat n_str);

         begin_subroutine_subprogram_scope n_str;

         let dnds =
           match das_opt with
           | Some (_, _, []) -> []
           | Some (st, ed, das) -> [mknode st ed (L.DummyArgList n_str) das]
           | None -> []
         in
         mkstmtnode $symbolstartpos $endpos (Stmt.SubroutineStmt n_str) (pnds @ dnds @ (opt_to_list l_opt))
       }
;

%inline
ident_or_macro_var:
   | i=IDENTIFIER        { i }
   | n=PP_MACRO_VARIABLE { n }
;

subroutine_stmt_head:
   | ps=prefix SUBROUTINE n_str=ident_or_macro_var
       { 
         let pnds =
           match ps with
           | [] -> []
           | _ -> [mknode $startpos(ps) $endpos(ps) L.Prefix ps]
         in
         pnds, n_str
       }
   | ps=prefix SUBROUTINE re=PP_MACRO_NAME
       { 
         let n_str, expanded = re in
         let pnds =
           match ps with
           | [] -> []
           | _ -> [mknode $startpos(ps) $endpos(ps) L.Prefix ps]
         in
         pnds, n_str^(if expanded <> "" then ";"^expanded else "")
       }
   | sps=SUBROUTINE_STMT_HEAD
       { 
         let sp, s = sps in
         let n =
           try
             s#get_name
           with
             Not_found ->
               let l = ref [] in
               let rec scan nd =
                 if L.is_subroutine_stmt_head nd#label then
                   l := nd#get_name :: !l
                 else
                   List.iter scan nd#children
               in
               scan s;
               String.concat ";" (List.rev !l)
         in
         [s], n
       }
;

%inline
subroutine_das:
   | LPAREN das=clist0(dummy_arg) RPAREN { $startpos(das), $endpos(das), das }
;

%inline
end_subroutine_stmt:
   | e=end_stmt                      { e#relab_stmt (Stmt.EndSubroutineStmt None); e }
   | e=_end_subroutine_stmt stmt_end { e }
   | e=PU_TAIL                       { get_nd e }
;

_end_subroutine_stmt:
   | end_subroutine_stmt_head n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.EndSubroutineStmt (node_opt_to_name_opt n_opt)) }
;

end_subroutine_stmt_head:
   | END_SUBROUTINE { mark_EOPU() }
;

%inline
end_program_stmt:
   | e=end_stmt                   { e#relab_stmt (Stmt.EndProgramStmt None); e }
   | e=_end_program_stmt stmt_end { e }
   | e=PU_TAIL                    { get_nd e }
;

_end_program_stmt:
   | end_program_stmt_head n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.EndProgramStmt (node_opt_to_name_opt n_opt)) }
;

end_program_stmt_head:
   | END_PROGRAM { mark_EOPU() }
;

end_stmt:
   | e=_end_stmt stmt_end { e }
;

_end_stmt:
   | END { mark_EOPU(); mkstmtleaf $startpos $endpos (Stmt.EndStmt) }
;

if_then_stmt:
   | i=_if_then_stmt stmt_end { i }
;

_if_then_stmt:
   | n_opt=ioption(construct_name__colon) e=if__lparen__expr__rparen THEN 
         { 
           context_stack#pop; (* action_stmt *)
(*           context_stack#push (C.execution_part()); *)
           mkstmtnode $symbolstartpos $endpos (Stmt.IfThenStmt n_opt) [e]
         }
;

else_if_stmt:
   | e=_else_if_stmt stmt_end { e }
;

_else_if_stmt:
   | e=else_if_stmt_head RPAREN THEN n_opt=ioption(name)
       { 
(*         context_stack#push (C.execution_part()); *)
         mkstmtnode $startpos $endpos (Stmt.ElseIfStmt (node_opt_to_name_opt n_opt)) [e]
       }
;

else_if_stmt_head:
   | else_if LPAREN e=expr { env#exit_name_context; e }
;

else_if:
   | ELSE_IF { env#enter_name_context }
;

else_stmt:
   | e=_else_stmt stmt_end { e }
;

_else_stmt:
   | ELSE n_opt=ioption(name) 
       { 
(*         context_stack#push (C.execution_part()); *)
         mkstmtleaf $startpos $endpos (Stmt.ElseStmt (node_opt_to_name_opt n_opt)) 
       }
;

end_if_stmt:
   | e=_end_if_stmt stmt_end { e }
   | e=END_IF_STMT           { get_nd e }
;

_end_if_stmt:
   | END_IF n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.EndIfStmt (node_opt_to_name_opt n_opt)) }
;
(*
if_stmt:
   | i=_if_stmt stmt_end { i }
;
*)
_if_stmt:
   | e=if__lparen__expr__rparen a=_action_stmt 
         { 
           context_stack#pop; (* action_stmt *)
           mkstmtnode $startpos $endpos Stmt.IfStmt [e; a] 
         }
   | e=if__lparen__expr__rparen c=var_or_ref
         { 
           context_stack#pop; (* action_stmt *)
           disambiguate_linda_call c;
           mkstmtnode $startpos $endpos Stmt.IfStmt [e; c] 
         }
;

if_:
   | IF 
       { 
         env#enter_if_context; 
         env#enter_name_context;
         context_stack#push (C.expr());
       }
;

if__lparen__expr__rparen:
   | e=if__lparen__expr RPAREN { env#exit_if_context; e }
;

if__lparen__expr:
   | if_ LPAREN e=expr 
         { 
           env#exit_name_context; 
           context_stack#pop; (* expr *)
           context_stack#push (C.action_stmt()); 
           e 
         }
;


case_construct:
   | s=select_case_stmt cbs_opt=ioption(case_block+) e=end_select_stmt 
       { 
         context_stack#pop;
         mknode $startpos $endpos L.CaseConstruct ((s :: (list_opt_to_list cbs_opt)) @ [e])  
       }
   | sps=SELECT_CASE_STMT cbs_opt=ioption(case_block+) e=end_select_stmt 
       { 
         let sp, s = sps in
         mknode $startpos $endpos L.CaseConstruct ((s :: (list_opt_to_list cbs_opt)) @ [e])  
       }
;

case_block:
   | cb=case_stmt__block { cb }
   | spc=CASE_BLOCK      { let sp, c = spc in c }
;

select_case_stmt:
   | s=_select_case_stmt stmt_end { s }
;

_select_case_stmt:
   | n_opt=ioption(construct_name__colon) SELECT_CASE LPAREN e=expr RPAREN
       { 
         context_stack#push (C.case_block());
         mkstmtnode $symbolstartpos $endpos (Stmt.SelectCaseStmt n_opt) [e]
       }
;

case_stmt:
   | c=_case_stmt stmt_end { c }
;

_case_stmt:
   | CASE c=case_selector n_opt=ioption(name) 
       { 
         context_stack#push (C.execution_part());
         mkstmtnode $startpos $endpos (Stmt.CaseStmt (node_opt_to_name_opt n_opt)) [c] 
       }
;


case_stmt__block:
   | c=case_stmt b=block 
       { 
         context_stack#pop; (* execution_part *)
         mknode $startpos $endpos L.CaseBlock (c :: b)
       }
;

end_select_stmt:
   | e=_end_select_stmt stmt_end { e }
   | e=END_SELECT_STMT           { get_nd e }
;

_end_select_stmt:
   | END_SELECT n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.EndSelectStmt (node_opt_to_name_opt n_opt)) }
;

case_selector:
   | LPAREN rs=clist(case_value_range) RPAREN { mknode $startpos $endpos (L.CaseSelector CaseSelector.CaseValueRangeList) rs }
   | DEFAULT                                  { mkleaf $startpos $endpos (L.CaseSelector CaseSelector.Default) }
;

case_value_range:
   | e=expr                { mknode $startpos $endpos (L.CaseValueRange CaseValueRange.Value) [e] }
   | e=expr COLON          { mknode $startpos $endpos (L.CaseValueRange CaseValueRange.Lower) [e] }
   | COLON e=expr          { mknode $startpos $endpos (L.CaseValueRange CaseValueRange.Upper) [e] }
   | e0=expr COLON e1=expr { mknode $startpos $endpos (L.CaseValueRange CaseValueRange.LowerUpper) [e0; e1] }
;

forall_construct:
   | f=forall_construct_stmt ss=forall_body_construct* e=end_forall_stmt { mknode $startpos $endpos L.ForallConstruct ((f :: ss) @ [e]) }
   | sf=FORALL_CONSTRUCT_STMT ss=forall_body_construct* e=end_forall_stmt 
       { 
         let s, f = sf in
         mknode $startpos $endpos L.ForallConstruct ((f :: ss) @ [e]) 
       }
;

forall_construct_stmt:
   | f=_forall_construct_stmt stmt_end { f }
;

_forall_construct_stmt:
   | n_opt=ioption(construct_name__colon) FORALL f=forall_header
       { 
         mkstmtnode $symbolstartpos $endpos (Stmt.ForallConstructStmt n_opt) [f]
       }
;

forall_header:
   | LPAREN ss=clist(forall_triplet_spec_OR_expr) RPAREN 
       { mknode $startpos $endpos L.ForallHeader ss }
;

forall_triplet_spec_OR_expr:
   | n=name EQ e0=expr COLON e1=expr e2_opt=ioption(colon__expr) 
       { mknode $startpos $endpos (L.ForallTripletSpec n#get_name) ([e0; e1] @ (opt_to_list e2_opt)) }
   | e=expr { e }
;


forall_body_construct:
   | f=forall_assignment_stmt { f }
   | w=where_stmt             { w }
   | w=where_construct        { w }
   | w=forall_construct       { w }
   | w=forall_stmt            { w }

   | error { parse_error $startpos $endpos "syntax error" }
;

forall_assignment_stmt:
   | f=_forall_assignment_stmt stmt_end { f }
(*
   | a=assignment_stmt         { a }
   | p=pointer_assignment_stmt { p }
*)
;

_forall_assignment_stmt:
   | a=_assignment_stmt         { a }
   | p=_pointer_assignment_stmt { p }
;
   
(*
forall_stmt:
   | FORALL f=forall_header a=forall_assignment_stmt { mkstmtnode $startpos $endpos Stmt.ForallStmt [f; a] }
;
*)

forall_stmt:
   | f=_forall_stmt stmt_end { f }
;

_forall_stmt:
   | FORALL f=forall_header a=_forall_assignment_stmt { mkstmtnode $startpos $endpos Stmt.ForallStmt [f; a] }
;

end_forall_stmt:
   | e=_end_forall_stmt stmt_end { e }
   | e=END_FORALL_STMT           { get_nd e }
;

_end_forall_stmt:
   | END_FORALL n_opt=ioption(name) { mkstmtleaf $startpos $endpos (Stmt.EndForallStmt (node_opt_to_name_opt n_opt)) }
;

where_construct:
   | w=where_construct_head ews=masked_or_unmasked_elsewhere_block* e=end_where_stmt 
       { 
         mknode $startpos $endpos L.WhereConstruct (w :: ews @ [e]) 
       }
;

where_construct_head:
   | w=where_construct_stmt bs=where_body_construct*  { mknode $startpos $endpos L.WhereBlock (w :: bs) }
   | sw=WHERE_CONSTRUCT_STMT bs=where_body_construct* { let _, w = sw in mknode $startpos $endpos L.WhereBlock (w :: bs) }
;

where_construct_stmt:
   | w=_where_construct_stmt stmt_end { w }
;

_where_construct_stmt:
   | n_opt=ioption(construct_name__colon) WHERE LPAREN e=expr RPAREN
       { 
         mkstmtnode $symbolstartpos $endpos (Stmt.WhereConstructStmt n_opt) [e]
       }
;

where_body_construct:
   | a=assignment_stmt { a }
   | w=where_stmt      { w }
   | w=where_construct { w }

   | error { parse_error $startpos $endpos "syntax error" }
;


%inline
masked_or_unmasked_elsewhere_block:
   | m=masked_or_unmasked_elsewhere_stmt bs=where_body_construct* { mknode $startpos $endpos L.WhereBlock (m :: bs) }
;

masked_or_unmasked_elsewhere_stmt:
   | m=_masked_or_unmasked_elsewhere_stmt stmt_end { m }
;

_masked_or_unmasked_elsewhere_stmt:
   | ELSEWHERE m_opt=ioption(mask) n_opt=ioption(name) 
       { 
         mkstmtnode $startpos $endpos (Stmt.ElsewhereStmt (node_opt_to_name_opt n_opt)) (opt_to_list m_opt) 
       }
;

%inline
mask:
   | LPAREN e=expr RPAREN { e }
;

end_where_stmt:
   | e=_end_where_stmt stmt_end { e }
   | e=END_WHERE_STMT           { get_nd e }
;

_end_where_stmt:
   | END_WHERE n_opt=ioption(name) 
       { 
         let nm_opt = node_opt_to_name_opt n_opt in
         mkstmtleaf $startpos $endpos (Stmt.EndWhereStmt nm_opt) 
       }
;


%%
