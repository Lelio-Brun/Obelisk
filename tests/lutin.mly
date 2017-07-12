%{
(** SYNTAXE : analyse syntaxique

----------------------------------------------------------------*)
open Lexing
open Lexeme
open Syntaxe
open Errors
open Format
open Str


(* init parse *)

let lettab = ref (Hashtbl.create 50)
let nodetab = ref (Hashtbl.create 50)
let excepttab = ref (Hashtbl.create 50)
let deflist    = ref []

let add_info
   (htbl   : (string, 'a ) Hashtbl.t)
   (id  : Syntaxe.ident)   (* le lexeme en question        *)
   (info : 'a)         (* l'info en question           *)
= (
   try
      Some (Hashtbl.find htbl id.it)
   with Not_found -> (
      Hashtbl.add htbl id.it info ;
		None
   )
)


let add_except (idl : ident list) = (
	let f id = (
		match (add_info !excepttab id id) with
		None -> deflist := (ExceptDef id)  :: !deflist
		| Some ei -> raise (
			Compile_error ( id.src ,
				(sprintf "bad exception declaration, ident already linked %s"
					(Errors.lexeme_line_col ei.src)))
			)
	) in
	List.iter f idl
)

(* N.B. let et extern sont dans le meme espace de nom
	le seule différence est qu'on tient une liste à part des externes
*)
let add_extern (id : Syntaxe.ident) (ci : let_info) = (
	match ( add_info !lettab id ci) with
	None -> deflist := (ExternDef id) :: !deflist
	| Some mi -> raise (
		Compile_error ( id.src ,
			(sprintf "bad macro declaration, ident already linked %s"
				(Errors.lexeme_line_col mi.lti_ident.src))
      )
	)
)

let add_letdef (id : Syntaxe.ident) (ci : let_info) = (
	match ( add_info !lettab id ci) with
	None -> deflist := (LetDef id) :: !deflist
	| Some mi -> raise (
		Compile_error ( id.src ,
			(sprintf "bad macro declaration, ident already linked %s"
				(Errors.lexeme_line_col mi.lti_ident.src))
      )
	)
)

let add_node (id : Syntaxe.ident) (ci : node_info) = (
	match ( add_info !nodetab id ci ) with
	None -> deflist := (NodeDef id) :: !deflist
	| Some ni -> raise (
		Compile_error ( id.src ,
			(sprintf "bad node declaration, ident already linked %s"
				(Errors.lexeme_line_col ni.ndi_ident.src))
      )
	)
)

let distrib_type idlst texp = (
	let attach_type id = (id, texp) in
	List.map attach_type idlst
)

let distrib_type_and_init idlst texp vexp = (
	let attach_type id = (id, texp, vexp) in
	List.map attach_type idlst
)

let parse_end () = (
   let res = {
      pck_lettab = !lettab;
      pck_nodetab  = !nodetab;
      pck_excepttab  = !excepttab;
      pck_deflist  = (List.rev !deflist)
   } in
   lettab := Hashtbl.create 50;
   nodetab := Hashtbl.create 50;
   deflist := [];
   res
)

let ident_of_token lxm = (
	Lexeme.flagit lxm.str lxm
)

let make_val_exp ven lxm = {
	it = ven;
	src = lxm
}


%}

%token TK_EOF

%token <Lexeme.t> TK_ERROR

%token <Lexeme.t> TK_IDENT

%token <Lexeme.t> TK_LET
%token <Lexeme.t> TK_IN
%token <Lexeme.t> TK_EXTERN
%token <Lexeme.t> TK_NODE
%token <Lexeme.t> TK_SYSTEM
%token <Lexeme.t> TK_RETURNS

%token <Lexeme.t> TK_EXIST
%token <Lexeme.t> TK_ASSERT
%token <Lexeme.t> TK_RAISE
%token <Lexeme.t> TK_TRY
%token <Lexeme.t> TK_CATCH
%token <Lexeme.t> TK_TRAP
%token <Lexeme.t> TK_PARA
%token <Lexeme.t> TK_DO
%token <Lexeme.t> TK_FBY
%token <Lexeme.t> TK_LOOP
%token <Lexeme.t> TK_WEIGHT

%token <Lexeme.t> TK_TYPE
%token <Lexeme.t> TK_BOOL
%token <Lexeme.t> TK_INT
%token <Lexeme.t> TK_REAL
%token <Lexeme.t> TK_TRACE
%token <Lexeme.t> TK_REF
%token <Lexeme.t> TK_EXCEPTION

%token <Lexeme.t> TK_PRE
%token <Lexeme.t> TK_FALSE
%token <Lexeme.t> TK_TRUE

%token <Lexeme.t> TK_RCONST
%token <Lexeme.t> TK_ICONST

%token <Lexeme.t> TK_EQ
%token <Lexeme.t> TK_NEQ

%token <Lexeme.t> TK_BARSUP

%token <Lexeme.t> TK_PLUS
%token <Lexeme.t> TK_MINUS
%token <Lexeme.t> TK_TIMES
%token <Lexeme.t> TK_SLASH

%token <Lexeme.t> TK_DIV
%token <Lexeme.t> TK_MOD

%token <Lexeme.t> TK_LT
%token <Lexeme.t> TK_LTE
%token <Lexeme.t> TK_GT
%token <Lexeme.t> TK_GTE

%token <Lexeme.t> TK_BAR

%token <Lexeme.t> TK_DOT
%token <Lexeme.t> TK_COMA
%token <Lexeme.t> TK_SEMICOL
%token <Lexeme.t> TK_COLON
%token <Lexeme.t> TK_TILDA
%token <Lexeme.t> TK_OPEN_BRACE
%token <Lexeme.t> TK_CLOSE_BRACE
%token <Lexeme.t> TK_OPEN_BRACKET
%token <Lexeme.t> TK_CLOSE_BRACKET
%token <Lexeme.t> TK_OPEN_PAR
%token <Lexeme.t> TK_CLOSE_PAR

%token <Lexeme.t> TK_NOT
%token <Lexeme.t> TK_OR
%token <Lexeme.t> TK_XOR
%token <Lexeme.t> TK_AND
%token <Lexeme.t> TK_IMPL
%token <Lexeme.t> TK_ARROW
%token <Lexeme.t> TK_IF
%token <Lexeme.t> TK_THEN
%token <Lexeme.t> TK_ELSE


/* PRIORITIES */

/* dans les traces (statements) */
%nonassoc TK_TRY TK_IN
%left NO_DO_PART
%nonassoc TK_DO
%left TK_FBY
%nonassoc TK_LOOP


/* dans les expressions */
%left TK_ELSE
%left TK_OR TK_XOR
%left TK_AND
%nonassoc TK_LT TK_LTE TK_EQ TK_GTE TK_GT TK_NEQ TK_IMPL
%nonassoc TK_NOT
%left TK_PLUS TK_MINUS
%left TK_TIMES TK_SLASH TK_PCENT TK_MOD TK_DIV
%left TK_WHEN
%nonassoc TK_INT TK_REAL
%nonassoc TK_UMINUS TK_PRE TK_CURRENT TK_DIESE TK_NOR
%left TK_HAT TK_FIELD TK_DOT

/* %nonassoc TK_OPEN_PAR */

/* affreux hack pour traiter le
   problème de loop ~id ( ...
	On met des priorités de telle
	manière que chaque fois qu'on a
	un '(' qui suit un id, on considère
	ça comme un (début) de call
*/
%nonassoc HACK_CALL
%nonassoc HACK_ID
%nonassoc TK_OPEN_PAR

/* %nonassoc TK_TILDA TK_COLON */

/* Entry point
*/
%start File
%type <Syntaxe.package> File


%%

/*-------------------------------------------------------
	GRAMMAR
---------------------------------------------------------
-------------------------------------------------------*/

/* ebnf:group=decls */

File:
	|   DeclList
			{ parse_end () }
	;

/* Declarations */

DeclList:
			{ }
	|   DeclList OneDecl
			{ }
	;

OneDecl:
		LetDecl
		{ add_letdef (fst $1) (snd $1) }
	|	ExternDecl
		{ add_extern (fst $1) (snd $1) }
	|	ExceptDecl
		{ add_except $1 }
	|	NodeDecl
		{ add_node (fst $1) (snd $1) }
	;

ExceptDecl:
	TK_EXCEPTION IdentList
			{ List.rev $2 }
	;

/* top level macro def */
LetDecl:
  TK_LET TK_IDENT OptParams OptType TK_EQ Statement
  {
	  let id = ident_of_token $2 in
	  ( id,
		  {
			  lti_ident = id;
			  lti_inputs = $3;
			  lti_type = $4;
			  lti_def = Some $6;
		  }
	  )
  }
;

/* top level extern def */
ExternDecl:
  TK_EXTERN TK_IDENT OptParams OptType
  {
	  let id = ident_of_token $2 in
	  ( id,
		  {
			  lti_ident = id;
			  lti_inputs = $3;
			  lti_type = $4;
			  lti_def = None;
		  }
	  )
  }
;

/* top level node def */

NodeStart: /* ebnf:print=short */
	TK_NODE
		{}
|	TK_SYSTEM
		{}
;

NodeDecl:

	NodeStart TK_IDENT
	TK_OPEN_PAR TypedIdentListOpt TK_CLOSE_PAR
	TK_RETURNS TK_OPEN_PAR TypedIdentList TK_CLOSE_PAR
	TK_EQ Statement
	{
		let id = ident_of_token $2 in
		(id,
			{
				ndi_ident = id;
				ndi_inputs = List.rev $4;
				ndi_outputs = List.rev $8;
				ndi_def = $11
			}
		)
	}
	;



/* Identifiers and lists */

/* ebnf:group=varparams */

Ident: /* ebnf:print=ignore */
	TK_IDENT
		{ ident_of_token $1 }
	;

IdentList:
		Ident
			{ [$1] }
	|   IdentList TK_COMA Ident
			{ $3::$1 }
	;


TypedIdent:
	IdentList TK_COLON Type
		{ distrib_type_and_init $1 $3 None }
|	IdentList TK_COLON Type  TK_EQ Exp
		{ distrib_type_and_init $1 $3 (Some $5) }
	;

TypedIdentListOpt: /* nada */
			{ [] }
	|	TypedIdentList
			{ $1 }
	;


TypedIdentList: TypedIdentListA
			{ $1 }
	|   TypedIdentListA TK_SEMICOL
			{ $1 }
	;

TypedIdentListA: TypedIdent
			{ $1 }
	|   TypedIdentListA TK_SEMICOL TypedIdent
			{ $3@$1 }
	;

/*
TypedIdentList: TypedIdent
			{ $1 }
	|   TypedIdentList TK_SEMICOL TypedIdent OptSemicol
			{ $3@$1 }
	;

OptSemicol:
		{}
	| TK_SEMICOL
		{}
	;
*/
OptParams:
	  /* nada */
	  { None }
| TK_OPEN_PAR TK_CLOSE_PAR
	  { Some [] }
| TK_OPEN_PAR TypedParamList TK_CLOSE_PAR
		{ Some (List.rev $2) }
;

TypedParamList: TypedParam
	{ $1 }
|   TypedParamList TK_SEMICOL TypedParam
	{ $3@$1 }
;

TypedParam:
IdentList TK_COLON ParamType
	{ distrib_type $1 $3 }
;

OptType:
/* nada */
	{ None }
|	TK_COLON Type
	{ Some $2 }
;




/* Immediate type */

/* ebnf:group=types */

Type:
	PredefType
		{TEXP_predef $1}
|	TK_TRACE
		{ (TEXP_trace) }
;

PredefType:
  TK_BOOL
	  { Bool }
|  TK_INT
	  { Int }
|  TK_REAL
	  { Real }
;

/* Parameter type */
ParamType:
	Type
		{$1}
|	PredefType TK_REF
		{TEXP_ref $1}
;
/*

FunctionType:
		FunctionInType TK_ARROW PredefType
		{ (List.rev $1, TEXP_predef $3) }
	|	TK_OPEN_PAR TK_CLOSE_PAR TK_ARROW PredefType
		{ ([], TEXP_predef $4) }
;
FunctionInType:
		PredefType
		{ [TEXP_predef $1] }
	|  FunctionInType TK_TIMES PredefType
		{ (TEXP_predef $3)::$1 }
;
*/


/* Statement :
- Statement = a priori un statement, c'est-à-dire
  une expression parenthésée en {}
- Exp = une expression algébrique classique
  parenthésée en ()
- cas particulier : les idents et plus généralement
  les "call" sont traités dans Exp
  (voir + bas)
*/

/* ebnf:group=statements */

Statement:
/* ``feuilles''
*/
		Exp
			{ $1 }
	|	TK_RAISE Ident
			{ make_val_exp (RAISE_n $2) $1 }
/* les combinateurs de traces referment dès que possible, e.g.:
	loop x fby y   <-->  {loop {x}} fby y
*/
	|	Statement TK_FBY Statement
			{ make_val_exp (FBY_n ($1,$3)) $2}
	|	LoopStatement
			{ $1 }
/*
	les combinateurs entre accolades
*/
	|	BracedStatement
			{ $1 }
/*
	en général les statement en "in" referment AU PLUS TARD, e.g.
	assert e in x fby y fby
*/
	|	LetDecl TK_IN Statement
			{ make_val_exp (LET_n (snd $1,$3)) ((fst $1).src) }
	|	TK_ASSERT Exp TK_IN Statement
			{ make_val_exp (ASSERT_n ($2,$4)) $1 }
	|	TK_EXIST TypedIdentList TK_IN Statement
			{ make_val_exp (EXIST_n (List.rev $2,$4)) $1 }
	|	TK_EXCEPTION IdentList TK_IN Statement
			{ make_val_exp (EXCEPT_n (List.rev $2,$4)) $1 }
/*
	ceux qui ont une continuation
*/
/*
	|	TK_TRY Statement
			{ make_val_exp (TRY_n ($2,None)) $1 }
	|	TK_TRY Statement TK_DO Statement
			{ make_val_exp (TRY_n ($2,Some $4)) $1 }
	|	TK_CATCH Ident TK_IN Statement
			{ make_val_exp (CATCH_n($2,$4,None)) $1}
	|	TK_CATCH Ident TK_IN Statement TK_DO Statement
			{ make_val_exp (CATCH_n($2,$4,Some $6)) $1}
	|	TK_TRAP Ident TK_IN Statement
			{ make_val_exp (TRAP_n($2,$4,None)) $1}
	|	TK_TRAP Ident TK_IN Statement TK_DO Statement
			{ make_val_exp (TRAP_n($2,$4,Some $6)) $1}
*/
	|	TK_TRY Statement DoPart
			{ make_val_exp (TRY_n ($2,$3)) $1 }
	|	TK_CATCH Ident TK_IN Statement DoPart
			{ make_val_exp (CATCH_n($2,$4, $5)) $1}
	|	TK_TRAP Ident TK_IN Statement DoPart
			{ make_val_exp (TRAP_n($2,$4,$5)) $1}

/*
	|	TK_PARAHEAD Statement TK_PARA ParaList1
			{ make_val_exp (PARA_n ($2::(List.rev $4))) $1 }
	|	TK_PARAHEAD TK_OPEN_BRACE Statement TK_PARA ParaList1
				TK_CLOSE_BRACE
			{ make_val_exp (PARA_n ($3::(List.rev $5))) $1 }
*/
	;

DoPart :
	/* nada */ %prec NO_DO_PART
		{ None }
	|	TK_DO Statement
		{ Some $2 }
	;

LoopStatement:
		TK_LOOP Statement
			{  make_val_exp (LOOP_n $2) $1 }
	|	TK_LOOP Average Statement
			{ make_val_exp (LOOPI_n (fst $2, snd $2, $3)) $1 }
	|	TK_LOOP Gaussian  Statement
			{ make_val_exp (LOOPA_n (fst $2, snd $2, $3)) $1 }
	;

Average:
	TK_OPEN_BRACKET Exp TK_COMA Exp TK_CLOSE_BRACKET
		{ ($2, $4) }
	;

Gaussian:
		TK_TILDA Exp %prec TK_NOT
		{ ($2, None) }
	|	TK_TILDA Exp TK_COLON Exp %prec TK_NOT
		{ ($2, Some $4) }
	;

WeightOpt:
	/* nada */
		{ None }
	|	TK_WEIGHT Exp
		{ Some {it = $2; src = $1} }
	;

/* au moins 1 choix avec poids optionnel */
ChoicesList:
	/* inc le cas "simple parenthèse" */
		Statement WeightOpt
			{ [ ($1, $2) ] }
	|	ChoicesList TK_BAR Statement WeightOpt
			{ ($3,$4) :: $1 }
	;

/* au moins 1 choix prio */
PrioList:
	Statement
		{ [$1] }
	|	PrioList TK_BARSUP Statement
		{ $3::$1 }
	;

ParaList:
		Statement
			{ [$1] }
	|	ParaList TK_PARA Statement
			{ $3::$1 }
	;


/* attention: si un seul élément SANS poids -> simple exp */

BracedStatement:
		TK_OPEN_BRACE Statement TK_CLOSE_BRACE
		{ $2 }
	|	TK_OPEN_BRACE Statement TK_BARSUP PrioList TK_CLOSE_BRACE
		{
			let args = $2::(List.rev $4) in
			make_val_exp (PRIO_n args) $1
		}
	|	TK_OPEN_BRACE Statement WeightOpt TK_BAR ChoicesList TK_CLOSE_BRACE
		{
			let args = ($2, $3)::(List.rev $5) in
			make_val_exp (CHOICE_n args) $1
		}
/*
	|	TK_OPEN_BRACE Statement TK_BAR ChoicesList TK_CLOSE_BRACE
		{
			let args = ($2, None)::(List.rev $4) in
			make_val_exp (CHOICE_n args) $1
		}
	|	TK_OPEN_BRACE Statement TK_WEIGHT Exp TK_BAR ChoicesList TK_CLOSE_BRACE
		{
			let args = ($2, Some {it=$4; src=$3})::(List.rev $6) in
			make_val_exp (CHOICE_n args) $1
		}
*/
	|	TK_OPEN_BRACE Statement TK_PARA ParaList TK_CLOSE_BRACE
		{
			let args = $2::(List.rev $4) in
			make_val_exp (PARA_n args) $1
		}
	;

/* Expresssions algébriques classique s
*/

/* ATTENTION : ne pas changer à cause des prios ! */

/* ebnf:group=expressions */

Exp:
	/* identificateurs, constantes et call */
		IdentRef { $1 }
	|	Constant { $1 }
	|	TK_PRE Ident { make_val_exp (PRE_n $2) $1 }
	/* simple parenthese */
	|	TK_OPEN_PAR Exp TK_CLOSE_PAR { $2 }

	/* opérateurs infixés */

	|	TK_MINUS Exp %prec TK_UMINUS
		{ make_val_exp (CALL_n (flagit "uminus" $1, [$2])) $1 }
	|	TK_NOT Exp { make_val_exp (CALL_n (flagit "not" $1, [$2])) $1 }

	|	BinExp { $1 }

/*
	|	Exp TK_EQ Exp { make_val_exp (CALL_n (flagit "eq" $2, [$1;$3])) $2 }
	|	Exp TK_NEQ Exp { make_val_exp (CALL_n (flagit "neq" $2, [$1;$3])) $2 }
	|	Exp TK_OR Exp { make_val_exp (CALL_n (flagit "or" $2, [$1;$3])) $2 }
	|	Exp TK_XOR Exp { make_val_exp (CALL_n (flagit "xor" $2, [$1;$3])) $2 }
	|	Exp TK_AND Exp { make_val_exp (CALL_n (flagit "and" $2, [$1;$3])) $2 }
	|	Exp TK_IMPL Exp { make_val_exp (CALL_n (flagit "impl" $2, [$1;$3])) $2 }
	|	Exp TK_PLUS Exp { make_val_exp (CALL_n (flagit "plus" $2, [$1;$3])) $2 }
	|	Exp TK_MINUS Exp { make_val_exp (CALL_n (flagit "minus" $2, [$1;$3])) $2 }
	|	Exp TK_TIMES Exp { make_val_exp (CALL_n (flagit "times" $2, [$1;$3])) $2 }
	|	Exp TK_SLASH Exp { make_val_exp (CALL_n (flagit "slash" $2, [$1;$3])) $2 }
	|	Exp TK_DIV Exp { make_val_exp (CALL_n (flagit "div" $2, [$1;$3])) $2 }
	|	Exp TK_MOD Exp { make_val_exp (CALL_n (flagit "mod" $2, [$1;$3])) $2 }
	|	Exp TK_LT Exp { make_val_exp (CALL_n (flagit "lt" $2, [$1;$3])) $2 }
	|	Exp TK_LTE Exp { make_val_exp (CALL_n (flagit "lte" $2, [$1;$3])) $2 }
	|	Exp TK_GT Exp { make_val_exp (CALL_n (flagit "gt" $2, [$1;$3])) $2 }
	|	Exp TK_GTE Exp { make_val_exp (CALL_n (flagit "gte" $2, [$1;$3])) $2 }
*/

	|	TK_IF Exp TK_THEN Exp TK_ELSE Exp
			{ make_val_exp (CALL_n (flagit "ite" $1, [$2;$4;$6])) $1 }
	;

BinExp:
		Exp TK_EQ Exp { make_val_exp (CALL_n (flagit "eq" $2, [$1;$3])) $2 }
	|	Exp TK_NEQ Exp { make_val_exp (CALL_n (flagit "neq" $2, [$1;$3])) $2 }
	|	Exp TK_OR Exp { make_val_exp (CALL_n (flagit "or" $2, [$1;$3])) $2 }
	|	Exp TK_XOR Exp { make_val_exp (CALL_n (flagit "xor" $2, [$1;$3])) $2 }
	|	Exp TK_AND Exp { make_val_exp (CALL_n (flagit "and" $2, [$1;$3])) $2 }
	|	Exp TK_IMPL Exp { make_val_exp (CALL_n (flagit "impl" $2, [$1;$3])) $2 }
	|	Exp TK_PLUS Exp { make_val_exp (CALL_n (flagit "plus" $2, [$1;$3])) $2 }
	|	Exp TK_MINUS Exp { make_val_exp (CALL_n (flagit "minus" $2, [$1;$3])) $2 }
	|	Exp TK_TIMES Exp { make_val_exp (CALL_n (flagit "times" $2, [$1;$3])) $2 }
	|	Exp TK_SLASH Exp { make_val_exp (CALL_n (flagit "slash" $2, [$1;$3])) $2 }
	|	Exp TK_DIV Exp { make_val_exp (CALL_n (flagit "div" $2, [$1;$3])) $2 }
	|	Exp TK_MOD Exp { make_val_exp (CALL_n (flagit "mod" $2, [$1;$3])) $2 }
	|	Exp TK_LT Exp { make_val_exp (CALL_n (flagit "lt" $2, [$1;$3])) $2 }
	|	Exp TK_LTE Exp { make_val_exp (CALL_n (flagit "lte" $2, [$1;$3])) $2 }
	|	Exp TK_GT Exp { make_val_exp (CALL_n (flagit "gt" $2, [$1;$3])) $2 }
	|	Exp TK_GTE Exp { make_val_exp (CALL_n (flagit "gte" $2, [$1;$3])) $2 }
	;

Constant:
		TK_TRUE
		{ make_val_exp TRUE_n $1 }
	|   TK_FALSE
		{ make_val_exp FALSE_n $1}
	|    TK_ICONST
		{ make_val_exp (ICONST_n (ident_of_token $1)) $1 }
	|   TK_RCONST
		{ make_val_exp (RCONST_n (ident_of_token $1)) $1 }
	;

/* statement ou exp */
/* ebnf:group=identref */

IdentRef:
		Ident %prec HACK_ID
			{ make_val_exp (IDENT_n $1) $1.src }
	|	Ident TK_OPEN_PAR TK_CLOSE_PAR %prec HACK_CALL
			{ make_val_exp (CALL_n ($1, [])) $1.src }
	|	Ident TK_OPEN_PAR ArgList TK_CLOSE_PAR %prec HACK_CALL
			{ make_val_exp (CALL_n ($1, List.rev $3)) $1.src }
	;


ArgList:
		Arg
			{ [$1] }
	| ArgList TK_COMA Arg
			{ $3::$1 }
;

Arg: Statement
		{ $1 }
;
