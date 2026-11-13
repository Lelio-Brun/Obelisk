(* https://github.com/BinaryAnalysisPlatform/FrontC/tree/master/frontc *)

%token <string> IDENT
%token <string> CST_CHAR
%token <string> CST_INT
%token <string> CST_FLOAT
%token <string> CST_STRING
%token <string> NAMED_TYPE
%token <Cabs.gnu_attrs> GNU_ATTRS

%token EOF
%token CHAR BOOL INT DOUBLE FLOAT VOID
%token ENUM STRUCT TYPEDEF UNION
%token SIGNED UNSIGNED LONG SHORT COMPLEX
%token VOLATILE EXTERN STATIC CONST AUTO REGISTER RESTRICT
%token <string> BUILTIN_TYPE

%token SIZEOF ASM

%token <string * int> EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
%token <string * int> AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%token ARROW DOT

%token EQ_EQ EXCLAM_EQ INF SUP INF_EQ SUP_EQ
%token PLUS MINUS STAR SLASH PERCENT
%token TILDE AND PIPE CIRC
%token EXCLAM AND_AND PIPE_PIPE
%token INF_INF SUP_SUP
%token PLUS_PLUS MINUS_MINUS

%token <string * int> RPAREN LPAREN RBRACE LBRACE LBRACKET RBRACKET
%token <string * int> COLON SEMICOLON COMMA ELLIPSIS QUEST

%token <string * int> BREAK CONTINUE GOTO RETURN
%token <string * int> SWITCH CASE DEFAULT
%token <string * int> WHILE DO FOR
%token <string * int> IF ELSE

/* GNU attributes */
%token ATTRIBUTE EXTENSION INLINE

%{
    open Cabs

    exception Error

    let parse_error startpos endpos =
      Clexer.display_error "Syntax error" startpos endpos;
      raise Error

    (*
     ** Type analysis
     *)
    type modifier =
      BASE_SIZE of size
    | BASE_SIGN of sign
    | BASE_STORAGE of storage
    | BASE_VOLATILE
    | BASE_CONST
    | BASE_COMPLEX
    | BASE_GNU_ATTR of Cabs.gnu_attrs

    let apply_mod (typ, sto) modi =
      let rec mod_root typ =
	match (typ, modi) with
          (NO_TYPE, BASE_SIGN sign) -> INT (NO_SIZE, sign)
        | (NO_TYPE, BASE_SIZE size) -> INT (size, NO_SIGN)
        | (CHAR NO_SIGN, BASE_SIGN sign) -> CHAR sign
        | (INT (NO_SIZE, sign), BASE_SIZE size) -> INT (size, sign)
        | (INT (LONG, sign), BASE_SIZE LONG) -> INT (LONG_LONG, sign)
        | (INT (size, NO_SIGN), BASE_SIGN sign) -> INT (size, sign)
        | (BITFIELD (NO_SIGN, exp), BASE_SIGN sign) -> BITFIELD (sign, exp)
        | (FLOAT false, BASE_SIZE LONG) -> FLOAT true
        | (DOUBLE false, BASE_SIZE LONG) -> DOUBLE true
        | (FLOAT false, BASE_COMPLEX) -> COMPLEX_FLOAT
        | (DOUBLE false, BASE_COMPLEX) -> COMPLEX_DOUBLE
        | (DOUBLE true, BASE_COMPLEX) -> COMPLEX_LONG_DOUBLE
        | (PTR typ, _) -> PTR (mod_root typ)
        | (CONST typ, _) -> CONST (mod_root typ)
        | (VOLATILE typ, _) -> VOLATILE (mod_root typ)
        | (GNU_TYPE (attrs, typ), _) -> GNU_TYPE (attrs, mod_root typ)
        | (TYPE_LINE (f, l, t), _) -> TYPE_LINE (f, l, mod_root t)
        | _ -> raise BadModifier in
      let check_access typ =
        match typ with
          PROTO _ | OLD_PROTO _ -> false
          | _ -> true in
      match modi with
      | BASE_SIGN _ | BASE_SIZE _ | BASE_COMPLEX -> (mod_root typ, sto)
      | BASE_CONST ->
         if (check_access typ) then (CONST typ, sto)
         else raise BadModifier
      | BASE_VOLATILE ->
         if (check_access typ) then (VOLATILE typ, sto)
         else raise BadModifier
      | BASE_STORAGE sto' ->
         if sto = NO_STORAGE then (typ, sto')
         else raise BadModifier
      | BASE_GNU_ATTR attrs ->
         (GNU_TYPE (attrs, typ), sto)

    let apply_mods mods fty =
      List.fold_left apply_mod fty mods

    let apply_mods_to_base_type mods bty =
      fst@@apply_mods mods (bty,NO_STORAGE)

    let set_type tst tin =
      let rec set typ =
        match typ with
          NO_TYPE -> tst
        | PTR typ -> PTR (set typ)
        | RESTRICT_PTR typ -> RESTRICT_PTR (set typ)
        | ARRAY (typ, dim) -> ARRAY (set typ, dim)
        | PROTO (typ, pars, ell) -> PROTO (set typ, pars, ell)
        | OLD_PROTO (typ, pars, ell) -> OLD_PROTO (set typ, pars, ell)
        | CONST typ -> CONST (set typ)
        | VOLATILE typ -> VOLATILE (set typ)
        | TYPE_LINE (f, l, t) -> TYPE_LINE (f, l, set t)
        | BITFIELD (NO_SIGN, exp) ->
           (match tst with
              INT (_, sign) -> BITFIELD (sign, exp)
            | _ -> raise BadType)
        | _ -> raise BadType in
      set tin


    (*
     ** Expression building
     *)
    let smooth_expression lst =
      match lst with
        [] -> NOTHING
      | [expr] -> expr
      | _ -> COMMA (List.rev lst)
    let list_expression expr =
      match expr with
        COMMA lst -> lst
      | NOTHING -> []
      | _ -> [expr]


    (*** Named Building ***)
    let set_name (typ : base_type) (id, typ', attr, exp) =
      (id, set_type typ typ', attr, exp)

    let set_name_group (typ, sto) (lst : name list)
        : name_group =
      (typ, sto, List.map (set_name typ) lst)

    let set_single (typ, sto) name : single_name =
      (typ, sto, set_name typ name)

    let apply_qual ((t1, q1) : base_type * modifier list)
          ((t2, q2) : base_type * modifier list)
        : base_type * modifier list =
      ((if t1 = NO_TYPE then t2 else
          if t2 = NO_TYPE then t1 else  raise BadModifier),
       List.append q1 q2)

    (*** Line management ***)
    let set_line (file, line) stat =
      if Clexer.linerec !Clexer.current_handle
      then Cabs.STAT_LINE (stat, file, line)
      else stat

    let set_eline (file, line) expr =
      if Clexer.linerec !Clexer.current_handle
      then Cabs.EXPR_LINE (expr, file, line)
      else expr

    let set_tline _type =
      if Clexer.linerec !Clexer.current_handle
      then Cabs.TYPE_LINE (Clexer.curfile (), Clexer.curline(), _type)
      else _type
%}


/* operator precedence */
%nonassoc  IF
%nonassoc  ELSE

%right EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ PERCENT_EQ
AND_EQ PIPE_EQ CIRC_EQ INF_INF_EQ SUP_SUP_EQ
%right QUEST COLON
%left PIPE_PIPE
%left AND_AND
%left PIPE
%left CIRC
%left AND
%left EQ_EQ EXCLAM_EQ
%left INF SUP INF_EQ SUP_EQ
%left INF_INF SUP_SUP
%left PLUS MINUS
%left STAR SLASH PERCENT CONST VOLATILE RESTRICT
%right EXCLAM TILDE PLUS_PLUS MINUS_MINUS CAST RPAREN ADDROF
%left LBRACKET
%left DOT ARROW LPAREN SIZEOF

/* Non-terminals informations */
%start interpret file
%type <Cabs.definition list> file interpret globals

%type <Cabs.definition> global
%type <Cabs.base_type * Cabs.storage> global_type
%type <Cabs.base_type * modifier list> global_qual

%type <modifier> global_mod
%type <Cabs.name list> global_defs
%type <Cabs.name> global_def
%type <string * Cabs.base_type> global_dec

%type <Cabs.definition> local
%type <Cabs.base_type * Cabs.storage> local_type
%type <Cabs.base_type * modifier list> local_qual
%type <modifier list> local_mod_list_opt local_mod_list
%type <modifier> local_mod
%type <Cabs.name list> local_defs
%type <Cabs.name> local_def
%type <string * Cabs.base_type> local_dec

%type <Cabs.gnu_attrs> gcc_attributes
%type <(string * int) * (Cabs.definition list * Cabs.statement)> body
%type <Cabs.statement> statement opt_stats stats
%type <Cabs.constant> constant
%type <Cabs.expression> expression init_expression opt_expression
%type <Cabs.expression list> comma_expression
%type <Cabs.single_name list * bool> parameters
%type <string> string_list

%%

interpret: file {$1};

file: globals {$1};

globals:
  | global* EOF {$1}
  | error {parse_error $symbolstartofs $endofs}
;

typedef:
  |  TYPEDEF typedef_type typedef_defs
    {
      List.iter (fun (id, _, _, _) -> Clexer.add_type id) $3;
      TYPEDEF (set_name_group (fst $2, snd $2) $3, [])}
  |  gcc_attribute TYPEDEF typedef_type typedef_defs
    {List.iter (fun (id, _, _, _) -> Clexer.add_type id) $4;
     TYPEDEF (set_name_group (fst $3, snd $3) $4, $1)}
;

/*** Global Definition ***/
global:
  | global_type global_defs SEMICOLON
    {DECDEF (set_name_group $1 (List.rev $2))}
  | global_type global_proto body
    {
      let (_, base, _, _) = $2 in
      match base with
      | PROTO _ -> FUNDEF (set_single $1 $2, (snd $3))
      | OLD_PROTO _ -> OLDFUNDEF (set_single $1 $2, [], (snd $3))
      | _ -> parse_error $symbolstartofs $endofs
    }
  | global_type global_proto basic_asm opt_gcc_attributes SEMICOLON
    {
      let (id, base, attr, exp) = $2 in
      let name = (id, base, attr@$4, exp) in
      match base with
      | PROTO _ -> FUNDEF (set_single $1 name, ([], $3))
      | OLD_PROTO _ -> OLDFUNDEF (set_single $1 name, [], ([], $3))
      | _ -> parse_error $symbolstartofs $endofs
    }
  |  global_type old_proto old_pardefs body
    { OLDFUNDEF (set_single $1 $2, List.rev $3, (snd $4)) }
  |  global_type SEMICOLON
    {ONLYTYPEDEF (set_name_group $1 [])}
  | typedef SEMICOLON {$1}
;
global_type:
  | global_mod_list_opt global_qual
    {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
  | global_mod_list_opt comp_type global_mod_list_opt
    {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
  | global_mod_list_opt NAMED_TYPE global_mod_list_opt
    {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
  | global_mod_list_opt
    {apply_mods $1 (NO_TYPE, NO_STORAGE)}
;
global_mod_list_opt: global_mod* {$1};

global_mod:
  |  STATIC        {BASE_STORAGE STATIC}
  |  CONST         {BASE_CONST}
  |  VOLATILE      {BASE_VOLATILE}
  |  EXTERN        {BASE_STORAGE EXTERN}
  |  gcc_attribute {BASE_GNU_ATTR $1 }
;

global_qual:
  |  qual_type      {$1}
  |  global_qual qual_type   {apply_qual $1 $2}
  |  global_qual global_mod   {(fst $1, $2::(snd $1))}
;

global_defs:  separated_nonempty_list(COMMA, global_def) {$1};

global_def:
global_dec opt_gcc_attributes
    {(fst $1, snd $1, $2, NOTHING)}
  |  global_dec opt_gcc_attributes EQ init_expression
    {(fst $1, snd $1, $2, $4)}
;
global_dec:
  |  IDENT
    {($1, set_tline NO_TYPE)}
  |  LPAREN global_dec RPAREN
    {$2}
  |  STAR global_dec
    {(fst $2, set_type (PTR NO_TYPE) (snd $2))}
  |  STAR CONST global_dec
    {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
  |  STAR VOLATILE global_dec
    {(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
  |  STAR RESTRICT global_dec
    {(fst $3, set_type (RESTRICT_PTR NO_TYPE) (snd $3))}
  |  STAR gcc_attributes global_dec
    {(fst $3, set_type (GNU_TYPE ($2, PTR NO_TYPE)) (snd $3))}
  |  global_dec LBRACKET comma_expression RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, smooth_expression $3)) (snd $1))}
  |  global_dec LBRACKET RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
  |  global_dec LPAREN parameters RPAREN
    {(fst $1, PROTO (snd $1, fst $3, snd $3))}
  |  LPAREN global_dec RPAREN LPAREN parameters RPAREN
    {(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
  |  global_dec LPAREN old_parameters RPAREN
    {(fst $1, OLD_PROTO (snd $1, fst $3, snd $3))}
  |  LPAREN global_dec RPAREN LPAREN old_parameters RPAREN
    {(fst $2, set_type (OLD_PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
;
global_proto:
global_dec opt_gcc_attributes
    {match (snd $1) with
       PROTO _
     | OLD_PROTO _ ->
 (fst $1, snd $1, $2, NOTHING)
     | _ -> parse_error $symbolstartofs $endofs }
;
old_proto:
global_dec opt_gcc_attributes
    {match (snd $1) with
       OLD_PROTO _ -> (fst $1, snd $1, $2, NOTHING)
     (*| PROTO (typ, [], ell) -> fst $1, OLD_PROTO (typ, [], ell), $2, NOTHING*)
     | _ -> parse_error $symbolstartofs $endofs }
;


/*** Old Parameter Style ***/
old_parameters:
old_pardecs      {(List.rev $1, false)}
  |  old_pardecs ELLIPSIS   {(List.rev $1, true)}
;
old_pardecs:
  |  IDENT       {[$1]}
  |  old_pardecs COMMA IDENT   {$3::$1}
  |  old_pardecs COMMA NAMED_TYPE {$3::$1}
;

old_pardefs: old_pardef+ {$1};

old_pardef:
old_type old_defs SEMICOLON
    {set_name_group $1 (List.rev $2)}
;
old_type:
old_mods_opt NAMED_TYPE old_mods_opt
    {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
  |  old_mods_opt comp_type old_mods_opt
    {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
  |  old_mods_opt old_qual
    {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
;
old_mods_opt:
/* empty */      {[]}
  |  CONST       {[BASE_CONST]}
  |  REGISTER      {[BASE_STORAGE REGISTER]}
;
old_qual:
  |  qual_type      {$1}
  |  old_qual qual_type    {apply_qual $1 $2}
  |  old_qual CONST     {(fst $1, BASE_CONST::(snd $1))}
  |  old_qual REGISTER    {(fst $1, (BASE_STORAGE REGISTER)::(snd $1))}
;
old_defs:
old_def       {[$1]}
  |  old_defs COMMA old_def   {$3::$1}
;
old_def:
old_dec
    {(fst $1, snd $1, [], NOTHING)}
;
old_dec:
IDENT
    {($1, NO_TYPE)}
  |  STAR old_dec
    {(fst $2, set_type (PTR NO_TYPE) (snd $2))}
  |  STAR CONST old_dec
    {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
  |  STAR VOLATILE old_dec
    {(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
  |  old_dec LBRACKET comma_expression RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, smooth_expression $3)) (snd $1))}
  |  old_dec LBRACKET RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
  |  old_dec LPAREN parameters RPAREN
    {(fst $1, PROTO (snd $1, fst $3, snd $3))}
  |  LPAREN old_dec RPAREN LPAREN parameters RPAREN
    {(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
  |  LPAREN old_dec RPAREN
    {$2}
;


/*** Local Definition ***/
local:
local_type local_defs SEMICOLON
    {DECDEF (set_name_group $1 (List.rev $2))}
;
local_type:
local_mod_list_opt local_qual
    {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
  |  local_mod_list_opt comp_type local_mod_list_opt
    {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
  |  local_mod_list_opt NAMED_TYPE local_mod_list_opt
    {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
;
local_mod_list_opt:
/* empty */      {[]}
  |  local_mod_list     {List.rev $1}
;
local_mod_list:
local_mod      {[$1]}
  |  local_mod_list local_mod  {$2::$1}
;
local_mod:
STATIC       { BASE_STORAGE STATIC }
  |  AUTO       { BASE_STORAGE AUTO }
  |  CONST       { BASE_CONST }
  |  VOLATILE      { BASE_VOLATILE }
  |  REGISTER      { BASE_STORAGE REGISTER }
  |  EXTERN       { BASE_STORAGE EXTERN }
  |  gcc_attribute     { BASE_GNU_ATTR $1 }
;
local_qual:
  |  qual_type      {$1}
  |  local_qual qual_type   {apply_qual $1 $2}
  |  local_qual local_mod   {(fst $1, $2::(snd $1))}
;
local_defs:
  |  local_def      {[$1]}
  |  local_defs COMMA local_def  {$3::$1}
;
local_def:
  | local_dec opt_gcc_attributes
    {(fst $1, snd $1, $2, NOTHING)}
  | local_dec opt_gcc_attributes EQ init_expression
    {(fst $1, snd $1, $2, $4)}
;
local_dec:
IDENT
    {($1, NO_TYPE)}
  |  NAMED_TYPE
    {Clexer.add_identifier $1;($1, NO_TYPE)}
  |  STAR local_dec
    {(fst $2, set_type (PTR NO_TYPE) (snd $2))}
  |  STAR RESTRICT local_dec
    {(fst $3, set_type (RESTRICT_PTR NO_TYPE) (snd $3))}
  |  STAR CONST local_dec
    {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
  |  STAR VOLATILE local_dec
    {(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
  |  STAR gcc_attributes local_dec
    {(fst $3, set_type (GNU_TYPE ($2, PTR NO_TYPE)) (snd $3))}
  |  local_dec LBRACKET comma_expression RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, smooth_expression $3)) (snd $1))}
  |  local_dec LBRACKET RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
  |  local_dec LPAREN parameters RPAREN
    {(fst $1, PROTO (snd $1, fst $3, snd $3))}
  |  LPAREN local_dec RPAREN LPAREN parameters RPAREN
    {(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
  |  LPAREN local_dec RPAREN
    {$2}
;


/*** Typedef Definition ***/
typedef_type:
  | typedef_sub
    {apply_mods (snd $1) ((fst $1), NO_STORAGE)}
  |  CONST typedef_sub
    {apply_mods (BASE_CONST::(snd $2)) ((fst $2), NO_STORAGE)}
  |  VOLATILE typedef_sub
    {apply_mods (BASE_VOLATILE::(snd $2)) ((fst $2), NO_STORAGE)}
;
typedef_sub:
NAMED_TYPE      {(NAMED_TYPE $1, [])}
  |  comp_type      {($1, [])}
  |  typedef_qual     {$1}
/* !!TODO!! Unknown named type support: add option for it */
  |  IDENT       { Clexer.add_type $1; (NAMED_TYPE $1, [])}
  |  NAMED_TYPE CONST    {(NAMED_TYPE $1, [BASE_CONST])}
  |  NAMED_TYPE VOLATILE    {(NAMED_TYPE $1, [BASE_VOLATILE])}
  |  comp_type CONST     {($1, [BASE_CONST])}
  |  comp_type VOLATILE    {($1, [BASE_VOLATILE])}
  |  IDENT CONST      { Clexer.add_type $1; (NAMED_TYPE $1, [BASE_CONST])}
  |  IDENT VOLATILE     { Clexer.add_type $1; (NAMED_TYPE $1, [BASE_VOLATILE])}
;
typedef_qual:
qual_type      {$1}
  |  typedef_qual qual_type   {apply_qual $1 $2}
  |  typedef_qual CONST    {(fst $1, BASE_CONST::(snd $1))}
  |  typedef_qual VOLATILE   {(fst $1, BASE_VOLATILE::(snd $1))}
;
typedef_defs:
typedef_def      {[$1]}
  |  typedef_defs COMMA typedef_def {$3::$1}
;
typedef_def:
typedef_dec opt_gcc_attributes
    {(fst $1, snd $1, $2, NOTHING)}
;
typedef_dec:
  | IDENT | NAMED_TYPE
    {($1, NO_TYPE)}
  |  STAR typedef_dec
    {(fst $2, set_type (PTR NO_TYPE) (snd $2))}
  |  STAR RESTRICT typedef_dec
    {(fst $3, set_type (RESTRICT_PTR NO_TYPE) (snd $3))}
  |  STAR CONST typedef_dec
    {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
  |  STAR VOLATILE typedef_dec
    {(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
  |  STAR gcc_attributes typedef_dec
    {(fst $3, set_type (GNU_TYPE ($2, PTR NO_TYPE)) (snd $3))}
  |  typedef_dec LBRACKET comma_expression RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, smooth_expression $3)) (snd $1))}
  |  typedef_dec LBRACKET RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
  |  typedef_dec LPAREN parameters RPAREN
    {(fst $1, PROTO (snd $1, fst $3, snd $3))}
  |  LPAREN typedef_dec RPAREN LPAREN parameters RPAREN
    {(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
  |  LPAREN typedef_dec RPAREN
    {$2}
;


/*** Field Definition ***/
field_list: field* {$1};

field:
  | field_type field_defs opt_gcc_attributes SEMICOLON
   {
     match $3, set_name_group $1 $2 with
     | [],r -> r
     | attrs,(t,s,ns) -> GNU_TYPE (attrs,t),s,ns
   }
  | field_mod_list_opt struct_type field_mod_list_opt field_defs SEMICOLON
    { set_name_group (apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))) $4}
  | field_mod_list_opt struct_type field_mod_list_opt SEMICOLON
    { set_name_group (apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))) [("", NO_TYPE, [], NOTHING)]}
  | field_mod_list_opt union_type field_mod_list_opt field_defs SEMICOLON
    { set_name_group (apply_mods $1 (apply_mods $3 ($2, NO_STORAGE))) $4 }
  | field_mod_list_opt union_type field_mod_list_opt SEMICOLON
    { set_name_group (apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))) [("", NO_TYPE, [], NOTHING)]}
;
field_type:
field_mod_list_opt field_qual
    {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
  | field_mod_list_opt enum_type field_mod_list_opt
    {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
  | field_mod_list_opt NAMED_TYPE field_mod_list_opt
    {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
;
field_mod_list_opt:
/* empty */      {[]}
  |  field_mod_list     {List.rev $1}
;
field_mod_list:
field_mod      {[$1]}
  |  field_mod_list field_mod  {$2::$1}
;
field_mod:
CONST       {BASE_CONST}
  |  VOLATILE      {BASE_VOLATILE}
  |  gcc_attribute     { BASE_GNU_ATTR $1 }
;
field_qual:
qual_type      {$1}
  |  field_qual qual_type   {apply_qual $1 $2}
  |  field_qual field_mod   {(fst $1, $2::(snd $1))}
;
field_defs: separated_nonempty_list(COMMA, field_def) {$1};

field_def: field_dec {(fst $1, snd $1, [], NOTHING)} ;

field_dec:
  |  IDENT
    {($1, NO_TYPE)}
  |  NAMED_TYPE
    {($1, NO_TYPE)}
  |  STAR field_dec
    {(fst $2, set_type (PTR NO_TYPE) (snd $2))}
  |  STAR RESTRICT field_dec
    {(fst $3, set_type (RESTRICT_PTR NO_TYPE) (snd $3))}
  |  STAR CONST field_dec
    {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
  |  STAR VOLATILE field_dec
    {(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
  |  STAR gcc_attributes field_dec
    {(fst $3, set_type (GNU_TYPE ($2, PTR NO_TYPE)) (snd $3))}
  |  field_dec LBRACKET comma_expression RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, smooth_expression $3)) (snd $1))}
  |  field_dec LBRACKET RBRACKET
    {(fst $1, set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
  |  field_dec LPAREN parameters RPAREN
    {(fst $1, PROTO (snd $1, fst $3, snd $3))}
  |  LPAREN field_dec RPAREN LPAREN parameters RPAREN
    {(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
  |  LPAREN field_dec RPAREN
    {$2}
  |  IDENT? COLON expression
    {((match $1 with None -> "" | Some x -> x), BITFIELD (NO_SIGN, $3))}
;


/*** Parameter Definition ***/
parameters:
/* empty */      {([], false)}
  |  param_list      {(List.rev $1, false)}
  |  param_list COMMA ELLIPSIS  {(List.rev $1, true)}
;
param_list:
param_list COMMA param   {$3::$1}
  |  param       {[$1]}
;
param:
param_type param_def
    {set_single $1 $2}
;
param_type:
param_mods_opt NAMED_TYPE param_mods_opt
    {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
  |  param_mods_opt comp_type param_mods_opt
    {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
  |  param_mods_opt param_qual
    {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
;
param_mods_opt:
/* empty */      {[]}
  |  param_mods      {List.rev $1}
;
param_mods:
param_mod      {[$1]}
  |  param_mods param_mod   {$2::$1}
;
param_mod:
CONST       {BASE_CONST}
  |  REGISTER      {BASE_STORAGE REGISTER}
  |  VOLATILE      {BASE_VOLATILE}
  |  gcc_attribute     { BASE_GNU_ATTR $1 }
;
param_qual:
qual_type      {$1}
  |  param_qual qual_type   {apply_qual $1 $2}
  |  param_qual CONST    {(fst $1, BASE_CONST::(snd $1))}
  |  param_qual REGISTER    {(fst $1, (BASE_STORAGE REGISTER)::(snd $1))}
  |  param_qual VOLATILE    {(fst $1, BASE_VOLATILE::(snd $1))}
  |  param_qual gcc_attribute  {(fst $1, (BASE_GNU_ATTR $2)::(snd $1))}
;
param_def:
param_dec
    { let (name, _type) = $1 in (name, _type, [], NOTHING) }
;
param_dec:
/* empty */
    { ("", NO_TYPE) }
  |  IDENT
    { ($1, NO_TYPE) }
  |  NAMED_TYPE
    { ($1, NO_TYPE) }
  |  STAR param_dec
    {(fst $2, set_type (PTR NO_TYPE) (snd $2))}
  |  STAR RESTRICT param_dec
    {(fst $3, set_type (RESTRICT_PTR NO_TYPE) (snd $3))}
  |  STAR CONST param_dec
    {(fst $3, set_type (CONST (PTR NO_TYPE)) (snd $3))}
  |  STAR VOLATILE param_dec
    {(fst $3, set_type (VOLATILE (PTR NO_TYPE)) (snd $3))}
  |  STAR gcc_attributes param_dec
    {(fst $3, set_type (GNU_TYPE ($2, PTR NO_TYPE)) (snd $3))}
  |  param_dec LBRACKET global_mod_list_opt comma_expression RBRACKET
    {(fst $1, apply_mods_to_base_type $3 @@ set_type (ARRAY (NO_TYPE, smooth_expression $4)) (snd $1))}
  |  param_dec LBRACKET global_mod_list_opt RBRACKET
    {(fst $1, apply_mods_to_base_type $3 @@ set_type (ARRAY (NO_TYPE, NOTHING)) (snd $1))}
  |  LPAREN param_dec RPAREN LPAREN parameters RPAREN
    {(fst $2, set_type (PROTO (NO_TYPE, fst $5, snd $5)) (snd $2))}
  |  LPAREN param_dec RPAREN
    {$2}
;


/*** Only-type Definition ***/
only_type:
only_type_type only_def
    {set_type (fst $1) $2}
;
only_type_type:
only_mod_list_opt only_qual
    {apply_mods (snd $2) (apply_mods $1 ((fst $2), NO_STORAGE))}
  | only_mod_list_opt comp_type only_mod_list_opt
    {apply_mods $3 (apply_mods $1 ($2, NO_STORAGE))}
  |  only_mod_list_opt NAMED_TYPE only_mod_list_opt
    {apply_mods $3 (apply_mods $1 (NAMED_TYPE $2, NO_STORAGE))}
;
only_mod_list_opt:
/* empty */      {[]}
  |  only_mod_list     {List.rev $1}
;
only_qual:
qual_type      {$1}
  |  only_qual qual_type    {apply_qual $1 $2}
  |  only_qual only_mod    {(fst $1, $2::(snd $1))}
;
only_mod_list:
only_mod      {[$1]}
  |  only_mod_list only_mod  {$2::$1}
;
only_mod:
CONST       {BASE_CONST}
  |  VOLATILE      {BASE_VOLATILE}
  |  gcc_attribute     { BASE_GNU_ATTR $1 }
;
only_def:
only_dec      {$1}
;
only_dec:
/* empty */
    {NO_TYPE}
  |  STAR only_dec
    {set_type (PTR NO_TYPE) $2}
  |  STAR RESTRICT only_dec
    {set_type (RESTRICT_PTR NO_TYPE) $3}
  |  STAR CONST only_dec
    {set_type (CONST (PTR NO_TYPE)) $3}
  |  STAR VOLATILE only_dec
    {set_type (VOLATILE (PTR NO_TYPE)) $3}
  |  STAR gcc_attributes only_dec
    {set_type (GNU_TYPE ($2, PTR NO_TYPE)) $3}
  |  only_dec LBRACKET comma_expression RBRACKET
    {set_type (ARRAY (NO_TYPE, smooth_expression $3)) $1}
  |  only_dec LBRACKET RBRACKET
    {set_type (ARRAY (NO_TYPE, NOTHING)) $1}
  |  LPAREN only_dec RPAREN LPAREN parameters RPAREN
    {set_type (PROTO (NO_TYPE, fst $5, snd $5)) $2}
  |  LPAREN only_dec RPAREN
    {$2}
;


/*** Base type ***/
qual_type:
  |  VOID       {(VOID, [])}
  |  BOOL       {(BOOL, [])}
  |  CHAR       {(CHAR NO_SIGN, [])}
  |  INT        {(INT (NO_SIZE, NO_SIGN), [])}
  |  FLOAT      {(FLOAT false, [])}
  |  DOUBLE     {(DOUBLE false, [])}
  |  BUILTIN_TYPE {(BUILTIN_TYPE $1, [])}
  |  COMPLEX    {(NO_TYPE, [BASE_COMPLEX])}
  |  LONG       {(NO_TYPE, [BASE_SIZE LONG])}
  |  SHORT      {(NO_TYPE, [BASE_SIZE SHORT])}
  |  SIGNED     {(NO_TYPE, [BASE_SIGN SIGNED])}
  |  UNSIGNED   {(NO_TYPE, [BASE_SIGN UNSIGNED])}
;
comp_type:
  | struct_type {$1}
  | union_type {$1}
  | enum_type {$1}
;

struct_type:
STRUCT type_name
    {STRUCT ($2, [])}
  |  STRUCT LBRACE field_list RBRACE
    {STRUCT ("", $3)}
  |  STRUCT type_name LBRACE field_list RBRACE
    {STRUCT ($2, $4)}
;
union_type:
  |  UNION type_name
    {UNION ($2, [])}
  |  UNION LBRACE field_list RBRACE
    {UNION ("", $3)}
  |  UNION type_name LBRACE field_list RBRACE
    {UNION ($2, $4)}
;
enum_type:
  |  ENUM type_name
    {ENUM ($2, [])}
  |  ENUM LBRACE enum_list RBRACE
    {ENUM ("", List.rev $3)}
  |  ENUM LBRACE enum_list COMMA RBRACE
    {ENUM ("", List.rev $3)}
  |  ENUM type_name LBRACE enum_list RBRACE
    {ENUM ($2, List.rev $4)}
;
type_name:
IDENT       {$1}
  |  NAMED_TYPE      {$1}
;
enum_list:
  |  enum_name     {[$1]}
  |  enum_list COMMA enum_name {$3::$1}
;
enum_name: IDENT      {($1, NOTHING)}
  |   IDENT EQ expression   {($1, $3)}
;


/*** Expressions ****/
init_expression:
LBRACE compound_comma_expression RBRACE
    {CONSTANT (CONST_COMPOUND (List.rev $2))}
  |  expression
    {$1}
;

compound_expression:
LBRACE compound_comma_expression RBRACE
    {CONSTANT (CONST_COMPOUND (List.rev $2))}
  |  expression
    {$1}
  | DOT type_name EQ expression
    { DESIGNATED ($2, $4) }
;
compound_comma_expression:
compound_expression
    {[$1]}
  |  compound_comma_expression COMMA compound_expression
    {$3::$1}
  |  compound_comma_expression COMMA
    {$1}
;
opt_expression:
/* empty */
    {NOTHING}
  |  comma_expression
    {smooth_expression $1}
;
comma_expression:
expression
    {[$1]}
  |  comma_expression COMMA expression
    {$3::$1}
;
expression:
  | LPAREN EXTENSION expression RPAREN {$3}
  | constant
   {CONSTANT $1}
  |  IDENT
    {VARIABLE $1}
  |  SIZEOF expression
    {EXPR_SIZEOF $2}
  | SIZEOF LPAREN only_type RPAREN
    {TYPE_SIZEOF $3}
  |  PLUS expression
    {UNARY (PLUS, $2)}
  |  MINUS expression
    {UNARY (MINUS, $2)}
  |  STAR expression
    {UNARY (MEMOF, $2)}
  |  AND expression    %prec ADDROF
    {UNARY (ADDROF, $2)}
  |  EXCLAM expression
    {UNARY (NOT, $2)}
  |  TILDE expression
    {UNARY (BNOT, $2)}
  |  PLUS_PLUS expression %prec CAST
    {UNARY (PREINCR, $2)}
  |  expression PLUS_PLUS
    {UNARY (POSINCR, $1)}
  |  MINUS_MINUS expression %prec CAST
    {UNARY (PREDECR, $2)}
  |  expression MINUS_MINUS
    {UNARY (POSDECR, $1)}
  |  expression ARROW IDENT
    {MEMBEROFPTR ($1, $3)}
  |  expression ARROW NAMED_TYPE
    {MEMBEROFPTR ($1, $3)}
  |  expression DOT IDENT
    {MEMBEROF ($1, $3)}
  |  expression DOT NAMED_TYPE
    {MEMBEROF ($1, $3)}
  |  LPAREN body RPAREN
    {Clexer.test_gcc(); set_eline $1 (GNU_BODY (snd $2))}
  |  LPAREN comma_expression RPAREN
    {set_eline $1 (smooth_expression $2)}
  |  LPAREN only_type RPAREN expression %prec CAST
    {set_eline $1 (CAST ($2, $4))}
  |  expression LPAREN opt_expression RPAREN
    {set_eline $2 (CALL ($1, list_expression $3))}
  |  expression LBRACKET comma_expression RBRACKET
    {INDEX ($1, smooth_expression $3)}
  |  expression QUEST expression COLON expression
    {QUESTION ($1, $3, $5)}
  |  expression PLUS expression
    {BINARY(ADD ,$1 , $3)}
  |  expression MINUS expression
    {BINARY(SUB ,$1 , $3)}
  |  expression STAR expression
    {BINARY(MUL ,$1 , $3)}
  |  expression SLASH expression
    {BINARY(DIV ,$1 , $3)}
  |  expression PERCENT expression
    {BINARY(MOD ,$1 , $3)}
  |  expression AND_AND expression
    {BINARY(AND ,$1 , $3)}
  |  expression PIPE_PIPE expression
    {BINARY(OR ,$1 , $3)}
  |  expression AND expression
    {BINARY(BAND ,$1 , $3)}
  |  expression PIPE expression
    {BINARY(BOR ,$1 , $3)}
  |  expression CIRC expression
    {BINARY(XOR ,$1 , $3)}
  |  expression EQ_EQ expression
    {BINARY(EQ ,$1 , $3)}
  |  expression EXCLAM_EQ expression
    {BINARY(NE ,$1 , $3)}
  |  expression INF expression
    {BINARY(LT ,$1 , $3)}
  |  expression SUP expression
    {BINARY(GT ,$1 , $3)}
  |  expression INF_EQ expression
    {BINARY(LE ,$1 , $3)}
  |  expression SUP_EQ expression
    {BINARY(GE ,$1 , $3)}
  |  expression  INF_INF expression
    {BINARY(SHL ,$1 , $3)}
  |  expression  SUP_SUP expression
    {BINARY(SHR ,$1 , $3)}
  |  expression EQ expression
    {set_eline $2 (BINARY(ASSIGN ,$1 , $3))}
  |  expression PLUS_EQ expression
    {set_eline $2 (BINARY(ADD_ASSIGN ,$1 , $3))}
  |  expression MINUS_EQ expression
    {set_eline $2 (BINARY(SUB_ASSIGN ,$1 , $3))}
  |  expression STAR_EQ expression
    {set_eline $2 (BINARY(MUL_ASSIGN ,$1 , $3))}
  |  expression SLASH_EQ expression
    {set_eline $2 (BINARY(DIV_ASSIGN ,$1 , $3))}
  |  expression PERCENT_EQ expression
    {set_eline $2 (BINARY(MOD_ASSIGN ,$1 , $3))}
  |  expression AND_EQ expression
    {set_eline $2 (BINARY(BAND_ASSIGN ,$1 , $3))}
  |  expression PIPE_EQ expression
    {set_eline $2 (BINARY(BOR_ASSIGN ,$1 , $3))}
  |  expression CIRC_EQ expression
    {set_eline $2 (BINARY(XOR_ASSIGN ,$1 , $3))}
  |  expression INF_INF_EQ expression
    {set_eline $2 (BINARY(SHL_ASSIGN ,$1 , $3))}
  |  expression SUP_SUP_EQ expression
    {set_eline $2 (BINARY(SHR_ASSIGN ,$1 , $3))}
;
constant:
  |  CST_INT       {CONST_INT $1}
  |  CST_FLOAT      {CONST_FLOAT $1}
  |  CST_CHAR      {CONST_CHAR $1}
  |  string_list      {CONST_STRING $1}
;
string_list:
  | CST_STRING      {$1}
  | string_list CST_STRING   {$1 ^ $2}
;


/*** statements ***/
body_begin:
LBRACE        {Clexer.push_context (); $1}
;
body_middle:
opt_locals opt_stats    {($1, $2)}
;
body:
body_begin body_middle RBRACE  {Clexer.pop_context(); ($1, $2)}
;
opt_locals:
/* empty */      {[]}
  |  locals       {List.rev $1}
;
locals:
local       {[$1]}
  |  locals local     {$2::$1}
;
opt_stats:
/* empty */      {NOP}
  |  stats       {$1}
;
stats:
statement      {$1}
  |    stats statement     {SEQUENCE($1, $2)}
;
statement:
  | SEMICOLON
    {set_line $1 NOP}
  | comma_expression SEMICOLON
    {set_line $2 (COMPUTATION (smooth_expression $1))}
  |  body
    {set_line (fst $1) (BLOCK (snd $1))}
  |  IF LPAREN comma_expression RPAREN statement %prec IF
    {set_line $1 (IF (smooth_expression $3, $5, NOP))}
  |  IF LPAREN comma_expression RPAREN statement ELSE statement
    {set_line $1 (IF (smooth_expression $3, $5, set_line $6 $7))}
  |  SWITCH LPAREN comma_expression RPAREN statement
    {set_line $1 (SWITCH (smooth_expression $3, $5))}
  |  WHILE LPAREN comma_expression RPAREN statement
    {set_line $1 (WHILE (smooth_expression $3, $5))}
  |  DO statement WHILE LPAREN comma_expression RPAREN SEMICOLON
    {set_line $1 (DOWHILE (smooth_expression $5, $2))}
  |  FOR LPAREN opt_expression SEMICOLON opt_expression SEMICOLON opt_expression RPAREN statement
    {set_line $1 (FOR ($3, $5, $7, $9))}
  |  IDENT COLON statement
    {LABEL ($1, $3)}
  |  CASE expression COLON statement
    {set_line $1 (CASE ($2, $4))}
  |  DEFAULT COLON statement
    {set_line $1 (DEFAULT $3)}
  |  RETURN SEMICOLON
    {set_line $1 (RETURN NOTHING)}
  |  RETURN expression SEMICOLON
    {set_line $1 (RETURN $2)}
  |  BREAK SEMICOLON
    {set_line $1 BREAK}
  |  CONTINUE SEMICOLON
    {set_line $1 CONTINUE}
  |  GOTO IDENT SEMICOLON
    {set_line $1 (GOTO $2)}
  |  ASM LPAREN CST_STRING RPAREN SEMICOLON
    { ASM $3 }
  |  ASM LPAREN CST_STRING gnu_asm_io gnu_asm_io opt_gnu_asm_mods RPAREN SEMICOLON
    { Clexer.test_gcc(); GNU_ASM ($3, List.rev $4, List.rev $5, List.rev $6) }
;

/* "Basic asm" https://gcc.gnu.org/onlinedocs/gcc/Basic-Asm.html#Basic-Asm */
basic_asm:
  | ASM VOLATILE? opt_gcc_attributes LPAREN; asm=string_list; RPAREN
    { ASM asm }
;

/*** GNU asm ***/
gnu_asm_io: COLON gnu_asm_args { $2 };

gnu_asm_args: separated_nonempty_list(COMMA, gnu_asm_arg) {$1};

gnu_asm_arg:
  | CST_STRING LPAREN expression RPAREN
    { ("", $1, $3) }
  | LBRACKET IDENT RBRACKET CST_STRING LPAREN expression RPAREN
    { ($2, $4, $6) }
;

opt_gnu_asm_mods:
/* empty */
    { [] }
  | COLON gnu_asm_mods
    { $2 }
;

gnu_asm_mods:
  | CST_STRING
    { [$1] }
  | gnu_asm_mods COMMA CST_STRING
    { $3::$1 }
;


/*** GCC attributes ***/
opt_gcc_attributes:
/* empty */
    { Clexer.test_gcc(); [] }
  | gcc_attributes
    { Clexer.test_gcc(); List.rev $1 }

gcc_attributes:
gcc_attribute
    { $1 }
  | gcc_attributes gcc_attribute
    { List.append $1 $2 }

gcc_attribute:
ATTRIBUTE LPAREN LPAREN opt_gnu_args RPAREN RPAREN
    { List.rev $4 }
/*| GNU_ATTRS
    { $1 }*/
  | EXTENSION
    { [GNU_EXTENSION] }
  | INLINE
    { [GNU_INLINE] }
;

opt_gnu_args:
/* empty */
    { [] }
  | gnu_args
    { $1 }
;

gnu_args: separated_nonempty_list(COMMA,gnu_arg) {$1} ;

gnu_arg:
  | gnu_id
    { GNU_ID $1 }
  | local_type
    {GNU_TYPE_ARG (fst $1, snd $1)}
  | constant
    { GNU_CST $1 }
  | gnu_id LPAREN opt_gnu_args RPAREN
    { GNU_CALL ($1, $3) }
;

gnu_id:
  | IDENT  { $1 }
  | GNU_ATTRS
    {
      match $1 with
        [(Cabs.GNU_ID name)] -> name
      | _ -> parse_error $symbolstartofs $endofs
    }
;

%%
