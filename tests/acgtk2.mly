%{
    type fixity = {prec_spec : string option ;
                   (* if not [None], specifies the operator with an immediately higher precedence *)
                   assoc : Logic.Abstract_syntax.Abstract_syntax.associativity option ;
                  }

    let build_infix opt sym sg =
      let sym_id,_ = sym in
      match opt {assoc = None ; prec_spec = None } sg with
      | {assoc = None ; prec_spec = None } ->
         let p,sg' = Environment.Signature1.new_precedence sym_id sg in
         sym,Abstract_syntax.Infix (Abstract_syntax.Left,p),sg'
      | {assoc = None ; prec_spec = Some id } ->
         let p,sg' = Environment.Signature1.new_precedence ~before:id sym_id sg in
         sym,Abstract_syntax.Infix (Abstract_syntax.Left,p),sg'
      | {assoc = Some a ; prec_spec = None} ->
         let p,sg' = Environment.Signature1.new_precedence sym_id sg in
         sym,Abstract_syntax.Infix (a,p),sg'
      | {assoc = Some a ; prec_spec = Some id} ->
         let p,sg' = Environment.Signature1.new_precedence ~before:id sym_id sg in
         sym,Abstract_syntax.Infix (a,p),sg'

    type context =
      | Type_ctx
      | Term_ctx

                    
                  %}

%token <Logic.Abstract_syntax.Abstract_syntax.location> LSQBRACKET RSQBRACKET
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location)> SYMBOL
%token <Logic.Abstract_syntax.Abstract_syntax.location> COMMA

%token <Logic.Abstract_syntax.Abstract_syntax.location> TYPE
%token <Logic.Abstract_syntax.Abstract_syntax.location> PREFIX
%token <Logic.Abstract_syntax.Abstract_syntax.location> INFIX
%token <Logic.Abstract_syntax.Abstract_syntax.location> BINDER

                                                          %start < AcgData.Environment.Environment.Signature1.t -> AcgData.Environment.Environment.t -> AcgData.Environment.Environment.Signature1.t> sig_entry_eoi


%%

sig_entry_eoi :
| e = sig_entry EOI { e }
  
%public  sig_entry :
| decl = type_declaration { decl }
| def = type_definition { def }
| decl = term_declaration { decl }
| def = term_definition { def }


  type_declaration :
| ids = separated_nonempty_list(COMMA,IDENT) COLON TYPE
                     {
                       fun s _ ->
                       List.fold_left
	                 (fun acc id ->
                           let id_name,id_loc = id in
	                   try
	                     Environment.Signature1.add_entry (Abstract_syntax.Type_decl (id_name,id_loc,(Abstract_syntax.K []))) acc
	                   with
	                   | Environment.Signature1.Duplicate_type_definition -> 
		              emit_parse_error (Error.Duplicated_type id_name) id_loc)
	                 s
	                 ids
                     }

                     
   type_definition :
| id = IDENT EQUAL type_or_cst = type_or_term(COLON) COLON TYPE
                                              {
    let type_or_cst = type_or_cst Type_ctx in
    fun sg _ ->
    let id_name,id_loc = id in
    let type_expr,_ = get_type (type_or_cst false) sg [] in
    try
      Environment.Signature1.add_entry (Abstract_syntax.Type_def (id_name,id_loc,type_expr,Abstract_syntax.K [])) sg
    with
    | Environment.Signature1.Duplicate_type_definition -> 
       emit_parse_error (Error.Duplicated_type id_name) id_loc
                                              }
                                              
term_declaration :
| dec = term_dec_start COLON type_exp = type_or_term (SEMICOLON)
   {
    let type_exp = type_exp Type_ctx in
     fun s _e ->
     let dec',s' = dec s in
     List.fold_left
       (fun acc ((id,loc),kind) -> 
	 try
	   let ty,_ = get_type (type_exp false) acc [] in
	   Environment.Signature1.add_entry (Abstract_syntax.Term_decl (id,kind,loc,ty)) acc
	 with
	 | Environment.Signature1.Duplicate_term_definition -> 
	    emit_parse_error (Error.Duplicated_term id) loc)
       s'
       dec'
   }
   

%inline term_dec_start : 
| ids = separated_nonempty_list(COMMA,IDENT) { fun sg -> List.map (fun id -> (id,Abstract_syntax.Default)) ids,sg }
| PREFIX sym = SYMBOL { fun sg -> [sym,Abstract_syntax.Prefix],sg } 
| INFIX sym = SYMBOL { fun sg ->
                       let sym_id,_ = sym in
                       let p,sg' = Environment.Signature1.new_precedence sym_id sg in
                       [sym,Abstract_syntax.(Infix (Left,p))],sg' }
| INFIX opt = infix_option  sym = SYMBOL { fun sg ->
                                           let sym,fix,sg' = build_infix opt sym sg in
                                           [sym,fix],sg'
                                         }
                                                              
| BINDER id = IDENT { fun sg -> [id,Abstract_syntax.Binder],sg }

                    term_definition :
| id = IDENT EQUAL t = type_or_term(COLON) COLON ty = type_or_term(SEMICOLON)
                                                                  {
    let t = t Term_ctx in
    let ty = ty Type_ctx in
    fun s _ ->
    let id',l = id in
    try
      (* Attention, BUG : pas de gestion des warnings !!! *)
      let term,_ =  get_term (t false) Typing_env.empty s [] in
      let ty',_ = get_type (ty false) s [] in
      Environment.Signature1.add_entry (Abstract_syntax.Term_def (id',Abstract_syntax.Default,l,term,ty')) s
    with
    | Environment.Signature1.Duplicate_term_definition ->
       emit_parse_error (Error.Duplicated_term id') l}
  
| def = term_def_start EQUAL t = type_or_term(COLON) COLON ty = type_or_term(SEMICOLON)
                                                   {
    let t = t Term_ctx in
    let ty = ty Type_ctx in
    fun s _ ->
    let (id,l),k,s' = def s in
    try
      (* Attention, BUG : pas de gestion des warnings !!! *)
      let term,_ =  get_term (t false) Typing_env.empty s' [] in
      let ty',_ = get_type (ty false) s' [] in
      Environment.Signature1.add_entry (Abstract_syntax.Term_def (id,k,l,term,ty')) s'
    with
    | Environment.Signature1.Duplicate_term_definition ->
       emit_parse_error (Error.Duplicated_term id) l}
  


  
%inline term_def_start : 
| PREFIX sym = SYMBOL {fun sg -> sym,Abstract_syntax.Prefix,sg}
| INFIX sym = SYMBOL {fun sg ->
                      let sym_id,_ = sym in
                      let p,sg' = Environment.Signature1.new_precedence sym_id sg in
                      sym,Abstract_syntax.Infix (Abstract_syntax.Left,p),sg'}
| INFIX opt = infix_option  sym = SYMBOL {fun sg -> build_infix opt sym sg }
| BINDER id = IDENT {fun sg -> id,Abstract_syntax.Binder,sg}


%inline infix_option :
| LSQBRACKET v = separated_nonempty_list(COMMA,infix_option_value) RSQBRACKET {
                                          fun spec sg ->
                                          List.fold_left
                                            (fun spec' value -> value spec' sg)
                                            spec
                                            v
                                        }

%inline infix_option_value :
| sym = SYMBOL id = SYMBOL {
                                  fun spec sg ->
                                  let sym_id,sym_loc = sym in
                                  let op_id,op_loc = id in
                                  if sym_id = "<" then
                                    match Environment.Signature1.is_constant op_id sg with
                                    | true, Some (Abstract_syntax.Infix _) -> {spec with prec_spec = Some op_id }
                                    | true, _ -> raise Error.(Error (Parse_error (Not_def_as_infix op_id,op_loc)))
                                    | false , _ -> raise Error.(Error (Parse_error (Unknown_constant op_id,op_loc)))
                                  else
                                    raise Error.(Error (Parse_error (Syntax_error ("The less than symbol '<' is expected in a precedence specification."),sym_loc)))
                                }
| id = IDENT  {
                     fun spec _sg ->
                     let assoc_id,assoc_loc = id in
                     let assoc_id = String.lowercase_ascii assoc_id in
                     if List.mem assoc_id ["left";"right";"nonassoc"] then
                       let assoc = match assoc_id with
                         | "left" -> Abstract_syntax.Left
                         | "right" -> Abstract_syntax.Right 
                         | "nonassoc" -> Abstract_syntax.NonAss
                         | _ -> raise Error.(Error (Parse_error (Syntax_error ("An associativity specification (one of the keywords 'Left', 'Right', or 'NonAssoc') is expected."),assoc_loc)))

                       in
                       {spec with assoc = Some assoc }
                     else
                       raise Error.(Error (Parse_error (Syntax_error ("An associativity specification (one of the keywords 'Left', 'Right', or 'NonAssoc') is expected."),assoc_loc)))
                   }
             
