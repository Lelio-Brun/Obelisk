%{

    let id_to_term (id,loc) typing_env sg warnings =
      match Environment.Signature1.is_constant id sg,Typing_env.mem id typing_env with
      | (true,_),false -> Abstract_syntax.Const (id,loc),loc,warnings
      | (false,_),true -> Abstract_syntax.Var (id,loc),loc,warnings
      | (true,_),true -> Abstract_syntax.Var (id,loc),loc,(Error.Variable_or_constant (id,loc))::warnings
      | (false,_),false -> emit_parse_error (Error.Unknown_constant_nor_variable id) loc

    type 'a result = 'a * Error.warning list
                      
    type build_type = Environment.Signature1.t -> Error.warning list -> Abstract_syntax.type_def result
    type build_term = Typing_env.t -> Environment.Signature1.t -> Error.warning list -> Abstract_syntax.term result
    type build_token = Typing_env.t -> Environment.Signature1.t -> Error.warning list -> Term_sequence.token result

    type term0_result =
      | Type_or_token of (build_type * (build_token *Abstract_syntax.location) list * Abstract_syntax.location)

    type type_or_term_result =
      | Type_or_term of (build_type * build_term * Abstract_syntax.location)

                  
    let term0_to_term = function
      | Type_or_token (type_builder,token_builders,loc) -> 
         let term_builder type_env sg warnings =
           let rev_terms,warnings =
             List.fold_left
               (fun (t_lst,ws) (builder,_) ->
                 let t,ws' = builder type_env sg ws in
                 (t::t_lst),ws')
               ([],warnings)
               token_builders in
           let result,_ = Term_sequence.parse_sequence (List.rev rev_terms) sg in
           result,warnings in
         Type_or_term (type_builder,term_builder,loc)
              

    let get_term t type_env sg warnings = 
      match t with
        | Type_or_term (_,term_builder,_) -> term_builder type_env sg warnings
                                         
    let get_type t sg warnings = 
      match t with
        | Type_or_term (type_builder,_,_) -> type_builder sg warnings

                                         

        
    %}


%token <Logic.Abstract_syntax.Abstract_syntax.location> ARROW
%token <Logic.Abstract_syntax.Abstract_syntax.location> LIN_ARROW
                                                        (*%on_error_reduce type_expression *)

%token <Logic.Abstract_syntax.Abstract_syntax.location> LAMBDA
%token <Logic.Abstract_syntax.Abstract_syntax.location> LAMBDA0
%token <Logic.Abstract_syntax.Abstract_syntax.location> DOT
                                    
%start < AcgData.Environment.Environment.Signature1.t -> Logic.Lambda.Lambda.term * Logic.Lambda.Lambda.stype > term_alone
%start < AcgData.Environment.Environment.Signature1.t -> AcgData.Environment.Environment.Signature1.t -> Logic.Abstract_syntax.Abstract_syntax.term * Logic.Abstract_syntax.Abstract_syntax.type_def > heterogenous_term_and_type
%on_error_reduce type_or_term(EOI)
%on_error_reduce type_or_term(COLON)
%on_error_reduce type_or_term(SEMICOLON)




%%

%public term_alone:
| t = type_or_term(COLON) COLON ty = type_or_term(EOI) EOI {
                                             fun sg ->
                                             let t,_ = get_term (t Term_ctx false) Typing_env.empty sg [] in
                                             let ty,_ = get_type (ty Type_ctx false) sg [] in
                                             Environment.Signature1.convert_term t ty sg
                                           }
                                           
                              %public heterogenous_term_and_type:
| t = type_or_term(COLON) COLON ty = type_or_term(EOI) EOI
                                      {
                                        fun abs obj ->
                                        let t,_ = get_term (t Term_ctx false) Typing_env.empty obj [] in
                                        let ty,_ = get_type (ty Type_ctx false) abs [] in
                                        t,ty
  }

                                             %inline arrow :
| l =ARROW { Abstract_syntax.Non_linear,l }
  | l = LIN_ARROW { Abstract_syntax.Linear,l }



%public type_or_term(phantom) :
  | t = type_or_term0(phantom) { fun k brackets -> term0_to_term (t k brackets) }
                       
  | t1 = type_or_term0(phantom) arrow = arrow t2 = type_or_term(phantom)
                     {
                         let arrow,arrow_loc =
                           match arrow with
                           | Abstract_syntax.Linear,loc -> (fun ty1 ty2 l -> Abstract_syntax.Linear_arrow (ty1,ty2,l)),loc
                           | Abstract_syntax.Non_linear,loc -> (fun ty1 ty2 l -> Abstract_syntax.Arrow (ty1,ty2,l)), loc in
                         fun kind  ->
                         if kind = Term_ctx then
                           emit_parse_error
                             (Error.Syntax_error "A term \"<term>\", and not an arrow '->' or '=>', is expected.") arrow_loc
                         else
                           fun _ ->
                           match t1 kind false with
                           | Type_or_token (type_builder1,term_builders1,l1) ->
                              (match t2 kind false with
                               | Type_or_term (type_builder2,_,l2) ->
                                  let new_loc = new_loc l1 l2 in
                                  let type_builder sg ws =
                                    let ty1',ws' = type_builder1 sg ws in
                                    let ty2',ws'' = type_builder2  sg ws' in
                                    let new_type = arrow ty1' ty2' new_loc in
                                    new_type,ws'' in
                                  let () =
                                    match term_builders1 with
                                    | (_t_builder1,_)::(_t_builder2,loc2)::_ ->
                                       emit_parse_error 
                                         (Error.Syntax_error "An arrow ('->' or '=>') is expected.") loc2
                                    | _ -> () in
                                  let term_builder type_env sg ws =
                                    let _ =
                                      match term_builders1 with
                                      | [t_builder,_] -> t_builder type_env sg ws
                                      | _ -> failwith "Bug: Should not happen" in
                                    emit_parse_error
                                      (Error.Syntax_error "A term \"<term>\", and not an arrow '->' or '=>', is expected.") arrow_loc in
                                  Type_or_term (type_builder,term_builder,new_loc))
                     }
  | t = bound_term(phantom) { t }
                  
                  type_or_term0(phantom):
  | id = IDENT {
             fun _ _ -> 
             let id,loc = id in
             let type_builder sg warnings =
               match Environment.Signature1.is_type id sg with
	       | true -> Abstract_syntax.Type_atom (id,loc,[]),warnings
	       | false -> emit_parse_error (Error.Unknown_type id) loc in
             let token_builder type_env sg warnings =
               let t,ws =
                 match Environment.Signature1.is_constant id sg,Typing_env.mem id type_env with
                 | (true,_),false -> Abstract_syntax.Const (id,loc),warnings
                 | (false,_),true -> Abstract_syntax.Var (id,loc),warnings
                 | (true,_),true -> Abstract_syntax.Var (id,loc),(Error.Variable_or_constant (id,loc))::warnings
	         | (false,_),false -> emit_parse_error (Error.Unknown_constant_nor_variable id) loc in
               Term_sequence.Term (t,loc),ws in
             Type_or_token (type_builder,[token_builder,loc],loc)
           }
| id = SYMBOL {
           fun kind ->
           let name,loc = id in
           if kind = Type_ctx then
             emit_parse_error (Error.Unknown_type name) loc
           else
             fun brackets ->
             let token_builder _type_env sg warnings =
               match Environment.Signature1.is_constant name sg,brackets with
               | (true,Some fix),false -> Term_sequence.Op (Abstract_syntax.Const (name,loc),fix,loc),warnings
               | (true,Some _fix),true -> Term_sequence.Term (Abstract_syntax.Const (name,loc),loc),warnings
               | (true,None),_ -> failwith "Bug: Should no happen"
               | (false,_),_ -> emit_parse_error (Error.Unknown_constant name) loc in
             let type_builder _ _ =  emit_parse_error (Error.Unknown_type name) loc in  
             Type_or_token (type_builder,[token_builder,loc],loc)
         }
              
              
| LPAREN t = type_or_term(phantom) RPAREN {
                           fun kind _ -> 
                           match t kind true with
                           | Type_or_term (type_builder,term_builder,l) ->
                              let token_builder type_env sg ws =
                                let t,ws' = term_builder type_env sg ws in
                                Term_sequence.Term (t,l),ws' in
                              Type_or_token (type_builder,[token_builder,l],l)
                         }
                         
                         
| id = IDENT t = type_or_term0(phantom) {
                                fun kind ->
                                match t kind false with
                                | Type_or_token (_,token_builders,loc') ->
                                   let type_builder _ _ =
                                     emit_parse_error
                                       (Error.Syntax_error "An arrow ('->' or '=>') or a right parenthesis ')' are expected.") loc' in
                                   if kind = Type_ctx then
                                     type_builder () ()
                                   else
                                     fun _ ->
                                     let _,loc = id in
                                     let token_builder type_env sg warnings =
                                       let cst,loc,warnings = id_to_term id type_env sg warnings in
                                       Term_sequence.Term (cst,loc),warnings in
                                     Type_or_token (type_builder,(token_builder,loc)::token_builders,new_loc loc loc')
                   }
| id = SYMBOL t = type_or_term0(phantom) {
                      let name,loc = id in
                      fun kind ->
                      if kind = Type_ctx then
                        emit_parse_error (Error.Unknown_type name) loc
                      else
                        fun _ -> 
                        let token_builder _type_env sg warnings =
                          match Environment.Signature1.is_constant name sg with
                          | true,Some fix -> Term_sequence.Op (Abstract_syntax.Const (name,loc),fix,loc),warnings
                          | true,None -> failwith "Bug: Should no happen"
                          | false,_ -> emit_parse_error (Error.Unknown_constant name) loc in
                        match t kind false with
                        | Type_or_token (_type_builder,token_builders,loc') ->
                           let type_builder _ _ = emit_parse_error (Error.Unknown_type name) loc in  
                           Type_or_token (type_builder,(token_builder,loc)::token_builders,new_loc loc loc')
                               }

| LPAREN t = type_or_term(phantom) RPAREN  t0 = type_or_term0(phantom) {
   fun kind ->
   match t0 kind false with
   | Type_or_token (_,token_builders,loc') ->
      (match t kind true with
       | Type_or_term (type_builder,term_builder,l) ->
          if kind = Type_ctx then
            emit_parse_error
              (Error.Syntax_error "An arrow ('->' or '=>') or a right parenthesis ')' are expected.") loc'
          else
            let type_builder sg ws =
              let _ = type_builder sg ws in
              emit_parse_error
                (Error.Syntax_error "An arrow ('->' or '=>') or a right parenthesis ')' are expected.") loc' in
            fun _brackets ->
            let token_builder type_env sg ws =
              let t,ws' = term_builder type_env sg ws in
              Term_sequence.Term (t,l),ws' in
            Type_or_token (type_builder, (token_builder,l)::token_builders,new_loc l loc'))
                                                             }
