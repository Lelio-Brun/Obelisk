%{


    let add_cst_interpretations term abs_sig lex (id,l) =
      match Environment.Signature1.is_constant id abs_sig with
      | true,_ -> Environment.Lexicon.insert (Abstract_syntax.Constant (id,l,term)) lex
      | false,_ -> emit_parse_error (Error.Unknown_constant id) l

   let add_type_interpretation stype abs_sig lex (id,l) =
     match Environment.Signature1.is_type id abs_sig with
     | true -> Environment.Lexicon.insert (Abstract_syntax.Type (id,l,stype)) lex
     | false -> emit_parse_error (Error.Unknown_type id) l
   
%}

%token <Logic.Abstract_syntax.Abstract_syntax.location> COLON_EQUAL
%start <AcgData.Environment.Environment.Lexicon.t -> AcgData.Environment.Environment.t -> AcgData.Environment.Environment.Lexicon.t > lex_entry_eoi
                                                          
%%


  id_or_sym :
| id = IDENT { id }
| sym = SYMBOL { sym }

               lex_entry_eoi :
| e = lex_entry EOI { e }

      %public lex_entry :
         
| cst = separated_nonempty_list(COMMA,id_or_sym) COLON_EQUAL t=type_or_term(SEMICOLON)
          {
            fun lex _e ->
            let abs,obj = Environment.Lexicon.get_sig lex in
            let are_types,are_terms =
              List.fold_left
                (fun (are_types,are_terms) (id,loc) ->
                  match Environment.Signature1.is_type id abs,Environment.Signature1.is_constant id abs with
                  | false,(false, _) -> emit_parse_error (Error.Unknown_constant_nor_type id) loc
                  | false,(true, _) when not are_terms -> emit_parse_error (Error.Unknown_type id) loc
                  | false,(true, _) -> false,true
                  | true,(false, _) when not are_types -> emit_parse_error (Error.Unknown_constant id) loc
                  | true,(false, _) -> true,false
                  | true,(true,_) -> true && are_types,true && are_terms)
                (true,true)
                cst in
            match are_types,are_terms with
            | false,false -> failwith "Bug: Should not happen"
            | true,true ->
               (match t Type_ctx false with
                | Type_or_term (type_builder, term_builder,_) ->
                   let term,_ = term_builder Typing_env.empty obj [] in
                   let stype,_ = type_builder obj [] in
                   List.fold_left
                     (fun acc cst ->
                       add_type_interpretation
                         stype
                         abs
                         (add_cst_interpretations term abs acc cst)
                         cst)
                     lex
                     cst)
            | false,true ->
               (match t Term_ctx false with
                | Type_or_term (_, term_builder,_) ->
                   let term,_ = term_builder Typing_env.empty obj [] in
                   List.fold_left (fun acc cst -> add_cst_interpretations term abs acc cst) lex cst)
            | true,false ->
               (match t Type_ctx false with
                | Type_or_term (type_builder, _,_) ->
                   let stype,_ = type_builder obj [] in
                   List.fold_left (add_type_interpretation stype abs) lex cst)
                }
