%on_error_reduce bound_term_ending(EOI)
%on_error_reduce bound_term_ending(COLON)
%on_error_reduce bound_term_ending(SEMICOLON)


%%

  
%public bound_term(phantom) :
   | binder = LAMBDA vars_and_term = bound_term_ending(phantom) 
                                       { fun kind _ -> 
                                         let vars,(bounded_term_builder,t_loc) = vars_and_term in
                                         let n_loc = new_loc binder t_loc in
                                         let type_builder _ _ =
                                           emit_parse_error
                                             (Error.Syntax_error "A type identifier is expected") binder in
                                         if kind = Type_ctx then
                                           type_builder () ()
                                         else
                                           let term_builder type_env sg warnings =
                                             let type_env = List.fold_left
                                                              (fun acc (var,_) -> Typing_env.add var acc)
                                                              type_env
                                                              vars in                                   
                                             let t,ws =
                                               match bounded_term_builder with
                                               | Type_or_term (_,t_builder,_) -> 
                                                  t_builder type_env sg warnings in
                                             (fst
                                                (List.fold_left
                                                   (fun (acc,first_var) (var,loc) ->
                                                     if first_var then
                                                       (fun t -> acc (abs (var,n_loc,t) Abstract_syntax.Non_linear)),false
                                                     else
                                                       (fun t -> acc (abs (var,loc,t) Abstract_syntax.Non_linear)),false)
                                                   ((fun x -> x),true)
                                                   vars)) t,
                                             ws in
                                           Type_or_term (type_builder,term_builder,n_loc)
                                       }
| binder = LAMBDA0 vars_and_term = bound_term_ending(phantom)
                                     { fun kind _ -> 
                                         let vars,(bounded_term_builder,t_loc) = vars_and_term in
                                         let n_loc = new_loc binder t_loc in
                                         let type_builder _ _ =
                                           emit_parse_error
                                             (Error.Syntax_error "A type identifier is expected") binder in
                                         if kind = Type_ctx then
                                           type_builder () ()
                                         else
                                         let term_builder type_env sg warnings =
                                           let type_env = List.fold_left
                                                            (fun acc (var,_) -> Typing_env.add var acc)
                                                            type_env
                                                            vars in              
                                           let t,ws =
                                             match bounded_term_builder with
                                             | Type_or_term (_,t_builder,_) -> 
                                                t_builder type_env sg warnings in
                                           (fst
                                              (List.fold_left
                                                 (fun (acc,first_var) (var,loc) ->
                                                   if first_var then
                                                     (fun t -> acc (abs (var,n_loc,t) Abstract_syntax.Linear)),false
                                                   else
                                                     (fun t -> acc (abs (var,loc,t) Abstract_syntax.Linear)),false)
                                                 ((fun x -> x),true)
                                                 vars)) t,
                                           ws in
                                         Type_or_term (type_builder,term_builder,n_loc) }
| binder =  IDENT vars_and_term = bound_term_ending(phantom)
                                    {fun kind _ -> 
                                      let binder,loc = binder  in
                                      let vars,(bounded_term_builder,t_loc) = vars_and_term  in
                                      let n_loc = new_loc loc t_loc in
                                      let type_builder sg _warnings =
                                        match Environment.Signature1.is_type binder sg,vars_and_term with
	                                | true,((_,var_loc)::_,_) ->
                                           emit_parse_error
                                             (Error.Syntax_error "An arrow ('->' or '=>') or a right parenthesis ')' are expected.") var_loc
                                        | true,([],_) -> failwith "Bug: should not happen"
	                                | false,_ -> emit_parse_error (Error.Unknown_type binder) loc in
                                      let () =
                                        match kind, vars_and_term with
	                                | Type_ctx,((_,var_loc)::_,_) ->
                                           emit_parse_error
                                             (Error.Syntax_error "An arrow ('->' or '=>') or a right parenthesis ')' are expected.") var_loc
                                        | Type_ctx,([],_) -> failwith "Bug: Should not happen"
                                        | Term_ctx,_ -> () in
                                      let term_builder type_env sg warnings =
                                        let linearity =
                                          match Environment.Signature1.is_constant binder sg with
                                          | true,Some Abstract_syntax.Binder ->
                                             (match Environment.Signature1.get_binder_argument_functional_type binder sg with
					      | None -> failwith "Bug: Binder of non functional type"
					      | Some k -> k)
				          | _ -> emit_parse_error (Error.Binder_expected binder) loc in
                                        let type_env = List.fold_left
                                                         (fun acc (var,_) -> Typing_env.add var acc)
                                                         type_env
                                                         vars in                                   
                                        let t,ws =
                                          match bounded_term_builder with
                                          | Type_or_term (_,t_builder,_) -> t_builder type_env sg warnings in
                                        Abstract_syntax.(App 
                                          (Const (binder,loc),
                                           (fst (List.fold_left
                                                   (fun (acc,first_var) (var,loc) ->
                                                     if first_var then
                                                       (fun t -> acc (abs (var,n_loc,t) linearity )),false
                                                     else
                                                       (fun t -> acc (abs (var,loc,t) linearity )),false)
                                                   ((fun x -> x),true)
                                                   vars)) t,
                                           n_loc)),
                                        ws in
                                      Type_or_term (type_builder,term_builder,n_loc) }
                                    
                                    bound_term_ending(phantom) :
| var = IDENT d = DOT t=type_or_term(phantom) {[var],
                                               let type_builder _ _ =
                                                 emit_parse_error
                                                   (Error.Syntax_error "An arrow ('->' or '=>') or a right parenthesis ')' are expected.") d in
                                               match t Term_ctx false with
                                               | Type_or_term (_,term_builder,loc) -> Type_or_term (type_builder,term_builder,loc),loc
                                              }
| var = IDENT vars_and_term = bound_term_ending(phantom) {let vars,t = vars_and_term in var::vars,t}
                                               
