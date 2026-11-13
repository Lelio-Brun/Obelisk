 %{

    open UtilsLib
    open Logic.Abstract_syntax
    open AcgData
    open Environment

    module Typing_env = Utils.StringSet

    let emit_parse_error e loc = raise (Error.Error (Error.Parse_error (e,loc)))

    let new_loc (s,_) (_,e) = (s,e)
                                
    let get_term_location = function
      | Abstract_syntax.Var (_,l) -> l
      | Abstract_syntax.Const (_,l) -> l
      | Abstract_syntax.Abs (_,_,_,l) -> l
      | Abstract_syntax.LAbs (_,_,_,l) -> l
      | Abstract_syntax.App (_,_,l) -> l
                                         
                                         
    let abs (x,l1,t) = function
      | Abstract_syntax.Linear -> Abstract_syntax.LAbs (x,l1,t,new_loc l1 (get_term_location t))
      | Abstract_syntax.Non_linear -> Abstract_syntax.Abs (x,l1,t,new_loc l1 (get_term_location t))
                                                          
    let is_signature s e =
      try
        let _ = Environment.get_signature s e in
        true
      with
      | Environment.Signature_not_found _ -> false

    let is_lexicon l e =
      try
        let _ = Environment.get_lexicon l e in
        true
      with
      | Environment.Lexicon_not_found _ -> false


    let get_sig (name,loc) e =
      try
       Environment.get_signature name e
      with
      | Environment.Signature_not_found _ -> emit_parse_error (Error.No_such_signature name) loc

    let get_lex (name,loc) e =
      try
        Environment.get_lexicon name e
      with
      | Environment.Lexicon_not_found _ -> 
         emit_parse_error (Error.No_such_lexicon name) loc

     
                                                              
                                           %}

%token EOI
%token <Logic.Abstract_syntax.Abstract_syntax.location> LPAREN
%token <Logic.Abstract_syntax.Abstract_syntax.location> RPAREN
%token <Logic.Abstract_syntax.Abstract_syntax.location> SIG_OPEN
%token <Logic.Abstract_syntax.Abstract_syntax.location> LEX_OPEN
%token <Logic.Abstract_syntax.Abstract_syntax.location> NL_LEX_OPEN
%token <Logic.Abstract_syntax.Abstract_syntax.location> END_OF_DEC
%token <(string*Logic.Abstract_syntax.Abstract_syntax.location)> IDENT
%token <Logic.Abstract_syntax.Abstract_syntax.location> COLON
%token <Logic.Abstract_syntax.Abstract_syntax.location> EQUAL
%token <Logic.Abstract_syntax.Abstract_syntax.location> SEMICOLON

%token <Logic.Abstract_syntax.Abstract_syntax.location> COMPOSE
                                                        (*%right COMPOSE*)


%start <?overwrite:bool -> AcgData.Environment.Environment.t -> AcgData.Environment.Environment.t> main

%%

main:
  | dec=sig_or_lex+ EOI { fun ?(overwrite=false) e -> List.fold_left (fun acc d -> d ~overwrite acc) e dec
			}

sig_or_lex:
  | s=signature  { fun ~overwrite e -> s ~overwrite e }
  | l=lexicon  { fun ~overwrite e -> l ~overwrite e }

signature :
  | SIG_OPEN id=IDENT EQUAL entries = end_of_dec (sig_entry)
    {
      fun ~overwrite e ->
      let s,loc = id in
      if is_signature s e then
        raise (Error.(Error (Env_error (Duplicated_signature s,loc))))
      else
        let new_sig =
          List.fold_left
            (fun acc entry -> entry acc e)
            (Environment.Signature1.empty id)
            entries in
        Environment.(insert ~overwrite (Signature new_sig) ~to_be_dumped:true e)
    }

lexicon :
  | LEX_OPEN lex=lex_declaration
    {fun ~overwrite e -> lex ~overwrite ~non_linear:false e }
  | NL_LEX_OPEN lex=lex_declaration
    {fun ~overwrite e -> lex ~overwrite ~non_linear:true e }
  | LEX_OPEN lex=IDENT EQUAL exp=lexicon_exp {fun ~overwrite e ->
                                              let new_lex = exp (Some lex) e in
                                              Environment.(insert ~overwrite (Lexicon new_lex) ~to_be_dumped:true e)}



%inline lex_declaration :
  | lex=IDENT LPAREN abs=IDENT RPAREN COLON obj=IDENT EQUAL entries = end_of_dec(lex_entry)
    {fun ~overwrite ~non_linear e ->
     let lex_name,lex_loc = lex in
     let abs',obj'= get_sig abs e,get_sig obj e in
     if is_lexicon lex_name e then
       raise (Error.(Error (Env_error (Duplicated_lexicon lex_name,lex_loc))))
     else
       let lex' = List.fold_left
                    (fun acc entry -> entry acc e)
                    (Environment.Lexicon.empty lex ~abs:abs' ~obj:obj' ~non_linear)
                    entries  in
       let () = Environment.Lexicon.check lex' in
       Environment.(insert ~overwrite (Lexicon lex') ~to_be_dumped:true e)
    }

end_of_dec(entry_type):
  | entry = entry_type SEMICOLON? END_OF_DEC { [entry] }
  | entry = entry_type SEMICOLON entries = end_of_dec(entry_type) { entry :: entries }


lexicon_exp0 :
  | lex = IDENT { fun _ e -> get_lex lex e }
  | LPAREN lex = lexicon_exp RPAREN { lex }

lexicon_exp :
  | lex = lexicon_exp0 { lex }
  | lex1 = lexicon_exp0 COMPOSE lex2 = lexicon_exp
                                      {
                                        fun name e ->
                                        let l1,l2 = (lex1 None e),(lex2 None e) in
                                        let new_name =
                                          match name with
                                          | Some (n,loc) -> n,loc
                                          | None ->
                                             let l1_name,(l1_start,_) = Environment.Lexicon.name l1 in
                                             let l2_name,(_,l2_end) = Environment.Lexicon.name l2 in
                                             let new_name = l1_name^" << "^l2_name in
                                             let new_loc = l1_start,l2_end in
                                             new_name,new_loc in
                                        Environment.Lexicon.compose l1 l2 new_name
                                      }

