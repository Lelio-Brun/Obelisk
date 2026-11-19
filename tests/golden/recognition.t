  $ obelisk ../../misc/reco.mly
  <my_option(X, Y)> ::= [Y X]
  
  <my_list(A)> ::= A*
  
  <my_nonempty_list(C)> ::= C+
  
  <my_separated_nonempty_list(X, Y)> ::= X (Y X)*
  
  <my_separated_list(X, S)> ::= [X (S X)*]
  
  <my_rule> ::= <my_option(E, F)>
              | <my_list(E)>
              | <my_nonempty_list(F)>
              | <my_separated_nonempty_list(E, S1)>
              | <my_separated_list(F, S2)>
  
  

  $ obelisk -i ../../misc/reco.mly
  <my_rule> ::= [F E]
              | E*
              | F+
              | E (S1 E)*
              | [F (S2 F)*]
  
  

  $ obelisk ebnf ../../misc/reco.mly
  my_rule ::= my_option_0
            | my_list_0
            | my_nonempty_list_0
            | my_separated_nonempty_list_0
            | my_separated_list_0
  
  my_option_0 ::= (F E)?
  
  my_list_0 ::= E*
  
  my_nonempty_list_0 ::= F+
  
  my_separated_nonempty_list_0 ::= E (S1 E)*
  
  my_separated_list_0 ::= (F (S2 F)*)?
  
  my_separated_nonempty_list_1 ::= F (S2 F)*
  
  

  $ obelisk ebnf -i ../../misc/reco.mly
  my_rule ::= (F E)?
            | E*
            | F+
            | E (S1 E)*
            | (F (S2 F)*)?
  
  

  $ obelisk ../llists.mly
  <main> ::= <llist(<expr>)> EOL
  
  <expr> ::= INT
           | LPAREN <expr> RPAREN
           | <expr> PLUS <expr>
           | <expr> TIMES <expr>
  
  <llist(X)> ::= X*
  
  

  $ obelisk -i ../llists.mly
  <main> ::= <expr>* EOL
  
  <expr> ::= INT
           | LPAREN <expr> RPAREN
           | <expr> PLUS <expr>
           | <expr> TIMES <expr>
  
  

  $ obelisk ebnf -i ../llists.mly
  main ::= expr* EOL
  
  expr ::= INT
         | LPAREN expr RPAREN
         | expr PLUS expr
         | expr TIMES expr
  
  
