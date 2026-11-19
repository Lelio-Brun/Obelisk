  $ obelisk ../anonymous-nested.mly
  <id(X)> ::= X
  
  <foo> ::= A B
  
  <bar> ::= C D
  
  <phrase> ::= <id(<id(<foo> | <bar>)>)> <id(<foo> | <id(<bar>)>)> EOF
  
  

  $ obelisk ebnf ../anonymous-nested.mly
  foo ::= A B
  
  bar ::= C D
  
  phrase ::= id_0 id_2 EOF
  
  id_0 ::= id_1
  
  id_1 ::= foo | bar
  
  id_2 ::= foo | id_3
  
  id_3 ::= bar
  
  

  $ obelisk ../anonymous-param.mly
  <mixed_list(X, Y)> ::= (X | Y)*
  
  <phrase> ::= <mixed_list(A, B)> <mixed_list(C, D)> EOF
  
  

  $ obelisk ebnf ../anonymous-param.mly
  phrase ::= mixed_list_0 mixed_list_1 EOF
  
  mixed_list_0 ::= (A | B)*
  
  mixed_list_1 ::= (C | D)*
  
  

  $ obelisk ../pair-new-syntax.mly
  <pair(x, y)> ::= x y
  
  <fst(p)> ::= p
  
  <snd(p)> ::= p
  
  <main> ::= <pair(A, B)> <fst(<pair(A, B)>)> <snd(<pair(A, B)>)> C D EOF
  
  

  $ obelisk -i ../pair-new-syntax.mly
  <pair(x, y)> ::= x y
  
  <fst(p)> ::= p
  
  <snd(p)> ::= p
  
  <main> ::= <pair(A, B)> <fst(<pair(A, B)>)> <snd(<pair(A, B)>)> C D EOF
  
  

  $ obelisk ../parameterized-nonterminal.mly
  <a> ::= <b(A)>
  
  <b(X)> ::= <c(X)> <d>
  
  <d> ::= A
  
  <c(X)> ::= X
  
  

  $ obelisk ebnf ../parameterized-nonterminal.mly
  a ::= b_0
  
  d ::= A
  
  b_0 ::= c_0 d
  
  c_0 ::= A
  
  

  $ obelisk ../multi-token-alias-0.mly ../multi-token-alias-1.mly ../multi-token-alias-2.mly
  <atom> ::= '(' <term> ')'
           | INT
  
  <factor> ::= <factor> '*' <atom>
             | <factor> '/' <atom>
             | <atom>
  
  <term> ::= <term> '+' <factor>
           | <term> '-' <factor>
           | <factor>
  
  

  $ obelisk -noaliases ../multi-token-alias-0.mly ../multi-token-alias-1.mly ../multi-token-alias-2.mly
  <atom> ::= LPAREN <term> RPAREN
           | INT
  
  <factor> ::= <factor> MUL <atom>
             | <factor> DIV <atom>
             | <atom>
  
  <term> ::= <term> PLUS <factor>
           | <term> MINUS <factor>
           | <factor>
  
  

  $ obelisk latex ../forbid_end.mly | grep -i "endif"
  \NewDocumentCommand\zzzzzzENDIF{s}{\def\tmp{ENDIF}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\zzzzzzendif{s}{\def\tmp{endif}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \zzzzzzendif{}& \gramdef & \zzzzzzEND*{}\\
    & \grambar &\zzzzzzENDIF*{}

  $ obelisk latex -prefix my ../forbid_end.mly | grep -i "endif"
  \NewDocumentCommand\myENDIF{s}{\def\tmp{ENDIF}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myendif{s}{\def\tmp{endif}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \myendif{}& \mygramdef & \myEND*{}\\
    & \mygrambar &\myENDIF*{}

  $ cat > char-literals.mly <<'EOF'
  > %{ let quote = '"' %}
  > %token A
  > %start <unit> main
  > %%
  > main: x { () }
  > x:
  >   | A { match c with '"' -> 1 | '{' -> 2 | '}' -> 3 | '\'' -> 4 | _ -> 0 }
  >   | A A { (* "*)" '"' *) fun (l : 'a list) -> l }
  > EOF
  $ obelisk char-literals.mly
  <main> ::= <x>
  
  <x> ::= A
        | A A
  
  

  $ cat > attribute-payloads.mly <<'EOF'
  > %token A B
  > %start <unit> main
  > %%
  > main [@rule "]"]:
  >   x [@string "]"] y [@char ']'] { () } [@comment (* ] *)] [@braces { ] }]
  > x: A {}
  > y: B {}
  > EOF
  $ obelisk attribute-payloads.mly
  <main> ::= <x> <y>
  
  <x> ::= A
  
  <y> ::= B
  
  

  $ cat > alias-strings.mly <<'EOF'
  > %token BACKSLASH "\\"
  > %token QUOTE "\""
  > %token APOSTROPHE "'"
  > %token ARROW "→"
  > %token A
  > %start <unit> main
  > %%
  > main: A "\\" "\"" "'" "→" A {}
  > EOF
  $ obelisk alias-strings.mly
  <main> ::= A '\\' '\"' ''' '→' A
  
  

  $ obelisk -noaliases alias-strings.mly
  <main> ::= A BACKSLASH QUOTE APOSTROPHE ARROW A
  
  

  $ cat > multi-line-token-aliases.mly <<'EOF'
  > %token
  >   PLUS "+" MINUS "-"
  >   TIMES"*"
  > %token <int> INT
  > %start <int> main
  > %%
  > main: INT "+" INT "-" INT "*" INT { 0 }
  > EOF
  $ obelisk -noaliases multi-line-token-aliases.mly
  <main> ::= INT PLUS INT MINUS INT TIMES INT
  
  

  $ cat > new-syntax-attributes.mly <<'EOF'
  > %token FOO BAR COMMA
  > %nonassoc P
  > %start <unit> main
  > %%
  > let main :=
  >   | y = FOO [@a]; b = BAR* [@b]; { () } [@c]
  >   | separated_list(COMMA, FOO) [@d]; %prec P { () } [@e]
  >   | BAR; { () } %prec P [@f]
  > EOF
  $ obelisk new-syntax-attributes.mly
  <main> ::= FOO BAR*
           | [FOO (COMMA FOO)*]
           | BAR
  
  

  $ cat > grouped-alternatives.mly <<'EOF'
  > %token A B C D E
  > %start <unit> main
  > %%
  > main:
  >   | A endrule(B {} | C {}) D { () }
  >   | midrule(B {} | C {}) { () }
  >   | pair(endrule(A {} | B {}), C) { () }
  >   | list(endrule(D {} | E {})) { () }
  > EOF
  $ obelisk grouped-alternatives.mly
  <main> ::= A (B | C) D
           | B | C
           | (A | B) C
           | (D | E)*
  
  

  $ obelisk ebnf grouped-alternatives.mly
  main ::= A (B | C) D
         | B | C
         | (A | B) C
         | (D | E)*
  
  
