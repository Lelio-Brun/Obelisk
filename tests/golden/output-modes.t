  $ obelisk ../../src/parser.mly
  <specification> ::= <rule>* EOF
  
  <rule> ::= <old_rule>
           | <new_rule>
  
  <old_rule> ::= [<flags>] <ident> ATTRIBUTE* <parameters(<ident>)> COLON
                 <optional_bar> <group> (BAR <group>)* [<merge_function>]
                 SEMICOLON*
  
  <flags> ::= PUBLIC
            | INLINE
            | PUBLIC INLINE
            | INLINE PUBLIC
  
  <optional_bar> ::= [BAR]
  
  <group> ::= <production> (BAR <production>)* ACTION [<precedence>] ATTRIBUTE*
  
  <production> ::= <producer>* [<precedence>]
  
  <producer> ::= [LID EQ] <actual> ATTRIBUTE* SEMICOLON*
  
  <generic_actual(A, B)> ::= <ident> <parameters(A)>
                           | B <modifier>
  
  <actual> ::= <generic_actual(<lax_actual>, <actual>)>
  
  <lax_actual> ::= <generic_actual(<lax_actual>, <actual>)>
                 | <group> (BAR <group>)*
  
  <new_rule> ::= [PUBLIC] LET LID ATTRIBUTE* <parameters(<ident>)> <binder>
                 <expression> [<merge_function>]
  
  <binder> ::= COLONEQ
             | EQEQ
  
  <expression> ::= <optional_bar> <seq_expression> (BAR <seq_expression>)*
  
  <seq_expression> ::= [<pattern> EQ] <symbol_expression> SEMICOLON
                       <seq_expression>
                     | <symbol_expression>
                     | <action_expression>
  
  <symbol_expression> ::= <ident> <parameters(<expression>)> ATTRIBUTE*
                        | <symbol_expression> <modifier> ATTRIBUTE*
  
  <action_expression> ::= <action> ATTRIBUTE*
                        | <action> <precedence> ATTRIBUTE*
                        | <precedence> <action> ATTRIBUTE*
  
  <action> ::= ACTION
             | POINTFREEACTION
  
  <pattern> ::= LID
              | UNDERSCORE
              | TILDE
              | LPAR [<pattern> (COMMA <pattern>)*] RPAR
  
  <modifier> ::= OPT
               | PLUS
               | STAR
  
  <merge_function> ::= MERGE ACTION
  
  <precedence> ::= PREC <ident>
  
  <parameters(X)> ::= [LPAR [X (COMMA X)*] RPAR]
  
  <ident> ::= UID
            | LID
            | QID
  
  

  $ obelisk ebnf ../../src/parser.mly
  specification ::= rule* EOF
  
  rule ::= old_rule
         | new_rule
  
  old_rule ::= flags? ident ATTRIBUTE* parameters_0 COLON optional_bar group
               (BAR group)* merge_function? SEMICOLON*
  
  flags ::= PUBLIC
          | INLINE
          | PUBLIC INLINE
          | INLINE PUBLIC
  
  optional_bar ::= BAR?
  
  group ::= production (BAR production)* ACTION precedence? ATTRIBUTE*
  
  production ::= producer* precedence?
  
  producer ::= (LID EQ)? actual ATTRIBUTE* SEMICOLON*
  
  actual ::= generic_actual_0
  
  lax_actual ::= generic_actual_0
               | group (BAR group)*
  
  new_rule ::= PUBLIC? LET LID ATTRIBUTE* parameters_0 binder expression
               merge_function?
  
  binder ::= COLONEQ
           | EQEQ
  
  expression ::= optional_bar seq_expression (BAR seq_expression)*
  
  seq_expression ::= (pattern EQ)? symbol_expression SEMICOLON seq_expression
                   | symbol_expression
                   | action_expression
  
  symbol_expression ::= ident parameters_2 ATTRIBUTE*
                      | symbol_expression modifier ATTRIBUTE*
  
  action_expression ::= action ATTRIBUTE*
                      | action precedence ATTRIBUTE*
                      | precedence action ATTRIBUTE*
  
  action ::= ACTION
           | POINTFREEACTION
  
  pattern ::= LID
            | UNDERSCORE
            | TILDE
            | LPAR (pattern (COMMA pattern)*)? RPAR
  
  modifier ::= OPT
             | PLUS
             | STAR
  
  merge_function ::= MERGE ACTION
  
  precedence ::= PREC ident
  
  ident ::= UID
          | LID
          | QID
  
  parameters_0 ::= (LPAR (ident (COMMA ident)*)? RPAR)?
  
  generic_actual_0 ::= ident parameters_1
                     | actual modifier
  
  parameters_1 ::= (LPAR (lax_actual (COMMA lax_actual)*)? RPAR)?
  
  parameters_2 ::= (LPAR (expression (COMMA expression)*)? RPAR)?
  
  

  $ obelisk latex -tabular ../../src/parser.mly
  \documentclass[preview]{standalone}
  
  \usepackage[T1]{fontenc}
  
  \usepackage{xparse}
  \usepackage{tabularray}
  
  \NewDocumentEnvironment{obeliskgrammar}{+b}{
    \begin{tblr}{colspec={@{}r@{}c@{}X@{}}, column{1}={cmd=\gramnonterm}}
    #1
    \end{tblr}
  }{}
  
  \newcommand\gramsp{\quad}
  \newcommand\gramdef{$\gramsp::=\gramsp$}
  \newcommand\grambar{$\gramsp|\gramsp$}
  \newcommand\grambaranon{$|$}
  \newcommand\grameps{\ensuremath{\epsilon}}
  \newcommand\gramnonterm[1]{\IfBlankF{#1}{\ensuremath{\langle\textnormal{#1}\rangle}}}
  \newcommand\gramfunc[1]{\gramnonterm{#1}}
  \newcommand\gramterm[1]{#1}
  
  \NewDocumentCommand\ACTION{s}{\def\tmp{ACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ATTRIBUTE{s}{\def\tmp{ATTRIBUTE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\BAR{s}{\def\tmp{BAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLON{s}{\def\tmp{COLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLONEQ{s}{\def\tmp{COLONEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COMMA{s}{\def\tmp{COMMA}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EOF{s}{\def\tmp{EOF}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQ{s}{\def\tmp{EQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQEQ{s}{\def\tmp{EQEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\INLINE{s}{\def\tmp{INLINE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LET{s}{\def\tmp{LET}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LID{s}{\def\tmp{LID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LPAR{s}{\def\tmp{LPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\MERGE{s}{\def\tmp{MERGE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\OPT{s}{\def\tmp{OPT}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PLUS{s}{\def\tmp{PLUS}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\POINTFREEACTION{s}{\def\tmp{POINTFREEACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PREC{s}{\def\tmp{PREC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PUBLIC{s}{\def\tmp{PUBLIC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\QID{s}{\def\tmp{QID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\RPAR{s}{\def\tmp{RPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\SEMICOLON{s}{\def\tmp{SEMICOLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\STAR{s}{\def\tmp{STAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\TILDE{s}{\def\tmp{TILDE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UID{s}{\def\tmp{UID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UNDERSCORE{s}{\def\tmp{UNDERSCORE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\action{s}{\def\tmp{action}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actionUNDERSCOREexpression{s}{\def\tmp{action\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actual{s}{\def\tmp{actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\binder{s}{\def\tmp{binder}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\expression{s}{\def\tmp{expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\flags{s}{\def\tmp{flags}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\group{s}{\def\tmp{group}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ident{s}{\def\tmp{ident}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\laxUNDERSCOREactual{s}{\def\tmp{lax\_actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mergeUNDERSCOREfunction{s}{\def\tmp{merge\_function}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\modifier{s}{\def\tmp{modifier}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\newUNDERSCORErule{s}{\def\tmp{new\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\oldUNDERSCORErule{s}{\def\tmp{old\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\optionalUNDERSCOREbar{s}{\def\tmp{optional\_bar}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\pattern{s}{\def\tmp{pattern}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\precedence{s}{\def\tmp{precedence}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\producer{s}{\def\tmp{producer}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\production{s}{\def\tmp{production}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\rule{s}{\def\tmp{rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\seqUNDERSCOREexpression{s}{\def\tmp{seq\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\specification{s}{\def\tmp{specification}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\symbolUNDERSCOREexpression{s}{\def\tmp{symbol\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\genericUNDERSCOREactual{s}{\def\tmp{generic\_actual}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  \NewDocumentCommand\parameters{s}{\def\tmp{parameters}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  
  \NewDocumentCommand\gramopt{sm}{\def\tmp{[#2]}\IfBooleanTF#1{\tmp}{\tmp}}
  \NewDocumentCommand\gramplus{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^+}}
  \NewDocumentCommand\gramstar{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^*}}
  \NewDocumentCommand\gramseplist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^*}}
  \NewDocumentCommand\gramsepnelist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^+}}
  \newcommand\gramparen[1]{(#1)}
  
  \begin{document}
  
  \begin{obeliskgrammar}
  \specification{}& \gramdef & \gramstar{\rule*{}} \EOF*{}
    \\
  
  \rule{}& \gramdef & \oldUNDERSCORErule*{}\\
    & \grambar &\newUNDERSCORErule*{}
    \\
  
  \oldUNDERSCORErule{}& \gramdef & \gramopt{\flags*{}} \ident*{}
                                   \gramstar{\ATTRIBUTE*{}}
                                   \gramfunc{\parameters{}(\ident*{})}
                                   \COLON*{} \optionalUNDERSCOREbar*{}
                                   \gramsepnelist{\BAR*{}}{\group*{}}
                                   \gramopt{\mergeUNDERSCOREfunction*{}}
                                   \gramstar{\SEMICOLON*{}}
    \\
  
  \flags{}& \gramdef & \PUBLIC*{}\\
    & \grambar &\INLINE*{}\\
    & \grambar &\PUBLIC*{} \INLINE*{}\\
    & \grambar &\INLINE*{} \PUBLIC*{}
    \\
  
  \optionalUNDERSCOREbar{}& \gramdef & \gramopt{\BAR*{}}
    \\
  
  \group{}& \gramdef & \gramsepnelist{\BAR*{}}{\production*{}} \ACTION*{}
                       \gramopt{\precedence*{}} \gramstar{\ATTRIBUTE*{}}
    \\
  
  \production{}& \gramdef & \gramstar{\producer*{}} \gramopt{\precedence*{}}
    \\
  
  \producer{}& \gramdef & \gramopt*{\LID*{} \EQ*{}} \actual*{}
                          \gramstar{\ATTRIBUTE*{}} \gramstar{\SEMICOLON*{}}
    \\
  
  \genericUNDERSCOREactual{}(A, B)& \gramdef & \ident*{}
                                               \gramfunc{\parameters{}(A)}\\
    & \grambar &B \modifier*{}
    \\
  
  \actual{}& \gramdef & \gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},
                        \actual*{})}
    \\
  
  \laxUNDERSCOREactual{}& \gramdef & \gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},
                                     \actual*{})}\\
    & \grambar &\gramsepnelist{\BAR*{}}{\group*{}}
    \\
  
  \newUNDERSCORErule{}& \gramdef & \gramopt{\PUBLIC*{}} \LET*{} \LID*{}
                                   \gramstar{\ATTRIBUTE*{}}
                                   \gramfunc{\parameters{}(\ident*{})}
                                   \binder*{} \expression*{}
                                   \gramopt{\mergeUNDERSCOREfunction*{}}
    \\
  
  \binder{}& \gramdef & \COLONEQ*{}\\
    & \grambar &\EQEQ*{}
    \\
  
  \expression{}& \gramdef & \optionalUNDERSCOREbar*{}
                            \gramsepnelist{\BAR*{}}{\seqUNDERSCOREexpression*{}}
    \\
  
  \seqUNDERSCOREexpression{}& \gramdef & \gramopt*{\pattern*{} \EQ*{}}
                                         \symbolUNDERSCOREexpression*{}
                                         \SEMICOLON*{}
                                         \seqUNDERSCOREexpression*{}\\
    & \grambar &\symbolUNDERSCOREexpression*{}\\
    & \grambar &\actionUNDERSCOREexpression*{}
    \\
  
  \symbolUNDERSCOREexpression{}& \gramdef & \ident*{}
                                            \gramfunc{\parameters{}(\expression*{})}
                                            \gramstar{\ATTRIBUTE*{}}\\
    & \grambar &\symbolUNDERSCOREexpression*{} \modifier*{}
                \gramstar{\ATTRIBUTE*{}}
    \\
  
  \actionUNDERSCOREexpression{}& \gramdef & \action*{} \gramstar{\ATTRIBUTE*{}}\\
    & \grambar &\action*{} \precedence*{} \gramstar{\ATTRIBUTE*{}}\\
    & \grambar &\precedence*{} \action*{} \gramstar{\ATTRIBUTE*{}}
    \\
  
  \action{}& \gramdef & \ACTION*{}\\
    & \grambar &\POINTFREEACTION*{}
    \\
  
  \pattern{}& \gramdef & \LID*{}\\
    & \grambar &\UNDERSCORE*{}\\
    & \grambar &\TILDE*{}\\
    & \grambar &\LPAR*{} \gramseplist{\COMMA*{}}{\pattern*{}} \RPAR*{}
    \\
  
  \modifier{}& \gramdef & \OPT*{}\\
    & \grambar &\PLUS*{}\\
    & \grambar &\STAR*{}
    \\
  
  \mergeUNDERSCOREfunction{}& \gramdef & \MERGE*{} \ACTION*{}
    \\
  
  \precedence{}& \gramdef & \PREC*{} \ident*{}
    \\
  
  \parameters{}(X)& \gramdef & \gramopt*{\LPAR*{} \gramseplist{\COMMA*{}}{X}
                               \RPAR*{}}
    \\
  
  \ident{}& \gramdef & \UID*{}\\
    & \grambar &\LID*{}\\
    & \grambar &\QID*{}
    \\
  
  \end{obeliskgrammar}
  
  \end{document}

  $ obelisk latex -simplebnf ../../src/parser.mly
  \documentclass[preview]{standalone}
  
  \usepackage[T1]{fontenc}
  
  \usepackage{xparse}
  \usepackage{simplebnf}
  
  \newenvironment{obeliskgrammar}{
    \begin{bnf}[
      colspec={@{}l@{}rcX@{}l@{}},
      column{2}={cmd=\gramnonterm}
    ]
  }{
    \end{bnf}
  }
  
  \newcommand\gramterm[1]{#1}
  \newcommand\gramnonterm[1]{\IfBlankF{#1}{\ensuremath{\langle\textnormal{#1}\rangle}}}
  \newcommand\gramfunc[1]{\gramnonterm{#1}}
  \newcommand\gramdef{\ensuremath{\Coloneqq}}
  \newcommand\grambar{\ensuremath{|}}
  \newcommand\grambaranon{\ensuremath{|}}
  \newcommand\grameps{\ensuremath{\epsilon}}
  
  \SetBNFConfig{
    relation-sym-map={{::=}={\gramdef}},
    or-sym={\grambar}
  }
  
  \NewDocumentCommand\ACTION{s}{\def\tmp{ACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ATTRIBUTE{s}{\def\tmp{ATTRIBUTE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\BAR{s}{\def\tmp{BAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLON{s}{\def\tmp{COLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLONEQ{s}{\def\tmp{COLONEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COMMA{s}{\def\tmp{COMMA}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EOF{s}{\def\tmp{EOF}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQ{s}{\def\tmp{EQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQEQ{s}{\def\tmp{EQEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\INLINE{s}{\def\tmp{INLINE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LET{s}{\def\tmp{LET}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LID{s}{\def\tmp{LID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LPAR{s}{\def\tmp{LPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\MERGE{s}{\def\tmp{MERGE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\OPT{s}{\def\tmp{OPT}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PLUS{s}{\def\tmp{PLUS}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\POINTFREEACTION{s}{\def\tmp{POINTFREEACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PREC{s}{\def\tmp{PREC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PUBLIC{s}{\def\tmp{PUBLIC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\QID{s}{\def\tmp{QID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\RPAR{s}{\def\tmp{RPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\SEMICOLON{s}{\def\tmp{SEMICOLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\STAR{s}{\def\tmp{STAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\TILDE{s}{\def\tmp{TILDE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UID{s}{\def\tmp{UID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UNDERSCORE{s}{\def\tmp{UNDERSCORE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\action{s}{\def\tmp{action}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actionUNDERSCOREexpression{s}{\def\tmp{action\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actual{s}{\def\tmp{actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\binder{s}{\def\tmp{binder}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\expression{s}{\def\tmp{expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\flags{s}{\def\tmp{flags}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\group{s}{\def\tmp{group}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ident{s}{\def\tmp{ident}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\laxUNDERSCOREactual{s}{\def\tmp{lax\_actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mergeUNDERSCOREfunction{s}{\def\tmp{merge\_function}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\modifier{s}{\def\tmp{modifier}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\newUNDERSCORErule{s}{\def\tmp{new\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\oldUNDERSCORErule{s}{\def\tmp{old\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\optionalUNDERSCOREbar{s}{\def\tmp{optional\_bar}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\pattern{s}{\def\tmp{pattern}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\precedence{s}{\def\tmp{precedence}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\producer{s}{\def\tmp{producer}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\production{s}{\def\tmp{production}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\rule{s}{\def\tmp{rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\seqUNDERSCOREexpression{s}{\def\tmp{seq\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\specification{s}{\def\tmp{specification}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\symbolUNDERSCOREexpression{s}{\def\tmp{symbol\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\genericUNDERSCOREactual{s}{\def\tmp{generic\_actual}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  \NewDocumentCommand\parameters{s}{\def\tmp{parameters}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  
  \NewDocumentCommand\gramopt{sm}{\def\tmp{[#2]}\IfBooleanTF#1{\tmp}{\tmp}}
  \NewDocumentCommand\gramplus{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^+}}
  \NewDocumentCommand\gramstar{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^*}}
  \NewDocumentCommand\gramseplist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^*}}
  \NewDocumentCommand\gramsepnelist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^+}}
  \newcommand\gramparen[1]{(#1)}
  
  \begin{document}
  
  \begin{obeliskgrammar}
  \specification{} : ::= \gramstar{\rule*{}} \EOF*{}
  ;;
  \rule{} : ::= \oldUNDERSCORErule*{}
    | \newUNDERSCORErule*{}
  ;;
  \oldUNDERSCORErule{} : ::= \gramopt{\flags*{}} \ident*{}
                             \gramstar{\ATTRIBUTE*{}}
                             \gramfunc{\parameters{}(\ident*{})} \COLON*{}
                             \optionalUNDERSCOREbar*{}
                             \gramsepnelist{\BAR*{}}{\group*{}}
                             \gramopt{\mergeUNDERSCOREfunction*{}}
                             \gramstar{\SEMICOLON*{}}
  ;;
  \flags{} : ::= \PUBLIC*{}
    | \INLINE*{}
    | \PUBLIC*{} \INLINE*{}
    | \INLINE*{} \PUBLIC*{}
  ;;
  \optionalUNDERSCOREbar{} : ::= \gramopt{\BAR*{}}
  ;;
  \group{} : ::= \gramsepnelist{\BAR*{}}{\production*{}} \ACTION*{}
                 \gramopt{\precedence*{}} \gramstar{\ATTRIBUTE*{}}
  ;;
  \production{} : ::= \gramstar{\producer*{}} \gramopt{\precedence*{}}
  ;;
  \producer{} : ::= \gramopt*{\LID*{} \EQ*{}} \actual*{}
                    \gramstar{\ATTRIBUTE*{}} \gramstar{\SEMICOLON*{}}
  ;;
  \genericUNDERSCOREactual{}(A, B) : ::= \ident*{} \gramfunc{\parameters{}(A)}
    | B \modifier*{}
  ;;
  \actual{} : ::= \gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},
                  \actual*{})}
  ;;
  \laxUNDERSCOREactual{} : ::= \gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},
                               \actual*{})}
    | \gramsepnelist{\BAR*{}}{\group*{}}
  ;;
  \newUNDERSCORErule{} : ::= \gramopt{\PUBLIC*{}} \LET*{} \LID*{}
                             \gramstar{\ATTRIBUTE*{}}
                             \gramfunc{\parameters{}(\ident*{})} \binder*{}
                             \expression*{}
                             \gramopt{\mergeUNDERSCOREfunction*{}}
  ;;
  \binder{} : ::= \COLONEQ*{}
    | \EQEQ*{}
  ;;
  \expression{} : ::= \optionalUNDERSCOREbar*{}
                      \gramsepnelist{\BAR*{}}{\seqUNDERSCOREexpression*{}}
  ;;
  \seqUNDERSCOREexpression{} : ::= \gramopt*{\pattern*{} \EQ*{}}
                                   \symbolUNDERSCOREexpression*{} \SEMICOLON*{}
                                   \seqUNDERSCOREexpression*{}
    | \symbolUNDERSCOREexpression*{}
    | \actionUNDERSCOREexpression*{}
  ;;
  \symbolUNDERSCOREexpression{} : ::= \ident*{}
                                      \gramfunc{\parameters{}(\expression*{})}
                                      \gramstar{\ATTRIBUTE*{}}
    | \symbolUNDERSCOREexpression*{} \modifier*{} \gramstar{\ATTRIBUTE*{}}
  ;;
  \actionUNDERSCOREexpression{} : ::= \action*{} \gramstar{\ATTRIBUTE*{}}
    | \action*{} \precedence*{} \gramstar{\ATTRIBUTE*{}}
    | \precedence*{} \action*{} \gramstar{\ATTRIBUTE*{}}
  ;;
  \action{} : ::= \ACTION*{}
    | \POINTFREEACTION*{}
  ;;
  \pattern{} : ::= \LID*{}
    | \UNDERSCORE*{}
    | \TILDE*{}
    | \LPAR*{} \gramseplist{\COMMA*{}}{\pattern*{}} \RPAR*{}
  ;;
  \modifier{} : ::= \OPT*{}
    | \PLUS*{}
    | \STAR*{}
  ;;
  \mergeUNDERSCOREfunction{} : ::= \MERGE*{} \ACTION*{}
  ;;
  \precedence{} : ::= \PREC*{} \ident*{}
  ;;
  \parameters{}(X) : ::= \gramopt*{\LPAR*{} \gramseplist{\COMMA*{}}{X}
                         \RPAR*{}}
  ;;
  \ident{} : ::= \UID*{}
    | \LID*{}
    | \QID*{}
  ;;
  \end{obeliskgrammar}
  
  \end{document}

  $ obelisk latex -syntax ../../src/parser.mly
  \documentclass[preview]{standalone}
  
  \usepackage[T1]{fontenc}
  
  \usepackage{xparse}
  \usepackage{syntax}
  
  \newenvironment{obeliskgrammar}{\begin{grammar}}{\end{grammar}}
  
  \newcommand\gramterm[1]{\lit{#1}}
  \newcommand\gramnonterm[1]{\synt{#1}}
  \newcommand\gramfunc[1]{\gramnonterm{#1}}
  \newcommand\gramdef{::=}
  \newcommand\grambar{\alt}
  \newcommand\grambaranon{\ensuremath{|}}
  \newcommand\grameps{\ensuremath{\epsilon}}
  \renewcommand\grammarlabel[2]{\gramnonterm{#1} #2}
  \newlength{\grammaxindent}
  \settowidth{\grammaxindent}{\synt{generic\_actual(A, B)} \gramdef{} }
  
  \NewDocumentCommand\ACTION{s}{\def\tmp{ACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ATTRIBUTE{s}{\def\tmp{ATTRIBUTE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\BAR{s}{\def\tmp{BAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLON{s}{\def\tmp{COLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLONEQ{s}{\def\tmp{COLONEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COMMA{s}{\def\tmp{COMMA}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EOF{s}{\def\tmp{EOF}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQ{s}{\def\tmp{EQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQEQ{s}{\def\tmp{EQEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\INLINE{s}{\def\tmp{INLINE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LET{s}{\def\tmp{LET}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LID{s}{\def\tmp{LID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LPAR{s}{\def\tmp{LPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\MERGE{s}{\def\tmp{MERGE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\OPT{s}{\def\tmp{OPT}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PLUS{s}{\def\tmp{PLUS}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\POINTFREEACTION{s}{\def\tmp{POINTFREEACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PREC{s}{\def\tmp{PREC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PUBLIC{s}{\def\tmp{PUBLIC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\QID{s}{\def\tmp{QID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\RPAR{s}{\def\tmp{RPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\SEMICOLON{s}{\def\tmp{SEMICOLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\STAR{s}{\def\tmp{STAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\TILDE{s}{\def\tmp{TILDE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UID{s}{\def\tmp{UID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UNDERSCORE{s}{\def\tmp{UNDERSCORE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\action{s}{\def\tmp{action}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actionUNDERSCOREexpression{s}{\def\tmp{action\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actual{s}{\def\tmp{actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\binder{s}{\def\tmp{binder}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\expression{s}{\def\tmp{expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\flags{s}{\def\tmp{flags}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\group{s}{\def\tmp{group}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ident{s}{\def\tmp{ident}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\laxUNDERSCOREactual{s}{\def\tmp{lax\_actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mergeUNDERSCOREfunction{s}{\def\tmp{merge\_function}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\modifier{s}{\def\tmp{modifier}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\newUNDERSCORErule{s}{\def\tmp{new\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\oldUNDERSCORErule{s}{\def\tmp{old\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\optionalUNDERSCOREbar{s}{\def\tmp{optional\_bar}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\pattern{s}{\def\tmp{pattern}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\precedence{s}{\def\tmp{precedence}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\producer{s}{\def\tmp{producer}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\production{s}{\def\tmp{production}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\rule{s}{\def\tmp{rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\seqUNDERSCOREexpression{s}{\def\tmp{seq\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\specification{s}{\def\tmp{specification}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\symbolUNDERSCOREexpression{s}{\def\tmp{symbol\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\genericUNDERSCOREactual{s}{\def\tmp{generic\_actual}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  \NewDocumentCommand\parameters{s}{\def\tmp{parameters}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  
  \NewDocumentCommand\gramopt{sm}{\def\tmp{[#2]}\IfBooleanTF#1{\tmp}{\tmp}}
  \NewDocumentCommand\gramplus{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^+}}
  \NewDocumentCommand\gramstar{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^*}}
  \NewDocumentCommand\gramseplist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^*}}
  \NewDocumentCommand\gramsepnelist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^+}}
  \newcommand\gramparen[1]{(#1)}
  
  \begin{document}
  
  \setlength{\grammarindent}{\grammaxindent}\begin{obeliskgrammar}
  <\specification{}> \gramdef{} \gramstar{\rule*{}} \EOF*{}
  
  <\rule{}> \gramdef{} \oldUNDERSCORErule*{}
    \grambar \newUNDERSCORErule*{}
  
  <\oldUNDERSCORErule{}> \gramdef{} \gramopt{\flags*{}} \ident*{}
                                    \gramstar{\ATTRIBUTE*{}}
                                    \gramfunc{\parameters{}(\ident*{})}
                                    \COLON*{} \optionalUNDERSCOREbar*{}
                                    \gramsepnelist{\BAR*{}}{\group*{}}
                                    \gramopt{\mergeUNDERSCOREfunction*{}}
                                    \gramstar{\SEMICOLON*{}}
  
  <\flags{}> \gramdef{} \PUBLIC*{}
    \grambar \INLINE*{}
    \grambar \PUBLIC*{} \INLINE*{}
    \grambar \INLINE*{} \PUBLIC*{}
  
  <\optionalUNDERSCOREbar{}> \gramdef{} \gramopt{\BAR*{}}
  
  <\group{}> \gramdef{} \gramsepnelist{\BAR*{}}{\production*{}} \ACTION*{}
                        \gramopt{\precedence*{}} \gramstar{\ATTRIBUTE*{}}
  
  <\production{}> \gramdef{} \gramstar{\producer*{}} \gramopt{\precedence*{}}
  
  <\producer{}> \gramdef{} \gramopt*{\LID*{} \EQ*{}} \actual*{}
                           \gramstar{\ATTRIBUTE*{}} \gramstar{\SEMICOLON*{}}
  
  <\genericUNDERSCOREactual{}(A, B)> \gramdef{} \ident*{}
                                                \gramfunc{\parameters{}(A)}
    \grambar B \modifier*{}
  
  <\actual{}> \gramdef{} \gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},
                         \actual*{})}
  
  <\laxUNDERSCOREactual{}> \gramdef{} \gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},
                                      \actual*{})}
    \grambar \gramsepnelist{\BAR*{}}{\group*{}}
  
  <\newUNDERSCORErule{}> \gramdef{} \gramopt{\PUBLIC*{}} \LET*{} \LID*{}
                                    \gramstar{\ATTRIBUTE*{}}
                                    \gramfunc{\parameters{}(\ident*{})}
                                    \binder*{} \expression*{}
                                    \gramopt{\mergeUNDERSCOREfunction*{}}
  
  <\binder{}> \gramdef{} \COLONEQ*{}
    \grambar \EQEQ*{}
  
  <\expression{}> \gramdef{} \optionalUNDERSCOREbar*{}
                             \gramsepnelist{\BAR*{}}{\seqUNDERSCOREexpression*{}}
  
  <\seqUNDERSCOREexpression{}> \gramdef{} \gramopt*{\pattern*{} \EQ*{}}
                                          \symbolUNDERSCOREexpression*{}
                                          \SEMICOLON*{}
                                          \seqUNDERSCOREexpression*{}
    \grambar \symbolUNDERSCOREexpression*{}
    \grambar \actionUNDERSCOREexpression*{}
  
  <\symbolUNDERSCOREexpression{}> \gramdef{} \ident*{}
                                             \gramfunc{\parameters{}(\expression*{})}
                                             \gramstar{\ATTRIBUTE*{}}
    \grambar \symbolUNDERSCOREexpression*{} \modifier*{}
             \gramstar{\ATTRIBUTE*{}}
  
  <\actionUNDERSCOREexpression{}> \gramdef{} \action*{}
                                             \gramstar{\ATTRIBUTE*{}}
    \grambar \action*{} \precedence*{} \gramstar{\ATTRIBUTE*{}}
    \grambar \precedence*{} \action*{} \gramstar{\ATTRIBUTE*{}}
  
  <\action{}> \gramdef{} \ACTION*{}
    \grambar \POINTFREEACTION*{}
  
  <\pattern{}> \gramdef{} \LID*{}
    \grambar \UNDERSCORE*{}
    \grambar \TILDE*{}
    \grambar \LPAR*{} \gramseplist{\COMMA*{}}{\pattern*{}} \RPAR*{}
  
  <\modifier{}> \gramdef{} \OPT*{}
    \grambar \PLUS*{}
    \grambar \STAR*{}
  
  <\mergeUNDERSCOREfunction{}> \gramdef{} \MERGE*{} \ACTION*{}
  
  <\precedence{}> \gramdef{} \PREC*{} \ident*{}
  
  <\parameters{}(X)> \gramdef{} \gramopt*{\LPAR*{} \gramseplist{\COMMA*{}}{X}
                                \RPAR*{}}
  
  <\ident{}> \gramdef{} \UID*{}
    \grambar \LID*{}
    \grambar \QID*{}
  
  \end{obeliskgrammar}
  
  \end{document}

  $ obelisk latex -backnaur ../../src/parser.mly
  \documentclass[preview]{standalone}
  
  \usepackage[T1]{fontenc}
  
  \usepackage{xparse}
  \usepackage[epsilon]{backnaur}
  
  \newenvironment{obeliskgrammar}{\begin{bnf*}}{\end{bnf*}}
  
  \newcommand\gramsp{\ensuremath{\bnfsp}}
  \newcommand\gramterm[1]{\bnfts{#1}}
  \newcommand\gramnonterm[1]{\ensuremath{\bnfpn{#1}}}
  \newcommand\gramfunc[1]{\ensuremath{\bnfpn{#1}}}
  \newcommand\grameps{\ensuremath{\bnfes}}
  \newcommand\gramprod[3][\textwidth]{\bnfprod{#2}{%
    \begin{minipage}[t]{#1}
      $#3$
    \end{minipage}}}
  \newcommand\grambar{\hspace*{-2.5em}\bnfor\hspace*{1.2em}}
  \newcommand\grambaranon{\ensuremath{\bnfor}}
  
  \NewDocumentCommand\ACTION{s}{\def\tmp{ACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ATTRIBUTE{s}{\def\tmp{ATTRIBUTE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\BAR{s}{\def\tmp{BAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLON{s}{\def\tmp{COLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COLONEQ{s}{\def\tmp{COLONEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\COMMA{s}{\def\tmp{COMMA}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EOF{s}{\def\tmp{EOF}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQ{s}{\def\tmp{EQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\EQEQ{s}{\def\tmp{EQEQ}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\INLINE{s}{\def\tmp{INLINE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LET{s}{\def\tmp{LET}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LID{s}{\def\tmp{LID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\LPAR{s}{\def\tmp{LPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\MERGE{s}{\def\tmp{MERGE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\OPT{s}{\def\tmp{OPT}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PLUS{s}{\def\tmp{PLUS}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\POINTFREEACTION{s}{\def\tmp{POINTFREEACTION}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PREC{s}{\def\tmp{PREC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\PUBLIC{s}{\def\tmp{PUBLIC}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\QID{s}{\def\tmp{QID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\RPAR{s}{\def\tmp{RPAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\SEMICOLON{s}{\def\tmp{SEMICOLON}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\STAR{s}{\def\tmp{STAR}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\TILDE{s}{\def\tmp{TILDE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UID{s}{\def\tmp{UID}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\UNDERSCORE{s}{\def\tmp{UNDERSCORE}\IfBooleanTF#1{\gramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\action{s}{\def\tmp{action}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actionUNDERSCOREexpression{s}{\def\tmp{action\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\actual{s}{\def\tmp{actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\binder{s}{\def\tmp{binder}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\expression{s}{\def\tmp{expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\flags{s}{\def\tmp{flags}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\group{s}{\def\tmp{group}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\ident{s}{\def\tmp{ident}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\laxUNDERSCOREactual{s}{\def\tmp{lax\_actual}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mergeUNDERSCOREfunction{s}{\def\tmp{merge\_function}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\modifier{s}{\def\tmp{modifier}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\newUNDERSCORErule{s}{\def\tmp{new\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\oldUNDERSCORErule{s}{\def\tmp{old\_rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\optionalUNDERSCOREbar{s}{\def\tmp{optional\_bar}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\pattern{s}{\def\tmp{pattern}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\precedence{s}{\def\tmp{precedence}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\producer{s}{\def\tmp{producer}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\production{s}{\def\tmp{production}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\rule{s}{\def\tmp{rule}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\seqUNDERSCOREexpression{s}{\def\tmp{seq\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\specification{s}{\def\tmp{specification}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\symbolUNDERSCOREexpression{s}{\def\tmp{symbol\_expression}\IfBooleanTF#1{\gramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\genericUNDERSCOREactual{s}{\def\tmp{generic\_actual}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  \NewDocumentCommand\parameters{s}{\def\tmp{parameters}\IfBooleanTF#1{\gramfunc{\tmp}}{\tmp}}
  
  \NewDocumentCommand\gramopt{sm}{\def\tmp{[#2]}\IfBooleanTF#1{\tmp}{\tmp}}
  \NewDocumentCommand\gramplus{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^+}}
  \NewDocumentCommand\gramstar{sm}{\def\tmp{#2}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{^*}}
  \NewDocumentCommand\gramseplist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^*}}
  \NewDocumentCommand\gramsepnelist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\gramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^+}}
  \newcommand\gramparen[1]{(#1)}
  
  \begin{document}
  
  \begin{obeliskgrammar}
  \gramprod{\specification{}}{\gramstar{\rule*{}}\gramsp \EOF*{}}\\
  \gramprod{\rule{}}{\oldUNDERSCORErule*{}\\
    \grambar \newUNDERSCORErule*{}}\\
  \gramprod{\oldUNDERSCORErule{}}{\gramopt{\flags*{}}\gramsp \ident*{}\gramsp
                                  \gramstar{\ATTRIBUTE*{}}\gramsp
                                  \gramfunc{\parameters{}(\ident*{})}\gramsp
                                  \COLON*{}\gramsp
                                  \optionalUNDERSCOREbar*{}\gramsp
                                  \gramsepnelist{\BAR*{}}{\group*{}}\gramsp
                                  \gramopt{\mergeUNDERSCOREfunction*{}}\gramsp
                                  \gramstar{\SEMICOLON*{}}}\\
  \gramprod{\flags{}}{\PUBLIC*{}\\
    \grambar \INLINE*{}\\
    \grambar \PUBLIC*{}\gramsp \INLINE*{}\\
    \grambar \INLINE*{}\gramsp \PUBLIC*{}}\\
  \gramprod{\optionalUNDERSCOREbar{}}{\gramopt{\BAR*{}}}\\
  \gramprod{\group{}}{\gramsepnelist{\BAR*{}}{\production*{}}\gramsp
                      \ACTION*{}\gramsp \gramopt{\precedence*{}}\gramsp
                      \gramstar{\ATTRIBUTE*{}}}\\
  \gramprod{\production{}}{\gramstar{\producer*{}}\gramsp
                           \gramopt{\precedence*{}}}\\
  \gramprod{\producer{}}{\gramopt*{\LID*{}\gramsp \EQ*{}}\gramsp
                         \actual*{}\gramsp \gramstar{\ATTRIBUTE*{}}\gramsp
                         \gramstar{\SEMICOLON*{}}}\\
  \gramprod{\genericUNDERSCOREactual{}(A, B)}{\ident*{}\gramsp
                                              \gramfunc{\parameters{}(A)}\\
    \grambar B\gramsp \modifier*{}}\\
  \gramprod{\actual{}}{\gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},\gramsp
                       \actual*{})}}\\
  \gramprod{\laxUNDERSCOREactual{}}{\gramfunc{\genericUNDERSCOREactual{}(\laxUNDERSCOREactual*{},\gramsp
                                    \actual*{})}\\
    \grambar \gramsepnelist{\BAR*{}}{\group*{}}}\\
  \gramprod{\newUNDERSCORErule{}}{\gramopt{\PUBLIC*{}}\gramsp \LET*{}\gramsp
                                  \LID*{}\gramsp
                                  \gramstar{\ATTRIBUTE*{}}\gramsp
                                  \gramfunc{\parameters{}(\ident*{})}\gramsp
                                  \binder*{}\gramsp \expression*{}\gramsp
                                  \gramopt{\mergeUNDERSCOREfunction*{}}}\\
  \gramprod{\binder{}}{\COLONEQ*{}\\
    \grambar \EQEQ*{}}\\
  \gramprod{\expression{}}{\optionalUNDERSCOREbar*{}\gramsp
                           \gramsepnelist{\BAR*{}}{\seqUNDERSCOREexpression*{}}}\\
  \gramprod{\seqUNDERSCOREexpression{}}{\gramopt*{\pattern*{}\gramsp
                                        \EQ*{}}\gramsp
                                        \symbolUNDERSCOREexpression*{}\gramsp
                                        \SEMICOLON*{}\gramsp
                                        \seqUNDERSCOREexpression*{}\\
    \grambar \symbolUNDERSCOREexpression*{}\\
    \grambar \actionUNDERSCOREexpression*{}}\\
  \gramprod{\symbolUNDERSCOREexpression{}}{\ident*{}\gramsp
                                           \gramfunc{\parameters{}(\expression*{})}\gramsp
                                           \gramstar{\ATTRIBUTE*{}}\\
    \grambar \symbolUNDERSCOREexpression*{}\gramsp \modifier*{}\gramsp
             \gramstar{\ATTRIBUTE*{}}}\\
  \gramprod{\actionUNDERSCOREexpression{}}{\action*{}\gramsp
                                           \gramstar{\ATTRIBUTE*{}}\\
    \grambar \action*{}\gramsp \precedence*{}\gramsp \gramstar{\ATTRIBUTE*{}}\\
    \grambar \precedence*{}\gramsp \action*{}\gramsp \gramstar{\ATTRIBUTE*{}}}\\
  \gramprod{\action{}}{\ACTION*{}\\
    \grambar \POINTFREEACTION*{}}\\
  \gramprod{\pattern{}}{\LID*{}\\
    \grambar \UNDERSCORE*{}\\
    \grambar \TILDE*{}\\
    \grambar \LPAR*{}\gramsp \gramseplist{\COMMA*{}}{\pattern*{}}\gramsp
             \RPAR*{}}\\
  \gramprod{\modifier{}}{\OPT*{}\\
    \grambar \PLUS*{}\\
    \grambar \STAR*{}}\\
  \gramprod{\mergeUNDERSCOREfunction{}}{\MERGE*{}\gramsp \ACTION*{}}\\
  \gramprod{\precedence{}}{\PREC*{}\gramsp \ident*{}}\\
  \gramprod{\parameters{}(X)}{\gramopt*{\LPAR*{}\gramsp
                              \gramseplist{\COMMA*{}}{X}\gramsp \RPAR*{}}}\\
  \gramprod{\ident{}}{\UID*{}\\
    \grambar \LID*{}\\
    \grambar \QID*{}}\\
  \end{obeliskgrammar}
  
  \end{document}

  $ obelisk latex -prefix my -o grammar.tex -package definitions ../../src/parser.mly
  $ cat grammar.tex
  \begin{obeliskgrammar}
  \myspecification{}& \mygramdef & \mygramstar{\myrule*{}} \myEOF*{}
    \\
  
  \myrule{}& \mygramdef & \myoldUNDERSCORErule*{}\\
    & \mygrambar &\mynewUNDERSCORErule*{}
    \\
  
  \myoldUNDERSCORErule{}& \mygramdef & \mygramopt{\myflags*{}} \myident*{}
                                       \mygramstar{\myATTRIBUTE*{}}
                                       \mygramfunc{\myparameters{}(\myident*{})}
                                       \myCOLON*{} \myoptionalUNDERSCOREbar*{}
                                       \mygramsepnelist{\myBAR*{}}{\mygroup*{}}
                                       \mygramopt{\mymergeUNDERSCOREfunction*{}}
                                       \mygramstar{\mySEMICOLON*{}}
    \\
  
  \myflags{}& \mygramdef & \myPUBLIC*{}\\
    & \mygrambar &\myINLINE*{}\\
    & \mygrambar &\myPUBLIC*{} \myINLINE*{}\\
    & \mygrambar &\myINLINE*{} \myPUBLIC*{}
    \\
  
  \myoptionalUNDERSCOREbar{}& \mygramdef & \mygramopt{\myBAR*{}}
    \\
  
  \mygroup{}& \mygramdef & \mygramsepnelist{\myBAR*{}}{\myproduction*{}}
                           \myACTION*{} \mygramopt{\myprecedence*{}}
                           \mygramstar{\myATTRIBUTE*{}}
    \\
  
  \myproduction{}& \mygramdef & \mygramstar{\myproducer*{}}
                                \mygramopt{\myprecedence*{}}
    \\
  
  \myproducer{}& \mygramdef & \mygramopt*{\myLID*{} \myEQ*{}} \myactual*{}
                              \mygramstar{\myATTRIBUTE*{}}
                              \mygramstar{\mySEMICOLON*{}}
    \\
  
  \mygenericUNDERSCOREactual{}(A, B)& \mygramdef & \myident*{}
                                                   \mygramfunc{\myparameters{}(A)}\\
    & \mygrambar &B \mymodifier*{}
    \\
  
  \myactual{}& \mygramdef & \mygramfunc{\mygenericUNDERSCOREactual{}(\mylaxUNDERSCOREactual*{},
                            \myactual*{})}
    \\
  
  \mylaxUNDERSCOREactual{}& \mygramdef & \mygramfunc{\mygenericUNDERSCOREactual{}(\mylaxUNDERSCOREactual*{},
                                         \myactual*{})}\\
    & \mygrambar &\mygramsepnelist{\myBAR*{}}{\mygroup*{}}
    \\
  
  \mynewUNDERSCORErule{}& \mygramdef & \mygramopt{\myPUBLIC*{}} \myLET*{}
                                       \myLID*{} \mygramstar{\myATTRIBUTE*{}}
                                       \mygramfunc{\myparameters{}(\myident*{})}
                                       \mybinder*{} \myexpression*{}
                                       \mygramopt{\mymergeUNDERSCOREfunction*{}}
    \\
  
  \mybinder{}& \mygramdef & \myCOLONEQ*{}\\
    & \mygrambar &\myEQEQ*{}
    \\
  
  \myexpression{}& \mygramdef & \myoptionalUNDERSCOREbar*{}
                                \mygramsepnelist{\myBAR*{}}{\myseqUNDERSCOREexpression*{}}
    \\
  
  \myseqUNDERSCOREexpression{}& \mygramdef & \mygramopt*{\mypattern*{}
                                             \myEQ*{}}
                                             \mysymbolUNDERSCOREexpression*{}
                                             \mySEMICOLON*{}
                                             \myseqUNDERSCOREexpression*{}\\
    & \mygrambar &\mysymbolUNDERSCOREexpression*{}\\
    & \mygrambar &\myactionUNDERSCOREexpression*{}
    \\
  
  \mysymbolUNDERSCOREexpression{}& \mygramdef & \myident*{}
                                                \mygramfunc{\myparameters{}(\myexpression*{})}
                                                \mygramstar{\myATTRIBUTE*{}}\\
    & \mygrambar &\mysymbolUNDERSCOREexpression*{} \mymodifier*{}
                  \mygramstar{\myATTRIBUTE*{}}
    \\
  
  \myactionUNDERSCOREexpression{}& \mygramdef & \myaction*{}
                                                \mygramstar{\myATTRIBUTE*{}}\\
    & \mygrambar &\myaction*{} \myprecedence*{} \mygramstar{\myATTRIBUTE*{}}\\
    & \mygrambar &\myprecedence*{} \myaction*{} \mygramstar{\myATTRIBUTE*{}}
    \\
  
  \myaction{}& \mygramdef & \myACTION*{}\\
    & \mygrambar &\myPOINTFREEACTION*{}
    \\
  
  \mypattern{}& \mygramdef & \myLID*{}\\
    & \mygrambar &\myUNDERSCORE*{}\\
    & \mygrambar &\myTILDE*{}\\
    & \mygrambar &\myLPAR*{} \mygramseplist{\myCOMMA*{}}{\mypattern*{}}
                  \myRPAR*{}
    \\
  
  \mymodifier{}& \mygramdef & \myOPT*{}\\
    & \mygrambar &\myPLUS*{}\\
    & \mygrambar &\mySTAR*{}
    \\
  
  \mymergeUNDERSCOREfunction{}& \mygramdef & \myMERGE*{} \myACTION*{}
    \\
  
  \myprecedence{}& \mygramdef & \myPREC*{} \myident*{}
    \\
  
  \myparameters{}(X)& \mygramdef & \mygramopt*{\myLPAR*{}
                                   \mygramseplist{\myCOMMA*{}}{X} \myRPAR*{}}
    \\
  
  \myident{}& \mygramdef & \myUID*{}\\
    & \mygrambar &\myLID*{}\\
    & \mygrambar &\myQID*{}
    \\
  
  \end{obeliskgrammar}

  $ cat definitions.sty
  \NeedsTeXFormat{LaTeX2e}
  \ProvidesPackage{definitions}
  
  \RequirePackage{xparse}
  \RequirePackage{tabularray}
  
  \NewTblrTheme{obeliskgrammar}{
    \DefTblrTemplate{caption}{default}{}
    \DefTblrTemplate{capcont}{default}{}
    \DefTblrTemplate{conthead}{default}{}
    \DefTblrTemplate{contfoot}{default}{}
  }
  \NewDocumentEnvironment{obeliskgrammar}{+b}{
    \begin{longtblr}[theme=obeliskgrammar, entry=none, label=none]{colspec={@{}r@{}c@{}X@{}}, column{1}={cmd=\mygramnonterm}}
    #1
    \end{longtblr}
  }{}
  
  \newcommand\mygramsp{\quad}
  \newcommand\mygramdef{$\mygramsp::=\mygramsp$}
  \newcommand\mygrambar{$\mygramsp|\mygramsp$}
  \newcommand\mygrambaranon{$|$}
  \newcommand\mygrameps{\ensuremath{\epsilon}}
  \newcommand\mygramnonterm[1]{\IfBlankF{#1}{\ensuremath{\langle\textnormal{#1}\rangle}}}
  \newcommand\mygramfunc[1]{\mygramnonterm{#1}}
  \newcommand\mygramterm[1]{#1}
  
  \NewDocumentCommand\myACTION{s}{\def\tmp{ACTION}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myATTRIBUTE{s}{\def\tmp{ATTRIBUTE}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myBAR{s}{\def\tmp{BAR}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myCOLON{s}{\def\tmp{COLON}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myCOLONEQ{s}{\def\tmp{COLONEQ}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myCOMMA{s}{\def\tmp{COMMA}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myEOF{s}{\def\tmp{EOF}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myEQ{s}{\def\tmp{EQ}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myEQEQ{s}{\def\tmp{EQEQ}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myINLINE{s}{\def\tmp{INLINE}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myLET{s}{\def\tmp{LET}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myLID{s}{\def\tmp{LID}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myLPAR{s}{\def\tmp{LPAR}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myMERGE{s}{\def\tmp{MERGE}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myOPT{s}{\def\tmp{OPT}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myPLUS{s}{\def\tmp{PLUS}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myPOINTFREEACTION{s}{\def\tmp{POINTFREEACTION}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myPREC{s}{\def\tmp{PREC}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myPUBLIC{s}{\def\tmp{PUBLIC}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myQID{s}{\def\tmp{QID}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myRPAR{s}{\def\tmp{RPAR}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mySEMICOLON{s}{\def\tmp{SEMICOLON}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mySTAR{s}{\def\tmp{STAR}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myTILDE{s}{\def\tmp{TILDE}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myUID{s}{\def\tmp{UID}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myUNDERSCORE{s}{\def\tmp{UNDERSCORE}\IfBooleanTF#1{\mygramterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myaction{s}{\def\tmp{action}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myactionUNDERSCOREexpression{s}{\def\tmp{action\_expression}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myactual{s}{\def\tmp{actual}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mybinder{s}{\def\tmp{binder}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myexpression{s}{\def\tmp{expression}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myflags{s}{\def\tmp{flags}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mygroup{s}{\def\tmp{group}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myident{s}{\def\tmp{ident}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mylaxUNDERSCOREactual{s}{\def\tmp{lax\_actual}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mymergeUNDERSCOREfunction{s}{\def\tmp{merge\_function}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mymodifier{s}{\def\tmp{modifier}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mynewUNDERSCORErule{s}{\def\tmp{new\_rule}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myoldUNDERSCORErule{s}{\def\tmp{old\_rule}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myoptionalUNDERSCOREbar{s}{\def\tmp{optional\_bar}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mypattern{s}{\def\tmp{pattern}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myprecedence{s}{\def\tmp{precedence}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myproducer{s}{\def\tmp{producer}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myproduction{s}{\def\tmp{production}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myrule{s}{\def\tmp{rule}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myseqUNDERSCOREexpression{s}{\def\tmp{seq\_expression}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\myspecification{s}{\def\tmp{specification}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mysymbolUNDERSCOREexpression{s}{\def\tmp{symbol\_expression}\IfBooleanTF#1{\mygramnonterm{\tmp}}{\tmp}}
  \NewDocumentCommand\mygenericUNDERSCOREactual{s}{\def\tmp{generic\_actual}\IfBooleanTF#1{\mygramfunc{\tmp}}{\tmp}}
  \NewDocumentCommand\myparameters{s}{\def\tmp{parameters}\IfBooleanTF#1{\mygramfunc{\tmp}}{\tmp}}
  
  \NewDocumentCommand\mygramopt{sm}{\def\tmp{[#2]}\IfBooleanTF#1{\tmp}{\tmp}}
  \NewDocumentCommand\mygramplus{sm}{\def\tmp{#2}{\IfBooleanTF#1{\mygramparen{\tmp}}{\tmp}}\ensuremath{^+}}
  \NewDocumentCommand\mygramstar{sm}{\def\tmp{#2}{\IfBooleanTF#1{\mygramparen{\tmp}}{\tmp}}\ensuremath{^*}}
  \NewDocumentCommand\mygramseplist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\mygramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^*}}
  \NewDocumentCommand\mygramsepnelist{smm}{\def\tmp{#3}{\IfBooleanTF#1{\mygramparen{\tmp}}{\tmp}}\ensuremath{_{\textnormal{#2}}^+}}
  \newcommand\mygramparen[1]{(#1)}

  $ obelisk html -css ../../src/parser.mly
  <!DOCTYPE html>
  <html>
    <head>
      <title>Grammar</title>
      <style>
        .specification td, th{
            vertical-align: baseline;
            padding: 0;
            margin: 0;
            font-weight: normal;
        }
        .specification td {
            text-align: left;
        }
        .specification th {
            text-align: right;
            white-space: nowrap;
        }
        .specification th::after {
            content: "\a0::=\a0";
        }
        .specification th.bar {
            text-align: right;
        }
        .specification th.bar::after {
            content: "|\a0";
        }
        .rule th, td {
            padding-top: .5em;
        }
        .nonterminal::before {
            content: "<";
        }
        .nonterminal::after {
            content: ">";
        }
        .list::after {
            content: "*";
            vertical-align: super;
            font-size: smaller;
        }
        .ne_list::after {
            content: "+";
            vertical-align: super;
            font-size: smaller;
        }
        .option::before {
            content: "[";
        }
        .option::after {
            content: "]";
        }
      </style>
    </head>
    
    <body>
      
      <table class="specification">
        
        <tr class="rule">
          <th><span class="nonterminal">specification</span></th>
          <td><span class="list"><span class="nonterminal">rule</span></span>
          EOF</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">rule</span></th>
          <td><span class="nonterminal">old_rule</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">new_rule</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">old_rule</span></th>
          <td><span class="option"><span class="nonterminal">flags</span></span>
          <span class="nonterminal">ident</span>
          <span class="list">ATTRIBUTE</span>
          <span class="nonterminal">parameters(<span class="nonterminal">ident</span>)</span>
          COLON
          <span class="nonterminal">optional_bar</span>
          <span class="nonterminal">group</span><sup>+</sup><sub>BAR</sub>
          <span class="option"><span class="nonterminal">merge_function</span></span>
          <span class="list">SEMICOLON</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">flags</span></th>
          <td>PUBLIC</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>INLINE</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>PUBLIC
          INLINE</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>INLINE
          PUBLIC</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">optional_bar</span></th>
          <td><span class="option">BAR</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">group</span></th>
          <td><span class="nonterminal">production</span><sup>+</sup><sub>BAR</sub>
          ACTION
          <span class="option"><span class="nonterminal">precedence</span></span>
          <span class="list">ATTRIBUTE</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">production</span></th>
          <td><span class="list"><span class="nonterminal">producer</span></span>
          <span class="option"><span class="nonterminal">precedence</span></span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">producer</span></th>
          <td><span class="option">LID
          EQ</span>
          <span class="nonterminal">actual</span>
          <span class="list">ATTRIBUTE</span>
          <span class="list">SEMICOLON</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">generic_actual(A, B)</span></th>
          <td><span class="nonterminal">ident</span>
          <span class="nonterminal">parameters(A)</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>B
          <span class="nonterminal">modifier</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">actual</span></th>
          <td><span class="nonterminal">generic_actual(<span class="nonterminal">lax_actual</span>,
          <span class="nonterminal">actual</span>)</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">lax_actual</span></th>
          <td><span class="nonterminal">generic_actual(<span class="nonterminal">lax_actual</span>,
          <span class="nonterminal">actual</span>)</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">group</span><sup>+</sup><sub>BAR</sub></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">new_rule</span></th>
          <td><span class="option">PUBLIC</span>
          LET
          LID
          <span class="list">ATTRIBUTE</span>
          <span class="nonterminal">parameters(<span class="nonterminal">ident</span>)</span>
          <span class="nonterminal">binder</span>
          <span class="nonterminal">expression</span>
          <span class="option"><span class="nonterminal">merge_function</span></span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">binder</span></th>
          <td>COLONEQ</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>EQEQ</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">expression</span></th>
          <td><span class="nonterminal">optional_bar</span>
          <span class="nonterminal">seq_expression</span><sup>+</sup><sub>BAR</sub></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">seq_expression</span></th>
          <td><span class="option"><span class="nonterminal">pattern</span>
          EQ</span>
          <span class="nonterminal">symbol_expression</span>
          SEMICOLON
          <span class="nonterminal">seq_expression</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">symbol_expression</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">action_expression</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">symbol_expression</span></th>
          <td><span class="nonterminal">ident</span>
          <span class="nonterminal">parameters(<span class="nonterminal">expression</span>)</span>
          <span class="list">ATTRIBUTE</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">symbol_expression</span>
          <span class="nonterminal">modifier</span>
          <span class="list">ATTRIBUTE</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">action_expression</span></th>
          <td><span class="nonterminal">action</span>
          <span class="list">ATTRIBUTE</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">action</span>
          <span class="nonterminal">precedence</span>
          <span class="list">ATTRIBUTE</span></td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td><span class="nonterminal">precedence</span>
          <span class="nonterminal">action</span>
          <span class="list">ATTRIBUTE</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">action</span></th>
          <td>ACTION</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>POINTFREEACTION</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">pattern</span></th>
          <td>LID</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>UNDERSCORE</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>TILDE</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>LPAR
          <span class="nonterminal">pattern</span><sup>*</sup><sub>COMMA</sub>
          RPAR</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">modifier</span></th>
          <td>OPT</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>PLUS</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>STAR</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">merge_function</span></th>
          <td>MERGE
          ACTION</td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">precedence</span></th>
          <td>PREC
          <span class="nonterminal">ident</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">parameters(X)</span></th>
          <td><span class="option">LPAR
          X<sup>*</sup><sub>COMMA</sub>
          RPAR</span></td>
        </tr>
        
        
        <tr class="rule">
          <th><span class="nonterminal">ident</span></th>
          <td>UID</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>LID</td>
        </tr>
        <tr>
          <th class="bar"></th>
          <td>QID</td>
        </tr>
        
        
  
      </table>
      
    </body>
    
  </html>

  $ obelisk html -nocss ../../src/parser.mly
  <!DOCTYPE html>
  <html>
    <head>
      <title>Grammar</title>
      <style>
        .specification td, th{
            vertical-align: baseline;
            padding: 0;
            margin: 0;
            font-weight: normal;
        }
        .specification td {
            text-align: left;
        }
        .specification th {
            text-align: right;
            white-space: nowrap;
        }
        .specification th.bar {
            text-align: right;
        }
        .rule th, td {
            padding-top: .5em;
        }
        .list_after {
            vertical-align: super;
            font-size: smaller;
        }
        .ne_list_after {
            vertical-align: super;
            font-size: smaller;
        }
        
      </style>
    </head>
    
    <body>
      
      <table class="specification">
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">specification</span>&gt;&nbsp;::=&nbsp;</th>
          <td><span class="list">&lt;<span class="nonterminal">rule</span>&gt;</span><span class="list_after">*</span>
          EOF</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">rule</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">old_rule</span>&gt;</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">new_rule</span>&gt;</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">old_rule</span>&gt;&nbsp;::=&nbsp;</th>
          <td>[<span class="option">&lt;<span class="nonterminal">flags</span>&gt;</span>]
          &lt;<span class="nonterminal">ident</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span>
          &lt;<span class="nonterminal">parameters(&lt;<span class="nonterminal">ident</span>&gt;)</span>&gt;
          COLON
          &lt;<span class="nonterminal">optional_bar</span>&gt;
          &lt;<span class="nonterminal">group</span>&gt;<sup>+</sup><sub>BAR</sub>
          [<span class="option">&lt;<span class="nonterminal">merge_function</span>&gt;</span>]
          <span class="list">SEMICOLON</span><span class="list_after">*</span></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">flags</span>&gt;&nbsp;::=&nbsp;</th>
          <td>PUBLIC</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>INLINE</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>PUBLIC
          INLINE</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>INLINE
          PUBLIC</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">optional_bar</span>&gt;&nbsp;::=&nbsp;</th>
          <td>[<span class="option">BAR</span>]</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">group</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">production</span>&gt;<sup>+</sup><sub>BAR</sub>
          ACTION
          [<span class="option">&lt;<span class="nonterminal">precedence</span>&gt;</span>]
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">production</span>&gt;&nbsp;::=&nbsp;</th>
          <td><span class="list">&lt;<span class="nonterminal">producer</span>&gt;</span><span class="list_after">*</span>
          [<span class="option">&lt;<span class="nonterminal">precedence</span>&gt;</span>]</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">producer</span>&gt;&nbsp;::=&nbsp;</th>
          <td>[<span class="option">LID
          EQ</span>]
          &lt;<span class="nonterminal">actual</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span>
          <span class="list">SEMICOLON</span><span class="list_after">*</span></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">generic_actual(A, B)</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">ident</span>&gt;
          &lt;<span class="nonterminal">parameters(A)</span>&gt;</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>B
          &lt;<span class="nonterminal">modifier</span>&gt;</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">actual</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">generic_actual(&lt;<span class="nonterminal">lax_actual</span>&gt;,
          &lt;<span class="nonterminal">actual</span>&gt;)</span>&gt;</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">lax_actual</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">generic_actual(&lt;<span class="nonterminal">lax_actual</span>&gt;,
          &lt;<span class="nonterminal">actual</span>&gt;)</span>&gt;</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">group</span>&gt;<sup>+</sup><sub>BAR</sub></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">new_rule</span>&gt;&nbsp;::=&nbsp;</th>
          <td>[<span class="option">PUBLIC</span>]
          LET
          LID
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span>
          &lt;<span class="nonterminal">parameters(&lt;<span class="nonterminal">ident</span>&gt;)</span>&gt;
          &lt;<span class="nonterminal">binder</span>&gt;
          &lt;<span class="nonterminal">expression</span>&gt;
          [<span class="option">&lt;<span class="nonterminal">merge_function</span>&gt;</span>]</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">binder</span>&gt;&nbsp;::=&nbsp;</th>
          <td>COLONEQ</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>EQEQ</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">expression</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">optional_bar</span>&gt;
          &lt;<span class="nonterminal">seq_expression</span>&gt;<sup>+</sup><sub>BAR</sub></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">seq_expression</span>&gt;&nbsp;::=&nbsp;</th>
          <td>[<span class="option">&lt;<span class="nonterminal">pattern</span>&gt;
          EQ</span>]
          &lt;<span class="nonterminal">symbol_expression</span>&gt;
          SEMICOLON
          &lt;<span class="nonterminal">seq_expression</span>&gt;</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">symbol_expression</span>&gt;</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">action_expression</span>&gt;</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">symbol_expression</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">ident</span>&gt;
          &lt;<span class="nonterminal">parameters(&lt;<span class="nonterminal">expression</span>&gt;)</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span></td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">symbol_expression</span>&gt;
          &lt;<span class="nonterminal">modifier</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">action_expression</span>&gt;&nbsp;::=&nbsp;</th>
          <td>&lt;<span class="nonterminal">action</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span></td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">action</span>&gt;
          &lt;<span class="nonterminal">precedence</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span></td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>&lt;<span class="nonterminal">precedence</span>&gt;
          &lt;<span class="nonterminal">action</span>&gt;
          <span class="list">ATTRIBUTE</span><span class="list_after">*</span></td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">action</span>&gt;&nbsp;::=&nbsp;</th>
          <td>ACTION</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>POINTFREEACTION</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">pattern</span>&gt;&nbsp;::=&nbsp;</th>
          <td>LID</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>UNDERSCORE</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>TILDE</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>LPAR
          &lt;<span class="nonterminal">pattern</span>&gt;<sup>*</sup><sub>COMMA</sub>
          RPAR</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">modifier</span>&gt;&nbsp;::=&nbsp;</th>
          <td>OPT</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>PLUS</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>STAR</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">merge_function</span>&gt;&nbsp;::=&nbsp;</th>
          <td>MERGE
          ACTION</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">precedence</span>&gt;&nbsp;::=&nbsp;</th>
          <td>PREC
          &lt;<span class="nonterminal">ident</span>&gt;</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">parameters(X)</span>&gt;&nbsp;::=&nbsp;</th>
          <td>[<span class="option">LPAR
          X<sup>*</sup><sub>COMMA</sub>
          RPAR</span>]</td>
        </tr>
        
        
        <tr class="rule">
          <th>&lt;<span class="nonterminal">ident</span>&gt;&nbsp;::=&nbsp;</th>
          <td>UID</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>LID</td>
        </tr>
        <tr>
          <th class="bar">|&nbsp;</th>
          <td>QID</td>
        </tr>
        
        
  
      </table>
      
    </body>
    
  </html>
