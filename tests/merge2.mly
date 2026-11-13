%token C
%%
X: C {}
%public liste(foo): foo X liste(foo) {}
  /* On purpose, we mention the private symbol X,
     which in the other partial grammar is used
     as a parameter name in the definition of liste(X).
     This can cause a name collision in JoinGrammars
     unless one is careful. */
