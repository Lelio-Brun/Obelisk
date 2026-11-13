%token A B
%start<unit> main
%%
foo: {}
%public liste(X): A foo X {}
main: liste(B)    {}
