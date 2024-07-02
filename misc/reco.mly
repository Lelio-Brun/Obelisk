%%
my_option(X, Y):
  |     {}
  | Y X {}

my_list(A):
  |              {}
  | A my_list(A) {}

my_nonempty_list(C):
  | C                     {}
  | C my_nonempty_list(C) {}

my_separated_nonempty_list(X,Y):
  | X                                   {}
  | X Y my_separated_nonempty_list(X,Y) {}

my_separated_list(X,S):
  |                                 {}
  | my_separated_nonempty_list(X,S) {}

my_rule:
  | my_option(E, F)                    {}
  | my_list(E)                         {}
  | my_nonempty_list(F)                {}
  | my_separated_nonempty_list(E,S1)   {}
  | my_separated_list(F,S2)            {}
