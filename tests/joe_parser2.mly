/* Sent by Joe on 2020/05/26. This grammar has an "unexplainable conflict".
   If one removes any of the (apparently unused!) tokens, then the problem
   disappears. The conflict token is '#'. There is a reduce/reduce conflict. */

/* All of the unused tokens below are actually necessary to reproduce the
   problem. It turns out that there are 62 tokens, plus error and #, and
   this causes TerminalSet to be implemented as a DWordBitSet. The bug
   was caused by the fact that [DWordBitSet.add] did not preserve physical
   equality when the element was already present in the set. */

%token COOL_TOKEN

%token GGG FFF EEE DDD CCC BBB AAA
%token PPP OOO NNN MMM LLL KKK JJJ III HHH
%token RRR SSS TTT UUU VVV WWW XXX YYY ZZZ
%token A1 A2 A3 A5 A6 A7 A8 A9 B1 B2 B3 B4 B5
%token B6 B7 B8 B9 C1 C2 C3 C4 C5 C6 C7 C8 C9
%token D1 D2 D3 D4 D5 D6 D7 D8 D9
%token E1

%start <int> look_for_trouble

%%

look_for_trouble:
  | COOL_TOKEN { }
  | COOL_TOKEN { }
;
