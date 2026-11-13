%{
open Parser_utils

%}

%token EQ
%token LEFT_BRACE
%token RIGHT_BRACE
%token ARRAY_START
%token ARRAY_END
%token TABLE_HEADER_START
%token TABLE_HEADER_END
%token TABLE_ARRAY_HEADER_START
%token TABLE_ARRAY_HEADER_END
%token COMMA
%token DOT
%token NEWLINE

(* Primitive values *)
%token <string> BOOLEAN
%token <string> INTEGER
%token <string> FLOAT
%token <string> STRING
%token <string> MULTILINE_STRING
%token <string> LOCAL_TIME
%token <string> LOCAL_DATE
%token <string> LOCAL_DATETIME
%token <string> OFFSET_DATETIME
%token <string> KEY

%token EOF

%start <node list> toml_ast
%%

key:
  | s = KEY { s }

value:
  | b = BOOLEAN
    { NodeBoolean b }
  | i = INTEGER
    { NodeInteger i }
  | f = FLOAT
    { NodeFloat f }
  | s = STRING
    { NodeString s }
  | s = MULTILINE_STRING
    { NodeString s }
  | t = LOCAL_TIME
    { NodeLocalTime t }
  | d = LOCAL_DATE
    { NodeLocalDate d }
  | dt = LOCAL_DATETIME
    { NodeLocalDateTime dt }
  | dt = OFFSET_DATETIME
    { NodeOffsetDateTime dt }
  | a = array
    { NodeArray a }
  | i = inline_table
    { NodeInlineTable i }

(* Arrays allow trailing separators and newlines anywhere inside the square brackets.
   That's why the built-in separated_list() won't do -- we need a custom macro.
 *)
let item_sequence(Sep, X) :=
  | 
    { [] }
  | x = X; 
    { [x] }
  | x = X; Sep; xs = item_sequence(Sep, X);
    { x :: xs }

array:
  | ARRAY_START; ARRAY_END { [] }
  | ARRAY_START; vs = item_sequence(COMMA, value); ARRAY_END { vs }

table_path:
  | ks = separated_nonempty_list(DOT, key) { ks }

key_value_pair:
  | k = table_path; EQ; v = value; { (k, v) } 

(* Unlike arrays, inline tables do not allow trailing commas and newlines inside
   (for whatever reason, I hope TOML standard maintainers eventually reconsider it).
   That's why we use an ordinary separated_list() here.
 *)
inline_table:
  | LEFT_BRACE; kvs = separated_list(COMMA, key_value_pair); RIGHT_BRACE { kvs }

(* Non-inline table handling *)

table_header:
  | TABLE_HEADER_START; ks = table_path; TABLE_HEADER_END { ks }

table_array_header:
  | TABLE_ARRAY_HEADER_START; ks = table_path; TABLE_ARRAY_HEADER_END { ks }

table_entry:
  | kv = key_value_pair; 
    { let (k, v) = kv in Pair (k, v) }
  | ks = table_header
    { TableHeader ks }
  | ks = table_array_header
    { TableArrayHeader ks }

let items_on_lines(X) :=
  | (* Empty *) { [] }
  | x = X; 
    { [x] }
  | x = X; NEWLINE+; xs = items_on_lines(X);
    { x :: xs }

table:
  es = items_on_lines(table_entry); { es }

toml_ast: 
  | NEWLINE*; t = table; EOF { t }
