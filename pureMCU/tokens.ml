
type token = 
  | WITH
  | WHERE
  | THEN
  | SLASH_DOT
  | SHOW
  | SEMICOLON
  | RPAREN
  | LPAREN
  | REC
  | RBRACKET
  | PLUS_DOT
  | PLUS
  | OTHERWISE
  | NOT
  | MOD
  | MINUS_DOT
  | MINUS
  | LET
  | LESS_MINUS
  | LESS_GREATER
  | LESS_EQUAL
  | LESS
  | LEFTARROW
  | LBRACKET
  | INT of (int)
  | IN
  | IF
  | IDENT of (Id.t)
  | GREATER_EQUAL
  | GREATER
  | FLOAT of (float)
  | EQUAL
  | EOF
  | ELSE
  | DOT
  | DIV
  | COMMA
  | COLON2EQ
  | COLON
  | BOOL of (bool)
  | AST_DOT
  | ARROW
  | ARRAY_CREATE
  | ABSTYPE
