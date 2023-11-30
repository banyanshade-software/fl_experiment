
type token = 
  | WITH
  | WHERE
  | THEN
  | SLASH_DOT
  | SHOW
  | RPAREN
  | LPAREN
  | REC
  | RBRACKET
  | PLUS_DOT
  | PLUS
  | OTHERWISE
  | NOT
  | MOD
  | VBAR
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
  | STR of (string)
  | IN
  | IF
  | IDENT of (Id.t)
  | TIDENT of (Id.t)
  | GREATER_EQUAL
  | GREATER
  | FLOAT of (float)
  | EQUAL
  | EOF
  | ELSE
  | DOTDOT
  | DOT
  | DIV
  | MULT
  | COMMA
  | SEMICOLON
  | COLON2EQ
  | COLON
  | COLCOL
  | COLCOLEQ
  | BOOL of (bool)
  | AST_DOT
  | ARROW
  | ARRAY_CREATE
  | ABSTYPE
