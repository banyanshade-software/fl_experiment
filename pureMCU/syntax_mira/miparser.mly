%{
open Syntax
let addtyp x = (x, Type.gentyp ())

let get_pos_info pos =
  (pos.Lexing.pos_fname, pos.Lexing.pos_lnum, pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
;;

%}
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token MOD
%token SHOW
%token WITH
%token OTHERWISE
%token WHERE
%token ABSTYPE
%token NOT
%token MINUS
%token PLUS
%token DIV
%token MINUS_DOT
%token PLUS_DOT
%token AST_DOT
%token SLASH_DOT
%token EQUAL
%token LESS_GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token LESS 
%token GREATER 
%token IF
%token THEN
%token ELSE
%token <Id.t> IDENT
%token LET
%token IN
%token REC
%token COMMA
%token ARRAY_CREATE
%token DOT
%token LESS_MINUS
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token ARROW
%token LEFTARROW
%token COLON2EQ
%token COLON
%token EOF
%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%left COMMA
%left EQUAL LESS_GREATER LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS PLUS_DOT MINUS_DOT 
%left AST_DOT SLASH_DOT
%right prec_unary_minus
%left prec_app
%left DOT


%type <Syntax.t> exp
%start exp

%%
simple_exp:
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }

exp:
| simple_exp
    { $1 }
