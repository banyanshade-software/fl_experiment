%{
(* parserが利用する変数、関数、型などの定義 *)
open Syntax
let addtyp x = (x, Type.gentyp ())

(* return file, line, char from the given position *)
let get_pos_info pos =
  (pos.Lexing.pos_fname, pos.Lexing.pos_lnum, pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
;;

%}

/* (* 字句を表すデータ型の定義 (caml2html: parser_token) *) */
%token <bool> BOOL
%token <int> INT
%token <float> FLOAT
%token NOT
%token MINUS
%token PLUS
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
%token EOF

/* (* 優先順位とassociativityの定義（低い方から高い方へ） (caml2html: parser_prior) *) */
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

/* (* 開始記号の定義 *) */
%type <Syntax.t> exp
%start exp

%%

simple_exp: /* (* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) *) */
| LPAREN exp1 RPAREN
    { $2 }
| LPAREN RPAREN
    { Unit }
| BOOL
    { Bool($1) }
| INT
    { Int($1) }
| FLOAT
    { Float($1) }
| IDENT
    { Var($1) }
| simple_exp DOT LPAREN exp1 RPAREN
    { Get($1, $4) }

exp1: /* (* 一般の式 (caml2html: parser_exp) *) */
| simple_exp 
    { $1 }
| NOT exp1
    %prec prec_app
    { Not($2) }
| MINUS exp1
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f) (* -1.23などは型エラーではないので別扱い *)
    | e -> Neg(e) }
| exp1 PLUS exp1 /* (* 足し算を構文解析するルール (caml2html: parser_add) *) */
    { Add($1, $3) }
| exp1 MINUS exp1
    { Sub($1, $3) }
| exp1 EQUAL exp1
    { Eq($1, $3) }
| exp1 LESS_GREATER exp1
    { Not(Eq($1, $3)) (* some float comparisons differ from OCaml for NaN; see: https://github.com/esumii/min-caml/issues/13#issuecomment-1147032750 *) }
| exp1 LESS exp1
    { Not(LE($3, $1)) }
| exp1 GREATER exp1
    { Not(LE($1, $3)) }
| exp1 LESS_EQUAL exp1
    { LE($1, $3) }
| exp1 GREATER_EQUAL exp1
    { LE($3, $1) }
| IF exp1 THEN exp1 ELSE exp1
    %prec prec_if
    { If($2, $4, $6) }
| MINUS_DOT exp1
    %prec prec_unary_minus
    { FNeg($2) }
| exp1 PLUS_DOT exp1
    { FAdd($1, $3) }
| exp1 MINUS_DOT exp1
    { FSub($1, $3) }
| exp1 AST_DOT exp1
    { FMul($1, $3) }
| exp1 SLASH_DOT exp1
    { FDiv($1, $3) }
| LET IDENT EQUAL exp1 IN exp1
    %prec prec_let
    { Let(addtyp $2, $4, $6) }
| LET REC fundef IN exp1
    %prec prec_let
    { LetRec($3, $5) }
| simple_exp actual_args
    %prec prec_app
    { App($1, $2) }
| elems
    %prec prec_tuple
    { Tuple($1) }
| LET LPAREN pat RPAREN EQUAL exp1 IN exp1
    { LetTuple($3, $6, $8) }
| simple_exp DOT LPAREN exp1 RPAREN LESS_MINUS exp1
    { Put($1, $4, $7) }
| exp1 SEMICOLON exp1
    { Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) }
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array($2, $3) }
| error
    { let (_, start_line, start_char) = get_pos_info (Parsing.symbol_start_pos ()) in
      let (_, end_line, end_char) = get_pos_info (Parsing.symbol_end_pos ()) in
      failwith
        (Printf.sprintf "parse error near line %d pos %d -- line %d pos %d"
           start_line start_char end_line end_char) }

exp:
| exp1 EOF
    { $1 }

fundef:
| IDENT formal_args EQUAL exp1
    { { name = addtyp $1; args = $2; body = $4 } }

formal_args:
| IDENT formal_args
    { addtyp $1 :: $2 }
| IDENT
    { [addtyp $1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp1
    { $1 @ [$3] }
| exp1 COMMA exp1
    { [$1; $3] }

pat:
| pat COMMA IDENT
    { $1 @ [addtyp $3] }
| IDENT COMMA IDENT
    { [addtyp $1; addtyp $3] }
