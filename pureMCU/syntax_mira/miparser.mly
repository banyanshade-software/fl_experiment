(* https://www.haskell.org/onlinereport/syntax-iso.html *)
(* hugs98 *)
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
%token COCO
%token SHOW
%token WITH
%token OTHERWISE
%token UNDERSCORE
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
%token LBRACE
%token RBRACE
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

topDecls:
| topDecl   
    { $1 }
| topDecl topDecls
    { $1 }

topDecl:
| decl       { $1 }

decls:
| LBRACE decls1 RBRACE
    { $2 }

decls1:
| decls0 decl
    { $1 }

decl:
| gendecl
    { $1 }
| funlhs rhs
    { $1 }
| funlhs COCO typed rhs
    { $1 }
| pat0 rhs
    { $1 }

funlhs:
| funlhs0         { $1 }
| funlhs1         { $1 }
(*| npk             { $1 } var '+' UMLIT *)


typed:
| type1         { $1 }
| btype2        { $1 }

type1:
| btype1                    {}
| bpolyType ARROW typed     {}
| btype1    ARROW typed     {}
| btype2    ARROW typed     {}

btype:
| btype1                    {}
| btype2                    {}

btype1: 
| btype1 atype              {}
| atype1                    {}

btype2: 
| btype2 atype              {}
| qconid                    {}

atype:
| atype1                    {}
| qconid                    {}

atype1:
| varid
| LPAREN RPAREN                         {}
| LPAREN type1 RPAREN                   {}
| LPAREN btype2 RPAREN                  {}
| LPAREN tupCommas RPAREN               {}
| LPAREN btypes2 RPAREN                 {}
| LPAREN typeTuple RPAREN               {}
| LPAREN tfields RPAREN                 {}
| LPAREN tfields PIPE typed RPAREN      {}
| LBRACKET typed RBRACKET               {}
| LBRACKET RBRACKET                     {}
| UNDERSCORE                            {}

btypes2:
| btypes2 COMMA btype2                  {}
| btype2 COMMA btype2                   {}

tfields:
| tfields COMMA tfield                  {}
| tfield                                {}

tfield:
| varid COCO typed                      {}

numlit:
| INT                                   {}
| FLOAT                                 {}

pat0:
| var                                   {}
| numlit                                {}
| pat0_vI                               {}
