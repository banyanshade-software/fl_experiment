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
%token <string> STR
%token CASE
%token OF
%token VBAR
%token ANTISLASH
%token DOTDOT
%token IMPLIES
%token MOD
%token AROBAS
%token COLCOL
%token SHOW
%token WITH
%token OTHERWISE
%token UNDERSCORE
%token WHERE
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
(*%token DOT*)
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
(* %left DOT *)


%type <Syntax.t> topDecls
%start topDecls

%%


topDecls:
| topDecl+   
    { Todo }

topDecl:
| decl       { Todo }

decls:
| LBRACE decls1 RBRACE
    { $2 }

decls1:
| decls0 decl
    { Todo }

decls0:
| { Todo }
| decls0 SEMICOLON { Todo }
| decls1 SEMICOLON { Todo }

decl:
| gendecl
    { Todo }
| funlhs rhs
    { Todo }
| funlhs COLCOL typed rhs
    { Todo }
| pat0 rhs
    { Todo }

funlhs:
| funlhs0         { Todo }
| funlhs1         { Todo }
(*| npk             { Todo } var '+' UMLIT *)


typed:
| type1         { Todo }
| btype2        { Todo }


btype:
| btype1                    {}
| btype2                    {}

btype1: 
| btype1 atype              {}
| atype1                    {}

btype2: 
| btype2 atype              {}
(* | qconid                    {} *)

atype:
| atype1                    {}
(* | qconid                    {} *)

atype1:
| varid
| LPAREN RPAREN                         { Todo }
| LPAREN type1 RPAREN                   { Todo }
| LPAREN btype2 RPAREN                  { Todo }
| LPAREN tupCommas RPAREN               { Todo }
| LPAREN btypes2 RPAREN                 { Todo }
| LPAREN typeTuple RPAREN               { Todo }
| LPAREN tfields RPAREN                 { Todo }
| LPAREN tfields VBAR typed RPAREN      { Todo }
| LBRACKET typed RBRACKET               { Todo }
| LBRACKET RBRACKET                     { Todo }
| UNDERSCORE                            { Todo }

btypes2:
| btypes2 COMMA btype2                  { Todo }
| btype2 COMMA btype2                   { Todo }

tfields:
| tfields COMMA tfield                  { Todo }
| tfield                                { Todo }

tfield:
| varid COLCOL typed                      { Todo }

numlit:
| INT                                   { Int($1) }
| FLOAT                                 { Float($1) }

pat0:
| var                                   { Todo }
| numlit                                { Todo }
| pat0_vI                               { Todo }

varid:
| IDENT					{ Var($1) }


rhs:
| rhs1 wherePart			{ Todo }

rhs1:
| EQUAL exp				{Todo}
| gdrhs				{Todo}

gdrhs:
| gdrhs gddef				{Todo}
| gddef				{Todo}

gddef:
| VBAR exp0 EQUAL exp				{Todo}

wherePart:
| {}
| WHERE decls				{}

funlhs0:
| pat10_vI varop    pat0      { Todo }
(* | infixPat varop    pat0      {} *)
| numlit   varop    pat0      { Todo }
(*| var      varop_pl pat0      {} *)
| var      PLUS pat0_INT  { Todo }

funlhs1:
| LBRACE funlhs0 RBRACE 	{ Todo }
| LBRACE funlhs1 RBRACE 	{ Todo }
(*| LBRACE npk RBRACE 	{ Todo } *)
| var apat		{ Todo }
| funlhs1 apat		{ Todo }

varop:
| PLUS			{ Todo }
| MINUS			{ Todo }
(*
| varop_mipl

varop_mipl:
| VAROP
| BACKQUOTE varid BACKQUOTE
| SUBSCRIPT
| DOT

varop_pl:
| MINUS
| varop_mipl

*)

pat10_vI:
| fpat			{ Todo }
| apat_vI		{ Todo }

pat0_INT:
| var			{ Todo }
| pat0_vI		{ Todo }

pat0_vI:
| pat10_vI		{ Todo }
(*| infixPat		{} *)

pat:
| npk	{ Todo }
| pat_npk	{ Todo }

npk:
| var PLUS numlit { Todo }

pat_npk:
| pat0 COLCOL typed { Todo }
| pat0 { Todo }
var:
| varid			{ Todo }

tupCommas:
| tupCommas COMMA	{ Todo }
| COMMA	{ Todo }

fpat:
|  apat+		{ Todo }
(* | gcon apat		{ Todo } *)

apat:
| numlit		{ Todo }
| var		{ Todo }
| apat_vI		{ Todo }

apat_vI:
| var AROBAS apat	{ Todo  }
(* | gcon | qcon '{' patbinds '}' *)
(*| CHARLIT*)
| STR			{ Todo }
| UNDERSCORE		{ Todo }
(* '(' pat_npk ')' | '(' npk ')'| '(' pats2 ')'| '[' pats1 ']'  | '~' apat                    *)

gendecl:
(* INFIXN ... *)
| vars COLCOL topType { Todo }

topType:
(* | ALL varids '.' topType0 *)
| topType0 { Todo }

topType0: 
| context IMPLIES topType1    { Todo }
| topType1  { Todo }

topType1:
(* | bpolyType ARROW topType1   { Todo }*)
| btype1    ARROW topType1    { Todo }
| btype2    ARROW topType1    { Todo }
| btype                       { Todo }


type1: 
| btype1                      { Todo }
(* | bpolyType ARROW typed       { Todo }*)
| btype1    ARROW typed       { Todo }
| btype2    ARROW typed       { Todo }






typeTuple:
| type1     COMMA typed          { Todo }
| btype2    COMMA type1         { Todo }
| btypes2   COMMA type1         { Todo }
| typeTuple COMMA typed          { Todo }


vars:
| vars COMMA var { Todo }
| var { Todo }


context:
| LPAREN RPAREN			{ Unit }
| btype2			{ Todo }
| LPAREN btype2 RPAREN			{ Todo }
| LPAREN btypes2 RPAREN			{ Todo }
| lacks			{ Todo }
| LPAREN lacks1 RPAREN			{ Todo }

lacks:
| varid ANTISLASH varid { Todo }
(* .. *)

lacks1:
| btypes2 COMMA lacks		{ Todo }
| lacks1 COMMA btypes2		{ Todo }
| lacks1 COMMA lacks		{ Todo }
| btype2 COMMA lacks		{ Todo }
| lacks		{ Todo }

exp:
| exp_err	{ Todo }

exp_err:
| exp0a COLCOL sigType { Todo }
| exp0 { Todo }

exp0:
| exp0a { Todo }
| exp0b { Todo }

exp0a:
| infixExpa  { Todo }
| exp10a { Todo }

exp0b:
| infixExpb { Todo }
| exp10b { Todo }

infixExpa:
| infixExpa qop MINUS exp10a { Todo }
| infixExpa qop exp10a { Todo }
| MINUS exp10a { Todo }
| exp10a qop MINUS exp10a { Todo }
| exp10a qop exp10a { Todo }

infixExpb:
| infixExpa qop MINUS exp10b { Todo }
| infixExpa qop exp10b { Todo }
| MINUS exp10b { Todo }
| exp10a qop MINUS exp10b { Todo }
| exp10a qop exp10b { Todo }

exp10a:
| CASE exp OF LBRACE alts RBRACE { Todo }
(* | DO MDO *)
| appExp { Todo }

exp10b:
| ANTISLASH pats ARROW exp { Todo }
| LET ldecls IN exp { Todo }
| IF exp THEN exp ELSE exp { Todo }

ldecls:
| LBRACE ldecls0 RBRACE { Todo }
| LBRACE ldecls1 RBRACE { Todo }

ldecls0:
| { Todo }
| ldecls0 SEMICOLON { Todo }
| ldecls1 SEMICOLON { Todo }

ldecls1:
| ldecls0 ldecl { Todo }

ldecl:
(* IPVARID = exp *)
| decl { Todo }

appExp:
| appExp aexp { Todo }
| aexp { Todo }

aexp:
| qvar { Todo }
| qvar AROBAS aexp { Todo }
(* | TILDE aexp { Todo }*)
| UNDERSCORE { Todo }
(*IPVARIDi qcon qcon { fbinds }n aexp {fbinds}*)
| numlit { Todo }
| STR { Todo }
(* charleat *)
| LPAREN exp RPAREN { Todo }
| LPAREN exps2 RPAREN { Todo }
| LPAREN vfields RPAREN { Todo }
| LPAREN vfields VBAR exp RPAREN { Todo }
| LBRACKET listd RBRACKET { Todo }
(*..*)

qvar:
| var { Todo }
(* QVARID | ( QVAROP ) *)

qop:
| qvarop { Todo }
| qconop { Todo }

qvarop:
| MINUS { Todo }
(* | qvarop_mi { Todo }*)

qconop:
(* CONOP | `CONID`*)
| conop { Todo }

conop:
| { Todo }

exps2:
| exps2 COMMA exp { Todo }
| exp COMMA exp { Todo }

vfields:
| vfields COMMA vfield { Todo }
| vfield { Todo }

vfield:
| varid EQUAL exp { Todo }

alts:
| alts1  { Todo }
| COLON alts { Todo }

alts1:
| alts1 COLON alt { Todo }
| alts1 COLON { Todo }
| alt { Todo }

alt:
| pat altRhs wherePart { Todo }

altRhs:
| guardAlts { Todo }
| ARROW exp { Todo }

guardAlts:
| guardAlts guardAlt { Todo }
| guardAlt { Todo }

guardAlt:
| VBAR exp0 ARROW exp { Todo }

pats:
| pats apat { Todo }
| apat { Todo }


sigType:
| context IMPLIES typed { Todo }
| typed { Todo }


(* list exp *)
listd:
| exp { Todo }
| exps2 { Todo }
| exp zipquals { Todo }
| exp DOTDOT exp { Todo }
| exp COMMA exp DOTDOT  { Todo }
| exp DOTDOT  { Todo }
| exp COMMA exp DOTDOT exp { Todo }

zipquals:
| zipquals VBAR quals { Todo }
| VBAR quals { Todo }

quals:
| quals COMMA qual { Todo }
| qual { Todo }

qual:
| exp LEFTARROW exp { Todo }
| exp { Todo }


