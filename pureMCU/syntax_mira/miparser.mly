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


%type <Syntax.t> topDecls
%start topDecls

%%


topDecls:
| topDecl   
    { $1 }
| topDecl topDecls
    { $1 }

topDecl:
| decl       { $1 }

decls:
| LBRACE decls1 RBRACE
    { $2 }

decls1:
| decls0 decl
    { $1 }

decls0:
| {}
| decls0 SEMICOLON { $1 }
| decls1 SEMICOLON { $1 }

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
| LPAREN RPAREN                         {}
| LPAREN type1 RPAREN                   {}
| LPAREN btype2 RPAREN                  {}
| LPAREN tupCommas RPAREN               {}
| LPAREN btypes2 RPAREN                 {}
| LPAREN typeTuple RPAREN               {}
| LPAREN tfields RPAREN                 {}
| LPAREN tfields VBAR typed RPAREN      {}
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
| INT                                   { $1 }
| FLOAT                                 { $1 }

pat0:
| var                                   { $1 }
| numlit                                { $1 }
| pat0_vI                               { $1 }

varid:
| IDENT					{ $1 }


rhs:
| rhs1 wherePart			{ $1 }

rhs1:
| EQUAL exp				{}
| gdrhs				{}

gdrhs:
| gdrhs gddef				{}
| gddef				{}

gddef:
| VBAR exp0 EQUAL exp				{}

wherePart:
| {}
| WHERE decls				{}

funlhs0:
| pat10_vI varop    pat0      {}
(* | infixPat varop    pat0      {} *)
| numlit   varop    pat0      {}
(*| var      varop_pl pat0      {} *)
| var      PLUS pat0_INT  {}

funlhs1:
| LBRACE funlhs0 RBRACE 	{}
| LBRACE funlhs1 RBRACE 	{}
(*| LBRACE npk RBRACE 	{} *)
| var apat		{}
| funlhs1 apat		{}

varop:
| PLUS			{}
| MINUS			{}
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
| fpat			{}
| apat_vI		{}

pat0_INT:
| var			{}
| pat0_vI		{}

pat0_vI:
| pat10_vI		{}
(*| infixPat		{} *)

pat:
| npk	{}
| pat_npk	{}

npk:
| var PLUS numlit {}

pat_npk:
| pat0 COCO typed {}
| pat0 {}
var:
| varid			{ $1 }

tupCommas:
| tupCommas COMMA	{}
| COMMA	{}

fpat:
| fpat apat		{}
(* | gcon apat		{} *)

apat:
| numlit		{}
| var		{}
| apat_vI		{}

apat_vI:
| var AROBAS apat	{}
(* | gcon | qcon '{' patbinds '}' *)
(*| CHARLIT*)
| STR			{}
| UNDERSCORE			{}
(* '(' pat_npk ')' | '(' npk ')'| '(' pats2 ')'| '[' pats1 ']'  | '~' apat                    *)

gendecl:
(* INFIXN ... *)
| vars COCO topType {}

topType:
(* | ALL varids '.' topType0 *)
| topType0 {}

topType0: 
| context IMPLIES topType1    {}
| topType1  {}

topType1:
(* | bpolyType ARROW topType1   {}*)
| btype1    ARROW topType1    {}
| btype2    ARROW topType1    {}
| btype                       {}


type1: 
| btype1                      {}
(* | bpolyType ARROW typed       {}*)
| btype1    ARROW typed       {}
| btype2    ARROW typed       {}






typeTuple:
| type1     COMMA typed          {}
| btype2    COMMA type1         {}
| btypes2   COMMA type1         {}
| typeTuple COMMA typed          {}


vars:
| vars COMMA var {}
| var {}


context:
| LPAREN RPAREN			{}
| btype2			{}
| LPAREN btype2 RPAREN			{}
| LPAREN btypes2 RPAREN			{}
| lacks			{}
| LPAREN lacks1 RPAREN			{}

lacks:
| varid ANTISLASH varid {}
(* .. *)

lacks1:
| btypes2 COMMA lacks		{}
| lacks1 COMMA btypes2		{}
| lacks1 COMMA lacks		{}
| btype2 COMMA lacks		{}
| lacks		{}

exp:
| exp_err	{}

exp_err:
| exp0a COCO sigType {}
| exp0 {}

exp0:
| exp0a {}
| exp0b {}

exp0a:
| infixExpa  {}
| exp10a {}

exp0b:
| infixExpb {}
| exp10b {}

infixExpa:
| infixExpa qop MINUS exp10a {}
| infixExpa qop exp10a {}
| MINUS exp10a {}
| exp10a qop MINUS exp10a {}
| exp10a qop exp10a {}

infixExpb:
| infixExpa qop MINUS exp10b {}
| infixExpa qop exp10b {}
| MINUS exp10b {}
| exp10a qop MINUS exp10b {}
| exp10a qop exp10b {}

exp10a:
| CASE exp OF LBRACE alts RBRACE {}
(* | DO MDO *)
| appExp {}

exp10b:
| ANTISLASH pats ARROW exp {}
| LET ldecls IN exp {}
| IF exp THEN exp ELSE exp {}

ldecls:
| LBRACE ldecls0 RBRACE {}
| LBRACE ldecls1 RBRACE {}

ldecls0:
| {}
| ldecls0 SEMICOLON {}
| ldecls1 SEMICOLON {}

ldecls1:
| ldecls0 ldecl {}

ldecl:
(* IPVARID = exp *)
| decl {}

appExp:
| appExp aexp {}
| aexp {}

aexp:
| qvar {}
| qvar AROBAS aexp {}
(* | TILDE aexp {}*)
| UNDERSCORE {}
(*IPVARIDi qcon qcon { fbinds }n aexp {fbinds}*)
| numlit {}
| STR {}
(* charleat *)
| LPAREN exp RPAREN {}
| LPAREN exps2 RPAREN {}
| LPAREN vfields RPAREN {}
| LPAREN vfields VBAR exp RPAREN {}
| LBRACKET listd RBRACKET {}
(*..*)

qvar:
| var {}
(* QVARID | ( QVAROP ) *)

qop:
| qvarop {}
| qconop {}

qvarop:
| MINUS {}
(* | qvarop_mi {}*)

qconop:
(* CONOP | `CONID`*)
| conop {}

conop:
| {}

exps2:
| exps2 COMMA exp {}
| exp COMMA exp {}

vfields:
| vfields COMMA vfield {}
| vfield {}

vfield:
| varid EQUAL exp {}

alts:
| alts1  {}
| COLON alts {}

alts1:
| alts1 COLON alt {}
| alts1 COLON {}
| alt {}

alt:
| pat altRhs wherePart {}

altRhs:
| guardAlts {}
| ARROW exp {}

guardAlts:
| guardAlts guardAlt {}
| guardAlt {}

guardAlt:
| VBAR exp0 ARROW exp {}

pats:
| pats apat {}
| apat {}


sigType:
| context IMPLIES typed {}
| typed {}


(* list exp *)
listd:
| exp {}
| exps2 {}
| exp zipquals {}
| exp DOTDOT exp {}
| exp COMMA exp DOTDOT  {}
| exp DOTDOT  {}
| exp COMMA exp DOTDOT exp {}

zipquals:
| zipquals VBAR quals {}
| VBAR quals {}

quals:
| quals COMMA qual {}
| qual {}

qual:
| exp LEFTARROW exp {}
| exp {}


