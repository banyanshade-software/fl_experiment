{
open Tokens
open Type

(* indentation based (INDENT/DEDENT) inspired by 
    hugs98-Sep2006
    https://gist.github.com/zehnpaard/124a9c6df632839d01b4fede8684ddd8
and (but not really) :
    https://github.com/marcelgoh/opythn/blob/master/src/lexer.mll 
*)

}

let eol = '\r'? '\n'
let indent = eol ' '*
let blank = [' ' '\t']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| indent as s
    { Lexing.new_line lexbuf; SPACE (String.length s - 1) }
| blank+
    { token lexbuf }
| eol
    { Lexing.new_line lexbuf; token lexbuf}
| "/*"
    { comment lexbuf; token lexbuf }
| "||"
    { commentline lexbuf; token lexbuf }
| '"'
    {  STR (stringval (Buffer.create 100) lexbuf) }
| '|'
    { VBAR }
| "where"
    { WHERE }
| "abstype"
    { ABSTYPE }
| "if"
    { IF }
| "div" 
    { DIV }
| "mod"
    { MOD }
| "otherwise"
    { OTHERWISE }
| "show"
    { SHOW }
| "True"
    { BOOL(true) }
| "False"
    { BOOL(false) }
| "with"
    { WITH }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '['
    { LBRACKET }
| ']'
    { RBRACKET }
| "->"
    { ARROW }
| '-'
    { MINUS }
| '+'
    { PLUS }
| '='
    { EQUAL }
| "<-"
    { LEFTARROW }
| "<="
    { LESS_EQUAL }
| '<' 
    { LESS }
| ">="
    { GREATER_EQUAL }
| '>'
    { GREATER }
| "::="
    { COLCOLEQ }
| ":="
    { COLON2EQ }
| "::"
    { COLCOL }
| ':'
    { COLON }
| '#'
    { LENGTH }
| '*'
    { MULT }
| ','
    { COMMA }
| ".."
    { DOTDOT }
| "."
    { DOT }
| ';'
    { COLON }
| '~'
    { NOT }
| "\\/"
    { OR }
| '&'
    { AND }
| '!'
    { SUBSCRIPT }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| digit+
    { INT(int_of_string(Lexing.lexeme lexbuf)) }
| digit+ ('.' digit*)? (['e' 'E'] ['+' '-']? digit+)?
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| (lower|'_') (digit|lower|upper|'_')* (* Â¾¤Î¡ÖÍ½Ìó¸ì¡×¤è¤ê¸å¤Ç¤Ê¤¤¤È¤¤¤±¤Ê¤¤ *)
    { IDENT(Lexing.lexeme lexbuf) }
| upper (digit|lower|upper|'_')*
    { TIDENT(Lexing.lexeme lexbuf) }
| eof
    { EOF }
| _
    { failwith
        (Printf.sprintf "(mira) unknown token '%s' near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }



and comment = parse
| "*/" 
    { () }
| "/*"
    { comment lexbuf;
      comment lexbuf }
| eol
    { Lexing.new_line lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }

and commentline = parse
 eol
    { Lexing.new_line lexbuf; () }
| _
    { commentline lexbuf }


and stringval buf = parse
| [^'"' '\n' '\\']+  
    { Buffer.add_string buf @@ Lexing.lexeme lexbuf; 
      stringval buf lexbuf 
    }
| '\n'
    { Buffer.add_string buf @@ Lexing.lexeme lexbuf; 
      Lexing.new_line lexbuf;
      stringval buf lexbuf
    }
| '\\' '"'  
    { Buffer.add_char buf '"'; 
      stringval buf lexbuf
    }
| '\\'
    { Buffer.add_char buf '\\';
      stringval buf lexbuf
    }
| '"'
    { Buffer.contents buf } (* return *)
| eof  
    { failwith
        (Printf.sprintf "(mira) eof in string token '%s' near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
| _  { failwith
        (Printf.sprintf "(mira) unknown token '%s' near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }    
