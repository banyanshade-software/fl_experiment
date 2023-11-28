{
open Miparser
open Type
}

let eol = '\r'? '\n'
let blank = [' ' '\t']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| blank+
    { token lexbuf }
| eol
    {Lexing.new_line lexbuf; token lexbuf}
| "/*"
    { comment lexbuf; token lexbuf }
| "||"
    { commentline lexbuf; token lexbuf }
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
| "<-"
    { LEFTARROW }
| ":="
    { COLON2EQ }
| ':'
    { COLON }
and comment = parse
| "*/" 
    { () }
| "/*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }

and commentline = parse
 eol
    { () }

