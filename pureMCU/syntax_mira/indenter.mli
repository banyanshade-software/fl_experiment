val convert_space_to_indent: (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> Tokens.token list 
val flatten: ('a -> 'b list) -> 'a -> 'b
val token: Lexing.lexbuf -> Tokens.token
val error_location: Lexing.lexbuf -> string
