val convert_space_to_indent: int -> (Lexing.lexbuf -> Tokens.token) -> Lexing.lexbuf -> Tokens.token list 
val flatten: ('a -> 'b list) -> 'a -> 'b
val token: Lexing.lexbuf -> Tokens.token
