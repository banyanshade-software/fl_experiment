val limit : int ref
val string : string -> unit
val file : string -> unit
val file_ast : string -> Syntax.t
val file_lex : string -> Tokens.token list
val lexer : Lexing.lexbuf -> Tokens.token
val file_lexer : string -> Tokens.token
