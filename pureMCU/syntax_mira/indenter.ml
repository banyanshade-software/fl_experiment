module T = Tokens

(* val convert_space_to_indent: int -> ('a -> Tokens.token) -> 'a -> Tokens.token list *)

let error_location lexbuf =
   let s = Lexing.lexeme_start_p lexbuf in
   (*let e = Lexing.lexeme_end_p lexbuf in*)
   Printf.sprintf "line %d col %d (near '%s')"
        s.Lexing.pos_lnum
        (s.Lexing.pos_cnum-s.Lexing.pos_bol)
        (Lexing.lexeme lexbuf)

let convert_space_to_indent f =
  let istack = ref [] in
  let nextindent = ref false in
  let store_indent_on_next t = (nextindent := true; [t]) in
  let col lb = 
    let s = Lexing.lexeme_start_p lb in
    s.pos_cnum - s.pos_bol in
  let cole lb = 
    let s = Lexing.lexeme_end_p lb in
    s.pos_cnum - s.pos_bol in
  let store_indent lb = 
    nextindent:=false; 
    (* Format.printf "store ident %d-%d\n" (col lb) (cole lb); *)
    istack := (col lb) :: !istack
    in
  let rec pop1 lexbuf st n =
      if [] = st then [] else 
      let h::r = st in
      (* Format.printf "pop1 %d %d\n" n h;*)
      if n>h then failwith (Printf.sprintf "bad indent '%s'" (error_location lexbuf))
             else if n=h then (istack:=st;[T.RBRACE]) 
                         else T.RBRACE::(pop1 lexbuf r n) 
     in
  let pop2 lexbuf st n =
      if [] = st then [T.NOP] else 
      let h::_ = st in
      if n==h then [T.SEMICOLON] 
              else if n>h then [T.NOP2] else (pop1 lexbuf st n) in

  (*let pop lexbuf st n = T.BR :: (pop2 lexbuf st n) in *)
  let pop = pop2 in 

  fun lexbuf -> match f lexbuf with
    | T.SPACE n ->
        (*Format.printf "space indent %d\n" n;*)
        pop lexbuf !istack (n+1)  (* next token will be on n+1 *)
    | T.WHERE -> store_indent_on_next T.WHERE
    | T.OF    -> store_indent_on_next T.OF
    | T.LET   -> store_indent_on_next T.LET
    | T.LBRACE -> (nextindent:=false;[T.LBRACE])
    | T.EOF ->
        (*Format.printf "found EOF";*)
        (pop lexbuf !istack 0) @ [ T.EOF ]
    | e -> if !nextindent then (store_indent lexbuf;
                                [T.LBRACE; e]
                               )
           else [e] (* (Format.printf "found ident %d-%d\n" (col lexbuf) (cole lexbuf); [e]) *)

let flatten f =
  let xs = ref [] in
  fun lexbuf -> match !xs with
    | x::xs' -> xs := xs'; x
    | [] -> (match f lexbuf with
      | x::xs' -> xs := xs'; x
      | [] -> failwith "Lexer did not return EOF token")


let rec remove_nop f =
  fun lexbuf -> match f lexbuf with
    | T.NOP  -> remove_nop f lexbuf
    | T.NOP2 -> remove_nop f lexbuf
    | e -> e

let token = Milexer.token |> convert_space_to_indent |> flatten |> remove_nop
