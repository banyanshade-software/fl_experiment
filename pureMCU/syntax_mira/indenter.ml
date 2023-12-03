module T = Tokens

(* val convert_space_to_indent: int -> ('a -> Tokens.token) -> 'a -> Tokens.token list *)

let error_location lexbuf =
   let s = Lexing.lexeme_start_p lexbuf in
   (*let e = Lexing.lexeme_end_p lexbuf in*)
   Printf.sprintf "line %d col %d (near '%s')"
        s.Lexing.pos_lnum
        (s.Lexing.pos_cnum-s.Lexing.pos_bol)
        (Lexing.lexeme lexbuf)

let convert_space_to_indent width f =
  let istack = ref [] in
  let nextindent = ref false in
  let store_indent_on_next t = (nextindent := true; [t]) in
  let col lb = 
    let s = Lexing.lexeme_start_p lb in
    s.pos_cnum - s.pos_bol in
  let store_indent lb = 
    nextindent:=false; 
    Format.printf "store ident %d\n" (col lb);
    istack := (col lb) :: !istack
    in
  let rec pop1 lexbuf st n =
      if [] = st then [] else 
      let h::r = st in
      Format.printf "pop1 %d %d\n" n h;
      if n>h then failwith (Printf.sprintf "bad indent '%s'" (error_location lexbuf))
             else if n=h then (istack:=st;[T.RBRACE]) else T.RBRACE::(pop1 lexbuf r n) 
     in
  let pop2 lexbuf st n =
      if [] = st then [] else 
      let h::_ = st in
      if n>h then [] else (pop1 lexbuf st n) in
  let pop lexbuf st n = T.BR :: (pop2 lexbuf st n) in

  fun lexbuf -> match f lexbuf with
    | T.SPACE n ->
        Format.printf "space indent %d\n" n;
        pop lexbuf !istack n
    | T.WHERE -> store_indent_on_next T.WHERE
    | T.OF    -> store_indent_on_next T.OF
    | T.LET   -> store_indent_on_next T.LET
    | T.LBRACE -> (nextindent:=false;[T.LBRACE])
    | T.EOF ->
        pop lexbuf !istack 0; [EOF]
    | e -> if !nextindent then (store_indent lexbuf;
                                [T.LBRACE; e]
                               )
           else [e]

let flatten f =
  let xs = ref [] in
  fun lexbuf -> match !xs with
    | x::xs' -> xs := xs'; x
    | [] -> (match f lexbuf with
      | x::xs' -> xs := xs'; x
      | [] -> failwith "Lexer did nto return EOF token")

let token = Milexer.token |> convert_space_to_indent 4 |> flatten
