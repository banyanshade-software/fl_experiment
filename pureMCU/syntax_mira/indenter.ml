module T = Tokens

(* val convert_space_to_indent: int -> ('a -> Tokens.token) -> 'a -> Tokens.token list *)

let convert_space_to_indent width f =
  let indent = ref 0 in
  let nextindent = ref false in
  let make_indent _ = [T.BR; T.INDENT] in
  let make_dedent _ = [T.BR; T.DEDENT] in
  let g h a b = List.init (a - b) h |> List.concat in
  let store_indent_on_next t = (nextindent := true; [t]) in
  let col lb = 
    let s = Lexing.lexeme_start_p lb in
    s.pos_cnum - s.pos_bol in
  let store_indent lb = 
    nextindent:=false; 
    Format.printf "store ident %d" (col lb);
    in

  fun lexbuf -> match f lexbuf with
    | T.SPACE n ->
        let m = n / width in
        let k = !indent in
        if m > k then (indent := m; g make_indent m k)
        else if m < k then (indent := m; g make_dedent k m)
        else [T.BR]
    | T.WHERE -> store_indent_on_next T.WHERE
    | T.OF    -> store_indent_on_next T.OF
    | T.LET   -> store_indent_on_next T.LET
    | T.LBRACE -> (nextindent:=false;[T.LBRACE])
    | e -> if !nextindent then (store_indent lexbuf;
                                [T.LBRACE; e]
                               )
           else [e]
    | T.EOF ->
        let k = !indent in
        (indent := 0; g make_dedent k 0 @ [T.EOF])

let flatten f =
  let xs = ref [] in
  fun lexbuf -> match !xs with
    | x::xs' -> xs := xs'; x
    | [] -> (match f lexbuf with
      | x::xs' -> xs := xs'; x
      | [] -> failwith "Lexer did nto return EOF token")

let token = Milexer.token |> convert_space_to_indent 4 |> flatten
