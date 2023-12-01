module T = Tokens

let convert_space_to_indent width f =
  let indent = ref 0 in
  let make_indent _ = [T.BR; T.INDENT] in
  let make_dedent _ = [T.BR; T.DEDENT] in
  let g h a b = List.init (a - b) h |> List.concat in
  fun lexbuf -> match f lexbuf with
    | T.SPACE n ->
        let m = n / width in
        let k = !indent in
        if m > k then (indent := m; g make_indent m k)
        else if m < k then (indent := m; g make_dedent k m)
        else [T.BR]
    | T.EOF ->
        let k = !indent in
        (indent := 0; g make_dedent k 0 @ [T.EOF])
    | e -> [e]

let flatten f =
  let xs = ref [] in
  fun lexbuf -> match !xs with
    | x::xs' -> xs := xs'; x
    | [] -> (match f lexbuf with
      | x::xs' -> xs := xs'; x
      | [] -> failwith "Lexer did nto return EOF token")

let token = Milexer.token |> convert_space_to_indent 4 |> flatten
