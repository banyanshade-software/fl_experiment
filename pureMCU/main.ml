let limit = ref 1000

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

(*
let parsef = Mlparser.exp
let lexerf = Mllexer.token
*)

let print_position lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Format.sprintf "%s:%d:%d" 
         pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parsechan parsef lexerf l =
    try parsef lexerf l with
    (* | Mlparser.MenhirBasics.Error as e -> Format.printf "basic error at: %s" (print_position l);
      raise e *)
    |  _ as e ->  Format.printf "other error at: %s" (print_position l);
      raise e



let lexbuf p outchan = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  Typing.extenv := M.empty;
  Emit.f outchan
    (RegAlloc.f
       (Simm.f
          (Virtual.f
             (Closure.f
                (iter !limit
                   (Alpha.f
                      (KNormal.f
                         (Typing.f
                            (p)))))))))


let string s = 
    let lex = Lexing.from_string s in
    let p = parsechan Mlparser.exp Mllexer.token lex in
    lexbuf p stdout  (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

(*let lexpar f = if (Filename.extension f) = ".ml" then (Mllexer.token, Mlparser.exp) else (Milexer.token, Miparser.exp)*)
let lexpar f = if (Filename.extension f) = ".ml" then (Mllexer.token, Mlparser.exp) else (Indenter.token, Miparser.topDecls)

let file_ast f =
  let inchan = open_in f in
  let (lexerf, parsef) = lexpar f in
  let p = parsechan parsef lexerf (Lexing.from_channel inchan) in
    p
  
let file_lex f =
  let inchan = open_in f in
  let (lexerf, _) = lexpar f in
  let l = (Lexing.from_channel inchan) in
  let rec loop r =
    let t = lexerf l in
    if t = EOF then r else loop (t :: r )
  in List.rev (loop [])

let lexer = Mllexer.token

let file_lexer f = 
    let inchan = open_in f in
    let (lexerf, _) = lexpar f in
    let l = (Lexing.from_channel inchan) in
    lexerf (Lexing.from_channel inchan) 



let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  Format.printf "extension '%s'\n" (Filename.extension f);
  let inchan = open_in f in
  let (lexerf, parsef) = if (Filename.extension f) = ".ml" then (Mllexer.token, Mlparser.exp) else (Milexer.token, Miparser.topDecls) in
  let outchan = open_out (f ^ ".s") in
  let p = parsechan parsef lexerf (Lexing.from_channel inchan) in
  try
    lexbuf p outchan;
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files


