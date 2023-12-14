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


(* https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc-syntax-errors/calc.ml *)

module HI = Miparser.MenhirInterpreter
module MI = Mlparser.MenhirInterpreter
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil




let env checkpoint =
  match checkpoint with
  | HI.HandlingError env ->
      env
  | _ ->
      assert false



let state checkpoint : int =
  match HI.top (env checkpoint) with
  | Some (HI.Element (s, _, _, _)) ->
      HI.number s
  | None ->
      0


let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 

let get text checkpoint i =
  match HI.get i (env checkpoint) with
  | Some (HI.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

let fail text buffer (checkpoint : _ HI.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = MiParsingErrors.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  Format.eprintf "%s%s%s%!" location indication message;
  exit 1


let succeed _v =
  assert false


let parsechan parsef lexerf l =
    let supplier = HI.lexer_lexbuf_to_supplier lexerf l in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint = Miparser.Incremental.topDecls l.lex_curr_p in
    HI.loop_handle succeed (fail "hoo" buffer) supplier checkpoint



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


