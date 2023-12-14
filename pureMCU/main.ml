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




let mienv checkpoint =
  match checkpoint with
  | HI.HandlingError env ->
      env
  | _ ->
      assert false

let mistate checkpoint : int =
  match HI.top (mienv checkpoint) with
  | Some (HI.Element (s, _, _, _)) ->
      HI.number s
  | None ->
      0

let mlenv checkpoint =
  match checkpoint with
  | MI.HandlingError env ->
      env
  | _ ->
      assert false

let mlstate checkpoint : int =
  match MI.top (mlenv checkpoint) with
  | Some (MI.Element (s, _, _, _)) ->
      MI.number s
  | None ->
      0


let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 

let miget text checkpoint i =
  match HI.get i (mienv checkpoint) with
  | Some (HI.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

let mlget text checkpoint i =
match MI.get i (mlenv checkpoint) with
  | Some (MI.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

(*
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
*)


let succeed _v =
  assert false

(*
let parsechan parsef lexerf l =
    let supplier = HI.lexer_lexbuf_to_supplier lexerf l in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint = Miparser.Incremental.topDecls l.lex_curr_p in
    HI.loop_handle succeed (fail "hoo" buffer) supplier checkpoint
*)


let parsechan_mi l =
    let parsef = Miparser.topDecls in
    let lexerf = Indenter.token in
    let fail text buffer (checkpoint : _ HI.checkpoint) =
        (* Indicate where in the input file the error occurred. *)
        let location = L.range (E.last buffer) in
        (* Show the tokens just before and just after the error. *)
        let indication = Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
        (* Fetch an error message from the database. *)
        let message = MiParsingErrors.message (mistate checkpoint) in
        (* Expand away the $i keywords that might appear in the message. *)
        let message = E.expand (miget text checkpoint) message in
        (* Show these three components. *)
        Format.eprintf "%s%s%s%!" location indication message;
        exit 1 in
    let supplier = HI.lexer_lexbuf_to_supplier lexerf l in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint = Miparser.Incremental.topDecls l.lex_curr_p in
    HI.loop_handle succeed (fail "hoo" buffer) supplier checkpoint

let parsechan_ml l =
    let parsef = Mlparser.exp in
    let lexerf = Mllexer.token in
    let fail text buffer (checkpoint : _ MI.checkpoint) =
        (* Indicate where in the input file the error occurred. *)
        let location = L.range (E.last buffer) in
        (* Show the tokens just before and just after the error. *)
        let indication = Printf.sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
        (* Fetch an error message from the database. *)
        let message = MiParsingErrors.message (mlstate checkpoint) in
        (* Expand away the $i keywords that might appear in the message. *)
        let message = E.expand (mlget text checkpoint) message in
        (* Show these three components. *)
        Format.eprintf "%s%s%s%!" location indication message;
        exit 1 in
    let supplier = MI.lexer_lexbuf_to_supplier lexerf l in
    let buffer, supplier = E.wrap_supplier supplier in
    let checkpoint = Mlparser.Incremental.exp l.lex_curr_p in
    MI.loop_handle succeed (fail "hoo" buffer) supplier checkpoint

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
    let p = parsechan_ml lex in
    lexbuf p stdout  (* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)

(*let lexpar f = if (Filename.extension f) = ".ml" then (Mllexer.token, Mlparser.exp) else (Milexer.token, Miparser.exp)*)
let lexpar f = if (Filename.extension f) = ".ml" then (Mllexer.token, parsechan_ml) else (Indenter.token, parsechan_mi)

let file_ast f =
  let inchan = open_in f in
  let (lexerf, parsech) = lexpar f in
  let p = parsech (Lexing.from_channel inchan) in
    p
  
let file_lex f =
  let inchan = open_in f in
  let (lexerf, _) = lexpar f in
  let l = (Lexing.from_channel inchan) in
  let rec loop r =
    let t = lexerf l in
    if t = EOF then r else loop (t :: r )
  in List.rev (loop [])

(*let lexer = Mllexer.token*)

let file_lexer f = 
    let inchan = open_in f in
    let (lexerf, _) = lexpar f in
    let l = (Lexing.from_channel inchan) in
    lexerf (Lexing.from_channel inchan) 



let file f = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  Format.printf "extension '%s'\n" (Filename.extension f);
  let inchan = open_in f in
  let (lexerf, parsechan) = lexpar f in (*if (Filename.extension f) = ".ml" then (Mllexer.token, Mlparser.exp) else (Milexer.token, Miparser.topDecls) in *)
  let outchan = open_out (f ^ ".s") in
  let p = parsechan (Lexing.from_channel inchan) in
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


