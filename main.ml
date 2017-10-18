(* メイン関数 *)
let go () =
  let program = Parser.expr Lexer.token (Lexing.from_channel stdin) in
  if Array.length Sys.argv = 1 then begin
    Syntax.print program;     (* 入力を表示 *)
    end

(* スタートアップ *)
let _ = go ()
