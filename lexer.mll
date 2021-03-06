{
open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let dot   = '.'
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let beta  = lower | upper | digit

rule token = parse
| space+ { token lexbuf }       (* スペースは読み飛ばす *)
| ";;"   { token lexbuf }
| "(*" [^ '\n']* "\n"           (* ( * から行末まではコメント *)
         { token lexbuf }
| "("    { LPAREN }
| ")"    { RPAREN }
| "+"    { PLUS }
| "-"    { MINUS }
| "*"    { TIMES }
| "/"    { DIVIDE }
| "+."   { PLUSDOT }
| "-."   { MINUSDOT }
| "*."   { TIMESDOT }
| "/."   { DIVIDEDOT }
| "mod"  { MOD }
| "%"    { RESIDUE }
| "="    { EQUAL }
| "<>"   { NEQ }
| "<"    { LESS }
| ">"    { MORE }
| "<="   { LEQ }
| ">="   { MOREQ }
| "true" { TRUE }
| "false"{ FALSE }
| "if"   { IF }
| "then" { THEN }
| "else" { ELSE }
| "let"  { LET }
| "rec"  { REC }
| "in"   { IN }
| digit+                        (* 数字が１個以上 *)
         { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
| digit+ dot digit* (* 小数 *)
  	 { REAL (float_of_string (Lexing.lexeme lexbuf)) }
| lower beta*
	 { VAR (Lexing.lexeme lexbuf) }
| eof    { EOF }
| _      { failwith ("unknown token: " ^ Lexing.lexeme lexbuf) }