%{

%}

%token LPAREN RPAREN 
%token PLUS MINUS TIMES DIVIDE MOD RESIDUE
%token PLUSDOT MINUSDOT TIMESDOT DIVIDEDOT 
%token EQUAL NEQ LESS MORE LEQ MOREQ
%token TRUE FALSE
%token LET REC IN
%token IF THEN ELSE
%token <int> NUMBER
%token <float> REAL
%token <string> VAR
%token EOF

/* 非終端記号の型の宣言 */
%type <Syntax.t> start

/* 開始記号 */
%start start

%nonassoc ELSE IN
%left EQUAL
%left LESS MORE LEQ MOREQ NEQ
%left PLUS MINUS PLUSDOT MINUSDOT
%left TIMES TIMESDOT DIVIDE DIVIDEDOT MOD RESIDUE
%nonassoc UNARY

/* less ⟷ moreq → more ⟷ leq */
/* %% は省略不可 */
%%
start:
| expr
  { $1 }

simple_expr:
| NUMBER
  { Syntax.Number ($1) }
| REAL
  { Syntax.Real ($1) }
| VAR
  { Syntax.Variable ($1) }
| LPAREN expr RPAREN
  { $2 }
    
expr:
| simple_expr
  { $1 }
| expr PLUS expr
  { Syntax.Op ($1, Operator.Plus, $3) }
| expr MINUS expr
  { Syntax.Op ($1, Operator.Minus, $3) }
| MINUS expr %prec UNARY
  { Syntax.Op (Syntax.Number (0), Operator.Minus, $2) }
| expr TIMES expr
  { Syntax.Op ($1, Operator.Times, $3) }
| expr DIVIDE expr
  { Syntax.Op ($1, Operator.Divide, $3) }
| expr MOD expr
  { Syntax.Op ($1, Operator.Mod, $3) }
| expr RESIDUE expr
  { Syntax.Op ($1, Operator.Residue, $3) }
| expr PLUSDOT expr
  { Syntax.Op ($1, Operator.PlusDot, $3) }
| expr MINUSDOT expr
  { Syntax.Op ($1, Operator.MinusDot, $3) }
| expr TIMESDOT expr
  { Syntax.Op ($1, Operator.TimesDot, $3) }
| expr DIVIDEDOT expr
  { Syntax.Op ($1, Operator.DivideDot, $3) }
| IF expr EQUAL expr THEN expr ELSE expr
  { Syntax.IfEqual ($2, $4, $6, $8) }
| IF expr NEQ expr THEN expr ELSE expr
  { Syntax.IfEqual ($2, $4, $8, $6) }
| IF expr LESS expr THEN expr ELSE expr
  { Syntax.IfLess ($2, $4, $6, $8) }
| IF expr MOREQ expr THEN expr ELSE expr
  { Syntax.IfLess ($2, $4, $8, $6) }
| IF expr MORE expr THEN expr ELSE expr
  { Syntax.IfLess ($4, $2, $6, $8) }
| IF expr LEQ expr THEN expr ELSE expr
  { Syntax.IfLess ($4, $2, $8, $6) }
| LET VAR EQUAL expr IN expr
  { Syntax.Let (($2, Type.gen_type ()), $4, $6) }
| LET REC VAR var EQUAL expr IN expr
  { Syntax.LetRec (($3, Type.gen_type ()), $4, $6, $8) }
| simple_expr args
  { Syntax.Application ($1, $2) }

args:
| simple_expr args
  { $1 :: $2 }
| simple_expr
  { [$1] }

var:
| VAR
  { [$1, Type.gen_type ()] }
| VAR var
  { ($1, Type.gen_type ()) :: $2 }