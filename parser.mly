%{

%}

%token LPAREN RPAREN 
%token PLUS MINUS TIMES DIVIDE MOD
%token PLUSDOT MINUSDOT TIMESDOT DIVIDEDOT 
%token EQUAL NEQ LESS MORE LEQ MOREQ
%token TRUE FALSE
%token LET REC IN
%token IF THEN ELSE
%token <int> NUMBER
/* %token <float> NUMBER */
%token <string> VAR
%token EOF

/* 非終端記号の型の宣言 */
%type <Syntax.t> expr

/* 開始記号 */
%start expr

%nonassoc ELSE IN
%left EQUAL
%left LESS MORE LEQ MOREQ NEQ
%left PLUS MINUS PLUSDOT MINUSDOT
%left TIMES TIMESDOT DIVIDE DIVIDEDOT
%left MOD

/* less ⟷ moreq → more ⟷ leq */
/* %% は省略不可 */
%%

simple_expr:
| NUMBER
  { Syntax.Number ($1) }
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
| expr TIMES expr
  { Syntax.Op ($1, Operator.Times, $3) }
| expr DIVIDE expr
  { Syntax.Op ($1, Operator.Divide, $3) }
| expr MOD expr
  { Syntax.Op ($1, Operator.Mod, $3) }
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
  { Syntax.IfLess ($2, $4, (Syntax.IfEqual ($2, $4, $6, $8)), $6) }
| IF expr LEQ expr THEN expr ELSE expr
  { Syntax.IfLess ($2, $4, $6, (Syntax.IfEqual ($2, $4, $6, $8))) }
| LET VAR EQUAL expr IN expr
  { Syntax.Let (($2, Type.gen_type ()), $4, $6) }
| LET REC VAR VAR EQUAL expr IN expr
  { Syntax.LetRec (($3, Type.gen_type ()), [($4, Type.gen_type ())], $6, $8) }
| app
  { $1 }

app:
| app simple_expr
  { Syntax.Application ($1, [$2]) }
| simple_expr simple_expr
  { Syntax.Application ($1, [$2]) }