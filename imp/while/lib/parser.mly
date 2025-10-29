%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token TAKES
%token EQ
%token WHILE
%token DO
%token LEQ
%token MINUS
%token PLUS
%token MUL
%token SEQ
%token AND
%token OR
%token IF
%token THEN
%token ELSE
%token SKIP
%token LPAREN
%token RPAREN
%token <string> ID
%token <string> CONST
%token EOF

%nonassoc ELSE DO
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%start <cmd> prog

%%

prog:
  | c = cmd; EOF { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | x = ID  { Var(x) }
  | n = CONST { Const(n) }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | LPAREN; e=expr; RPAREN {e}
;


cmd:
  | SKIP; { Skip }
  | x = ID; TAKES; e = expr; { Assign(x,e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  | IF; e0=expr ; THEN; c1=cmd; ELSE; c2=cmd; { If(e0,c1,c2) }
  | WHILE; e=expr; DO; c=cmd; { While(e,c) }
  | LPAREN; c=cmd; RPAREN; {c}
