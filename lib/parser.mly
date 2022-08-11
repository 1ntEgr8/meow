%token TRUE
%token FALSE
%token FUN
%token REF
%token BANG
%token LPAREN
%token RPAREN
%token UNKNOWN
%token COLON
%token DOT
%token ARROW
%token ASSIGN
%token TYINT
%token TYBOOL
%token <string> VAR
%token <int> INT
%token EOF

%start <Expr.expr * Expr.ty> main

%%

main:
  e = expr COLON t = ty EOF
  { (e, t) }

expr:
  | i = INT
    { Expr.EConst (Expr.CInt i) }
  | TRUE
    { Expr.EConst (Expr.CBool true) }
  | FALSE
    { Expr.EConst (Expr.CBool false) }
  | x = VAR
    { Expr.EVar x }
  | LPAREN FUN x = VAR COLON binder_ty = ty DOT body = expr RPAREN
    { Expr.ELambda ((x, binder_ty), body) }
  | LPAREN e1 = expr e2 = expr RPAREN
    { Expr.EApp (e1, e2) }
  | REF e = expr
    { Expr.ERef e }
  | BANG e = expr
    { Expr.EDeref e }
  | LPAREN e1 = expr ASSIGN e2 = expr RPAREN
    { Expr.EAssign (e1, e2) }

ty:
  | TYINT
    { Expr.TGround Expr.TInt }
  | TYBOOL
    { Expr.TGround Expr.TBool }
  | UNKNOWN
    { Expr.TUnknown }
  | LPAREN t1 = ty ARROW t2 = ty RPAREN
    { Expr.TArrow (t1, t2) }
