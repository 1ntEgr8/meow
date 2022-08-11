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

%start <Input.expr * Types.t> main

%%

main:
  e = expr COLON t = ty EOF
  { (e, t) }

expr:
  | i = INT
    { Input.IConst (Constant.CInt i) }
  | TRUE
    { Input.IConst (Constant.CBool true) }
  | FALSE
    { Input.IConst (Constant.CBool false) }
  | x = VAR
    { Input.IVar x }
  | LPAREN FUN x = VAR COLON binder_ty = ty DOT body = expr RPAREN
    { Input.ILambda ((x, binder_ty), body) }
  | LPAREN e1 = expr e2 = expr RPAREN
    { Input.IApp (e1, e2) }
  | REF e = expr
    { Input.IRef e }
  | BANG e = expr
    { Input.IDeref e }
  | LPAREN e1 = expr ASSIGN e2 = expr RPAREN
    { Input.IAssign (e1, e2) }

ty:
  | TYINT
    { Types.TGround Types.TInt }
  | TYBOOL
    { Types.TGround Types.TBool }
  | UNKNOWN
    { Types.TUnknown }
  | LPAREN t1 = ty ARROW t2 = ty RPAREN
    { Types.TArrow (t1, t2) }
