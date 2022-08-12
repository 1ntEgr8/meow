%token MEOW
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
%token SUCC
%token <string> VAR
%token <int> INT
%token EOF

%nonassoc SUCC INT VAR LPAREN TRUE FALSE
%left ARROW
%left ASSIGN
%left BANG
%left REF
%left APP

%start <Input.expr * (Types.t option)> main

%%

main:
  | e = expr COLON t = ty EOF
    { (e, Some t) }
  | e = expr EOF
    { (e, None) }

expr:
  | i = INT
    { Input.IConst (Constant.CInt i) }
  | TRUE
    { Input.IConst (Constant.CBool true) }
  | FALSE
    { Input.IConst (Constant.CBool false) }
  | SUCC
    { Input.IConst (Constant.CSucc) }
  | MEOW
    { Input.IConst (Constant.CMeow) }
  | x = VAR
    { Input.IVar x }
  | LPAREN FUN x = VAR COLON binder_ty = ty DOT body = expr RPAREN
    { Input.ILambda ((x, Some binder_ty), body) }
  | LPAREN FUN x = VAR DOT body = expr RPAREN
    { Input.ILambda ((x, None), body) }
  | e1 = expr e2 = expr
    { Input.IApp (e1, e2) } %prec APP
  | REF e = expr
    { Input.IRef e }
  | BANG e = expr
    { Input.IDeref e }
  | e1 = expr ASSIGN e2 = expr
    { Input.IAssign (e1, e2) }
  | LPAREN e = expr RPAREN
    { e }

ty:
  | TYINT
    { Types.TGround Types.TInt }
  | TYBOOL
    { Types.TGround Types.TBool }
  | REF t = ty
    { Types.TRef t }
  | UNKNOWN | MEOW
    { Types.TUnknown }
  | t1 = ty ARROW t2 = ty
    { Types.TArrow (t1, t2) }
  | LPAREN t = ty RPAREN
    { t }
