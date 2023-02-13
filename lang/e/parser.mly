%{    open E.Exp
%}

%token <int> NUM
%token <string> STR
%token <string> VAR
%token PLUS "+"
%token TIMES "*"
%token CAT "++"
%token LEN
%token ANNOT ":"
%token POINT "."
%token LAMBDA
%token LET EQ IN
%token ARROW_T "->"
%token NUM_T STR_T
%token OPEN_PAREN "("
%token CLOSE_PAREN ")"
%token EOF

%nonassoc IN
%nonassoc "."
%nonassoc ":"
%right "->"
%left "*"
%left "+"
%left "++"
%nonassoc LEN

%start <E.Exp.t option> prog
%%

let prog :=
  | EOF     ; { None }
  | ~ = exp ; EOF; < Some >

let exp :=
  | paren(exp)
  | atom
  | binop
  | unop
  | let_
  | lam
  | app
  | annot
  | x = VAR ; < v >

let paren(exp) :=
  | "(" ; ~ = exp ; ")" ; <>

let atom :=
  | n = NUM ; < num >
  | s = STR ; < str >

let binop :=
  | e1 = exp ; "+"  ; e2 = exp ; { plus e1 e2 }
  | e1 = exp ; "*"  ; e2 = exp ; { times e1 e2 }
  | e1 = exp ; "++" ; e2 = exp ; { cat e1 e2 }

let unop :=
  | LEN ; ~ = exp ; { len exp }

let let_ :=
  | LET ; var = VAR ; ":" ; ~ = typ ; EQ ; value = exp ; IN ; ~ = exp;
    { let_ ~var ~typ ~value exp }

let lam :=
  | LAMBDA ; var = VAR ; ":" ; ~ = typ ; POINT ; ~ = exp ;
    { lam ~var ~typ exp }

let app :=
  | m = exp ; n = exp ;
    { app m n }

let annot :=
  | ~ = exp ; ":" ; ~ = typ; { annot exp typ }

let typ :=
  | NUM_T ; { E.Typ.num }
  | STR_T ; { E.Typ.str }
  | t1 = typ ; "->" ; t2 = typ ; { E.Typ.arr t1 t2 }
