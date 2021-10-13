%{
    open E.Exp
%}

%token <int> NUM
%token <string> STR
%token <string> VAR
%token PLUS "+"
%token TIMES "*"
%token CAT
%token LEN
%token ANNOT ":"
%token LET EQ IN
%token EOF
%token NUM_T STR_T

%nonassoc IN
%nonassoc ":"
%left "*"
%left "+"
%nonassoc CAT
%nonassoc LEN

%start <E.Exp.t option> prog
%%

let prog :=
  | EOF     ; { None }
  | ~ = exp ; EOF; < Some >

let exp :=
  | ~ = let_  ; <>
  | ~ = atom  ; <>
  | ~ = unop  ; <>
  | ~ = binop ; <>
  | ~ = annot ; <>

let atom :=
  | n = NUM ; < num >
  | s = STR ; < str >

let binop :=
  | e1 = exp ; "+" ; e2 = exp ; { plus e1 e2 }
  | e1 = exp ; "*" ; e2 = exp ; { times e1 e2 }
  | CAT ; e1 = exp ; e2 = exp ; { cat e1 e2 }

let unop :=
  | LEN ; ~ = exp ; { len exp }
let let_ :=
  | LET ; var = VAR ; ":" ; ~ = typ ; EQ ; value = exp ; IN ; ~ = exp;
    { let_ ~var ~typ ~value exp }

let annot :=
  | ~ = exp ; ":" ; ~ = typ; { annot exp typ }

let typ :=
  | NUM_T ; { E.Typ.num }
  | STR_T ; { E.Typ.str }
