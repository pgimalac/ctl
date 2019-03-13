%{
    open Formule
    open Marqueur
%}

%token <string> VAR
%token <bool> BOOL
%token LPAR RPAR
%token NOT
%token AND OR IMPL EQU XOR
%token ALL EXIST
%token FUTURE ALWAYS TOMORROW
%token UNTIL WEAKUNTIL
%token EOF
%start main
%type <string Formule.formule> main

%token <int> INT
%token EOL
%token DOT
%type <T.SV.t> varlist
%type <S.t>    intlist
%type <(Marqueur.T.SV.t * Marqueur.S.t) Marqueur.M.t> graph

%start graph_main
%type <(Marqueur.T.SV.t * Marqueur.S.t) Marqueur.M.t> graph_main
%%
main:
    expr EOF                  { $1 }
;
expr:
    VAR                       { P($1) }
  | BOOL                      { B($1) }
  | LPAR expr RPAR            { $2 }
  | NOT expr                  { Not($2) }
  | expr OR expr              { Binop(Or, $1, $3) }
  | expr AND expr             { Binop(And, $1, $3) }
  | expr XOR expr             { Binop(Xor, $1, $3) }
  | expr IMPL expr            { Binop(Impl, $1, $3) }
  | expr EQU expr             { Binop(Eq, $1, $3) }
  | ALL FUTURE expr           { TempUnop(AF, $3) }
  | EXIST FUTURE expr         { TempUnop(EF, $3) }
  | ALL ALWAYS expr           { TempUnop(AG, $3) }
  | EXIST ALWAYS expr         { TempUnop(EG, $3) }
  | ALL TOMORROW expr         { TempUnop(AX, $3) }
  | EXIST TOMORROW expr       { TempUnop(EX, $3) }
  | ALL expr UNTIL expr       { TempBinop(AU, $2, $4) }
  | EXIST expr UNTIL expr     { TempBinop(EU, $2, $4) }
  | ALL expr WEAKUNTIL expr   { TempBinop(AW, $2, $4) }
  | EXIST expr WEAKUNTIL expr { TempBinop(EW, $2, $4) }
;

graph_main:
    graph EOF {$1}
;

graph:
    line {let (x,y) = $1 in M.singleton x y}
  | line EOL graph {let (x,y) = $1 in M.add x y $3}
;

line:
    INT DOT varlist DOT intlist {($1,($3,$5))}
;

varlist:
    VAR                       {T.SV.singleton $1}
  | VAR varlist               {T.SV.add $1 $2}
;

intlist:
    INT                       {S.singleton $1}
  | INT intlist               {S.add $1 $2}
;
