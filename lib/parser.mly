%{
  open Formule
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
