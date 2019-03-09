type binop = And | Or | Xor | Impl | Eq
type tempUnop = AX | EX | AF | EF | AG | EG
type tempBinop = EU | AU | EW | AW

type 'a formule =
(* Logique propositionnelle *)
  B of bool
| P of 'a
| Not of 'a formule
| Binop of binop * 'a formule * 'a formule
(* Combinateurs temporels *)
| TempUnop of tempUnop * 'a formule
| TempBinop of tempBinop * 'a formule * 'a formule

val getop : binop -> bool -> bool -> bool
val get_string : binop -> string
val get_string_temp : tempUnop -> string
val print_formule : ('a -> string) -> 'a formule -> unit
