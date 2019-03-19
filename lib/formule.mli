type binop = And | Or
type tempUnop = AX | EX
type tempBinop = EU | AU | EW | AW

type 'a formule =
  (* Logique propositionnelle *)
  | B of bool
  | P of 'a
  | Not of 'a formule
  | Binop of binop * 'a formule * 'a formule
  (* Combinateurs temporels *)
  | TempUnop of tempUnop * 'a formule
  | TempBinop of tempBinop * 'a formule * 'a formule

val af : 'a formule -> 'a formule
val eg : 'a formule -> 'a formule
val getop : binop -> bool -> bool -> bool

val desc_neg : 'a formule -> 'a formule
val formule_from_pretty : 'a Pretty_formule.pretty_formule -> 'a formule
