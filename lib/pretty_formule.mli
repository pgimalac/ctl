type binop = And | Or | Xor | Impl | Eq
type tempUnop = AX | EX | AF | EF | AG | EG
type tempBinop = EU | AU | EW | AW

type 'a pretty_formule =
  (* Logique propositionnelle *)
  | B of bool
  | P of 'a
  | Not of 'a pretty_formule
  | Binop of binop * 'a pretty_formule * 'a pretty_formule
  (* Combinateurs temporels *)
  | TempUnop of tempUnop * 'a pretty_formule
  | TempBinop of tempBinop * 'a pretty_formule * 'a pretty_formule

val get_string : binop -> string
val get_string_temp : tempUnop -> string
val print_formule : ('a -> string) -> 'a pretty_formule -> unit
