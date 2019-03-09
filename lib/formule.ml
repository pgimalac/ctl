type binop = And | Or | Xor | Impl | Eq
type tempUnop = AX | EX | AF | EF | AG | EG
type tempBinop = EU | AU (* | EW | AW *)

type 'a formule =
(* Logique propositionnelle *)
  B of bool
| P of 'a
| Not of 'a formule
| Binop of binop * 'a formule * 'a formule
(* Combinateurs temporels *)
| TempUnop of tempUnop * 'a formule
| TempBinop of tempBinop * 'a formule * 'a formule

let getop c =
  match c with
  | And -> (&&)
  | Or -> (||)
  | Impl -> (fun a b -> not a || b)
  | Xor -> (fun a b -> if a then not b else b)
  | Eq -> (=)
