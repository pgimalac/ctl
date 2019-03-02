type binop = And | Or

type 'a state =
  P of 'a (* Juste un état *)
| B of bool
| SNot of 'a state
| SBinop of binop * 'a state * 'a state

type 'a formule =
  D of 'a state (* Juste un état *)
(* Combinateurs temporels *)
| EX of 'a state
| AX of 'a state
| EU of 'a state * 'a state
| AU of 'a state * 'a state
(* Récursivité *)
| FNot of 'a formule
| FBinop of binop * 'a formule * 'a formule

(* Le combinateur f *)
let f x = EU (B true, x)
(* Le combinateur g *)
let g x = FNot (f (SNot x))
