type binop = And | Or

type 'a state =
  P of 'a (* Juste un état *)
| B of bool
| SNot of 'a state
| SBinop of binop * 'a state * 'a state

type 'a formule =
  State of 'a state (* Juste un état *)
(* Combinateurs temporels *)
| EX of 'a state
| EU of 'a state * 'a state
| AU of 'a state * 'a state
(* Récursivité *)
| FNot of 'a formule
| FBinop of binop * 'a formule * 'a formule

(* Le combinateur f *)
val f : 'a state -> 'a formule
(* Le combinateur g *)
val g : 'a state -> 'a formule
  (* Le combinateur AX *)
val ax : 'a state -> 'a formule

(* Implies for states *)
val impliesS : 'a state -> 'a state -> 'a state
val impliesF : 'a formule -> 'a formule -> 'a formule

val getop : binop -> bool -> bool -> bool
