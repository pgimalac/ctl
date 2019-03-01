type binop = And | Or | Implies

type 'a state =
  P of 'a
| B of bool
| Not of 'a state
| Binop of binop * 'a state * 'a state

type 'a formule =
  EX of 'a state
| AX of 'a state
| EU of 'a state * 'a state
| AU of 'a state * 'a state

let f x = EU (B true, x)
