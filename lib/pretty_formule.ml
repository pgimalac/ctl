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

let get_string_temp c =
    match c with
    | AX -> "AX"
    | EX -> "EX"
    | AF -> "AF"
    | EF -> "EF"
    | AG -> "AG"
    | EG -> "EG"

let get_string c =
    match c with
    | And -> "∧"
    | Or -> "∨"
    | Impl -> "⇒"
    | Xor -> "⊕"
    | Eq -> "⇔"

let string_of_formule printer f =
  let rec to_string f =
    match f with
    | B b -> if b then "⊤" else "⊥"
    | P p -> printer p
    | Not f -> "¬ (" ^ (to_string f) ^")"
    | Binop(b, phi, psi) ->
       "(" ^ (to_string phi) ^ ") " ^ (get_string b) ^ " (" ^ (to_string psi) ^")"
    | TempUnop(u, phi) -> (get_string_temp u) ^ " (" ^ (to_string phi) ^")"
    | TempBinop(b, phi, psi) ->
       let phi = to_string phi in
       let psi = to_string psi in
       match b with
       | EU -> "E (" ^ phi ^ ") U (" ^ psi ^")"
       | AU -> "A (" ^ phi ^ ") U (" ^ psi ^")"
       | EW -> "E (" ^ phi ^ ") W (" ^ psi ^")"
       | AW -> "A (" ^ phi ^ ") W (" ^ psi ^")"
  in to_string f
           
let print_formule printer f = print_endline (string_of_formule printer f)
