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

let getop c =
  match c with
  | And -> (&&)
  | Or -> (||)
  | Impl -> (fun a b -> not a || b)
  | Xor -> (fun a b -> if a then not b else b)
  | Eq -> (=)

let print_formule printer f =
    let rec to_string f =
        match f with
        | B(b) -> if b then "⊤" else "⊥"
        | P(p) -> printer p
        | Not (f) -> "¬ (" ^ (to_string f) ^")"
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
    in print_endline (to_string f)
