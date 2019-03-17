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
  in print_endline (to_string f)

let rec desc_neg f =
  match f with
  | B _ | P _ -> f
  | Binop (b, phi, psi) -> Binop (b, desc_neg phi, desc_neg psi)
  | TempUnop (u, phi) -> TempUnop (u, desc_neg phi)
  | TempBinop (b, phi, psi) -> TempBinop (b, desc_neg  phi, desc_neg  psi)
  | Not g ->
     match g with
     | P _ -> f
     | B b -> B (not b)
     | Binop (b, phi, psi) ->
        let newop =
          match b with
          | And -> Binop (Or, Not phi, Not psi)
          | Or -> Binop (And, Not phi, Not psi)
          | Impl -> Binop (And, phi, Not psi)
          | Xor -> Binop (Eq, phi, psi)
          | Eq -> Binop (Xor, phi, psi)
        in desc_neg newop
     | TempUnop (u, phi) ->
        let newop =
          match u with
            | AX -> EX
            | EX -> AX
            | AF -> EG
            | EG -> AF
            | EF -> AG
            | AG -> EF
        in desc_neg (TempUnop (newop, Not phi))
     | TempBinop (b, phi, psi) ->
        let newop = (* TODO verify *)
          match b with
          | AU -> EW
          | EW -> AU
          | EU -> AW
          | AW -> EU
        in desc_neg (TempBinop (newop, Not phi, Not psi))
     | Not f -> desc_neg f
