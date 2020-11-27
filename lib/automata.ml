open Poor_formule
open Kripke

type 'a pbf =
  | P_pbf of 'a
  | Et_pbf of ('a pbf * 'a pbf)
  | Ou_pbf of ('a pbf * 'a pbf)
  | B_pbf of bool

(* Permet de simplifier le Et_pbf si possible *)
let et x y =
  match x with
  | B_pbf b -> if b then y else x
  | _ -> ( match y with B_pbf b -> if b then x else y | _ -> Et_pbf (x, y) )

(* Permet de simplifier le Ou_pbf si possible *)
let ou x y =
  match x with
  | B_pbf b -> if b then x else y
  | _ -> ( match y with B_pbf b -> if b then y else x | _ -> Ou_pbf (x, y) )

let string_of_pbf stringer =
  let rec aux f =
    match f with
    | P_pbf x -> stringer x
    | Et_pbf (x, y) -> "Et_pbf (" ^ aux x ^ ") (" ^ aux y ^ ")"
    | Ou_pbf (x, y) -> "Ou_pbf (" ^ aux x ^ ") (" ^ aux y ^ ")"
    | B_pbf x -> string_of_bool x
  in
  aux

module Make (K : Kripke.K) = struct
  let rec tau d ((phi, sigma) : K.SV.elt poor_formule * K.SV.t) =
    let mult f i phi =
      (* f = true -> Et_pbf | f = false -> Ou_pbf *)
      (* TODO boolean blindlness *)
      if i = 0 (* clause vide *) then if f then B_pbf true else B_pbf false
      else
        let f = if f then et else ou in
        let rec mult' i =
          if i <= 1 then P_pbf (i, phi) else f (P_pbf (i, phi)) (mult' (i - 1))
        in
        mult' i
    in
    match phi with
    | B b -> B_pbf b
    | L p -> (
        match p with
        | P p -> B_pbf (K.SV.mem p sigma)
        | N p -> B_pbf (not (K.SV.mem p sigma)) )
    | Binop (t, phi, psi) -> (
        match t with
        | And -> et (tau d (phi, sigma)) (tau d (psi, sigma))
        | Or -> ou (tau d (phi, sigma)) (tau d (psi, sigma)) )
    | TempUnop (t, phi) -> (
        match t with EX -> mult false d phi | AX -> mult true d phi )
    | TempBinop (t, psi1, psi2) ->
        let left = tau d (psi2, sigma) in
        let left' = tau d (psi1, sigma) in
        let right' =
          match t with
          | EU | EW -> mult false d phi
          | AU | AW -> mult true d phi
        in
        ou left (et left' right')
end

module AutomataS = Make (KripkeS)

let poids (f : 'a poor_formule) =
  match f with
  | TempBinop (a, _, _) -> ( match a with EU | AU -> 1 | EW | AW -> 2 )
  | _ -> 0
