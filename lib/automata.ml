open Formule
open Marqueur

type 'a pbf =
  | P_pbf of 'a
  | Et_pbf of ('a pbf * 'a pbf)
  | Ou_pbf of ('a pbf * 'a pbf)
  | B_pbf of bool

let string_of_pbf stringer f =
  let rec aux f =
    match f with
    | P_pbf x -> stringer x
    | Et_pbf (x,y) -> "Et_pbf (" ^ aux x  ^ ") (" ^ aux y ^ ")"
    | Ou_pbf (x,y) -> "Ou_pbf (" ^ aux x  ^ ") (" ^ aux y ^ ")"
    | B_pbf x -> string_of_bool x
  in aux f

let rec tau d (phi,sigma) =
  let mult f i phi = (* f = true -> Et_pbf | f = false -> Ou_pbf *) (* TODO boolean blindlness *)
    if i = 0 (* clause vide *)
    then
      if f
      then B_pbf true
      else B_pbf false
    else
      let f t = if f then Et_pbf t else Ou_pbf t in
      let rec mult' i =
        if i <= 1
        then P_pbf (i,phi)
        else f (P_pbf (i,phi), mult' (i-1))
      in mult' i
  in
  match phi with
  | B b -> B_pbf b
  | L p ->
     begin
       match p with
       | P p -> B_pbf (T.SV.mem p sigma)
       | N p -> B_pbf (not (T.SV.mem p sigma))
     end
  | Binop (t, phi, psi) ->
     begin
       match t with
       | And -> Et_pbf (tau d (phi, sigma), tau d (psi, sigma))
       | Or ->  Ou_pbf (tau d (phi, sigma), tau d (psi, sigma))
     end
  | TempUnop (t,phi) ->
     begin
       match t with
       | EX -> mult false d phi
       | AX -> mult true d  phi
     end
  | TempBinop (t,psi1,psi2) ->
     let left = tau d (psi2, sigma) in
     let left' = tau d (psi1, sigma) in
     let right' =
       match t with
       | EU | EW -> mult false d phi
       | AU | AW ->  mult true d phi in
    Ou_pbf (left, Et_pbf (left',right'))

let poids f =
  match f with
  | TempBinop (a,_,_) ->
     begin
       match a with
       | EU | AU -> 1
       | EW | AW -> 2
     end
  | _ -> 0
