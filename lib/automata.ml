open Formule
open Marqueur

type 'a pbf =
  | P_pbf of 'a
  | Et_pbf of ('a pbf * 'a pbf)
  | Ou_pbf of ('a pbf * 'a pbf)
  | B_pbf of bool

let rec tau d (phi,sigma) =
  let mult f i phi =
    let rec mult' i =
      if i <= 0
      then P_pbf (i,phi)
      else f (P_pbf (i,phi), mult' (i-1))
    in mult' i
  in
  match phi with
  | B b -> B_pbf b
  | L p ->
     begin
       match p with
       | P p -> B_pbf (S.mem p sigma)
       | N p -> B_pbf (not (S.mem p sigma))
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
       | EX -> mult (fun (x,y) -> Ou_pbf (x,y)) d phi
       | AX -> mult (fun (x,y) -> Et_pbf (x,y)) d phi
     end
  | TempBinop (t,psi1,psi2) ->
     let left = tau d (psi2, sigma) in
     let left' = tau d (psi1, sigma) in
     let right' =
       match t with
       | EU | EW -> mult (fun (x,y) -> Ou_pbf (x,y)) d phi
       | AU | AW ->  mult (fun (x,y) -> Et_pbf (x,y)) d phi in
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
