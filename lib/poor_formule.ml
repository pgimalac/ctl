open Formule

type poor = unit

type 'a lit =
  | N of 'a
  | P of 'a

type 't poor_formule = (poor, 't lit) formule

let getop (c : poor binop) =
  match c with
  | And -> (&&)
  | Or -> (||)

let et x y =
  match x with
  | B b -> if b then y else x
  | _ ->
     match y with
     | B b -> if b then x else y
     | _ -> Binop (And,x,y)

let ou x y =
  match x with
  | B b -> if b then x else y
  | _ ->
     match y with
     | B b -> if b then y else x
     | _ -> Binop (Or,x,y)

let rec neg (f : 'a poor_formule) =
  match f with
  | B b -> B (not b)
  | L b ->
     L (
       match b with
       | P b -> N b
       | N b -> P b
       )
  | Binop (t,a,b) ->
     let a = neg a in
     let b = neg b in
     let t =
       match t with
       | And -> ou
       | Or -> et
     in t a b
  | TempUnop (t,a) ->
     let a = neg a in
     let t =
       match t with
       | EX -> AX
       | AX -> EX
     in TempUnop (t,a)
  | TempBinop (t,a,b) ->
     let a = neg a in
     let b = neg b in
     let t =
       match t with
       | AU -> EW (* TODO vérifier celui-ci *)
       | EW -> AU (* TODO vérifier celui-ci *)
       | EU -> AW
       | AW -> EU
     in TempBinop (t,b,et a b)

let af x = TempBinop (AU,B true,x)
let eg x = neg (af (neg x))

let ef x = TempBinop (EU,B true,x)
let ag x = neg (ef (neg x))

let rec to_poor : type t. (t,'a) formule -> 'a poor_formule = function
  | B b -> B b
  | L b -> L (P b)
  | Not b -> neg (to_poor b)
  | Binop (t,a,b) ->
     let a = to_poor a in
     let b = to_poor b in
     begin
       match t with
       | And -> et a b
       | Or -> ou a b
       | Xor ->
          et (ou a b) (ou (neg a) (neg b))
       | Impl -> Binop (Or,neg a, b)
       | Eq ->
          ou (et a b) (et (neg a) (neg b))
     end
  | TempBinop (t,a,b) ->
     TempBinop (t, to_poor a, to_poor b)
  | TempUnop (t,a) ->
     let a = to_poor a in
     begin
       match t with
       | AX -> TempUnop (AX,a)
       | EX -> TempUnop (EX,a)
       | AF -> af a
       | EF -> ef a
       | AG -> ag a
       | EG -> eg a
     end

let string_of_poor_formule printer =
  let aux = function
    | N p ->  "¬ (" ^ (printer p) ^")"
    | P p -> printer p
  in string_of_formule aux
