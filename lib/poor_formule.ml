open Formule

type poor = Poor

type 't poor_formule = (poor, 't) formule

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

let rec neg (f : (poor, 'a) formule) =
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

let rec poor_from_rich : type t. (t,'a) formule -> (poor,'a) formule = function
  | B b -> B b
  | L b -> L b
  | Not b -> neg (poor_from_rich b)
  | Binop (t,a,b) ->
     let a = poor_from_rich a in
     let b = poor_from_rich b in
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
     let a = poor_from_rich a in
     let b = poor_from_rich b in
     begin
       match t with
       | EU -> TempBinop (EU,a,b)
       | AU -> TempBinop (AU,a,b)
       | EW -> TempBinop (EW,a,b)
       | AW -> TempBinop (AW,a,b)
     end
  | TempUnop (t,a) ->
     let a = poor_from_rich a in
     begin
       match t with
       | AX -> TempUnop (AX,a)
       | EX -> TempUnop (EX,a)
       | AF -> af a
       | EF -> ef a
       | AG -> ag a
       | EG -> eg a
     end
