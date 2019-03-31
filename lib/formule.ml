type binop = And | Or
type tempUnop = AX | EX
type tempBinop = EU | AU | EW | AW

type 'a lit =
  | P of 'a (* Une variable *)
  | N of 'a (* Sa négation *)

type 'a formule =
  (* Logique propositionnelle *)
  | B of bool
  | L of 'a lit
  | Binop of binop * 'a formule * 'a formule
  (* Combinateurs temporels *)
  | TempUnop of tempUnop * 'a formule
  | TempBinop of tempBinop * 'a formule * 'a formule

let getop c =
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

let rec neg f =
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

let rec pretty_from_formule f =
  match f with
  | B b -> Pretty_formule.B b
  | L c ->
     begin
       match c with
       | P b -> Pretty_formule.P b
       | N b -> Pretty_formule.Not (Pretty_formule.P b)
     end
  | Binop (t,a,b) ->
     let a = pretty_from_formule a in
     let b = pretty_from_formule b in
     let t =
       match t with
       | And -> Pretty_formule.And
       | Or -> Pretty_formule.Or in
     Pretty_formule.Binop (t,a,b)
  | TempUnop (t,a) ->
      let a = pretty_from_formule a in
      let t =
        match t with
        | EX -> Pretty_formule.EX
        | AX -> Pretty_formule.AX in
      Pretty_formule.TempUnop (t,a)
  | TempBinop (t,a,b) ->
     let a = pretty_from_formule a in
     let b = pretty_from_formule b in
     let t =
       match t with
       | EU -> Pretty_formule.EU
       | EW -> Pretty_formule.EW
       | AU -> Pretty_formule.AU
       | AW -> Pretty_formule.AW  in
     Pretty_formule.TempBinop (t,a,b)

let rec formule_from_pretty f =
  match f with
  | Pretty_formule.B b -> B b
  | Pretty_formule.P b -> L (P b)
  | Pretty_formule.Not b -> neg (formule_from_pretty b)
  | Pretty_formule.Binop (t,a,b) ->
     let a = formule_from_pretty a in
     let b = formule_from_pretty b in
     begin
       match t with
       | Pretty_formule.And -> et a b
       | Pretty_formule.Or -> ou a b
       | Pretty_formule.Xor ->
          et (ou a b) (ou (neg a) (neg b))
       | Pretty_formule.Impl -> Binop (Or,neg a, b)
       | Pretty_formule.Eq ->
          ou (et a b) (et (neg a) (neg b))
     end
  | Pretty_formule.TempBinop (t,a,b) ->
     let a = formule_from_pretty a in
     let b = formule_from_pretty b in
     begin
       match t with
       | Pretty_formule.EU -> TempBinop (EU,a,b)
       | Pretty_formule.AU -> TempBinop (AU,a,b)
       | Pretty_formule.EW -> TempBinop (EW,a,b)
       | Pretty_formule.AW -> TempBinop (AW,a,b)
     end
  | Pretty_formule.TempUnop (t,a) ->
     let a = formule_from_pretty a in
     begin
       match t with
       | Pretty_formule.AX -> TempUnop (AX,a)
       | Pretty_formule.EX -> TempUnop (EX,a)
       | Pretty_formule.AF -> af a
       | Pretty_formule.EF -> ef a
       | Pretty_formule.AG -> ag a
       | Pretty_formule.EG -> eg a
     end

let string_of_formule printer f = Pretty_formule.string_of_formule printer (pretty_from_formule f)
