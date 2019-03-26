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
       | And -> Or
       | Or -> And
     in Binop (t,a,b)
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
     in TempBinop (t,b,Binop (And,a,b))

let af x = TempBinop (AU,B true,x)
let eg x = neg (af (neg x))

let ef x = TempBinop (EU,B true,x)
let ag x = neg (ef (neg x))

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
       | Pretty_formule.And -> Binop (And, a, b)
       | Pretty_formule.Or -> Binop (Or, a, b)
       | Pretty_formule.Xor ->
          Binop
            (And,
             Binop (Or, a, b),
             Binop (Or,eg a,neg b))
       | Pretty_formule.Impl -> Binop (Or,neg a, b)
       | Pretty_formule.Eq ->
          Binop
            (Or,
             Binop (And, a, b),
             Binop (And, neg a, neg b))
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
