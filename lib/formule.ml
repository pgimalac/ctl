type binop = And | Or
type tempUnop = AX | EX
type tempBinop = EU | AU | EW | AW

type 'a formule =
  (* Logique propositionnelle *)
  | B of bool
  | P of 'a
  | Not of 'a formule
  | Binop of binop * 'a formule * 'a formule
  (* Combinateurs temporels *)
  | TempUnop of tempUnop * 'a formule
  | TempBinop of tempBinop * 'a formule * 'a formule

let getop c =
  match c with
  | And -> (&&)
  | Or -> (||)

let af x = TempBinop (AU,B true,x)
let eg x = Not (af (Not x))

let ef x = TempBinop (EU,B true,x)
let ag x = Not (ef (Not x))

let rec formule_from_pretty f =
  match f with
  | Pretty_formule.B b -> B b
  | Pretty_formule.P b -> P b
  | Pretty_formule.Not b -> Not (formule_from_pretty b)
  | Pretty_formule.Binop (t,a,b) ->
     begin
       let a = formule_from_pretty a in
       let b = formule_from_pretty b in
       match t with
       | Pretty_formule.And -> Binop (And,a,b)
       | Pretty_formule.Or -> Binop (Or,a,b)
       | Pretty_formule.Xor -> Binop (And, Binop (Or,a,b), Binop (Or,Not a,Not b))
       | Pretty_formule.Impl -> Binop (Or,Not a,b)
       | Pretty_formule.Eq -> Binop (Or, Binop (And,a,b), Binop (And,Not a,Not b))
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
        in desc_neg newop
     | TempUnop (u, phi) ->
        let newop =
          match u with
          | AX -> EX
          | EX -> AX
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
