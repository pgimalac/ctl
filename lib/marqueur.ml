open Formule
(* On représente les graphes comme une Map (Int,(bool, (Set Int)))*)

let rec marquageS f _ =
  match f with
  | P _ -> failwith "todo"
  | _ -> failwith "todo"

let rec marquageF f m =
  match f with
  | State s -> marquageS s m
  | _ -> failwith "todo"
