open Formule

module L = struct
  type t = int
  let compare = compare
end

module S = Set.Make(L)
module M = Map.Make(L)

module type VARIABLES = sig
  type t
  val compare : t -> t -> int
end

module Make (V : VARIABLES) = struct

  module SV = Set.Make(V)

  let opt_map f k =
    match k with
    | Some x -> Some (f x)
    | None -> None

  let rec marquage f m =
    match f with
    | B b -> M.map (fun _ -> b) m
    | L q ->
       begin
         match q with
         | P q -> M.map (fun (e,_) -> SV.mem q e) m
         | N q ->  M.map (fun (e,_) -> not (SV.mem q e)) m
       end
    | Binop (c, a, b) ->  (* Todo opti ? *)
       let op = getop c in
       let a',b' = marquage a m, marquage b m in
       M.mapi (fun k e -> op e (M.find k b')) a'
    | TempUnop (u, phi) ->
       begin
       match u with
       | AX -> marquage (neg (TempUnop (EX, neg phi))) m
       | EX ->
           let m' = marquage phi m in
           M.map (fun (_,s) -> S.exists (fun e -> M.find e m') s) m
       end
    | TempBinop (b, psi1, psi2) ->
       begin
       match b with
       | AW -> M.map not (marquage (TempBinop(EU, neg psi1, neg (Binop(Or, psi1, psi2)))) m)
       | EW -> marquage (Binop(Or, eg psi1, TempBinop(EU, psi1, psi2))) m
       | EU ->
          let mpsi1 = marquage psi1 m in
          let mpsi2 = marquage psi2 m in
          let todo = ref [] in (* todo est L *)
          let res = (* Map (res,dejavu) *)
            M.mapi
              (fun i _ ->
                if M.find i mpsi2
                then
                  begin
                    todo := i :: !todo;
                    (false,true)
                  end
                else (false,false)) m in
          let rec tantque todo res =
            match todo with
            | [] -> M.map fst res
            | q::xs ->
               let res = M.update q (opt_map (fun (_,y) -> (true,y))) res in
               let (newlist,res) =
                 M.fold (fun q' (_,s) (newlist,res) ->
                     (* C'est un prédécesseur que l'on n'a pas vu *)
                     if S.mem q s && (not (snd (M.find q' res)))
                     then
                       let res = M.update q' (opt_map (fun (x,_) -> (x,true))) res in
                       if M.find q' mpsi1
                       then (q'::newlist,res)
                       else (newlist,res)
                     else (newlist,res)
                   )
                   m (xs,res) in
               tantque newlist res
          in tantque !todo res
       | AU ->
          let mpsi1 = marquage psi1 m in
          let mpsi2 = marquage psi2 m in
          let todo = ref [] in
          let res = (* Map (rex, nb) *)
            M.mapi
              (fun q (_,s) ->
                if M.find q mpsi2
                then
                  todo := q :: !todo;
                (false,S.cardinal s)
              ) m in
          let rec tantque todo res =
            match todo with
            | [] -> M.map fst res
            | q::xs ->
               let res = M.update q (opt_map (fun (_,y) -> (true,y))) res in
               let (newlist,res) =
                 M.fold (fun q' (_,s) (newlist,res) ->
                     (* C'est un prédécesseur que l'on n'a pas vu *)
                     if S.mem q s
                     then
                       let newqnb = ref (true,0) in
                       let res = M.update q' (opt_map (fun (x,y) -> newqnb := (x,y-1); !newqnb)) res in
                       if (snd !newqnb = 0) && (M.find q' mpsi1) && not (fst !newqnb)
                       then (q'::newlist,res)
                       else (newlist,res)
                     else (newlist,res)
                   )
                   m (xs,res) in
               tantque newlist res
          in tantque !todo res
       end

  let check phi g x = M.find x (marquage phi g)

end

module T =
  Make(
      struct
        type t = string
        let compare = compare
      end)
