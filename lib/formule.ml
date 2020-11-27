type rich = Rich

type 'a lit = N of 'a | P of 'a

type _ binop =
  | And : 'a binop
  | Or : 'a binop
  | Xor : rich binop
  | Impl : rich binop
  | Eq : rich binop

type _ tempUnop =
  | AX : 'a tempUnop
  | EX : 'a tempUnop
  | AF : rich tempUnop
  | EF : rich tempUnop
  | AG : rich tempUnop
  | EG : rich tempUnop

type tempBinop = EU | AU | EW | AW

(* GADT, useful for poor_formule *)
type (_, 'a) formule =
  (* Logique propositionnelle *)
  | B : bool -> ('t, 'a) formule
  | L : 'a lit -> ('t, 'a) formule
  | Not : ('t, 'a) formule -> (rich, 'a) formule
  | Binop : 't binop * ('t, 'a) formule * ('t, 'a) formule -> ('t, 'a) formule
  (* Combinateurs temporels *)
  | TempUnop : 't tempUnop * ('t, 'a) formule -> ('t, 'a) formule
  | TempBinop :
      tempBinop * ('t, 'a) formule * ('t, 'a) formule
      -> ('t, 'a) formule

let get_string_temp : type t. t tempUnop -> string = function
  | AX -> "AX"
  | EX -> "EX"
  | AF -> "AF"
  | EF -> "EF"
  | AG -> "AG"
  | EG -> "EG"

let get_string : type t. t binop -> string = function
  | And -> "∧"
  | Or -> "∨"
  | Impl -> "⇒"
  | Xor -> "⊕"
  | Eq -> "⇔"

let par_needed : type t. (t, 'a) formule -> bool = function
  | B _ | L _ -> false
  | _ -> true

let string_of_formule printer f =
  let rec to_string : type t. (t, 'a) formule -> string =
    let pprint f =
      let s = to_string f in
      if par_needed f then "(" ^ s ^ ")" else s
    in
    fun x ->
      match x with
      | B b -> if b then "⊤" else "⊥"
      | L p -> ( match p with P p -> printer p | N p -> "¬" ^ printer p )
      | Not f -> "¬ " ^ pprint f
      | Binop (b, phi, psi) ->
          pprint phi ^ " " ^ get_string b ^ " " ^ pprint psi
      | TempUnop (u, phi) -> get_string_temp u ^ " " ^ pprint phi
      | TempBinop (b, phi, psi) ->
          let a, b =
            match b with
            | EU -> ("E ", " U ")
            | AU -> ("A ", " U ")
            | EW -> ("E ", " W ")
            | AW -> ("A ", " W ")
          in
          a ^ pprint phi ^ b ^ pprint psi
  in
  to_string f

let print_formule printer f = print_endline (string_of_formule printer f)

(* prend un tableau pour limiter le temps de génération... *)
let generate_formulas max_depth number labels =
  let labels = Array.of_list labels in
  Random.self_init ();
  let len = Array.length labels in
  let rec aux depth =
    let depth = depth + 1 in
    let constructors =
      [|
        (fun () -> Binop (And, aux depth, aux depth));
        (fun () -> Binop (Or, aux depth, aux depth));
        (fun () -> Binop (Xor, aux depth, aux depth));
        (fun () -> Binop (Impl, aux depth, aux depth));
        (fun () -> Binop (Eq, aux depth, aux depth));
        (fun () -> TempUnop (AX, aux depth));
        (fun () -> TempUnop (EX, aux depth));
        (fun () -> TempUnop (AF, aux depth));
        (fun () -> TempUnop (EF, aux depth));
        (fun () -> TempUnop (AG, aux depth));
        (fun () -> TempUnop (EG, aux depth));
        (fun () -> TempBinop (EU, aux depth, aux depth));
        (fun () -> TempBinop (AU, aux depth, aux depth));
        (fun () -> TempBinop (EW, aux depth, aux depth));
        (fun () -> TempBinop (AW, aux depth, aux depth));
        (fun () -> Not (aux depth));
      |]
    in
    let size = Array.length constructors in
    let n = if depth >= max_depth then size else Random.int (size + 1) in
    if n = size then
      let v = Random.int (len + 2) in
      if v = 0 then B false else if v = 1 then B true else L (P labels.(v - 2))
    else constructors.(n) ()
  in
  List.init number (fun _ -> aux 0)
