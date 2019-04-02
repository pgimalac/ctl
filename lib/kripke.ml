module L = struct
  type t = int
  let compare = compare
end

module S = Set.Make(L)
module M = Map.Make(L)

module type K =
  sig
    module SV :
    sig
      type elt
      type t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : (elt -> unit) -> t -> unit
      val map : (elt -> elt) -> t -> t
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt : t -> elt option
      val max_elt : t -> elt
      val max_elt_opt : t -> elt option
      val choose : t -> elt
      val choose_opt : t -> elt option
      val split : elt -> t -> t * bool * t
      val find : elt -> t -> elt
      val find_opt : elt -> t -> elt option
      val find_first : (elt -> bool) -> t -> elt
      val find_first_opt : (elt -> bool) -> t -> elt option
      val find_last : (elt -> bool) -> t -> elt
      val find_last_opt : (elt -> bool) -> t -> elt option
      val of_list : elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
    end

    type kripke = (SV.t * S.t) M.t
    val deg : int -> kripke -> int

    val etiquettes : int -> kripke -> SV.t
    val succ : int -> kripke -> S.t
  end

module type VARIABLES = sig
  type t
  val compare : t -> t -> int
end

module Make (V : Set.S) = struct

  module SV = V
  type kripke = (SV.t * S.t) M.t

  let deg s (m : kripke) =
    S.cardinal (snd (M.find s m))

  let etiquettes s m = fst (M.find s m)
  let succ s m = snd (M.find s m)
end

module SV =
  Set.Make
    ( struct
        type t = string
        let compare = compare
      end)

module KripkeS = Make(SV)
