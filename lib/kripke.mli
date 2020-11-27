module L : sig
  type t = int

  val compare : 'a -> 'a -> int
end

module S : sig
  type elt = L.t

  type t = Set.Make(L).t

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
end

module M : sig
  type key = L.t

  type 'a t = 'a Map.Make(L).t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding : 'a t -> key * 'a

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding : 'a t -> key * 'a

  val max_binding_opt : 'a t -> (key * 'a) option

  val choose : 'a t -> key * 'a

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a

  val find_opt : key -> 'a t -> 'a option

  val find_first : (key -> bool) -> 'a t -> key * 'a

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val find_last : (key -> bool) -> 'a t -> key * 'a

  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
end

module type K = sig
  module SV : sig
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
  end

  type kripke = (SV.t * S.t) M.t

  val deg : int -> kripke -> int

  val get_labels : kripke -> SV.elt list

  val etiquettes : int -> kripke -> SV.t

  val succ : int -> kripke -> S.t
end

module type VARIABLES = sig
  type t

  val compare : t -> t -> int
end

module Make : functor (V : Set.S) -> sig
  module SV : sig
    type elt = V.elt

    type t = V.t

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
  end

  type kripke = (SV.t * S.t) M.t

  val deg : int -> kripke -> int

  val get_labels : kripke -> SV.elt list

  val etiquettes : int -> kripke -> SV.t

  val succ : int -> kripke -> S.t
end

module SV : sig
  type elt = string

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
end

module KripkeS : sig
  module SV : sig
    type elt = SV.elt

    type t = SV.t

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
  end

  type kripke = (SV.t * S.t) M.t

  val deg : int -> kripke -> int

  val get_labels : kripke -> SV.elt list

  val etiquettes : int -> kripke -> SV.t

  val succ : int -> kripke -> S.t
end
