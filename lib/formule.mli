type rich = Rich
type poor = Poor
type _ binop =
    And : 'a binop
  | Or : 'a binop
  | Xor : rich binop
  | Impl : rich binop
  | Eq : rich binop
type _ tempUnop =
    AX : 'a tempUnop
  | EX : 'a tempUnop
  | AF : rich tempUnop
  | EF : rich tempUnop
  | AG : rich tempUnop
  | EG : rich tempUnop
type tempBinop = EU | AU | EW | AW
type 'a lit = N of 'a | P of 'a
type (_, 'a) formule =
    B : bool -> ('t, 'a) formule
  | L : 'a lit -> ('t, 'a) formule
  | Not : ('t, 'a) formule -> (rich, 'a) formule
  | Binop : 't binop * ('t, 'a) formule *
      ('t, 'a) formule -> ('t, 'a) formule
  | TempUnop : 't tempUnop * ('t, 'a) formule -> ('t, 'a) formule
  | TempBinop : tempBinop * ('t, 'a) formule *
      ('t, 'a) formule -> ('t, 'a) formule
val get_string_temp : 'a tempUnop -> string
val get_string : 'a binop -> string
val string_of_formule : ('a -> string) -> ('b, 'a) formule -> string
val print_formule : ('a -> string) -> ('b, 'a) formule -> unit
val generate_formulas : int -> 'a array -> (rich, 'a) formule list
