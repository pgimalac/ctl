open Automata
open Formule
open Marqueur

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

type game_state =
  int * (T.SV.elt formule, (int * T.SV.elt formule) pbf) either

val gsphi :
  (T.SV.t * S.t) M.t -> game_state -> game_state list
