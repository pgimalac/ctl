open Marqueur

type 'a pbf =
  | P_pbf of 'a
  | Et_pbf of ('a pbf * 'a pbf)
  | Ou_pbf of ('a pbf * 'a pbf)
  | B_pbf of bool

val et : 'a pbf -> 'a pbf -> 'a pbf
val ou : 'a pbf -> 'a pbf -> 'a pbf

val tau :
  int ->
  T.SV.elt Formule.formule * Marqueur.T.SV.t ->
  (int * T.SV.elt Formule.formule) pbf

val poids : 'a Formule.formule -> int

val string_of_pbf : ('a -> string) -> 'a pbf -> string
