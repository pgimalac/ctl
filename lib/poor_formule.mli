type poor = unit

type 't poor_formule = (poor, 't) Formule.formule

val getop : poor Formule.binop -> bool -> bool -> bool
val et :
  ('a, 'b) Formule.formule ->
  ('a, 'b) Formule.formule -> ('a, 'b) Formule.formule
val ou :
  ('a, 'b) Formule.formule ->
  ('a, 'b) Formule.formule -> ('a, 'b) Formule.formule

val neg : 'a poor_formule -> 'a poor_formule
val af : 'a poor_formule -> 'a poor_formule
val eg : 'a poor_formule -> 'a poor_formule
val ef : 'a poor_formule -> 'a poor_formule
val ag : 'a poor_formule -> 'a poor_formule
val to_poor :
  ('t, 'a) Formule.formule -> 'a poor_formule
