# Model-checking de CTL

Ce projet propose deux implémentations distinctes du model-checking de CTL.

Il est organisé de la manière suivante :

* `lib/` les différents modules :
  * `pretty_formule.ml` et` formule.ml` contiennent les définitions relatives aux formules de CTL. La première définit un type strictement plus riche que le second, utile à la lecture et à l’affichage. Le second définit un type plus restreint, ne comprenant par exemple des négations que sur les propositions atomiques : c’est utile pour le model-checking.
  * `kripke.ml` définit un foncteur représentant une structure de Kripke où les états sont indicés par des entiers. Il prend en paramètre un module `Set` des variables qui étiquettent les états
  * `marqueur.ml` définit un foncteur permettant de faire le model-checking de CTL en utilisant des raisonnements de graphes. Il prend en paramètre un module `Kripke`.
  * `automata.ml` et `fpg.ml` définissent des foncteurs permettant de faire le model-checking de CTL en utilisant des raisonnements sur les automates d’arbres alternants et la théorie des jeux à parité faible. Ils prennent en paramètre un module `Kripke`.
  * `parser.mly` et `reader.mll` permettent de pouvoir lire des structures de Kripke et des formules depuis un fichier texte.

* `bin/` contient le fichier principal exécutant les deux model-checker sur des exemples contenus dans le dossier `graphs/`.
