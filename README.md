# Model-checking de CTL

Ce projet propose deux implémentations distinctes du model-checking de CTL.

Il est organisé de la manière suivante :

* `lib/` les différents modules :
  *  `formule.ml` contient les définitions relatives aux formules de CTL. On y définit en un type les deux types de formules possibles : le premier de ces types est strictement plus riche que le second, utile à la lecture et à l’affichage. Le second définit un type plus restreint, ne comprenant par exemple des négations que sur les propositions atomiques : c’est utile pour le model-checking.
  * `kripke.ml` définit un foncteur représentant une structure de Kripke où les états sont indicés par des entiers. Il prend en paramètre un module `Set` des variables qui étiquettent les états
  * `marqueur.ml` définit un foncteur permettant de faire le model-checking de CTL en utilisant des raisonnements de graphes. Il prend en paramètre un module `Kripke`.
  * `automata.ml` et `fpg.ml` définissent des foncteurs permettant de faire le model-checking de CTL en utilisant des raisonnements sur les automates d’arbres alternants et la théorie des jeux à parité faible. Ils prennent en paramètre un module `Kripke`.
  * `parser.mly` et `reader.mll` permettent de pouvoir lire des structures de Kripke et des formules depuis un fichier texte.

* `bin/` contient le fichier principal exécutant les deux model-checker sur des exemples contenus dans le dossier `graphs/`.

### Compilation

La commande `dune build bin/checker.exe` compile l'ensemble des fichiers du projet dans un dossier `_build`. La commande `dune exec bin/checker.exe` permet d'éxecuter le projet ainsi compilé.

### Rapport

Le fichier `rapport.tex` contient une présentation de la logique `CTL`, une explication de la méthode de marquage avec sa preuve ainsi qu'une explication et une preuve de la méthode utilisant les automates d'arbres et les jeux de parité.
