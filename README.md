Mini C++
===========
State of the art
==========

Le compilateur permet pour le moment de compiler plein de choses comme :

#include <iostream>
int main () {
  int x = 42
  std::cout << x ;
}

TODO : utiliser correctement \n et \t


Dépendances
===========

OCaml 4.01.0 et Menhir

Installation
===========

    cd compilateur
    make

Pour supprimer l'ensemble des fichiers auxiliaires :

    make clean

Exécution et exemples
===========

Pour compiler un fichier exemple.cpp :

    ./minic++ exemple.cpp

Pour n'exécuter que l'analyse syntaxique :

    ./minic++ --parse-only exemple.cpp

Un script permettant d'effectuer les tests donnés par le sujet est inclus :

    ./test.sh
