Mini C++
===========

Dépendances
===========

OCaml 4.01.0
Menhir

Installation
===========

    cd compilateur
    make

Pour supprimmer l'ensemble des fichiers auxiliaires :

    make clean

Exécution et exemples
===========

Pour compiler un fichier exemple.cpp :

    ./minic++ exemple.cpp

Pour n'exécuter que l'analyse syntaxique :

    ./minic++ --parse-only exemple.cpp

Un script permettant d'effectuer les tests donnés par le sujet est inclus :

    ./test.sh
