Mini C++
===========
State of the art
==========

Le compilateur permet pour le moment de compiler (sans créer le code) :

   int maint () {
       cout << 42 ;
   }


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
