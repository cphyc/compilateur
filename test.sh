#!/bin/bash
if [ -e sortie_test ]; then rm sortie_test; fi
echo "###############################"
echo "-parse-only sur docs/tests/syntax/bad/*"
for file in docs/tests/syntax/bad/*.cpp
do
    echo "Devrait produire une erreur :" $file
    ./minic++ -parse-only $file
    echo
done

echo
echo "###############################"
echo " -parse-only sur le reste"
for file in docs/tests/syntax/good/*.cpp docs/tests/typing/*/*.cpp docs/tests/exec/*.cpp
do
    echo "Ne devrait pas produire d'erreur :" $file
    ./minic++ -parse-only $file
done
