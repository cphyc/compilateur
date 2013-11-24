#!/bin/bash
if [ -e sortie_test ]; then rm sortie_test; fi
echo "###############################"
echo "-parse-only sur docs/tests/syntax/bad/*"
for file in docs/tests/syntax/bad/*.cpp
do
./minic++ -parse-only $file
done

echo
echo "###############################"
echo " -parse-only sur le reste"
for file in docs/tests/syntax/good/*.cpp 
do
./minic++ -parse-only $file
done
