#!/bin/bash
rm sortie_test
echo "-parse-only sur docs/tests/syntax/bad/*"
for file in docs/tests/syntax/bad/*.cpp
do
./minic++ -parse-only $file
done

echo "\n \n -parse-only sur le reste"
for file in docs/tests/syntax/good/*.cpp docs/tests/typing/*/*.cpp docs/tests/exec/*.cpp
do
./minic++ -parse-only $file
done