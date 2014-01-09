#!/bin/sh
if [ $1 == "parse" ]; then
    
    echo "#-##############################"
    echo "--parse-only sur docs/tests/syntax/bad/*"
    for file in docs/tests/syntax/bad/*.cpp
    do
	echo "Devrait produire une erreur :" $file
	./minic++ --parse-only $file
	echo
    done

    echo
    echo "###############################"
    echo " --parse-only sur le reste"
    for file in docs/tests/syntax/good/*.cpp docs/tests/typing/*/*.cpp docs/tests/exec/*.cpp
    do
	echo "Ne devrait pas produire d'erreur :" $file
	./minic++ --parse-only $file
    done
    echo
fi
if [ $1 == "type" ]; then 
    echo
    echo "#-##############################"
    echo "--type-only sur docs/tests/typing/bad/*"
    for file in docs/tests/typing/bad/*.cpp
    do
	echo "Devrait produire une erreur :" $file
	./minic++ --type-only $file
	echo
    done
    
    echo
    echo "###############################"
    echo " --type-only sur le reste"
    for file in docs/tests/typing/good/*.cpp docs/tests/exec/*.cpp
    do
	echo "Ne devrait pas produire d'erreur :" $file
	./minic++ --type-only $file
	echo
    done
fi

if [ $1 == "exec" ]; then
    for file in docs/tests/exec/*.cpp
    do
	if [ $ans == "q" ]; then break ;fi
	echo "Fichier " $file
	echo "################### Sortie attendue ##################"
	g++ $file -o /tmp/caca ; /tmp/caca
	echo "################### Sortie mips ##################"
	./minic++ $file && 
	java -jar /opt/mars/Mars.jar `echo $file | cut -d "." -f -1`".s" | tail -n +3
	
	echo "Entrée pour passer au suivant, 'q' pour quitter."
	read ans
	echo 
    done
fi
	
