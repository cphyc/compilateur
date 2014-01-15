#!/bin/bash

shopt -s nullglob

# script de test pour le projet de compilation

option=$1
compilo=$2
score=0
max=0
verbose=0


echo "Test de $2"

echo

# tous les tests passent avec g++
test_gcc() {
for f in syntax/bad/*.cpp; do
    if g++ -c $f > /dev/null 2>&1 ; then
      echo "succès de g++ -c sur $f"; exit 1
    fi
done

for f in typing/good/*.cpp exec/*.cpp exec-fail/*.cpp; do
    g++ -c $f > /dev/null 2>&1 ||
     (echo "echec de g++ sur $f"; exit 1)
done
for f in typing/bad/*.cpp; do
    if g++ -c $f > /dev/null 2>&1 ; then
      echo "succès de g++ -c sur $f"; exit 1
    fi
done
for f in exec/*.cpp; do
    if test $f == exec/sizeof1.cpp; then continue; fi
    if test $f == exec/sizeof2.cpp; then continue; fi
    echo "test g++ sur $f"
    expected=exec/`basename $f .cpp`.out
    if g++ $f > /dev/null 2>&1 ; then
      ./a.out > out
      if ! cmp --quiet out $expected; then
          echo "mauvaise sortie de g++ sur $f"; exit 1
      fi
    else
      echo "échec de g++ -c sur $f"; exit 1
    fi
done
}

compile () {
if [[ $verbose != 0 ]]; then
  echo Compile $1 $2
  $compilo $1 $2;
else
  $compilo $1 $2 > /dev/null 2>&1;
fi;
}


# partie 1 : tests d'analyse syntaxique

partie1 () {

score=0
max=0

echo "Partie 1"

# les mauvais
echo -n "mauvais "
for f in syntax/bad/*.cpp; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait échouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in syntax/good/*.cpp typing/bad/*.cpp typing/good/*.cpp exec/*.cpp exec-fail/*.cpp; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Syntaxe : $score/$max : $percent%"; }

# partie 2 : tests d'analyse sémantique


partie2 () {
echo
echo "Partie 2"

score=0
max=0

# les mauvais
echo -n "mauvais "
for f in typing/bad/*.cpp; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"0")
	echo
	echo "ECHEC sur "$f" (devrait échouer)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

# les bons
echo -n "bons "
for f in typing/good/*.cpp exec/*.cpp exec-fail/*.cpp; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
    case $? in
	"1")
	echo
	echo "ECHEC sur "$f" (devrait reussir)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
	echo "ECHEC sur "$f" (pour une mauvaise raison)";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo    "Typage  : $score/$max : $percent%";
}


# partie 3 : tests d'exécution

partie3 () {

score_comp=0
score_out=0
score_test=0
max=0

echo
echo "Partie 3"
echo "Execution normale"
echo "-----------------"

timeout=""
spim="spim -ldata 20000000 -lstack 20000000"
mars="java -jar Mars_4_4.jar"

for f in exec/*.cpp; do
    echo -n "."
    mips=exec/`basename $f .cpp`.s
    rm -f $mips
    expected=exec/`basename $f .cpp`.out
    max=`expr $max + 1`;
    if compile $f; then
	rm -f out
	score_comp=`expr $score_comp + 1`;
	# if $timeout $spim -file $mips | grep -v SPIM | grep -v Copyright | grep -v Reserved | grep -v README | grep -v Loaded > out; then
        if $timeout $mars $mips | tail -n +3 | tail -r | tail -n +2 | tail -r > out; then
	    score_out=`expr $score_out + 1`;
	    if cmp --quiet out $expected; then
		score_test=`expr $score_test + 1`;
	    else
		echo
		echo "ECHEC : mauvaise sortie pour $f"
	    fi
	else
		echo
		echo "ECHEC du code produit pour $f"
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait réussir)"
    fi
done
echo

echo "Execution conduisant à un échec"
echo "-------------------------------"

for f in exec-fail/*.cpp; do
    echo -n "."
    mips=exec-fail/`basename $f .cpp`.s
    rm -f $mips
    max=`expr $max + 1`;
    if compile $f; then
	score_comp=`expr $score_comp + 1`;
	if $timeout $spim -file $mips > out; then
	    if grep -q TypeError out; then
		score_test=`expr $score_test + 1`;
	        score_out=`expr $score_out + 1`;
	    else
		echo
		echo "ECHEC : le code $f devrait échouer"
	    fi
	else
		score_test=`expr $score_test + 1`;
	        score_out=`expr $score_out + 1`;
	fi
    else
	echo
	echo "ECHEC de la compilation sur $f (devrait réussir)"
    fi
done

echo
percent=`expr 100 \* $score / $max`;

echo "Compilation:";
percent=`expr 100 \* $score_comp / $max`;
echo "Compilation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Code produit : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Comportement du code : $score_test/$max : $percent%";}


case $option in
    "-1" )
        partie1;;
    "-2" )
        partie2;;
    "-3" )
        partie3;;
    "-v1" )
	verbose=1;
	partie1;;
    "-v2" )
    	verbose=1;
        partie2;;
    "-v3" )
    	verbose=1;
        partie3;;
    "-all" )
    	partie1;
    	partie2;
    	partie3;;
    "-g++" )
        test_gcc;;
    * )
        echo "usage : $0 <option> <compilo>"
        echo "spécifier une option parmi : "
        echo "-1      : tester la partie 1"
        echo "-2      : tester la partie 2"
        echo "-3      : tester la partie 3"
        echo "-v1     : tester la partie 1 (verbose)"
        echo "-v2     : tester la partie 2 (verbose)"
        echo "-v3     : tester la partie 3 (verbose)"
        echo "-all    : tout tester";;

esac
echo