#!/bin/sh
# gnu-sed : brew install gnu-sed


# for task4: generating machine language.
cp headerIntel.s $1.s
echo ====== copied from headerIntel.s =====
cat $1.ml | ./compiler >> $1.s
echo ====== added ./compiler results ======
gcc -m64 mainIntel.c $1.s -o $1.x
echo ====== gcc done ======================
time ./$1.x

<< COMMENTOUT
# for task5 & task6: simple optimizations and comparing them each other
# prepare multiple executable files

# update files 
rsync -u *.ml* Makefile ./original/
# rsync -u *.ml* Makefile ./beta/
# rsync -u *.ml* Makefile ./eta/
# rsync -u *.ml* Makefile ./assoc/
rsync -u *.ml* Makefile ./simple/
rsync -u *.ml* Makefile ./elim/
rsync -u *.ml* Makefile ./constf/
rsync -u *.ml* Makefile ./all/

# make
cd ./original
make --quiet # ./compiler
cd ../

cd ./beta
gsed -i -e "5s/(\*\ //" -e "5s/\*)\ //" main.ml >> main.ml
make --quiet # ./compiler-beta
gsed -i -e "5s/let/(\*\ let/" -e "5s/in/in\ \*)/" main.ml >> main.ml 
cd ../

cd ./eta
gsed -i -e "6s/(\*\ //" -e "6s/\*)\ //" main.ml >> main.ml
make --quiet # ./compiler-eta
gsed -i -e "6s/let/(\*\ let/" -e "6s/in/in\ \*)/" main.ml >> main.ml 
cd ../

cd ./assoc
gsed -i -e "7s/(\*\ //" -e "7s/\*)\ //" main.ml >> main.ml
make --quiet # ./compiler-assoc
gsed -i -e "7s/let/(\*\ let/" -e "7s/in/in\ \*)/" main.ml >> main.ml 
cd ../

cd ./elim/
gsed -i -e "8s/(\*\ //" -e "8s/\*)\ //" main.ml >> main.ml
make --quiet # elim/compiler
gsed -i -e "8s/let/(\*\ let/" -e "8s/in/in\ \*)/" main.ml >> main.ml 
cd ../

cd ./constf/
# gsed -i -e "5,7s/(\*\ //" -e "5,7s/\*)\ //" main.ml >> main.ml
gsed -i -e "9s/(\*\ //" -e "9s/\*)\ //" main.ml >> main.ml
make --quiet # constf/compiler
# gsed -i -e "5,7s/let/(\*\ let/" -e "5,7s/in/in\ \*)/" main.ml >> main.ml 
gsed -i -e "9s/let/(\*\ let/" -e "9s/in/in\ \*)/" main.ml >> main.ml 
cd ../

cd ./simple
gsed -i -e "5,7s/(\*\ //" -e "5,7s/\*)\ //" main.ml >> main.ml
make --quiet # simple/compiler
gsed -i -e "5,7s/let/(\*\ let/" -e "5,7s/in/in\ \*)/" main.ml >> main.ml 
cd ../

cd ./all/
gsed -i -e "5,9s/(\*\ //" -e "5,9s/\*)\ //" main.ml >> main.ml
make --quiet # all/compiler
gsed -i -e "5,9s/let/(\*\ let/" -e "5,9s/in/in\ \*)/" main.ml >> main.ml 
cd ../

# print knormal.t and compare
for file in `\find ./test/optim2/constf1.ml -maxdepth 1 -type f`; do
    echo ======== ${file} ========
    echo -------- original file ----------
    cat $file
    echo -------- original result --------
    cat $file | ./original/compiler
    # echo -------- simple optims. ---------
    # cat $file | ./simple/compiler
    # echo -------- dead code elim. --------
    # cat $file | ./elim/compiler
    echo -------- constant folding -------
    cat $file | ./constf/compiler
    echo -------- all optimizations ------
    cat $file | ./all/compiler
done

# generate machine language
for file in `\find ./test/optim2/elim3.ml -maxdepth 1 -type f`; do
    echo "======== ${file} ========"
    name=`echo ${file} | gsed s/.ml//` # remove ".ml" from filename
    echo "-------- original result --------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | original/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    rm ${name}.s
    echo "-------- simple optims. ---------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | simple/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    rm ${name}.s
    echo "-------- simple + elim. ---------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | elim/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    rm ${name}.s
    echo "-------- simple + constf --------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | constf/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    rm ${name}.s
    echo "-------- all optimizations ------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | all/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
done
COMMENTOUT
