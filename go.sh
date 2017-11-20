#!/bin/sh
# gnu-sed : brew install gnu-sed

<< COMMENTOUT
# for task4: generating machine language.
cp headerIntel.s $1.s
# echo "copied from headerIntel.s"
cat $1.ml | ./compiler >> $1.s
# echo "added ./compiler results"
gcc -m64 mainIntel.c $1.s -o $1.x
# echo "gcc done"
time ./$1.x
COMMENTOUT

# for task5: simple optimizations and comparing them each other
# prepare multiple executable files

# update files 
rsync -u *.ml* Makefile ./original/
rsync -u *.ml* Makefile ./beta/
rsync -u *.ml* Makefile ./eta/
rsync -u *.ml* Makefile ./assoc/
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

cd ./all
gsed -i -e "5,7s/(\*\ //" -e "5,7s/\*)\ //" main.ml >> main.ml
make --quiet # ./compiler-all
gsed -i -e "5,7s/let/(\*\ let/" -e "5,7s/in/in\ \*)/" main.ml >> main.ml 
cd ../

<< COMMENTOUT
# print knormal.t and compare
for file in `\find ./test/optim/eta1.ml -maxdepth 1 -type f`; do
    echo "======== ${file} ========"
    echo "-------- original result --------"
    cat $file | ./original/compiler 
    echo "-------- beta conversion --------"
    cat $file | ./beta/compiler
    echo "-------- eta conversion ---------"
    cat $file | ./eta/compiler
    echo "-------- let assoc conv. --------"
    cat $file | ./assoc/compiler
    echo "-------- all optimizations ------"
    cat $file | ./all/compiler
done
COMMENTOUT

# generate machine language
for file in `\find ./test/optim/gcd.ml -maxdepth 1 -type f`; do
    echo "======== ${file} ========"
    name=`echo ${file} | gsed s/.ml//` # remove ".ml" from filename
    echo "-------- original result --------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | original/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    echo "-------- beta conversion --------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | beta/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    echo "-------- eta conversion ---------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | eta/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    echo  "-------- let assoc conv. --------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | assoc/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
    echo "-------- all optimizations ------"
    touch ${name}.s
    cp headerIntel.s ${name}.s
    cat ${name}.ml | all/compiler >> ${name}.s
    gcc -m64 mainIntel2.c ${name}.s -o ${name}.x
    time ${name}.x
done
