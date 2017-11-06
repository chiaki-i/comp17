#!/bin/sh
cp headerIntel.s $1.s
echo "copied from headerIntel.s"
cat $1.ml | ./compiler >> $1.s
echo "added ./compiler results"
gcc -m64 mainIntel.c $1.s -o $1.x
echo "gcc done"
time ./$1.x
