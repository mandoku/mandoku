#!/bin/sh
#rm index.log
idxdir=/tmp/index
coll=cbeta
rm -Rf $idxdir/*/*.$coll.idx
for subcoll in H J T W X
do
    cd $subcoll
    rm index.log
    for file in */*.txt
    do
	echo $file $subcoll.$coll
	python ~/projects/mandoku/mandoku_idx.py $file $subcoll.$coll
	sleep 1
    done
    cd ..
done