#!/bin/sh
#rm index.log
mandokudir=~/db
idxdir=/tmp/index
coll=cbeta
rm -Rf $idxdir/*/*.$coll.idx
#for subcoll in H J T W X
for subcoll in S U P L M K G F C A 
do
    cd $subcoll
    rm index.log
    for file in */*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku/python/index/mandoku_idx.py $file $subcoll.$coll
	sleep 1
    done
    cd ..
done