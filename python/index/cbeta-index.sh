#!/bin/sh
#rm index.log
mandokudir=`dirname $0`
idxdir=~/tmp/index
coll=cbeta
src=~/tmp/md
cd $src
#for subcoll in H J T W X
for subcoll in S U P L M K G F C A H J T W X
#for subcoll in X
do
#    rm -Rf $idxdir/*/*$subcoll.$coll.idx
    cd $subcoll
    rm index.log
    for file in */*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll.$coll
	sleep 1
    done
    cd ..
done