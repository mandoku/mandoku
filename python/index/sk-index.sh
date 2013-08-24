#!/bin/sh

mandokudir=`dirname $0`
idxdir=/tmp/index
coll=krp
src=/tmp/skqs
cd $src
for subcoll in ZB*
do
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
