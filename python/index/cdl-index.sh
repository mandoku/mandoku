#!/bin/sh
mandokudir=`dirname $0`
idxdir=$mandokudir/index
mkdir -p $idxdir
coll=krp
src=~/md/text/ZB6q/
cd $src
for subcoll in ZB6q
do
    cd $subcoll
    rm index.log
    for file in ZB6q0002/*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll.$coll $idxdir
#	sleep 1
    done
    cd ..
done
