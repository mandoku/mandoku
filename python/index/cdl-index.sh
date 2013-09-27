#!/bin/sh

mandokudir=`dirname $0`
idxdir=/tmp/cdl-index
mkdir -p $idxdir
coll=krp
src=/tmp/cdl
cd $src
for subcoll in ZB6q*
do
    cd $subcoll
    rm index.log
    for file in */*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll.$coll $idxdir
#	sleep 1
    done
    cd ..
done
