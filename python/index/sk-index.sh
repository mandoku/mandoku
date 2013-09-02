#!/bin/sh

mandokudir=`dirname $0`
idxdir=/tmp/index
mkdir -p $idxdir
coll=krp
src=/Users/chris/db/text/krp
cd $src
for subcoll in ZB3*
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
