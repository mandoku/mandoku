#!/bin/sh
mandokudir=`dirname $0`
idxdir=/tmp/index
mkdir -p $idxdir
coll=krp
#src=/Users/chris/krp/text
src=/tmp/text
cd $src
for subcoll in ZB*
do
    cd $subcoll
    rm index.log
    for file in */*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll $idxdir
#	sleep 1
    done
    cd ..
done
