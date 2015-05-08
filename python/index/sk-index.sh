#!/bin/sh
mandokudir=`dirname $0`
idxdir=/tmp/index
mkdir -p $idxdir
coll=krp
#src=/Users/chris/krp/text
src=/Users/chris/00scratch/KR
cd $src
for subcoll in KR*
do
    cd $subcoll
    rm index.log
    for file in KR*
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll $idxdir
#	sleep 1
    done
    cd ..
done
