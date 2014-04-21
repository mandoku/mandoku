#!/bin/sh
mandokudir=`dirname $0`
idxdir=/Users/Shared/md-remote/index
mkdir -p $idxdir
coll=krp
src=/Users/Shared/md-remote/text/
cd $src
for subcoll in ZB5a ZB5b ZB5c ZB5d ZB5e ZB5f ZB5g ZB5h
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
