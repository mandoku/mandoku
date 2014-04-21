#!/bin/sh
mandokudir=/Users/chris/db/mandoku/python/index
idxdir=/tmp/zb5-index
mkdir -p $idxdir
coll=krp
src=/Users/Shared/md-remote/text/
cd $src
for subcoll in ZB5a ZB5b ZB5c ZB5d ZB5e ZB5f ZB5g ZB5h
#for subcoll in ZB5a
do
    cd $subcoll
    rm index.log
    for file in ZB5[a-z0-9]*/*.txt
#    for file in ZB5a0001/*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll.$coll $idxdir
#	sleep 1
    done
    cd ..
done
