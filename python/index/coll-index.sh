#!/bin/sh
rm logfile.log
idxdir=/tmp/index
mkdir $idxdir
coll=$1
rm -Rf $idxdir/*/*.$coll.idx

for file in */*.txt
do
    echo $file
    python /Users/chris/db/mandoku/python/index/mandoku_idx.py $file $coll $idxdir
    sleep 0.05
done
