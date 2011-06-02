#!/bin/sh
rm logfile.log
idxdir=/tmp/index
mkdir $idxdir
coll=$1
rm -Rf $idxdir/*/*.$coll.idx

for file in */*.txt
do
    echo $file
    python ~/projects/mandoku/mandoku_idx.py $file $coll $idxdir
    sleep 1
done
