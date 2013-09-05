#!/bin/sh
zb=/Users/Shared/md-remote/text
for dir in ZB[1-4]*
do
    cd $dir
    for d2 in ZB*
    do
	if [ -d $d2 ]
	then
	    echo $d2
	    cd $d2
	    echo "git init "
	    cd ..
	fi
    done
    cd ..
done
