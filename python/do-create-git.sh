#!/bin/sh
gitignore=/Users/chris/db/mandoku/python/.gitignore
#zb=/Users/Shared/md-remote/text
zb=/tmp/
cd $zb
for dir in ZB[1-4]*
do
    cd $dir
    for d2 in ZB*
    do
	if [ -d $d2 ]
	then
	    echo $d2
	    cd $d2
	    git init
	    cp $gitignore .
	    git add .
	    git commit -am "Initial commit"
	    git branch WYG
	    cd ..
	fi
    done
    cd ..
done
