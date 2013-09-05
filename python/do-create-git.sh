#!/bin/sh
for dir in *
do
    if [ -d $dir ]
	then
	echo "mkdir $dir"
	echo "cd $dir"
	cd $dir
	for d2 in *
	do
	    if [ -d $d2 ]
		then
		echo "mkdir $d2.git"
		echo "cd $d2.git"
		echo "git init --bare"
		echo "cd .."
	    fi
	done
	echo "cd .."
	cd ..
    fi
done
