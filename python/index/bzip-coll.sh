#!/bin/sh
#cd ~/db/index
for dir in *
do
    for file in $dir/*/*.idx
    do
	tmp=$(wc -c "$file" )
	FILESIZE=`echo $tmp | cut -d " " -f 1`
	if [ $FILESIZE -gt 500 ]; then
    	    bzip2 --best $file
	    # echo "big"
	fi
	echo $FILESIZE
    done
done
