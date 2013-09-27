#!/bin/sh
#cd ~/db/index
for file in */*.idx
do
    tmp=$(wc -c "$file" )
    FILESIZE=`echo $tmp | cut -d " " -f 1`
    if [ $FILESIZE -gt 500 ]; then
    	bzip2 --best $file
    fi
    echo $FILESIZE
done