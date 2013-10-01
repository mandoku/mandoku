#!/bin/sh
privtok=hzj3TXjgzysKdqcVPmSF

for d1 in ZB*
do
    if [ -d $d1]
	then
	cd $d1 
	for d2 in ZB*
	do
	    if [ -d $d2]
		then
		cd $d2
		
		cd ..
	    fi
	cd ..
    fi
done
