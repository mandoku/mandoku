#!/bin/sh
privtok=hzj3TXjgzysKdqcVPmSF
remotelog=/tmp/gitlab-rem.log
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
		tit = `head -1 ZB1a0001.org | sed -e "s/#+TITLE: //"`
		curl --connect-timeout 60 --header "PRIVATE-TOKEN: $privtok" -X POST "http://test.kanripo.org/gitlab/api/v3/projects?name=$d2&description=$tit" >> $remotelog
		git remote add origin git@zb.kanripo.org:zb/$df.git
		git push remote 
		cd ..
	    fi
	cd ..
    fi
done
