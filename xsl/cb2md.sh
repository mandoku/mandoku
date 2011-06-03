#!/bin/sh
wd=`dirname $0`
xslbin=$wd
pybin=`dirname $wd`/python/index
file=$1
target="/Users/chris/tmp/md"
mkdir $target
f=`basename $file .xml`
b=${f:0:1}
mkdir $target/$b
cd $target/$b
git clone ssh://dao/Users/chris/gitrep/md-cbeta/$b/$f.git
#mkdir $target/$b/$f
cwd=`pwd`
cd $target/$b/$f
git pull origin
saxon $file $xslbin/cb2mandoku.xsl baseedition=$b curwit=master curwitlabel=【CBETA】master> $f.txt
python $pybin/splitmandoku.py $f.txt split >> /dev/null
rm $f.txt
#git init
git add .
git commit -am "initial commit"
git remote add origin ssh://dao/Users/chris/gitrep/md-cbeta/$b/$f.git
git push origin
for wit in `saxon $file $xslbin/list-wit.xsl`
do
    branch=${wit#*:}
    git branch $branch
    git checkout $branch
    echo $wit >> index.log
    saxon $file $xslbin/cb2mandoku.xsl baseedition=$b curwit=${wit%:*} curwitlabel=$branch> $f.txt
    python $pybin/splitmandoku.py $f.txt split >> index.log
    rm $f.txt
    git add .
    git commit -am "created branch $branch"
    git checkout master
done
git push origin
#git checkout 【CBETA】
cd $cwd
