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
rm *.txt
saxon $file $xslbin/cb2mandoku.xsl baseedition=$b curwit=master curwitlabel=【CBETA】master> $f.txt
python $pybin/splitmandoku.py $f.txt split >> /dev/null
rm $f.txt
#git init
git add .
git commit -am "new version"
#git remote add origin ssh://dao/Users/chris/gitrep/md-cbeta/$b/$f.git
git push -f origin master
for wit in `saxon $file $xslbin/list-wit.xsl`
do
    branch=${wit#*:}
#    git pull origin $branch
    git branch $branch
    git checkout $branch
    rm *.txt
    echo $wit >> index.log
    saxon $file $xslbin/cb2mandoku.xsl baseedition=$b curwit=${wit%:*} curwitlabel=$branch> $f.txt
    python $pybin/splitmandoku.py $f.txt split >> index.log
    rm $f.txt
    git add .
    git commit -am "created branch $branch"
    git push -f origin $branch
    git checkout master
done
git push -f origin
#git checkout 【CBETA】
cd $cwd
