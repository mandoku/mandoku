wd=`dirname $0`
for dir in A C F G K L M P S U H I J T X
do
    cd $dir
    for d2 in *
    do
	cd $d2
	for file in *.xml
	do
	    echo $file
	    sh $wd/cb2md.sh `pwd`/$file
	done
	cd ..
    done
    cd ..
done