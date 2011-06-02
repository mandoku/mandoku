#for dir in A C F G K L M P S U 
for dir in J
do
    cd $dir
    for d2 in *
    do
	cd $d2
	for file in *.xml
	do
	    echo $file
	    sh ~/work/xsl/cb2md.sh `pwd`/$file
	done
	cd ..
    done
    cd ..
done