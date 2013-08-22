#!/bin/sh

# ZB6a 阿含部類
# ZB6b 本緣部類
# ZB6c 般若部類
# ZB6d 法華部類
# ZB6e 華嚴部類
# ZB6f 寶積部類
# ZB6g 涅槃部類
# ZB6h 大集部類
# ZB6i 經集部類
# ZB6j 密教部類
# ZB6k 律部類
# ZB6l 毘曇部類
# ZB6m 中觀部類
# ZB6n 瑜伽部類
# ZB6o 論集部類
# ZB6p 淨土宗部類
# ZB6q 禪宗部類
# ZB6r 史傳部類
# ZB6s 事彙部類
# ZB6t 敦煌寫本部類
# ZB6u 新編部類


#rm index.log

mandokudir=`dirname $0`
idxdir=~/tmp/index
coll=cbeta
src=~/tmp/md
cd $src
#for subcoll in H J T W X
for subcoll in S U P L M K G F C A H J T W X
#for subcoll in X
do
#    rm -Rf $idxdir/*/*$subcoll.$coll.idx
    cd $subcoll
    rm index.log
    for file in */*.txt
    do
	echo $file $subcoll.$coll
	python $mandokudir/mandoku_idx.py $file $subcoll.$coll
	sleep 1
    done
    cd ..
done
