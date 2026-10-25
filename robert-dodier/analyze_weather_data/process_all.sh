script=$1
for f in data/4*.csv
do
    echo ------ $script: process $f
    g=`echo $f| sed 's+data/++;s+\.csv$++'`
    INPUT=$f maxima --batch $script > "$script"-output-$g
done
