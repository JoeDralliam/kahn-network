export TIMEFMT=$'%E'
for w in 1 3 5 10 20 50 100 200
do
    printf "$w workers:\n" ;
    for x in "sequentiel" "pipes" "pipes-thread" "thread" "network" "network-thread"
    do
	printf "$x:" ;
	time timeout 30 ./mapReduce.native -hard -workers $w -silent -network $x;
    done
done