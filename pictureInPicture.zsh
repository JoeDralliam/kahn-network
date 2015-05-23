export TIMEFMT=$'%E'
for w in 1 2 4 8 16 32
do
    printf "$w level of recursivity:\n" ;
    for x in "sequentiel" "pipes" "pipes-thread" "thread" "network" "network-thread"
    do
	printf "$x:" ;
	time timeout 30 ./pictureInPicture.native -recursivity $w -silent -network $x;
    done
done