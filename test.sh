for ((i=1;i<=$1;i++)); do 
	echo -n test: $i   generating input data...
	scala.bat EventSender $2 $3 $4 > testdata/$5i$i; 
	echo -n "   performing event matching..."
	./eventSort < testdata/$5i$i > testdata/$5s$i;
	echo "   check clock algo..."
	scala.bat EventChecker --rec-clock-algo < testdata/$5s$i > testdata/$5c$i 2> testdata/$5e$i;
done;
echo 
echo finished
echo
ls -s testdata/$5e*