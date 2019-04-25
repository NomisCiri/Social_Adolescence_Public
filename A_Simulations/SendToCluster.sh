Groups=3;
HowManySubs=1; #here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyModels=4;
HowManyValues=5;
for z in `seq 1 $Groups`;
do
for k in `seq 0 $HowManyValues`;
do
	for i in `seq 1 $HowManySubs`;
	do
		for j in `seq 1 $HowManyModels`;
		do
			echo "$i $j $k";
			#here i specify the job for the cluster.
			#for input_file in INPUT/* ; do
				echo "#PBS -m n"                          > simulateOCU.pbs
				echo "#PBS -N simulateOCU$i$j"				>> simulateOCU.pbs
				echo "#PBS -j oe"                        >> simulateOCU.pbs
				echo "#PBS -l walltime=0:5:0"            >> simulateOCU.pbs
				echo "#PBS -d ."                         >> simulateOCU.pbs
				echo "Rscript SimulateSubwise.R $i $j $k $z"	>> simulateOCU.pbs
				qsub simulateOCU.pbs
				#done
			done
		done
	done 
done
	rm -f simulateOCU.pbs
