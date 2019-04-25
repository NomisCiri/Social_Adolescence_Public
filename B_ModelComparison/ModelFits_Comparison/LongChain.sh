#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyValues=5
HowManyModels=6 
HowManyModelsCross=5
#j=2;
#i=1;
#k=5;
for k in `seq 0 $HowManyValues`;
do
	for i in `seq 0 $HowManyModels`;
	do
		for j in `seq 1 $HowManyModelsCross`;
		do
			echo "$j $i $k";
			#here i specify the job for the cluster.
			#for input_file in INPUT/* ; do
				#echo "#PBS -m n"                         > LongChain.pbs
				echo "#PBS -N CrossValOCU_Sep_$j$i$k" >> LongChain.pbs
				echo "#PBS -o /logs/"                >> LongChain.pbs
				echo "#PBS -l mem=32gb" >> LongChain.pbs
				echo "#PBS -j oe"                       >> LongChain.pbs
				echo "#PBS -l walltime=100:00:0"          >> LongChain.pbs
				echo "#PBS -d ."                        >> LongChain.pbs
				echo "#PBS -l nodes=1:ppn=4"            >>LongChain.pbs
				echo "module load R/3.5.1; Rscript stan_scriptLongChain.R $i $j $k 1 1"	>> LongChain.pbs
				qsub LongChain.pbs
			done
				#done
				#done
			rm -f LongChain.pbs
		done
	done