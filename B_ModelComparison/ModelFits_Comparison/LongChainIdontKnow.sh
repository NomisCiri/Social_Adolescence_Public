#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyValues=5
HowManyModels=4 
HowManyModelsCross=4
#j=2;
#i=1;
#k=5;
for k in `seq 0 $HowManyValues`;
do
	for i in `seq 1 $HowManyModels`;
	do
		for j in `seq 1 $HowManyModelsCross`;
		do
			echo "$j $i $k";
			#here i specify the job for the cluster.
			#for input_file in INPUT/* ; do
				#echo "#PBS -m n"                         > IDK.pbs
				echo "#PBS -N InverseTemp_OCU_Other_$j$i$k" >> IDK.pbs
				echo "#PBS -o /dev/null"                >> IDK.pbs
				echo "#PBS -l mem=10gb" >> IDK.pbs
				echo "#PBS -j oe"                       >> IDK.pbs
				echo "#PBS -l walltime=100:00:0"          >> IDK.pbs
				echo "#PBS -d ."                        >> IDK.pbs
				echo "#PBS -l nodes=1:ppn=3"            >>IDK.pbs
				echo "Rscript stan_scriptLongChain.R $i $j $k 1 1"	>> IDK.pbs
				qsub IDK.pbs
			done
				#done
				#done
			rm -f IDK.pbs
		done
	done