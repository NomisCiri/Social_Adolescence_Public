#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyValues=5
HowManyModels=5 
HowManyModelsCross=5
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
				#echo "#PBS -m n"                         > LongChain.pbs
				echo "#PBS -N LongTrembl_OCU_Other_$j$i$k" >> LongChain.pbs
				echo "#PBS -o /dev/null"                >> LongChain.pbs
				echo "#PBS -l mem=10gb" >> LongChain.pbs
				echo "#PBS -j oe"                       >> LongChain.pbs
				echo "#PBS -l walltime=100:00:0"          >> LongChain.pbs
				echo "#PBS -d ."                        >> LongChain.pbs
				echo "#PBS -l nodes=1:ppn=3"            >>LongChain.pbs
				echo "Rscript FitOCUModels.R $i $j $k 1 1"	>> LongChain.pbs
				qsub LongChain.pbs
			done
				#done
				#done
			rm -f LongChain.pbs
		done
	done