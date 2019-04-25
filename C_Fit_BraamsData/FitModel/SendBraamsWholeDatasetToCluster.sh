#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyModels=5
j=99



for i in `seq 1 $HowManyModels`;
	do
			echo "Model $i";
			#here i specify the job for the cluster.
			#for input_file in INPUT/* ; do
				#echo "#PBS -m n"                         > Braams.pbs
				echo "#PBS -N OnePriorBraams_$i" > Braams.pbs
				echo "#PBS -o /dev/null"                >> Braams.pbs
				echo "#PBS -l mem=10gb" 				>> Braams.pbs
				echo "#PBS -j oe"                       >> Braams.pbs
				echo "#PBS -l walltime=100:00:0"        >> Braams.pbs
				echo "#PBS -d ."                        >> Braams.pbs
				echo "#PBS -l nodes=1:ppn=3"            >>Braams.pbs
				echo "Rscript Braams_perYear.R $j $i"	>> Braams.pbs
				qsub Braams.pbs
				rm -f Braams.pbs
done
