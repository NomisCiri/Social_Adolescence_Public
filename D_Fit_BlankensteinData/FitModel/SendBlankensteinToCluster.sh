#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyModels=5
AgeGroups=4
#j=2;
#i=1;
#k=5;


for i in `seq 1 $HowManyModels`;
	do
		for j in `seq 1 $AgeGroups`;
		do
			echo "$j $i $k $Subjects";
			#here i specify the job for the cluster.
			#for input_file in INPUT/* ; do
				#echo "#PBS -m n"                         > Blanky.pbs
				echo "#PBS -N RiskAmb_OCU_Other_$j\_$i\_$k" > Blanky.pbs
				echo "#PBS -o /dev/null"                >> Blanky.pbs
				echo "#PBS -l mem=10gb" 				>> Blanky.pbs
				echo "#PBS -j oe"                       >> Blanky.pbs
				echo "#PBS -l walltime=100:00:0"        >> Blanky.pbs
				echo "#PBS -d ."                        >> Blanky.pbs
				echo "#PBS -l nodes=1:ppn=3"            >>Blanky.pbs
				echo "Rscript stan_script_Blankenstein.R $j $i"	>> Blanky.pbs
				qsub Blanky.pbs
			done

			#rm -f Blanky.pbs
done
