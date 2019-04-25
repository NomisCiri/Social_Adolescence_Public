## this script is made so it can run many simulations from the command line.
bashInput <- commandArgs(trailingOnly = TRUE)
#for debugging
#bashInput<-c(1,2)
#setwd(here())
load('simulate.RData')
#What happens if i just duplicate it.... which is a more realistic scenario in our experiment anyway....
simulate$RiskSafeAdv<-1#risky advisor
simulateRev<-simulate
simulateRev$OtherChoseRisk<-as.numeric(!simulateRev$OtherChoseRisk)
simulateRev$RiskSafeAdv<-2#safe advisor
simulate<-rbind(simulate,simulateRev)
simulate<-rbind(simulate,simulate)
#parameter<-expand.grid(ocu=seq(-50,50,0.5),alpha=seq(1,1),theta=seq(1,1))
#parameter<-expand.grid(ocu=seq(0.1,5,1),alpha=seq(0.1,2,1),theta=seq(0.1,10,1))
#subset with the bash command.
rhomu=0;
switch(bashInput[4],
       "1"={rhomu=0.4},
       "2"={rhomu=1},
       "3"={rhomu=1.6}
       #"4"={rhomu=1.5}
)
OCUParams<-seq(0,5,length.out = 12)
G1<-OCUParams[1:(length(OCUParams)/2)]#FirstHalf
G2<-OCUParams[(length(OCUParams)/2):length(OCUParams)]#SecondHalf

parameterG1<-data.frame(ocuSafe=rnorm(50,abs(G1[(1+as.numeric(bashInput[3]))]),0.1),alpha=abs(rnorm(50,rhomu,0.3)),theta=rnorm(50,0.8,0.1))
parameterG2<-data.frame(ocuSafe=rnorm(50,abs(G2[(1+as.numeric(bashInput[3]))]),0.1),alpha=abs(rnorm(50,rhomu,0.3)),theta=rnorm(50,0.8,0.1))  

howManySubs<-length(unique(simulate$subject))
subarray<-unique(simulate$subject)#for subsetting.

if (bashInput[2]==1){
  
  #########################################################
  #########################################################
  ######### Thats the "Risk Seeking" OCU Model.############
  #########################################################
  #########################################################
  #
  #this wierd model just adds some value to the risky option irrespective of the quality of the information.
  #parameter<-expand.grid(ocu=seq(-10,10,0.5),alpha=seq(1,1),theta=seq(0.3,10))
  MakeOCUDataRisk <- function(v) {
    ocu<-v[1]
    alpha<-v[2]
    theta<-v[3]
    ##Utilities for each choice
    for (i in 1:nrow(simulateSubset)){
      #get the social choices.
      if (is.na(simulateSubset$OtherChoseRisk[i])){ # NoAdvice.
        simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
        simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)) #risk
      }else
        if (simulateSubset$OtherChoseRisk[i]==1){ # advice is risky
          simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
          simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha))+ocu #risk
        }
      else 
        if (simulateSubset$OtherChoseRisk[i]==0){# advice is safe
          simulateSubset$Util_Sure[i] <- (simulateSubset$valueSure[i]^alpha)
          simulateSubset$Util_Risk[i] <- (simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)+ocu
        }
      simulateSubset$ProbChooseRisk[i] <- 1/(1 + exp(-(( simulateSubset$Util_Risk[i] - simulateSubset$Util_Sure[i])*(theta)))) # no it doesn
      simulateSubset$ChooseRisk[i]<-rbinom(1,1,simulateSubset$ProbChooseRisk[i])
    }# choice rule Here OCU might make sense, too? 
    #save the parameters.
    simulateSubset$alpha<-alpha
    simulateSubset$theta<-theta
    simulateSubset$ocu<-ocu
    return(simulateSubset)
  }
  #simulate<-simulate[simulate$subject==subarray[bashInput[1]],]
  #bashInput<-1
  simulateSubset<-simulate[simulate$subject==subarray[as.numeric(bashInput[1])],]
  dummytomakelists<-MakeOCUDataRisk(c(parameterG1[1,1],parameterG1[1,2],parameterG1[1,3]))
  Simulations1=as.list(dummytomakelists)
  Simulations2=as.list(dummytomakelists)
  
  #make data of the FIRST group.
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataRisk(c(parameterG1[i,1],parameterG1[i,2],parameterG1[i,3]))
    Simulations1[[i]]<-as.list(dummytomakelists)
    Simulations1[[i]]$group<-1# i make a new entry here and tell the simulations flag that this is a new group.
  }#end first
  #make data of the SECOND group:
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataRisk(c(parameterG2[i,1],parameterG2[i,2],parameterG2[i,3]))
    Simulations2[[i]]<-as.list(dummytomakelists)
    Simulations2[[i]]$group<-2# i make a new entry here and tell the simulations flag that this is a new group.
  }
  #combine the lists
  Simulations<-do.call(c, list(Simulations1, Simulations2))
  #rnorm(n=as.numeric(bashInput[1]), mean=as.numeric(bashInput[2]))
  save(Simulations,file=paste0("ModelRisk/",as.character(bashInput[4]),"RiskSocial",as.character(bashInput[3]),as.character(bashInput[1]),".RData"))
  
}else if (bashInput[2]==2){
  MakeOCUDataInfo <- function(v) {
    
    ########################################################
    ########################################################
    ######### Thats the a "common Currency" OCU Model.######
    ########################################################
    ########################################################
    
    ocu<-v[1]
    alpha<-v[2]
    theta<-v[3]
    ##Utilities for each choice
    for (i in 1:nrow(simulateSubset)){
      #get the social choices.
      if (is.na(simulateSubset$OtherChoseRisk[i])){ # NoAdvice.
        simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
        simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)) #risk
      }else
        if (simulateSubset$OtherChoseRisk[i]==1){ # advice is risky
          simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
          simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha))+ocu #risk
        }
      else 
        if (simulateSubset$OtherChoseRisk[i]==0){# advice is safe
          simulateSubset$Util_Sure[i] <- (simulateSubset$valueSure[i]^alpha)+ocu
          simulateSubset$Util_Risk[i] <- (simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)
        }
      simulateSubset$ProbChooseRisk[i] <- 1/(1 + exp(-((simulateSubset$Util_Risk[i] - simulateSubset$Util_Sure[i])*(theta)))) # no it doesn
      simulateSubset$ChooseRisk[i]<-rbinom(1,1,simulateSubset$ProbChooseRisk[i])
    }# choice rule Here OCU might make sense, too? 
    #save the parameters.
    simulateSubset$alpha<-alpha
    simulateSubset$theta<-theta
    simulateSubset$ocu<-ocu
    return(simulateSubset)
  }
  # bashInput<-1
  simulateSubset<-simulate[simulate$subject==subarray[as.numeric(bashInput[1])],]
  dummytomakelists<-MakeOCUDataInfo(c(parameterG1[1,1],parameterG1[1,2],parameterG1[1,3]))
  Simulations1=as.list(dummytomakelists)
  Simulations2=as.list(dummytomakelists)
  #make data of the FIRST group.
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataInfo(c(parameterG1[i,1],parameterG1[i,2],parameterG1[i,3]))
    Simulations1[[i]]<-as.list(dummytomakelists)
    Simulations1[[i]]$group<-1# i make a new entry here and tell the simulations flag that this is a new group.
  }#end first
  #make data of the SECOND group:
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataInfo(c(parameterG2[i,1],parameterG2[i,2],parameterG2[i,3]))
    Simulations2[[i]]<-as.list(dummytomakelists)
    Simulations2[[i]]$group<-2# i make a new entry here and tell the simulations flag that this is a new group.
  }
  #combine the lists
  Simulations<-do.call(c, list(Simulations1, Simulations2))
  save(Simulations,file=paste0("ModelInfo/",as.character(bashInput[4]),"InfoSocial",as.character(bashInput[3]),as.character(bashInput[1]),".RData"))
}else if (bashInput[2]==3){
  
  # Here i make the values for the Noise OCU Model. I make a list of 12 values that lie between 0 (no noise) and 1 (complete Randomness) and use these values
  # for my groups. 
  NoiseParams<-seq(0, 0.8, length.out=12)# Prevent the Values from becoming 0 with abs
  G1<-NoiseParams[1:(length(NoiseParams)/2)]#FirstHalf
  G2<-NoiseParams[(length(NoiseParams)/2):length(NoiseParams)]#SecondHalf
  
  parameterG1<-data.frame(ocuSafe=abs(rnorm(50,G1[(1+as.numeric(bashInput[3]))],0.1)),alpha=abs(rnorm(50,rhomu,0.3)),theta=rnorm(50,0.8,0.1))
  parameterG2<-data.frame(ocuSafe=abs(rnorm(50,G2[(1+as.numeric(bashInput[3]))],0.1)),alpha=abs(rnorm(50,rhomu,0.3)),theta=rnorm(50,0.8,0.1))  
  
  ########################################################
  ########################################################
  ######### Thats the a "Distraction" OCU Model.##########
  ########################################################
  ########################################################
  
  
  
  MakeOCUDataNoise <- function(v) {
    ocu<-v[1]
    alpha<-v[2]
    theta<-v[3]
    ##Utilities for each choice
    for (i in 1:nrow(simulateSubset)){
      #get the social choices.
      simulateSubset$Util_Sure[i] <- (simulateSubset$valueSure[i]^alpha)
      simulateSubset$Util_Risk[i] <- (simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)
      if (is.na(simulateSubset$OtherChoseRisk[i])){ # if there is no Advice.
        simulateSubset$ProbChooseRisk[i] <- 1/(1 + exp(-((simulateSubset$Util_Risk[i] - simulateSubset$Util_Sure[i])*(theta)))) # no it doesn
      }else {
        simulateSubset$ProbChooseRisk[i] <-(1-abs(ocu)) * (1/(1 + exp(-((simulateSubset$Util_Risk[i] - simulateSubset$Util_Sure[i])*(theta))))) + (abs(ocu)/2) # no it doesn
      }
      simulateSubset$ChooseRisk[i]<-rbinom(1,1,simulateSubset$ProbChooseRisk[i])
    }# choice rule Here OCU might make sense, too? 
    #save the parameters.
    simulateSubset$alpha<-alpha
    simulateSubset$theta<-theta
    simulateSubset$ocu<-ocu
    # simulateSubset$ChooseRisk[i]<-rbinom(1,1,simulateSubset$ProbChooseRisk[i])
    return(simulateSubset)
  }
  # if you want to do this not on the cluster you have to put this into a loop.
  # here i pick the subject that shall be used for simulation the data.
  simulateSubset<-simulate[simulate$subject==subarray[as.numeric(bashInput[1])],]
  dummytomakelists<-MakeOCUDataNoise(c(parameterG1[1,1],parameterG1[1,2],parameterG1[1,3]))
  Simulations1=as.list(dummytomakelists)
  Simulations2=as.list(dummytomakelists)
  #for (i in 1:length(parameter$ocu)){# go through this
  #make data of the FIRST group.
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataNoise(c(parameterG1[i,1],parameterG1[i,2],parameterG1[i,3]))
    Simulations1[[i]]<-as.list(dummytomakelists)
    Simulations1[[i]]$group<-1# i make a new entry here and tell the simulations flag that this is a new group.
  }#end first
  #make data of the SECOND group:
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataNoise(c(parameterG2[i,1],parameterG2[i,2],parameterG2[i,3]))
    Simulations2[[i]]<-as.list(dummytomakelists)
    Simulations2[[i]]$group<-2# i make a new entry here and tell the simulations flag that this is a new group.
  }
  #combine the lists
  Simulations<-do.call(c, list(Simulations1, Simulations2))
  save(Simulations,file=paste0("ModelTemperature/",as.character(bashInput[4]),"TemperatureSocial",as.character(bashInput[3]),as.character(bashInput[1]),".RData"))
} else if (bashInput[2]==4){
  
  
  MakeOCUDataNull <- function(v) {
    
    ########################################################
    ########################################################
    ######### Thats the a "Null" OCU Model.######
    ########################################################
    ########################################################
    
    ocu<-0
    alpha<-v[2]
    theta<-v[3]
    ##Utilities for each choice
    for (i in 1:nrow(simulateSubset)){
      #get the social choices.
      if (is.na(simulateSubset$OtherChoseRisk[i])){ # NoAdvice.
        simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
        simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)) #risk
      }else
        if (simulateSubset$OtherChoseRisk[i]==1){ # advice is risky
          simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
          simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha))
        }
      else 
        if (simulateSubset$OtherChoseRisk[i]==0){# advice is safe
          simulateSubset$Util_Sure[i] <- (simulateSubset$valueSure[i]^alpha)+ocu
          simulateSubset$Util_Risk[i] <- (simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)
        }
      simulateSubset$ProbChooseRisk[i] <- 1/(1 + exp(-(( simulateSubset$Util_Risk[i] - simulateSubset$Util_Sure[i])*(theta)))) # no it doesn
      simulateSubset$ChooseRisk[i]<-rbinom(1,1,simulateSubset$ProbChooseRisk[i])
    }# choice rule Here OCU might make sense, too? 
    #save the parameters.
    simulateSubset$alpha<-alpha
    simulateSubset$theta<-theta
    simulateSubset$ocu<-ocu
    return(simulateSubset)
  }
  # bashInput<-1
  simulateSubset<-simulate[simulate$subject==subarray[as.numeric(bashInput[1])],]
  dummytomakelists<-MakeOCUDataNull(c(parameterG1[1,1],parameterG1[1,2],parameterG1[1,3]))
  Simulations1=as.list(dummytomakelists)
  Simulations2=as.list(dummytomakelists)
  #make data of the FIRST group.
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataNull(c(parameterG1[i,1],parameterG1[i,2],parameterG1[i,3]))
    Simulations1[[i]]<-as.list(dummytomakelists)
    Simulations1[[i]]$group<-1# i make a new entry here and tell the simulations flag that this is a new group.
  }#end first
  #make data of the SECOND group:
  for (i in 1:length(parameterG1$ocu)){
    dummytomakelists<-MakeOCUDataNull(c(parameterG2[i,1],parameterG2[i,2],parameterG2[i,3]))
    Simulations2[[i]]<-as.list(dummytomakelists)
    Simulations2[[i]]$group<-2# i make a new entry here and tell the simulations flag that this is a new group.
  }
  #combine the lists
  Simulations<-do.call(c, list(Simulations1, Simulations2))
  save(Simulations,file=paste0("ModelNull/",as.character(bashInput[4]),"NullSocial",as.character(bashInput[3]),as.character(bashInput[1]),".RData"))
}else if(bashInput[2]==5){
  # to decorrelate OCU safe and Risk for my simulations. 
  # This sequence as been generated by calling SepParams<-sample(abs(seq(0, 5, length.out=12)),12), and adding a 0 to the end and 
  # partitioning the Vector into 2. 
  #Decorrelate the values.
  #Prevent the Values from becoming 0 with abs
  SepParamsRisk<-c( 1.3636364, 2.2727273, 2.7272727, 5.0000000, 3.6363636, 0.4545455, 1.8181818, 0.0000000, 4.0909091, 0.9090909, 3.1818182, 4.5454545)
  SepParamsSafe<-c(4.5454545, 2.7272727, 2.2727273, 3.6363636, 1.8181818, 0.9090909, 3.1818182, 4.0909091, 0.4545455, 0.000000, 5.0000000, 1.3636364)
  
  G1<-SepParamsRisk[1:(length(SepParamsRisk)/2)]#FirstHalf
  G2<-SepParamsRisk[(length(SepParamsRisk)/2):length(SepParamsRisk)]#SecondHalf
  
  G3<-SepParamsSafe[1:(length(SepParamsSafe)/2)]#FirstHalf
  G3<-SepParamsSafe[(length(SepParamsSafe)/2):length(SepParamsSafe)]#SecondHalf
  
  parameterG1<-data.frame(ocuSafe=abs(rnorm(50,G1[(1+as.numeric(bashInput[3]))],0.3)),ocuRisk=abs(rnorm(50,G3[(1+as.numeric(bashInput[3]))],0.3)),alpha=abs(rnorm(50,rhomu,0.3)),theta=rnorm(50,0.8,0.1))
  parameterG2<-data.frame(ocuSafe=abs(rnorm(50,G2[(1+as.numeric(bashInput[3]))],0.3)),ocuRisk=abs(rnorm(50,G4[(1+as.numeric(bashInput[3]))],0.3)),alpha=abs(rnorm(50,rhomu,0.3)),theta=rnorm(50,0.8,0.1))  
  
  
  MakeOCUDataSep <- function(v) {
    
    ########################################################
    ########################################################
    ######### Thats the a "assymetric" OCU Model.######
    ########################################################
    ########################################################
    ocuSafe<-v[1]
    ocuRisk<-v[2]
    alpha<-v[3]
    theta<-v[4]
    ##Utilities for each choice
    for (i in 1:nrow(simulateSubset)){
      #get the social choices.
      if (is.na(simulateSubset$OtherChoseRisk[i])){ # NoAdvice.
        simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
        simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)) #risk
      }else
        if (simulateSubset$OtherChoseRisk[i]==1){ # advice is risky
          simulateSubset$Util_Sure[i] <-  (simulateSubset$valueSure[i]^alpha)
          simulateSubset$Util_Risk[i] <-  ((simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha))+ocuRisk #risk
        }
      else 
        if (simulateSubset$OtherChoseRisk[i]==0){# advice is safe
          simulateSubset$Util_Sure[i] <- (simulateSubset$valueSure[i]^alpha)+ocuSafe
          simulateSubset$Util_Risk[i] <- (simulateSubset$probGamble[i])*(simulateSubset$valueGamble[i]^alpha)
        }
      simulateSubset$ProbChooseRisk[i] <- 1/(1 + exp(-((simulateSubset$Util_Risk[i] - simulateSubset$Util_Sure[i])*(theta)))) # no it doesn
      simulateSubset$ChooseRisk[i]<-rbinom(1,1,simulateSubset$ProbChooseRisk[i])
    }# choice rule Here OCU might make sense, too? 
    #save the parameters.
    simulateSubset$alpha<-alpha
    simulateSubset$theta<-theta
    simulateSubset$ocuSafe<-ocuSafe
    simulateSubset$ocuRisk<-ocuRisk
    
    return(simulateSubset)
  }
  # bashInput<-1
  simulateSubset<-simulate[simulate$subject==subarray[as.numeric(bashInput[1])],]
  dummytomakelists<-MakeOCUDataSep(c(parameterG1[1,1],parameterG1[1,2],parameterG1[1,3],parameterG1[1,4]))
  Simulations1=as.list(dummytomakelists)
  Simulations2=as.list(dummytomakelists)
  #make data of the FIRST group.
  for (i in 1:length(parameterG1$ocuSafe)){
    dummytomakelists<-MakeOCUDataSep( c(parameterG1[i,1],parameterG1[i,2],parameterG1[i,3],parameterG1[i,4]))
    Simulations1[[i]]<-as.list(dummytomakelists)
    Simulations1[[i]]$group<-1# i make a new entry here and tell the simulations flag that this is a new group.
  }#end first
  #make data of the SECOND group:
  for (i in 1:length(parameterG1$ocuSafe)){
    dummytomakelists<-MakeOCUDataSep(c(parameterG2[i,1],parameterG2[i,2],parameterG2[i,3],parameterG2[i,4]))
    Simulations2[[i]]<-as.list(dummytomakelists)
    Simulations2[[i]]$group<-2# i make a new entry here and tell the simulations flag that this is a new group.
  }
  #combine the lists
  Simulations<-do.call(c, list(Simulations1, Simulations2))
  save(Simulations,file=paste0("ModelSep/",as.character(bashInput[4]),"SepSocial",as.character(bashInput[3]),as.character(bashInput[1]),".RData"))
}
