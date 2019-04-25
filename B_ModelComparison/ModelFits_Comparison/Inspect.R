# Inspect Models.
load('ModelFitsInfo1.RData')
load('ModelFitsRisk.RData')
load('ModelFitsNoise.RData')


load("InfoSocial1.RData")
rstan_options(auto_write = TRUE) 
##### FIRST MAKE THE SAME DATAFORMAT AS YOU PASS IT TO STAN IN ORDER TO CHECK THE PAREMTER CORRELATIONS.


#change colname of subject into subjID
# Individual Subjects
#numSubjs<-30 #DEBUG
numSubjs<-length(Simulations)#Total Number of Subs
nGroups<-2
numSubjsG <- (length(Simulations)/2) # number of subjects per group
subjList <- 1:numSubjs
#first i run through every entry and replace subnr with the index.
# i need to recode the "conditions" so they fit into stan. maybe it
for (i in 1:numSubjs){
  for(j in 1:length(Simulations[[i]]$subject)){
    Simulations[[i]]$subject[j]=i
    if(is.na(Simulations[[i]]$OtherChoseRisk[j])){#stan cant deal with NA s
      Simulations[[i]]$OtherChoseRisk[j]<-2;# i make it a 0 then.
    }else if (Simulations[[i]]$OtherChoseRisk[j]==1){
      Simulations[[i]]$OtherChoseRisk[j]<-3;# i make it a 3 then.
    } else if (Simulations[[i]]$OtherChoseRisk[j]==0){
      Simulations[[i]]$OtherChoseRisk[j]<-1;# i make it a 3 then.
    }
  }
}
####### number of trials and see which group he is in for each subject######
Tsubj <- as.vector(rep(0, numSubjs)) 
GSubj <- as.vector(rep(0, numSubjs)) 
for (sIdx in 1:numSubjs)  {
  curSubj     <- subjList[sIdx]
  Tsubj[sIdx] <- length(Simulations[[i]]$subject)  # How many entries per Subject?
  GSubj[sIdx] <- Simulations[[i]]$group
}
maxTrials <- max(Tsubj)
nGroups<- max(GSubj)
# Information for user continued
cat(" # of (max) trials per subject = ", maxTrials, "\n\n")
# for multiple subjects
safe_payoff    <- array(0, c(numSubjsG, maxTrials,nGroups))
risky_payoff    <- array(0, c(numSubjsG, maxTrials,nGroups))
condition    <- array(0, c(numSubjsG, maxTrials,nGroups))#Other Chose Risk?
p_gamble    <- array(0, c(numSubjsG, maxTrials,nGroups))
choice  <- array(-1, c(numSubjsG, maxTrials,nGroups))
rho<- array(-1, c(numSubjsG,nGroups))
tau<- array(-1, c(numSubjsG,nGroups))
ocu<- array(-1, c(numSubjsG,nGroups))
# generate the data Lists to be passed to stan
# concatenate different groups in the third dimension.
g1=1
g2=1
TsubjG<-array(1,c(numSubjsG,nGroups))
for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  if (Simulations[[i]]$group==1){
    safe_payoff[g1, 1:useTrials, Simulations[[i]]$group]    <- 5
    risky_payoff[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$valueGamble
    condition[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$OtherChoseRisk
    p_gamble[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$probGamble
    choice[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$ChooseRisk
    TsubjG[g1,Simulations[[i]]$group]<-useTrials
    # FOR PARAMETER RECOVERY:
    rho[g1,Simulations[[i]]$group]<-Simulations[[i]]$alpha[1]
    tau[g1,Simulations[[i]]$group]<-Simulations[[i]]$theta[1]
    ocu[g1,Simulations[[i]]$group]<-Simulations[[i]]$ocu[1]
    g1=g1+1;
  }else if (Simulations[[i]]$group==2){
    safe_payoff[g2, 1:useTrials, Simulations[[i]]$group]    <- 5
    risky_payoff[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$valueGamble
    condition[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$OtherChoseRisk
    p_gamble[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$probGamble
    choice[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$ChooseRisk
    TsubjG[g2,Simulations[[i]]$group]<-useTrials
    # FOR PARAMETER RECOVERY:
    rho[g2,Simulations[[i]]$group]<-Simulations[[i]]$alpha[1]
    tau[g2,Simulations[[i]]$group]<-Simulations[[i]]$theta[1]
    ocu[g2,Simulations[[i]]$group]<-Simulations[[i]]$ocu[1]
    g2=g2+1
  }
}
Tsubj<-TsubjG
# Specify the number of parameters and parameters of interest





#### HERE YOU START LOOKING AT THE DATA:
library(shinystan)
library(tidyverse)
library(rstan)
library(tidybayes)
library(magrittr)
library(dplyr)
library(ggplot2)
library(ggstance)
library(rstan)
library(tidybayes)
library(emmeans)
library(broom)
library(brms)
library(modelr)
library(forcats)
launch_shinystan(fitInfo)

k<-rstan::extract(fitInfo, permuted = TRUE, inc_warmup = FALSE,
        include = TRUE)
k %>%
  spread_samples() %>%
  mean_qi()



