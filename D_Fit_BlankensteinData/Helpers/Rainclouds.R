source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")
#source("/Users/ciranka/Documents/Projects/work_in_Progress/!HelperFunctions/Start_Workflow.R")
source("plotProbFunRisk.R")

# first i need to extract the parameter values i dummy load the first dataset

packages <- c("ggplot2", "dplyr", "lavaan", "plyr", "cowplot", "rmarkdown", 
              "readr", "caTools", "bitops")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(cowplot)
library(dplyr)
library(readr)
library(loo)
# okay at the end you have to do this for the WINNING! model. 
# load early adolescents.
load("Kids_Braams_ModelFitsInfo_Model1.RData")# load early adolescents.
load("Kids_Braams_ModelFitsRisk_Model2.RData")
load("Kids_Braams_ModelFitsNoise_Model3.RData")
load("Kids_Braams_ModelFitsNull_Model4.RData")

paramsInfo<-extract(fitInfo)
paramsRisk<-extract(fitRisk)
paramsNoise<-extract(fitNoise)
paramsNull<-extract(fitNull)

nKids=22
# here i make a matrix that stores all the loglikelhoods in a way that i can pass them to 
loglikInfo<- array(NA, dim=c(10002*180, 1, 22))#
loglikRisk<- array(NA, dim=c(10002*180, 1, 22))#
loglikNoise<- array(NA, dim=c(10002*180, 1, 22))#
loglikNull<- array(NA, dim=c(10002*180, 1, 22))#

for(i in 1:nKids){
  # here i make the vectors that i need for model comparison.
  loglikInfo[,,i]<-as.vector(paramsInfo$log_lik[,i,])
  loglikRisk[,,i]<-as.vector(paramsRisk$log_lik[,i,])
  loglikNoise[,,i]<-as.vector(paramsNoise$log_lik[,i,])
  loglikNull[,,i]<-as.vector(paramsNull$log_lik[,i,])
}

reffInfo<- relative_eff(exp(loglikInfo))
reffRisk<-relative_eff(exp(loglikRisk))
reffNoise<-relative_eff(exp(loglikNoise))
reffNull<-relative_eff(exp(loglikNull))


LooInfo<-loo(loglikInfo,r_eff = reffInfo,cores=3)
LooRisk<-loo(loglikRisk,r_eff = reffRisk,cores=3)
LooNoise<-loo(loglikNoise,r_eff = reffNoise,cores=3)
LooNull<-loo(loglikNull,r_eff = reffNull,cores=3)

compare(LooInfo,LooRisk,LooNoise,LooNull)

#load early adolescents.
load("EarlyAdol_Braams_ModelFitsInfo_Model1.RData")# load early adolescents.
load("EarlyAdol_Braams_ModelFitsRisk_Model2.RData")
load("EarlyAdol_Braams_ModelFitsNoise_Model3.RData")
load("EarlyAdol_Braams_ModelFitsNull_Model4.RData")

paramsInfoEA<-extract(fitInfo)
paramsRiskEA<-extract(fitRisk)
paramsNoiseEA<-extract(fitNoise)
paramsNullEA<-extract(fitNull)

nEA=38
# here i make a matrix that stores all the loglikelhoods in a way that i can pass them to 
loglikInfoEA<- array(NA, dim=c(10002*180, 1, nEA))#
loglikRiskEA<- array(NA, dim=c(10002*180, 1, nEA))#
loglikNoiseEA<- array(NA, dim=c(10002*180, 1, nEA))#
loglikNullEA<- array(NA, dim=c(10002*180, 1, nEA))#

for(i in 1:nEA){
  # here i make the vectors that i need for model comparison.
  loglikInfoEA[,,i]<-as.vector(paramsInfoEA$log_lik[,i,])
  loglikRiskEA[,,i]<-as.vector(paramsRiskEA$log_lik[,i,])
  loglikNoiseEA[,,i]<-as.vector(paramsNoiseEA$log_lik[,i,])
  loglikNullEA[,,i]<-as.vector(paramsNullEA$log_lik[,i,])
}

reffInfo<- relative_eff(exp(loglikInfoEA))
reffRisk<-relative_eff(exp(loglikRiskEA))
reffNoise<-relative_eff(exp(loglikNoiseEA))
reffNull<-relative_eff(exp(loglikNullEA))


LooInfoEA<-loo(loglikInfoEA,r_eff = reffInfo,cores=3)
LooRiskEA<-loo(loglikRiskEA,r_eff = reffRisk,cores=3)
LooNoiseEA<-loo(loglikNoiseEA,r_eff = reffNoise,cores=3)
LooNullEA<-loo(loglikNullEA,r_eff = reffNull,cores=3)

compare(LooInfoEA,LooRiskEA,LooNoiseEA,LooNullEA)

load("LateAdol_Braams_ModelFitsInfo_Model1.RData")# load Mid adolescents.

#load early adolescents.
load("LateAdol_Braams_ModelFitsInfo_Model1.RData")# load early adolescents.
load("LateAdol_Braams_ModelFitsRisk_Model2.RData")
load("LateAdol_Braams_ModelFitsNoise_Model3.RData")
load("LateAdol_Braams_ModelFitsNull_Model4.RData")

paramsInfoLA<-extract(fitInfo)
paramsRiskLA<-extract(fitRisk)
paramsNoiseLA<-extract(fitNoise)
paramsNullLA<-extract(fitNull)

nLA=14
# here i make a matrix that stores all the loglikelhoods in a way that i can pass them to 
loglikInfoLA<- array(NA, dim=c(10002*180, 1, nLA))#
loglikRiskLA<- array(NA, dim=c(10002*180, 1, nLA))#
loglikNoiseLA<- array(NA, dim=c(10002*180, 1, nLA))#
loglikNullLA<- array(NA, dim=c(10002*180, 1, nLA))#

for(i in 1:nLA){
  # here i make the vectors that i need for model comparison.
  loglikInfoLA[,,i]<-as.vector(paramsInfoLA$log_lik[,i,])
  
  loglikRiskLA[,,i]<-as.vector(paramsRiskLA$log_lik[,i,])
  
  loglikNoiseLA[,,i]<-as.vector(paramsNoiseLA$log_lik[,i,])
  
  loglikNullLA[,,i]<-as.vector(paramsNullLA$log_lik[,i,])
}
reffInfo<- relative_eff(exp(loglikInfoLA))
reffRisk<-relative_eff(exp(loglikRiskLA))
reffNoise<-relative_eff(exp(loglikNoiseLA))
reffNull<-relative_eff(exp(loglikNullLA))


looInfoLA<-loo(loglikInfoLA,r_eff = reffInfo,cores=3)
looRiskLA<-loo(loglikRiskLA,r_eff = reffRisk,cores=3)
looNoiseLA<-loo(loglikNoiseLA,r_eff = reffNoise,cores=3)
looNullLA<-loo(loglikNullLA,r_eff = reffNull,cores=3)



compare(LooInfoLA,LooRiskLA,LooNoiseLA,LooNullLA)

paramsLA<-extract(fitInfo)

#load early adolescents.
load("YoungAdul_Braams_ModelFitsInfo_Model1.RData")# load early adolescents.
load("YoungAdul_Braams_ModelFitsRisk_Model2.RData")
load("YoungAdul_Braams_ModelFitsNoise_Model3.RData")
load("YoungAdul_Braams_ModelFitsNull_Model4.RData")

paramsInfoYA<-extract(fitInfo)
paramsRiskYA<-extract(fitRisk)
paramsNoiseYA<-extract(fitNoise)
paramsNullYA<-extract(fitNull)

nYA=25
# here i make a matrix that stores all the loglikelhoods in a way that i can pass them to 
loglikInfoYA<- array(NA, dim=c(10002*180, 1, nYA))#
loglikRiskYA<- array(NA, dim=c(10002*180, 1, nYA))#
loglikNoiseYA<- array(NA, dim=c(10002*180, 1, nYA))#
loglikNullYA<- array(NA, dim=c(10002*180, 1, nYA))#

for(i in 1:nYA){
  # here i make the vectors that i need for model comparison.
  loglikInfoYA[,,i]<-as.vector(paramsInfoYA$log_lik[,i,])
  
  loglikRiskYA[,,i]<-as.vector(paramsRiskYA$log_lik[,i,])
  
  loglikNoiseYA[,,i]<-as.vector(paramsNoiseYA$log_lik[,i,])
  
  loglikNullYA[,,i]<-as.vector(paramsNullYA$log_lik[,i,])
}
reffInfo<- relative_eff(exp(loglikInfoYA))
reffRisk<-relative_eff(exp(loglikRiskYA))
reffNoise<-relative_eff(exp(loglikNoiseYA))
reffNull<-relative_eff(exp(loglikNullYA))


LooInfoYA<-loo(loglikInfoYA,r_eff = reffInfo,cores=3)
LooRiskYA<-loo(loglikRiskYA,r_eff = reffRisk,cores=3)
LooNoiseYA<-loo(loglikNoiseYA,r_eff = reffNois e,cores=3)
LooNullYA<-loo(loglikNullYA,r_eff = reffNull,cores=3)



YA<-compare(LooInfoLA,LooRiskLA,LooNoiseLA,LooNullLA)
rm(fitInfo,FitNoise,FitNull,FitRisk)

save.image()


Subjs<-length(paramsEA$rho[1,])+length(paramsMA$rho[1,])+length(paramsLA$rho[1,])+length(paramsYA$rho[1,])
raincloudTibble<-tibble(
  PostMeanRho=rep(0,Subjs),
  PostVarRho=rep(0,Subjs),
  PostMeanTau=rep(0,Subjs),
  PostVarTau=rep(0,Subjs),
  PostMeanOCU=rep(0,Subjs),
  PostVarOCU=rep(0,Subjs),
  Group=rep(0,Subjs)
)

i=1;
# get means and variance of each agegroup and 
for (j in 1:length(paramsEA$rho[1,])){
  raincloudTibble$PostMeanRho[i]=mean(paramsEA$rho[,j])
  raincloudTibble$PostVarRho[i]=var(paramsEA$rho[,j])
  raincloudTibble$PostMeanTau[i]=mean(paramsEA$tau[,j])
  raincloudTibble$PostVarTau[i]=var(paramsEA$tau[,j])
  raincloudTibble$PostMeanOCU[i]=mean(paramsEA$ocu[,j])
  raincloudTibble$PostVarOCU[i]=var(paramsEA$ocu[,j])
  raincloudTibble$Group[i]=1
  i=i+1;
}
for (j in 1:length(paramsMA$rho[1,])){
  raincloudTibble$PostMeanRho[i]=mean(paramsMA$rho[,j])
  raincloudTibble$PostVarRho[i]=var(paramsMA$rho[,j])
  raincloudTibble$PostMeanTau[i]=mean(paramsMA$tau[,j])
  raincloudTibble$PostVarTau[i]=var(paramsMA$tau[,j])
  raincloudTibble$PostMeanOCU[i]=mean(paramsMA$ocu[,j])
  raincloudTibble$PostVarOCU[i]=var(paramsMA$ocu[,j])
  raincloudTibble$Group[i]=2  
  i=i+1;
}
for (j in 1:length(paramsLA$rho[1,])){
  raincloudTibble$PostMeanRho[i]=mean(paramsLA$rho[,j])
  raincloudTibble$PostVarRho[i]=var(paramsLA$rho[,j])
  raincloudTibble$PostMeanTau[i]=mean(paramsLA$tau[,j])
  raincloudTibble$PostVarTau[i]=var(paramsLA$tau[,j])
  raincloudTibble$PostMeanOCU[i]=mean(paramsLA$ocu[,j])
  raincloudTibble$PostVarOCU[i]=var(paramsLA$ocu[,j])
  raincloudTibble$Group[i]=3  
  i=i+1;
}
for (j in 1:length(paramsYA$rho[1,])){
  raincloudTibble$PostMeanRho[i]=mean(paramsYA$rho[,j])
  raincloudTibble$PostVarRho[i]=var(paramsYA$rho[,j])
  raincloudTibble$PostMeanTau[i]=mean(paramsYA$tau[,j])
  raincloudTibble$PostVarTau[i]=var(paramsYA$tau[,j])
  raincloudTibble$PostMeanOCU[i]=mean(paramsYA$ocu[,j])
  raincloudTibble$PostVarOCU[i]=var(paramsYA$ocu[,j])
  raincloudTibble$Group[i]=4
  i=i+1;
}


ggplot(raincloudTibble,aes(x=as.numeric(Group),y=PostMeanOCU, fill = Group))+
  geom_flat_violin(position = position_nudge(x = .35, y = 0),adjust =2,trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = 1,shape=9)+
  #note that here we need to set the x-variable to a numeric variable and bump it to get the boxplots to line up with the rainclouds. 
  geom_boxplot(aes(x = as.numeric(Group) +0.25, y = PostMeanOCU), alpha = 0.3, width = .1, colour = "BLACK") +
  ylab(expression(paste("Posterior Mean of ",psi)))+xlab('Age Group')+coord_flip()+guides(fill = FALSE, colour = FALSE) +
  ggtitle(expression(paste("Mean Posterior Estimates of ",psi," by Age")))+ my_theme






#Here i call the Function which curves the OCU thingy for different models. 
subz=unique(ModelParamsFull$ppn)
curve<-list()
for(i in 1:length(subz)){
  data<-subset(ModelParamsFull, ModelParamsFull$ppn==subz[i]) 
  curve[[i]]<-plotProbFunRisk()
}
simulation<-do.call("rbind", curve)
#beware: SummarySE probably calculates  your SE& the Confidence interval wrong if you have multiple observations per subject.
CIs<-summarySE(simulation, measurevar="measurement", groupvars=c("deltaEV","prob","condition", "Age")) # this is good but it calculates se+ci wrong for my purpuses.
simulation$sd<-NaN
simulation$se<-NaN
simulation$ci<-NaN
#Heres the ci Correction:
Ageloop=unique(CIs$Age)# loop through all the agegroups you can find
Conditionloop=unique(CIs$condition)#loop through all the conditions you can find
#here i correct the se and the ci and put it into the long dataframe which i then use in ggplot
for (i in 1:length(Ageloop)){
  CIs[CIs$Age==Ageloop[i],]$ci=1.96*(CIs[CIs$Age==Ageloop[i],]$sd/sqrt(length(unique(ModelParamsFull[ModelParamsFull$Agegroup==Ageloop[i],]$ppn))))  #here I correct for se
  CIs[CIs$Age==Ageloop[i],]$se=(CIs[CIs$Age==Ageloop[i],]$sd/sqrt(length(unique(ModelParamsFull[ModelParamsFull$Agegroup==Ageloop[i],]$ppn)))) #here is the se correction
}
CIs$upperSE=CIs$measurement+CIs$se
CIs$lowerSE=CIs$measurement-CIs$se
#Actual Data for the Winning Model. 





