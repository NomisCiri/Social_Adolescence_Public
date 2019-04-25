source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")
source("/Users/ciranka/Documents/Projects/work_in_Progress/!HelperFunctions/Start_Workflow.R")
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

# okay at the end you have to do this for the WINNING! model. 
load("Kids_Braams_ModelFitsInfo_Model1.RData")# load early adolescents.
paramsEA<-extract(fitInfo)
load("EarlyAdol_Braams_ModelFitsInfo_Model1.RData")# load Mid adolescents.
paramsEA<-extract(fitInfo)
load("LateAdol_Braams_ModelFitsInfo_Model1.RData")# load Mid adolescents.
paramsLA<-extract(fitInfo)
load("YoungAdul_Braams_ModelFitsInfo_Model1.RData")# load Mid adolescents.
paramsLA<-extract(fitInfo)

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





