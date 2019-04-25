
library(rstan)
library(dplyr)
rstan_options(auto_write = TRUE) 
bashInput <- commandArgs(trailingOnly = TRUE)

#load Blankensteins Data.
ModelParamsFull<-read.csv("PG_raw_data_combined_cleaned_Wouter.csv")
bashInput<-c(1,1)
#########
#########
#########
#todo. transfer Model parameters so i can use it with the STAN model file.
as.numeric(ModelParamsFull$OSFparticipantID)
#change colname of subject into subjID
numSubjs<-length(unique(ModelParamsFull$OSFparticipantID))#Total Number of Subs
subjList <- unique(ModelParamsFull$OSFparticipantID)
#first i run through every entry and replace subnr with the index.
# i need to recode the "conditions" so they fit into stan. maybe it

## mutate the Data so that it looks the same as the Blankenstein Data and i have less trouble in fitting my model.
ModelParamsFull<- ModelParamsFull%>%dplyr::mutate(
  PeerChoiceSafe0_Risk1 = case_when(
    condition1=="solo" ~ 2,# i dont need this but i restricted the numbers in stan between 1 and 3
    condition1=="socialrisky" ~ 3,# risky choices are coded as 3 in my stan code
    condition1=="socialsafe" ~ 1,# safe choices are coded as 1 in my stan code
    TRUE~0 # keep the rest.
  ),#end PeerChoice.
  typeRA = case_when(
    Ambiguity>0~0,# AMBIGUITY IS RECODED AS 0
    Ambiguity==0~1,# RISK IS RECODED AS 1
    TRUE~0
  ),Agegroup = case_when(
    Age.bins=="12-13"~1,# AMBIGUITY IS RECODED AS 0
    Age.bins=="14-15"~2,# RISK IS RECODED AS 1
    Age.bins=="16-17"~2,
    Age.bins=="16-17"~3,
    Age.bins=="18-19"~3,
    Age.bins=="20-21"~4,
    Age.bins=="22"~4,
    TRUE~0
  )
  # end Riskamb
)
ModelParamsFull<-ModelParamsFull[ModelParamsFull$typeRA=="1",]# keep only The Risk trails.
Agegroups<-unique(ModelParamsFull$Agegroup)# for indexing my agegroups.

if (as.numeric(bashInput[1])!=5){# partition it into groups if its unequal five. If its five; then do fitting on grouplevel.
  ModelParamsFull<-ModelParamsFull%>%dplyr::filter(Agegroup==Agegroups[as.numeric(bashInput[1])])#select only one Agegroup
}
# now check how many participants we have.
as.numeric(ModelParamsFull$OSFparticipantID)
#change colname of subject into subjID
numSubjs<-length(unique(ModelParamsFull$OSFparticipantID))#Total Number of Subs
subjList <- unique(ModelParamsFull$OSFparticipantID)

####### number of trials and see which group he is in for each subject######
Tsubj <- as.vector(rep(0, numSubjs)) 
GSubj <- as.vector(rep(0, numSubjs)) 
for (sIdx in 1:numSubjs)  {
  curSubj     <- subjList[sIdx]
  Tsubj[sIdx] <- length(ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$OSFparticipantID)  # How many entries per Subject?
  #GSubj[sIdx] <- Simulations[[i]]$group
}
maxTrials <- max(Tsubj)

# Information for user continued
cat(" # of (max) trials per subject = ", maxTrials, "\n\n")

# for multiple subjects
safe_payoff    <- array(0, c(numSubjs, maxTrials))
risky_payoff    <- array(0, c(numSubjs, maxTrials))
condition    <- array(0, c(numSubjs, maxTrials))#Other Chose Risk?
p_gamble    <- array(0, c(numSubjs, maxTrials))
choice  <- array(0, c(numSubjs, maxTrials))


safe_Hpayoff<- array(0, c(numSubjs, maxTrials))
safe_Lpayoff<- array(0, c(numSubjs, maxTrials))
risky_Hpayoff<- array(0, c(numSubjs, maxTrials))
risky_Lpayoff<- array(0, c(numSubjs, maxTrials))
#Agegroup<- array(0, c(numSubjs, maxTrials))

# generate the data Lists to be passed to stan
# concatenate different groups in the third dimension.

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  condition[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$PeerChoiceSafe0_Risk1
  p_gamble[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Phigh
  choice[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$choice
  safe_Hpayoff[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vhighsafe
  safe_Lpayoff[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vlowsafe
  risky_Hpayoff[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vhighrisky
  risky_Lpayoff[i, 1:useTrials] <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vlowrisky
}

# Specify the number of parameters and parameters of interest
numPars <- 4
POI     <- c("mu_rho", "mu_tau", "mu_ocu",
             "rho","ocu","tau", "log_lik","y_pred")


inits<-"fixed"
# priors
if (inits[1] != "random") {
  if (inits[1] == "fixed") {
    inits_fixed <- c(0.5, 0.9, 0.1)
  } else {
    if (length(inits) == numPars) {
      inits_fixed <- inits
      # mu_ocu   =rep(inits_fixed[3],2)
    } else {
      stop("Check your inital values!")
    }
  }
  genInitList <- function() {
    list(
      #initial values.
      mu_rho   =rep(inits_fixed[1]),
      mu_tau   =rep(inits_fixed[2]),
      mu_ocu   =c(inits_fixed[3]),
      
      sigma_rho= rep(c(1.0)),
      sigma_tau= rep(c(1.0)),
      sigma_ocu= rep(c(1.0)),
      
      rho_p    = rep(inits_fixed[1],numSubjs),
      tau_p    = rep(inits_fixed[2],numSubjs),
      ocu_p    = rep(inits_fixed[3],numSubjs)
      #ocu_p    = matrix(inits_fixed[3],numSubjsG, nGroups)
    )
  }
} else {
  genInitList <- "random"
}


#this is all i need for stan. 
dataList <- list(
  N       = numSubjs,## number of subjects in each group.
  T       = maxTrials,
  Tsubj   = Tsubj,
  numPars = numPars,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gamble,
  choice = choice,
  safe_Hpayoff    = safe_Hpayoff,
  safe_Lpayoff    = safe_Lpayoff,
  risky_Hpayoff    = risky_Hpayoff,
  risky_Lpayoff = risky_Lpayoff
 # agegroup=agegroup
  
)

#setup the different modelfitting things.
if(as.numeric(bashInput[2])==1){
  fitInfo = stan("ocu_hier_InfoRisk_Braams.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 21000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
  switch(as.character(bashInput[1]),
         "1"={
           save(fitInfo,file=paste0("Kids_Braams_ModelFitsInfo_Model",bashInput[2],".RData"))
         },
         "2"={
           save(fitInfo,file=paste0("EarlyAdol_Braams_ModelFitsInfo_Model",bashInput[2],".RData"))
         },
         "3"={
           save(fitInfo,file=paste0("LateAdol_Braams_ModelFitsInfo_Model",bashInput[2],".RData"))
         },
         "4"={
           save(fitInfo,file=paste0("YoungAdul_Braams_ModelFitsInfo_Model",bashInput[2],".RData"))
         },
         "5"={
           save(fitInfo,file=paste0("WholeFit_Braams_ModelFitsInfo_Model",bashInput[2],".RData"))
         }
  )#end AgeGroupswitch. 
} else if(as.numeric(bashInput[2])==2){
  
  fitRisk = stan("ocu_hier_RiskRisk_Braams.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 21000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta =0.99)
  )
  switch(as.character(bashInput[1]),
         "1"={
           save(fitRisk,file=paste0("Kids_Braams_ModelFitsRisk_Model",bashInput[2],".RData"))
         },
         "2"={
           save(fitRisk,file=paste0("EarlyAdol_Braams_ModelFitsRisk_Model",bashInput[2],".RData"))
         },
         "3"={
           save(fitRisk,file=paste0("LateAdol_Braams_ModelFitsRisk_Model",bashInput[2],".RData"))
         },
         "4"={
           save(fitRisk,file=paste0("YoungAdul_Braams_ModelFitsRisk_Model",bashInput[2],".RData"))
         },
         "5"={
           save(fitRisk,file=paste0("WholeFit_Braams_ModelFitsRisk_Model",bashInput[2],".RData"))
         }
  )#end AgeGroupswitch.
}else if(as.numeric(bashInput[2])==3){
  fitNoise = stan("ocu_hier_NoiseRisk_Braams.stan",
                  data   = dataList,
                  pars   = POI,
                  init   = genInitList,
                  iter   = 21000,
                  thin   = 3,
                  cores = 3,
                  chains =3,
                  control = list(adapt_delta = 0.99)
  )
  switch(as.character(bashInput[1]),
         "1"={
           save(fitNoise,file=paste0("Kids_Braams_ModelFitsNoise_Model",bashInput[2],".RData"))
         },
         "2"={
           save(fitNoise,file=paste0("EarlyAdol_Braams_ModelFitsNoise_Model",bashInput[2],".RData"))
         },
         "3"={
           save(fitNoise,file=paste0("LateAdol_Braams_ModelFitsNoise_Model",bashInput[2],".RData"))
         },
         "4"={
           save(fitNoise,file=paste0("YoungAdul_Braams_ModelFitsNoise_Model",bashInput[2],".RData"))
         },
         "5"={
           save(fitNoise,file=paste0("WholeFit_Braams_ModelFitsNull_Model",bashInput[2],".RData"))
         }
  )#end AgeGroupswitch.
  
} else if(as.numeric(bashInput[2])==4){
  
  # here  i need to define other parameters of interest.
  # Specify the number of parameters and parameters of interest
  numPars <- 4
  POI     <- c("mu_rho", "mu_tau",
               "rho","tau", "log_lik","y_pred")
  
  
  inits<-"fixed"
  # priors
  if (inits[1] != "random") {
    if (inits[1] == "fixed") {
      inits_fixed <- c(0.5, 0.9, 0.0)
    } else {
      if (length(inits) == numPars) {
        inits_fixed <- inits
        # mu_ocu   =rep(inits_fixed[3],2)
      } else {
        stop("Check your inital values!")
      }
    }
    
    genInitList <- function() {
      list(
        #initial values.
        mu_rho   =rep(inits_fixed[1]),
        mu_tau   =rep(inits_fixed[2]),
        
        sigma_rho= rep(c(1.0)),
        sigma_tau= rep(c(1.0)),
        
        rho_p    = rep(inits_fixed[1],numSubjs),
        tau_p    = rep(inits_fixed[2],numSubjs)
        #ocu_p    = matrix(inits_fixed[3],numSubjsG, nGroups)
      )
    }
  } else {
    genInitList <- "random"
  }
  
  fitNull = stan("expUtil_hier_Risk_Braams.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 21000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
  
  
  switch(as.character(bashInput[1]),
         "1"={
           save(fitNull,file=paste0("Kids_Braams_ModelFitsNull_Model",bashInput[2],".RData"))
         },
         "2"={
           save(fitNull,file=paste0("EarlyAdol_Braams_ModelFitsNull_Model",bashInput[2],".RData"))
         },
         "3"={
           save(fitNull,file=paste0("LateAdol_Braams_ModelFitsNull_Model",bashInput[2],".RData"))
         },
         "4"={
           save(fitNull,file=paste0("YoungAdul_Braams_ModelFitsNull_Model",bashInput[2],".RData"))
         },
         "5"={
           save(fitNull,file=paste0("WholeFit_Braams_ModelFitsNull_Model",bashInput[2],".RData"))
         }
  )#end AgeGroupswitch.
  }
