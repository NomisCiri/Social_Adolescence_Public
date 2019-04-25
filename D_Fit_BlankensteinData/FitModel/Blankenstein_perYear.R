library(rstan)
library(shinystan)
library(dplyr)
library(plyr)
rstan_options(auto_write = TRUE) 
bashInput <- commandArgs(trailingOnly = TRUE)


# In this script i fit the different OCU models with different hyperdistributions per age.
# This is like fitting a random slope per age in a glmer.
# FOR THIS IT SI IMPORANT THAT LENGTH(AGEGROUPS) IS ADDED IN YOUR JOB-WRAPPER ON THE CLUSTER
# so it reflects the different ages appropreately.
#load Blankensteins Data.
load("../RawData/GambleData.RData")
#bashInput<-c(1,5)
# Get actual Age and not only Agegroup.
AgeAndMore<-read.csv2("../RawData/Rest.csv")# This is a df with nusiance. and all the other fun.
AgeAndMore$Age<-round(as.numeric(as.character(AgeAndMore$Age)))
#Arrange it correctly and merge so that i can glue it together later.
ModelParamsFull<-merge(ModelParamsFull,AgeAndMore , by.x="ppn", by.y="Subject")%>%arrange(Age)
Agegroups<-unique(ModelParamsFull$Age)# for indexing my agegroups.
ModelParamsFull<-ModelParamsFull%>%dplyr::filter(Age==Agegroups[as.numeric(bashInput[1])])#select only one Agegroup
#change colname of subject into subjID
numSubjs<-length(unique(ModelParamsFull$ppn))#Total Number of Subs
subjList <- unique(ModelParamsFull$ppn)
#first i run through every entry and replace subnr with the index.
# i need to recode the "conditions" so they fit into stan. maybe it
## do the same recoding for typeRA
ModelParamsFull<- ModelParamsFull%>%dplyr::mutate(
  PeerChoiceSafe0_Risk1 = case_when(
    PeerChoiceSafe0_Risk1==99 ~ 2,# i dont need this but i restricted the numbers in stan between 1 and 3 Its the solo Condition.
    PeerChoiceSafe0_Risk1==1 ~ 3,# risky choices are coded as 3 in my stan code
    PeerChoiceSafe0_Risk1==0 ~ 1,# safe choices are coded as 1 in my stan code
    TRUE~0 # keep the rest.
  ),#end PeerChoice.
  typeRA = case_when(
    typeRA=='A'~0,# AMBIGUITY IS RECODED AS 0
    typeRA=='R'~1,# RISK IS RECODED AS 1
    TRUE~0
  )# end Riskamb
)
#ModelParamsFull<-ModelParamsFull[ModelParamsFull$typeRA==1,]# keep only The Risk trails.
####### number of trials and see which group he is in for each subject######
Tsubj <- as.vector(rep(0, numSubjs)) 
GSubj <- as.vector(rep(0, numSubjs)) 
for (sIdx in 1:numSubjs)  {
  curSubj     <- subjList[sIdx]
  Tsubj[sIdx] <- length(ModelParamsFull[ModelParamsFull$ppn==curSubj,]$ppn)  # How many entries per Subject?
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
risk1Ambig0  <- array(0, c(numSubjs, maxTrials))
ambigLevel  <- array(0, c(numSubjs, maxTrials))
#Agegroup<- array(0, c(numSubjs, maxTrials))

# generate the data Lists to be passed to stan
# concatenate different groups in the third dimension.

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  safe_payoff[i, 1:useTrials] <- 5
  risky_payoff[i, 1:useTrials]<- ModelParamsFull[ModelParamsFull$ppn==curSubj,]$valueGamble
  condition[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$ppn==curSubj,]$PeerChoiceSafe0_Risk1
  p_gamble[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$ppn==curSubj,]$probAv
  choice[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$ppn==curSubj,]$choice
  risk1Ambig0[i, 1:useTrials] <- ModelParamsFull[ModelParamsFull$ppn==curSubj,]$typeRA
  ambigLevel[i, 1:useTrials] <- ModelParamsFull[ModelParamsFull$ppn==curSubj,]$ambigLev
}

# Specify the number of parameters and parameters of interest
numPars <- 4
POI     <- c("mu_rho", "mu_tau", "mu_ocu","mu_beta","beta",
             "rho","ocu","tau", "log_lik","y_pred")


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
      mu_beta   =rep(inits_fixed[1]),
      mu_tau   =rep(inits_fixed[2]),
      mu_ocu   =c(0.1),
      
      sigma_rho= rep(c(1.0)),
      sigma_beta= rep(c(1.0)),
      sigma_tau= rep(c(1.0)),
      sigma_ocu= rep(c(1.0)),
      
      rho_p    = rep(qnorm(inits_fixed[1]/2),numSubjs),
      beta_p    = rep(qnorm(inits_fixed[1]/2),numSubjs),
      tau_p    = rep(log(inits_fixed[2]),numSubjs),
      ocu_p    = rep(0.1,numSubjs)
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
  safe_payoff    = safe_payoff,
  risky_payoff    = risky_payoff,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gamble,
  choice = choice,
  risk1Ambig0=risk1Ambig0,
  ambigLevel=ambigLevel
 # agegroup=agegroup
)

#setup the different modelfitting things.
if(as.numeric(bashInput[2])==1){
  fitInfo = stan("../Model_Code/ocu_hier_InfoRiskAmb_Blank.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 20000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
        save(fitInfo,file=paste0("../Model_Comparison/Age",Agegroups[bashInput[1]],"_Blank_ModelFitsInfo_Model",bashInput[2],".RData"))
       } else if(as.numeric(bashInput[2])==2){
  fitRisk = stan("../Model_Code/ocu_hier_RiskRiskAmb_Blank.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 20000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta =0.99)
  )
  
           save(fitRisk,file=paste0("../Model_Comparison/Age",Agegroups[as.numeric(bashInput[1])],"_Blank_ModelFitsRisk_Model",bashInput[2],".RData"))

}else if(as.numeric(bashInput[2])==3){
  fitNoise = stan("../Model_Code/ocu_hier_NoiseRiskAmb_Blank.stan",
                  data   = dataList,
                  pars   = POI,
                  init   = genInitList,
                  iter   = 20000,
                  thin   = 3,
                  cores = 3,
                  chains =3,
                  control = list(adapt_delta = 0.99)
  )
 
           save(fitNoise,file=paste0("../Model_Comparison/Age",Agegroups[as.numeric(bashInput[1])],"_Blank_ModelFitsNoise_Model",bashInput[2],".RData"))
        ###
  #################################
  ################################
  ################################
  ###############################
} else if(as.numeric(bashInput[2])==4){
  
  # here  i need to define other parameters of interest.
  # Specify the number of parameters and parameters of interest
  numPars <- 4
  POI     <- c("mu_rho", "mu_tau","mu_beta","beta",
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
        mu_beta   =rep(inits_fixed[1]),
        mu_tau   =rep(inits_fixed[2]),
        
        sigma_rho= rep(c(1.0)),
        sigma_beta= rep(c(1.0)),
        sigma_tau= rep(c(1.0)),
        
        rho_p    = rep(qnorm(inits_fixed[1]/2),numSubjs),
        beta_p    = rep(qnorm(inits_fixed[1]/2),numSubjs),
        tau_p    = rep(log(inits_fixed[2]),numSubjs)
        #ocu_p    = matrix(inits_fixed[3],numSubjsG, nGroups)
      )
    }
  } else {
    genInitList <- "random"
  }
  
  fitNull = stan("../Model_Code/expUtil_hier_RiskAmb_Blank.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 20000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
  save(fitNull,file=paste0("../Model_Comparison/Age",Agegroups[as.numeric(bashInput[1])],"_Blank_ModelFitsNull_Model",bashInput[2],".RData"))
  
}else if(as.numeric(bashInput[2])==5){
  
  # here  i need to define other parameters of interest.
  # Specify the number of parameters and parameters of interest
  numPars <- 4
  POI     <- c("mu_rho", "mu_tau","mu_beta","ocuSafe","ocuRisk","mu_ocuSafe","mu_ocuRisk","beta",
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
        mu_beta   =rep(inits_fixed[1]),
        mu_tau   =rep(inits_fixed[2]),
        mu_ocuRisk=rep(inits_fixed[3]),
        mu_ocuSafe= rep(inits_fixed[3]),
        
        sigma_rho= rep(c(1.0)),
        sigma_beta= rep(c(1.0)),
        sigma_tau= rep(c(1.0)),
        sigma_ocuSafe=1.0,
        sigma_ocuSafe=1.0,
        
        rho_p    = rep(qnorm(inits_fixed[1]/2),numSubjs),
        beta_p    = rep(qnorm(inits_fixed[1]/2),numSubjs),
        tau_p    = rep(log(inits_fixed[2]),numSubjs),
        ocu_pSafe    = rep(inits_fixed[3],numSubjs),
        ocu_pRisk    = rep(inits_fixed[3],numSubjs)
        
      )
    }
  } else {
    genInitList <- "random"
  }
  
  fitSep = stan("../Model_Code/ocu_hier_InfoSeperateRiskAmb_Blank.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 20000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
           save(fitSep,file=paste0("../Model_Comparison/Age",Agegroups[as.numeric(bashInput[1])],"_Blank_ModelFitsSep_Model",bashInput[2],".RData"))
}


