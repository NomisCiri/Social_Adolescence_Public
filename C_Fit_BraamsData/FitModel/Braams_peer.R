# In this script i fit the expected utility model to the peers.
library(rstan)
library(dplyr)
library(here)
library(foreach)
rstan_options(auto_write = TRUE) 
bashInput <- commandArgs(trailingOnly = TRUE)
bashInput<-c(2,3,4)
library(doMC)
registerDoMC(9)

foreach (pff = 1:length(bashInput)) %dopar% {

#setwd(paste0(here(),"/FitModel"))#this is for debugging.
# In this script i fit the different OCU models with different hyperdistributions per age.
# This is like fitting a random slope per age in a glmer.
# FOR THIS IT SI IMPORANT THAT LENGTH(AGEGROUPS) IS ADDED IN YOUR JOB-WRAPPER ON THE CLUSTER
# so it reflects the different ages appropreately.
#load Braams Data.
ModelParamsFull<-read.csv("RawData/PG_raw_data_combined_cleaned_Wouter_with_peer.csv")
#bashInput<-2
#########
#########
#########
#todo. transfer Model parameters so i can use it with the STAN model file.
#as.numeric(ModelParamsFull$peer)
## other than in the script before, i here fit only the expected utility model to 3 peers.
ModelParamsFull<- ModelParamsFull%>%dplyr::mutate(
  choice = case_when(
    condition1=="solo" ~ 2,# i dont need this but i restricted the numbers in stan between 1 and 3
    condition1=="socialrisky" ~ 1,# risky choices are coded as 3 in my stan code
    condition1=="socialsafe" ~ 0,# safe choices are coded as 1 in my stan code
    TRUE~0 # keep the rest.
  ),#end PeerChoice.
  typeRA = case_when(
    Ambiguity>0~0,# AMBIGUITY IS RECODED AS 0
    Ambiguity==0~1,# RISK IS RECODED AS 1
    TRUE~0
  ),Agegroup = case_when(
    # i dont need this now but i needed it before. 
    Age.bins=="12-13"~1,# AMBIGUITY IS RECODED AS 0
    Age.bins=="14-15"~2,# RISK IS RECODED AS 1
    Age.bins=="16-17"~2,
    Age.bins=="16-17"~3,
    Age.bins=="18-19"~3,
    Age.bins=="20-21"~4,
    Age.bins=="22"~4,
    TRUE~0
  ),EVDiff= case_when(
    Ambiguity>0 ~ ((0.5*Vhighrisky)+((1-0.5)*Vlowrisky))-((0.5*Vhighsafe)+((1-0.5)*Vlowsafe)),#calculate ambiguity neutral expected value
    Ambiguity==0 ~ ((Phigh*Vhighrisky)+((1-Phigh)*Vlowrisky))-((Phigh*Vhighsafe)+((1-Phigh)*Vlowsafe))#calculate risk expected value
  )# end Riskamb
  # end Riskamb
)
#ModelParamsFull<-ModelParamsFull[ModelParamsFull$typeRA=="1",]# keep only The Risk trails.
ModelParamsFull<-ModelParamsFull%>%arrange(Age.bins)# i order it first so i can make sure that 
Agegroups<-unique(ModelParamsFull$Age.bins)# for indexing my agegroups.
peerList<-as.character(unique(ModelParamsFull$peer))

ModelParamsFull<-ModelParamsFull%>%dplyr::filter(peer == peerList[as.numeric(bashInput[pff])])#select the agents
# now check how many participants we have.
as.numeric(ModelParamsFull$peer)
#change colname of subject into subjID
numSubjs<-length(unique(ModelParamsFull$OSFparticipantID))#Total Number of Subs
subjList <- unique(ModelParamsFull$OSFparticipantID)######

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

condition    <- array(0, c(numSubjs, maxTrials))#Other Chose Risk?
p_gamble    <- array(0, c(numSubjs, maxTrials))
choice  <- array(0, c(numSubjs, maxTrials))

safe_Hpayoff<- array(0, c(numSubjs, maxTrials))
safe_Lpayoff<- array(0, c(numSubjs, maxTrials))
risky_Hpayoff<- array(0, c(numSubjs, maxTrials))
risky_Lpayoff<- array(0, c(numSubjs, maxTrials))

risk1Ambig0  <- array(0, c(numSubjs, maxTrials))
ambigLevel  <- array(0, c(numSubjs, maxTrials))
#Agegroup<- array(0, c(numSubjs, maxTrials))

# generate the data Lists to be passed to stan
# concatenate different groups in the third dimension.

for (i in 1:numSubjs) {
  curSubj      <- subjList[i]
  useTrials    <- Tsubj[i]
  condition[i, 1:useTrials]    <- 2
  p_gamble[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Phigh
  choice[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$choice
  
  safe_Hpayoff[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vhighsafe
  safe_Lpayoff[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vlowsafe
  risky_Hpayoff[i, 1:useTrials]    <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vhighrisky
  risky_Lpayoff[i, 1:useTrials] <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Vlowrisky
  
  risk1Ambig0[i, 1:useTrials] <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$typeRA
  ambigLevel[i, 1:useTrials] <- ModelParamsFull[ModelParamsFull$OSFparticipantID==curSubj,]$Ambiguity
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
  safe_Hpayoff    = safe_Hpayoff,
  safe_Lpayoff    = safe_Lpayoff,
  risky_Hpayoff    = risky_Hpayoff,
  risky_Lpayoff = risky_Lpayoff,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gamble,
  choice = choice,
  risk1Ambig0=risk1Ambig0,
  ambigLevel=ambigLevel
 # agegroup=agegroup
  
)

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
  
  fitAgent<- stan("Model_Code/expUtil_hier_RiskAmb_Braams.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 20000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
    save(fitAgent,file=paste0("../Model_Comparison/Agent_Braams_ModelFits", as.character(peerList[as.numeric(bashInput[pff])]),"_Model.RData"))
}
