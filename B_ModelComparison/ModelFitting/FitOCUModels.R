library(rstan)
library(shinystan)
#library(here)
bashInput <- commandArgs(trailingOnly = TRUE)
#bashInput<-c(1,1,5,1)



########## BASH INPUT EXPLAINATION
#BashInput 1: Which model am i fitting?
#BashInput 2: Which Model was the Generative Model
#BashInput 3: OCU Value Used for the generative Model
#BashInput 4: DataSet Index Used
#BashInput 5: Risk Attitude Group

#rhomu=1;#set up priors.

if (as.numeric(bashInput[2])==1){
  load(paste0("SimulationFiles/1InfoSocial",bashInput[3],bashInput[4],".RData"))
} else if (as.numeric(bashInput[2])==2){
  load(paste0("SimulationFiles/1RiskSocial",bashInput[3],bashInput[4],".RData"))
}else if(as.numeric(bashInput[2])==3){
  load(paste0("SimulationFiles/1TemperatureSocial",bashInput[3],bashInput[4],".RData"))
}else if (as.numeric(bashInput[2])==4){
  load(paste0("SimulationFiles/1NullSocial",bashInput[3],bashInput[4],".RData"))
}else if (as.numeric(bashInput[2])==5){
  load(paste0("SimulationFiles/1SepSocial",bashInput[3],bashInput[4],".RData"))
}

rstan_options(auto_write = TRUE) 
#ModelParamsFull<-as.data.frame("Simulations")
#########
#########
#########
#todo. transfer Model parameters so i can use it with the STAN model file.


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
  # Before i only used half of the trials. i change this back again. Lets see how it turns out.
  Tsubj[sIdx] <- (length(Simulations[[i]]$subject)) 
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
    risky_payoff[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$valueGamble[1:useTrials]
    condition[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$OtherChoseRisk[1:useTrials]
    p_gamble[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$probGamble[1:useTrials]
    choice[g1, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$ChooseRisk[1:useTrials]
    TsubjG[g1,Simulations[[i]]$group]<-useTrials
    g1=g1+1;
  }else if (Simulations[[i]]$group==2){
    safe_payoff[g2, 1:useTrials, Simulations[[i]]$group]    <- 5
    risky_payoff[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$valueGamble[1:useTrials]
    condition[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$OtherChoseRisk[1:useTrials]
    p_gamble[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$probGamble[1:useTrials]
    choice[g2, 1:useTrials, Simulations[[i]]$group]    <- Simulations[[i]]$ChooseRisk[1:useTrials]
    TsubjG[g2,Simulations[[i]]$group]<-useTrials
    g2=g2+1
  }
}
Tsubj<-TsubjG
# Specify the number of parameters and parameters of interest
numPars <- 3
#what are the parameters of interest, that should be saved in the parameters object?
POI     <- c("mu_rho", "mu_tau", "mu_ocu",
             "rho","ocu","tau", "log_lik")
#get initial values for Risk and Information Model
inits<-"fixed"
# priors
if (inits[1] != "random") {
  if (inits[1] == "fixed") {
    inits_fixed <- data.frame(rho<-seq(pnorm(0.1),pnorm(1.9),length.out=100), 
                              tau<-seq(exp(0.1),exp(0.999),length.out=100), 
                              ocu<-seq((0.1),(4.9),length.out=100)
    )
    rho<-inits_fixed$rho[50]
    tau<-inits_fixed$tau[50]
    ocu<-inits_fixed$ocu[50]
    
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
      mu_rho   =rep(rho,nGroups),
      mu_tau   =rep(tau,nGroups),
      mu_ocu   =rep(ocu,nGroups),

      sigma_rho= rep(c(1.0),nGroups),
      sigma_tau= rep(c(1.0),nGroups),
      sigma_ocuSafe= rep(c(1.0),nGroups),

      rho_p    = matrix(qnorm(rho/2),numSubjsG, nGroups),
      tau_p    = matrix(log(tau),numSubjsG, nGroups),
      ocu_p    = matrix(pnorm(abs(ocu)),numSubjsG, nGroups)
    )
  }
} else {
  genInitList <- "random"
}


#this is the data that gets passed to stan.
dataList <- list(
  N       = numSubjsG,## number of subjects in each group.
  T       = maxTrials,
  G       = nGroups,
  Tsubj   = Tsubj,
  numPars = numPars,
  safe_payoff    = safe_payoff,
  risky_payoff    = risky_payoff,
  condition  = condition, # condition is 0= solo ,1 = safe  ,3 = risky
  p_gamble = p_gamble,
  choice = choice
)

#setup the different modelfitting things.
if(as.numeric(bashInput[1])==1){
  ##################### Fit the Social Information Model
  #####################
  fitInfo = stan("ocu_hier.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 12000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
  save(fitInfo,file=paste0("LongModelFitsInfo",bashInput[1],"_Model",bashInput[2],"OCU_",bashInput[3],"_",bashInput[4],".RData"))
} else if(as.numeric(bashInput[1])==2){
  ################## fit the Reward Sensitivity Model
  ##################
  fitRisk = stan("ocu_hier_Risk.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 12000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta =0.99)
  )
  save(fitRisk,file=paste0("LongModelFitsRisk",bashInput[1],"_Model",bashInput[2],"OCU_",bashInput[3],"_",bashInput[4],".RData"))
}else if(as.numeric(bashInput[1])==3){
  
  ##################### fit the noise Model.
  ##################### Restrict OCU between 1 and 0
  
  if (inits[1] != "random") {
    if (inits[1] == "fixed") {
      inits_fixed <- data.frame(rho<-seq(pnorm(0.1),pnorm(1.9),length.out=100), 
                                tau<-seq(exp(0.01),exp(0.999),length.out=100), 
                                ocu<-seq((0.1),(0.9),length.out=100)
      )
      
      # rho<-sample(inits_fixed$rho,1)
      # tau<-sample(inits_fixed$tau,1)
      # ocu<-sample(inits_fixed$ocu,1)
      
      rho<-inits_fixed$rho[50]
      tau<-inits_fixed$tau[50]
      ocu<-inits_fixed$ocu[50]
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
        mu_rho   =rep(rho,nGroups),
        mu_tau   =rep(tau,nGroups),
        mu_ocu   =rep(ocu,nGroups),
        
        sigma_rho= rep(c(1.0),nGroups),
        sigma_tau= rep(c(1.0),nGroups),
        sigma_ocuSafe= rep(c(1.0),nGroups),
        
        rho_p    = matrix(qnorm(rho/2),numSubjsG, nGroups),
        tau_p    = matrix(log(tau),numSubjsG, nGroups),
        ocu_p    = matrix(ocu,numSubjsG, nGroups)
      )
    }
  } else {
    genInitList <- "random"
  }
  
  
  fitNoise = stan("ocu_hier_Noise.stan",
                  data   = dataList,
                  pars   = POI,
                  init   = genInitList,
                  iter   = 12000,
                  thin   = 3,
                  cores = 3,
                  chains =3,
                  control = list(adapt_delta = 0.99)
  )
  save(fitNoise,file=paste0("LongModelFitsNoise",bashInput[1],"_Model",bashInput[2],"OCU_",bashInput[3],"_",bashInput[4],".RData"))
} else if(as.numeric(bashInput[1])==4){
  
  # here  i need to define other parameters of interest.
  POI     <- c("mu_rho", "mu_tau",
               "rho","tau", "log_lik")
  inits<-"fixed"
  # priors
  if (inits[1] != "random") {
    if (inits[1] == "fixed") {
      inits_fixed <- data.frame(rho<-seq(pnorm(0.1),pnorm(1.9),length.out=100), 
                                tau<-seq(exp(0.01),exp(0.999),length.out=100)
      )
      # rho<-sample(inits_fixed$rho,1)
      # tau<-sample(inits_fixed$tau,1)
      rho<-inits_fixed$rho[50]
      tau<-inits_fixed$tau[50]
      
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
        mu_rho   =rep(rho,nGroups),
        mu_tau   =rep(tau,nGroups),
        sigma_rho= rep(c(1.0),nGroups),
        sigma_tau= rep(c(1.0),nGroups),
        rho_p    = matrix(qnorm(rho/2),numSubjsG, nGroups),
        tau_p    = matrix(log(tau),numSubjsG, nGroups)
        # ocu_p    = matrix(inits_fixed[3],numSubjsG, nGroups)
      )
    }
  } else {
    genInitList <- "random"
  }
  
  fitNull = stan("expUtil_hier.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 12000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta = 0.99)
  )
  save(fitNull,file=paste0("LongModelFitsNull",bashInput[1],"_Model",bashInput[2],"OCU_",bashInput[3],"_",bashInput[4],".RData"))
}else if(as.numeric(bashInput[1])==5){
  # i need to define other initial values and POIs.
  
  # here  i need to define other parameters of interest.
  # Specify the number of parameters and parameters of interest
  numPars <- 4
  POI     <- c("mu_rho", "mu_tau","rho","tau", "log_lik",
               "y_pred","ocuSafe","ocuRisk","mu_ocuSafe","mu_ocuRisk")
  inits<-"fixed"
  
  # priors
  if (inits[1] != "random") {
    if (inits[1] == "fixed") {
      # get my pseudo random initial Values
      inits_fixed <- data.frame(rho<-seq(pnorm(0.1),pnorm(1.9),length.out=100), 
                                tau<-seq(exp(0.01),exp(0.999),length.out=100), 
                                ocuSafe<-seq((0.1),(4.9),length.out=100),
                                ocuRisk<-seq((0.1),(4.9),length.out=100)
      )
      
      # rho<-sample(inits_fixed$rho,1)
      # tau<-sample(inits_fixed$tau,1)
      # ocuSafe<-sample(inits_fixed$ocuSafe,1)
      # ocuRisk<-sample(inits_fixed$ocuRisk,1)
      
      rho<-inits_fixed$rho[50]
      tau<-inits_fixed$tau[50]
      ocuSafe<-inits_fixed$ocuSafe[50]
      ocuRisk<-inits_fixed$ocuRisk[50]
      
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
        mu_rho   =rep(rho,nGroups),
        mu_tau   =rep(tau,nGroups),
        mu_ocuSafe   =rep(ocuSafe,nGroups),
        mu_ocuRisk   =rep(ocuRisk,nGroups),
        
        sigma_rho= rep(c(1.0),nGroups),
        sigma_tau= rep(c(1.0),nGroups),
        sigma_ocuSafe= rep(c(1.0),nGroups),
        sigma_ocuRisk= rep(c(1.0),nGroups),
        
        rho_p    = matrix(qnorm(rho/2),numSubjsG, nGroups),
        tau_p    = matrix(log(tau),numSubjsG, nGroups),
        ocuSafe_p    = matrix(pnorm(abs(ocuSafe)),numSubjsG, nGroups),
        ocuRisk_p    = matrix(pnorm(abs(ocuRisk)),numSubjsG, nGroups)
        # ocu_p    = matrix(inits_fixed[3],numSubjsG, nGroups)
      )
    }
  } else {
    genInitList <- "random"
  }
  
  fitRisk = stan("ocu_hier_Sep.stan",
                 data   = dataList,
                 pars   = POI,
                 init   = genInitList,
                 iter   = 12000,
                 thin   = 3,
                 cores = 3,
                 chains =3,
                 control = list(adapt_delta =0.99)
  )
  save(fitRisk,file=paste0("LongModelFitsSep",bashInput[1],"_Model",bashInput[2],"OCU_",bashInput[3],"_",bashInput[4],".RData"))
}
