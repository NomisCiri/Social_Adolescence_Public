# this function takes as input the parameters which has been fitted for the AlphaRiskUtilModel
# and simulates some (i) responses
#  this one is for solo choices.
#Here i use the parameters of the noocumodel and fit it to the ocu model.
#if you fitted 
plotProbFunRiskBraams <- function(data) {
  library(tidyr)# This i need to make Tidy Data later on
  
  prob=seq(0, 1, by=0.001)# make an artificial probability vector, with which you can create a fine grained function. and replicate it for each possible gamble
  
  ######## These are the Stimuli Used in Braams Dataset. 
  ########You Can play around with this. I used Maximum Valuerange to compute the Utility functions.
  Vlowsafe=min(c(1.52, 2.30, 2.66, 2.49, 2.14, 2.31, 1.80, 2.52)*10)
  Vhighsafe=min(c(2.08, 2.44, 2.83, 2.55, 2.67, 3.32, 1.96, 2.65)*10)
  VhighRisky=max(c(3.74, 5.11, 5.55, 5.08, 5.16, 5.68, 3.86, 5.53)*10)
  VlowRisky=max(c(0.11, 0.12, 0.16, 0.13, 0.14, 0.17, 0.09)*10)
  
  #Here i make a df containing the things i need for plotting. 
  #The nice thing about R is that i dont need to do loop but can just manipulate the df directly.
  # This immensly speeds up things.
  forPlot=expand.grid("prob"=prob,
                      "Vlowsafe"=Vlowsafe,
                      "Vhighsafe"=Vhighsafe,
                      "VhighRisky"=VhighRisky,
                      "VlowRisky"=VlowRisky,
                      "Age"=unique(data$Group),
                      "ppn"=unique(data$ppn)
  )
  
  
  #here you find the values. The Function takes a Dataframe of one subject as Input and eextracts everything.
  alpha=unique(data$PostMeanRho)
  theta=unique(data$PostMeanTau)
  beta=unique(data$PostMeanBeta)
  ocu=unique(data$PostMeanPsiRisk)*10
  ocuSafe=unique(data$PostMeanPsiSafe)*10
  
  alphaAR=1.14
  alphaAS=0.328
  alphaARN=0.99
  
  thetaAR=1.02
  thetaAS=2.67
  thetaARN=2.67
  
  
  betaAR=-0.319
  betaAS=0.661
  betaRN=0.661
  
  ############ Compute expected Utilities Lonely
  forPlot$Util_SureLonely <- forPlot$prob * forPlot$Vhighsafe^alpha  + (1-forPlot$prob) * forPlot$Vhighsafe^alpha;
  forPlot$Util_RiskLonely <- forPlot$prob * forPlot$VhighRisky^alpha  + (1-forPlot$prob) * forPlot$VlowRisky^alpha;
  ############ Prepare Utilities with Risky Advice
  forPlot$Util_SureSocial <- forPlot$prob* forPlot$Vhighsafe^alpha  + (1-forPlot$prob) * forPlot$Vhighsafe^alpha;
  forPlot$Util_RiskSocial <- forPlot$prob * forPlot$VhighRisky^alpha  + (1-forPlot$prob) * forPlot$VlowRisky^alpha;
  ########### Prepare Utilities with Safe Advice
  forPlot$Util_SureSocialSafe <- forPlot$prob * forPlot$Vhighsafe^alpha  + (1-forPlot$prob) * forPlot$Vhighsafe^alpha;
  forPlot$Util_RiskSocialSafe <- forPlot$prob * forPlot$VhighRisky^alpha  + (1-forPlot$prob) * forPlot$VlowRisky^alpha;

  ############ Add Socail Utilities.
  forPlot$Util_SureSocialSafe=forPlot$Util_SureLonely+ocuSafe
  forPlot$Util_RiskSocial=forPlot$Util_RiskLonel+ocu
  
  # choice rules
  forPlot$ProbChooseRiskSocialSafe<- 1/(1 + exp(-( forPlot$Util_RiskSocialSafe- forPlot$Util_SureSocialSafe)*theta))
  forPlot$ProbChooseRiskSocial <- 1/(1 + exp(-( forPlot$Util_RiskSocial- forPlot$Util_SureSocial)*theta))
  forPlot$ProbChooseRiskLonely <- 1/(1 + exp(-(forPlot$Util_RiskLonely- forPlot$Util_SureLonely)*theta))
  #data
  
  #Agent1
  forPlot$Util_SureAgentR <- forPlot$prob * forPlot$Vhighsafe^alphaAR  + (1-forPlot$prob) * forPlot$Vhighsafe^alphaAR;
  forPlot$Util_RiskAgentR <- forPlot$prob * forPlot$VhighRisky^alphaAR  + (1-forPlot$prob) * forPlot$VlowRisky^alphaAR;
  forPlot$ProbChooseRiskAgentR <- 1/(1 + exp(-(forPlot$Util_RiskAgentR- forPlot$Util_SureAgentR)*thetaAR))
  
  #Agent2
  forPlot$Util_SureAgentS <- forPlot$prob * forPlot$Vhighsafe^alphaAS  + (1-forPlot$prob) * forPlot$Vhighsafe^alphaAS;
  forPlot$Util_RiskAgentS <- forPlot$prob * forPlot$VhighRisky^alphaAS  + (1-forPlot$prob) * forPlot$VlowRisky^alphaAS;
  forPlot$ProbChooseRiskAgentS <- 1/(1 + exp(-(forPlot$Util_RiskAgentS- forPlot$Util_SureAgentS)*thetaAS))
  
  #Agent3
  forPlot$Util_SureAgentARN <- forPlot$prob * forPlot$Vhighsafe^alphaARN  + (1-forPlot$prob) * forPlot$Vhighsafe^alphaARN;
  forPlot$Util_RiskAgentARN <- forPlot$prob * forPlot$VhighRisky^alphaARN  + (1-forPlot$prob) * forPlot$VlowRisky^alphaARN;
  forPlot$ProbChooseRiskAgentARN <- 1/(1 + exp(-(forPlot$Util_RiskAgentARN- forPlot$Util_SureAgentARN)*thetaARN))
  
  forPlot$deltaEV=(forPlot$prob * forPlot$Vhighsafe  + (1-forPlot$prob) * forPlot$Vhighsafe)  -  (forPlot$prob * forPlot$VlowRisky  + (1-forPlot$prob) * forPlot$Vlowsafe) 
  #ggplot(forPlot,(aes(x=deltaEV,y=ProbChooseRiskLonely)))+
  #  geom_line()
  # facet_grid(~GambleValue) 
  forPlot.long<-gather(forPlot, condition, measurement, ProbChooseRiskLonely, ProbChooseRiskSocial,ProbChooseRiskSocialSafe,ProbChooseRiskAgentS,ProbChooseRiskAgentR,ProbChooseRiskAgentARN)# this really works. oh wow. how many hours of my life wasted.
  #ggplot(forPlot.long,(aes(x=deltaEV,y=measurement,color=condition)))+
  #  geom_line()
  return(forPlot.long)
}

plotProbFunRiskAgentBraams <- function(alpha, theta) {
  alpha=alpha
  theta=theta
  library(tidyr)# This i need to make Tidy Data later on
  prob=seq(0, 1, by=0.001)# make an artificial probability vector, with which you can create a fine grained function. and replicate it for each possible gamble
  ######## These are the Stimuli Used in Braams Dataset. 
  ########You Can play around with this. I used Maximum Valuerange to compute the Utility functions.
  Vlowsafe=min(c(1.52, 2.30, 2.66, 2.49, 2.14, 2.31, 1.80, 2.52)*10)
  Vhighsafe=min(c(2.08, 2.44, 2.83, 2.55, 2.67, 3.32, 1.96, 2.65)*10)
  VhighRisky=max(c(3.74, 5.11, 5.55, 5.08, 5.16, 5.68, 3.86, 5.53)*10)
  VlowRisky=max(c(0.11, 0.12, 0.16, 0.13, 0.14, 0.17, 0.09)*10)
  
  #Here i make a df containing the things i need for plotting. 
  #The nice thing about R is that i dont need to do loop but can just manipulate the df directly.
  # This immensly speeds up things.
  forPlot=expand.grid("prob"=prob,
                      "Vlowsafe"=Vlowsafe,
                      "Vhighsafe"=Vhighsafe,
                      "VhighRisky"=VhighRisky,
                      "VlowRisky"=VlowRisky
  )
  ########## You probably also want to plot the Agents Utility Function. 
  forPlot$Util_SureAgent <- forPlot$prob * forPlot$Vhighsafe^alpha  + (1-forPlot$prob) * forPlot$Vhighsafe^alpha;
  forPlot$Util_RiskAgent <- forPlot$prob * forPlot$VhighRisky^alpha  + (1-forPlot$prob) * forPlot$VlowRisky^alpha;
  forPlot$ProbChooseRiskAgent <- 1/(1 + exp(-(forPlot$Util_RiskAgent- forPlot$Util_SureAgent)*theta))
  
  # I add the Difference in expected Values.
  forPlot$deltaEV=(forPlot$prob * forPlot$Vhighsafe  + (1-forPlot$prob) * forPlot$Vhighsafe)  -  (forPlot$prob * forPlot$VlowRisky  + (1-forPlot$prob) * forPlot$Vlowsafe) 
  #And make long Data
  forPlot.long<-gather(forPlot, condition, measurement, ProbChooseRiskAgent)# this really works. oh wow. how many hours of my life wasted.
}
