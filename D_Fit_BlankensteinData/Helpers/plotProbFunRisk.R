# this function takes as input the parameters which has been fitted for the AlphaRiskUtilModel
# and simulates some (i) responses
#  this one is for solo choices.
#Here i use the parameters of the noocumodel and fit it to the ocu model.
#if you fitted 
plotProbFunRisk <- function(data) {
  library(tidyr)# coooolio this one really works for making wide data
  prob=rep(seq(0.001,1,by=0.001))# make an artificial probability vector, with which you can create a fine grained function. and replicate it for each possible gamble
  GambleValue=50#make an helper vector so you can just iterate through one loop.
  forPlot=expand.grid("prob"=prob,
                      "GambleValue"=GambleValue,
                      "Age"=unique(data$Group),
                      "ppn"=unique(data$ppn)
                      )
  Reference=5#the Sure value / Certainty Equivalent
  
  #This
  alpha=unique(data$PostMeanRho)
  theta=unique(data$PostMeanTau)
  beta=unique(data$PostMeanBeta)
  ocu=unique(data$PostMeanPsiRisk)
  ocuSafe=unique(data$PostMeanPsiSafe)
  #define th Agents Parameters.
  alpha_Agent=1.44
  theta_Agent=0.5
  
    #Solo Utilites.
    forPlot$Util_SureLonely <- 1*(Reference^alpha)
    forPlot$Util_RiskLonely <- ((forPlot$prob)*(forPlot$GambleValue^alpha))
    # Social Risky Advice Utilites
    forPlot$Util_SureSocial <- 1*(Reference^alpha)
    forPlot$Util_RiskSocial <- ((forPlot$prob)*(forPlot$GambleValue^alpha))+ocu
    # Social Safe Advice Utilites
    forPlot$Util_SureSocialSafe<- 1*(Reference^alpha)+ocuSafe
    forPlot$Util_RiskSocialSafe<- ((forPlot$prob)*(forPlot$GambleValue^alpha))
    # Agent Utilities
    forPlot$Util_SureAgent<- 1*(Reference^alpha_Agent)
    forPlot$Util_RiskAgent<- ((forPlot$prob)*(forPlot$GambleValue^alpha_Agent))
    
    # choice rules
    forPlot$ProbChooseRiskAgent<- 1/(1 + exp(-(forPlot$Util_RiskAgent- forPlot$Util_SureAgent)/theta_Agent))
    forPlot$ProbChooseRiskSocialSafe <- 1/(1 + exp(-( forPlot$Util_RiskSocialSafe- forPlot$Util_SureSocialSafe)*theta))
    forPlot$ProbChooseRiskSocial <- 1/(1 + exp(-( forPlot$Util_RiskSocial- forPlot$Util_SureSocial)*theta))
    forPlot$ProbChooseRiskLonely <- 1/(1 + exp(-(forPlot$Util_RiskLonely- forPlot$Util_SureLonely)*theta))

  forPlot$deltaEV=(forPlot$prob*forPlot$GambleValue)-Reference
  # Retrun Stuff in Long Dataformat.
  forPlot.long<-gather(forPlot, condition, measurement, ProbChooseRiskLonely, ProbChooseRiskSocial, ProbChooseRiskAgent,ProbChooseRiskSocialSafe)# this really works. oh wow. how many hours of my life wasted.
  return(forPlot.long)
}
