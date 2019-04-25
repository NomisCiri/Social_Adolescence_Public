# this function takes as input the parameters which has been fitted for the AlphaRiskUtilModel
# and simulates some (i) responses
#  this one is for solo choices.
#Here i use the parameters of the noocumodel and fit it to the ocu model.
#if you fitted 
plotProbFunRisk <- function(data) {
  prob=rep(seq(0.001,1,by=0.001))# make an artificial probability vector, with which you can create a fine grained function. and replicate it for each possible gamble
  GambleValue=c(rep(50,1000))#make an helper vector so you can just iterate through one loop.
 # Ambig<-unique(data$ambigLev)#An ambigous Vector
  forPlot=data.frame(prob,GambleValue)
  Reference=5
  #here you find the values.
  alpha=unique(data$PostMeanRho)
  theta=unique(data$PostMeanTau)
  beta=unique(data$PostMeanBeta)
  ocu=unique(data$PostMeanPsiRisk)
  ocuSafe=unique(data$PostMeanPsiSafe)
  forPlot$Age=unique(data$Group)
  forPlot$ppn=unique(data$ppn)
  
  
  alpha_Agent=1.44
  theta_Agent=0.5
  
  for (i in 1:length(GambleValue)){
    #linear model (Tymula)
    # normal choice
    forPlot$Util_SureLonely[i] <- 1*(Reference^alpha)
    forPlot$Util_RiskLonely[i] <- ((forPlot$prob[i])*(forPlot$GambleValue[i]^alpha))
    # choice rule
    forPlot$ProbChooseRiskLonely[i] <- 1/(1 + exp(-(forPlot$Util_RiskLonely[i]- forPlot$Util_SureLonely[i])*theta))
  }
  #  data
  
  #social - find out if we have an risky advice and add the ocu to this option, too... 
  for (i in 1:length(GambleValue)){
    #linear model (Tymula)
    # normal choice
    forPlot$Util_SureSocial[i] <- 1*(Reference^alpha)
    forPlot$Util_RiskSocial[i] <- ((forPlot$prob[i])*(forPlot$GambleValue[i]^alpha))+ocu
    # choice rule
    forPlot$ProbChooseRiskSocial[i] <- 1/(1 + exp(-( forPlot$Util_RiskSocial[i]- forPlot$Util_SureSocial[i])*theta))
    #data
  }
  
  #social - find out if we have an safe advice and add the ocu to this option, too... 
  for (i in 1:length(GambleValue)){
    #linear model (Tymula)
    # normal choice
    forPlot$Util_SureSocialSafe[i] <- 1*(Reference^alpha)+ocuSafe
    forPlot$Util_RiskSocialSafe[i] <- ((forPlot$prob[i])*(forPlot$GambleValue[i]^alpha))
    # choice rule
    forPlot$ProbChooseRiskSocialSafe[i] <- 1/(1 + exp(-( forPlot$Util_RiskSocialSafe[i]- forPlot$Util_SureSocialSafe[i])*theta))
    #data
  }
  
  #plot the function for the recovered agent  
  for (i in 1:length(GambleValue)){
    #linear model (Tymula)
    # normal choice
    forPlot$Util_SureAgent[i] <- 1*(Reference^alpha_Agent)
    forPlot$Util_RiskAgent[i] <- ((forPlot$prob[i])*(forPlot$GambleValue[i]^alpha_Agent))
    # choice rule
    forPlot$ProbChooseRiskAgent[i] <- 1/(1 + exp(-(forPlot$Util_RiskAgent[i]- forPlot$Util_SureAgent[i])/theta_Agent))
    #forPlot$Age[i]<-99
  }
  
  
  #library(ggplot2)
  library(tidyr)# coooolio this one really works for making wide data
  forPlot$deltaEV=(forPlot$prob*forPlot$GambleValue)-Reference
  #ggplot(forPlot,(aes(x=deltaEV,y=ProbChooseRiskLonely)))+
  #  geom_line()
  # facet_grid(~GambleValue) 
  forPlot.long<-gather(forPlot, condition, measurement, ProbChooseRiskLonely, ProbChooseRiskSocial, ProbChooseRiskAgent,ProbChooseRiskSocialSafe)# this really works. oh wow. how many hours of my life wasted.
  #ggplot(forPlot.long,(aes(x=deltaEV,y=measurement,color=condition)))+
  #  geom_line()
  return(forPlot.long)
}
