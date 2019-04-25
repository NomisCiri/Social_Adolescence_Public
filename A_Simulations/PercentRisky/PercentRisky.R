# here i peek into the change in percent risky choice given that all model parameters
# except ocu remained constant.

library(tidyverse)
library(here)
library(sigmoid)
library(gganimate)

setwd(paste0(here(),"/ModelNoise"))
load(paste0("TemperatureSocial01.RData"));

df<-as.data.frame(do.call(cbind,Simulations[[1]]))
#df$mean<-mean(df$ChooseRisk)
for (j in 1:5){
  for(i in 2:length(Simulations)){#subjectloop
    dfH<-as.data.frame(do.call(cbind,Simulations[[i]]))
    df<-rbind(df,dfH)
  }
  load(paste0("TemperatureSocial",j,"1.RData"))#experimentloop
}


labels<-c("RiskyAdvisor","SafeAdvisor")
labels<-c('8'="social",'10'="solo")

# this works for the temperature models.
df %>% group_by(test_part,ocu,RiskSafeAdv)%>% # ok krass
  summarise( mean = mean(ChooseRisk) ) %>%
  ggplot(aes(x=as.numeric(ocu),y=mean,color=as.factor(RiskSafeAdv)))+
  geom_jitter(aes(x=ocu,y=mean),width =1)+
  #geom_line()+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  #geom_point(aes(x=ocu,y=as.numeric(mean(ChooseRisk))))+
  scale_x_continuous(name="OCU_Value")+
  scale_y_continuous(name="Percent Risky Choice")+
  scale_color_discrete(name="Safe Or Risky Other",breaks=c(1,2),labels=c("RiskyAdvisor","SafeAdvisor"))+
  scale_linetype_discrete(name="Generative Distribution", breaks=c(1,2),labels=c("Low OCU", "High OCU"))+
  guides(fill=F)+facet_grid(cols = vars(test_part),labeller = labeller(test_part = labels) )+
  ggtitle("Safe & Risky Advisor Effect of OCU on Temperature Parameter")+my_theme


##########

setwd(paste0(here(),"/ModelRisky"))
load(paste0("RiskSocial01.RData"));
df<-as.data.frame(do.call(cbind,Simulations[[1]]))
#df$mean<-mean(df$ChooseRisk)
for (j in 1:5){
  for(i in 2:length(Simulations)){#subjectloop
    dfH<-as.data.frame(do.call(cbind,Simulations[[i]]))
    df<-rbind(df,dfH)
  }
  load(paste0("RiskSocial",j,"1.RData"))#experimentloop
}

labels<-c("RiskyAdvisor","SafeAdvisor")
labels<-c('8'="social",'10'="solo")

df %>% group_by(test_part,ocu,RiskSafeAdv)%>% # ok krass
  summarise( mean = mean(ChooseRisk) ) %>%
  ggplot(aes(x=as.numeric(ocu),y=mean,color=as.factor(RiskSafeAdv),fill=as.factor(RiskSafeAdv)))+
  geom_point(aes(x=ocu,y=mean))+
  #geom_line()+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_x_continuous(name="OCU_Value")+
  scale_y_continuous(name="Percent Risky Choice")+
  scale_color_discrete(name="Safe Or Risky Other",breaks=c(1,2),labels=c("RiskyAdvisor","SafeAdvisor"))+
  guides(fill=F)+facet_grid(cols = vars(test_part),labeller = labeller(test_part = labels) )+
  ggtitle("RISK Safe & Risky Advisor Effect of OCU ")+my_theme




##########

setwd(paste0(here(),"/ModelInfo"))
load(paste0("InfoSocial01.RData"));
#here i load all my simulations and concatanete them all in the same array.
df<-as.data.frame(do.call(cbind,Simulations[[1]]))
#df$mean<-mean(df$ChooseRisk)
for (j in 1:5){
  for(i in 2:length(Simulations)){#subjectloop
    dfH<-as.data.frame(do.call(cbind,Simulations[[i]]))
    df<-rbind(df,dfH)
  }
  load(paste0("InfoSocial",j,"1.RData"))#experimentloop
}

labels<-c("RiskyAdvisor","SafeAdvisor")
labels<-c('8'="social",'10'="solo")

df %>% group_by(test_part,ocu,RiskSafeAdv)%>% # ok krass
  summarise( mean = mean(ChooseRisk) ) %>%
  ggplot(aes(x=as.numeric(ocu),y=mean,color=as.factor(RiskSafeAdv),fill=as.factor(RiskSafeAdv)))+
  geom_point(aes(x=ocu,y=mean))+
  #geom_line()+
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_x_continuous(name="OCU_Value")+
  scale_y_continuous(name="Percent Risky Choice")+
  scale_color_discrete(name="Safe Or Risky Other",breaks=c(1,2),labels=c("RiskyAdvisor","SafeAdvisor"))+
  guides(fill=F)+facet_grid(cols = vars(test_part),labeller = labeller(test_part = labels) )+
  ggtitle("INFORMATION Safe & Risky Advisor Effect of OCU ")+my_theme



#############
load("TemperatureSocial51.RData")

df<-as.data.frame(do.call(cbind,Simulations[[1]]))
#df$mean<-mean(df$ChooseRisk)
for(i in 2:length(Simulations)){
  dfH<-as.data.frame(do.call(cbind,Simulations[[i]]))
  df<-rbind(df,dfH)
}


labels<-c("RiskyAdvisor","SafeAdvisor")
labels<-c('8'="social",'10'="solo")

df %>% group_by(test_part,ocu,RiskSafeAdv)%>% # ok krass
  summarise( mean = mean(ChooseRisk) ) %>%
  ggplot(aes(x=as.numeric(ocu),y=mean,color=as.factor(RiskSafeAdv),fill=as.factor(RiskSafeAdv)))+
  geom_point(aes(x=ocu,y=mean))+
  #make a logistic regression line. How messy.
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), 
              se = TRUE) +
  scale_x_continuous(name="OCU_Value")+
  scale_y_continuous(name="Percent Risky Choice")+
  scale_color_discrete(name="Safe Or Risky Other",breaks=c(1,2),labels=c("RiskyAdvisor","SafeAdvisor"))+
  guides(fill=F)+facet_grid(cols = vars(test_part),labeller = labeller(test_part = labels) )+
  ggtitle("NOISE Safe & Risky Advisor Effect of OCU ")+my_theme


##########

load("NullSocial51.RData")

df<-as.data.frame(do.call(cbind,Simulations[[1]]))
#df$mean<-mean(df$ChooseRisk)
for(i in 2:length(Simulations)){
  dfH<-as.data.frame(do.call(cbind,Simulations[[i]]))
  df<-rbind(df,dfH)
}


labels<-c("RiskyAdvisor","SafeAdvisor")
labels<-c('8'="social",'10'="solo")

df %>% group_by(test_part,group,ocu,RiskSafeAdv)%>% # ok krass
  summarise( mean = mean(ChooseRisk) ) %>%
  ggplot(aes(x=as.numeric(ocu),y=mean,linetype=as.factor(group),fill=as.factor(RiskSafeAdv)))+
  geom_line(aes(x=ocu,y=mean,color=as.factor(RiskSafeAdv),linetype=as.factor(group)))+
  geom_smooth()+
  scale_x_continuous(name="OCU_Value")+
  scale_y_continuous(name="Percent Risky Choice")+
  scale_color_discrete(name="Safe Or Risky Other",breaks=c(1,2),labels=c("RiskyAdvisor","SafeAdvisor"))+
  scale_linetype_discrete(name="Generative Distribution", breaks=c(1,2),labels=c("Low OCU", "High OCU"))+
  guides(fill=F)+facet_grid(cols = vars(test_part),labeller = labeller(test_part = labels) )+
  ggtitle("NULL Safe & Risky Advisor Effect of OCU ")+my_theme



library("PerformanceAnalytics")
FullMatrix=data.frame(asd=1:600,pf=1:600,rt=1:600,asd=1:600,asr=1:600,fff=1:600)
FullMatrix[1:600 , c(1:2,4:6)] %>% chart.Correlation( histogram=TRUE, pch=19)


stimulusVector=expand.grid(
  value=c(1:100),
  prob=seq(0.01,1,0.01),
  rho=c(0.1,0.3,0.6,0.9),
  ocu=seq(1,10,1),
  tau=c(0.1,0.5,1,1.5,2)
)

makePlot<-function(prob,value,rho,tau,ocu){
  tibble(
    diffExpV=(prob*value-5),
    Usafe=5^rho,
    Urisk=prob*value^rho,
    SoloChoice=sigmoid((Urisk-Usafe)*(1/tau)),
    SocialChoiceT=sigmoid((Urisk-Usafe)*(1/(tau+(ocu)))),
    rho=rho,
    tau=tau,
    ocu=ocu
    #SocialChoiceRisk=sigmoid(((Urisk+ocu)-Usafe)*theta)
  )
}

RhoLabs<-(c('0.1'="rho = 0.1 ",'0.3'="rho = 0.3 ",'0.6'="rho = 0.6 ",'0.9'="rho = 0.9"))
TauLabs<-c('0.1'= "tau= 0.1", '0.5'= "tau= 0.5",'1'= "tau= 1",'1.5'= "tau= 1.5","tau=2")
animated<- makePlot(stimulusVector$prob,stimulusVector$value,stimulusVector$rho,stimulusVector$tau,stimulusVector$ocu)%>%
  gather(.,condition, pchoose,SoloChoice:SocialChoiceT)%>%
  ggplot(.,aes(x=diffExpV,y=pchoose,group=as.factor(condition),color=as.factor(condition)))+
  geom_point(aes(y=pchoose,x=diffExpV,group=as.factor(condition)),alpha=.01)+geom_smooth()+
  facet_grid(tau~rho,labeller=labeller(rho=RhoLabs, tau=TauLabs))+
  labs(title = 'OCU on TemperatureParameter: {frame_time}', x = 'Diff Expected Value', y = 'probabilityChooseRisk') +
  transition_time(ocu)+
  ease_aes('linear')
gganimate::animate(animated)
