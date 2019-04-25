
library(plyr)
library(ggplot2)
library(cowplot)
library(viridis)
library(tidyverse)
# this just applies a power function and makes a dataframe that can be used by ggplot.
utility<- function(x,alpha){
  return(data.frame(x = x,  Utility=x^alpha))
}

# this uses 5 as a refernce value and pukes out hypothetical
# choice probabilites.
# Here i define the Models

# Utility Model
probChoose<- function(x,alpha,theta){
  return(data.frame(x = x,
                    Utility=x^alpha, 
                    p= 1/(1+exp(-((x^alpha)-5)*theta))
                    ))
}

#Classic OCU
probChooseOCU<- function(x,alpha,theta,ocu){
  return(data.frame(x = x,   
                    Utility=x^alpha, p= 1/(1+exp(-((x^alpha)-5)*theta)), 
                    pRisk= 1/(1+exp(-((x^alpha+ocu)-5)*theta)), 
                    pSafe= 1/(1+exp(-((x^alpha)-(5+ocu))*theta)) 
                    ))
}

#Assymetric OCU
probChooseASY<- function(x,alpha,theta,ocuSafe,ocuRisk){
  return(data.frame(x = x,
                    Utility=x^alpha,
                    p= 1/(1+exp(-((x^alpha)-5)*theta)),
                    pRisk= 1/(1+exp(-((x^alpha+ocuRisk)-5)*theta)), 
                    pSafe= 1/(1+exp(-((x^alpha)-(5+ocuSafe))*theta))
                    ))
}

#Reward Sensitivity
probChooseREWSENS<- function(x,alpha,theta,ocu){
  return(data.frame(x = x,  
                    Utility=x^alpha, 
                    p= 1/(1+exp(-((x^alpha)-5)*theta)),
                    pSoc= 1/(1+exp(-((x^(alpha+ocu))-5)*theta)) 
                    ))
}

#Trembling Hand
probChooseTREMBL<- function(x,alpha,theta,ocu){
  return(data.frame(x = x,
                    Utility=x^alpha,
                    p= 1/(1+exp(-((x^alpha)-5)*theta)),
                    pSoc= (1-ocu)*1/(1+exp(-((x^alpha)-5)*theta)) + (ocu/2) 
                    ))
}

#Define Parameters
paramsConvex <- expand.grid( alpha = c(0.2))
paramsConcave <- expand.grid( alpha = c(1.7))
paramsLinear<- expand.grid( alpha = c(1))

#For Simple Utility Choice Rule
paramsConvexChoice <- expand.grid(theta=c(0.3), alpha = c(0.8))
paramsConcaveChoice <- expand.grid(theta=c(1), alpha = c(1.2))
paramsLinearChoice<- expand.grid(theta=c(2.3), alpha = c(0.8))

#For Social Hypothesis
paramsRewSens<- expand.grid(theta=c(2.3), alpha = c(0.8),ocu=0.3)
paramsOCU<- expand.grid(theta=c(2.3), alpha = c(0.8),ocuSafe=2,ocuRisk=2)
paramsTrembl<- expand.grid(theta=c(2.3), alpha = c(0.8),ocu=0.5)
paramsAsym<- expand.grid(theta=c(2.3), alpha = c(0.8),ocuSafe=2,ocuRisk=0.5)

# Utility Functions
convex <- mdply(paramsConvex, utility, x = seq(0, 50, 1))
concave <- mdply(paramsConcave, utility, x = seq(0, 50, 1))
linear <-mdply(paramsLinear,utility,x=seq(0,5,0.01))

# define the plot Labels
Names=c("A","B")
Labs=c("Solo Choice","Social Choice")


NamesInfo=c("A","B","C")
LabsInfo=c("Advice Risky","Solo Choice","Advice Safe")
# Define the plot titles
Hypothesis1 <- c(
  "A" = "Symmetric Social Influence",
  "B" = "Asymmetric Social Influence")

Hypothesis2 <- c(
  "C" = "Reward Sensitivity",
  "D" = "Social Distraction")
# Make Hypothesis Plots.


OcuSym <-mdply(paramsOCU, probChooseASY,x=seq(0.1,15,0.1))
OcuAsym <-mdply(paramsAsym, probChooseASY,x=seq(0.1,15,0.1))
RewSens <-mdply(paramsRewSens, probChooseREWSENS,x=seq(0.1,15,0.1))
Trembling <-mdply(paramsTrembl, probChooseTREMBL,x=seq(0.1,15,0.1))

OcuSym%>%gather("condition","Prob",p,pRisk,pSafe)%>%
  mutate(condition=
           case_when(
             condition=="p"~"B",
             condition=="pRisk"~"A",
             condition=="pSafe"~"C",
             TRUE~condition
           ),
         Hypo="A"
  )%>%rbind(
    #######add the trembling hand Model.
    OcuAsym%>%gather("condition","Prob",p,pRisk,pSafe)%>%
               mutate(condition=
                        case_when(
                          condition=="p"~"B",
                          condition=="pRisk"~"A",
                          condition=="pSafe"~"C",
                          TRUE~condition
                        ),
                      Hypo="B"
               )
    
  )%>%
  ggplot(aes(x=x,y=Prob,color=condition))+
  geom_line(size=2)+
  geom_hline(yintercept=0.5,linetype="dotdash")+
  scale_color_viridis(name="Social Condition",discrete=TRUE,breaks=NamesInfo,labels=LabsInfo)+
  scale_y_continuous(name="Probability to Choose Risk")+
  scale_x_continuous(name=NULL)+
  facet_grid(.~Hypo,labeller = labeller(
    Hypo=Hypothesis1
  ))+theme(strip.background =element_rect(fill="White"))->B
print(B)



Trembling%>%gather("condition","Prob",p,pSoc)%>%
  mutate(condition=
           case_when(
             condition=="p"~"A",
             condition=="pSoc"~"B",
             TRUE~condition
           ),
         Hypo="D"
  )%>%rbind(
    #######add the trembling hand Model.
    RewSens%>%gather("condition","Prob",p,pSoc)%>%
      mutate(condition=
               case_when(
                 condition=="p"~"A",
                 condition=="pSoc"~"B",
                 TRUE~condition
               ),
             Hypo="C"
      )
    
  )%>%
  ggplot(aes(x=x,y=Prob,color=condition))+
  geom_line(size=2)+
  geom_hline(yintercept=0.5,linetype="dotdash")+
  scale_color_viridis(name="Social Condition",discrete=TRUE,breaks=Names,labels=Labs)+
  scale_y_continuous(name="Probability to Choose Risk")+
  scale_x_continuous(name=expression(paste(Delta," Expected Value // Risk - Safe [AU]")))+
  facet_grid(.~Hypo,
             labeller = labeller(
             Hypo=Hypothesis2  
             )
             )+theme(strip.background =element_rect(fill="White"))->A




cowplot::plot_grid(B,A, labels = c("(A)","(B)"), nrow = 2, rel_heights = c(1, 1.2))
ggsave("ModelHypo.tiff",width = 10, height = 7,dpi=300)

linearChoice <-mdply(paramsLinearChoice, probChoose,x=seq(0.1,15,0.1))


OnePlot<-rbind(concave,linear)

ggplot(linearChoice,aes(x=x,y=p),size=2)+geom_line()+
  xlab("Expected Value (AU)")+
  ylab("Expected Utility (AU)")+
  scale_color_discrete(name="Developmental Stage")+
  scale_size_continuous(guide=F)+
  ggtitle("Reward Sensitivity Hypothesis")

VertLine1<-data.frame(
x_hor1=10,
x_hor2=10,
y_hor1=0,
y_hor2=convex[convex$x==10,]$Utility)

VertLine2<-data.frame(
  x_hor1=30,
  x_hor2=30,
  y_hor1=0,
  y_hor2=convex[convex$x==30,]$Utility)

HorLine1<-data.frame(
  x_hor1=0,
  x_hor2=10,
  y_hor1=convex[convex$x==10,]$Utility,
  y_hor2=convex[convex$x==10,]$Utility)

HorLine2<-data.frame(
  x_hor1=0,
  x_hor2=30,
  y_hor1=convex[convex$x==30,]$Utility,
  y_hor2=convex[convex$x==30,]$Utility)

ggplot(convex, aes(x,Utility,color=blue,size=2))+geom_line(color="#fde725ff")+
  xlab("Expected Value (AU)")+
  ylab("Expected Utility (AU)")+
  scale_color_manual(guide=F)+
  scale_size_continuous(guide=F)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=VertLine1, color="black",size=1)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=VertLine2, color="black",size=1)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=HorLine1, color="black",size=1)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=HorLine2, color="black",size=1)+
  ggtitle("Convex Utility Function")+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(color="white"))->Vex



VertLineConc1<-data.frame(
  x_hor1=10,
  x_hor2=10,
  y_hor1=0,
  y_hor2=concave[concave$x==10,]$Utility)

VertLineConc2<-data.frame(
  x_hor1=30,
  x_hor2=30,
  y_hor1=0,
  y_hor2=concave[concave$x==30,]$Utility)

HorLineConc1<-data.frame(
  x_hor1=0,
  x_hor2=10,
  y_hor1=concave[concave$x==10,]$Utility,
  y_hor2=concave[concave$x==10,]$Utility)

HorLineConc2<-data.frame(
  x_hor1=0,
  x_hor2=30,
  y_hor1=concave[concave$x==30,]$Utility,
  y_hor2=concave[concave$x==30,]$Utility)


ggplot(concave, aes(x,Utility,size=2))+geom_line(color="#440154ff")+
  xlab("Expected Value (AU)")+
  ylab("Expected Utility (AU)")+
  scale_color_discrete(guide=F)+
  scale_size_continuous(guide=F)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=VertLineConc1, color="black",size=1)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=VertLineConc2, color="black",size=1)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=HorLineConc1, color="black",size=1)+
  geom_segment(aes(x = x_hor1, y = y_hor1, xend = x_hor2, yend = y_hor2),data=HorLineConc2, color="black",size=1)+
  ggtitle("Concave Utility Function")+
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_blank())->Cave

cowplot::plot_grid(Cave,Vex, labels = c("(A)","(B)"), nrow = 1, rel_heights = c(1, 1.2))

ggsave("ConvexConcave.tiff",width = 10, height = 4,dpi=300)




ggplot(linear, aes(x,Utility, color=factor(alpha),size=2))+geom_line(color="red")+
  xlab("Expected Value (AU)")+
  ylab("Expected Utility (AU)")+
  scale_color_discrete(guide=F)+
  scale_size_continuous(guide=F)+
  ggtitle("Linear Utility Function")