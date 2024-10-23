#This script contains the code for data analysis presented in "Carduus nutans and C. acanthoides, two invasive thistle species, strongly rebound from disturbance"
#by Emma van der Heide, Joseph Keller, Elyse Johnson, and Katriona Shea

#Download the data for analysis from https://github.com/emmavdheide/Rebounding_from_disturbance_data_analysis

#set your working directory
setwd("~/Grad School/Penn State/Project Information/2022 Summer Pilot Study/Data Analysis/2022 Thistle Pilot Study Data Analysis")

#load required packages
library(dplyr)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(ggplot2)

#import data
dat<-read.csv("DataForAnalysis.csv", header=TRUE)
head(dat)

#split data by species
dataCA<-filter(dat, Species=="CA")
dataCN<-filter(dat, Species=="CN")

#make character data into factors
dataCA$Species<-as.factor(dataCA$Species)
dataCA$Treatment<-as.factor(dataCA$Treatment)
dataCA$Plot<-as.factor(dataCA$Plot)
dataCA$OTC.On<-as.factor(dataCA$OTC.On)
dataCA$Group<-as.factor(dataCA$Group)
dataCA$Row<-as.factor(dataCA$Row)
dataCN$Species<-as.factor(dataCN$Species)
dataCN$Treatment<-as.factor(dataCN$Treatment)
dataCN$Plot<-as.factor(dataCN$Plot)
dataCN$OTC.On<-as.factor(dataCN$OTC.On)
dataCN$Group<-as.factor(dataCN$Group)
dataCN$Row<-as.factor(dataCN$Row)

#Analysis
#Final Stems (C. acanthoides)
#Fit model and summarize
finalstemsCA<-glmmTMB(FinalStems~OTC.On*Treatment+MayLLL+MayStems+(1|Plot), data=dataCA, family="poisson")
summary(finalstemsCA)

#check residuals
sim.finalstemsCA<-simulateResiduals(finalstemsCA)
plot(sim.finalstemsCA) #visual inspection: looks okay

#estimated marginal means and mean comparisons
emmfsCA<-emmeans(finalstemsCA, specs=pairwise~Treatment|OTC.On, type="response")
emmfsCA
#inf df appears when emmeans is dealing with z-test (which are used in glmmTMB)
#https://cran.r-project.org/web/packages/emmeans/vignettes/FAQs.html#asymp

#Visualize results
No.stems.CA<-as.data.frame(emmfsCA[1])
plot.order <- c("Control", "Ice", "Gravel", "Stomp")
uprightstemplotCA<-ggplot(No.stems.CA, aes(fill=emmeans.OTC.On, y=emmeans.rate, x=factor(emmeans.Treatment, plot.order))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("dodgerblue","brown1"))+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Number of upright stems")+
  ylim(0, 4.7)+
  ggtitle("Number of upright stems (C. acanthoides)")+
  geom_errorbar(position=position_dodge(0.9), aes(x=emmeans.Treatment, ymin=emmeans.asymp.LCL, ymax=emmeans.asymp.UCL), width=0.4, colour="gray20", alpha=0.9, linewidth=1.3)
uprightstemplotCA

#Final stems (C. nutans)
#fit model and summarize
finalstemsCN<-glmmTMB(FinalStems~OTC.On*Treatment+MayLLL+MayStems+(1|Plot), data=dataCN, family="poisson")
summary(finalstemsCN)

#check residuals
sim.finalstemsCN<-simulateResiduals(finalstemsCN)
plot(sim.finalstemsCN)
  #slight underdispersion, sticking with Poisson family for consistency, but note that power of this regression may be reduced

#calculate estimated marginal means
emmfsCN<-emmeans(finalstemsCN, specs=pairwise~Treatment|OTC.On, type="response")
emmfsCN

#visualize results
No.stems.CN<-as.data.frame(emmfsCN[1])
uprightstemplotCN<-ggplot(No.stems.CN, aes(fill=emmeans.OTC.On, y=emmeans.rate, x=factor(emmeans.Treatment, plot.order))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("dodgerblue","brown1"))+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Number of upright stems")+
  ylim(-0.1, 4.7)+
  ggtitle("Number of upright stems (C. nutans)")+
  geom_errorbar(position=position_dodge(0.9), aes(x=emmeans.Treatment, ymin=emmeans.asymp.LCL, ymax=emmeans.asymp.UCL), width=0.4, colour="gray20", alpha=0.9, linewidth=1.3)
uprightstemplotCN


#Final Stems + Rosettes (C. acanthoides)
#fit model and summarize
finalstemsrosettesCA<-glmmTMB(FinalStemsRosettes~OTC.On*Treatment+MayLLL+MayStems+(1|Plot), data=dataCA, family="poisson")
summary(finalstemsrosettesCA)

#check residuals
sim.finalstemsrosettesCA<-simulateResiduals(finalstemsrosettesCA)
plot(sim.finalstemsrosettesCA) #visual inspection: looks okay

#calculate estimated marginal means
emmfsrCA<-emmeans(finalstemsrosettesCA, specs=pairwise~Treatment|OTC.On, type="response")
emmfsrCA

#Final Stems + Rosettes (C. nutans)
#fit model and summarize
finalstemsrosettesCN<-glmmTMB(FinalStemsRosettes~OTC.On*Treatment+MayLLL+MayStems+(1|Plot), data=dataCN, family="poisson")
summary(finalstemsrosettesCN)

#check residuals
sim.finalstemsrosettesCN<-simulateResiduals(finalstemsrosettesCN)
plot(sim.finalstemsrosettesCN) #visual inspection: looks okay

#calculate estimated marginal means
emmfsrCN<-emmeans(finalstemsrosettesCN, specs=pairwise~Treatment|OTC.On, type="response")
emmfsrCN


#Maximum Height (C. acanthoides)
#first, convert height data from millimeters to centimeters
dataCA$MaxHtcm<-dataCA$MaxHt/10
dataCA$MayHtcm<-dataCA$MayHt/10

#fit the model and summarize
maxhtCA<-glmmTMB(MaxHtcm~OTC.On*Treatment+MayLLL+FinalStems+(1|Plot), data=dataCA, family = "gaussian")
summary(maxhtCA)

#check residuals
sim.maxhtCA<-simulateResiduals(maxhtCA)
plot(sim.maxhtCA) #looks okay

#calculate estimated marginal means
emmmhCA<-emmeans(maxhtCA, specs=pairwise~Treatment|OTC.On, type="response")
emmmhCA

#Visualize results
Maxht.CA<-as.data.frame(emmmhCA[1])
maxhtplotCA<-ggplot(Maxht.CA, aes(fill=emmeans.OTC.On, y=emmeans.emmean, x=factor(emmeans.Treatment, plot.order))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("dodgerblue","brown1"))+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Maximum height (cm)")+
  ylim(0, 110)+
  ggtitle("Maximum height (C. acanthoides)")+
  geom_errorbar(position=position_dodge(0.9), aes(x=emmeans.Treatment, ymin=emmeans.lower.CL, ymax=emmeans.upper.CL), width=0.4, colour="gray20", alpha=0.9, linewidth=1.3)
maxhtplotCA

#Maximum Height (C. acanthoides)
#first, convert height data from millimeters to centimeters
dataCN$MaxHtcm<-dataCN$MaxHt/10

#fit model and summarize
maxhtCN<-glmmTMB(MaxHtcm~OTC.On*Treatment+MayLLL+FinalStems+(1|Plot), data=dataCN, family = "gaussian")
summary(maxhtCN)

#check residuals
sim.maxhtCN<-simulateResiduals(maxhtCN)
plot(sim.maxhtCN) #looks good

#calculate estimated marginal means
emmmhCN<-emmeans(maxhtCN, specs=pairwise~Treatment|OTC.On, type="response")
emmmhCN

#Visualize results
Maxht.CN<-as.data.frame(emmmhCN[1])
maxhtplotCN<-ggplot(Maxht.CN, aes(fill=emmeans.OTC.On, y=emmeans.emmean, x=factor(emmeans.Treatment, plot.order))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("dodgerblue","brown1"))+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Maximum height (cm)")+
  ylim(0, 110)+
  ggtitle("Maximum height (C. nutans)")+
  geom_errorbar(position=position_dodge(0.9), aes(x=emmeans.Treatment, ymin=emmeans.lower.CL, ymax=emmeans.upper.CL), width=0.4, colour="gray20", alpha=0.9, linewidth=1.3)
maxhtplotCN


#Maximum capitula (C. acanthoides)
#fit model and summarize
maxflowersbudsCA<-glmmTMB(MaxFlowersBuds~OTC.On*Treatment+MayLLL+(1|Plot), data=dataCA, family=nbinom2)
summary(maxflowersbudsCA)

#check residuals
sim.maxflowersbudsCA<-simulateResiduals(maxflowersbudsCA)
plot(sim.maxflowersbudsCA) #looks good

#calculate estimated marginal means
emmmfbCA<-emmeans(maxflowersbudsCA, specs = pairwise~Treatment|OTC.On, type = "response")
emmmfbCA

#visualize results
Maxflowersbuds.CA<-as.data.frame(emmmfbCA[1])
maxflowersbudsplotCA<-ggplot(Maxflowersbuds.CA, aes(fill=emmeans.OTC.On, y=emmeans.response, x=factor(emmeans.Treatment, plot.order))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("dodgerblue","brown1"))+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Number of capitula + buds")+
  ylim(0, 50)+
  ggtitle("Maximum number of capitula + buds (C. acanthoides)")+
  geom_errorbar(position=position_dodge(0.9), aes(x=emmeans.Treatment, ymin=emmeans.asymp.LCL, ymax=emmeans.asymp.UCL), width=0.4, colour="gray20", alpha=0.9, linewidth=1.3)
maxflowersbudsplotCA

#Maximum capitula (C. nutans)
#fit model and summarize
maxflowersbudsCN<-glmmTMB(MaxFlowersBuds~OTC.On*Treatment+MayLLL+(1|Plot), data=dataCN, family=nbinom2)
summary(maxflowersbudsCN)

#check residuals
sim.maxflowersbudsCN<-simulateResiduals(maxflowersbudsCN)
plot(sim.maxflowersbudsCN) #looks good

#calculate estimated marginal means
emmmfbCN<-emmeans(maxflowersbudsCN, specs=pairwise~Treatment|OTC.On, type="response")
emmmfbCN

#visualize results
Maxflowersbuds.CN<-as.data.frame(emmmfbCN[1])
maxflowersbudsplotCN<-ggplot(Maxflowersbuds.CN, aes(fill=emmeans.OTC.On, y=emmeans.response, x=factor(emmeans.Treatment, plot.order))) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values=c("dodgerblue","brown1"))+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Number of capitula + buds")+
  ylim(0, 50)+
  ggtitle("Maximum number of capitula + buds (C. nutans)")+
  geom_errorbar(position=position_dodge(0.9), aes(x=emmeans.Treatment, ymin=emmeans.asymp.LCL, ymax=emmeans.asymp.UCL), width=0.4, colour="gray20", alpha=0.9, linewidth=1.3)
maxflowersbudsplotCN
