## Packages and Functions ----
library(arm)
library(rv)
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(patchwork)
library(MASS)
library(car)
library(emmeans)
library(car)

## Data Management ----
data1<-read.csv("HerpSpr23.csv")
data1<-data1[,-c(2:3,9:10,22:24,37,39,41,43,45,47,48)]
colnames(data1)<-c("ID","Date","Weather","StartingTemp","EndingTemp",
                   "Time","Perch","StartDist","FID","DistFled",
                   "PerchTemp","TrailDist","PerchHeight","PerchDiameter","ApproachBehavior",
                   "Orientation","CatchBehavior","StartTime","Sex","Morph",
                   "SVL","TotalLength","BrokenTail","RegrowthTail","Mass",
                   "Ectos","HandlingBehavior","Endurance","Forelimb1","Forelimb2",
                   "Hindlimb1","Hindlimb2","Dewlap1","Dewlap2","TTC")
data.behavior<-read.csv("Behavior.csv")
data1<-merge(data1,data.behavior)
  
# Full data only
data2<-subset(data1, Endurance>0)
data2$Morph<-factor(data2$Morph)
data2$Sex<-factor(data2$Sex)
data2$ID<-factor(data2$ID)
data2$Forelimb<-(data2$Forelimb1+data2$Forelimb2)/2
data2$Hindlimb<-(data2$Hindlimb1+data2$Hindlimb2)/2
data2$Dewlap<-(data2$Dewlap1+data2$Dewlap2)/2
# Remove T4 because it took forever to capture after jumping the cage multiple times
data2<-subset(data2, ID!="T4")

SexF<-subset(data2, Sex=="F")
SexM<-subset(data2, Sex=="M")
## Exploration ----

## Distributions
hist(data2$FID)
hist(log(data2$DistFled)) # Needs to be log-transformed
hist(data2$PerchTemp) 
hist(log(data2$TrailDist+1))
hist(log(data2$PerchHeight))
hist(log(data2$PerchDiameter+1))
hist(data2$SVL)
hist(SexF$SVL)
hist(SexM$SVL)
hist(data2$TotalLength)
hist(log(data2$Mass))
hist(data2$Endurance)

## Analyses
# Sex Differences
aov1<-aov(SVL~Sex, data=data2) # Significant M>F
summary(aov1)
t.test(SVL~Sex, data=data2)
plot(SVL~Sex, data=data2)
Fig_SVL<-ggplot(data=data2, aes(x=Sex, y=SVL))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitter(seed=2,width=0.15), color="#636363", size=2.5)+
  #geom_point(data=data2, aes(x=Sex, y=Mean), size=4, shape=18)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold", color="black"), axis.title=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_x_discrete(labels = c('Female','Male'))+
  scale_y_continuous(expand = c(0, 0), limits = c(35, 65), breaks=seq(35,65,5))+
  xlab("")+ ylab("SVL (mm)")
#ggsave("Fig_SVL.jpeg", width=8, height=8, plot=Fig_SVL)

aov2<-aov(log(TrailDist+1)~Sex, data=data2)
summary(aov2)
t.test(log(TrailDist+1)~Sex, data=data2)
plot(TrailDist~Sex, data=data2)

aov3<-aov(FID~Sex, data=data2)
summary(aov3)
t.test(FID~Sex, data=data2)
plot(FID~Sex, data=data2)

aov4<-aov(log(DistFled)~Sex, data=data2) # Significant M>F
summary(aov4)
t.test(log(DistFled)~Sex, data=data2)
plot(log(DistFled)~Sex, data=data2)
aggregate(DistFled~Sex, mean, data=data2)
aggregate(log(DistFled)~Sex, mean, data=data2)
aggregate(DistFled~Sex, range, data=data2)
Fig_DistFled<-ggplot(data=data2, aes(x=Sex, y=log(DistFled)))+
  geom_boxplot(outlier.shape=NA)+
  geom_point(position=position_jitter(seed=2,width=0.15), color="#636363", size=2.5)+
  #geom_point(data=data2, aes(x=Sex, y=Mean), size=4, shape=18)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold",color="black"), axis.title=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_x_discrete(labels = c('Female','Male'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0,5,1))+
  xlab("")+ ylab("log(Distance Fled (cm))")
#ggsave("Fig_DistFled.jpeg", width=8, height=8, plot=Fig_DistFled)

aov5<-aov(Endurance~Sex, data=data2)
summary(aov5)
t.test(Endurance~Sex, data=data2)
plot(Endurance~Sex, data=data2)

aov6<-aov(PerchTemp~Sex, data=data2)
summary(aov6)
t.test(PerchTemp~Sex, data=data2)
plot(PerchTemp~Sex, data=data2)

aov7<-aov(log(PerchHeight+1)~Sex, data=data2)
summary(aov7)
t.test(log(PerchHeight+1)~Sex, data=data2)
plot(log(PerchHeight+1)~Sex, data=data2)

aov8<-aov(log(PerchDiameter+1)~Sex, data=data2) # Perch Diameter has some suspect measurements
summary(aov8)
plot(PerchDiameter~Sex, data=data2)

# All three morphological characters scale positively with SVL and differ between sexes- Plot
aov9<-aov(Dewlap~SVL*Sex, data=data2)
summary(aov9)
ggplot(aes(x=SVL, y=Dewlap, group=Sex), data=data2)+
  geom_point(aes(color=Sex), size=2)+
  geom_smooth(aes(color=Sex), method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 250), breaks=seq(0,250,50))

aov10<-aov(Hindlimb~SVL*Sex, data=data2)
summary(aov10)
ggplot(aes(x=SVL, y=Hindlimb, group=Sex), data=data2)+
  geom_point(aes(color=Sex), size=2)+
  geom_smooth(aes(color=Sex), method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,10))

aov11<-aov(Forelimb~SVL*Sex, data=data2)
summary(aov11)
ggplot(aes(x=SVL, y=Forelimb, group=Sex), data=data2)+
  geom_point(aes(color=Sex), size=2)+
  geom_smooth(aes(color=Sex), method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40), breaks=seq(0,40,10))

# Females only- morph comparisons
summary(aov(SVL~Morph, data=SexF))

summary(aov(log(TrailDist+1)~Morph, data=SexF))

summary(aov(FID~Morph, data=SexF))
plot(FID~Morph, data=SexF)

summary(aov(log(DistFled)~Morph, data=SexF))
plot(log(DistFled)~Morph, data=SexF)

summary(aov(Endurance~Morph, data=SexF))
plot(Endurance~Morph, data=SexF)

summary(aov(PerchTemp~Morph, data=SexF))

summary(aov(log(PerchHeight+1)~Morph, data=SexF))

summary(aov(log(PerchDiameter+1)~Morph, data=SexF))

summary(aov(Dewlap~SVL*Morph, data=SexF))

summary(aov(Hindlimb~SVL*Morph, data=SexF))

summary(aov(Forelimb~SVL*Morph, data=SexF))

# Regressions & ANCOVAs
lm1<-lm(Endurance~SVL*Sex, data=data2) # Plot this relationship
summary(lm1)
ggplot(aes(x=SVL, y=Endurance, group=Sex), data=data2)+
  geom_point(aes(color=Sex), size=2)+
  geom_smooth(aes(color=Sex), method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))

lm1.M<-lm(Endurance~SVL, data=SexM) # No relationship when it's just males
summary(lm1.M)
End.M<-ggplot(aes(x=SVL, y=Endurance), data=SexM)+
  geom_point(size=3)+
  geom_abline(intercept=193.258, slope=-1.556, size=1.5)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold",color="black"), axis.title=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75),
        plot.title=element_text(size=26, face="bold", hjust = 0.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))+
  scale_x_continuous(limits = c(35, 66), breaks=seq(35,65,5))+
  xlab("SVL (mm)")+ ylab("Endurance (s)")+ ggtitle("Male")+
  annotate("text", x=35, y=290, fontface="bold", label="A", size=8)

lm1.F<-lm(Endurance~SVL, data=SexF) # Significant positive relationship between SVL and Endurance in females
summary(lm1.F)
End.F<-ggplot(aes(x=SVL, y=Endurance), data=SexF)+
  geom_point(size=3)+
  geom_abline(intercept=-221.146, slope=8.022, size=1.5)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold",color="black"), axis.title=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75),
        plot.title=element_text(size=26, face="bold", hjust = 0.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))+
  scale_x_continuous(limits = c(35, 65), breaks=seq(35,65,5))+
  xlab("SVL (mm)")+ ylab("")+ggtitle("Female")+
  annotate("text", x=35, y=290, fontface="bold", label="B", size=8)

End.Fig<-plot_grid(End.M, End.F,
                labels = "", nrow = 1, align="h")
#ggsave("Fig_End.jpeg", width=16, height=8, plot=End.Fig)

lm2<-lm(FID~Endurance, data=data2) # NS
summary(lm2)

lm2.M<-lm(FID~Endurance, data=SexM) # NS
summary(lm2.M)

lm2.F<-lm(FID~Endurance, data=SexF) # NS
summary(lm2.F)

aov12<-aov(FID~Endurance, data=data2) # NS
summary(aov12)

lm3<-lm(FID~log(DistFled), data=data2) # NS
summary(lm3)

lm3.M<-lm(FID~log(DistFled), data=SexM) # NS
summary(lm3.M)

lm3.F<-lm(FID~log(DistFled), data=SexF) # NS
summary(lm3.F)

lm4<-lm(Endurance~log(DistFled), data=data2) # NS
summary(lm4)

lm4.M<-lm(Endurance~log(DistFled), data=SexM) # NS
summary(lm4.M)

lm4.F<-lm(Endurance~log(DistFled), data=SexF) # NS
summary(lm4.F)

lm5<-lm(FID~Dewlap, data=data2) # NS
summary(lm5)

lm5.M<-lm(FID~Dewlap, data=SexM) # NS
summary(lm5.M)

lm5<-lm(FID~Dewlap, data=data2) # NS
summary(lm5)

lm5.M<-lm(FID~Dewlap, data=SexM) # NS
summary(lm5.M)

lm6<-lm(FID~Hindlimb, data=data2) # NS
summary(lm6)

lm7<-lm(FID~Forelimb, data=data2) # NS
summary(lm7)

lm8<-lm(log(DistFled)~Dewlap, data=data2) # Interesting relationship between dewlap and distance fled- Disappears when sex is included
summary(lm8)
plot(log(DistFled)~Dewlap, data=data2)

aov13<-aov(log(DistFled)~Dewlap+Sex, data=data2)
summary(aov13)

lm9<-lm(log(DistFled)~Hindlimb, data=data2)
summary(lm9)

lm10<-lm(log(DistFled)~Forelimb, data=data2)
summary(lm10)

lm11<-lm(Endurance~Dewlap*Sex, data=data2)
summary(lm11)

lm12<-lm(Endurance~Hindlimb*Sex, data=data2) # Significant interaction that goes away when you break things out. Probably an artifact of the body size relationship
summary(lm12)
ggplot(aes(x=Hindlimb, y=Endurance, group=Sex), data=data2)+
  geom_point(aes(color=Sex), size=2)+
  geom_smooth(aes(color=Sex), method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))

lm12.M<-lm(Endurance~Hindlimb, data=SexM) # NS
summary(lm12.M)
ggplot(aes(x=Hindlimb, y=Endurance), data=SexM)+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))

lm12.F<-lm(Endurance~Hindlimb, data=SexF) # NS
summary(lm12.F)
ggplot(aes(x=Hindlimb, y=Endurance), data=SexF)+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))

lm13<-lm(Endurance~Forelimb*Sex, data=data2)
summary(lm13)

aov14<-lm(log(DistFled)~PerchTemp, data=data2) # Negative relationship between perch temp and distance fled
summary(aov14)
Fig_PTemp<-ggplot(aes(x=PerchTemp, y=log(DistFled)), group=Sex, data=data2)+
  geom_point(aes(colour=Sex), size=3)+
  geom_abline(intercept=5.05238, slope=-0.06678, size=1.5)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold",color="black"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.25, .23), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0,5,1))+
  scale_x_continuous(limits = c(15, 40), breaks=seq(15,40,5))+
  xlab("Perch Temperature (°C)")+ ylab("log(Distance Fled (cm))")
#ggsave("Fig_PTemp.jpeg", width=8, height=8, plot=Fig_PTemp)

aov14.M<-lm(log(DistFled)~PerchTemp, data=SexM)
summary(aov14.M)
ggplot(aes(x=PerchTemp, y=log(DistFled)), data=SexM)+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0,5,1))

aov14.F<-lm(log(DistFled)~PerchTemp, data=SexF)
summary(aov14.F)
ggplot(aes(x=PerchTemp, y=log(DistFled)), data=SexF)+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0,5,1))

## Behavior----
# Approach Struggle? = 73%
(sum(data2[36])/74)*100
# Approach pushup = 7%
(sum(data2[37])/74)*100
# Approach dewlap = 4%
(sum(data2[38])/74)*100
# Approach headbob = 1%
(sum(data2[39])/74)*100

## Do logistic regressions to compare probabilities of behaviors between sexes
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Catch bite
aggregate(Catch.Behavior.B~Sex, sum, data=data2)
aggregate(Catch.Behavior.B~Sex, length, data=data2)
glm1<-glm(Catch.Behavior.B~Sex, data=data2, family=binomial(link=logit))
Anova(glm1)
Probability1<-as.data.frame(c(logit2prob(coef(glm1)[1]),logit2prob(coef(glm1)[1]+(1*coef(glm1)[2]))))
colnames(Probability1)<-"Probability"
Probability1$Sex<-c("Female","Male")
Probability1$Time<-as.factor("1")

# Catch struggle
aggregate(Catch.Behavior.S~Sex, sum, data=data2)
aggregate(Catch.Behavior.S~Sex, length, data=data2)
glm2<-glm(Catch.Behavior.S~Sex, data=data2, family=binomial(link=logit))
Anova(glm2)
Probability2<-as.data.frame(c(logit2prob(coef(glm2)[1]),logit2prob(coef(glm2)[1]+(1*coef(glm2)[2]))))
colnames(Probability2)<-"Probability"
Probability2$Sex<-c("Female","Male")
Probability2$Time<-as.factor("1")

# Catch defecate
aggregate(Catch.Behavior.D~Sex, sum, data=data2)
aggregate(Catch.Behavior.D~Sex, length, data=data2)
glm3<-glm(Catch.Behavior.D~Sex, data=data2, family=binomial(link=logit))
Anova(glm3)
Probability3<-as.data.frame(c(logit2prob(coef(glm3)[1]),logit2prob(coef(glm3)[1]+(1*coef(glm3)[2]))))
colnames(Probability3)<-"Probability"
Probability3$Sex<-c("Female","Male")
Probability3$Time<-as.factor("1")

# Handling bite
aggregate(Handling.Behavior.B~Sex, sum, data=data2)
aggregate(Handling.Behavior.B~Sex, length, data=data2)
glm4<-glm(Handling.Behavior.B~Sex, data=data2, family=binomial(link=logit))
Anova(glm4) # males are more likely to bite
Probability4<-as.data.frame(c(logit2prob(coef(glm4)[1]),logit2prob(coef(glm4)[1]+(1*coef(glm4)[2]))))
colnames(Probability4)<-"Probability"
Probability4$Sex<-c("Female","Male")
Probability4$Time<-as.factor("2")

# Handling struggle
aggregate(Handling.Behavior.S~Sex, sum, data=data2)
aggregate(Handling.Behavior.S~Sex, length, data=data2)
glm5<-glm(Handling.Behavior.S~Sex, data=data2, family=binomial(link=logit))
Anova(glm5)
Probability5<-as.data.frame(c(logit2prob(coef(glm5)[1]),logit2prob(coef(glm5)[1]+(1*coef(glm5)[2]))))
colnames(Probability5)<-"Probability"
Probability5$Sex<-c("Female","Male")
Probability5$Time<-as.factor("2")

# Handling defecate
aggregate(Handling.Behavior.D~Sex, sum, data=data2)
aggregate(Handling.Behavior.D~Sex, length, data=data2)
glm6<-glm(Handling.Behavior.D~Sex, data=data2, family=binomial(link=logit))
Anova(glm6)
Probability6<-as.data.frame(c(logit2prob(coef(glm6)[1]),logit2prob(coef(glm6)[1]+(1*coef(glm6)[2]))))
colnames(Probability6)<-"Probability"
Probability6$Sex<-c("Female","Male")
Probability6$Time<-as.factor("2")

Probability.B<-rbind(Probability1, Probability4)
Probability.S<-rbind(Probability2, Probability5)
Probability.D<-rbind(Probability3, Probability6)

## Behavioral figures comparing sexes between capture and handling
ggplot(Probability.B, aes(x=Sex, y=Probability, fill=Time))+
  geom_col(position=position_dodge(.7), colour="black", width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('Female','Male'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  coord_cartesian(clip = "off")

ggplot(Probability.S, aes(x=Sex, y=Probability, fill=Time))+
  geom_col(position=position_dodge(.7), colour="black", width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('Female','Male'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  coord_cartesian(clip = "off")

ggplot(Probability.D, aes(x=Sex, y=Probability, fill=Time))+
  geom_col(position=position_dodge(.7), colour="black", width=.7)+
  theme_classic()+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(size=20,face="bold"), axis.text.x=element_text(size=22,face="bold"))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=2.5), axis.line=element_line(size=2.5))+
  theme(plot.margin = margin(11, 5.5, 5.5, 0, "pt"), legend.position="none", plot.title=element_text(size=20, face="bold", hjust = 0.5))+
  scale_x_discrete(labels = c('Female','Male'))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks=c(0,.25,.50,.75,1))+ # Forces the y-axis to start at zero and end at 100
  xlab("")+
  ylab("")+
  scale_fill_manual(values=c('#ABABAB','#000000'))+
  coord_cartesian(clip = "off")

# Bite Sex*Time
glm7<-glm(Handling.Behavior.B~Sex+Catch.Behavior.B, data=data2, family=binomial(link=logit))
Anova(glm7)
Probability7<-as.data.frame(c(logit2prob(coef(glm7)[1]),logit2prob(coef(glm7)[1]+(1*coef(glm7)[2]))))
colnames(Probability1)<-"Probability"
Probability1$Sex<-c("Female","Male")
Probability1$Time<-as.factor("1")






