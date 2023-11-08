## Packages and Functions ----
library(ggplot2)
library(cowplot)

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
hist(data2$TrailDist)
hist(data2$PerchHeight)
hist(data2$PerchDiameter)
hist(data2$SVL)
hist(data2$TotalLength)
hist(log(data2$Mass))
hist(data2$Endurance)

## Analyses
# Sex Differences
aov1<-aov(SVL~Sex, data=data2)
summary(aov1)
plot(SVL~Sex, data=data2)

aov2<-aov(TrailDist~Sex, data=data2)
summary(aov2)
plot(TrailDist~Sex, data=data2)

aov3<-aov(FID~Sex, data=data2)
summary(aov3)
plot(FID~Sex, data=data2)

aov4<-aov(log(DistFled)~Sex, data=data2) # Significant M>F
summary(aov4)
plot(log(DistFled)~Sex, data=data2)
aggregate(DistFled~Sex, mean, data=data2)
aggregate(log(DistFled)~Sex, mean, data=data2)
aggregate(DistFled~Sex, range, data=data2)

aov5<-aov(Endurance~Sex, data=data2)
summary(aov5)
plot(Endurance~Sex, data=data2)

aov6<-aov(PerchTemp~Sex, data=data2)
summary(aov6)
plot(PerchTemp~Sex, data=data2)

aov7<-aov(PerchHeight~Sex, data=data2)
summary(aov7)
plot(PerchHeight~Sex, data=data2)

aov8<-aov(PerchDiameter~Sex, data=data2) # Perch Diameter has some suspect measurements
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
summary(aov(FID~Morph, data=SexF))
plot(FID~Morph, data=SexF)

summary(aov(Endurance~Morph, data=SexF))
plot(Endurance~Morph, data=SexF)

summary(aov(log(DistFled)~Morph, data=SexF))
plot(log(DistFled)~Morph, data=SexF)

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
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75),
        plot.title=element_text(size=26, face="bold", hjust = 0.5))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
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
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75),
        plot.title=element_text(size=26, face="bold", hjust = 0.5))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 300), breaks=seq(0,300,50))+
  scale_x_continuous(limits = c(35, 65), breaks=seq(35,65,5))+
  xlab("SVL (mm)")+ ylab("")+ggtitle("Female")+
  annotate("text", x=35, y=290, fontface="bold", label="B", size=8)

End.Fig<-plot_grid(End.M, End.F,
                labels = "", nrow = 1, align="h")
ggsave("Fig_End.jpeg", width=16, height=8, plot=End.Fig)

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
ggplot(aes(x=PerchTemp, y=log(DistFled)), data=data2)+
  geom_point(size=2)+
  geom_smooth(method="lm", se=F)+
  theme_classic()+
  theme(axis.text=element_text(size=20,face="bold"), axis.title=element_text(size=22,face="bold"),
        legend.text=element_text(size=22, face="bold"), legend.title=element_text(size=25, face="bold", hjust=0.5))+
  theme(axis.ticks.length.y=unit(.5, "cm"), axis.ticks.y=element_line(size=1.75), axis.line=element_line(size=1.75))+
  theme(legend.position = c(.75, .2), plot.margin = margin(11, 5.5, 5.5, 5.5, "pt"))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5), breaks=seq(0,5,1))

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

## Behavior- Not using at this point----
# Approach Struggle? = 73%
(sum(data2[36])/74)*100
# Approach pushup = 7%
(sum(data2[37])/74)*100
# Approach dewlap = 4%
(sum(data2[38])/74)*100
# Approach headbob = 1%
(sum(data2[39])/74)*100

# Catch bite
(sum(data2[42])/74)*100 # 51%
# Catch bite
(sum(SexF[42])/48)*100 # 40%
# Catch bite
(sum(SexM[42])/26)*100 # 73%
# Catch struggle
(sum(data2[43])/74)*100 # 73%
# Catch struggle
(sum(SexF[43])/48)*100 # 73%
# Catch struggle
(sum(SexM[43])/26)*100 #73%
# Catch defecate
(sum(data2[44])/74)*100 # 14%
# Catch defecate
(sum(SexF[44])/48)*100 # 13%
# Catch defecate = 15%
(sum(SexM[44])/26)*100 

# Handling bite
(sum(data2[45])/74)*100 # 69%
# Handling bite
(sum(SexF[45])/48)*100 # 60%
# Handling bite
(sum(SexM[45])/26)*100 # 84%
# Handling struggle
(sum(data2[46])/74)*100 # 89%
# Handling struggle
(sum(SexF[46])/48)*100 # 90%
# Handling struggle
(sum(SexM[46])/26)*100 # 88%
# Handling defecate
(sum(data2[47])/74)*100 # 20%
# Handling defecate
(sum(SexF[47])/48)*100 # 13%
# Handling defecate
(sum(SexM[47])/26)*100 # 34%














