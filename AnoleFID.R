## Data Management ----
data1<-read.csv("HerpSpr23.csv")
data1<-data1[,-c(2:3,9:10,22:24,37,39,41,43,45,47,48)]
colnames(data1)<-c("ID","Date","Weather","StartingTemp","EndingTemp",
                   "Time","Perch","StartDist",
                   "FID","DistFled","PerchTemp","TrailDist","PerchHeight",
                   "PerchDiameter","ApproachBehavior","Orientation","CatchBehavior","StartTime",
                   "Sex","Morph","SVL","TotalLength","BrokenTail",
                   "RegrowthTail","Mass","Ectos","HandlingBehavior","Endurance",
                   "Forelimb1","Forelimb2","Hindlimb1","Hindlimb2","Dewlap1","Dewlap2")
data1.End<-subset(data1, Endurance>0)
data1.End$Morph<-factor(data1.End$Morph)
data1.End$Sex<-factor(data1.End$Sex)

## Exploration ----

# Distributions
hist(data1.End$FID)
hist(data1.End$DistFled)
hist(data1.End$PerchTemp) 
hist(data1.End$TrailDist)
hist(data1.End$PerchHeight)
hist(data1.End$PerchDiameter)
hist(data1.End$SVL)
hist(data1.End$TotalLength)
hist(data1.End$Mass)
hist(data1.End$Endurance)

# Correlations
summary(lm(FID~TrailDist, data=data1.End))
plot(FID~TrailDist, data=data1.End)

summary(lm(FID~Endurance, data=data1.End))
plot(FID~Endurance, data=data1.End)

summary(lm(FID~Sex*SVL, data=data1.End))
plot(FID~SVL, data=data1.End)

summary(lm(FID~Sex*Mass, data=data1.End))
plot(FID~Mass, data=data1.End)

# Group Comparisons
SexF<-subset(data1.End, Sex=="F")
SexM<-subset(data1.End, Sex=="M")

plot(DistFled~Sex, data=data1.End)
plot(PerchDiameter~Sex, data=data1.End)
plot(Mass~Sex, data=data1.End)

plot(DistFled~CheckedMorph, data=SexF)
aggregate(FID~CheckedMorph, length, data=SexF)


