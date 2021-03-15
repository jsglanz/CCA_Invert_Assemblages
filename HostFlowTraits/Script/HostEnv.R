####Flow/Algal Traits Analysis

#Load data
host<-envalgtraits
assem<-FxnlAbEnv

#remove H18 since it is an outlier that could leverage trends
hosto<-host[-18,]
assemo<-assem[-18,]
assemo<-assemo[-c(52:92),]

#Correlation between flow and algal traits and among algal traits

mca<-cor.test(hosto$FlowBin, hosto$ParMort, method="spearman", na.rm=TRUE)
mca

mcb<-cor.test(hosto$FlowBin, hosto$Vol.IS, method="spearman", na.rm=TRUE)
mcb

mcc<-cor.test(hosto$FlowBin, hosto$Rugosity, method="spearman", na.rm=TRUE)
mcc

mcd<-cor.test(hosto$FlowBin, hosto$MeanIS, method="spearman", na.rm=TRUE)
mcd

mce<-cor.test(hosto$FlowBin, hosto$MeanPD, method="spearman", na.rm=TRUE)
mce

mcf<-cor.test(hosto$FlowBin, hosto$MeanBD, method="spearman", na.rm=TRUE)
mcf

mcg<-cor.test(hosto$FlowBin, hosto$MeanB, method="spearman", na.rm=TRUE)
mcg

mc1<-cor.test(hosto$Vol.IS, hosto$Rugosity, method="spearman", na.rm=TRUE)
mc1

mc2<-cor.test(hosto$Vol.IS, hosto$MeanIS, method="spearman", na.rm=TRUE)
mc2

mc3<-cor.test(hosto$Vol.IS, hosto$MeanPD, method="spearman", na.rm=TRUE)
mc3

mc4<-cor.test(hosto$Vol.IS, hosto$MeanBD, method="spearman", na.rm=TRUE)
mc4

mc5<-cor.test(hosto$Vol.IS, hosto$MeanB, method="spearman", na.rm=TRUE)
mc5

mc6<-cor.test(hosto$Rugosity, hosto$MeanIS, method="spearman", na.rm=TRUE)
mc6

mc7<-cor.test(hosto$Rugosity, hosto$MeanPD, method="spearman", na.rm=TRUE)
mc7

mc8<-cor.test(hosto$Rugosity, hosto$MeanBD, method="spearman", na.rm=TRUE)
mc8

mc9<-cor.test(hosto$Rugosity, hosto$MeanB, method="spearman", na.rm=TRUE)
mc9

mc10<-cor.test(hosto$MeanIS, hosto$MeanPD, method="spearman", na.rm=TRUE)
mc10

mc11<-cor.test(hosto$MeanIS, hosto$MeanBD, method="spearman", na.rm=TRUE)
mc11

mc12<-cor.test(hosto$MeanIS, hosto$MeanB, method="spearman", na.rm=TRUE)
mc12

mc13<-cor.test(hosto$MeanPD, hosto$MeanBD, method="spearman", na.rm=TRUE)
mc13

mc14<-cor.test(hosto$MeanPD, hosto$MeanB, method="spearman", na.rm=TRUE)
mc14

mc15<-cor.test(hosto$MeanBD, hosto$MeanB, method="spearman", na.rm=TRUE)
mc15

mc16<-cor.test(hosto$ParMort, hosto$Vol.IS, method="spearman", na.rm=TRUE)
mc16

mc17<-cor.test(hosto$ParMort, hosto$MeanIS, method="spearman", na.rm=TRUE)
mc17

mc18<-cor.test(hosto$ParMort, hosto$MeanPD, method="spearman", na.rm=TRUE)
mc18

mc19<-cor.test(hosto$ParMort, hosto$MeanBD, method="spearman", na.rm=TRUE)
mc19

mc20<-cor.test(hosto$ParMort, hosto$MeanB, method="spearman", na.rm=TRUE)
mc20

mc21<-cor.test(hosto$ParMort, hosto$Rugosity, method="spearman", na.rm=TRUE)
mc21

#Check for normality of each variable
library(car)
qqp(hosto$MeanB, "norm")


#correlation between branch diameter and branch density as well as interstitial width and branch height (PD) and branch height & interstitial width and branch density. Thus branch density was left in models with volume and rugosity to remove variables that likely explain same variation as branch density

#Obtain VIF values for collinearity between predictors
View(assemo)
library(mctest)
X<-assemo[,c(4,5,6,7,8)] #gives you just columns 4,5,6,7,8,those corresponding to traits selected as predictors 
imcdiag(X, assemo$Density)

#boxplot distributions of algal traits by flow
library(tidyr)
library(dplyr)

#Partial Mortality
pm<-assemo %>% select(1,3,5)

pm_wide<- spread(pm,Flow,ParMort)
pm_wide<-pm_wide[,-1]

boxplot(pm_wide, horizontal=FALSE,las=1,frame=FALSE,cex.axis=0.6,par(mar = c(4, 5, 4, 2)+ 0.1),xlab="Flow Environment", ylab="%Mortality")

#Interstitial Volume
vol<-assemo %>% select(1,3,6)

vol_wide<- spread(vol,Flow,Vol.IS)
vol_wide<-vol_wide[,-1]
ylab.text = expression('Interstitial Volume, cm'^"3")

boxplot(vol_wide, horizontal=FALSE,las=1,frame=FALSE,cex.axis=0.6,par(mar = c(4, 5, 4, 2)+ 0.1),xlab="Flow Environment", ylab=ylab.text)

#Rugosity
rug<-assemo %>% select(1,3,7)

rug_wide<- spread(rug,Flow,Rugosity)
rug_wide<-rug_wide[,-1]

boxplot(rug_wide, horizontal=FALSE,las=1,frame=FALSE,cex.axis=0.6,par(mar = c(4, 5, 4, 2)+ 0.1),xlab="Flow Environment", ylab="Rugosity, unitless")

#Branch Density
bd<-assemo %>% select(1,3,8)

bd_wide<- spread(bd,Flow,MeanB)
bd_wide<-bd_wide[,-1]
ylab.text = expression('Mean Branch Density, branches'^"1"*'cm'^"-2")

boxplot(bd_wide, horizontal=FALSE,las=1,frame=FALSE,cex.axis=0.6,par(mar = c(4, 5, 4, 2)+ 0.1),xlab="Flow Environment", ylab=ylab.text)

#horizontal is whether you want your boxplots horizontal or vertical, las=2 makes axis labels (in this case taxa genera) vertical instead of horizontal
