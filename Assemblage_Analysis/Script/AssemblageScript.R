###Summer 19, Assemblage ~ Host and Environmental Trait Surveys
library(mvabund) # multivariate analysis
library(dplyr)
library(HH) # for variance inflation factor analysis (vif) in multiple regression


#boxplot distributions of functional group abundances by flow
#Load data
assem<-FxnlAbEnv

#separate by flow
hass<-assem%>% filter(Flow=="High")
lass<-assem%>% filter(Flow=="Low")
View(hass)

lass<-lass[,-c(1:10)]#convert into community matrix aka just taxa count data
hass<-hass[,-c(1:10)]#convert into community matrix aka just taxa count data

boxplot(hass, horizontal=TRUE,las=1,frame=FALSE,cex.axis=0.6,col = c("darkolivegreen","red4","red4","red4","orange","mediumpurple4","mediumpurple4","mediumpurple4","blue"),par(mar = c(2, 5, 4, 2)+ 0.1),xlab="Abundance" )

boxplot(lass, horizontal=TRUE,las=1,frame=FALSE,cex.axis=0.6,col = c("darkolivegreen","red4","red4","red4","orange","mediumpurple4","mediumpurple4","mediumpurple4","blue"),par(mar = c(2, 5, 4, 2)+ 0.1),xlab="Abundance" )

#horizontal is whether you want your boxplots horizontal or vertical, las=2 makes axis labels (in this case functional group) vertical instead of horizontal


#Multivariate analysis: Model Selection with potential assemblage drivers
assem<-assem[-18,]#remove outlier with 80% partial mortality
as<-assem[,-c(2:10)]#make community matrix except keep sample names


as<-as.data.frame(as)#necessary for next step
row.names(as) <- as$Samples#make sample names row names
as[1] <- NULL#get rid of Samples column
View(as)#Are all columns numeric?


as_spp<-mvabund(as)#convert abundance data into an mvabund object


par(mar=c(2,10,2,2)) # adjusts the margins
boxplot(as,horizontal = TRUE,las=2, main="Abundance")#quick look at spread of data using boxplot
#some spp are much more abundant and variable than others so we need to check our mean-variance relationship

meanvar.plot(as_spp)# checks mean-var relationship

mod1 <- manyglm(as_spp ~ assem$Flow, family="negative_binomial")
mod2 <- manyglm(as_spp ~ assem$ParMort, family="negative_binomial")
mod3 <- manyglm(as_spp ~ assem$Vol.IS+assem$Rugosity+assem$MeanB, family="negative_binomial")
mod4 <- manyglm(as_spp ~ assem$Flow+assem$ParMort+assem$Vol.IS+assem$Rugosity+assem$MeanB, family="negative_binomial")

#check assumptions 
## examine residual plots of the model for model fit
plot(mod4, which = 1:3)#plot of residuals, should see random scatter of points. DON'T WANT linear, curvilinear or fan shape

#use AIC to compare models

AIC.mods <- data.frame(Model = 1:4, # progressive order in which models were fit
                                      Formula = c(paste(formula(mod4)[3]), 
                                                  paste(formula(mod3)[3]),
                                                  paste(formula(mod2)[3]), 
                                                  paste(formula(mod1)[3])),
                                      AIC = c(sum(AIC(mod4)), 
                                              sum(AIC(mod3)), 
                                              sum(AIC(mod2)), 
                                              sum(AIC(mod1))))
arrange(AIC.mods, AIC)[1:4,] # display all of the models and their AIC values in order from lowest to highest

# Likelihood ratio tests suggest that model 4 provides a better fit to the data than the next closest model (model 3, algal morphology)
anova.manyglm(mod3, mod4)
anova.manyglm(mod2, mod4)
anova.manyglm(mod1, mod4)


#Obtain VIF values to check collinearity between predictors
#assemo contains complete observations and predictor variables
# variance inflation factor analysis
vif(xx = as.data.frame(assemo[ ,c("FlowBin","ParMort","Vol.IS","Rugosity","MeanB")])) # Little evidence of variance inflation among predictor variables.


#Correlation coefficients and heatmaps of manyglm models
require(graphics)
library(lattice)
X<-assem[,4:8]#Matrix of flow and algal traits
Xs<-scale(X)#scale environmental vars
Xs<-as.data.frame(Xs)
View(Xs)

#Separate each set of drivers
Xflo<-Xs[,1] 
Xpm<-Xs[,2]
Xmorph<-Xs[,3:5]

#Water Flow
ftspp1=traitglm(as,Xflo, family="negative.binomial",method="glm1path")
ftspp1$fourth#returns values of coefficients
a        = max( abs(ftspp1$fourth.corner) )
colort   = colorRampPalette(c("blue","white","darkorange")) 
plot.spp = levelplot(t(as.matrix(ftspp1$fourth.corner)), xlab=FALSE, ylab="Functional Group", col.regions=colort(100), at=seq(-a, a, length=100),scales = list( x= list(rot = 45,labels= "Low Flow"), y= list(labels=c("Annelida", "Crustacea-l", "Crustacea-m", "Crustacea-s","Echinodermata","Fish", "Mollusca-l","Mollusca-m","Mollusca-s"))))
print(plot.spp)

#Algal partial mortality
ftspp1=traitglm(as,Xpm, family="negative.binomial",method="glm1path")
ftspp1$fourth#returns values of coefficients
a        = max( abs(ftspp1$fourth.corner) )
colort   = colorRampPalette(c("blue","white","darkorange")) 
plot.spp = levelplot(t(as.matrix(ftspp1$fourth.corner)), xlab=FALSE, ylab="Functional Group", col.regions=colort(100), at=seq(-a, a, length=100),scales = list( x= list(rot = 45,labels= "%Mortality"), y= list(labels=c("Annelida", "Crustacea-l", "Crustacea-m", "Crustacea-s","Echinodermata","Fish", "Mollusca-l","Mollusca-m","Mollusca-s"))))
print(plot.spp)

#Algal morphology
ftspp1=traitglm(as,Xmorph, family="negative.binomial",method="glm1path")
ftspp1$fourth#returns values of coefficients
a        = max( abs(ftspp1$fourth.corner) )
colort   = colorRampPalette(c("blue","white","darkorange")) 
plot.spp = levelplot(t(as.matrix(ftspp1$fourth.corner)), xlab=FALSE, ylab="Functional Group", col.regions=colort(100), at=seq(-a, a, length=100),scales = list( x= list(rot = 45,labels= c("Volume","Rugosity","BranchDensity")), y= list(labels=c("Annelida", "Crustacea-l", "Crustacea-m", "Crustacea-s","Echinodermata","Fish", "Mollusca-l","Mollusca-m","Mollusca-s"))))
print(plot.spp)
