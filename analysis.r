#
.libPaths("M:/Personal Data/R/win-library/3.5.1")

library(survival)

#############Data preparation

#read data
efficacy=read.csv("M:/Personal Data/Orga/R/data/efficacy.csv")
subject= read.csv("M:/Personal Data/Orga/R/data/subject.csv")
randomization= read.csv("M:/Personal Data/Orga/R/data/randomization.csv")

# Data markup
efficacy$subject=as.factor(efficacy$subject)
subject$subject=as.factor(subject$subject) 
randomization$arm=as.factor(randomization$arm)


# Combining datasets
subject[,c("nosebleeds","duration")]=efficacy[which(efficacy$subject==subject$subject),2:3]
subject[,c("arm")]=randomization$arm[which(randomization$subject==subject$subject)]
#data overview
print(summary(subject))
summary(as.factor(subject$nosebleeds))
#Patient with missing mucus viscosity
print(subject[which(is.na(subject$mucus.viscosity)),])

### Additional variables
#indicator variables
subject$noseblny=ifelse(subject$nosebleeds>0,1,0) #Outcome
subject$trt=ifelse(subject$arm=="ACTIVE",1,0) #Treatment
subject$tissueny=ifelse(subject$tissue.use=="MEDIUM",1,0) #Tissue use
subject$inc.rate=100*subject$nosebleeds/subject$duration

subject$eye.4cat=ifelse(is.na(subject$eye.colour),"NA",as.character(subject$eye.colour)) #eye colour using NA as category
subject$A=ifelse(subject$country=="A",1,0) #Country A
subject$B=ifelse(subject$country=="B",1,0) #Country B
subject$C=ifelse(subject$country=="C",1,0) #Country C
subject$D=ifelse(subject$country=="D",1,0) #Country d
subject$E=ifelse(subject$country=="E",1,0) #Country E
subject$F=ifelse(subject$country=="F",1,0) #Country f
subject$G=ifelse(subject$country=="G",1,0) #Country G
subject$H=ifelse(subject$country=="H",1,0) #Country h
subject$I=ifelse(subject$country=="I",1,0) #Country I
subject$J=ifelse(subject$country=="J",1,0) #Country J
subject$BLACK=ifelse(subject$eye.colour=="BLACK",1,0) #eye colour black
subject$BLACK=ifelse(is.na(subject$eye.colour),0,subject$BLACK) #eye colour black
subject$BLUE=ifelse(subject$eye.colour=="BLUE",1,0) #eye colour blue
subject$BLUE=ifelse(is.na(subject$eye.colour),0,subject$BLUE) #eye colour blue
subject$BROWN=ifelse(subject$eye.colour=="BROWN",1,0) #eye colour brown
subject$BROWN=ifelse(is.na(subject$eye.colour),0,subject$BROWN) #eye colour brown
subject$NotA=ifelse(is.na(subject$eye.colour),1,0) #eye colour NA


#Threshold values
subject$mucus.3804.ny=ifelse(subject$mucus.viscosity>=0.3804,1,0) #Mucus viscosity >= 0.3804 (y/n)
subject$mucus.2.5.ny=ifelse(subject$mucus.viscosity>=2.5,1,0) #Mucus viscosity >= 2.5 (y/n)







library(tableone)

####Variable list for demography table
listVars <- c("arm","country","eye.4cat","tissue.use","previous.year","mucus.viscosity","nosebleeds","inc.rate","duration")

 #Define categorical variables
catVars <- c("arm","country","eye.4cat","tissue.use")


#Total Population
table1 <- CreateTableOne(vars = listVars, data = subject, factorVars = catVars)
table1
#Total by treatment
table1.1 <- CreateTableOne(vars=listVars[-1], data = subject, factorVars = catVars, strata = c("arm"))
table1.1

# eye.colour by country
table(subject$country,subject$eye.4cat)
table1.2a <- CreateTableOne(vars=listVars[-3], data = subject, factorVars = catVars, strata = c("eye.4cat"))
table1.2a

#Total by eye.colour
table1.2b <- CreateTableOne(vars=listVars[-3], data = subject, factorVars = catVars, strata = c("eye.4cat"))
table1.2b

#Total by country
table1.2c <- CreateTableOne(vars=listVars[-2], data = subject, factorVars = catVars, strata = c("country"))
table1.2c

#Mucus viscosity >=2.5 by country
table1.2d <- CreateTableOne(vars=listVars[-2], data = subject[which((subject$mucus.2.5.ny==F)),], factorVars = catVars, strata = c("country"))
table1.2d




#Total by arm & eye.colour
table1.3a <- CreateTableOne(vars=listVars[-c(1,3)], data = subject[which(subject$arm=="ACTIVE"),], factorVars = catVars, strata = c("eye.4cat"))
table1.3a
table1.3b <- CreateTableOne(vars=listVars[-c(1,3)], data = subject[which(subject$arm=="PLACEBO"),], factorVars = catVars, strata = c("eye.4cat"))
table1.3b

#Total by arm & tissue.use
table1.4a <- CreateTableOne(vars=listVars[-c(1:2,4)], data = subject[which(subject$tissue.use=="MEDIUM"),], factorVars = catVars, strata = c("arm"))
table1.4a
table1.4b <- CreateTableOne(vars=listVars[-c(1:2,4)], data = subject[which(subject$tissue.use=="HIGH"),], factorVars = catVars, strata = c("arm"))
table1.4b

#Total by arm & tissue.use
table1.5a <- CreateTableOne(vars=listVars[-c(1:2,5)], data = subject[which(subject$arm=="ACTIVE"),], factorVars = catVars, strata = c("previous.year"))
table1.5a
table1.5b <- CreateTableOne(vars=listVars[-c(1:2,5)], data = subject[which(subject$arm=="PLACEBO"),], factorVars = catVars, strata = c("previous.year"))
table1.5b


#### Data Visualizations

#Histograms
par(mfrow=c(2,2))
sapply(levels(subject$arm),FUN=function(x,data){hist(data$nosebleeds[which(data$arm==x)],main="Nosebleed frequency",xlab=x,ylim=c(0,200),las=1)},subject)
sapply(levels(subject$arm),FUN=function(x,data){hist(data$inc.rate[which(data$arm==x)],main="Nosebleed incidence rate frequency",xlab=x,ylim=c(0,200),las=1)},subject)

par(mfrow=c(2,2))
sapply(levels(subject$arm),FUN=function(x,data){hist(log(data$nosebleeds[which(data$arm==x)]),main="Log(Nosebleed frequency)",xlab=x,ylim=c(0,200),las=1)},subject)
sapply(levels(subject$arm),FUN=function(x,data){hist(log(data$inc.rate[which(data$arm==x)]),main="Log(Nosebleed incidence rate frequency)",xlab=x,ylim=c(0,200),las=1)},subject)





###Kaplan-Meier for duration


km.by.trt <- survfit(Surv(duration,duration<365) ~ arm, type="kaplan-meier", conf.type="log-log", data=subject)
summary(km.by.trt)
dev.off()
#Plot without CIs
plot(km.by.trt, main=expression(paste("Kaplan-Meier-estimate by treatment")),
     xlab="days", ylab="Patients still in study", lwd=2,ylim=c(0.8,1),col=c(1,2),lty=2:3,conf.int=F)
legend(50, .85, levels(subject$arm), col=1:2,lty=2:3)



#KM-Plot with CIs
plot(km.by.trt, main=expression(paste("Kaplan-Meier-estimate with CI by treatment")),
     xlab="days", ylab="Patients still in study", lwd=2,ylim=c(0.8,1),col=c(1,2),lty=2:3,conf.int=T)
legend(50, .85, levels(subject$arm), col=1:2,lty=2:3) 


##########
#scatterplot of nosebleeds  by mucus.viscosity

library(car)
Arm=subject$arm
scatterplot(subject$mucus.viscosity,subject$nosebleeds, main="Nosebleeds by mucus viscosity",
   xlab=" Mucus Viscosity", ylab="# Nosebleeds",groups=Arm,jitter=list( y=1),las=1,smooth=F)

scatterplot(subject$mucus.viscosity,subject$inc.rate, main="Nosebleed inc. rate by mucus viscosity",
   xlab=" Mucus Viscosity", ylab="Nosebleeds incidence rate",groups=Arm,las=1,smooth=F)

scatterplot(subject$duration,subject$nosebleeds, main="Nosebleeds by duration",
   xlab="Duration", ylab="# Nosebleeds",groups=Arm,jitter=list( y=1),las=1,smooth=F)

scatterplot(subject$duration,subject$inc.rate, main="Nosebleed incidence rate by duration",
   xlab="Duration", ylab="Nosebleed incidence rate",groups=Arm,las=1,smooth=F)

scatterplot(subject$duration,subject$nosebleeds, main="Nosebleeds by duration",
   xlab="Duration", ylab="# Nosebleeds",groups=Arm,subset=subject$nosebleeds>0),jitter=list( y=1),las=1)

scatterplot(subject$duration,subject$inc.rate, main="Nosebleeds by duration",
   xlab="Duration", ylab="# Nosebleeds",groups=Arm,subset=which(subject$nosebleeds>0),las=1)


##########Patients with mucus visc <2.5

Arm=subject$arm[which((subject$mucus.2.5.ny==F))]
# #NB vs #NB previous year
scatterplot(data=subject[which((subject$mucus.2.5.ny==F)),],nosebleeds~previous.year, main="Nosebleeds by #previous year",
   xlab=" Nosebleeds Previous year", ylab="# Nosebleeds",groups=Arm,jitter=list( y=1),las=1,smooth=F)
# #NB inc. rate vs #NB previous year
scatterplot(data=subject[which((subject$mucus.2.5.ny==F)),],inc.rate~previous.year,main="Nosebleeds by #previous year",
   xlab=" Nosebleeds Previous year", ylab="Nosebleed incidence rate",groups=Arm,jitter=list( y=1),las=1,smooth=F)


##########Patients with mucus visc >=2.5

Arm=subject$arm[which((subject$mucus.2.5.ny==T))]
# #NB vs #NB previous year
scatterplot(data=subject[which((subject$mucus.2.5.ny==T)),],nosebleeds~previous.year, main="Nosebleeds by #previous year",
   xlab=" Nosebleeds Previous year", ylab="# Nosebleeds",groups=Arm,jitter=list( y=1),las=1,smooth=F)
# #NB inc. rate vs #NB previous year
scatterplot(data=subject[which((subject$mucus.2.5.ny==T)),],inc.rate~previous.year,main="Nosebleeds by #previous year",
   xlab="Nosebleeds Previous year", ylab="Nosebleed incidence rate",groups=Arm,jitter=list( y=1),las=1,smooth=F)


scatterplot(subject$previous.year,subject$mucus.viscosity, main="Mucus viscosity by #previous year",
   xlab=" Nosebleeds Previous year", ylab="Mucus viscosity",groups=Arm,jitter=list(x=1),las=1,smooth=F)



dev.off()
boxplot(inc.rate~eye.4cat+arm,data=subject)


#Tree for discrimination

#rpart to find candidate for threshold value
library(rpart)
trt.mucus.fit=rpart(nosebleeds ~ arm+mucus.viscosity, data=subject)
plot(trt.mucus.fit)
text(trt.mucus.fit,use.n=T)


#rpart on arm with tissue use
trt.at.fit=rpart(nosebleeds ~ arm+mucus.viscosity+tissue.use, data=subject)
plot(trt.at.fit)
text(trt.at.fit,use.n=T)



#rpart on all data
trt.all.fit=rpart(nosebleeds ~ arm+mucus.viscosity+duration+country+tissue.use+previous.year+eye.4cat, data=subject)
plot(trt.all.fit)
text(trt.all.fit,use.n=T)

#rpart on all data using inc.rate
trt.inc.fit=rpart(inc.rate ~ arm+mucus.viscosity+tissue.use+country+previous.year+eye.4cat, data=subject)
plot(trt.inc.fit)
text(trt.inc.fit,use.n=T)




### Negative Binomial regression model

require(MASS)
negbin.model.uni=glm.nb(nosebleeds ~trt+duration,data=subject)
summary(negbin.model.uni)
as.matrix(round(exp(negbin.model.uni$coef),3))


###Treatment effect as function of mucus viscosity
#Values of mucus viscosity
glm.nb.mucus=function(x,data)
	{
	coef=exp(glm.nb(nosebleeds ~trt+duration,data=data[which(data$mucus.viscosity>=x),])$coef[2])
	}
mucus.values=sort(unique(subject$mucus.viscosity))
n.m=length(mucus.values)
mucus.candidates=mucus.values[-c((n.m-10):n.m)]
effect.by.mucus=sapply(mucus.candidates,glm.nb.mucus,data=subject)
scatterplot(mucus.candidates,effect.by.mucus,main="Ratio Arm/Placebo for #Nosebleeds dependent on mucus viscosity",
   xlab=" Mucus Viscosity", ylab="Ratio Arm/Placebo for #Nosebleeds",las=1)
print(data.frame(mucus.candidates,effect.by.mucus))
#### #of patients considerd
#above/equal 2.5
sum(subject$mucus.2.5.ny,na.rm=T)
#lower than 2.5
sum(subject$mucus.2.5.ny==0,na.rm=T)


#####################
### Treatment effect as function of mucus viscosity - by tissue use or country or eye colour
mucus.scatter=function(data,class="",variable)
	{
	glm.nb.mucus=function(x,data)
		{
		coef=exp(glm.nb(nosebleeds ~trt+duration,data=data[which(data$mucus.viscosity>=x),])$coef[2])
		}
	mucus.values=sort(unique(data$mucus.viscosity[which(data[,variable]==class)]))
	n.m=length(mucus.values)
	mucus.candidates=mucus.values[-c((n.m-10):n.m)]
	effect.by.mucus=sapply(mucus.candidates,glm.nb.mucus,data=data[which(data[,variable]==class),])
	scatterplot(mucus.candidates,effect.by.mucus,
	main=paste("Ratio Arm/Placebo for #Nosebleeds dependent on mucus viscosity",class,variable,sep=" "),
	   xlab=" Mucus Viscosity", ylab="Ratio Arm/Placebo for #Nosebleeds",las=1)
	print(round(data.frame(mucus.candidates,effect.by.mucus),3))
	}
mucus.scatter(subject,"HIGH","tissue.use")
mucus.scatter(subject,"MEDIUM","tissue.use")
mucus.scatter(subject,"NA","eye.4cat")
mucus.scatter(subject,"BROWN","eye.4cat")
mucus.scatter(subject,"BLUE","eye.4cat")
mucus.scatter(subject,"BLACK","eye.4cat")
mucus.scatter(subject,"A","country")
mucus.scatter(subject,"B","country")
mucus.scatter(subject,"C","country")
mucus.scatter(subject,"D","country")
mucus.scatter(subject,"E","country")
mucus.scatter(subject,"F","country")
mucus.scatter(subject,"G","country")
mucus.scatter(subject,"H","country")
mucus.scatter(subject,"I","country")
mucus.scatter(subject,"J","country")




###############
########Negative binomial regression using cut-offs for mucus viscosity


### function for neg bin regression by cutoff
neg.bin.cutoff= function(formula,data,cutoff)
	{
par(mfrow=c(2,3))
	data$mucusny=ifelse(data$mucus.viscosity>=cutoff,1,0) #Mucus viscosity >= cutoff (y/n)
	negbin.model.high.musc=glm.nb(formula,data=data[which(data$mucusny==1),])
	cat(" Mucus viscosity greater equal ",cutoff,"\n")
	print(summary(negbin.model.high.musc))
	print(as.matrix(round(exp(negbin.model.high.musc$coef),3)))
	negbin.model.low.musc=glm.nb(formula,data=subject[which(data$mucusny==0),])
	cat(" Mucus viscosity lower ",cutoff,"\n")
	print(summary(negbin.model.low.musc))
	print(as.matrix(round(exp(negbin.model.low.musc$coef),3)))
	plot(data$mucus.visc[which(data$mucusny==1)],negbin.model.high.musc$residu)
	plot(data$duration[which(data$mucusny==1)],(negbin.model.high.musc)$residu)
	plot(data$nosebleeds[which(data$mucusny==1)],(negbin.model.high.musc)$residu)
	plot(data$mucus.visc[which(data$mucusny==0)],(negbin.model.low.musc)$residu)
	plot(data$duration[which(data$mucusny==0)],(negbin.model.low.musc)$residu)
	plot(data$nosebleeds[which(data$mucusny==0)],(negbin.model.low.musc)$residu)
}
neg.bin.cutoff(nosebleeds ~trt+duration,subject,0.3804)
neg.bin.cutoff(nosebleeds ~trt+duration,subject,1)
neg.bin.cutoff(nosebleeds ~trt+duration,subject[which(subject$tissue.use=="HIGH"),],2.5)

#### #of patients considered
sum(subject$mucus.2.5.ny*(subject$tissue.use=="HIGH"),na.rm=T)

neg.bin.cutoff(nosebleeds ~trt+duration,subject,2.5)

#Inclusion of previous year into model
neg.bin.cutoff(nosebleeds ~trt+duration+previous.year,subject,0.3804)
neg.bin.cutoff(nosebleeds ~trt+duration+previous.year,subject,1)
neg.bin.cutoff(nosebleeds ~trt+duration+previous.year,subject,2.5)
neg.bin.cutoff(nosebleeds ~trt+duration+previous.year,subject[which(subject$tissue.use=="HIGH"),],2.5)
neg.bin.cutoff(nosebleeds ~trt+previous.year,subject[which(subject$tissue.use=="MEDIUM"),],2.5)
#### #of patients considerd
sum((subject$mucus.viscosity>=1.5)*(subject$tissue.use=="MEDIUM"),na.rm=T)



#Inclusion of previous year * treatment interaction
neg.bin.cutoff(nosebleeds ~duration+previous.year*trt,subject,0.3804)
neg.bin.cutoff(nosebleeds ~trt+duration+previous.year*trt,subject,1)
neg.bin.cutoff(nosebleeds ~duration+previous.year*trt,subject,2.5)
neg.bin.cutoff(nosebleeds ~duration+previous.year*trt,subject[which(subject$tissue.use=="HIGH"),],2.5)
neg.bin.cutoff(nosebleeds ~duration+previous.year*trt,subject[which(subject$tissue.use=="MEDIUM"),],2.5)




###Other neg binomial models using adjustments

negbin.model.comp=glm.nb(nosebleeds ~trt+tissue.use+mucus.viscosity+previous.year+duration,data=subject)
summary(negbin.model.comp)
as.matrix(round(exp(negbin.model.comp$coef),3))

negbin.model.eye=glm.nb(nosebleeds ~arm+tissue.use+mucus.viscosity+previous.year+BLACK+BLUE+BROWN,data=subject)
summary(negbin.model.eye)

negbin.model.high.musc=glm.nb(nosebleeds ~arm+tissue.use+previous.year,data=subject[which(subject$mucus.3804.ny==1),])
summary(negbin.model.high.musc)

negbin.model.low.musc=glm.nb(nosebleeds ~arm+tissue.use+previous.year,data=subject[which(subject$mucus.3804.ny==0),])
summary(negbin.model.low.musc)

negbin.model.am.inter=glm.nb(nosebleeds ~arm+tissue.use+mucus.3804.ny*arm+previous.year,data=subject)
summary(negbin.model.am.inter)










### Poisson regression and overdispersion check

poiss.model.2.5=glm(nosebleeds ~trt+duration,family=poisson(link=log),data=subject[which(subject$mucus.viscosity>=2.5),])
summary(poiss.model.2.5)
pchisq(poiss.model.2.5$deviance, poiss.model.2.5$df.residual, lower.tail=F)
round(as.matrix(exp(poiss.model.2.5$coef)),3)
#No overdispersion here, but more or less same results


poiss.model.uni=glm(nosebleeds ~arm+duration,family=poisson(link=log),data=subject)
summary(poiss.model.uni)
pchisq(poiss.model.uni$deviance, poiss.model.uni$df.residual, lower.tail=F)

poiss.model.comp=glm(nosebleeds ~arm+tissue.use+mucus.viscosity+previous.year,family=poisson(link=log),data=subject)
summary(poiss.model.comp)
pchisq(poiss.model.comp$deviance, poiss.model.comp$df.residual, lower.tail=F)


poiss.model.eye=glm(nosebleeds ~arm+tissue.use+mucus.viscosity+previous.year+BLACK+BLUE+BROWN,family=poisson(link=log),data=subject)
summary(poiss.model.eye)
pchisq(poiss.model.eye$deviance, poiss.model.eye$df.residual, lower.tail=F)



poiss.model.high.musc=glm(nosebleeds ~arm+tissue.use+previous.year,family=poisson(link=log),data=subject[which(subject$mucus.3804.ny==1),])
summary(poiss.model.high.musc)
pchisq(poiss.model.high.musc$deviance, poiss.model.high.musc$df.residual, lower.tail=F)

poiss.model.low.musc=glm(nosebleeds ~arm+tissue.use+previous.year,family=poisson(link=log),data=subject[which(subject$mucus.3804.ny==0),])
summary(poiss.model.low.musc)
pchisq(poiss.model.low.musc$deviance, poiss.model.low.musc$df.residual, lower.tail=F)

#Check Country 


poisson.model.prev.year=glm(previous.year ~country+tissue.use,family=poisson(link=log),data=subject)
summary(poisson.model.prev.year)
pchisq(poisson.model.prev.year$deviance, poisson.model.prev.year$df.residual, lower.tail=F)

poisson.model.NBC=glm(nosebleeds ~arm*mucus.2.5.ny+country+tissue.use,family=poisson(link=log),data=subject)
summary(poisson.model.NBC)
pchisq(poisson.model.NBC$deviance, poisson.model.NBC$df.residual, lower.tail=F)

poisson.model.prev.year.e4=glm(previous.year ~eye.4cat+tissue.use,family=poisson(link=log),data=subject)
summary(poisson.model.prev.year.e4)
pchisq(poisson.model.prev.year.e4$deviance, poisson.model.prev.year.e4$df.residual, lower.tail=F)

########## Other models, not used






#linear model
linearMod1 <- lm(nosebleeds ~ trt, data=subject)  # linear regression for nosebleeds using treatment
print(linearMod1)

linearMod2 <- lm(nosebleeds~ trt+tissueny+mucus.viscosity+previous.year, data=subject)  # linear regression for nosebleeds using treatment
print(linearMod2)

linearMod3 <- lm(nosebleeds~ trt+tissueny+mucus.viscosity+previous.year+A+B+C+D+E+F+G+H+I+BLACK+BLUE+BROWN, data=subject)  # linear regression for nosebleeds using treatment
print(linearMod3)

#Check Country or eye colour

linearMod4 <- lm(previous.year ~A+B+C+D+E+F+G+H+I+tissue.use,data=subject)
summary(linearMod4)
linearMod5 <- lm(nosebleeds ~A+B+C+D+E+F+G+H+I+tissue.use+arm*mucus.2.5.ny,data=subject)
summary(linearMod5)
linearMod6 <- lm(previous.year ~BLACK+BLUE+BROWN,data=subject)
summary(linearMod6)
linearMod7 <- lm(nosebleeds ~BLACK+BLUE+BROWN,data=subject)
summary(linearMod7)


#Logistic Regression models
LRmodel1 <- glm(noseblny ~arm,family=binomial(link='logit'),data=subject)
summary(LRmodel1)

LRmodel2 <- glm(noseblny ~arm +tissue.use+mucus.viscosity+previous.year,family=binomial(link='logit'),data=subject)
summary(LRmodel2)

LRmodel3 <- glm(noseblny ~arm +tissue.use+mucus.viscosity+previous.year+A+B+C+D+E+F+G+H+I+BLACK+BLUE+BROWN,family=binomial(link='logit'),data=subject)
summary(LRmodel3)



#Subjects with medium tissue and mucus viscosity between 1.5 and 2.2

subject.medium=subject[which(subject$tissue.use=="MEDIUM"),]

subject.medium$mucus.1.5.2.2=ifelse(subject.medium$mucus.viscosity<2.2,1,0)*ifelse(subject.medium$mucus.viscosity>1.5,1,0)



#Total by Mucus for 1.5 - 2.2 (y/n)
tablex.1 <- CreateTableOne(vars=listVars[-c(1,4,6)], data = subject.medium, factorVars = catVars, strata = c("mucus.1.5.2.2"))
tablex.1


