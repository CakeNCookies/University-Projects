library(ggplot2)
library(gridExtra)
library(dplyr)
library(gamlss)
library(gamlss.mx)
library(tidyverse)  
library(gridExtra)
library(corrplot)
library(plot3D) 
library(factoextra)
#library(clustertend)
library(hopkins)
library(NbClust)
library(cluster)
library(fpc)
library(clValid)
library(mclust)
library(labstatR)

#install.packages("labstatR")

mydata <- read.csv("bodyPerformance.csv",
                   stringsAsFactors = TRUE)
head(mydata)

mydata <- mydata %>% 
  rename("AGE" = "age",
         "GENDER" = "gender",
         "HEIGHT" = "height_cm",
         "WEIGHT" = "weight_kg",
         "BODYFAT" = "body.fat_.",
         "DIASTOLIC" = "diastolic",
         "SYSTOLIC" = "systolic",
         "GRIP_FORCE" = "gripForce",
         "BEND_FWD" = "sit.and.bend.forward_cm",
         "SIT_UPS" = "sit.ups.counts",
         "BROAD_JUMP" = "broad.jump_cm",
         "CLASS" = "class")
head(mydata)
str(mydata)
summary(mydata)
#BODYFAT <- BODYFAT /100 #DIVIDING TO MAKE IT [0,1], BEING THE VARIABLE A PERCENTAGE
#detach(mydata)
#search()
attach(mydata)

#the dataset has a great number of observations and the computing times were too long;
#for the sake of this report i sampled the original dataset.
set.seed(123)
mydata <- mydata[sample(nrow(mydata), 1300, replace = FALSE), ]
head(mydata)
str(mydata)
summary(mydata)


#?gamlss.family

# Scale data for comparison
numdata <- mydata[, -c(2,12)]
scaled_df <- apply(numdata, 2, scale)
boxplot(scaled_df)
#there are a lot of outliers.
#Outliers in each variable will be treated differently 


## Categorical Variable: Gender

summary(GENDER)
barplot(table(GENDER))

GENDER_CLASS <- table(CLASS, GENDER)

barplot(GENDER_CLASS,beside = TRUE,legend = 
          rownames(GENDER_CLASS),cex.axis = 0.7,cex.names = 0.6, axes = 
          TRUE,axis.lty = 1, main = "Classes by Gender", args.legend = list(x = "topright", bty = "o"),col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")) 

## Ordinal Variable: Class

summary(CLASS)
barplot(table(CLASS))

#The datased is evenly distributed across the 4 classes.
#Will beused as an external validation variable later in the analysis


### Discrete Variables: all in [0,+infinity] 
# Age
summary(AGE)
barplot(table(AGE))

AGE_CLASS <- table(CLASS, AGE)
barplot(AGE_CLASS, legend = rownames(AGE_CLASS),cex.axis = 0.7,cex.names = 0.6, axes = 
          TRUE,axis.lty = 1, main = "Classes by Age", args.legend = list(x = "topright", bty = "o"),col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))

AGE_GENDER <- (table(GENDER, AGE))
barplot(AGE_GENDER, legend = rownames(AGE_GENDER),cex.axis = 0.7,cex.names = 0.6, axes = 
          TRUE,axis.lty = 1, main = "Gender by Age", args.legend = list(x = "topright", bty = "o"),col= c("#2E9FDF", "#00AFBB"))

fit.GEOM <- histDist(AGE, family=GEOM(), main="Geometric distribution")
fit.LG <- histDist(AGE, family=LG(), main="Logarithmic distribution")
fit.NB <- histDist(AGE, family=NBI(), main="Negative Binomial distribution")
fit.PO <- histDist(AGE, family=PO(), main="Poisson distribution")


AIC(fit.GEOM,fit.LG,fit.NB,fit.PO) #NB is the best model

BICLL <-data.frame()
BICLL[1,1] <- "Geometric"
BICLL[1,2] <- fit.GEOM$sbc
BICLL[1,3] <- logLik(fit.GEOM)
BICLL[2,1] <- "Logarithmic"
BICLL[2,2] <- fit.LG$sbc
BICLL[2,3] <- logLik(fit.LG)
BICLL[3,1] <- "Neg. Binomial"
BICLL[3,2] <- fit.NB$sbc
BICLL[3,3] <- logLik(fit.NB)
BICLL[4,1] <- "Poisson"
BICLL[4,2] <- fit.PO$sbc
BICLL[4,3] <- logLik(fit.PO)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Negative Binomial is the best model


# Sit Ups 
summary(SIT_UPS)
hist(SIT_UPS,prob=TRUE,breaks = 30) 
lines(density(SIT_UPS), col='darkorange1')

SITUPS_GENDER <- table(GENDER, SIT_UPS)
barplot(SITUPS_GENDER, legend = rownames(SITUPS_GENDER),cex.axis = 0.7,cex.names = 0.6, axes = 
          TRUE,axis.lty = 1, main = "Situps by gender", args.legend = list(x = "topright", bty = "o"),col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))
#there are two strange values so we need to truncate the values
trunc_SIT_UPS <- trunc(SIT_UPS)
SITUPS_GENDER <- table(GENDER, trunc_SIT_UPS)
barplot(SITUPS_GENDER, legend = rownames(SITUPS_GENDER),cex.axis = 0.7,cex.names = 0.6, axes = 
          TRUE,axis.lty = 1, main = "Situps by gender", args.legend = list(x = "topright", bty = "o"),col= c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))



fit.BNB <- histDist(trunc_SIT_UPS, family=BNB(), main="Beta Negative Binomial distribution")
fit.PO <- histDist(trunc_SIT_UPS, family=PO(), main="Poisson distribution")
fit.NB <- histDist(trunc_SIT_UPS, family=NBI(), main="Negative Binomial distribution")

AIC(fit.BNB,fit.NB,fit.PO) #NB is the best model

BICLL <-data.frame()
BICLL[1,1] <- "BNB"
BICLL[1,2] <- fit.BNB$sbc
BICLL[1,3] <- logLik(fit.BNB)
BICLL[2,1] <- "Poisson"
BICLL[2,2] <- fit.PO$sbc
BICLL[2,3] <- logLik(fit.LG)
BICLL[3,1] <- "Neg. Binomial"
BICLL[3,2] <- fit.NB$sbc
BICLL[3,3] <- logLik(fit.NB)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Negative Binomial is the best model



### Continuous Variables: 

# Height
summary(HEIGHT)
hist(HEIGHT,prob=TRUE,breaks = 30) 
lines(density(HEIGHT), col='darkorange1') 

box_plot<-boxplot(HEIGHT)
print(box_plot$out)

length(HEIGHT)

quartiles <- quantile(HEIGHT, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(HEIGHT)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

HEIGHT_NOUT <- subset(HEIGHT, HEIGHT> lower & HEIGHT < upper)
length(HEIGHT_NOUT)
boxplot(HEIGHT_NOUT)
hist(HEIGHT_NOUT,prob=TRUE,breaks = 30) 
lines(density(HEIGHT_NOUT), col='darkorange1')


kurt(HEIGHT) # LEPTOKURTIC
skew(HEIGHT) # slightly negatively skewed
kurt(HEIGHT_NOUT) # Platykurtic
skew(HEIGHT_NOUT) # slightly negatively skewed

par(mfrow=c(3,2))
fit.EXP <- histDist(HEIGHT, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(HEIGHT, family=GA, nbins = 30, main="Gamma distribution") #best one
fit.IG <- histDist(HEIGHT, family=IG, nbins = 30, main="Iinverse Gaussian distribution")
fit.LOGNO <- histDist(HEIGHT, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(HEIGHT, family=WEI, nbins = 30, main="Weibull distribution") 

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI)
LR.test(fit.EXP,fit.GA)
BICLL <-data.frame()
BICLL[1,1] <- "Exponential"
BICLL[1,2] <- fit.EXP$sbc
BICLL[1,3] <- logLik(fit.EXP)
BICLL[2,1] <- "Gamma"
BICLL[2,2] <- fit.GA$sbc
BICLL[2,3] <- logLik(fit.GA)
BICLL[3,1] <- "Inv. Gauss."
BICLL[3,2] <- fit.IG$sbc
BICLL[3,3] <- logLik(fit.IG)
BICLL[4,1] <- "Logno"
BICLL[4,2] <- fit.LOGNO$sbc
BICLL[4,3] <- logLik(fit.LOGNO)
BICLL[5,1] <- "Weibull"
BICLL[5,2] <- fit.WEI$sbc
BICLL[5,3] <- logLik(fit.WEI)

colnames(BICLL) <- c("Distribution", "BIC", "LL")
print(BICLL) #Gamma has the lowest BIC 

## Mixture Model 

XX <- HEIGHT
fit.GA.2 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 2, data = NULL)
str(fit.GA.2)
logLik(fit.GA.2)
fit.GA.2$prob # mixture weights
fitted(fit.GA.2, "mu")[1]

mu.hat1 <- exp(fit.GA.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.2[["models"]][[1]][["sigma.coefficients"]])
mu.hat2 <- exp(fit.GA.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.2[["models"]][[2]][["sigma.coefficients"]])

hist(XX, breaks = 50,freq = FALSE, main = "Mixture Model GA2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI,fit.GA.2) #Mixture model has best AIC
BICLL[6,1] <- "mix.GA.2"
BICLL[6,2] <- fit.GA.2$sbc
BICLL[6,3] <- logLik(fit.GA.2)

print(BICLL) #mixture model is the best one


# Weight
summary(WEIGHT)
hist(WEIGHT,prob=TRUE,breaks = 30) 
lines(density(WEIGHT), col='darkorange1') 

box_plot<-boxplot(WEIGHT)
print(box_plot$out)
length(WEIGHT)

quartiles <- quantile(WEIGHT, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(WEIGHT)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

WEIGHT_NOUT <- subset(WEIGHT, WEIGHT> lower & WEIGHT < upper)
length(WEIGHT_NOUT)
boxplot(WEIGHT_NOUT)

kurt(WEIGHT) # LEPTOKURTIC
skew(WEIGHT) # positively skewed
kurt(WEIGHT_NOUT) # Platykurtic
skew(WEIGHT_NOUT) # slightly positively skewed


fit.EXP <- histDist(WEIGHT_NOUT, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(WEIGHT_NOUT, family=GA, nbins = 30, main="Gamma distribution") #Best one
fit.IG <- histDist(WEIGHT_NOUT, family=IG, nbins = 30, main="Iinverse Gaussian distribution")
fit.LOGNO <- histDist(WEIGHT_NOUT, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(WEIGHT_NOUT, family=WEI, nbins = 30, main="Weibull distribution")


AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI) #gamma has the lowest AIC
LR.test(fit.EXP,fit.GA)
BICLL <-data.frame()
BICLL[1,1] <- "Exponential"
BICLL[1,2] <- fit.EXP$sbc
BICLL[1,3] <- logLik(fit.EXP)
BICLL[2,1] <- "Gamma"
BICLL[2,2] <- fit.GA$sbc
BICLL[2,3] <- logLik(fit.GA)
BICLL[3,1] <- "Inv. Gauss."
BICLL[3,2] <- fit.IG$sbc
BICLL[3,3] <- logLik(fit.IG)
BICLL[4,1] <- "Logno"
BICLL[4,2] <- fit.LOGNO$sbc
BICLL[4,3] <- logLik(fit.LOGNO)
BICLL[5,1] <- "Weibull"
BICLL[5,2] <- fit.WEI$sbc
BICLL[5,3] <- logLik(fit.WEI)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Gamma has the lowest BIC


### mistrure model
XX <- WEIGHT_NOUT
fit.GA.2 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 2, data = NULL)
str(fit.GA.2)
logLik(fit.GA.2)
fit.GA.2$prob # mixture weights
fitted(fit.GA.2, "mu")[1]

mu.hat1 <- exp(fit.GA.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.2[["models"]][[1]][["sigma.coefficients"]])
mu.hat2 <- exp(fit.GA.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.2[["models"]][[2]][["sigma.coefficients"]])

hist(XX, breaks = 50,freq = FALSE, main = "Mixture Model GA2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI,fit.GA.2) #Mixture model has best AIC
BICLL[6,1] <- "mix.GA.2"
BICLL[6,2] <- fit.GA.2$sbc
BICLL[6,3] <- logLik(fit.GA.2)

print(BICLL) #mixture model is the best one





# Bodyfat
summary(BODYFAT)
hist(BODYFAT,prob=TRUE,breaks = 30) 
lines(density(BODYFAT), col='darkorange1')

box_plot<-boxplot(BODYFAT)
print(box_plot$out)
length(BODYFAT)

quartiles <- quantile(BODYFAT, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(BODYFAT)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

BODYFAT_NOUT <- subset(BODYFAT, BODYFAT> lower & BODYFAT < upper)
length(BODYFAT_NOUT)
boxplot(BODYFAT_NOUT)

kurt(BODYFAT) # LEPTOKURTIC
skew(BODYFAT) # positively skewed
kurt(BODYFAT_NOUT) # Platykurtic
skew(BODYFAT_NOUT) # slightly positively skewed

fit.BE <- histDist(BODYFAT_NOUT, family = BE, nbins = 30, main="Beta distribution")
fit.LOGITNO <- histDist(BODYFAT_NOUT, family = LOGITNO, nbins = 30, main="Logitnormal distribution")

AIC(fit.BE, fit.LOGITNO) #LOGITNORMAL

BICLL <-data.frame()
BICLL[1,1] <- "Beta Dist."
BICLL[1,2] <- fit.BE$sbc
BICLL[1,3] <- logLik(fit.BE)
BICLL[2,1] <- "Logitno"
BICLL[2,2] <- fit.LOGITNO$sbc
BICLL[2,3] <- logLik(fit.LOGITNO)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #logitno has best BIC




# Diastolic
summary(DIASTOLIC)
hist(DIASTOLIC,prob=TRUE,breaks = 30) 
lines(density(DIASTOLIC), col='darkorange1')
box_plot<-boxplot(DIASTOLIC)
print(box_plot$out)
length(DIASTOLIC)

quartiles <- quantile(DIASTOLIC, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(DIASTOLIC)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

DIASTOLIC_NOUT <- subset(DIASTOLIC, DIASTOLIC> lower & DIASTOLIC < upper)
length(DIASTOLIC_NOUT)
boxplot(DIASTOLIC_NOUT)

kurt(DIASTOLIC) # LEPTOKURTIC
skew(DIASTOLIC) # slightly negatively skewed
kurt(DIASTOLIC_NOUT) # Platykurtic
skew(DIASTOLIC_NOUT) # slightly negatively skewed

fit.EXP <- histDist(DIASTOLIC_NOUT, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(DIASTOLIC_NOUT, family=GA, nbins = 30, main="Gamma distribution") #best one
fit.IG <- histDist(DIASTOLIC_NOUT, family=IG, nbins = 30, main="Iinverse Gaussian distribution")
fit.LOGNO <- histDist(DIASTOLIC_NOUT, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(DIASTOLIC_NOUT, family=WEI, nbins = 30, main="Weibull distribution") 

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI)
LR.test(fit.EXP,fit.GA)
BICLL <-data.frame()
BICLL[1,1] <- "Exponential"
BICLL[1,2] <- fit.EXP$sbc
BICLL[1,3] <- logLik(fit.EXP)
BICLL[2,1] <- "Gamma"
BICLL[2,2] <- fit.GA$sbc
BICLL[2,3] <- logLik(fit.GA)
BICLL[3,1] <- "Inv. Gauss."
BICLL[3,2] <- fit.IG$sbc
BICLL[3,3] <- logLik(fit.IG)
BICLL[4,1] <- "Logno"
BICLL[4,2] <- fit.LOGNO$sbc
BICLL[4,3] <- logLik(fit.LOGNO)
BICLL[5,1] <- "Weibull"
BICLL[5,2] <- fit.WEI$sbc
BICLL[5,3] <- logLik(fit.WEI)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Gamma has the lowest BIC


# Systolic
summary(SYSTOLIC)
hist(SYSTOLIC,prob=TRUE,breaks = 30) 
lines(density(SYSTOLIC), col='darkorange1')

box_plot<-boxplot(SYSTOLIC)
print(box_plot$out)
length(SYSTOLIC)

quartiles <- quantile(SYSTOLIC, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(SYSTOLIC)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

SYSTOLIC_NOUT <- subset(SYSTOLIC, SYSTOLIC> lower & SYSTOLIC < upper)
length(SYSTOLIC_NOUT)
boxplot(SYSTOLIC_NOUT)


kurt(SYSTOLIC) # LEPTOKURTIC
skew(SYSTOLIC) # slightly negatively skewed
kurt(SYSTOLIC_NOUT) # Platykurtic
skew(SYSTOLIC_NOUT) # almost no skew
BICLL <-data.frame()


fit.EXP <- histDist(SYSTOLIC_NOUT, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(SYSTOLIC_NOUT, family=GA, nbins = 30, main="Gamma distribution") #best one
fit.IG <- histDist(SYSTOLIC_NOUT, family=IG, nbins = 30, main="Iinverse Gaussian distribution")
fit.LOGNO <- histDist(SYSTOLIC_NOUT, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(SYSTOLIC_NOUT, family=WEI, nbins = 30, main="Weibull distribution") 

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI)
LR.test(fit.EXP,fit.GA)
BICLL[1,1] <- "Exponential"
BICLL[1,2] <- fit.EXP$sbc
BICLL[1,3] <- logLik(fit.EXP)
BICLL[2,1] <- "Gamma"
BICLL[2,2] <- fit.GA$sbc
BICLL[2,3] <- logLik(fit.GA)
BICLL[3,1] <- "Inv. Gauss."
BICLL[3,2] <- fit.IG$sbc
BICLL[3,3] <- logLik(fit.IG)
BICLL[4,1] <- "Logno"
BICLL[4,2] <- fit.LOGNO$sbc
BICLL[4,3] <- logLik(fit.LOGNO)
BICLL[5,1] <- "Weibull"
BICLL[5,2] <- fit.WEI$sbc
BICLL[5,3] <- logLik(fit.WEI)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Gamma has the lowest BIC

# Grip Force
summary(GRIP_FORCE)
hist(GRIP_FORCE,prob=TRUE,breaks = 30) 
lines(density(GRIP_FORCE), col='darkorange1')

box_plot<-boxplot(GRIP_FORCE)
print(box_plot$out)
length(GRIP_FORCE)

quartiles <- quantile(GRIP_FORCE, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(GRIP_FORCE)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

GRIP_FORCE_NOUT <- subset(GRIP_FORCE, GRIP_FORCE> lower & GRIP_FORCE < upper)
length(GRIP_FORCE_NOUT)
boxplot(GRIP_FORCE_NOUT)


kurt(GRIP_FORCE) # Platykurtic
skew(GRIP_FORCE) # almost no skew
kurt(GRIP_FORCE_NOUT) # Platykurtic
skew(GRIP_FORCE_NOUT) # almost no skew


fit.EXP <- histDist(GRIP_FORCE_NOUT, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(GRIP_FORCE_NOUT, family=GA, nbins = 30, main="Gamma distribution") 
fit.IG <- histDist(GRIP_FORCE_NOUT, family=IG, nbins = 30, main="Iinverse Gaussian distribution")
fit.LOGNO <- histDist(GRIP_FORCE_NOUT, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(GRIP_FORCE_NOUT, family=WEI, nbins = 30, main="Weibull distribution") #best one

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI) #WEI has best AIC
LR.test(fit.EXP,fit.GA)
BICLL <-data.frame()
BICLL[1,1] <- "Exponential"
BICLL[1,2] <- fit.EXP$sbc
BICLL[1,3] <- logLik(fit.EXP)
BICLL[2,1] <- "Gamma"
BICLL[2,2] <- fit.GA$sbc
BICLL[2,3] <- logLik(fit.GA)
BICLL[3,1] <- "Inv. Gauss."
BICLL[3,2] <- fit.IG$sbc
BICLL[3,3] <- logLik(fit.IG)
BICLL[4,1] <- "Logno"
BICLL[4,2] <- fit.LOGNO$sbc
BICLL[4,3] <- logLik(fit.LOGNO)
BICLL[5,1] <- "Weibull"
BICLL[5,2] <- fit.WEI$sbc
BICLL[5,3] <- logLik(fit.WEI)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Weibull has the lowest BIC



### Mixture model:
XX <- GRIP_FORCE_NOUT
fit.GA.2 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 2, data = NULL)
str(fit.GA.2)
logLik(fit.GA.2)
fit.GA.2$prob # mixture weights
fitted(fit.GA.2, "mu")[1]

mu.hat1 <- exp(fit.GA.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.2[["models"]][[1]][["sigma.coefficients"]])

mu.hat2 <- exp(fit.GA.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.2[["models"]][[2]][["sigma.coefficients"]])

hist(XX, breaks = 50,freq = FALSE, main = "Mixture Model GA2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI,fit.GA.2) #Mixture model has best AIC
BICLL[6,1] <- "mix.GA.2"
BICLL[6,2] <- fit.GA.2$sbc
BICLL[6,3] <- logLik(fit.GA.2)

print(BICLL) #mixture model is the best one


#Bend Forward
#whole real line
summary(BEND_FWD)
hist(BEND_FWD,prob=TRUE,breaks = 30) 
lines(density(BEND_FWD), col='darkorange1')
box_plot<-boxplot(BEND_FWD)
print(box_plot$out)
length(BEND_FWD)

quartiles <- quantile(BEND_FWD, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(BEND_FWD)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

BEND_FWD_NOUT <- subset(BEND_FWD, BEND_FWD> lower & BEND_FWD < upper)
length(BEND_FWD_NOUT)
boxplot(BEND_FWD_NOUT)

kurt(BEND_FWD) # Extremely Leptokurtic
skew(BEND_FWD) # Positively skewed
kurt(BEND_FWD_NOUT) # Platykurtic
skew(BEND_FWD_NOUT) # negatively skewed

fit.LO <- histDist(BEND_FWD_NOUT, family=LO, nbins = 30, main="Logistic distribution")
fit.NO <- histDist(BEND_FWD_NOUT, family=NO, nbins = 30, main="Normal distribution") 
fit.JSU <- histDist(BEND_FWD_NOUT, family=JSU, nbins = 30, main="Johnson SU distribution")

AIC(fit.LO,fit.NO,fit.JSU) #JSU has best AIC
BICLL <-data.frame()
BICLL[1,1] <- "Logistic"
BICLL[1,2] <- fit.LO$sbc
BICLL[1,3] <- logLik(fit.LO)
BICLL[2,1] <- "Normal"
BICLL[2,2] <- fit.NO$sbc
BICLL[2,3] <- logLik(fit.NO)
BICLL[3,1] <- "Johnson SU"
BICLL[3,2] <- fit.JSU$sbc
BICLL[3,3] <- logLik(fit.JSU)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #JSU 


#Broad Jump
summary(BROAD_JUMP)
hist(BROAD_JUMP,prob=TRUE,breaks = 30) 
lines(density(BROAD_JUMP), col='darkorange1')
box_plot<-boxplot(BROAD_JUMP)
print(box_plot$out)
length(BROAD_JUMP)

quartiles <- quantile(BROAD_JUMP, probs = c(.25, .75),na.rm = FALSE)
IQR <- IQR(BROAD_JUMP)

lower <- quartiles[1] - 1.5*IQR
upper <- quartiles[2] + 1.5*IQR

BROAD_JUMP_NOUT <- subset(BROAD_JUMP, BROAD_JUMP> lower & BROAD_JUMP < upper)
length(BROAD_JUMP_NOUT)
boxplot(BROAD_JUMP_NOUT)


kurt(BROAD_JUMP) # mesokurtic
skew(BROAD_JUMP) # negatively skewed
kurt(BROAD_JUMP_NOUT) # Platykurtic
skew(BROAD_JUMP_NOUT) # negatively skewed


fit.EXP <- histDist(BROAD_JUMP_NOUT, family=EXP, nbins = 30, main="Exponential distribution")
fit.GA <- histDist(BROAD_JUMP_NOUT, family=GA, nbins = 30, main="Gamma distribution") 
fit.IG <- histDist(BROAD_JUMP_NOUT, family=IG, nbins = 30, main="Iinverse Gaussian distribution")
fit.LOGNO <- histDist(BROAD_JUMP_NOUT, family=LOGNO, nbins = 30, main="Log-Normal distribution")
fit.WEI <- histDist(BROAD_JUMP_NOUT, family=WEI, nbins = 30, main="Weibull distribution") #best one

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI) #WEI has best AIC
LR.test(fit.EXP,fit.GA)
BICLL <-data.frame()
BICLL[1,1] <- "Exponential"
BICLL[1,2] <- fit.EXP$sbc
BICLL[1,3] <- logLik(fit.EXP)
BICLL[2,1] <- "Gamma"
BICLL[2,2] <- fit.GA$sbc
BICLL[2,3] <- logLik(fit.GA)
BICLL[3,1] <- "Inv. Gauss."
BICLL[3,2] <- fit.IG$sbc
BICLL[3,3] <- logLik(fit.IG)
BICLL[4,1] <- "Logno"
BICLL[4,2] <- fit.LOGNO$sbc
BICLL[4,3] <- logLik(fit.LOGNO)
BICLL[5,1] <- "Weibull"
BICLL[5,2] <- fit.WEI$sbc
BICLL[5,3] <- logLik(fit.WEI)

colnames(BICLL) <- c("Distribution", "BIC", "LL")

print(BICLL) #Weibull has the lowest BIC



### Mixture model:
XX <- BROAD_JUMP_NOUT
fit.GA.2 <- gamlssMXfits(n = 5, XX~1, family = GA, K = 2, data = NULL)
str(fit.GA.2)
logLik(fit.GA.2)
fit.GA.2$prob # mixture weights
fitted(fit.GA.2, "mu")[1]

mu.hat1 <- exp(fit.GA.2[["models"]][[1]][["mu.coefficients"]])    
sigma.hat1 <- exp(fit.GA.2[["models"]][[1]][["sigma.coefficients"]])

mu.hat2 <- exp(fit.GA.2[["models"]][[2]][["mu.coefficients"]])    
sigma.hat2 <- exp(fit.GA.2[["models"]][[2]][["sigma.coefficients"]])

hist(XX, breaks = 50,freq = FALSE, main = "Mixture Model GA2")
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1),lty=2,lwd=3,col=2)
lines(seq(min(XX),max(XX),length=length(XX)),fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),lty=2,lwd=3,col=3)
lines(seq(min(XX),max(XX),length=length(XX)),
      fit.GA.2[["prob"]][1]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat1, sigma = sigma.hat1) +
        fit.GA.2[["prob"]][2]*dGA(seq(min(XX),max(XX),length=length(XX)), mu = mu.hat2, sigma = sigma.hat2),
      lty = 1, lwd = 3, col = 1)

AIC(fit.EXP,fit.GA,fit.IG,fit.LOGNO,fit.WEI,fit.GA.2) #Mixture model has best AIC
BICLL[6,1] <- "mix.GA.2"
BICLL[6,2] <- fit.GA.2$sbc
BICLL[6,3] <- logLik(fit.GA.2)

print(BICLL) #mixture model is the best one







#############################PCA ANALYSIS

pairs(scaled_df, gap=0, pch=16, asp=1)

corrmatrix <- cor(scaled_df)
corrmatrix


corrplot(cormatrix, type="upper", diag=FALSE, tl.col="black", tl.srt=45, tl.cex=0.7)

data.cov <- cov(scaled_df)
data.eigen <- eigen(data.cov)
str(data.eigen)


#Selecting the number of PCs:

# 1. calculating PVE and Cumulative PVE
PVE <- data.eigen$values/sum(data.eigen$values)
round(PVE,3)
cumPVE <- round(cumsum(PVE),digits = 3)
cumPVE

#plotting PVE to see the scree plot

PVEplot <- qplot(c(1:10), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# Cumulative PVE plot

cumPVE <- qplot(c(1:10), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab(NULL) + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0,1)


grid.arrange(PVEplot, cumPVE, ncol = 2)

# 2. Kaiser's rule
data.eigen$values[data.eigen$values>1] #results suggest 3 PCs



#looking at the loadings
phi <- data.eigen$vectors[,1:3]
phi <- -phi
row.names(phi) <- c("AGE","HEIGHT","WEIGHT","BODYFAT","DIASTOLIC","SYSTOLIC","GRIP_FORCE","BEND_FWD","SIT_UPS","BROAD_JUMP")
colnames(phi) <- c("PC1","PC2","PC3")
phi

PC1 <- scaled_df %*% phi[,1] 
PC2 <- scaled_df %*% phi[,2] 
PC3 <- scaled_df %*% phi[,3] 

PC <- data.frame( PC1, PC2, PC3) 
pairs(PC, gap = 0,pch= 20, col="navy")

#Plotting PCs against each other
f_s <- ggplot(PC, aes(PC1, PC2)) + 
  modelr::geom_ref_line(h = 0) + 
  modelr::geom_ref_line(v = 0) + 
  geom_point(pch=20, col = "navy")+ 
  xlab("PC 1") + 
  ylab("PC 2") 
f_t <- ggplot(PC, aes(PC1, PC3)) + 
  modelr::geom_ref_line(h = 0) + 
  modelr::geom_ref_line(v = 0) + 
  geom_point(pch=20, col = "navy")+ 
  xlab("PC 1") + 
  ylab("PC 3") 
s_t <- ggplot(PC, aes(PC2, PC3)) + 
  modelr::geom_ref_line(h = 0) + 
  modelr::geom_ref_line(v = 0) + 
  geom_point(pch=20, col = "navy")+ 
  xlab("PC 2") + 
  ylab("PC 3") 

grid.arrange(f_s,f_t,s_t,ncol = 2, nrow 
               = 2, as.table = FALSE) 


#plotting the first 3 PCs in 3D space
scatter3D(x=PC1,y=PC2,z=PC3,colvar = NULL, col="blue",pch=19,theta = 120, phi 
          = 200, xlab="PC1", ylab="PC2", zlab="PC3")







# CLUSTER ANALYSIS
# 1. Assessing Cluster Tendency

# Preliminaries and Graphical representation of the data
random_df <- apply(numdata, 2,
                   function(x){runif(length(x), min(x), max(x))})
random_df <- as.data.frame(random_df)
random_df <- scale(random_df) #standardizing the data

# Standardized df data

pairs(scaled_df, gap=0, pch=16)

# Standardized uniform random df data

pairs(random_df, gap=0, pch=16)

# Plot the standardized df data 

fviz_pca_ind(prcomp(scaled_df), title = "PCA Original Data",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom") #there seem to be at least two clusters

# Plot the random df data

fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())

# VAT algorithm

D_euc<-dist(scaled_df, method="euclidean")
D_man<-dist(scaled_df, method="manhattan")


VAT.euc<-fviz_dist(D_euc, show_labels=FALSE)+ labs(title="Euclidean distance")
VAT.man<-fviz_dist(D_man, show_labels=FALSE)+ labs(title="Manhattan distance")
VAT.ran<-fviz_dist(dist(random_df), show_labels=FALSE)+ labs(title="Random Data")
grid.arrange(VAT.euc,VAT.man,VAT.ran, ncol=2)
#we can see at least 2 clusteres in EUC and 3/4 in manhattan


# Statistical method: Hopkins Statistic

set.seed(123)
HPdata <- hopkins::hopkins(scaled_df, m=nrow(scaled_df)-1)
HPrandom <- hopkins::hopkins(random_df, m=nrow(random_df)-1)

HPdata #almost 1: conferms that there are clusters
HPrandom # almost exactly 0.5: there are no clusters




# Determining the optimal number of Clusters
# 1. Direct methods
# 1.1 Elbow method
fviz_nbclust(scaled_df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method") # there seem to be 2 elbows: one at 2 and one at 4 clusters


#1.2 Silhouette method
fviz_nbclust(scaled_df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method k-means") #silhouette for kmeans seems to suggest 2 clusters

fviz_nbclust(scaled_df, pam, method = "silhouette")+
  labs(subtitle = "Silhouette method PAM")+
  theme_classic()

#2 Statistical Testing Methods: Gap Statistic

# nboot waskept = 50 since the number of observatiobs nade the computational time too long. 
#Put 500 for report!!!

set.seed(123)
fviz_nbclust(scaled_df, kmeans, nstart = 25, method = "gap_stat", nboot = 100)+
  labs(subtitle = "Gap statistic method")

## NbClust function

nbkm <- NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
nbwa <- NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2")
nbsi <- NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "single")
nbco <- NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete")
nbav <- NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "average")
nbcen <- NbClust(scaled_df, distance = "euclidean", min.nc = 2, max.nc = 10, method = "centroid")

nbkmm <- NbClust(scaled_df, distance = "manhattan", min.nc = 2, max.nc = 10, method = "kmeans")
nbwam <- NbClust(scaled_df, distance = "manhattan", min.nc = 2, max.nc = 10, method = "ward.D2")
nbsim <- NbClust(scaled_df, distance = "manhattan", min.nc = 2, max.nc = 10, method = "single")
nbcom <- NbClust(scaled_df, distance = "manhattan", min.nc = 2, max.nc = 10, method = "complete")
nbavm <- NbClust(scaled_df, distance = "manhattan", min.nc = 2, max.nc = 10, method = "average")
nbcenm <- NbClust(scaled_df, distance = "manhattan", min.nc = 2, max.nc = 10, method = "centroid")




######## HIERARCHICAL CLUSTERING

ccoph <- data.frame()
### Euclidean

res.dist <- dist(scaled_df, method = "euclidean")
par(mfrow = c(3,2))
# 1 Euclidean - Ward
res.hc <- hclust(d = res.dist, method = "ward.D2")
fviz_dend(res.hc, cex = 0.5, main = "Dendogram Euc Ward")
res.coph <- cophenetic(res.hc)
ccoph[1,1] <- "Euc Ward"
ccoph[1,2] <- cor(res.dist, res.coph) # Correlation between cophenetic and original distances


# 2 Euclidean - Average
res.hc2 <- hclust(res.dist, method = "average")
fviz_dend(res.hc2, cex = 0.5, main = "Dendogram Euc Average")
ccoph[2,2] <- cor(res.dist, cophenetic(res.hc2))
ccoph[2,1] <- "Euc Avg"

# 4 Euclidean - Single
res.hc4 <- hclust(res.dist, method = "single")
fviz_dend(res.hc4, cex = 0.5, main = "Dendogram Euc Single")
ccoph[3,2] <- cor(res.dist, cophenetic(res.hc4))
ccoph[3,1] <- "Euc Sig"

# 5 Euclidean - Complete
res.hc5 <- hclust(res.dist, method = "complete")
fviz_dend(res.hc5, cex = 0.5, main = "Dendogram Euc Complete")
ccoph[4,2] <- cor(res.dist, cophenetic(res.hc5))
ccoph[4,1] <- "Euc Comp"


# 6 Euclidean - Centroid
res.hc6 <- hclust(res.dist, method = "centroid")
fviz_dend(res.hc6, cex = 0.5, main = "Dendogram Euc Centroid")
ccoph[5,2] <- cor(res.dist, cophenetic(res.hc6))
ccoph[5,1] <- "Euc Centr"

colnames(ccoph) <- c("Method", "Corr")

### Manhattan

res.dist2 <- dist(scaled_df, method = "manhattan")

# 3 Manhattan average
res.hc3 <- hclust(res.dist2, method = "average")
fviz_dend(res.hc3, cex = 0.5, main = "Dendogram Manhattan Average")
ccoph[6,2] <- cor(res.dist, cophenetic(res.hc3))
ccoph[6,1] <- "Manh Avg"

# 7 Manhattan Ward
res.hc7 <- hclust(res.dist2, method = "ward.D2")
fviz_dend(res.hc7, cex = 0.5, main = "Dendogram Manhattan Ward")
ccoph[7,2] <- cor(res.dist, cophenetic(res.hc7))
ccoph[7,1] <- "Manh Ward"

# 8 Manhattan Complete
res.hc8 <- hclust(res.dist2, method = "complete")
fviz_dend(res.hc8, cex = 0.5, main = "Dendogram Manhattan Complete")
ccoph[8,2] <- cor(res.dist, cophenetic(res.hc8))
ccoph[8,1] <- "Manh Complete"

# 9 Manhattan Single
res.hc9 <- hclust(res.dist2, method = "single")
fviz_dend(res.hc9, cex = 0.5, main = "Dendogram Manhattan Single")
ccoph[9,2] <- cor(res.dist, cophenetic(res.hc9))
ccoph[9,1] <- "Manh Single"

ccoph

#The methods that show the highest correlation with the original distances are Euc Avg and Manh Avg:
#I will show both 2 (best K) and 4 K

#The "second best" methods are the Ward distances

# 1. Euclidean average


grp2 <- cutree(res.hc2, k = 2) 
table(grp2) #we get a strange partition, so we this analysis won't be taken further

grp2 <- cutree(res.hc2, k = 4) 
table(grp2) 

# 2. MAnhattan avg

grp3 <- cutree(res.hc3, k = 2) 
table(grp3)

grp3 <- cutree(res.hc3, k = 4) 
table(grp3)

# Testing also for the other partitions methods
# Euclidean Single
grp4 <- cutree(res.hc4, k = 4) 
table(grp4)
grp4 <- cutree(res.hc4, k = 2) 
table(grp4)

# Euclidean Complete
grp5 <- cutree(res.hc5, k = 2) 
table(grp5)
grp5 <- cutree(res.hc5, k = 4) 
table(grp5)

# Euclidean Centroid
grp6 <- cutree(res.hc6, k = 2) 
table(grp6)
grp6 <- cutree(res.hc6, k = 4) 
table(grp6)

# Manhattan Single
grp9 <- cutree(res.hc9, k = 2) 
table(grp9)
grp9 <- cutree(res.hc9, k = 4) 
table(grp9)

#### All of the above methods had strange partitions of the data. Those who performed better are below:

# Euclidean - Ward with 2 clusters

grp <- cutree(res.hc, k = 2) 
table(grp)
fviz_dend(res.hc, k = 2, # Cut in four groups
          cex = 0.5, # label size
          main = "Euclidean - Ward K=2",
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups

#potting to original data
pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#E7B800")[grp])

#plotting to the PC space
fviz_cluster(list(data = scaled_df, cluster = grp),
             palette = c("#2E9FDF", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Euclidean - Ward with 4 clusters
grp <- cutree(res.hc, k = 4) 
table(grp)
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          main = "Euclidean - Ward K=4",
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups

pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")[grp])

fviz_cluster(list(data = scaled_df, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Manhattan - Ward with 2 clusters
grp7 <- cutree(res.hc7, k = 2) 
table(grp7)
fviz_dend(res.hc7, k = 2, # Cut in four groups
          cex = 0.5, # label size
          main = "Manhattan - Ward K=2",
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups

pairs(scaled_df, gap=0, pch=grp7, col=c("#2E9FDF", "#E7B800")[grp7])

fviz_cluster(list(data = scaled_df, cluster = grp7),
             palette = c("#2E9FDF", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Manhattan - Ward with 4 clusters
grp7 <- cutree(res.hc7, k = 4) 
table(grp7)
fviz_dend(res.hc7, k = 4, # Cut in four groups
          cex = 0.5, # label size
          main = "Manhattan - Ward K=4",
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups

pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")[grp7])

fviz_cluster(list(data = scaled_df, cluster = grp7),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

# Manhattan - Complete with 2 Clusters
grp8 <- cutree(res.hc8, k = 2) 
table(grp8)
fviz_dend(res.hc8, k = 2,
          cex = 0.5,
          main = "Manhattan - Complete K=2",
          k_colors = c("#2E9FDF", "#00AFBB"),
          color_labels_by_k = TRUE,
          rect = TRUE)

pairs(scaled_df, gap=0, pch=grp8, col=c("#2E9FDF", "#E7B800")[grp8])

fviz_cluster(list(data = scaled_df, cluster = grp8),
             palette = c("#2E9FDF", "#E7B800"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())

#Manhattan - Complete with 4 clusters
grp8 <- cutree(res.hc8, k = 4) 
table(grp8)

fviz_dend(res.hc8, k = 4,
          cex = 0.5, 
          main = "Manhattan - Complete K=4",
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          rect = TRUE) 

pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")[grp8])

fviz_cluster(list(data = scaled_df, cluster = grp8),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE, ggtheme = theme_minimal())






# AGglomerative NESting (Hierarchical Clustering)

res.agnes <- agnes(x =mydata, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward")
fviz_dend(res.agnes, cex = 0.6, k = 2, main = "Agglomerative Nesting K=2")
fviz_dend(res.agnes, cex = 0.6, k = 4, main = "Agglomerative Nesting K=4")

ccoph[10,2] <- cor(res.dist, cophenetic(res.agnes))
ccoph[10,1] <- "Agnes"


#checking Agnes k=2
agnesClust2 <- hcut(x = numdata, k=2,hc_func = "agnes",hc_method = 
                     "ward",hc_metric = "euclidean",stand = TRUE) 
grp102 <- agnesClust2$cluster
table(grp102)
pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#E7B800")[grp102])

#Checking Agnes k=4

agnesClust4 <- hcut(x = numdata, k=4,hc_func = "agnes",hc_method = 
                     "ward",hc_metric = "euclidean",stand = TRUE) 
grp104 <- agnesClust4$cluster
table(grp104)
pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")[grp104])



# DIvisive ANAlysis Clustering

res.diana <- diana(x = mydata, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean")
fviz_dend(res.diana, cex = 0.6, k = 2, main = "Divisive analysis Clustering K=2")
fviz_dend(res.diana, cex = 0.6, k = 4, main = "Divisive analysis Clustering K=4")
#k=4 is not convincing visually

ccoph[11,2] <- cor(res.dist, cophenetic(res.diana))
ccoph[11,1] <- "Diana"

#checking Diana k=2
dianaClust <- hcut(x = numdata, k=2,hc_func = "diana",hc_method = 
                     "average",hc_metric = "euclidean",stand = TRUE) 
grp11 <- dianaClust$cluster
table(grp11)
pairs(PC, gap = 0,pch= 20, col= c("darkorange1","forestgreen") 
      [dianaClust$cluster]) 
pairs(scaled_df, gap=0, pch=grp, col=c("#2E9FDF", "#E7B800")[grp11])

dianaClust <- hcut(x = numdata, k=2,hc_func = "diana",hc_method = 
                     "average",hc_metric = "euclidean",stand = TRUE) 



ccoph

###### Cluster Validation for hierarchical methods
# The cluster validation analysis will be applied to the methods that have been previosuly selected
## Internal Validation: 
## Silhouette Method

sil <- data.frame()

# 1. Euclidean - Ward k=2
hc.reseuward2 <- eclust(scaled_df, "hclust", k = 2, hc_metric = "euclidean",
                 hc_method = "ward.D2", graph = FALSE)
# silhouette plot
fviz_silhouette(hc.reseuward2, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Euc Ward K=2")
#sil info
silinfoeuward2 <- hc.reseuward2$silinfo
sil[1,1] <- "Euc Ward k=2"
sil[1,2] <- silinfoeuward2$avg.width

colnames(sil) <- c("Method", "Avg Sil")

# 2. Euclidean - Ward k=4
hc.reseuward4 <- eclust(scaled_df, "hclust", k = 4, hc_metric = "euclidean",
                        hc_method = "ward.D2", graph = FALSE)
fviz_silhouette(hc.reseuward4, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Euc Ward K=4")
silinfoeuward4 <- hc.reseuward4$silinfo
sil[2,1] <- "Euc Ward k=4"
sil[2,2] <- silinfoeuward4$avg.width

# 3. Manhattan - Ward k=2
hc.resmanward2 <- eclust(scaled_df, "hclust", k = 2, hc_metric = "manhattan",
                        hc_method = "ward.D2", graph = FALSE)
fviz_silhouette(hc.resmanward2, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Manh Ward K=2")

silinfomanward2 <- hc.resmanward2$silinfo
sil[3,1] <- "Manh Ward k=2"
sil[3,2] <- silinfomanward2$avg.width

# 4. Manhattan - Ward k=4
hc.resmanward4 <- eclust(scaled_df, "hclust", k = 4, hc_metric = "manhattan",
                        hc_method = "ward.D2", graph = FALSE)
fviz_silhouette(hc.resmanward4, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Manh Ward K=4")
silinfomanward4 <- hc.resmanward4$silinfo
sil[4,1] <- "Manh Ward k=4"
sil[4,2] <- silinfomanward4$avg.width

# 5. Manhattan - Complete k=2
hc.resmancomp2 <- eclust(scaled_df, "hclust", k = 2, hc_metric = "manhattan",
                         hc_method = "complete", graph = FALSE)
fviz_silhouette(hc.resmancomp2, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Manh Complete K=2")

silinfomancomp2 <- hc.resmancomp2$silinfo
sil[5,1] <- "Manh Comp k=2"
sil[5,2] <- silinfomancomp2$avg.width

# Manhattan - Complete k=4
hc.resmancomp4 <- eclust(scaled_df, "hclust", k = 4, hc_metric = "manhattan",
                         hc_method = "complete", graph = FALSE)
fviz_silhouette(hc.resmancomp4, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Manh Complete K=4")

silinfomancomp4 <- hc.resmancomp4$silinfo
sil[6,1] <- "Manh Comp k=4"
sil[6,2] <- silinfomancomp4$avg.width

#Agnes K=2
agnessil2 <- agnesClust2$silinfo
sil[7,1] <- "Agnes K=2"
sil[7,2] <- agnessil2$avg.width
fviz_silhouette(agnesClust2, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Agnes K=2")

#Agnes K=4
agnessil4 <- agnesClust4$silinfo
sil[8,1] <- "Agnes K=4"
sil[8,2] <- agnessil4$avg.width
fviz_silhouette(agnesClust4, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Agnes K=4")

#Diana K=2
dianasil <- dianaClust$silinfo
sil[9,1] <- "Diana K=2"
sil[9,2] <- dianasil$avg.width
fviz_silhouette(dianaClust, palette = "jco",
                ggtheme = theme_classic(),
                main = "Silhouette Diana K=2")

sil #Best performer is Manhattan Ward k=2, then Diana k=2


#Checking Manhattan Ward:
clustsil <- hc.reseuward2$silinfo$clus.avg.widths
clustsil
datasil <- hc.resmanward2$silinfo$widths[, 1:3]

# Units with negative silhouette widths

neg_sil_index <- which(datasil[, "sil_width"] < 0)
datasil[neg_sil_index, , drop = FALSE]   

neg_sil_index

#checking Diana:
clustsildiana <- dianaClust$silinfo$clus.avg.widths
clustsildiana
datasildiana <- dianaClust$silinfo$widths[, 1:3]

# Diana has no Units with negative silhouette widths



## Computing Dunn Index
DItable <- data.frame()
DIeucWard2 <- (cluster.stats(D_euc, hc.reseuward2$cluster))$dunn
DIeucWard4 <- (cluster.stats(D_euc, hc.reseuward4$cluster))$dunn 
DImanWard2 <- (cluster.stats(D_man, hc.resmanward2$cluster))$dunn
DImanWard4 <- (cluster.stats(D_man, hc.resmanward4$cluster))$dunn 
DImanComp2 <- (cluster.stats(D_man, hc.resmancomp2$cluster))$dunn  
DImancomp4 <- (cluster.stats(D_man, hc.resmancomp4$cluster))$dunn 
DIagnes2 <- (cluster.stats(D_euc, agnesClust2$cluster))$dunn 
DIagnes4 <- (cluster.stats(D_euc, agnesClust4$cluster))$dunn 
DIdiana <- (cluster.stats(D_euc, dianaClust$cluster))$dunn 

DItable[1,1] <- "DIeucWard2"
DItable[1,2] <- DIeucWard2
DItable[2,1] <- "DIeucWard4"
DItable[2,2] <- DIeucWard4
DItable[3,1] <- "DImanWard2"
DItable[3,2] <- DImanWard2
DItable[4,1] <- "DImanWard4"
DItable[4,2] <- DImanWard4
DItable[5,1] <- "DImanComp2"
DItable[5,2] <- DImanComp2
DItable[6,1] <- "DImancomp42"
DItable[6,2] <- DImancomp4
DItable[7,1] <- "DIagnes2"
DItable[7,2] <- DIagnes2
DItable[8,1] <- "DIagnes4"
DItable[8,2] <- DIagnes4
DItable[9,1] <- "DIdiana"
DItable[9,2] <- DIdiana

colnames(DItable) <- c("Method", "DI")
DItable #Clustering methods that divide by 4 have the highest dunn index



#### External Cluster Validation
## Confusion matrix, Corrected Rand Index and Meila VI
#Comparing k=4 to the CLASS variable and k=2 to the GENDER variable
EX_matrix <- data.frame()
gend <- as.numeric(mydata$GENDER)
cl <- as.numeric(mydata$CLASS)


table(mydata$GENDER, hc.reseuward2$cluster)
euward2_stats <- cluster.stats(d = D_euc, gend, hc.reseuward2$cluster)
EX_matrix[1,1] <- "euward2"
EX_matrix[1,2] <- euward2_stats$corrected.rand
EX_matrix[1,3] <- euward2_stats$vi


table(mydata$GENDER, hc.resmanward2$cluster)
manward2_stats <- cluster.stats(d = D_man, gend, hc.resmanward2$cluster)
EX_matrix[2,1] <- "manward2"
EX_matrix[2,2] <- manward2_stats$corrected.rand
EX_matrix[2,3] <- manward2_stats$vi


table(mydata$GENDER, hc.resmancomp2$cluster)
mancomp2_stats <- cluster.stats(d = D_man, gend, hc.resmancomp2$cluster)
EX_matrix[3,1] <- "mancomp2"
EX_matrix[3,2] <- mancomp2_stats$corrected.rand
EX_matrix[3,3] <- mancomp2_stats$vi


table(mydata$GENDER, agnesClust2$cluster)
agnesClust2_stats <- cluster.stats(d = D_euc, gend, agnesClust2$cluster)
EX_matrix[4,1] <- "agnesClust2"
EX_matrix[4,2] <- agnesClust2_stats$corrected.rand
EX_matrix[4,3] <- agnesClust2_stats$vi

table(mydata$GENDER, dianaClust$cluster)
dianaClust_stats <- cluster.stats(d = D_euc, gend, dianaClust$cluster)
EX_matrix[5,1] <- "dianaClust"
EX_matrix[5,2] <- dianaClust_stats$corrected.rand
EX_matrix[5,3] <- dianaClust_stats$vi



table(mydata$CLASS, hc.reseuward4$cluster)
euward4_stats <- cluster.stats(d = D_euc, cl, hc.reseuward4$cluster)
EX_matrix[6,1] <- "euward4"
EX_matrix[6,2] <- euward4_stats$corrected.rand
EX_matrix[6,3] <- euward4_stats$vi


table(mydata$CLASS, hc.resmanward4$cluster)
manward4_stats <- cluster.stats(d = D_man, cl, hc.resmanward4$cluster)
EX_matrix[7,1] <- "manward4"
EX_matrix[7,2] <- manward4_stats$corrected.rand
EX_matrix[7,3] <- manward4_stats$vi


table(mydata$CLASS, hc.resmancomp4$cluster)
mancomp4_stats <- cluster.stats(d = D_man, cl, hc.resmancomp4$cluster)
EX_matrix[8,1] <- "mancomp4"
EX_matrix[8,2] <- mancomp4_stats$corrected.rand
EX_matrix[8,3] <- mancomp4_stats$vi


table(mydata$CLASS, agnesClust4$cluster)
agnesClust4_stats <- cluster.stats(d = D_man, cl, agnesClust4$cluster)
EX_matrix[9,1] <- "agnesClust4"
EX_matrix[9,2] <- agnesClust4_stats$corrected.rand
EX_matrix[9,3] <- agnesClust4_stats$vi


colnames(EX_matrix) <- c("Method", "C.Rand.Ind.", "Meila VI")
EX_matrix

#Rand Index goes from [-1,1] and should be maximized while Meila's VI should be minimized.
#Both indexes agree on manward2. The clustering probably captures gender.



########## Partitioning Clustering
#we already saw that the optimal number of cluster estimated for kmeans is K=2 
##EUCLIDEAN distance

km.res <- eclust(scaled_df, "kmeans", k = 2, nstart = 25, graph = FALSE, hc_metric = "euclidean")
print(km.res)


aggregate(numdata, by=list(cluster=km.res$cluster), mean)
dd <- cbind(mydata, cluster = km.res$cluster)
head(dd)
km.res$size
cl <- km.res$cluster
pairs(scaled_df, gap=0, pch=cl, col=c("#2E9FDF","#E7B800")[cl])

fviz_cluster(km.res, 
             data = scaled_df,
             palette = c("#2E9FDF", "#E7B800"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

###Cluster validation for Kmeans:

# Silhouette
fviz_silhouette(km.res, palette = "jco", ggtheme = theme_classic())
silinfo <- km.res$silinfo

clussil <- silinfo$clus.avg.widths
totsil <- silinfo$avg.width
obssil <- km.res$silinfo$widths[, 1:3]

neg_sil_index <- which(obssil[, "sil_width"] < 0)
obssil[neg_sil_index, , drop = FALSE] #all units classified as 1 that should be in 2


# Dunn Index
km_stats <- cluster.stats(dist(scaled_df), km.res$cluster)
km_stats$dunn  


### External
# Confusion matrix
table(mydata$GENDER, km.res$cluster) 
kmeanclust_stats <- cluster.stats(d = dist(scaled_df), gend, km.res$cluster)

EX_matrix[10,1] <- "Kmeans2"
EX_matrix[10,2] <- kmeanclust_stats$corrected.rand
EX_matrix[10,3] <- kmeanclust_stats$vi
EX_matrix


############# Manhattan Distance

km.resman <- eclust(scaled_df, "kmeans", k = 2, nstart = 25, graph = FALSE, hc_metric = "manhattan")
print(km.resman)


aggregate(numdata, by=list(cluster=km.resman$cluster), mean)
dd <- cbind(mydata, cluster = km.resman$cluster)
head(dd)
km.resman$size
clman <- km.resman$cluster

table(km.res$cluster, km.resman$cluster) #they are identical




############# Kmedoids (PAM) - Euclidean
pam.res <- eclust(scaled_df, "pam", k = 2, graph = FALSE, hc_metric = "euclidean")

# Adding the point classifications to the original data

dd <- cbind(mydata, cluster = pam.res$cluster)
head(dd, n = 8)
(medoids <- pam.res$medoids)

cl <- pam.res$clustering
pairs(scaled_df, gap=0, pch=cl, col=c("#00AFBB", "#FC4E07")[cl])

fviz_cluster(pam.res, 
             data = scaled_df,
             palette = c("#2E9FDF", "#E7B800"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())

## Cluster Validation

## Internal 
# Silhouette
fviz_silhouette(pam.res, palette = "jco", ggtheme = theme_classic())
silinfo <- pam.res$silinfo

clussil <- silinfo$clus.avg.widths
totsil <- silinfo$avg.width
obssil <- pam.res$silinfo$widths[, 1:3]

neg_sil_index <- which(obssil[, "sil_width"] < 0)
obssil[neg_sil_index, , drop = FALSE]

# Dunn Index
pam_stats <- cluster.stats(dist(scaled_df), pam.res$cluster)
pam_stats$dunn  


##### External
### External
# Confusion matrix
table(mydata$GENDER, pam.res$cluster) 
pamclust_stats <- cluster.stats(d = dist(scaled_df), gend, pam.res$cluster)

EX_matrix[11,1] <- "PAM2"
EX_matrix[11,2] <- pamclust_stats$corrected.rand
EX_matrix[11,3] <- pamclust_stats$vi
EX_matrix




############# Kmedoids (PAM) - Manhattan
pamman.res <- eclust(scaled_df, "pam", k = 2, graph = FALSE, hc_metric = "manhattan")

# Adding the point classifications to the original data

ddman <- cbind(mydata, cluster = pamman.res$cluster)
head(ddman, n = 8)
(medoids <- pamman.res$medoids)

cl <- pamman.res$clustering
table(pam.res$cluster, pamman.res$cluster) #idential clusters






###################### Best Clustering algorithm

rownames(scaled_df) <- rownames(mydata)

##### Euclidean

clmethods <- c("hierarchical","kmeans","pam")
inteuc <- clValid(scaled_df, nClust = 2:6, clMethods = clmethods, validation = "internal", 
                  metric = "euclidean", method = "ward", maxitems = 10000)

optimalScores(inteuc)

## Stability measures

stabeuc <- clValid(scaled_df, nClust = 2:6, clMethods = clmethods,
                validation = "stability", metric = "euclidean", method = "ward", maxitems  = 10000)

optimalScores(stabeuc)


##### manhattan

clmethods <- c("hierarchical","kmeans","pam")
intman <- clValid(scaled_df, nClust = 2:6, clMethods = clmethods, validation = "internal", 
                  metric = "manhattan", method = "ward", maxitems  = 10000)
optimalScores(intman)

## Stability measures

stabman <- clValid(scaled_df, nClust = 2:6, clMethods = clmethods,
                   validation = "stability", metric = "manhattan", method = "ward", maxitems  = 10000)
optimalScores(stabman)


############## MODEL BASED CLUSTERING

mod <- Mclust(scaled_df, G = 1:9, modelNames = NULL) # Fit the parsimonious models
summary(mod$BIC) # Visualize the top three BIC models
plot(mod, what = "BIC", ylim = range(mod$BIC, na.rm = TRUE), legendArgs = list(x = "bottomleft")) # Plot of the BIC values for all the fitted models

summary(mod) # Visualize the main output

mod$modelName                # Selected parsimonious configuration
mod$G                        # Optimal number of clusters
head(round(mod$z, 6), 30)    # Probability to belong to a given cluster
head(mod$classification, 30) # Cluster assignment of each observation

pairs(scaled_df, gap=0, pch = 16, col = mod$classification)
table(mydata$CLASS, mod$classification)

adjustedRandIndex(mydata$CLASS, mod$classification) # Adjusted (or Corrected) Rand Index
fviz_mclust(mod, "BIC", palette = "jco") # BIC values used for choosing the number of clusters
fviz_mclust(mod, "classification", geom = "point", pointsize = 1.5, palette = "jco") # Classification uncertainty
fviz_mclust(mod, "uncertainty", palette = "jco") # larger symbols indicate the more uncertain observations
