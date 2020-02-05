##-- load libraries
library(unmarked)
library(svMisc)
library(abind)

##-- set working directory to data source folder
#setwd("")

##-- read in and format data
transect <- read.csv("transects_occupancy.csv",header=TRUE)
summary(transect)

##-- create detection matrix
y <- transect[,2:9]
n <- nrow(transect)

##-- no site-specific covariates

##-- survey-specific covariates
temper <- c(transect[,10],transect[,11],transect[,12],transect[,13],
            transect[,14],transect[,15],transect[,16],transect[,17])
rh <- c(transect[,18],transect[,19],transect[,20],transect[,21],
        transect[,22],transect[,23],transect[,24],transect[,25])
julian <- c(transect[,26],transect[,27],transect[,28],transect[,29],
            transect[,30],transect[,31],transect[,32],transect[,33])

blgr.obs <- data.frame(temper,rh,julian)

##-- create unmarked dataframe
blgr <- unmarkedFrameOccu(y=y, obsCovs=blgr.obs)
summary(blgr)

##-- models
fm1 <- occu(~1 ~1, blgr) #constant detection, constant occupancy
fm2 <- occu(~julian ~1, blgr)
fm3 <- occu(~julian + temper ~1, blgr)
fm4 <- occu(~julian + rh ~1, blgr)
fm5 <- occu(~julian + temper + rh ~1, blgr)
fm6 <- occu(~temper ~1, blgr)
fm7 <- occu(~temper + rh ~1, blgr)
fm8 <- occu(~rh ~1, blgr)

##-- AIC
fmlist <- fitList(fm1=fm1,fm2=fm2,fm3=fm3,fm4=fm4,fm5=fm5,fm6=fm6,fm7=fm7,fm8=fm8)
modSel(fmlist)

logLik(fm6)
logLik(fm7)
logLik(fm3)
logLik(fm5)
logLik(fm1)
logLik(fm2)
logLik(fm8)
logLik(fm4)

#- Three models are within 2 AIC of the lowest:
#- temperature; temperature + RH, and Julian day + temperature.
#- The most parsimonious model of those three is temperature.
fm6

#- What is the probability of occupancy?
backTransform(fm6, 'state')
#- The expected probability that a site was occupied is 1.00.

#- Probability of detection given that a site is occupied and all covariates are set to 0:
backTransform(linearComb(fm6, coefficients = c(1,0), type = 'det'))
#- The expected probability of detection was 1 when temperature is fixed at mean value.

#- Predict across range of temperatures:
newData <- data.frame(temper = -20:20)
round(predict(fm6, type='det',newdata=newData,appendData=TRUE),2)

#- confidence intervals
confint(fm6, type='det')

#- plot detection probability with temperature
nd <- data.frame(temper=seq(from=min(blgr.obs$temper),to=max(blgr.obs$temper),length.out=50))
E.det <- predict(fm6, type='det', newdata=nd, appendData=TRUE)

with(E.det, { 
  plot(temper, Predicted, ylim=c(0,1), type="l",
       xlab="Temperature (degrees Celsius)", ylab=expression( italic(p) ),
       cex.lab=0.8, cex.axis=0.8)
  lines(temper, upper, col=gray(0.7))
  lines(temper, lower, col=gray(0.7))
})

#- Model averaging mods 6, 7, and 3
modavg3 <- fitList(m1=fm6, m2=fm7, m3=fm3) #- Create object of three top models
predict(modavg3, type="state") #- See predicted occupancy by transect number
ma3pred <- predict(modavg3, type="det") #- See predicted detection probability by survey
hist(ma3pred$Predicted)
mean(ma3pred$Predicted)
