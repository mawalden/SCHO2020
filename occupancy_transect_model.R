##############################
## Devan-Song, Anne, M. A. Walden, Haley A. Moniz, Justine M. Fox,
## Mary-Ruth Low, Emma Wilkinson, Scott W. Buchanan, and Nancy E. Karraker.
## "Confirmation Bias Perpetuates Century-Old Ecological Misconception:
## Evidence Against 'Secretive' Behavior of Eastern Spadefoots." Journal
## of Herpetology 55, no. 2 (2021): 137-150.
##
## R code to create occupancy model for Table 1 and Fig. 4
##
## Author: M. A. Walden
## Date: Sept. 29, 2020
## Contact: mar.walden@gmail.com
##############################



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
precip <- c(transect[,34],transect[,35],transect[,36],transect[,37],
            transect[,38],transect[,39],transect[,40],transect[,41])

blgr.obs <- data.frame(temper,rh,julian,precip)

##-- create unmarked dataframe
blgr <- unmarkedFrameOccu(y=y, obsCovs=blgr.obs)
summary(blgr)

##-- models
fm1 <- occu(~1 ~1, blgr) #constant detection, constant occupancy
fm2 <- occu(~julian ~1, blgr)
fm3 <- occu(~julian + temper ~1, blgr)
fm4 <- occu(~julian + rh ~1, blgr)
fm5 <- occu(~julian + precip ~1, blgr)
fm6 <- occu(~julian + temper + rh ~1, blgr)
fm7 <- occu(~julian + temper + precip ~1, blgr)
fm8 <- occu(~julian + rh + precip ~1, blgr)
fm9 <- occu(~julian + temper + rh +precip ~1, blgr)
fm10 <- occu(~temper ~1, blgr)
fm11 <- occu(~temper + rh ~1, blgr)
fm12 <- occu(~temper + precip ~1, blgr)
fm13 <- occu(~temper + rh + precip ~1, blgr)
fm14 <- occu(~rh ~1, blgr)
fm15 <- occu(~rh + precip ~1, blgr)
fm16 <- occu(~precip ~1, blgr)

##-- AIC
fmlist <- fitList(fm1=fm1,fm2=fm2,fm3=fm3,fm4=fm4,fm5=fm5,fm6=fm6,
                  fm7=fm7,fm8=fm8,fm9=fm9,fm10=fm10,fm11=fm11,
                  fm12=fm12,fm13=fm13,fm14=fm14,fm15=fm15,fm16=fm16)
modSel(fmlist)

logLik(fm2)
logLik(fm3)
logLik(fm5)
logLik(fm4)
logLik(fm7)
logLik(fm6)
logLik(fm8)
logLik(fm9)
logLik(fm16)
logLik(fm15)
logLik(fm12)
logLik(fm1)
logLik(fm13)
logLik(fm10)
logLik(fm14)
logLik(fm11)

#- Four models are within 2 AIC of the lowest:

fm2

#- What is the probability of occupancy?
backTransform(fm2, 'state')
#- The expected probability that a site was occupied is 1.00.

#- Probability of detection given that a site is occupied and all covariates are set to 0:
backTransform(linearComb(fm2, coefficients = c(1,0), type = 'det'))
#- The expected probability of detection was 1 when Julian is fixed at mean value.

#- Predict across range of Julian:
newData <- data.frame(Julian = -20:20)
round(predict(fm2, type='det',newdata=newData,appendData=TRUE),2)

#- confidence intervals
confint(fm2, type='det')

#- plot detection probability with temperature
#nd <- data.frame(temper=seq(from=min(blgr.obs$temper),to=max(blgr.obs$temper),length.out=50))
#E.det <- predict(fm6, type='det', newdata=nd, appendData=TRUE)

with(E.det, { 
  plot(temper, Predicted, ylim=c(0,1), type="l",
       xlab="Temperature (degrees Celsius)", ylab=expression( italic(p) ),
       cex.lab=0.8, cex.axis=0.8)
  lines(temper, upper, col=gray(0.7))
  lines(temper, lower, col=gray(0.7))
})

#- Model averaging mods 6, 7, and 3
modavg4 <- fitList(m1=fm2, m2=fm3, m3=fm5, m4=fm4) #- Create object of three top models
predict(modavg4, type="state") #- See predicted occupancy by transect number
ma4pred <- predict(modavg4, type="det") #- See predicted detection probability by survey
hist(ma3pred$Predicted)
mean(ma3pred$Predicted)
