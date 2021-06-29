##############################
## Devan-Song, Anne, M. A. Walden, Haley A. Moniz, Justine M. Fox,
## Mary-Ruth Low, Emma Wilkinson, Scott W. Buchanan, and Nancy E. Karraker.
## "Confirmation Bias Perpetuates Century-Old Ecological Misconception:
## Evidence Against 'Secretive' Behavior of Eastern Spadefoots." Journal
## of Herpetology 55, no. 2 (2021): 137-150.
##
## R code to create occupancy model for Fig. 4
##
## Author: M. A. Walden
## Date: Sept. 29, 2020
## Contact: mar.walden@gmail.com
##############################

##-- set working directory to data source folder
#setwd("")

#------------------------------------------------------------------------------#
# Simulations - at three different detection probabilities, how does the power
# to correctly predict occupancy change by transect number and survey number?

###########
##Figures: change psi and p as needed, then save "results" to R Workspace for replotting figures
##as needed in the future without having to rerun the simulations
###########
transects <- 20
surveys <- 20
iter <- seq(1,1000,by=1)
iternum <- length(iter)

psi <- .95 ##Here is where you change occupancy as appropriate for your design
p <- .95  ##Here is where you change detection probability as appropriate for your design

home <- array(NA,dim=c(iternum,transects,surveys))

for(j in 1:transects){
  
  for(k in 1:surveys){
    
    dfrep <- data.frame(iter=iter,numocc=NA,numsim=NA,testp=NA)
    for(i in 1:length(iter)){
      z <- rbinom(j,1,psi)  #-- "true" occupancy by transect
      y <- matrix(NA, j, k)
      for(m in 1:j) {
        y[m,] <- rbinom(k, 1, z[m]*p) #simulated survey data incorporating detection
      }
      occu <- data.frame(z=z, simocc=NA)
      occu$simocc <- rowSums(y)
      occu$simocc <- ifelse(occu$simocc > 0, 1, 0)
      
      dfrep$numocc[i] <- sum(occu$z)
      dfrep$numsim[i] <- sum(occu$simocc)
      
      dfrep$testp[i] <- ifelse(dfrep$numocc[i]>0 & dfrep$numsim[i]>0,1,0)
    }
    home[,j,k] <- dfrep$testp
    
    cat("\r", k, ".", j, "of", transects, "\r") ##This displays the counter for progress
    flush.console() ##This is necessary for the counter to run correctly
    
  }
  
}

results <- colMeans(home, na.rm=T)
png(file="psi95p95t20s20.png",width=3, height=3.2, units="in",res=600)
contour(seq(1,transects,by=1),seq(1,surveys,by=1),results,
        xlim=c(0,5), ylim=c(0,5),
        labcex=.4,
        cex.axis=.7,
        drawlabels=TRUE,
        axes=TRUE,
        frame.plot=TRUE,
        levels=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
        col=c(1,1,1,1,1,1,1,1,1,1),
        lwd=c(1,1,1,1,1,1,1,4,1,1),
        method="flattest",
        crt=90,
        vfont=c("sans serif", "plain"),
        xlab="",ylab="")
text(3.5,4.5,expression(italic(Psi)==0.95),cex=.6,adj=c(0,NA))
text(3.5,4,expression(italic(p)==0.95),cex=.6,adj=c(0,NA))
dev.off()
##Only run this line if you want to save a new workspace. Change the filename as appropriate.
##psi = occupancy, p = detection, t = # transects, s = # surveys
save(results, file="psi95p95t20s20.RData")


