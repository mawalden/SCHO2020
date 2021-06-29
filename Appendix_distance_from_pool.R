#A. Devan-Song|JHerp 
#July 31 2020 
rm(list = ls())
#Caluclate transect frog distance to nearest wetland 

setwd("") #to where data are stored

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(spatstat)
library(abind)
library(reshape)
library(wesanderson)
library(lme4)

#Function to turn data into degree coordinates 
reproject <- function(data){
  data.df <- as.data.frame(data)
  data.df.UTM <- SpatialPoints(data.df, proj4string=CRS("+proj=utm +zone=18 +datum=NAD83"))
  data.df.deg <- spTransform(data.df.UTM, CRS("+proj=longlat +datum=WGS84"))
  data.coords <- coordinates(data.df.deg)
  return(data.coords)
}

#Import spadefoot data points 
spade <- read.csv("Transect_GPS.csv")
spade$Y <- as.numeric(spade$Ycoord) #make Y coordinate a number
spade$X <- as.numeric(spade$Xcoord) # make X coordinate a number 
spade_points <- spade[c("X", "Y")] # make a dataframe with all the xy coordinates  


#Extract coordinates and reproject to latlong 
spade_coords <- reproject(spade_points)

#Import wetland locations, extract coordinates and convert to latlong 
ponds <- read.csv("pond_points.csv")
pond_coords <- reproject(ponds)


#function to calculate distance and pull out nearest pond to nearest pond 
calc_dist <- function(data, pond.coords){
  dst<- pointDistance(data, pond.coords, lonlat=TRUE)
  nnd <- apply(dst, 1, FUN=min) #pull out distance to nearest pond for each random point 
  return(nnd)
}


spade_nnd<- calc_dist(spade_coords, pond_coords)
#plot_cdf(spade_nnd, 1200)
spade_nnd_df <- as.data.frame(spade_nnd)
spade$Distance <- spade_nnd_df$spade_nnd

hist(spade$Distance)

spade$Demography <- spade$Demog

#jpeg("Dist2WetlandDensity", units="in", width=6, height=4, res=300)
plot1<- ggplot(spade, aes(x=Distance, fill=Demography)) + 
  geom_density() + 
  ylab("Density") + 
  xlab("Distance to Nearest Breeding Pool (m)") + 
#  scale_y_continuous(breaks = seq(min(0), max(0.1), by = 0.02), limits=c(0,0.1))+
  scale_x_continuous(breaks = round(seq(min(0), max(1500), by = 200),0), limits=c(0,1500))+
  scale_fill_manual(values=c(rgb(255/255, 0/255, 0/255, 0.8),rgb(0/255, 0/255, 255/255, 0.5), rgb(255/255, 255/255, 0/255, 0.4))) + 
  scale_color_manual(values=c("red", "blue", "gold"))+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position=c(0.8,0.7))
#dev.off()


trans <- read.csv("transect_loc.csv")
trans$Y <- as.numeric(trans$Y) #make Y coordinate a number
trans$X <- as.numeric(trans$X) # make X coordinate a number 
trans_points <- trans[c("X", "Y")] # make a dataframe with all the xy coordinates
trans_coords <- reproject(trans_points)
trans_nnd<- calc_dist(trans_coords, pond_coords)
trans_nnd_df <- as.data.frame(trans_nnd)
trans$Distance <- trans_nnd_df$trans_nnd

det <- read.csv("ma3pred.csv")
det$Transect <- rep(c(1:7), each = 8)
det$Distance <- rep(trans$Distance, each=8)
det$Transect <- as.factor(det$Transect)
det$survey_no <- rep(c(1:8), times = 7)

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 



png("detectionvsdist.png", units="in", width=5, height=3, res=600)
detvsdist<- ggplot(det, aes(x=Distance, y=Predicted, group=Transect, color=Transect)) +
  geom_boxplot() +
  geom_point(alpha=0.3) +
  scale_fill_manual(values=cbbPalette)+
  theme_classic()+
  ylab("Detection Probability Per Survey")+
  xlab("Transect Distance to Nearest Breeding Pool (m)")+
  scale_x_continuous(breaks = round(seq(min(0), max(1500), by = 200),0), limits=c(0,1200))
dev.off()


str(det)


multiplot(plot1, plot2, cols=2)

jpeg("AppendixTransectDistance.jpg", units="in", width=8, height=4, res=300)
multiplot(plot1, plot2, cols=2)
dev.off()

jpeg("AppendixDetectionProbDistance.jpg", units="in", width=6, height=4, res=500)
plot2
dev.off()


scho_transect <- read.csv("SCHO_TRANSECT_R.csv")


det$Total <- scho_transect$Total


scaleFUN <- function(x) sprintf("%.1f", x)
#p + scale_y_continuous(labels=scaleFUN)

abvsdist <- ggplot(det, aes(x=Distance, y=Total, group=Transect, color=Transect)) +
  geom_boxplot() +
  geom_point(alpha=0.3) +
  scale_fill_manual(values=cbbPalette)+
  theme_classic()+
  ylab("No. of Individuals Detected Per Survey")+
  xlab("Transect Distance to Nearest Breeding Pool (m)")+
  scale_y_continuous(trans='log', breaks=c(0,2,4,8,16,32,64))+
  #scale_y_continuous(trans='log', labels=scaleFUN, breaks=round(seq(min(0), max(55), by=8)))+
  scale_x_continuous(breaks = round(seq(min(0), max(1500), by = 200),0), limits=c(0,1200))

png("distanceappendix.png", units="in", width=10, height=3, res=600)
multiplot(abvsdist, detvsdist, cols=2)
dev.off()




