#Code to produce Fig 3 barplot
#Written by A Devan-Song. July 2021, Bend, OR. 
#A. Devan-Song et al R code and datasets for manuscript: Devan-Song, A., M.A. Walden, H.A. Moniz, J.M. Fox, M.R. Low, E. #Wilkinson and N.E. Karraker. 2021. Confirmation bias fuels ecological misconception: evidence against 'secretive' nature of #eastern spadefoots (Scaphiopus h. holbrookii). Journal of Herpetology 55(2):137–150.


library(tidyverse)
library(ggplot2)
library(reshape)

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

#Import summary transect data 
scho_transect <- read.csv("SCHO_TRANSECT_R.csv")


scho_transect$Transect <- as.factor(scho_transect$Transect)

#subset data for graph
variables <- myvars <- c("Transect", "Adults", "Juveniles")
scho_df <- scho_transect[variables]
scho_df <- data.frame(scho_df) #convert to dataframe 
names(scho_df)[names(scho_df) == 'Juveniles'] <- 'Subadults/NBA' #rename columns 
reshape_scho <- melt(scho_df, id=c("Transect")) #reshape data 
reshape_scho$Transect <- as.factor(reshape_scho$Transect)
reshape_scho$Demography <- reshape_scho$variable

y_title <- expression(paste("Number of ", italic("Scaphiopus holbrookii")))

#plot 

p1 <- ggplot(reshape_scho, aes(Transect, value, fill = Demography), colour = "black") +
  ggtitle("A")+
  stat_summary(geom = "bar", fun= mean, position = "dodge", colour = "black") +
  stat_summary(geom = "errorbar", fun.data = mean_se, 
               position=position_dodge(0.9), 
               size=0.3, width=0.4, color = "grey24") + 
  ylab("Number of Individuals Captured") + xlab("Transect Number") +
  scale_fill_manual("", values = c("Adults" = 'black', "Subadults/NBA" = 'white')) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14, color = 'black'), 
        axis.title.y = element_text(size = 14, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position='right')


p1



scho_transect$Date <- as.Date(scho_transect$Date, "%m/%d/%Y") #format date

range <-  c(as.Date("2016-04-10"), as.Date("2016-09-10")) #set range for dates


transectdate <- ggplot(scho_transect, aes(x=Date, y=Total, color=Transect, shape=Transect)) +
  ggtitle("B")+
  geom_point(size=4) + 
  geom_line(aes(linetype=Transect), size=0.8) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15, color = 'black'),
        axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 14, color = 'black'), 
        axis.text.y = element_text(size = 14, color = 'black'), 
        legend.title = element_text(size=16),
        legend.text = element_text(size=16))+ 
  ylab("Number of Individuals Captured") + 
  xlab("Date")


png("transectvsdate.png", units="in", width=9, height=4, res=600)
transectdate


png("Fig3_lee_NEW.png", units="px", width =9600, height = 4200, res=1200)
multiplot(p1, transectdate, cols=2)
dev.off()



#####ADDTINAL IF NECESSARY

dprecip <- ggplot(scho_transect, aes(x=precip*25.4, y=detection_prob, color=Transect)) +
  geom_point(size=4)+
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 16, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position="")+
  xlab("Precipitation (mm)")+
  ylab("Detection Probability")


dtemp <- ggplot(scho_transect, aes(x=Ground_Temp..C., y=detection_prob, color=Transect)) +
  geom_point(size=4) + 
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 16, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'), 
        legend.position='')+
  xlab("Ground Temperature (C)")+
  ylab("")


drh <- ggplot(scho_transect, aes(x=RH...., y=detection_prob, color=Transect)) +
  geom_point(size=4) + 
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 16, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'))+
  xlab("Relative Humidity (%)")+
  ylab("")


multiplot(pprecip, dprecip, ptemp, dtemp, prh, drh, cols=3)




png("Transect.png", units="in", width=4, height=4, res=600)
p1
dev.off()


png("numbervsrainfall.png", units="in", width=6, height=6, res=600)
p2
dev.off()


graphics.off()
