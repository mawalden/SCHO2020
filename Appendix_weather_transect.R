##Code to produce appendixes 

#Code to produce Fig 2 barplot of transect data 
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


pprecip <- ggplot(scho_transect, aes(x=Precip*25.4, y=Total, color=Transect)) +
  geom_jitter(size=4, alpha=0.8) + 
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 16, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'),
        legend.position = "")+
  xlab("Daily Precipitation (mm)")+
  ylab("Individuals Captured")


ptemp <- ggplot(scho_transect, aes(x=Ground_Temp..C., y=Total, color=Transect)) +
  geom_point(size=4, alpha=0.8) + 
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 16, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'), 
        legend.position="")+
  xlab("Ground Temperature (C)")+
  ylab("")



prh <- ggplot(scho_transect, aes(x=RH...., y=Total, color=Transect)) +
  geom_point(size=4, alpha=0.8) + 
  theme_classic()+
  theme(axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 16, color = 'black'), 
        axis.text.x= element_text(size = 16, color = 'black'), 
        axis.text.y = element_text(size = 16, color = 'black'))+
  xlab("Relative Humidity (%)")+
  ylab("")


png("transectvsweather.png", units="in", width=12, height=4, res=600)
multiplot(pprecip, ptemp, prh, cols=3)
dev.off()


