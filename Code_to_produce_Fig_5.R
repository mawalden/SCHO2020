#Code to reproduce Fig 4; percentage of spadefoots on surface 
#A. Devan-Song et al R code and datasets for manuscript: Devan-Song, A., M.A. Walden, H.A. Moniz, J.M. Fox, M.R. Low, E. #Wilkinson and N.E. Karraker. 2021. Confirmation bias fuels ecological misconception: evidence against 'secretive' nature of #eastern spadefoots (Scaphiopus h. holbrookii). Journal of Herpetology 55(2):137–150.


library(tidyverse)

#import dataset 
plot <- read.csv("Plot_data_for_graph.csv")

plot$date <- as.Date(plot$date, "%m/%d/%Y") #format date
plot$Plot <- as.character(plot$Plot) #make sure Plot number is not numeric

head(plot) #check plot data 

range <-  c(as.Date("2016-04-10"), as.Date("2016-09-10")) #set range for dates


## Plot graph depicting percentage of spadefoots on surface 

p<- ggplot(plot, aes(x=date, y=percent, shape=Plot)) +
  geom_point(aes(size=Plot)) + 
  geom_line(aes(linetype=Plot), size=0.8) + 
  scale_linetype_manual(values=c("twodash", "dotted", 'solid', 'dashed')) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15, color = 'black'),
        axis.title.x = element_text(size = 16, color = 'black'), 
        axis.title.y = element_text(size = 15, color = 'black'), 
        axis.text.y = element_text(size = 14, color = 'black'), 
        panel.background = element_rect(fill = "white", colour = "grey50"), 
        legend.position=c(0.89, 0.74),
        legend.title = element_text(size=16),
        legend.text = element_text(size=16))+ 
  ylab("Percentage of spadefoots on surface (%)") + 
  xlab("Date") + 
  scale_shape_manual(values=c(15, 18, 1, 17)) + 
  scale_size_manual(values=c(2.5,3.5,3.5,3.5)) + 
  ylim(0, 90)

p


png("FIGURE5.png", units="px", width=9600, height=6000, res=1200)
p
dev.off()

units="px", width =9600, height = 4200, res=1200
