library(RMark)
library(FSA)

##Set working directory
#setwd("")

##Create function

run.plot.popan=function()
{
  plot.ddl=make.design.data(pdat)
  Phidot=list(formula=~1)
  ptime=list(formula=~time)
  pent.dot=list(formula=~1)
  Ns=list(formula=~1)
  pmod=mark(pdat,plot.ddl,retry=1000,model.parameters=list
                               (Phi=Phidot,p=ptime,pent=pent.dot,N=Ns),
                               invisible=FALSE,adjust=FALSE)
  return(collect.models() )
}

##Plot 1 model

p1ch <- read.csv("ch_plot1.csv",header=TRUE)
p1ch <- as.data.frame(p1ch)
p1 <- capHistConvert(df=p1ch,in.type="individual",id="ident",out.type="RMark")

pdat <- process.data(p1, begin.time = 1, model = "POPAN",
                   time.intervals = c(10, 16, 18, 24, 18, 32))

p1.plot.results=run.plot.popan()
popan.derived(pdat,p1.plot.results,drop=TRUE)

##Plot 2 model

p2ch <- read.csv("ch_plot2.csv",header=TRUE)
p2ch <- as.data.frame(p2ch)
p2 <- capHistConvert(df=p2ch,in.type="individual",id="ident",out.type="RMark")

pdat <- process.data(p2, begin.time = 1, model = "POPAN",
                   time.intervals = c(12, 14, 34, 12, 12, 42))

p2.plot.results=run.plot.popan()
popan.derived(pdat,p2.plot.results,drop=TRUE)

##Plot 3 model

p3ch <- read.csv("ch_plot3.csv",header=TRUE)
p3ch <- as.data.frame(p3ch)
p3 <- capHistConvert(df=p3ch,in.type="individual",id="ident",out.type="RMark")
pdat <- process.data(p3, begin.time = 1, model = "POPAN",
                   time.intervals = c(13, 15, 19, 24, 16, 40))
p3.plot.results=run.plot.popan()
popan.derived(pdat,p3.plot.results,drop=TRUE)

##Plot 4 model

p4ch <- read.csv("ch_plot4.csv",header=TRUE)
p4ch <- as.data.frame(p4ch)
p4 <- capHistConvert(df=p4ch,in.type="individual",id="ident",out.type="RMark")

pdat <- process.data(p4, begin.time = 1, model = "POPAN",
                   time.intervals = c(8, 8, 20, 23, 22, 33))

p4.plot.results=run.plot.popan()
popan.derived(pdat,p4.plot.results,drop=TRUE)