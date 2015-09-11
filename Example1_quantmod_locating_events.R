library(quantstrat)

TWII = getSymbols("^TWII",auto.assign = F)
chartSeries(TWII,TA=NULL)
addSMA(n = 5,col = 8)
addSMA(n = 60, col = 5)

Cl5SMA = SMA(Cl(TWII),n = 5)
Cl60SMA = SMA(Cl(TWII),n = 60)
addTA(sign(Cl5SMA - Cl60SMA),col=5)

chartSeries(Cl5SMA)
addTA(Cl60SMA,col=5,on = 1)
addTA(sign(Cl5SMA - Cl60SMA),col=7)

diff(sign(Cl5SMA - Cl60SMA)) > 0 
which(diff(sign(Cl5SMA - Cl60SMA)) > 0 )

TDnDates <- index(Cl5SMA)[which(diff(sign(Cl5SMA - Cl60SMA)) > 0 )]
TUpDates <- index(Cl5SMA)[which(diff(sign(Cl5SMA - Cl60SMA)) < 0 )]

liftRatio = 0.02
addTA(Hi(TWII)[TDnDates]*(1+liftRatio),on=1,type="p",col=3,pch=25,bg="green")
addTA(Lo(TWII)[TUpDates]*(1-liftRatio),on=1,type="p",col=2,pch=24,bg="red")

# order = rbind(xts(rep(1,length(TUpDates)),TUpDates),xts(rep(-1,length(TDnDates)),TDnDates))
# cbind(lag(order),order)[-1,]

