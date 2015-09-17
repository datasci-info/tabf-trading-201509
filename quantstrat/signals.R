library(quantstrat)

TW2330 = getSymbols('2330.TW', auto.assign = F)
chartSeries(tail(TW2330,300),TA = NULL)
addBBands()

BBandsDN = function(mktdata,n) BBands(mktdata)$dn

TW2330$BBandsDN = BBandsDN(HLC(TW2330),n=20)
TW2330$SMA = SMA(Cl(TW2330),n=20)

clCrossOverBBandDn = sigCrossover(label="Cl.lt.BBandsDN",
                                  data=TW2330,
                                  columns=c("Close","BBandsDN"),
                                  relationship="lt")

clCrossOverSMA60 = sigCrossover(label="Cl.gt.SMA",
                                  data=TW2330,
                                  columns=c("Close","SMA"),
                                  relationship="gt")

liftRatio = 0.02
chartSeries(tail(TW2330,600),TA = NULL)
addBBands()
addTA(Lo(TW2330)[!is.na(clCrossOverBBandDn)]*(1-liftRatio),on=1,type="p",col=2,pch=24,bg="red")
addTA(Hi(TW2330)[!is.na(clCrossOverSMA60)]*(1+liftRatio),on=1,type="p",col=3,pch=25,bg="green")
