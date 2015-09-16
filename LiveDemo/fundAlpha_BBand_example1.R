library(quantstrat)
initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <-  '2014-06-30'

TW2330 = getSymbols('2330.TW', from=startDate, to=endDate, index.class="POSIXct", adjust=T,auto.assign = F)

TW2330["2013-01-01::2014-01-01"]

chartSeries(tail(TW2330,500),TA = NULL)
addSMI()
KD = stoch(HLC(TW2330))
colnames(KD)

addSMA(n=60,col=5)

args(BBands)
addBBands()
bb = BBands(HLC(TW2330))

View(bb)

BBands(HLC(TW2330))$dn

BBandsDN = function(mktdata,n) BBands(mktdata)$dn


chartSeries(Cl(TW2330),TA = NULL)
addSMA(n=60,col=2)

######################################

qs.strategy <- "closeCROSS60MA"
ls(.strategy)
rm.strat(qs.strategy)
strategy(qs.strategy,store=TRUE)
summary(getStrategy(qs.strategy))

######################################

add.indicator(strategy = qs.strategy, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n=20), label="SMA")

args(BBands)
TW2330$BBandsDN = BBandsDN(HLC(TW2330),n=20)
# add.indicator(strategy = qs.strategy, name = "BBandsDN",
#               arguments = list(mktdata = quote(HLC(mktdata)), n=20), label="BBandsDN")


summary(getStrategy(qs.strategy))


######################################
colnames(TW2330)

args(add.signal)
?sigCrossover
sigCrossover(label="Cl.lt.BBandsDN",
             data=TW2330,
             columns=c("Close","BBandsDN"),
             relationship="lt")

add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","BBandsDN"),relationship="lt"),
           label="Cl.lt.BBandsDN")

add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA"),relationship="gt"),
           label="Cl.gt.SMA")

summary(getStrategy(qs.strategy))

######################################
args(ruleSignal)
?ruleSignal
add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.lt.BBandsDN", sigval=TRUE, orderqty=1000,
                          ordertype='market', orderside='long'),
         type='enter')

add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty='all',
                          ordertype='market', orderside='long'),
         type='exit')

summary(getStrategy(qs.strategy))

######################################

getPortfolio(qs.strategy)

currency("TWD")
stock("TW2330",currency="TWD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)

initEq <- 1e6

port.st <- "FundAlpha" 
rm.strat(port.st)

args(initPortf)
initPortf(port.st,'TW2330', initDate=initDate)

args(initAcct)
initAcct(port.st,portfolios=port.st, initDate=initDate, initEq=initEq)

args(initOrders)
initOrders(portfolio=port.st,initDate=initDate)

args(applyStrategy)
applyStrategy(qs.strategy,port.st)


View(mktdata)

args(getTxns)
View(getTxns(port.st,"TW2330"))

txn = getTxns(port.st,"TW2330")

class(txn)
tt = txn$Net.Txn.Realized.PL
Ws = tt[tt > 0]
Ls = tt[tt < 0]

mean(Ws) / -mean(Ls)
NROW(Ws) / (NROW(Ws) + NROW(Ls))
NROW(Ls) / (NROW(Ws) + NROW(Ls))

sum(Ws) + sum(Ls)



tradeStats(port.st)
# undebug(tradeStats)
tstats <- tradeStats(Portfolios = port.st,Symbols = "TW2330")

str(tstats)
tstats$Num.Txns
tstats$Num.Trades
tstats$Percent.Positive
tstats$Percent.Negative
tstats$Avg.WinLoss.Ratio
tstats$Gross.Profits  
tstats$Gross.Losses   
tstats$Gross.Profits+tstats$Gross.Losses   


######################################


View(t(tstats))

getPortfolio(port.st)

# hack for new quantmod graphics, remove later
themelist<-chart_theme()
themelist$col$up.col<-'lightgreen'
themelist$col$dn.col<-'pink'  


chart.Posn(Portfolio=port.st)
