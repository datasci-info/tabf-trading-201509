library(quantstrat)
search()

currency("TWD")
stock("TW2330",currency="TWD",multiplier=1)
ls(envir=FinancialInstrument:::.instrument)
ls(all=T)


initDate <- '1997-12-31'
startDate <- '1998-01-01'
endDate <-  '2014-06-30'
initEq <- 1e6

Sys.setenv(TZ="UTC")

TW2330 = getSymbols('2330.TW', from=startDate, to=endDate, index.class="POSIXct", adjust=T,auto.assign = F)
# TW2330$SMA60 <- SMA(Cl(TW2330), 60)


qs.strategy <- "qsFaber"
rm.strat(qs.strategy) # remove strategy etc. if this is a re-run

initPortf(qs.strategy,'TW2330', initDate=initDate)
initAcct(qs.strategy,portfolios=qs.strategy, initDate=initDate, initEq=initEq)


args(initOrders)
initOrders(portfolio=qs.strategy,initDate=initDate)

args(strategy)
strategy(qs.strategy,store=TRUE)

ls(all=T)
ls(.blotter)
ls(.strategy)


args(getStrategy)
strat <-getStrategy(qs.strategy)
class(strat)
summary(strat)

###################################### 
args(add.indicator)

add.indicator(strategy = qs.strategy, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n=60), label="SMA")
summary(getStrategy(qs.strategy))

######################################
args(add.signal)
add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA"),relationship="gt"),
           label="Cl.gt.SMA")

add.signal(qs.strategy,name="sigCrossover",
           arguments = list(columns=c("Close","SMA"),relationship="lt"),
           label="Cl.lt.SMA")

summary(getStrategy(qs.strategy))


###################################### 
args(add.rule)
args(ruleSignal)

add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=1000,
                          ordertype='market', orderside='long'),
         type='enter')

add.rule(qs.strategy, name='ruleSignal',
         arguments = list(sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all',
                          ordertype='market', orderside='long'),
         type='exit')


summary(getStrategy(qs.strategy))

###################################### 
args(applyStrategy)

applyStrategy(strategy=qs.strategy , portfolios=qs.strategy)
getTxns(Portfolio=qs.strategy, Symbol="TW2330")
View(getTxns(Portfolio=qs.strategy, Symbol="TW2330"))

chart.Posn(Portfolio=qs.strategy,Symbol="TW2330",log=TRUE)
# undebug(tradeStats)
tstats <- tradeStats(qs.strategy,"TW2330")

str(tstats)
tstats$Num.Txns
tstats$Num.Trades
tstats$Percent.Positive
tstats$Percent.Negative
tstats$Avg.WinLoss.Ratio

View(t(tstats))

getPortfolio(qs.strategy)
