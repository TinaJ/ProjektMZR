setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# podatki za close vrednost za vse papirje, ki so dovolj dolgo v S&P, od 3.1.2000 do 1.11.2013
load("./CloseVrednosti2000.rda")
load("./CloseVrednosti2003.rda")
load("./CloseVrednosti2008.rda")

# najprej moraš pognati kodo v strategije.R

# donosi za vsak papir v data5, za dane strategije
bollinger = apply(data5, 2, BollingerStrategy, n = 20, faktor = 2, budget = 1000)
buyHold = apply(data5, 2, BuyHoldStrategy, n = 1, budget = 1000)
RSI2 = apply(data5, 2, RSIstrategy, n = 2, budget = 1000)
RSI14 = apply(data5, 2, RSIstrategy, n = 14, budget = 1000)
SMA5 = apply(data5, 2, SMAstrategy, n = 5, budget = 1000)
SMA25 = apply(data5, 2, SMAstrategy, n = 25, budget = 1000)
SMA50 = apply(data5, 2, SMAstrategy, n = 50, budget = 1000)
SMA150 = apply(data5, 2, SMAstrategy, n = 150, budget = 1000)
random = apply(data5, 2, randomStrategy, n = 1, budget = 1000)

# povprečni donos papirjev za dane strategije
avgBollinger = apply(bollinger, 1, mean, na.rm=TRUE)
avgBuyHold = apply(buyHold, 1, mean, na.rm=TRUE)
avgRSI2 = apply(RSI2, 1, mean, na.rm=TRUE)
avgRSI14 = apply(RSI14, 1, mean, na.rm=TRUE)
avgSMA5 = apply(SMA5, 1, mean, na.rm=TRUE)
avgSMA25 = apply(SMA25, 1, mean, na.rm=TRUE)
avgSMA50 = apply(SMA50, 1, mean, na.rm=TRUE)
avgSMA150 = apply(SMA150, 1, mean, na.rm=TRUE)
avgRandom = apply(random, 1, mean, na.rm=TRUE)

M = cbind(avgBollinger, avgBuyHold, avgRSI2, avgRSI14, avgSMA5, avgSMA25, 
          avgSMA50, avgSMA150, avgRandom)

# prvih 150 vrstic dam preč, ker je pri SMA150 NA
M = M[151:nrow(M), ]


save(M, file = "./M.rda")
