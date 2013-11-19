setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# podatki za close vrednost za vse papirje, ki so dovolj dolgo v S&P, od 3.1.2000 do 1.11.2013
load("./data.rda")

# za podatke izračunam indekse: SMA5, 25, 50, 150, RSI 2, 14, bollinger za n = 20 in faktor = 2
SMA5 = apply(data, 2, SMA, n = 5)
SMA25 = apply(data, 2, SMA, n = 25)
SMA50 = apply(data, 2, SMA, n = 50)
SMA150 = apply(data, 2, SMA, n = 150)

RSI2 = apply(data, 2, RSI, n = 2)
RSI14= apply(data, 2, RSI, n = 14)

bollinger = apply(data, 2, BBands, n = 20, sd = 2) 
# => v isti stolpec zloži dn, mavg, up, pctB enega za drugim, ločit jih moraš

# najprej moraš pognati kodo v strategije.R

# matrika izgub in dobička za vsak papir v data, za dane strategije
# vsaka strategija ima svojo matriko, vsak stolpec v eni matriki je en papir

## rabm funkcijo, ki iz dveh matrik vzame istoležne stolpce in na njih uporabi strategijo?!



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
