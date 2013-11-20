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

# indekse shranim:
save(SMA5, SMA25, SMA50, SMA150, RSI2, RSI14, bollinger, 
     file = "./data/indeksi.rda")

# najprej moraš pognati kodo v strategije.R

# matrika izgub in dobička za vsak papir v data, za dane strategije
# vsaka strategija ima svojo matriko, vsak stolpec v eni matriki je en papir

# začnem pri 150, ker imam SMA150, kjer je prvih 150 vrednosti enako NA???
zacetek = 1
budget = 1000
PLSMA5 = sapply(1:ncol(data), 
                function(i) SMAstrategy(data[ , i], SMA5[ , i], 5, zacetek, budget))
PLSMA25 = sapply(1:ncol(data), 
                 function(i) SMAstrategy(data[ , i], SMA25[ , i], 25, zacetek, budget))
PLSMA50 = sapply(1:ncol(data), 
                 function(i) SMAstrategy(data[ , i], SMA50[ , i], 50, zacetek, budget))
PLSMA150 = sapply(1:ncol(data), 
                  function(i) SMAstrategy(data[ , i], SMA150[ , i], 150, zacetek, budget))

PLRSI2 = sapply(1:ncol(data), 
                function(i) RSIstrategy(data[ , i], RSI2[ , i], 2, zacetek, budget))
PLRSI14 = sapply(1:ncol(data), 
                 function(i) RSIstrategy(data[ , i], RSI14[ , i], 14, zacetek, budget))

PLbuyHold = sapply(1:ncol(data), 
                   function(i) BuyHoldStrategy(data[,i], zacetek, budget))

PLbollinger = sapply(1:ncol(data), 
                     function(i) BollingerStrategy(data[ , i], 
                                                   bollinger[(2 * nrow(data)+1): (3 * nrow(data)), i], 
                                                   bollinger[1:nrow(data) , i], 
                                                   20, zacetek, budget))

PLrandom = sapply(1:ncol(data), 
                  function(i) randomStrategy(data[,i], 1234, zacetek, budget))

# shranim dobičke/izgubo za vsak papir, za vsako strategijo
save(PLSMA5, PLSMA25, PLSMA50, PLSMA150, PLRSI2, PLRSI14, PLbuyHold, PLbollinger, PLrandom, 
     file = "./data/PLmatrike.rda")

# vsota dobička/izgube vseh papirjev papirjev za dane strategije
sumSMA5 = apply(PLSMA5, 1, sum, na.rm=TRUE)
sumSMA25 = apply(PLSMA25, 1, sum, na.rm=TRUE)
sumSMA50 = apply(PLSMA50, 1, sum, na.rm=TRUE)
sumSMA150 = apply(PLSMA150, 1, sum, na.rm=TRUE)

sumRSI2 = apply(PLRSI2, 1, sum, na.rm=TRUE)
sumRSI14 = apply(PLRSI14, 1, sum, na.rm=TRUE)

sumBuyHold = apply(PLbuyHold, 1, sum, na.rm=TRUE)

sumBollinger = apply(PLbollinger, 1, sum, na.rm=TRUE)

sumRandom = apply(PLrandom, 1, sum, na.rm=TRUE)

# te vsote združim v matriko M:
M = cbind(sumSMA5, sumSMA25, sumSMA50, sumSMA150, 
          sumRSI2, sumRSI14, sumBuyHold, sumBollinger, sumRandom)

# matriko M shranim
save(M, file = "./data/M.rda")
