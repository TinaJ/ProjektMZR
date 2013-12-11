### UPORABIM STRATEGIJE NA PAPIRJIH, KI SO V  INDEKSU SP 500 in na samam indeksu SP

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
load("./data/M.rda")
#######################################

## PODATKI, KI JIH POTREBUJEM
# podatki za close vrednost za vse papirje, ki so dovolj dolgo v S&P, od 3.1.2000 do 1.11.2013
load("./data/podatki.rda")

# podatki morajo biti v spremenljivki data!!!
data = podatki

## POŽENI KODO V strategije.R !

## KNJIŽNICE, KI JIH POTREBUJEM
library(TTR)

# za podatke izračunam indekse: SMA5, 25, 50, 150, RSI2, 14, bollinger za n = 20 in faktor = 2
SMA5 = apply(data, 2, SMA, n = 5)
SMA25 = apply(data, 2, SMA, n = 25)
SMA50 = apply(data, 2, SMA, n = 50)
SMA150 = apply(data, 2, SMA, n = 150)

RSI2 = apply(data, 2, RSI, n = 2)
RSI14= apply(data, 2, RSI, n = 14)

bollinger = apply(data, 2, BBands, n = 20, sd = 2) 
# => v isti stolpec zloži dn, mavg, up, pctB enega za drugim, ločit jih moraš


# za vsako strategijo izračunam dnevne vrednosti za vsak papir
# vsaka strategija ima svojo matriko, vsak stolpec v eni matriki je en papir

# začnem trgovati pri dnevu 150, ker imam SMA150, kjer je prvih 150 vrednosti enako NA
# vsakemu papirju na začetku namenim enak delež v portfelju
zacetek = 150
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
                  function(i) randomStrategy(data[,i], zacetek, budget))

# izračunam dnevne vrednosti celotnega portfelja za vsako strategijo
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
Mvrednost = cbind(sumSMA5, sumSMA25, sumSMA50, sumSMA150, 
          sumRSI2, sumRSI14, sumBuyHold, sumBollinger, sumRandom)
colnames(Mvrednost) = c("SMA5", "SMA25", "SMA50", "SMA150", 
                        "RSI2", "RSI14", "BuyHold", "Bollinger", "Random")
rownames(Mvrednost) = rownames(data)

# izračunam stopnjo donosa portfelja za vsako strategijo
Mdonos = apply(Mvrednost, 2, function(X) sapply(2:length(X), function(i) X[i]/X[i-1]))


# izračunam donos portfelja (stopnja donosa - 1)
M = Mdonos-1

#odstranim prvih 150 vrstic, ki so NA (ker je začetek 150)
M = M[-c(1:150),]

# shranim M
save(M, file = "./data/M.rda")

