### UPORABIM STRATEGIJE NA PAPIRJIH, KI SO V  INDEKSU SP 500 in na samam indeksu SP

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
load("./data/equityVseStrategije.rda")
load("./data/dnevniDonosiPortfelja.rda")
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

bollinger05 = apply(data, 2, BBands, n = 20, sd = 0.5) 
bollinger1 = apply(data, 2, BBands, n = 20, sd = 1) 
# => v isti stolpec zloži dn, mavg, up, pctB enega za drugim, ločit jih moraš


# za vsako strategijo izračunam dnevne vrednosti za vsak papir
# vsaka strategija ima svojo matriko, vsak stolpec v eni matriki je en papir

# začnem trgovati pri dnevu 150, ker imam SMA150, kjer je prvih 150 vrednosti enako NA
# vsakemu papirju na začetku namenim enak delež v portfelju
zacetek = 150
budget = 1000
vrednostiSMA5 = sapply(1:ncol(data), 
                function(i) SMAstrategy(data[ , i], SMA5[ , i], 5, zacetek, budget))
vrednostiSMA25 = sapply(1:ncol(data), 
                 function(i) SMAstrategy(data[ , i], SMA25[ , i], 25, zacetek, budget))
vrednostiSMA50 = sapply(1:ncol(data), 
                 function(i) SMAstrategy(data[ , i], SMA50[ , i], 50, zacetek, budget))
vrednostiSMA150 = sapply(1:ncol(data), 
                  function(i) SMAstrategy(data[ , i], SMA150[ , i], 150, zacetek, budget))

vrednostiRSI2 = sapply(1:ncol(data), 
                function(i) RSIstrategy(data[ , i], RSI2[ , i], 2, zacetek, budget))
vrednostiRSI14 = sapply(1:ncol(data), 
                 function(i) RSIstrategy(data[ , i], RSI14[ , i], 14, zacetek, budget))

vrednostiBuyHold = sapply(1:ncol(data), 
                   function(i) BuyHoldStrategy(data[,i], zacetek, budget))

vrednostiBollinger05 = sapply(1:ncol(data), 
                     function(i) BollingerStrategy(data[ , i], 
                                                   bollinger05[(2 * nrow(data)+1): (3 * nrow(data)), i], 
                                                   bollinger05[1:nrow(data) , i], 
                                                   20, zacetek, budget))
vrednostiBollinger1 = sapply(1:ncol(data), 
                       function(i) BollingerStrategy(data[ , i], 
                                                     bollinger1[(2 * nrow(data)+1): (3 * nrow(data)), i], 
                                                     bollinger1[1:nrow(data) , i], 
                                                     20, zacetek, budget))

vrednostiRandom = sapply(1:ncol(data), 
                  function(i) randomStrategy(data[,i], zacetek, budget))

# izračunam dnevne vrednosti celotnega portfelja za vsako strategijo
equitySMA5 = apply(vrednostiSMA5, 1, sum, na.rm=TRUE)
equitySMA25 = apply(vrednostiSMA25, 1, sum, na.rm=TRUE)
equitySMA50 = apply(vrednostiSMA50, 1, sum, na.rm=TRUE)
equitySMA150 = apply(vrednostiSMA150, 1, sum, na.rm=TRUE)

equityRSI2 = apply(vrednostiRSI2, 1, sum, na.rm=TRUE)
equityRSI14 = apply(vrednostiRSI14, 1, sum, na.rm=TRUE)

equityBuyHold = apply(vrednostiBuyHold, 1, sum, na.rm=TRUE)

equityBollinger05 = apply(vrednostiBollinger05, 1, sum, na.rm=TRUE)
equityBollinger1 = apply(vrednostiBollinger1, 1, sum, na.rm=TRUE)

equityRandom = apply(vrednostiRandom, 1, sum, na.rm=TRUE)

# te vsote združim v eno matriko:
equityVseStrategije = cbind(equitySMA5, equitySMA25, equitySMA50, equitySMA150, 
                            equityRSI2, equityRSI14, equityBuyHold, equityBollinger05, equityBollinger05, 
                            equityRandom)
colnames(equityVseStrategije) = c("SMA5", "SMA25", "SMA50", "SMA150", 
                        "RSI2", "RSI14", "BuyHold", "Bollinger0.5sd", "Bollinger1sd", "Random")
rownames(equityVseStrategije) = rownames(data)

# izračunam stopnjo donosa portfelja za vsako strategijo
dnevniDonosiPortfelja = apply(equityVseStrategije, 2, function(X) sapply(2:length(X), function(i) X[i]/X[i-1]))


#odstranim prvih 150 vrstic, ki so NA (ker je začetek 150)
equityVseStrategije = equityVseStrategije[-c(1:150),]
dnevniDonosiPortfelja = dnevniDonosiPortfelja[-c(1:150),]

# shranim equityVseStrategije in dnevniDonosiPortfelja
save(equityVseStrategije, file = "./data/equityVseStrategije.rda")
save(dnevniDonosiPortfelja, file = "./data/dnevniDonosiPortfelja.rda")
