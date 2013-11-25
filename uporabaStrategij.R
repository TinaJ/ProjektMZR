setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# podatki za close vrednost za vse papirje, ki so dovolj dolgo v S&P, od 3.1.2000 do 1.11.2013
load("./data/data.rda")

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

# matrika vrednosti za vsak papir v data, za dane strategije
# vsaka strategija ima svojo matriko, vsak stolpec v eni matriki je en papir

# začnem pri 150, ker imam SMA150, kjer je prvih 150 vrednosti enako NA
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
                  function(i) randomStrategy(data[,i], 1234, zacetek, budget))

# shranim vrednosti za vsak papir, za vsako strategijo
save(PLSMA5, PLSMA25, PLSMA50, PLSMA150, PLRSI2, PLRSI14, PLbuyHold, PLbollinger, PLrandom, 
     file = "./data/PLmatrike.rda")

load("./data/PLmatrike.rda")

# vsota vrednosti vseh papirjev papirjev za dane strategije
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

# izračunam donos na vsoti vrednosti
Mdonos = apply(Mvrednost, 2, function(X) sapply(2:length(X), function(i) X[i]/X[i-1]))


# matriki M in Mdonos shranim
save(Mvrednost, file = "./data/Mvrednost.rda")
save(Mdonos, file = "./data/Mdonos.rda")

M = Mdonos-1

#odstranim prvih 150 vrstic, ki so NA (ker je začetek 150)
M = M[-c(1:150),]

save(M, file = "./data/M.rda")

#################################
# izračunam strategije za indeks SP500
load("./data/SP.rda")

zacetek = 150
budget = 1000
SMA5_SP = SMAstrategy(SP, SMA(SP, n = 5), 5, zacetek, budget*ncol(data))
SMA25_SP = SMAstrategy(SP, SMA(SP, n = 25), 25, zacetek, budget*ncol(data))
SMA50_SP = SMAstrategy(SP, SMA(SP, n = 50), 50, zacetek, budget*ncol(data))
SMA150_SP = SMAstrategy(SP, SMA(SP, n = 150), 150, zacetek, budget*ncol(data))

RSI2_SP = RSIstrategy(SP, RSI(SP, n = 2), 2, zacetek, budget*ncol(data))
RSI14_SP = RSIstrategy(SP, RSI(SP, n = 14), 14, zacetek, budget*ncol(data))

buyHold_SP = BuyHoldStrategy(SP, zacetek, budget*ncol(data))

B_SP = BBands(SP, n = 20)
bollinger_SP = BollingerStrategy(SP, B_SP[(2 * nrow(SP)+1): (3 * nrow(SP))], 
                                 B_SP[1:nrow(SP)], 20, zacetek, budget*ncol(data))

random_SP = randomStrategy(SP, 1234, zacetek, budget*ncol(data))

# vrednosti združim v matriko M_SP:
M_SP = cbind(SMA5_SP, SMA25_SP, SMA50_SP, SMA150_SP, 
          RSI2_SP, RSI14_SP, buyHold_SP, bollinger_SP, random_SP)

# izračunam donose za SP
M_SP_donos = apply(M_SP, 2, function(X) sapply(2:length(X), function(i) X[i]/X[i-1]))

# matriki M_SP in M_SP_donos shranim
save(M_SP, file = "./data/M_SP.rda")
save(M_SP_donos, file = "./data/M_SP_donos.rda")


################ SHRANJENI PODATKI:
load("./data/indeksi.rda")
load("./data/PLmatrike.rda")
load("./data/Mvrednost.rda")
load("./data/Mdonos.rda")
load("./data/M_SP.rda")
load("./data/M_SP_donos.rda")