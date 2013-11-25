####### TRGOVALNE STRATEGIJE
# v vseh strategijah samo kupujem, nič ne prodajam
# mogoče bi bilo dobro implementirat še z prodajami (shortanje)?

### POTREBNE KNJIŽNICE:
library(TTR)

### VREDNOST STRATEGIJE:
# če kupiš, je vrednost strategije close tistega dne / close prejšnjega dne * vrednost prejšnjega dne
# če ne kupiš, je enaka vrednsoti prejšnjega dne
# pri buy & hold je vrednost vedno taka kot, če bi kupil
value = function(trguj, closeDanes, closeVceraj, vrednostVceraj){
  if (trguj == 1 || trguj == TRUE || trguj == T) {
    vrednostDanes = closeDanes / closeVceraj * vrednostVceraj
  }
  else if (trguj == 0 || trguj == FALSE || trguj == F) {
    vrednostDanes = vrednostVceraj
  }
  return(vrednostDanes)
}

# dobiček / izguba strategije = današnja vrednost - vrednost prejšnjega dne
PandL = function(vrednostDanes, vrednostVceraj){
  return(vrednostDanes - vrednostVceraj)
}

## DAILY PRICE CHANGE (donos??):
# vrednost tistega dne / vrednost prejšnjega dne
priceChange = function(vrednostDanes, vrednostVceraj) {
  return(vrednostDanes / vrednostVceraj)
}


### FUNKCIJE ZA RAZLIČNE STRATEGIJE
## 1. SMA = Simple Moving Average
## strategija: če je close prejšnjega dne nad SMA prejšnjega dne, kupi, če je pod, prodaj
## n = 5, 25, 50, 150
SMAstrategy = function(close, SMAn, n, zacetek=1, budget = 1000){
  ## close = vektor close cen
  ## SMAn = vektor moving averages izračunanih na n podatkih
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## n = dolžina moving averages
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  trguj = c()
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # če začetek < n začnem pri n, drugače pri začetek
  # začnem pri dnevu n+1, ker za dneve 1:(n-1) je SMA enak NA
  if (zacetek > n){
    a = zacetek
  }
  else if (zacetek <= n){
    a = n
  }
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[a] = budget
  
  for (i in (a+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na close in SMA
    trguj[i] = close[i-1] > SMAn[i-1]
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
#     # določim dobiček/izgubo
#     dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
#     
#     # določim razliko v ceni/donos
#     donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, SMA, trguj, vrednost, dobicekIzguba, donos
#   return(cbind(close, SMAn, trguj, vrednost, dobicekIzguba, donos))
  return(vrednost)
}

## 2. RSI = Relative Strength Index
## strategija: če je RSI prejšnjega dne pod 30, kupi, če je nad 70, prodaj
## RSI = 100 + 100/(1 + RS)
## RS = Relative Strength = Average Gain / Average Loss
## First Average Gain = Sum of Gains over the past n periods / n
## First Average Loss = Sum of Losses over the past n periods / n
## Average Gain = [(previous Average Gain) x (n-1) + current Gain] / n
## Average Loss = [(previous Average Loss) x (n-1) + current Loss] / n
## n = 2, 14
RSIstrategy = function(close, RSIn, n, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## RSIn = RSI indeks izračunan na dolžini n
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## n = dolžina na kateri računam povprečno izgubo/dobiček
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  trguj = c()
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # če začetek < n začnem pri n, drugače pri začetek
  # začnem pri dnevu n+1, ker za dneve 1:(n-1) je RSIn enak NA
  if (zacetek > n + 1){
    a = zacetek
  }
  else if (zacetek < n){
    a = n + 1
  }
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[a] = budget
  
  for (i in (a+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na RSI prejšnjega dne
    trguj[i] = RSIn[i-1] < 30
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
#     # določim dobiček/izgubo
#     dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
#     
#     # določim razliko v ceni/donos
#     donos[i] = priceChange(vrednost[i], vrednost[i-1])     
  }
  
  # funkcija vrne matriko s stolpci close, RSI, trguj, vrednost, dobicekIzguba, donos
#   return(cbind(close, RSI, trguj, vrednost, dobicekIzguba, donos))
  return(vrednost)
}

## 3. Buy & Hold
BuyHoldStrategy = function(close, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[zacetek] = budget
  
  # začnem pri dnevu n+1
  for (i in (zacetek+1) : length(close)){   
    # določim vrednost buy & hold strategije (vedno trgujem)
    vrednost[i] = value(1, close[i], close[i-1], vrednost[i-1])
    
#     # določim dobiček/izgubo
#     dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
#     
#     # določim razliko v ceni/donos
#     donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, vrednost, dobicekIzguba, donos
#   return(cbind(close, vrednost, dobicekIzguba, donos))
  return(vrednost)
}

## 4. Bollinger
## strategija: če je close prejšnjega dne pod spodnjim pasom prejšnjega dne, kupi, če je nad zgornjim pasom prejšnjega dne, prodaj
## spodnji pas = SMA - STD * faktor
## zgornji pas = SMA + STD * faktor
## SMA za n = 20
## STD = stadnardna deviacija na n podatkih
## faktor = 2
BollingerStrategy = function(close, upBand, lowBand, n, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## upBand = zgornji pas bollinger indeksa izračunanega na n podatkih
  ## lowBand = spodnji pas bollinger indeksa izračunanega na n podatkih
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## n = dolžina na kateri računam povprečno izgubo/dobiček
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  trguj = c()
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # če začetek < n začnem pri n, drugače pri začetek
  # začnem pri dnevu n+1, ker za dneve 1:(n-1) je bollinger indeks enak NA
  if (zacetek > n){
    a = zacetek
  }
  else if (zacetek < n){
    a = n
  }
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[a] = budget
  
  for (i in (a+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na close in lowBand
    trguj[i] = close[i-1] < lowBand[i-1]
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
#     # določim dobiček/izgubo
#     dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
#     
#     # določim razliko v ceni/donos
#     donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, SMA, trguj, vrednost, dobicekIzguba, donos
#   return(cbind(close, lowBand, trguj, vrednost, dobicekIzguba, donos))
  return(vrednost)
}



## 5. Random
## strategija: slučajno izberi ali kupiš, prodaš ali nič ne narediš
randomStrategy = function(close, seed = 1234, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## seed nastavim, da je vsakič ist random rezultat, da lahko večkrat računam z istimi podatki
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## budget = znesek, ki ga vložimo v dani papir
  
  set.seed = seed
  # trguj je vektor ničel in enk, kjer je: verjetnost, da je 1 = verjetnosti, da je 0 = 1/2
  # trguj ~ bern(0.5) = bin(1, 0.5)
  trguj = rbinom(length(close), 1, 0.5)
  
  # predpripravim vektorje, ki jih bom potrebovala:
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[zacetek] = budget
  
  for (i in (zacetek+1) : length(close)){    
    # določim vrednost strategije
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
#     # določim dobiček/izgubo
#     dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
#     
#     # določim razliko v ceni/donos
#     donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, vrednost, dobicekIzguba, donos
#   return(cbind(close, trguj, vrednost, dobicekIzguba, donos))
  return(vrednost)
}
  

## 6. Overfit ??


###################3 TESTIRANJE PRAVILNOSTI KODE STRATEGIJ
# SMAstrategy(1:100, SMA(1:100, 5), 5)
# SMAstrategy(1:100, SMA(1:100, 5), 5, zacetek = 23)
# RSIstrategy(1:100, RSI(1:100, 20), 20) 
# RSIstrategy(1:100, RSI(1:100, 20), 20, zacetek = 23) 
# BuyHoldStrategy(1:100)
# BuyHoldStrategy(1:100, zacetek = 23)
# BollingerStrategy(1:100, BBands(1:100)[, 3], BBands(1:100)[, 3], n = 20)
# BollingerStrategy(1:100, BBands(1:100)[, 3], BBands(1:100)[, 3], n = 20, zacetek = 23)
# randomStrategy(1:100)
# randomStrategy(1:100, zacetek = 23)