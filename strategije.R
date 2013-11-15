####### TRGOVALNE STRATEGIJE
# v vseh strategijah samo kupujem, nič ne prodajam
# mogoče bi bilo dobro implementirat še z prodajami (shortanje)?

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
  return(vrednsotDanes - vrednostVceraj)
}

## DAILY PRICE CHANGE (donos??):
# vrednost tistega dne / vrednost prejšnjega dne
priceChange = function(vrednsotDanes, vrednosVceraj) {
  return(vrednostDanes / vrednostVceraj)
}


### FUNKCIJE ZA RAZLIČNE STRATEGIJE
## 1. SMA = Simple Moving Average
## strategija: če je close prejšnjega dne nad SMA prejšnjega dne, kupi, če je pod, prodaj
## n = 5, 25, 50, 150
SMAstrategy = function(close, n, budget){
  # najprej izračunam SMA za dani n na close cenah
  SMAn = SMA(close, n = n)
  
  # predpripravim vektorje, ki jih bom potrebovala:
  trguj = c()
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[n] = budget
  
  # začnem pri dnevu n+1, ker za dneve 1:(n-1) je SMA enak NA
  for (i in (n+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na close in SMA
    trguj[i] = close[i-1] > SMAn[i-1]
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
    # določim dobiček/izgubo
    dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
    
    # določim razliko v ceni/donos
    donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, SMA, trguj, vrednost, dobicekIzguba, donos
  return(cbind(close, SMAn, trguj, vrednost, dobicekIzguba, donos))
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
RSIstrategy = function(close, n, budget){
  # predpripravim vektorje, ki jih bom potrebovala
  averageGain = c()
  averageLoss = c()
  RS = c()
  RSI = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[n] = budget
  
  # izračunam izgubo/dobiček za prvih n dni:
  gainLoss = c()
  for (i in 2:n){
    gainLoss[i] = close[i] - close[i-1]
  }
  
  # izračunam prvi Average Gain, Average Lost in RSI - n-ti
  averageGain[n] = sum(gainLoss[gainLoss > 0]) / n
  averageLoss[n] = abs(sum(gainLoss[gainLoss < 0])) / n
  RSI[n] = 100 + 100/(1 + averageGain[n] / averageLoss[n])
  
  # začnem pri dnevu n+1, ker sta za dneve 1:(n-1) averageGain in averageLoss enaka NA
  for (i in (n+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na RSI prejšnjega dne
    trguj[i] = RSI[i-1] < 30
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
    # določim dobiček/izgubo
    dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
    
    # določim razliko v ceni/donos
    donos[i] = priceChange(vrednost[i], vrednost[i-1])
    
    # določim gain, loss, averageGain, averageLoss in RSI na ta dan
    gainLoss = close[i] - close[i-1]
    # če je dobiček, ga dobim z sum(gainLoss[gainLoss > 0]), če je gainLoss < 0, je to enako 0
    # če je izguba, jo dobim z abs(sum(gainLoss[gainLoss < 0])), če je gainLoss > 0, je to enako 0
    averageGain[i] = (averageGain[i-1] * (n-1) + sum(gainLoss[gainLoss > 0])) / n
    averageLoss[i] = (averageLoss[i-1] * (n-1) + abs(gainLoss[gainLoss < 0])) / n
    
    RSI[i] = 100 + 100/(1 + averageGain[i] / averageLoss[i])    
  }
  
  # funkcija vrne matriko s stolpci close, RSI, trguj, vrednost, dobicekIzguba, donos
  return(cbind(close, RSI, trguj, vrednost, dobicekIzguba, donos))
}

## 3. Buy & Hold
BuyHoldStrategy = function(close, n, budget){
  # tukaj je n+1 dan, ko začnem trgovati
  
  # predpripravim vektorje, ki jih bom potrebovala:
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[n] = budget
  
  # začnem pri dnevu n+1
  for (i in (n+1) : length(close)){
    
    # določim vrednost buy & hold strategije (vedno trgujem)
    vrednost[i] = value(1, close[i], close[i-1], vrednost[i-1])
    
    # določim dobiček/izgubo
    dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
    
    # določim razliko v ceni/donos
    donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, vrednost, dobicekIzguba, donos
  return(cbind(close, vrednost, dobicekIzguba, donos))
}

## 4. Bollinger
## strategija: če je close prejšnjega dne pod spodnjim pasom prejšnjega dne, kupi, če je nad zgornjim pasom prejšnjega dne, prodaj
## spodnji pas = SMA - STD * faktor
## zgornji pas = SMA + STD * faktor
## SMA za n = 20
## STD = stadnardna deviacija na n podatkih
## faktor = 2
BollingerStrategy = function(close, n, budget){
  # najprej izračunam SMA za dani n na close cenah
  SMAn = SMA(close, n = n)
  
  # predpripravim vektorje, ki jih bom potrebovala:
  STD = c()
  upBand = c()
  lowBand = c()
  trguj = c()
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[n] = budget
  
  # izračunam standardno deviacijo na prvih n podatkih ter lower band in upper band za n-ti dan
  STD[n] = sd(close[1:n])
  upBand[n] = SMAn[n] + faktor * STD[n]
  lowBand[n] = SMAn[n] - faktor * STD[n]
  
  # začnem pri dnevu n+1, ker za dneve 1:(n-1) je SMA enak NA
  for (i in (n+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na close in lowBand
    trguj[i] = close[i-1] < lowBand[i-1]
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
    # določim dobiček/izgubo
    dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
    
    # določim razliko v ceni/donos
    donos[i] = priceChange(vrednost[i], vrednost[i-1])
    
    # izračunam standardno deviacijo, lower band in upper band za ta dan
    STD[i] = sd(close[(i-n+1):i])
    upBand[i] = SMAn[i] + faktor * STD[i]
    lowBand[i] = SMAn[i] - faktor * STD[i]    
  }
  
  # funkcija vrne matriko s stolpci close, SMA, trguj, vrednost, dobicekIzguba, donos
  return(cbind(close, lowBand, trguj, vrednost, dobicekIzguba, donos))
}



## 5. Random
## strategija: slučajno izberi ali kupiš, prodaš ali nič ne narediš
randomStrategy = function(close, n, budget){
  # tukaj je n+1 dan, ko začnem trgovati
  
  # trguj je vektor ničel in enk, kjer je: verjetnost, da je 1 = verjetnosti, da je 0 = 1/2
  # trguj ~ bern(0.5) = bin(1, 0.5)
  trguj = rbinom(length(close), 1, 0.5)
  
  # predpripravim vektorje, ki jih bom potrebovala:
  vrednost = c()
  dobicekIzguba = c()
  donos = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[n] = budget
  
  # začnem pri dnevu n+1
  for (i in (n+1) : length(close)){    
    # določim vrednost strategije
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
    
    # določim dobiček/izgubo
    dobicekIzguba[i] = PandL(vrednost[i], vrednost[i-1])
    
    # določim razliko v ceni/donos
    donos[i] = priceChange(vrednost[i], vrednost[i-1])
  }
  
  # funkcija vrne matriko s stolpci close, vrednost, dobicekIzguba, donos
  return(cbind(close, trguj, vrednost, dobicekIzguba, donos))
}
  

## 6. Overfit ??

