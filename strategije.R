####### TRGOVALNE STRATEGIJE
# kot rezultat strategije, vrnem dnevne vrednosti
# ko kupim delnico, ves razpoložljiv denar za dano delnico investiram vanjo
# ko prodam delnico, prodam vse kar jih imam, ta denar držim dokler ni nakupnega signala za to delnico

### VREDNOST STRATEGIJE:
# če trguješ, je vrednost strategije close tistega dne / close prejšnjega dne * vrednost prejšnjega dne
# če ne trguješ, je enaka vrednsoti prejšnjega dne
# trguješ takrat, ko imaš delnico (jo kupiš, ali pa jo imaš in jo ne prodaš)

value = function(trguj, closeDanes, closeVceraj, vrednostVceraj){
  if (trguj == 1 || trguj == TRUE || trguj == T) {
    vrednostDanes = closeDanes / closeVceraj * vrednostVceraj
  }
  else if (trguj == 0 || trguj == FALSE || trguj == F) {
    vrednostDanes = vrednostVceraj
  }
  return(vrednostDanes)
}


### FUNKCIJE ZA RAZLIČNE STRATEGIJE
## 1. SMA = Simple Moving Average
## strategija: če je close prejšnjega dne nad SMA prejšnjega dne, kupi, če je pod, prodaj
SMAstrategy = function(close, SMAn, n, zacetek=1, budget = 1000){
  ## close = vektor close cen
  ## SMAn = vektor moving averages izračunanih na n podatkih
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## n = dolžina na katerih je izračunan moving average
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  trguj = c()
  vrednost = c()
  
  # če začetek < n začnem pri n, drugače pri začetek
  # za 1:(n-1) je SMA enak NA
  if (zacetek >= n){
    a = zacetek
  }
  else if (zacetek < n){
    a = n
  }
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[a] = budget
  # in nimamo delnice
  trguj[a] = 0
  
  # začnem pri dnevu a+1, ker za dneve 1:(a-1) je SMA enak NA
  for (i in (a+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na close in SMA
	# delnico imam, če je vrednot včeraj večja od včerajšnje vrednosti SMA
    trguj[i] = close[i-1] > SMAn[i-1]
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
  }  
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
RSIstrategy = function(close, RSIn, n, meja = 30, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## RSIn = RSI indeks izračunan na dolžini n
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## n = dolžina na kateri računam povprečno izgubo/dobiček
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  trguj = c()
  vrednost = c()
  
  # če začetek < n+1 začnem pri n+1, drugače pri začetek
  # za 1:n je RSIn enak NA
  if (zacetek >= n+1){
    a = zacetek
  }
  else if (zacetek < n+1){
    a = n + 1
  }
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[a] = budget
  # in nimam delnice
  trguj[a] = 0
  
  for (i in (a+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na RSI prejšnjega dne
    # in glede na to ali sem prejšnji dan imela delnico
    # trgujem, oz. držim delnico, če 
    # - prejšnji dan nisem imela delnice in je vrednost RSI prejšnjega dne pod 30
    if (trguj[i-1] == 0 && RSIn[i-1] < 30){
      trguj[i] = 1
    }

    # - ali prejšnji dan sem imela delnico in je RSI prejšnjega dne pod 70
    else if (trguj[i-1] == 1 && RSIn[i-1] < 70){
      trguj[i] = 1
    }
    else {
      trguj[i] = 0
    }
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
  }
  return(vrednost)
}

  
## 3. Buy & Hold
BuyHoldStrategy = function(close, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## budget = znesek, ki ga vložimo v dani papir
  
  # predpripravim vektorje, ki jih bom potrebovala:
  vrednost = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[zacetek] = budget
  
  # začnem pri dnevu zacetek+1
  for (i in (zacetek+1) : length(close)){   
    # določim vrednost buy & hold strategije (vedno trgujem)
    vrednost[i] = value(1, close[i], close[i-1], vrednost[i-1])
  } 
  return(vrednost)
}

  
## 4. Bollinger
## strategija: če je close prejšnjega dne pod spodnjim pasom prejšnjega dne, kupi, če je nad zgornjim pasom prejšnjega dne, prodaj
## spodnji pas = SMA - STD * faktor
## zgornji pas = SMA + STD * faktor
## STD = stadnardna deviacija na n podatkih
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
  
  # če začetek < n začnem pri n, drugače pri začetek
  # za 1:(n-1) je bollinger enak NA
  if (zacetek > n){
    a = zacetek
  }
  else if (zacetek < n){
    a = n
  }
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[a] = budget
  # in nimam delnice
  trguj[a] = 0
  
  for (i in (a+1) : length(close)){
    # določim ali kupim ali ne naredim nič, glede na close, lowBand in upBand
	# in glede na to ali sem prejšnji dan imela delnico
    # trgujem, oz. držim delnico, če 
    # - prejšnji dan nisem imela delnice in je vrednost prejšnjega dne pod spodnjim pasom prejšnjega dne
    if (trguj[i-1] == 0 && close[i-1] < lowBand[i-1]){
      trguj[i] = 1
    }

    # - ali prejšnji dan sem imela delnico in je vrdnost prejšnjega dne pod zgornjim pasom prejšnjega dne
    else if (trguj[i-1] == 1 && close[i-1] < upBand[i-1]){
      trguj[i] = 1
    }
    else {
      trguj[i] = 0
    }
    trguj[i] = close[i-1] < lowBand[i-1]
    
    # glede na to ali trgujem ali ne, določim vrednost
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
  }
  return(vrednost)
}


## 5. Random
## strategija: slučajno izberi ali kupiš, prodaš ali nič ne narediš oz. ali imaš delnico ali je nimaš
randomStrategy = function(close, zacetek = 1, budget = 1000){
  ## close = vektor close cen
  ## zacetek = dan s katerim začnem trgovati (kateri element v vektorju close je prvi)
  ## budget = znesek, ki ga vložimo v dani papir
  
  # trguj je vektor ničel in enk, kjer je: verjetnost, da je 1 = verjetnosti, da je 0 = 1/2
  # trguj ~ bern(0.5) = bin(1, 0.5)
  trguj = rbinom(length(close), 1, 0.5)
  
  # predpripravim vektorje, ki jih bom potrebovala:
  vrednost = c()
  
  # na začetku je vrednost strategije enaka denarju, ki ga imamo na razpolago (budget)
  vrednost[zacetek] = budget
  
  for (i in (zacetek+1) : length(close)){    
    # določim vrednost strategije
    vrednost[i] = value(trguj[i], close[i], close[i-1], vrednost[i-1])
  }
  return(vrednost)
}
  
  
## 6. OVERFIT 1:
## strategija: delnico imam, samo, če vem, da bo cena v enem dnevu zrasla, prihodnost poznam samo od 1.1.2005 do 1.1.2006