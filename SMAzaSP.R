setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# install.packages("fImport")
# library(fImport) 

# SP = yahooSeries("^GSPC", from = "2000-01-01", to = "2013-11-01")
# save(SP, file = "./SP13let.rda")
load("./SP13let.rda")

library(TTR)

data = SP[,4]
colnames(data) = "close"


# strategija: če je close prejšnjega dne nad SMA prejšnjega dne, kupi
# vrednost, če kupiš je vrednos close tistega dne/close prejšnjega dne * vrednost prejšnjega dne
# če ne kupiš, je vrednost enaka vrednosti prejšnjega dne

n = 5
data$SMA5 = SMA(data$close, n=n)
data$vrednost5[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj5[i] = data$close[i-1] > data$SMA5[i-1]
  if (data$trguj5[i] == 1){
    data$vrednost5[i] = (data$close[i]/data$close[i-1]) * data$vrednost5[i-1]
  } 
  else data$vrednost5[i] = data$vrednost5[i-1]
  
  data$profit5[i] = data$vrednost5[i] - data$vrednost5[i-1]
}

n = 25
data$SMA25 = SMA(data$close, n=n)
data$vrednost25[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj25[i] = data$close[i-1] > data$SMA25[i-1]
  if (data$trguj25[i] == 1){
    data$vrednost25[i] = (data$close[i]/data$close[i-1]) * data$vrednost25[i-1]
  } 
  else data$vrednost25[i] = data$vrednost25[i-1]
  
  data$profit25[i] = data$vrednost25[i] - data$vrednost25[i-1]
}

n = 50
data$SMA50 = SMA(data$close, n=n)
data$vrednost50[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj50[i] = data$close[i-1] > data$SMA50[i-1]
  if (data$trguj50[i] == 1){
    data$vrednost50[i] = (data$close[i]/data$close[i-1]) * data$vrednost50[i-1]
  } 
  else data$vrednost50[i] = data$vrednost50[i-1]
  
  data$profit[i] = data$vrednost50[i] - data$vrednost50[i-1]
}

n = 150
data$SMA150 = SMA(data$close, n=n)
data$vrednost150[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj150[i] = data$close[i-1] > data$SMA150[i-1]
  if (data$trguj150[i] == 1){
    data$vrednost150[i] = (data$close[i]/data$close[i-1]) * data$vrednost150[i-1]
  } 
  else data$vrednost150[i] = data$vrednost150[i-1]
  
  data$profit150[i] = data$vrednost150[i] - data$vrednost150[i-1]
}

data