setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# install.packages("fImport")
# library(fImport) 

# SP = yahooSeries("^GSPC", from = "2000-01-01", to = "2013-11-01")
# save(SP, file = "./SP13let.rda")
load("./SP13let.rda")

library(TTR)

data = SP[,4]
colnames(data) = "close"

vrstnired = sort.list(rownames(data))
data = data[vrstnired, ]

# izračunam spremembe v ceni, oz. loss/gain
for (i in 2:nrow(data)){
  if(data$close[i]-data$close[i-1] > 0){
    data$gain[i] = data$close[i]-data$close[i-1] ##zakaj to ne dela!?!?!?!?
    data$loss[i] = 0
  }
  else if(data$close[i]-data$close[i-1] < 0){
    data$loss[i] = abs(data$close[i]-data$close[i-1])
    data$gain[i] = 0
  }
  else if(data$close[i]-data$close[i-1] == 0){
    data$loss[i] = 0
    data$gain[i] = 0
  }
}

data$loss[1] = 0
data$gain[1] = 0
# izračunam povprečno izgubo in povprečen dobiček za 2 in za 14 dni

# # uporabim SMA za n = 2 in za n = 14
# data$avgGain2 = SMA(data$gain, n = 2)
# data$avgLoss2 = SMA(data$loss, n = 2)
# data$avgGain14 = SMA(data$gain, n = 14)
# data$avgLoss14 = SMA(data$loss, n = 14)

# First Average Gain = Sum of Gains over the past 14 periods / 14.
# First Average Loss = Sum of Losses over the past 14 periods / 14
# Average Gain = [(previous Average Gain) x 13 + current Gain] / 14.
# Average Loss = [(previous Average Loss) x 13 + current Loss] / 14.
n = 2
data$avgGain2[n] = mean(data$gain[1:n])
data$avgLoss2[n] = mean(data$loss[1:n])
for (i in (n+1) : nrow(data)){
  data$avgGain2[i] = (data$avgGain2[i-1] * (n-1) + data$gain[i])/n
  data$avgLoss2[i] = (data$avgLoss2[i-1] * (n-1) + data$loss[i])/n
}

n = 14
data$avgGain14[n] = mean(data$gain[1:n])
data$avgLoss14[n] = mean(data$loss[1:n])
for (i in (n+1) : nrow(data)){
  data$avgGain14[i] = (data$avgGain2[i-1] * (n-1) + data$gain[i])/n
  data$avgLoss14[i] = (data$avgLoss2[i-1] * (n-1) + data$loss[i])/n
}

# izračunam RS za 2 in za 14 dni
# RS = average gain / average loss
data$RS2 = mapply(function(gain, loss) gain/loss, data$avgGain2, data$avgLoss2)
data$RS14 = mapply(function(gain, loss) gain/loss, data$avgGain14, data$avgLoss14)

# izračunam RSI2 in RSI14
# RSI = 100 - 100/(1+RS)
data$RSI2 = mapply(function(RS) 100 - 100/(1+RS), data$RS2)
data$RSI14 = mapply(function(RS) 100 - 100/(1+RS), data$RS14)



# strategija: če RSI pade pod 30 kupi, če naraste nad 70 prodaj
# -> jst samo za kupovanje??
# vrednost, če kupiš je vrednos close tistega dne/close prejšnjega dne * vrednost prejšnjega dne
# če ne kupiš, je vrednost enaka vrednosti prejšnjega dne

n = 2
data$vrednost5[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj2[i] = data$RSI2[i-1] < 30
  if (data$trguj2[i] == 1){
    data$vrednost2[i] = (data$close[i]/data$close[i-1]) * data$vrednost2[i-1]
  } 
  else data$vrednost2[i] = data$vrednost2[i-1]
  
  data$profit2[i] = data$vrednost2[i] - data$vrednost2[i-1]
}

n = 14
data$vrednost14[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj14[i] = data$RSI14[i-1] < 30
  if (data$trguj14[i] == 1){
    data$vrednost14[i] = (data$close[i]/data$close[i-1]) * data$vrednost14[i-1]
  } 
  else data$vrednost14[i] = data$vrednost14[i-1]
  
  data$profit14[i] = data$vrednost14[i] - data$vrednost14[i-1]
}


data