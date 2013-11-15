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

# izračunam SMA za n = 20
# izračunam zgornji in spodnji pas z SMA +/- STD * faktor, faktor = 2
# STD izračunan na n podatkih
n = 20
faktor = 2
data$SMA = SMA(data$close, n = n)
for (i in n:nrow(data)){
  STD = sd(data$close[(i-n + 1): i])
  data$upBand[i] = data$SMA[i] + faktor * STD
  data$lowBand = data$SMA[i] - faktor * STD
}


# strategija: če je close prejšnjega dne pod spodnjim pasom prejšnjega dne, kupi
# če je close prejšnjega dne nad zgornjim pasom, prodaj - jaz to ne implementiram?

# vrednost, če kupiš je vrednos close tistega dne/close prejšnjega dne * vrednost prejšnjega dne
# če ne kupiš, je vrednost enaka vrednosti prejšnjega dne

data$vrednost[n] = 1000
for (i in (n+1) : nrow(data)){
  if (i %in% seq(from = 1, to = nrow(data), by = 100)){
    print(i)
  }
  data$trguj[i] = data$close[i-1] < data$lowBand[i-1]
  if (data$trguj[i] == 1){
    data$vrednost[i] = (data$close[i]/data$close[i-1]) * data$vrednost[i-1]
  } 
  else data$vrednost[i] = data$vrednost[i-1]
  
  data$profit[i] = data$vrednost[i] - data$vrednost[i-1]
}


data