setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# install.packages("fImport")
# library(fImport) 

# SP = yahooSeries("^GSPC", from = "2000-01-01", to = "2013-11-01")
# save(SP, file = "./SP13let.rda")
load("./SP13let.rda")

closePrice = SP[,4]
closePrice$SMA5 = SMA(closePrice, n=5)

# strategija: če je nad SMA in v downtrendu prodaj, če je pod SMA in v uptrendu kupi
# uptrend: višje kot en dan prej
# downtrend: nižje kot en dan prej
# prodaj: -1
# kupi: 1
# sicer: 0

strategija = function(price, SMA5){
  if(price > SMA5 && price < price[-1]){
    pozicija = -1
  } elif(price < SMA5 && price > price[-1]){
    pozicija = 1
  } else
    pozicija = 0
}

closePrice$position = 