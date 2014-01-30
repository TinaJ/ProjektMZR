## GRAFI ZA VELIK IN MAJHEN PBO
setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")
library(timeSeries)
########################
load("./data/equityVseStrategije.rda")

## vse strategije za 13 let
equity = as.timeSeries(equityVseStrategije)
plot(equity, plot.type = "single", col = c("black", "aquamarine", "blue", "green", "pink", 
                                           "brown", "orange", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "pink", "brown", "orange", "purple", "red"),
       legend = colnames(equityVseStrategije), lty = 1)



## za 13 let
#majhen overfit
plot(as.timeSeries(equityVseStrategije[, c(5,8,9)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(5,8,9)]), lty = 1)

plot(as.timeSeries(equityVseStrategije[, c(2,5,6,8,9)]), plot.type = "single",
     col = c("black", "blue", "green", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green", "purple", "red"),
       legend = colnames(equityVseStrategije[, c(2,5,6,8,9)]), lty = 1)

#velik overfit
plot(as.timeSeries(equityVseStrategije[, c(4,6,7)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "bottomright", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(4,6,7)]), lty = 1)

plot(as.timeSeries(equityVseStrategije[, c(3,4,6,7,9)]), plot.type = "single",
     col = c("black", "blue", "green", "orange", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "bottomright", col = c("black", "blue", "green", "orange", "red"),
       legend = colnames(equityVseStrategije[, c(3,4,6,7,9)]), lty = 1)






#########################
load("./data/equityVseStrategije-5let.rda")

## vse strategije za 5 let
equity = as.timeSeries(equityVseStrategije)
plot(equity, plot.type = "single", col = c("black", "aquamarine", "blue", "green", "darkgreen", 
                                           "brown", "orange", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Vrednost", main = "Vrednost portfelja")
legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "darkgreen", "brown", "orange", "purple", "red"),
       legend = colnames(equityVseStrategije), lty = 1)

#majhen overfit
plot(as.timeSeries(equityVseStrategije[, c(5,8,9)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(5,8,9)]), lty = 1)

plot(as.timeSeries(equityVseStrategije[, c(2,5,6,8,9)]), plot.type = "single",
     col = c("black", "blue", "green", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Vrednost", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green", "purple", "red"),
       legend = colnames(equityVseStrategije[, c(2,5,6,8,9)]), lty = 1)

#velik overfit
plot(as.timeSeries(equityVseStrategije[, c(4,6,7)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "bottomright", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(4,6,7)]), lty = 1)

plot(as.timeSeries(equityVseStrategije[, c(3,4,6,7,9)]), plot.type = "single",
     col = c("black", "blue", "green", "orange", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "bottomright", col = c("black", "blue", "green", "orange", "red"),
       legend = colnames(equityVseStrategije[, c(3,4,6,7,9)]), lty = 1)


#################3
# strategije in SP500 indeks
library(TTR)
load("./data/SP1leto.rda")
SP.SMA5 = SMA(SP1leto, n = 5)
SP.SMA25 = SMA(SP1leto, n = 25)
SP.SMA50 = SMA(SP1leto, n = 50)
SP.SMA150 = SMA(SP1leto, n = 150)

plot(SP1leto, xlab = "Čas", ylab = "Vrednost", main = "SP500 & SMA25")
lines(SP.SMA25, col = "blue")

SP.RSI2 = RSI(SP1leto, n = 2)
SP.RSI14 = RSI(SP1leto, n = 14)

plot(SP.RSI2, xlab = "Čas", ylab = "Vrednost", main = "RSI2")
abline(h = 30, col = "red")
abline(h = 70, col = "red")

SP.Bollinger = as.timeSeries(BBands(SP1leto, n = 20, sd = 1))

plot(SP1leto, xlab = "Čas", ylab = "Vrednost", main = "SP500 & Bollingerjevi pasovi")
lines(SP.Bollinger[,1], col = "blue")
lines(SP.Bollinger[,2], col = "green")
lines(SP.Bollinger[,3], col = "red")

########################
########################3
# Rolling performance graf
library(PerformanceAnalytics)
library(quantmod)
library(timeSeries)

load("./data/equityVseStrategije-5let.rda")
load("./data/dnevniDonosiPortfelja-5let.rda")

tedenski_donosi = c()
for (i in 1:9){
  tedenski_donosi = cbind(tedenski_donosi, weeklyReturn(equityVseStrategije[,i], type='arithmetic'))
}
plot(as.timeSeries(tedenski_donosi[,c(5,8,9)]), plot.type = "single", 
     col = c("black", "red", "green"),
     main = "Tedenski donosi", 
     xlab = "Čas", ylab = "Donos")
legend(x = "bottomright", legend = colnames(equityVseStrategije[, c(5,8,9)]), lty = 1, 
       col = c("black", "red", "green"))

plot(as.timeSeries(tedenski_donosi[,c(4,6,7)]), plot.type = "single", 
     col = c("black", "red", "green"), main = "Tedenski donosi", 
     xlab = "Čas", ylab = "Donos")
legend(x = "bottomright", legend = colnames(equityVseStrategije[, c(4,6,7)]), lty = 1, 
       col = c("black", "red", "green"))

mesecni_donosi = c()
for (i in 1:9){
  mesecni_donosi = cbind(mesecni_donosi, monthlyReturn(equityVseStrategije[,i], type='arithmetic'))
}
plot(as.timeSeries(mesecni_donosi[,c(5,8,9)]), plot.type = "single", 
     col = c("black", "red", "green"), main = "Mesečni donosi", 
     xlab = "Čas", ylab = "Donos")
legend(x = "bottomright", legend = colnames(equityVseStrategije[, c(5,8,9)]), lty = 1, 
       col = c("black", "red", "green"))

plot(as.timeSeries(mesecni_donosi[,c(4,6,7)]), plot.type = "single", 
     col = c("black", "red", "green"),
     main = "Mesečni donosi", 
     xlab = "Čas", ylab = "Donos")
legend(x = "bottomright", legend = colnames(equityVseStrategije[, c(4,6,7)]), lty = 1, 
       col = c("black", "red", "green"))

plot(as.timeSeries(mesecni_donosi[,c(3,4,6,7,9)]), plot.type = "single", 
     col = c("black", "blue", "green", "pink", "red"), main = "Mesečni donosi", 
     xlab = "Čas", ylab = "Donos")
legend(x = "bottomright", legend = colnames(equityVseStrategije[, c(3,4,6,7,9)]), lty = 1, 
       col = c("black", "blue", "green", "pink", "red"))

plot(as.timeSeries(mesecni_donosi[,c(2,5,6,8,9)]), plot.type = "single", 
     col = c("black", "blue", "green", "pink", "red"),
     main = "Mesečni donosi", 
     xlab = "Čas", ylab = "Donos")
legend(x = "bottomright", legend = colnames(equityVseStrategije[, c(2,5,6,8,9)]), lty = 1, 
       col = c("black", "blue", "green", "pink", "red"))


c(3,4,6,7,9)
# # tedenske_razlike = diff(equityVseStrategije, lag = 7)
# # tedenski_donos = apply(tedenske_razlike, 2, function(X) sapply(2:length(X), function(i) X[i]/X[i-1]))
# 
# tedenski_donos = apply(equityVseStrategije, 2, function(X) sapply(8:length(X), function(i) X[i] / X[i-7]))
# log_tedenski_donos = log(tedenski_donos)
# plot(as.timeSeries(log_tedenski_donos[,c(5,8,9)]), plot.type = "single")
# 
# mesecne_razlike = diff(equityVseStrategije, lag = 30)
# mesecni_donos = apply(equityVseStrategije, 2, function(X) sapply(31:length(X), function(i) X[i]/X[i-30]))
# log_mesecni_donos = log(mesecni_donos)
# plot(as.timeSeries(mesecni_donos[,c(5,8,9)]-1), plot.type = "single")
# 
# 
# library(TTR)
# povprecen_tedenski_donos = apply(dnevniDonosiPortfelja, 2, SMA, n = 7)
# plot(as.timeSeries(povprecen_tedenski_donos[,c(5,8,9)]), plot.type = "single")
# 
# povprecen_mesecni_donos = apply(dnevniDonosiPortfelja, 2, SMA, n = 30)
# plot(as.timeSeries(povprecen_mesecni_donos[,c(5,8,9)]), plot.type = "single")
# 
# plot(as.timeSeries(log(povprecen_tedenski_donos[,c(4,6,7)]), plot.type = "single")
# plot(as.timeSeries(log(povprecen_mesecni_donos[,c(4,6,7)])), plot.type = "single")

     
