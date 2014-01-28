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
plot(equity, plot.type = "single", col = c("black", "aquamarine", "blue", "green", "pink", 
                                           "brown", "orange", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "pink", "brown", "orange", "purple", "red"),
       legend = colnames(equityVseStrategije), lty = 1)

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
