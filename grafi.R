setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

library(timeSeries)

## Narišem equity za vse strategije na en graf:
load("./data/equityVseStrategije.rda")

# določim maksimalno in minimalno  vrednost, za y os:
# y.max = max(equityVseStrategije)
# y.min = min(equityVseStrategije)
# 
# plot(1:nrow(equityVseStrategije), equityVseStrategije[, 1], "l", ylim = c(y.min, y.max),
#      xlab = "dnevi", ylab = "equity", main = "equity za posamezno strategijo")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 2], "l", col = "aquamarine")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 3], "l", col = "blue")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 4], "l", col = "green")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 5], "l", col = "pink")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 6], "l", col = "brown")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 7], "l", col = "orange")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 8], "l", col = "grey")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 9], "l", col = "purple")
# lines(1:nrow(equityVseStrategije), equityVseStrategije[, 10], "l", col = "red")
# legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "pink", "brown", "orange", "grey", "purple", "red"),
#        legend = colnames(equityVseStrategije), lty = 1)
# ## bollingerja sta enaka, zato enega ni :)


## graf z datumi :)
equity = as.timeSeries(equityVseStrategije)
plot(equity, plot.type = "single", col = c("black", "aquamarine", "blue", "green", "pink", 
                                      "brown", "orange", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "pink", "brown", "orange", "purple", "red"),
       legend = colnames(equityVseStrategije), lty = 1)

# graf za RSI14:
load("./data/podatki.rda")
data = podatki
library(TTR)
RSI2 = apply(data, 2, RSI, n = 2)
RSI14= apply(data, 2, RSI, n = 14)

plot(1:3481, RSI14[, 1], "l")
lines(1:3481, c(1:3481)*0+30, col = "red")

plot(1:3481, RSI14[, 2], "l")
lines(1:3481, c(1:3481)*0+30, col = "red")

# koliko trgovanj izvede RSI14 na vseh delnicah
sum(RSI14 < 35, na.rm = TRUE)

sum(RSI2 <30, na.rm = TRUE)

########### NARIŠEM SP za zadnje leto IN NJEGOVE INDIKATORJE
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

SP.Bollinger = as.timeSeries(BBands(SP1leto, n = 20, sd = 1))

plot(SP1leto, xlab = "Čas", ylab = "Vrednost", main = "SP500 & Bollingerjevi pasovi")
lines(SP.Bollinger[,1], col = "blue")
lines(SP.Bollinger[,2], col = "green")
lines(SP.Bollinger[,3], col = "red")

############################
## GRAFI ZA VELIK IN MAJHEN PBO
setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

load("./data/equityVseStrategije-5let.rda")
load("./data/dnevniDonosiPortfelja-5let.rda")
#majhen overfit
plot(as.timeSeries(equityVseStrategije[, c(5,7,9)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(5,7,9)]), lty = 1)

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

