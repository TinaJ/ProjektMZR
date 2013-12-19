setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

## Narišem equity za vse strategije na en graf:
load("./data/equityVseStrategije.rda")

# določim maksimalno in minimalno  vrednost, za y os:
y.max = max(equityVseStrategije)
y.min = min(equityVseStrategije)

plot(1:nrow(equityVseStrategije), equityVseStrategije[, 1], "l", ylim = c(y.min, y.max),
     xlab = "dnevi", ylab = "equity", main = "equity za posamezno strategijo")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 2], "l", col = "aquamarine")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 3], "l", col = "blue")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 4], "l", col = "green")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 5], "l", col = "pink")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 6], "l", col = "brown")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 7], "l", col = "orange")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 8], "l", col = "grey")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 9], "l", col = "purple")
lines(1:nrow(equityVseStrategije), equityVseStrategije[, 10], "l", col = "red")
legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "pink", "brown", "orange", "grey", "purple", "red"),
       legend = colnames(equityVseStrategije), lty = 1)
## bollingerja sta enaka, zato enega ni :)


## graf z datumi :)
equity = as.timeSeries(equityVseStrategije)
plot(equity, plot.type = "single", col = c("black", "aquamarine", "blue", "green", "pink", 
                                      "brown", "orange", "grey", "purple", "red"), 
     at = "auto", xlab = "Čas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "aquamarine", "blue", "green", "pink", "brown", "orange", "grey", "purple", "red"),
       legend = colnames(equityVseStrategije), lty = 1)

# graf za RSI14:
plot(1:3481, RSI14[, 1], "l")
lines(1:3481, c(1:3481)*0+30, col = "red")

plot(1:3481, RSI14[, 2], "l")
lines(1:3481, c(1:3481)*0+30, col = "red")

# koliko trgovanj izvede RSI14 na vseh delnicah
sum(RSI14 < 30, na.rm = TRUE)
