## GRAFI ZA VELIK IN MAJHEN PBO
setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z raèunalnikom/ProjektMZR")
library(timeSeries)
load("./data/equityVseStrategije-5let.rda")
load("./data/dnevniDonosiPortfelja-5let.rda")
#majhen overfit
plot(as.timeSeries(equityVseStrategije[, c(5,7,9)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Èas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(5,7,9)]), lty = 1)

plot(as.timeSeries(equityVseStrategije[, c(2,5,6,8,9)]), plot.type = "single",
     col = c("black", "blue", "green", "purple", "red"), 
     at = "auto", xlab = "Èas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "topleft", col = c("black", "blue", "green", "purple", "red"),
       legend = colnames(equityVseStrategije[, c(2,5,6,8,9)]), lty = 1)

#velik overfit
plot(as.timeSeries(equityVseStrategije[, c(4,6,7)]), plot.type = "single",
     col = c("black", "blue", "green"), 
     at = "auto", xlab = "Èas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "bottomright", col = c("black", "blue", "green"),
       legend = colnames(equityVseStrategije[, c(4,6,7)]), lty = 1)

plot(as.timeSeries(equityVseStrategije[, c(3,4,6,7,9)]), plot.type = "single",
     col = c("black", "blue", "green", "orange", "red"), 
     at = "auto", xlab = "Èas", ylab = "Kapital", main = "Kapital za posamezno strategijo")
legend(x = "bottomright", col = c("black", "blue", "green", "orange", "red"),
       legend = colnames(equityVseStrategije[, c(3,4,6,7,9)]), lty = 1)

