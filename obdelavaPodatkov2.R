setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# install.packages("fImport")
library(fImport) 

## v S&P500.txt imam shranjeno katerih 500 papirjev je v indeksu S&P500
con = file("S&P500.txt")
papirji = readLines(con)
close(con)

## če še nimam podatkov, potegnem podatke za teh 500 papirjev iz yahooja in jih shranim v SP500.rda
## traja fuuul časa!!!
# SP500 = yahooSeries(papirji, from = "2000-01-01", to = "2013-11-01")
# save(SP500,file="./SP500.rda")

## naložim podatke iz SP500.rda, če jih že imam shranjene - spremenjlivka SP500
load("./SP500.rda")

## potrebujem samo close vrednosti
imena.Close = sapply(papirji, paste0, ".Close")
podatki = SP500[, imena.Close]

# nekaj o podatkih
summary(podatki)
str(podatki)
head(podatki)

## pogledam kateri papirji imajo več kot 10 NA-jev:
NA10 = apply(podatki, 2, function(x) sum(is.na(x)) > 10)
sum(NA10)
colnames(podatki)[NA10]
## 88 je takih

## nekaj o teh 88 papirjih
summary(podatki[, NA10])
head(podatki[, NA10])

## papirji, ki imajo med 10 in 100 NAjev
NA100 = apply(podatki, 2, function(x) sum(is.na(x)) > 10 && sum(is.na(x)) < 100)
sum(NA100)

summary(podatki[, NA100])
head(podatki[, NA100], 50)

## pogledam kateri papirji imajo kakšen NA:
anyNA =  apply(podatki, 2, function(x) sum(is.na(x)) > 0)
sum(anyNA)

## obdržim samo papirje, ki imajo manj kot 10 NA-jev
podatki2 = podatki[, apply(podatki, 2, function(x) sum(is.na(x)) < 10)]
ncol(podatki2)

## ostanejo mi podatki za 412 papirjev
sum(is.na(podatki2))
# imam 1413 NA-jev

## pogledam če je kakšen dan, kjer so skoraj vsi papirji NA:
datumNA = apply(podatki2, 1, function(x) sum(is.na(x)) > 300)
sum(datumNA)

## pogledam koliko dni ima kakšne NA:
anyNA = apply(podatki2, 1, function(x) sum(is.na(x)) > 0)
sum(anyNA)
# takih dni je 21

## vržem te dni ven ali nadomestim NA z ???????????
## vržem vn:
# data = podatki2[apply(podatki2, 1, function(x) sum(is.na(x)) == 0), ]
data = podatki2

## shranim obdelane podatke v data.rda
save(data,file="./data.rda")

