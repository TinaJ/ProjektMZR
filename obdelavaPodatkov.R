## PRIPRAVA PODATKOV

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
load("./data/SP500.rda")
load("./data/podatki.rda")
#######################################

## KNJIŽNICE, KI JIH POTEBUJEM
library(fImport) 

##### PODATKI ZA VSE PAPIRJE V INDESKU SP500

## v S&P500.txt imam shranjeno katerih 500 papirjev je v indeksu S&P500
con = file("S&P500.txt")
papirji = readLines(con)
close(con)

## če še nimam podatkov, naložim podatke za teh 500 papirjev iz yahooja in jih shranim v SP500.rda
# SP500 = yahooSeries(papirji, from = "2000-01-01", to = "2013-11-01")
# save(SP500,file="./data/SP500.rda")

## potrebujem samo close vrednosti
imena.Close = sapply(papirji, paste0, ".Close")
podatki = SP500[, imena.Close]

# nekaj o podatkih
summary(podatki)
str(podatki)

# izločim tiste papirje, ki imajo tri ali več manjkajoče podatke na kupu
# ker bo interpolacija manjkajočih vrednosti v tem primeru slabša
a = c()

for (i in 1:ncol(podatki)){
  print(i)
  if (sum(is.na(podatki[,i])) > 2) {
    for (j in 3:nrow(podatki)){
      if (is.na(podatki[j, i]) && is.na(podatki[j-1, i]) && is.na(podatki[j-2, i])){
        a = append(a, i)
        break
      }
    }
  }
}

# odstranim tistih 89 papirjev, ki imajo vsaj 3 NA skupaj
podatki = podatki[, -a]

# ostale NA interpoliram
podatki = interpNA(podatki, method = "before")

# pogledam, če je še kakšen NA, ker če je na začetku ali na koncu, ga ne interpolira
sum(is.na(podatki))

# obdelane podatke shranim
save(podatki, file = "./data/podatki.rda")
