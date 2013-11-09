setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

install.packages("fImport")
library(fImport) 

SP = yahooSeries("^GSPC", from = "2000-01-01", to = "2013-11-01")

## v S&P500.txt imam shranjeno katerih 500 papirjev je v indeksu S&P500
con = file("S&P500.txt")
papirji = readLines(con)
close(con)

## če še nimam podatkov, potegnem podatke za teh 500 papirjev iz yahooja in jih shranim v SP500.rda
## traja fuuul časa!!!
if (!("SP500" %in% ls())) {
  SP500 = yahooSeries(papirji, from = "2000-01-01", to = "2013-11-01")
  save(SP500,file="./SP500.rda")
} else
  print("Podatki so že naloženi")

## naložim podatke iz SP500.rda, če jih že imam shranjene - spremenjlivka SP500
load("./SP500.rda")

datumi = rownames(SP500)
stolpci = colnames(SP500)

## grem čez papirje in izločim tiste, ki ne obstajajo dost dolg, vsaj 10 let
## če je 31.10.2003 NA
meja = datumi == "2003-10-31" # vektor TRUE in FALSE, ali je datum pravi
indeks = which(meja) # na katerem mestu je datum pravi

niPodatka = is.na(SP500[meja,]) # vektor TRUE in FALSE, ali je v vrstici z danim datumom NA ali ne
stolpciNA = stolpci[niPodatka] # imena stolpcev, kjer je NA pri danem datumu

firstElement <- function(x){x[1]} # prvi element vsakega elementa v seznamu
imena = sapply(strsplit(stolpciNA,"\\."),firstElement) # ločim imena stolpcev glede na piko in določim prvi element vsakega imena
papirjiNA = unique(imena) # kateri papirji za 31.10.2003 nimajo podatka

sum(!(is.na(SP500[1:indeks, niPodatka]))) # koliko podatkov ni NA pred danim datumom, za tiste stolpce, ki so na dani datum NA
# če bi bila številka velika, bi bilo treba še kaj gledat, ker pa ni, ni treba

# izbrišem stolpce, ki nimajo podatkov dovolj dolgo:
tabela = SP500[, !niPodatka]
rownames(tabela) = datumi

## podatki za 5 let in 10 let:
"2008-11-01" %in% datumi
"2008-10-31" %in% datumi
let5 = which(datumi == "2008-10-31")
podatki5let = tabela[let5: nrow(tabela), ]

"2003-11-01" %in% datumi
"2003-10-31" %in% datumi
let10 = which(datumi == "2003-10-31")
podatki10let = tabela[let10:nrow(tabela),]

## shranim tabelo, ter podatke za 5 in 10 let v datoteko:
save(tabela, podatki5let, podatki10let,file="./podatki.rda")

## naložim podatke
load("./podatki.rda")

