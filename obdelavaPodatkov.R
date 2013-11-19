setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# install.packages("fImport")
library(fImport) 

## v S&P500.txt imam shranjeno katerih 500 papirjev je v indeksu S&P500
con = file("S&P500.txt")
papirji = readLines(con)
close(con)

## če še nimam podatkov, potegnem podatke za teh 500 papirjev iz yahooja in jih shranim v SP500.rda
# SP500 = yahooSeries(papirji, from = "2000-01-01", to = "2013-11-01")
# save(SP500,file="./SP500.rda")

## naložim podatke iz SP500.rda, če jih že imam shranjene - spremenjlivka SP500
load("./SP500.rda")

<<<<<<< HEAD
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
data = podatki2[apply(podatki2, 1, function(x) sum(is.na(x)) == 0), ]

## shranim obdelane podatke v data.rda
save(data,file="./data.rda")

=======
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


############################################################
load("./podatki.rda")
## delam na podatkih v spremenljivki tabela
## za vsak papir potrebujem samo close ceno
## naredim seznam imen papirjev, ki so v tabela:
# prvi element vsakega elementa v seznamu:
firstElement <- function(x){x[1]} ,
# ločim imena stolpcev glede na piko in določim prvi element vsakega imena, vzamem samo eno ponovitev vsakega imena:
imena = unique(sapply(strsplit(colnames(tabela),"\\."),firstElement))
# imenom dodam ".Close"
imena.Close = sapply(imena, paste0, ".Close")

imena.Close %in% colnames(tabela)
imena.Close[!(imena.Close %in% colnames(tabela))] # katera so v imena.Close in niso v tabeli
# --> BRK.Close in BF.Close ## zakaj???
imena.CloseOK = imena.Close[imena.Close %in% colnames(tabela)] # tisti papirji, ki imajo close vrednost v tabeli
data = tabela[, imena.CloseOK] # za vsak papir, ki pride v poštev samo close vrednost
save(data, file = "./CloseVrednosti2000.rda")

##############################################################3
>>>>>>> parent of 5bfcc15... popravki
