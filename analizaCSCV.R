# analiziram podatke, ki sem jih dobila v CSCV

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

load("./data/sprem.rda")
load("./data/vhodne.rda")

## imam N = 9 strategij, za vsako imam T = 3320 donosov
## Matriko M razdelim na S = 20 podmatrik
S

# koliko kombinacij je naredil
dim(Cs)

# performance statistics za vsako strategijo IS za vse kombinacije ( strategije so vrstice, kombinacije so stolpci)
dim(Rji)

# za vsako kombinacijo določim katera strategija ima najboljši performance (največji sharpe ratio)
table(nji)
summary(nji)

# performance statistics OOS
dim(Rbars)

# relativni rank OOS za strategijo, ki je najboljša IS
table(omega)
summary(omega)

# logit relativnega ranka
table(L)
summary(L)

# relativna frekvenca logita -> verjetnost da lambda zavzame določeno vrednost
f
sum(f)

# ofstotek, v katerem je lambda negativna
fi

##############
# sharpe za vsako strategijo na vseh podatkih
Msharpe = apply(M, 2, sharpe)
Msharpe
which(Msharpe == max(Msharpe))

# sharpe za vsako strategijo na vseh podatkih
Msharpe2 = apply(M, 2, function(x) mean(x)/sd(x))
Msharpe2
which(Msharpe2 == max(Msharpe2))