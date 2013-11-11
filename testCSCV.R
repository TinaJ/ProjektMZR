## Metoda kombinatoričnega simetričnega preverjanja
## Repliciran postopek iz članka (poglavje 2.2)

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

# TODO: preveri vse funkcije, če delajo, na simpel majhnem primeru!!

## funkcija, ki zgenerira TxN matriko donosov:
matrikaDonosov = function(T, N) {
  matrix(data = rnorm(T*N), nrow = T, ncol = N)
}

M = matrikaDonosov(1000, 100)
# kasneje mora biti M matrika donosov, ki jo dobiš iz svojih strategij

S = 20

## matriko M razdelim na S podmatrik dimenzij T/S x N
## funkcija, ki pove, katere vrstice matrike M pripadajo podmatriki Ms, s = 1, ..., S
podmatrika = function(M, S, s){
  st.vrstic = nrow(M)/S
  a = st.vrstic * (s-1) + 1
  b = st.vrstic * s
  a : b
}

## vse možne kombinacije podmatrik matrike M, kjer izberemo S/2 podmatrik:
## funkcija vrne matriko kjer je v vsakem stolpcu ena možna kombinacija podmatrik
## vrednosti v matriki so zaporedne številke podmatrik Ms, oz. s-ji
kombinacije = function(S) {
  combn(S, S/2)
}

Cs = kombinacije(S)

## za vsako kombinacijo c V Cs naredimo matriko J iz podmatrik, ki so v c
## funkcija, ki vrne katere vrstice iz M so v J
library(gdata)
trainingSet = function(M, c){
  vrstice = unmatrix(sapply(c, podmatrika, M=M, S=length(c)*2))
  names(vrstice) = NULL  
  vrstice
}

## za vsak stolpec v Cs izračunam trainingSet
podmatrike = apply(Cs, MARGIN = 2, FUN = trainingSet, M = M)
## podmatrike je T/2 x length(Cs) matrika
## v vsakem stolpcu je vektor, ki pove katere vrstice v matriki M so v J pri dani kombinaciji


## preostale podmatrike, ki niso v c, damo v matriko Jbar
## funkcija, ki vrne katere vrstice iz M niso v J:
testSet = function(M, c) {
  setdiff(1:nrow(M), trainingSet(M, c))
}

## za vsak stolpec v Cs izračunam testSet
komplement = apply(Cs, MARGIN = 2, FUN = testSet, M = M)
## komplement je T/2 x length(Cs) matrika
## v vsakem stolpcu je vektor, ki pove katere vrstice v matriki M niso v J oz. so v Jbar pri dani kombinaciji


## za vsak stolpec v J izračunam "performance statistics" -- vektor R
## Sharpe Ratio: apply(J, 1, sharpe)
install.packages("tseries")
library(tseries)

# ## za vse možne J izračunam vektor R in jih zložim v matriko Rji po vrsticah
# Rji = c()
# for(i in 1:ncol(Cs)){
#   if (i %in% seq(from = 1, to = ncol(Cs), by = 100)){
#     print(i)
#   }
#   # najprej določim kaj je matrika J za en primer: M[stolpec v podmatrike, ]
#   J = M[podmatrike[ , i], ]
#   # za vsak stolpec v J izračunam sharpe ratio
#   R = apply(J, 2, sharpe)
#   Rji = rbind(Rji, R)
# }

# na stolpcih podmatrike komplement uporabim funkcijo, ki generira matriko J in izračuna njen R
R = function(stolpec){
  apply(M[stolpec, ], 2, sharpe)
}

Rji = apply(podmatrike, 2, R)
# N x length(Cs) matrika - v vsakem solpcu je en R


## za vsak vektor R določim kateri element v R ima največjo vrednost - katera strategija je najboljša IS
## za vsak stolpec v Rji določim kateri je max, dobim vektor maksimumov
nji = apply(Rji, 2, which.max)


## za vsak stolpec v Jbar izračunam "performance statistics" -- vektor Rbar
# ## za vse možne Jbar izračunam vektor Rbar in jih zložim v matriko Rbars po vrsticah
# Rbars = c()
# for(i in 1:ncol(Cs)){
#   if (i %in% seq(from = 1, to = ncol(Cs), by = 100)){
#     print(i)
#   }
#   # najprej določim kaj je matrika Jbar za en primer: M[stolpec v komplement, ]
#   Jbar = M[komplement[ , i], ]
#   # za vsak stolpec v Jbar izračunam sharpe ratio
#   Rbar = apply(Jbar, 2, sharpe)
#   Rbars = rbind(Rbars, Rbar)
# }

# na stolpcih podmatrike komplement uporabim funkcijo, ki generira matriko Jbar in izračuna njen Rbar
Rbar = function(stolpec){
  apply(M[stolpec, ], 2, sharpe)
}

Rbars = apply(komplement, 2, Rbar)
# N x length(Cs) matrika - v vsakem solpcu je en Rbar


## določim relativni rank strategije n v Rbar:
# rank(Rbar) -> najmanjše število v Rbar ima najmanjši rank
# omega = rank(Rbar)[n]/length(Rbar)
## za vsak stolpec v Rbars določim omego:
omega = c()
for(i in 1:ncol(Rbars)){
  w = rank(Rbars[ , i])[nji[i]]/length(Rbars[ , i])
  omega = c(omega, w)
}

# w = function(Rbar, n){
#   rank(Rbar)[n]/length(Rbar)
# }
# 
# Rbars = rbind(Rbars, nji)
# N = ncol(M)
# omega = apply(Rbars, 2, w, Rbar = Rbars[1:(N-1), ], n = Rbars[N, ])
# # vektor omeg

## definiram logit lambda:
lambda = function(omega){
  log(omega/(1-omega))
}

L = sapply(omega, lambda)

## porazdelitev lambd:
## za vsak c v Cs imam eno lambdo -- vektor L, izračunam relativno frekvenco za vsako lambdo:
f = table(L)/length(L) # relativna frekvenca za vsako različno lambdo
sum(f)

## PBO = probabitily of backtest overfit
## PBO lahko ocenim s tistimi lambdami, ki so negativne:
negativne = names(f) < 0
fi = sum(f[negativne])
## fi = odstotek v katerem je optimalna strategija IS slabša od mediane OOS
fi

