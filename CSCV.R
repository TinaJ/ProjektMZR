## Metoda kombinatoričnega simetričnega preverjanja
## Repliciran postopek iz članka (poglavje 2.2)


## funkcija, ki zgenerira txN matriko donosov:
matrikaDonosov = function(T, N) {
  matrix(data = rnorm(T*N), nrow = T, ncol = N)
}

M = matrikaDonosov(1000, 100)
# kasneje mora biti M matrika donosov, ki jo dobiš iz svojih strategij

## matriko M razdelim na S podmatrik dimenzij T/S x N
## funkcija, ki pove, katere vrstice matrike M pripadajo podmatriki Ms, s = 1, ..., S
podmatrika = function(M, S, s){
  st.vrstic = nrow(M)/S
  a = st.vrstic * (s-1) + 1
  b = st.vrstic * s
  a : b
}

## vse možne kombinacije podmatrik matrike M, kjer izberemo S/2 podmatrik:
## funkcija vrne matriko kjer je v vsaki vrstici ena možna kombinacija
## podatek so s-ji
kombinacije = function(S) {
  combn(S, S/2)
}

S = 20
Cs = kombinacije(S)

## za vsako kombinacijo c V Cs naredimo matriko J iz podmatrik, ki so v c
## funkcija, ki vrne katere vrstice iz M so v J
library(gdata)
trainingSet = function(M, c){
  vrstice = unmatrix(sapply(c, podmatrika, M=M, S=length(c)*2))
  names(vrstice) = NULL
  vrstice
}

c = 2:11
J = M[trainingSet(M, c), ]

## preostale podmatrike, ki niso v c, damo v matriko Jbar
## funkcija, ki vrne katere vrstice iz M niso v J:
testSet = function(M, c) {
  setdiff(1:nrow(M), trainingSet(M, c))
}
Jbar = M[testSet(M, c), ]

## za vsak stolpec v J izračunam "performance statistics" -- vektor R
## Sharpe Ratio: sharpe
install.packages("tseries")
library(tseries)
R = apply(J, 1, sharpe)

## določim kateri element v R ima največjo vrednost - katera strategija je najboljša IS
n = which.max(R)

## za vsak stolpec v Jbar izračunam "performance statistics" -- vektor Rbar
Rbar = apply(Jbar, 1, sharpe)

## določim relativni rank strategije n v Rbar:
# rank(Rbar) = najmanjše število v Rbar ima najmanjši rank
omega = rank(Rbar)[n]/length(Rbar)

## definiram logit lambda:
lambda = log(omega/(1-omega))

## porazdelitev lambd: