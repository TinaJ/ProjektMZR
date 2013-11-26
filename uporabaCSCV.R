## UPORABA METODE CSCV

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

## POŽENI KODO IZ CSCV.r!!!

## test funkcije CSCV na majhni matriki
matrikaDonosov = function(T, N) {
  matrix(data = rnorm(T*N), nrow = T, ncol = N)
}

A = matrikaDonosov(100, 5)
CSCV(A, 10)

## CSCV na podatkih iz uporabaStrategij.R:
# matrika M:
load("./data/M.rda")
dim(M)

S = 20

# ker št. vrstic matrike M (3330) ni deljvo z 20, vržem vn zadnjih 10 vrstic
M = M[1:3320, ]

datoteka = "./data/CSCV.rda"

CSCV(M, S, datoteka)
