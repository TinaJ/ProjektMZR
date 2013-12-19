## UPORABA METODE CSCV

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

## POŽENI KODO IZ CSCV.r!!!

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
load("./data/CSCV.rda")
load("./data/CSCV5let.rda")
load("./data/CSCVprvih5let.rda")
load("./data/CSCVS10.rda")
load("./data/CSCV5letS10.rda")
load("./data/CSCVprvih5letS10.rda")
#######################################

## PODATKI, KI JIH POTREBUJEM: dnevni donosi portfelja
load("./data/dnevniDonosiPortfelja.rda")

# matrika M
M = dnevniDonosiPortfelja - 1

####################
## test funkcije CSCV na majhni matriki
matrikaDonosov = function(T, N) {
  matrix(data = rnorm(T*N), nrow = T, ncol = N)
}

A = matrikaDonosov(100, 5)
CSCV(A, 10)


######################
## CSCV na podatkih iz uporabaStrategij.R:
dim(M)
S = 20

# ker št. vrstic matrike M (3330) ni deljvo z 20, vržem vn zadnjih 10 vrstic
M1 = M[1:3320, ]

datoteka1 = "./data/CSCV.rda"
CSCV(M1, 20, datoteka1)


#################
## CSCV na zadnjih 5 letih matrike M
# 5 let je približno 1280 trgovlanih dni
M2 = M[2051:3330, ]
datoteka2 = "./data/CSCV5let.rda"
CSCV(M2, 20, datoteka2)


#################
## CSCV na prvih 5 letih matrike M
# 5 let je približno 1280 trgovlanih dni
M3 = M[1:1280, ]
datoteka3 = "./data/CSCVprvih5let.rda"
CSCV(M3, 20, datoteka3)


######################
## CSCV na vseh 13 letih matrike M, s pol manjšim S
# matrika M:
S = 10
datoteka4 = "./data/CSCVS10.rda"
CSCV(M, 10, datoteka4)


#################
## CSCV na zadnjih 5 letih matrike M, s pol manjšim S
# 5 let je približno 1280 trgovlanih dni
M5 = M[2051:3330, ]
datoteka5 = "./data/CSCV5letS10.rda"
CSCV(M5, 10, datoteka5)


#################
## CSCV na prvih 5 letih matrike M, s pol manjšim S
# 5 let je približno 1280 trgovlanih dni
M6 = M[1:1280, ]
datoteka6 = "./data/CSCVprvih5letS10.rda"
CSCV(M6, 10, datoteka6)
