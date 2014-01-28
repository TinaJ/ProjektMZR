## UPORABA METODE CSCV

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

## POŽENI KODO IZ CSCV.r!!!

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
# load("./data/CSCV.rda")
# load("./data/CSCV5let.rda")
# load("./data/CSCVprvih5let.rda")
# load("./data/CSCVS10.rda")
# load("./data/CSCV5letS10.rda")
# load("./data/CSCVprvih5letS10.rda")
# #######################################

# ## PODATKI, KI JIH POTREBUJEM: dnevni donosi portfelja
# load("./data/dnevniDonosiPortfelja.rda")

# # matrika M
# M = dnevniDonosiPortfelja - 1

# ####################
# ## test funkcije CSCV na majhni matriki
# matrikaDonosov = function(T, N) {
  # matrix(data = rnorm(T*N), nrow = T, ncol = N)
# }

# A = matrikaDonosov(100, 5)
# CSCV(A, 10)


# ######################
# ## CSCV na podatkih iz uporabaStrategij.R:
# dim(M)
# S = 20

# # ker št. vrstic matrike M (3330) ni deljvo z 20, vržem vn zadnjih 10 vrstic
# M1 = M[1:3320, ]

# datoteka1 = "./data/CSCV.rda"
# CSCV(M1, 20, datoteka1)


# #################
# ## CSCV na zadnjih 5 letih matrike M
# # 5 let je približno 1280 trgovlanih dni
# M2 = M[2051:3330, ]
# datoteka2 = "./data/CSCV5let.rda"
# CSCV(M2, 20, datoteka2)


# #################
# ## CSCV na prvih 5 letih matrike M
# # 5 let je približno 1280 trgovlanih dni
# M3 = M[1:1280, ]
# datoteka3 = "./data/CSCVprvih5let.rda"
# CSCV(M3, 20, datoteka3)


# ######################
# ## CSCV na vseh 13 letih matrike M, s pol manjšim S
# # matrika M:
# S = 10
# datoteka4 = "./data/CSCVS10.rda"
# CSCV(M, 10, datoteka4)


# #################
# ## CSCV na zadnjih 5 letih matrike M, s pol manjšim S
# # 5 let je približno 1280 trgovlanih dni
# M5 = M[2051:3330, ]
# datoteka5 = "./data/CSCV5letS10.rda"
# CSCV(M5, 10, datoteka5)


# #################
# ## CSCV na prvih 5 letih matrike M, s pol manjšim S
# # 5 let je približno 1280 trgovlanih dni
# M6 = M[1:1280, ]
# datoteka6 = "./data/CSCVprvih5letS10.rda"
# CSCV(M6, 10, datoteka6)


###########################3
############################3

## PODATKI, KI JIH POTREBUJEM: dnevni donosi portfelja
# load("./data/dnevniDonosiPortfelja.rda")
# 
# # matrika M
# M = dnevniDonosiPortfelja - 1
# dim(M)
# # v M so podatki o donosih od 8.8.2000 do 1.11.2013 ==> za 3330 trgovalnih dni (12 let oz. 148 mesecev oz. 640 tednov)
# 
# # vzamem podatke za 5 let: od 2.1.2008 do 2.1.2013
# which(rownames(M)=="2008-01-01")
# which(rownames(M)=="2008-01-02")
# which(rownames(M)=="2013-01-01")
# which(rownames(M)=="2013-01-02")
# # ==> indeksi 1860 do 3119 ==> 1260 podatkov
# matrikaM = M[1860:3119,]
####################

load("./data/dnevniDonosiPortfelja-5let.rda")

# matrika M
M = dnevniDonosiPortfelja - 1
dim(M)
# v M so podatki o donosih od 2.1.2008 do 2.1.2013
# ==> 1260 podatkov

# S vzamem tak, da ena podmatrika predstavlja približno 1 mesec
# ==> v 5 letih je 60 mesecev ==> S = 60
# št. kombinacij: 118 264 581 564 861 424
S = 60
# ==> funkcija combn(60, 30) vrne napako, ker je preveč kombinacij
# ==> največji možni S, da funkcija combn(S, S/2) ne vrne napake je 28
# ==> ena podmatrika predstavlja približno 2 meseca
# ==> za S=28 mi vrne napako, da imam premalo delovnega spomina

# CSCV naredim na vseh možnih kombinacijah 9 strategij: 512 kombinacij

# CSCV po 1 strategijo ==> NE DELA!!!!
for (i in 1:9){
  CSCV(as.matrix(M[,i]), 10, datoteka = colnames(M)[i])
}

# CSCV po 2 strategiji
print("CSCV po 2 strategiji")
kombinacije2 = combn(9, 2)
for (i in 1:ncol(kombinacije2)){ 
  print(i)
  CSCV(M[,kombinacije2[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije2[1,i]], colnames(M)[kombinacije2[2,i]]))
}


# CSCV po 3 strategije
print("CSCV po 3 strategiji")
kombinacije3 = combn(9, 3)
for (i in 1:ncol(kombinacije3)){ 
  print(i)
  CSCV(M[,kombinacije3[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije3[1,i]], 
                        colnames(M)[kombinacije3[2,i]],
                        colnames(M)[kombinacije3[3,i]]
       ))
}

# CSCV po 4 strategije
print("CSCV po 4 strategiji")
kombinacije4 = combn(9, 4)
for (i in 1:ncol(kombinacije4)){ 
  print(i)
  CSCV(M[,kombinacije4[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije4[1,i]], 
                        colnames(M)[kombinacije4[2,i]],
                        colnames(M)[kombinacije4[3,i]],
                        colnames(M)[kombinacije4[4,i]]))
}

# CSCV po 5 strategij
print("CSCV po 5 strategiji")
kombinacije5 = combn(9, 5)
for (i in 1:ncol(kombinacije5)){ 
  print(i)
  CSCV(M[,kombinacije5[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije5[1,i]], 
                        colnames(M)[kombinacije5[2,i]],
                        colnames(M)[kombinacije5[3,i]],
                        colnames(M)[kombinacije5[4,i]],
                        colnames(M)[kombinacije5[5,i]]))
}

# CSCV po 6 strategij
print("CSCV po 6 strategiji")
kombinacije6 = combn(9, 6)
for (i in 1:ncol(kombinacije6)){ 
  print(i)
  CSCV(M[,kombinacije6[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije6[1,i]], 
                        colnames(M)[kombinacije6[2,i]],
                        colnames(M)[kombinacije6[3,i]],
                        colnames(M)[kombinacije6[4,i]],
                        colnames(M)[kombinacije6[5,i]],
                        colnames(M)[kombinacije6[6,i]]))
}

# CSCV po 7 strategij
print("CSCV po 7 strategiji")
kombinacije7 = combn(9, 7)
for (i in 1:ncol(kombinacije7)){
  print(i)
  CSCV(M[,kombinacije7[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije7[1,i]], 
                        colnames(M)[kombinacije7[2,i]],
                        colnames(M)[kombinacije7[3,i]],
                        colnames(M)[kombinacije7[4,i]],
                        colnames(M)[kombinacije7[5,i]],
                        colnames(M)[kombinacije7[6,i]],
                        colnames(M)[kombinacije7[7,i]]))
}

# CSCV po 8 strategij
print("CSCV po 8 strategiji")
kombinacije8 = combn(9, 8)
for (i in 1:ncol(kombinacije8)){
  print(i)
  CSCV(M[,kombinacije8[, i]], 20, 
       datoteka = paste(colnames(M)[kombinacije8[1,i]], 
                        colnames(M)[kombinacije8[2,i]],
                        colnames(M)[kombinacije8[3,i]],
                        colnames(M)[kombinacije8[4,i]],
                        colnames(M)[kombinacije8[5,i]],
                        colnames(M)[kombinacije8[6,i]],
                        colnames(M)[kombinacije8[7,i]],
                        colnames(M)[kombinacije8[8,i]]))
}

# CSCV po 9 strategij - vse strategije
print("CSCV po 9 strategiji")
CSCV(M, 20, datoteka = "vse strategije")

## PRIDOBIM PODATKE
# ustvarim tabele kjer je notri ime strategij v kombinaciji in verjetnost prekomernega prileganja
load("./data/dnevniDonosiPortfelja-5let.rda")
mapa = "./CSCV-5let/"

# CSCV po 2 strategiji
print("CSCV po 2 strategiji")
kombinacije2 = combn(9, 2)
PBO2 = matrix(data = NA, nrow = ncol(kombinacije2), ncol = 2, 
              dimnames = list(1:ncol(kombinacije2), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije2)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije2[1,i]], colnames(dnevniDonosiPortfelja)[kombinacije2[2,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO2[i, ] = c(datoteka, fi)
}


# CSCV po 3 strategije
print("CSCV po 3 strategiji")
kombinacije3 = combn(9, 3)
PBO3 = matrix(data = NA, nrow = ncol(kombinacije3), ncol = 2, 
              dimnames = list(1:ncol(kombinacije3), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije3)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije3[1,i]], 
                   colnames(dnevniDonosiPortfelja)[kombinacije3[2,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije3[3,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO3[i, ] = c(datoteka, fi)
}


# CSCV po 4 strategije
print("CSCV po 4 strategiji")
kombinacije4 = combn(9, 4)
PBO4 = matrix(data = NA, nrow = ncol(kombinacije4), ncol = 2, 
              dimnames = list(1:ncol(kombinacije4), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije4)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije4[1,i]], 
                   colnames(dnevniDonosiPortfelja)[kombinacije4[2,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije4[3,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije4[4,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO4[i, ] = c(datoteka, fi)
}


# CSCV po 5 strategij
print("CSCV po 5 strategiji")
kombinacije5 = combn(9, 5)
PBO5 = matrix(data = NA, nrow = ncol(kombinacije5), ncol = 2, 
              dimnames = list(1:ncol(kombinacije5), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije5)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije5[1,i]], 
                   colnames(dnevniDonosiPortfelja)[kombinacije5[2,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije5[3,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije5[4,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije5[5,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO5[i, ] = c(datoteka, fi)
}


# CSCV po 6 strategij
print("CSCV po 6 strategiji")
kombinacije6 = combn(9, 6)
PBO6 = matrix(data = NA, nrow = ncol(kombinacije6), ncol = 2, 
              dimnames = list(1:ncol(kombinacije6), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije6)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije6[1,i]], 
                   colnames(dnevniDonosiPortfelja)[kombinacije6[2,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije6[3,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije6[4,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije6[5,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije6[6,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO6[i, ] = c(datoteka, fi)
}


# CSCV po 7 strategij
print("CSCV po 7 strategiji")
kombinacije7 = combn(9, 7)
PBO7 = matrix(data = NA, nrow = ncol(kombinacije7), ncol = 2, 
              dimnames = list(1:ncol(kombinacije7), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije7)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije7[1,i]], 
                   colnames(dnevniDonosiPortfelja)[kombinacije7[2,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije7[3,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije7[4,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije7[5,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije7[6,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije7[7,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO7[i, ] = c(datoteka, fi)
}



# CSCV po 8 strategij
print("CSCV po 8 strategiji")
kombinacije8 = combn(9, 8)
PBO8 = matrix(data = NA, nrow = ncol(kombinacije8), ncol = 2, 
              dimnames = list(1:ncol(kombinacije8), c("strategije", "PBO")))
for (i in 1:ncol(kombinacije8)){ 
  print(i)
  datoteka = paste(colnames(dnevniDonosiPortfelja)[kombinacije8[1,i]], 
                   colnames(dnevniDonosiPortfelja)[kombinacije8[2,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije8[3,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije8[4,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije8[5,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije8[6,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije8[7,i]],
                   colnames(dnevniDonosiPortfelja)[kombinacije8[8,i]])
  datoteka = paste(mapa, datoteka, sep="")
  load(datoteka)
  PBO8[i, ] = c(datoteka, fi)
}


# CSCV po 9 strategij - vse strategije
print("CSCV po 9 strategiji")
datoteka = "vse strategije"
datoteka = paste(mapa, datoteka, sep="")
load(datoteka)
PBO9 = matrix(data = c(datoteka, fi), nrow = 1, ncol = 2, dimnames = list(1, c("strategije", "PBO")))

save(PBO2, PBO3, PBO4, PBO5, PBO6, PBO7, PBO8, PBO9, file = "./CSCV-5let/PBO-ji.rda")

## poiščem min in max PBO v vsaki PBOi matriki:
load("./CSCV-5let/PBO-ji.rda")
min2 = which.min(PBO2[,2])
max2 = which.max(PBO2[,2])
PBO2[min2,]
PBO2[max2,]
min3 = which.min(PBO3[,2])
max3 = which.max(PBO3[,2])
PBO3[min3,]
PBO3[max3,]
min4 = which.min(PBO4[,2])
max4 = which.max(PBO4[,2])
PBO4[min4,]
PBO4[max4,]
min5 = which.min(PBO5[,2])
max5 = which.max(PBO5[,2])
PBO5[min5,]
PBO5[max5,]
min6 = which.min(PBO6[,2])
max6 = which.max(PBO6[,2])
PBO6[min6,]
PBO6[max6,]
min7 = which.min(PBO7[,2])
max7 = which.max(PBO7[,2])
PBO7[min7,]
PBO7[max7,]
min8 = which.min(PBO8[,2])
max8 = which.max(PBO8[,2])
PBO8[min8,]
PBO8[max8,]

PBO9

######################
## naredim CSCV na 13 letih za tisti kombinaciji po 3 in 5 strategij, 
# ki so imele max in min PBO za 5 let in na vseh strategijah
load("./data/dnevniDonosiPortfelja.rda")

# matrika M
M = dnevniDonosiPortfelja - 1
dim(M)
# v M so podatki o donosih od 1.1.2000 do 1.11.2013
# ==> 3330 podatkov ==> zadnjih 10 vržem vn, da bo deljivo z 20
# ==> podatki od 22.8.2000 do 1.11.2013 ==> 3320 podatkov

CSCV(M[11:3330,c(5,8,9)], 20, 
     datoteka = "majhen_overfit_3strategije_13let")
CSCV(M[11:3330,c(4,6,7)], 20, 
     datoteka = "velik_overfit_3strategije_13let")

CSCV(M[11:3330,c(2,5,6,8,9)], 20, 
       datoteka = "majhen_overfit_5strategij_13let")
CSCV(M[11:3330,c(3,4,6,7,9)], 20, 
     datoteka = "velik_overfit__5strategij_13let")

CSCV(M[11:3330,], 20, datoteka = "vse strategije_13let")

load("majhen_overfit_3strategije_13let")
head(M)
fi

load("velik_overfit_3strategije_13let")
head(M)
fi

load("majhen_overfit_5strategij_13let")
head(M)
fi

load("velik_overfit__5strategij_13let")
head(M)
fi

load("vse strategije_13let")
head(M)
fi