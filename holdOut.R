## hold-out metoda:
## vrstice matrike M razdelim na 2 dela (naključno)
## izračunam sharpe ratio na prvem delu, določim strategijo, ki ima največji SR -- n
## izračunam SR na drugem delu in pogledam, če je SR strategije n < mediane SR

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
load("./data/HoldOut.rda")
#######################################


load("./data/M.rda")

HoldOut = function(M) {
  ## vrstice matrike M razdelim na 2 dela: IS in OOS
  vrstice = nrow(M)
  train = sample(1:vrstice, vrstice/2)
  IS = M[train, ]
  OOS = M[-train, ]
  
  ## izračunam SR na IS in na OOS
  Sharpe = function(x) {
    mean(x)/sd(x)
  }
  
  sharpeIS = apply(IS, 2, Sharpe)
  sharpeOOS = apply(OOS, 2, Sharpe)
  
  # pogledam katera strategija ima največji SR IS
  n = which.max(sharpeIS)
  
  # pogledam, če je SR na OOS najboljše strategije IS < mediane SR na OOS
  overfit = sharpeOOS[n] < median(sharpeOOS)
  return(c(n, overfit))
}


## ponovim hold out metodo 10000x, zlagam n-je in overfit po vrsticah v matriko

# za različne M-je (take kot CSCV):

##################
# holdout na celi M
M1 = M[1:3320, ]
MCHoldOut1 = c()
for (i in 1:100000){
  if (i %in% seq(from = 1, to = 100000, by = 500)){
    print(i)
  }
  MCHoldOut1 = rbind(MCHoldOut1, HoldOut(M1))
}

## verjetnost prekomernega prileganja izračunam kot št. poskusov, ko je bilo prekomerno prileganje / št. vseh poskusov
PBO1 = sum(MCHoldOut1[, 2])/nrow(MCHoldOut1)

#################
## holdout na zadnjih 5 letih matrike M
# 5 let je približno 1280 trgovlanih dni
M2 = M[2051:3330, ]
MCHoldOut2 = c()
for (i in 1:100000){
  if (i %in% seq(from = 1, to = 100000, by = 500)){
    print(i)
  }
  MCHoldOut2 = rbind(MCHoldOut2, HoldOut(M2))
}

## verjetnost prekomernega prileganja izračunam kot št. poskusov, ko je bilo prekomerno prileganje / št. vseh poskusov
PBO2 = sum(MCHoldOut2[, 2])/nrow(MCHoldOut2)

#################
## holdout na prvih 5 letih matrike M
# 5 let je približno 1280 trgovlanih dni
M3 = M[1:1280, ]
MCHoldOut3 = c()
for (i in 1:100000){
  if (i %in% seq(from = 1, to = 100000, by = 500)){
    print(i)
  }
  MCHoldOut3 = rbind(MCHoldOut3, HoldOut(M3))
}

## verjetnost prekomernega prileganja izračunam kot št. poskusov, ko je bilo prekomerno prileganje / št. vseh poskusov
PBO3 = sum(MCHoldOut3[, 2])/nrow(MCHoldOut3)


######################
# analiza hold out
PBO1
table(MCHoldOut1[,1])
colnames(M1)[as.numeric(names(which.max(table(MCHoldOut1[,1]))))]

PBO2
table(MCHoldOut2[,1])
colnames(M2)[as.numeric(names(which.max(table(MCHoldOut2[,1]))))]

PBO3
table(MCHoldOut3[,1])
colnames(M3)[as.numeric(names(which.max(table(MCHoldOut3[,1]))))]


### shranim MCHoldOut in PBO za vse tri poskuse
save(MCHoldOut1, PBO1, MCHoldOut2, PBO2, MCHoldOut3, PBO3, file = "./data/HoldOut.rda")