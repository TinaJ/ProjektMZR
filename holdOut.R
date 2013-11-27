## hold-out metoda:
## vrstice matrike M razdelim na 2 dela (naključno)
## izračunam sharpe ratio na prvem delu, določim strategijo, ki ima največji SR -- n
## izračunam SR na drugem delu in pogledam, če je SR strategije n < mediane SR

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")
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

## ponovim hold out metodo 1000x, zlagam n-je in overfit po vrsticah v matriko
MCHoldOut = c()
for (i in 1:1000000){
  if (i %in% seq(from = 1, to = 1000000, by = 1000)){
    print(i)
  }
  MCHoldOut = rbind(MCHoldOut, HoldOut(M))
}

## verjetnost prekomernega prileganja izračunam kot št. poskusov, ko je bilo prekomerno prileganje / št. vseh poskusov
PBO = sum(MCHoldOut[, 2])/nrow(MCHoldOut)
PBO

