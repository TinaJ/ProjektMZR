# za vsako strategijo pogledam rank IS in rank OOS, pogledam kakšna je razlika med njima
# matriko razdelim na 2 dela (naključno)
setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")
load("./data/dnevniDonosiPortfelja.rda")

M = dnevniDonosiPortfelja

overfit = function(M) {
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
  # višji SR, boljše je
  
  # rangiram strategije glede na SR IS in OOS:
  rankIS = rank(sharpeIS)
  rankOOS = rank(sharpeOOS)
  # višji rank ima, boljše je
  
  return(rankIS - rankOOS)
  
}

# ponovim zgornjo funkcijo 100x in  združim razlike v eno matriko:
razlike = c()
for (i in 1:10000){
  razlike = cbind(razlike, overfit(M))
}

# če je razlika > 0, je overfit, če je razlika < 0 je underfit
overfitano = apply(razlike, 1, function(x) sum(x>0))/10000 # v koliko primerih je rankIS > rankOOS
underfitano = apply(razlike, 1, function(x) sum(x<0))/10000 # v koliko primerih je rankOOS > rankIS
overfitano2 = apply(razlike, 1, function(x) sum(x>=2))/10000 # v koliko primerih je rankIS > rankOOS
underfitano2 = apply(razlike, 1, function(x) sum(x<=-2))/10000 # v koliko primerih je rankOOS > rankIS
overfitano3 = apply(razlike, 1, function(x) sum(x>=3))/10000 # v koliko primerih je rankIS > rankOOS
underfitano3 = apply(razlike, 1, function(x) sum(x<=-3))/10000 # v koliko primerih je rankOOS > rankIS
enako = apply(razlike, 1, function(x) sum(x==0))/10000

overfitting = cbind(overfitano, underfitano, overfitano2, underfitano2, overfitano3, underfitano3, enako)
apply(overfitting, 2, rank)