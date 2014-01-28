## Metoda kombinatoričnega simetričnega preverjanja

#### VHODNI PODATKI: M, S, datoteka
## M = matrika dobička oz. izgube
## S = število podmatrik, na katere razdelimo M
## datoteka = ime datoteke, v katero shranim vse zgenerirane podatke

# S mora biti sodo število
# število vrstic v M mora biti deljivo s S

#### IZHODNI PODATKI: 
## verjetnost prekomernega prileganja

## KNJIŽNICE, KI JIH POTREBUJEM:
library(gdata)
library(tseries)

## FUNKCIJA, KI IZRAČUNA VERJETNOST PREKOMERNEGA PRILEGANJA
CSCV = function(M, S, datoteka = ""){
  ## 1. matriko M razdelim na S podmatrik dimenzij T/S x N
  ## funkcija, ki pove, katere vrstice matrike M pripadajo podmatriki Ms, s = 1, ..., S
  podmatrika = function(M, S, s){
    st.vrstic = nrow(M)/S
    a = st.vrstic * (s-1) + 1
    b = st.vrstic * s
    a : b
  }
  
  
  ## 2. vse možne kombinacije podmatrik matrike M, kjer izberemo S/2 podmatrik:
  print("Določam kombinacije Cs")
  ## funkcija, ki vrne matriko kjer je v vsakem stolpcu ena možna kombinacija podmatrik
  ## vrednosti v matriki so zaporedne številke podmatrik Ms, oz. s-ji
  kombinacije = function(S) {
    combn(S, S/2)
  }
  Cs = kombinacije(S)
  
  
  ## 3. za vsako kombinacijo c V Cs:
  ## a.) naredimo matriko J iz podmatrik, ki so v c
  ## funkcija, ki vrne katere vrstice iz M so v J
  trainingSet = function(M, c){
    vrstice = unmatrix(sapply(c, podmatrika, M=M, S=length(c)*2))
    names(vrstice) = NULL  
    vrstice
  }
  
  ## za vsak stolpec v Cs določim katere vrstice v matriki M so v J pri dani kombinaciji z:
  # apply(Cs, MARGIN = 2, FUN = trainingSet, M = M)
  ## dobim T/2 x št. kombinacij matriko
  ## v vsakem stolpcu je vektor, ki pove katere vrstice v matriki M so v J pri dani kombinaciji
  
  
  ## b.) preostale podmatrike, ki niso v c, dam v matriko Jbar
  ## funkcija, ki vrne katere vrstice iz M niso v J:
  testSet = function(M, c) {
    setdiff(1:nrow(M), trainingSet(M, c))
  }
  
  ## za vsak stolpec v Cs določim katere vrstice matrike M niso v J oz. so v Jbar pri dani kombinaciji z:
  # apply(Cs, MARGIN = 2, FUN = testSet, M = M)
  ## dobim T/2 x št. kombinacij matriko
  ## v vsakem stolpcu je vektor, ki pove katere vrstice matrike M niso v J oz. so v Jbar pri dani kombinaciji
  
  
  ## c.) za vsak stolpec v J (strategijo) izračunam "performance statistics" -- vektor R
  print("Določam performance statistics za J")
  ## funkcija, ki izračuna sharpe ratio na vsakem stolpcu dane matrike
  SR = function(podmatrika){
    apply(M[podmatrika, ], 2, function(x) mean(x)/sd(x))
  }
  
  # za vsako možno kombinacijo izračunam SR za vsako strategijo
  Rji = apply(apply(Cs, MARGIN = 2, FUN = trainingSet, M = M), 2, SR)
  # dobim N x št. kombinacij matriko - v vsakem solpcu je R za dano kombinacijo
  
  
  ## d.) določim najboljšo strategijo za dan J (največji SR)
  print("Določam najboljšo strategijo v J")
  ## za vsak vektor R določim kateri element v R ima največjo vrednost - katera strategija je najboljša IS
  ## za vsak stolpec v Rji določim kateri je max, dobim vektor maksimumov
  nji = apply(Rji, 2, which.max)
  
  
  ## e.) za vsak stolpec v Jbar izračunam "performance statistics" -- vektor Rbar
  print("Določam performance statistics za Jbar")
  # za vsako možno kombinacijo izračunam SR za vsako strategijo
  Rbars = apply(apply(Cs, MARGIN = 2, FUN = testSet, M = M), 2, SR)
  # dobim N x št. kombinacij matriko - v vsakem solpcu je Rbar za eno kombinacijo
  
  
  ## f.) določim relativni rank strategije n (najboljše strategije v J) v Rbar:
  print("Določam relativni rank za n v Rbar")
  # rank(Rbar) -> najmanjše število v Rbar ima najmanjši rank
  # omega = rank(Rbar)[n]/length(Rbar)
  ## za vsak stolpec v Rbars (vsako kombinacijo) določim omego:
  omega = c()
#   for(i in 1:ncol(Rbars)){
#     if (i %in% seq(from=1, to = ncol(Rbars), by = 200)) {
#       print(i)
#     }
#     w = rank(Rbars[ , i])[nji[i]]/length(Rbars[ , i])
#     omega = c(omega, w)
#   }
  
  w1 = function(i){
#     if (i %in% seq(from=1, to = ncol(Rbars), by = 500)) {
#       print(i)
#     }
    return(rank(Rbars[ , i])[nji[i]]/length(Rbars[ , i]))
  }
  omega = sapply(1:ncol(Rbars), w1)

  # omega je vektor dolžine št. kombinacij
  # vsak element je relativni rank strategije n v Rbar za eno kombinacijo
  
  
  ## g.) definiram logit:
  L = function(omega){
    log(omega/(1-omega))
  }
  
  print("Določam logit")
  
  # logit določim za vsako kombinacijo
  lambda = sapply(omega, L)
  
  
  ## 4. določim porazdelitev logitov:
  print("Določam porazdelitev logitov")
  ## za vsak c v Cs imam eno logit -- vektor lambda, 
  ## izračunam relativno frekvenco za vsako lambdo:
  f = table(lambda)/length(lambda) # relativna frekvenca za vsako različno lambdo
  sum(f) # morajo se sešteti v 1
  

  ## 5. izračunam PBO
  print("Računam PBO")
  ## PBO = probabitily of backtest overfit
  ## PBO lahko ocenim s tistimi lambdami, ki so negativne:
  negativne = names(f) < 0
  fi = sum(f[negativne])
  ## fi = odstotek v katerem je optimalna strategija IS slabša od mediane OOS

  ## SHRANIM VSE ZGENERIRANE SPREMENLJIVKE, če je podana datoteka:
  if (datoteka != "") {
    print("Shranjujem M, S, Rji, nji, Rbars, omega, lambda, f, fi")
    save(M, S, Rji, nji, Rbars, omega, lambda, f, fi, file = datoteka)
  }
  
  ## vrne PBO
  return(fi)
}