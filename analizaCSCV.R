# analiziram podatke, ki sem jih dobila v uporabaCSCV

setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

########################################
### PODATKI, KI SEM JIH DOBILA S TO KODO:
load("./data/analiza.rda")
#######################################

################
load("./data/CSCV.rda")
M1 = M
T1 = nrow(M1)
S1 = S
od1 = rownames(M1)[1]
do1 = rownames(M1)[T1]
Rji1 = Rji
nji1 = nji
Rbars1 = Rbars
omega1 = omega
lambda1 = lambda
f1 = f
PBO1 = fi

load("./data/CSCV5let.rda")
M2 = M
T2 = nrow(M2)
S2 = S
od2 = rownames(M2)[1]
do2 = rownames(M2)[T2]
Rji2 = Rji
nji2 = nji
Rbars2 = Rbars
omega2 = omega
lambda2 = lambda
f2 = f
PBO2 = fi

load("./data/CSCVprvih5let.rda")
M3 = M
T3 = nrow(M3)
S3 = S
od3 = rownames(M3)[1]
do3 = rownames(M3)[T3]
Rji3 = Rji
nji3 = nji
Rbars3 = Rbars
omega3 = omega
lambda3 = lambda
f3 = f
PBO3 = fi

load("./data/CSCVS10.rda")
M4 = M
T4 = nrow(M4)
S4 = S
od4 = rownames(M4)[1]
do4 = rownames(M4)[T4]
Rji4 = Rji
nji4 = nji
Rbars4 = Rbars
omega4 = omega
lambda4 = lambda
f4 = f
PBO4 = fi

load("./data/CSCV5letS10.rda")
M5 = M
T5 = nrow(M1)
S5 = S
od5 = rownames(M5)[1]
do5 = rownames(M5)[T5]
Rji5 = Rji
nji5 = nji
Rbars5 = Rbars
omega5 = omega
lambda5 = lambda
f5 = f
PBO5 = fi

load("./data/CSCVprvih5letS10.rda")
M6 = M
T6 = nrow(M6)
S6 = S
od6 = rownames(M6)[1]
do6 = rownames(M6)[T6]
Rji2 = Rji
nji6 = nji
Rbars6 = Rbars
omega6 = omega
lambda6 = lambda
f6 = f
PBO6 = fi


## imam N = 9 strategij, za vsako imam T donosov
## Matriko M razdelim na S podmatrik
## število kombinacij: choose(S, S/2)

# za vsako kombinacijo določim katera strategija ima najboljši performance (največji sharpe ratio)

T1
od1
do1
S1
choose(S1, S1/2)
table(nji1)
colnames(M1)[as.numeric(names(which.max(table(nji1))))]
PBO1

T2
od2
do2
S2
choose(S2, S2/2)
table(nji2)
colnames(M2)[as.numeric(names(which.max(table(nji2))))]
PBO2

T3
od3
do3
S3
choose(S3, S3/2)
table(nji3)
colnames(M3)[as.numeric(names(which.max(table(nji3))))]
PBO3

T4
od4
do4
S4
choose(S4, S4/2)
table(nji4)
colnames(M4)[as.numeric(names(which.max(table(nji4))))]
PBO4

T5
od5
do5
S5
choose(S5, S5/2)
table(nji5)
colnames(M5)[as.numeric(names(which.max(table(nji5))))]
PBO5

T6
od6
do6
S6
choose(S6, S6/2)
table(nji6)
colnames(M6)[as.numeric(names(which.max(table(nji6))))]
PBO6

# shranim podatke analize
save(M1, T1, od1, do1, S1, nji1, PBO1,
     M2, T2, od2, do2, S2, nji2, PBO2,
     M3, T3, od3, do3, S3, nji3, PBO3, 
     M4, T4, od4, do4, S4, nji4, PBO4, 
     M5, T5, od5, do5, S5, nji5, PBO5,
     M6, T6, od6, do6, S6, nji6, PBO6, file = "./data/analiza.rda")

##############
# sharpe za vsako strategijo na vseh podatkih
load("./data/M.rda")
Msharpe = apply(M, 2, function(x) mean(x)/sd(x))
Msharpe
which.max(Msharpe)