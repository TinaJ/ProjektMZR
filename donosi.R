setwd("C:/Users/Tina/Documents/faks/2. letnik magisterija/matematika z računalnikom/ProjektMZR")

load("./data/Mdonos.rda")
load("./data/M_SP_donos.rda")

# narišem donose za SP in za portfelj SP papirjev za vse strategije
par(mfrow=c(3,3))
plot(Mdonos[,1], type="l",col="red", main = colnames(Mdonos)[1])
lines(M_SP_donos[,1],col="green")

plot(Mdonos[,2], type="l",col="red", main = colnames(Mdonos)[2])
lines(M_SP_donos[,2],col="green")

plot(Mdonos[,3], type="l",col="red", main = colnames(Mdonos)[3])
lines(M_SP_donos[,3],col="green")

plot(Mdonos[,4], type="l",col="red", main = colnames(Mdonos)[4])
lines(M_SP_donos[,4],col="green")

plot(Mdonos[,5], type="l",col="red", main = colnames(Mdonos)[5])
lines(M_SP_donos[,5],col="green")

plot(Mdonos[,6], type="l",col="red", main = colnames(Mdonos)[6])
lines(M_SP_donos[,6],col="green")

plot(Mdonos[,7], type="l",col="red", main = colnames(Mdonos)[7])
lines(M_SP_donos[,7],col="green")

plot(Mdonos[,8], type="l",col="red", main = colnames(Mdonos)[8])
lines(M_SP_donos[,8],col="green")

plot(Mdonos[,9], type="l",col="red", main = colnames(Mdonos)[9])
lines(M_SP_donos[,9],col="green")

