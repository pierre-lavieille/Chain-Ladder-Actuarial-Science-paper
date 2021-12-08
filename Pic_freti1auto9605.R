rm(list=ls())
load("~/Desktop/Memoire actuariat/fretri1auto9605.rda")
total.paid<-fretri1auto9605[["total"]][["paid"]]
total.incur<-fretri1auto9605[["total"]][["incur"]]
print(total.paid)
print(total.incur)
############################################################################################
library(ChainLadder)
mack1<-MackChainLadder(total.paid)
f1<-mack1[["f"]]
mack2<-MackChainLadder(total.incur)
f2<-mack1[["f"]]
print(f1)
print(f2)
############################################################################################
for (i in 1:3){
  for (j in (3-i+1):1){
    total.paid[i,j]<-total.paid[i,j+1]/f1[j]
  }
}
print(total.paid)
###
for (i in 1:3){
  for (j in (3-i+1):1){
    total.incur[i,j]<-total.incur[i,j+1]/f2[j]
  }
}
print(total.incur)
############################################################################################
PIC <- PaidIncurredChain(total.paid,total.incur)
print(total.paid)
PIC
############################################################################################
Ult<-PIC[["Ult.Loss.Origin"]]
Res<-PIC[["Res.Origin"]]
print(Ult)
print(Res)
############################################################################################
Pour<-c(100.00)
for(i in 1:9){
  a<-total.paid[i+1,10-i]
  b<-a/Ult[i]
  Pour<-append(Pour,b*100)
}
Ult<-c(total.paid[1,10],Ult)
Res<-c(0,Res)
print(Pour)
print(Ult)
sum(Ult)
print(Res)
############################################################################################
c<-data.frame("Année"=1996:2005,'Ultime'=Ult,'Developement'=Pour,'Provision'=Res)
c<-round(c,2)
print(t(c))
c<-t(c)
c
############################################################################################
library(xtable)
xtable(c)



