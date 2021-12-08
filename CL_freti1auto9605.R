rm(list=ls())
load("~/Desktop/Memoire actuariat/fretri1auto9605.rda")
fretri1auto9605
total.paid<-fretri1auto9605[["total"]][["paid"]]
total.incur<-fretri1auto9605[["total"]][["incur"]]
damage.paid<-fretri1auto9605[["damage"]][["paid"]]
damage.incur<-fretri1auto9605[["damage"]][["incur"]]
body.paid<-fretri1auto9605[["body"]][["paid"]]
body.incur<-fretri1auto9605[["body"]][["incur"]]
print(total.paid)
print(total.incur)
############################################################################################
library(ChainLadder)
mack<-MackChainLadder(total.paid)
mack
plot(mack, which=5)
############################################################################################
t<-mack[["FullTriangle"]]
tt<-c()
for(i in 1:10){
  a<-t[i,10]/t[i,11-i]
  tt<-append(a,tt)
}
print(round(tt,3))
############################################################################################
MCL<-MunichChainLadder(Paid = total.paid, Incurred = total.incur)
summary(MCL)
plot(MCL)
############################################################################################
m<-MCL[["MCLPaid"]]
print(m)
mm<-matrix(nrow=9,ncol=9)
for (i in 1:(10-1)){
  for (j in 1:(10-1)){
    mm[i,j]<-m[i,j+1]/m[i,j]
  }
}
mm<-round(mm,3)
print(mm)
############################################################################################
PIC <- PaidIncurredChain(total.paid,total.incur)
PIC
############################################################################################
x<-total.paid[1:4,6]
y<-total.paid[1:4,7]
plot(x,y)
############################################################################################