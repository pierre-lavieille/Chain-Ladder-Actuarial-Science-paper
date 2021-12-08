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
P<-total.paid
I<-total.incur
n<-10
fP<-c()
for(j in 1:(n-1)){
  a<-sum(P[1:(n-j),j+1])/sum(P[1:(n-j),j])
  fP<-append(fP,a)
}
print(fP)
###
fI<-c()
for(j in 1:(n-1)){
  a<-sum(I[1:(n-j),j+1])/sum(I[1:(n-j),j])
  fI<-append(fI,a)
}
print(fI)
############################################################################################
phiP<-c()
for(j in 1:(n-2)){
  a<-sum(P[1:(n-j),j]*((P[1:(n-j),j+1]/P[1:(n-j),j])-fP[j])*((P[1:(n-j),j+1]/P[1:(n-j),j])-fP[j]))
  a<-a/(n-j-1)
  phiP<-append(phiP,a)
}
phiP<-sqrt(phiP)
b<-min((phiP[n-2]^4)/(phiP[n-3]^2),phiP[n-2]^2,phiP[n-3]^2)
phiP<-append(phiP,sqrt(b))
print(phiP)
###
phiI<-c()
for(j in 1:(n-2)){
  a<-sum(I[1:(n-j),j]*((I[1:(n-j),j+1]/I[1:(n-j),j])-fI[j])*((I[1:(n-j),j+1]/I[1:(n-j),j])-fI[j]))
  a<-a/(n-j-1)
  phiI<-append(phiI,a)
}
phiI<-sqrt(phiI)
b<-min((phiI[n-2]^4)/(phiI[n-3]^2),phiI[n-2]^2,phiI[n-3]^2)
phiI<-append(phiI,sqrt(b))
print(phiI)

P1<-data.frame("j"=c(1,2,3,4,5,6,7,8,9),'f-j'=fP,'phi-j'=phiP)
P1<-(t(P1))
P2<-data.frame("j"=c(1,2,3,4,5,6,7,8,9),'f-j'=fI,'phi-j'=phiI)
P2<-(t(P2))
############################################################################################
Q<-I/P
print(Q)
fQ<-c()
for(j in 1:(n)){
  a<-sum(I[1:(n-j+1),j])/sum(P[1:(n-j+1),j])
  fQ<-append(fQ,a)
}
print(fQ)
###
phiQP<-c()
for(j in 1:(n-1)){
  a<-sum(P[1:(n-j+1),j]*(Q[1:(n-j+1),j]-fQ[j])*(Q[1:(n-j+1),j]-fQ[j]))
  a<-a/(n-j)
  phiQP<-append(phiQP,a)
}
phiQP<-sqrt(phiQP)
print(phiQP)
###
resQ<-Q
print(resQ)
for (i in 1:n){
  for (j in 1:(n-i+1)){
    resQ[i,j]<-((Q[i,j]-fQ[j])*sqrt(P[i,j]))/phiQP[j]
  }
}
print(resQ)
############################################################################################
P2<-matrix(nrow=(n-1),ncol=(n-1))
for (i in 1:(n-1)){
  for (j in 1:(n-1)){
    P2[i,j]<-P[i,j+1]/P[i,j]
  }
}
print(P2)
###
resP<-matrix(nrow=(n-1),ncol=(n-1))
for (i in 1:(n-1)){
  for (j in 1:(n-1)){
    resP[i,j]<-(((P[i,j+1]/P[i,j])-fP[j])*sqrt(P[i,j]))/phiP[j]
  }
}
print(resP)
############################################################################################
a<-0
b<-0
for (i in 1:(n-1)){
  for (j in 1:(n-i)){
    a<-a+(resP[i,j]*resQ[i,j])
    b<-b+(resQ[i,j]*resQ[i,j])
  }
}
lambdaP<-a/b
############################################################################################
x<-seq(-2,2,0.1)
y<-lambdaP*x
plot(resQ[1:(n-1),1:(n-1)],resP)
lines(x, y, col="red")
############################################################################################
phiQI<-c()
for(j in 1:(n-1)){
  a<-sum(I[1:(n-j+1),j]*(1/Q[1:(n-j+1),j]-1/fQ[j])*(1/Q[1:(n-j+1),j]-1/fQ[j]))
  a<-a/(n-j)
  phiQI<-append(phiQI,a)
}
phiQI<-sqrt(phiQI)
print(phiQI)
############################################################################################
I2<-matrix(nrow=(n-1),ncol=(n-1))
for (i in 1:(n-1)){
  for (j in 1:(n-1)){
    I2[i,j]<-I[i,j+1]/I[i,j]
  }
}
print(I2)
###
resI<-matrix(nrow=(n-1),ncol=(n-1))
for (i in 1:(n-1)){
  for (j in 1:(n-1)){
    resI[i,j]<-(((I[i,j+1]/I[i,j])-fI[j])*sqrt(I[i,j]))/phiI[j]
  }
}
print(resI)
###
resQ2<-Q
for (i in 1:n){
  for (j in 1:(n-i+1)){
    resQ2[i,j]<-((1/Q[i,j]-1/fQ[j])*sqrt(I[i,j]))/phiQI[j]
  }
}
print(resQ2)
############################################################################################
a2<-0
b2<-0
for (i in 1:(n-1)){
  for (j in 1:(n-i)){
    a2<-a2+(resI[i,j]*resQ2[i,j])
    b2<-b2+(resQ2[i,j]*resQ2[i,j])
  }
}
lambdaI<-a2/b2
print(lambdaI)
############################################################################################
UP<-P
UI<-I
DP<-matrix(nrow=(n),ncol=(n-1))
DI<-matrix(nrow=(n),ncol=(n-1))
for (k in (n+1):(2*n-1)){
  for (i in 2:n){
    for (j in (1:(n-1))){
      if ((i+j)==k){
        DP[i,j]<-fP[j]+lambdaP*phiP[j]*((UI[i,j]/UP[i,j])-fQ[j])/phiQP[j]
        UP[i,(j+1)]<-UP[i,j]*DP[i,j]
        DI[i,j]<-fI[j]+lambdaI*phiI[j]*((UP[i,j]/UI[i,j])-1/fQ[j])/phiQI[j]
        UI[i,(j+1)]<-UI[i,j]*DI[i,j]
      }
    }
  }
}
print(DP)
print(UP)
print(DI)
print(UI)
############################################################################################
MCL<-MunichChainLadder(Paid = total.paid, Incurred = total.incur)
summary(MCL)
############################################################################################
mack<-MackChainLadder(total.paid)
summary(mack)
sum(mack[["Mack.S.E"]][,10])
############################################################################################
IbnrP=c()
for (i in 1:n){
  a<-UP[i,n]-UP[i,n-i+1]
  IbnrP<-append(IbnrP,a)
}
print(IbnrP)
###
IbnrI=c()
for (i in 1:n){
  a<-UI[i,n]-UI[i,n-i+1]
  IbnrI<-append(IbnrI,a)
}
print(IbnrI)
###
DevP<-c()
for (i in 1:n){
  a<-UP[i,n-i+1]/UP[i,n]
  DevP<-append(DevP,a)
}
print(DevP)
###
DevI<-c()
for (i in 1:n){
  a<-UI[i,n-i+1]/UI[i,n]
  DevI<-append(DevI,a)
}
print(DevI)
###
sum(IbnrP)




