rm(list=ls())
load("~/Desktop/Memoire actuariat/fretri2auto9605.rda")
fretri2auto9605
total.paid2<-fretri2auto9605[["total"]][["paid"]]
total.incur2<-fretri2auto9605[["total"]][["incur"]]
damage.paid2<-fretri2auto9605[["damage"]][["paid"]]
damage.incuré<-fretri2auto9605[["damage"]][["incur"]]
body.paid2<-fretri2auto9605[["body"]][["paid"]]
body.incur2<-fretri2auto9605[["body"]][["incur"]]
print(total.paid2)
print(total.incur2)
############################################################################################
f1<-c()
f2<-c()
for (j in 1:3){
  a1<-0
  b1<-0
  a2<-0
  b2<-0
  for (i in (5-j):(10-j)){
    a1<-a1+total.paid2[i,j+1]
    b1<-b1+total.paid2[i,j]
    a2<-a2+total.incur2[i,j+1]
    b2<-b2+total.incur2[i,j]
  }
  f1<-append(f1,(a1-total.paid2[4,(j+1)])/(b1-total.paid2[4,(j)]))
  f2<-append(f2,(a2-total.incur2[4,(j+1)])/(b2-total.incur2[4,(j+1)]))
}
###
for (j in 4:6){
  a<-(sum(total.paid2[1:(10-j),(j+1)])-total.paid2[4,(j+1)])/(sum(total.paid2[1:(10-j),j])-total.paid2[4,(j)])
  b<-(sum(total.incur2[1:(10-j),(j+1)])-total.incur2[4,(j+1)])/(sum(total.incur2[1:(10-j),j])-total.incur2[4,(j+1)])
  f1<-append(f1,a)
  f2<-append(f2,b)
}
for (j in 7:9){
  a<-(sum(total.paid2[1:(10-j),(j+1)]))/(sum(total.paid2[1:(10-j),j]))
  b<-(sum(total.incur2[1:(10-j),(j+1)]))/(sum(total.incur2[1:(10-j),j]))
  f1<-append(f1,a)
  f2<-append(f2,b)
}
f1<-append(f1,1)
f2<-append(f2,1)
############################################################################################
library(ChainLadder)
mack1<-MackChainLadder(total.paid2)
f11<-mack1[["f"]]
mack2<-MackChainLadder(total.incur2)
f22<-mack1[["f"]]
print(f11)
print(f1)
print(f22)
print(f2)
############################################################################################
mack<-MackChainLadder(total.paid2)
mack
UltCL<-c(56494,54045,34208,90964,38443,41486,47847,50355,58981,67382)
DevCL<-c(1.000,0.999,0.998,0.998,0.995,0.991,0.984,0.965,0.875,0.440)
IbnrCL<-c(0,40,78,168,199,360,764,1780,7354,37765)
############################################################################################
MCL<-MunichChainLadder(Paid = total.paid2, Incurred = total.incur2)
MCL
UltMCL<-MCL[["MCLPaid"]][,10]
DevMCL<-c()
IbnrMCL<-c()
for(i in 1:10){
  a<-total.paid2[i,10-i+1]
  b<-a/UltMCL[i]
  c<-UltMCL[i]-a
  DevMCL<-append(DevMCL,b)
  IbnrMCL<-append(IbnrMCL,c)
}
print(DevMCL)
print(IbnrMCL)
print(sum(IbnrMCL))
############################################################################################
for (i in 1:3){
  for (j in (3-i+1):1){
    total.paid2[i,j]<-total.paid2[i,j+1]/f1[j]
  }
}
print(total.paid2)
###
for (i in 1:3){
  for (j in (3-i+1):1){
    total.incur2[i,j]<-total.incur2[i,j+1]/f2[j]
  }
}
print(total.incur2)
############################################################################################
PIC <- PaidIncurredChain(total.paid2,total.incur2)
PIC
###
UltPIC<-PIC[["Ult.Loss.Origin"]]
IbnrPIC<-PIC[["Res.Origin"]]
print(UltPIC)
print(IbnrPIC)
###
DevPIC<-c(1.000)
for(i in 1:9){
  a<-total.paid2[i+1,10-i]
  b<-a/UltPIC[i]
  DevPIC<-append(DevPIC,b)
}
UltPIC<-c(total.paid2[1,10],UltPIC)
IbnrPIC<-c(0,IbnrPIC)
print(DevPIC)
print(UltPIC)
sum(UltPIC)
print(IbnrPIC)
sum(IbnrPIC)
############################################################################################
Ult<-data.frame('CL'=UltCL,'MCL'=UltMCL,'PIC'=UltPIC)
Ult<-t(Ult)
print(Ult)
xtable(Ult)
###
Dev<-data.frame('CL'=round(DevCL,3),'MCL'=round(DevMCL,3),'PIC'=round(DevPIC,3))
Dev<-t(Dev)
print(Dev)
xtable(Dev)
###
Ibnr<-data.frame('CL'=IbnrCL,'MCL'=IbnrMCL,'PIC'=IbnrPIC)
Ibnr<-t(Ibnr)
print(Ibnr)
xtable(Ibnr)
############################################################################################
calcul<-function(x){
  a<-c(1)
  for (j in 2:10){
    b<-x[j]
    for (k in a){
      b<-b/k
    }
    a<-append(a,b/total.paid2[j,11-j])
  }
  return(rev(a))
}
testCL<-calcul(UltCL)
testMCL<-calcul(UltMCL)
testPIC<-calcul(UltPIC)
test<-data.frame('CL'=round(testCL,3),'MCL'=round(testMCL,3),'PIC'=round(testPIC,3))
test<-t(test)
print(test)

