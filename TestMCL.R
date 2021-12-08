rm(list=ls())
a1<-c(576,1804,1970,2024,2074,2102,2131)
a2<-c(866,1948,2162,2232,2284,2348,NA)
a3<-c(1412,3758,4252,4416,4494,NA,NA)
a4<-c(2286,5292,5724,5850,NA,NA,NA)
a5<-c(1868,3778,4648,NA,NA,NA,NA)
a6<-c(1442,4010,NA,NA,NA,NA,NA)
a7<-c(2044,NA,NA,NA,NA,NA,NA) 
P<-matrix (c(a1,a2,a3,a4,a5,a6,a7),7,7)
P<-t(P)
print(P)
############################################################################################
a1<-c(978,2104,2134,2144,2174,2182,2174)
a2<-c(1844,2552,2466,2480,2508,2454,NA)
a3<-c(2904,4354,4698,4600,4644,NA,NA)
a4<-c(3502,5958,6070,6142,NA,NA,NA)
a5<-c(2812,4882,4852,NA,NA,NA,NA)
a6<-c(2642,4406,NA,NA,NA,NA,NA)
a7<-c(5022,NA,NA,NA,NA,NA,NA) 
I<-matrix (c(a1,a2,a3,a4,a5,a6,a7),7,7)
I<-t(I)
print(I)
############################################################################################
n=7
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












