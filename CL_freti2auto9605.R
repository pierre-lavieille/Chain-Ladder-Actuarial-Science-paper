rm(list=ls())
load("~/Desktop/Memoire actuariat/fretri2auto9605.rda")
fretri2auto9605
total.paid2<-fretri2auto9605[["total"]][["paid"]]
total.incur2<-fretri2auto9605[["total"]][["incur"]]
damage.paid2<-fretri2auto9605[["damage"]][["paid"]]
damage.incur2<-fretri2auto9605[["damage"]][["incur"]]
body.paid2<-fretri2auto9605[["body"]][["paid"]]
body.incur2<-fretri2auto9605[["body"]][["incur"]]
print(body.paid2)
print(damage.incur2)
############################################################################################
library(ChainLadder)
mack<-MackChainLadder(total.paid2)
mack
plot(mack)
############################################################################################
MCL<-MunichChainLadder(Paid = total.paid2, Incurred = total.incur2)
MCL
plot(MCL)
############################################################################################
PIC <- PaidIncurredChain(total.paid2,total.incur2)
PIC
