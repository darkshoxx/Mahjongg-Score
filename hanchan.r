## Hanchan Scoresheet Calculator
# Regular command is han(c(p1,p2,p3,p4)) where pi is the points of player i or han(P) where P is a 4-dimensional array or vector containing the points
# Credits must be preadded to the points, i.e. P can have negative entries, but must sum up to 4x[Starting Points]
# With names, call han(P,c("N1","N2","N3","N4")) where "Ni" is the name of player i or han(P,N), where N is a 4-dim. array containing Names.
# Chombo must be entered as a vector C in multiples of -20000
# UMA is currently fixed to 15,5

Names=c("East","South","West","North")
Start=30000
Chombo=c(0,0,0,0) #Chombo points should be entered negative, i.e. c(-20000,0,0,0)
han=function(Points,N=Names,S=rep(Start,4),C=Chombo){
if(abs(sum(Points)-sum(S))>1e-5){print("Points do not add up!");break()}
if(sum(abs(C/20000-round(C/20000)))>1e-5){print("Incorrect Chombo Input (not a multiple of 20k)");break()}
if(sum(abs(C)+C)>1e-5){print("Incorrect Chombo Sign");break()}
RESULT=matrix(nrow=7,ncol=4)
print(N)
print(RESULT)
colnames(RESULT)<-N
rownames(RESULT)<-c("Points","Starting Points","Zwischensumme","Rang","UMA","Chombo","Gesamtsumme")
RESULT[1,]<-Points
RESULT[2,]<- -S
RESULT[3,]<- RESULT[1,]+RESULT[2,]
RESULT[4,]<- 5-rank(RESULT[3,])
RESULT[5,]<-(RESULT[4,]*10000-25000)*-1
RESULT[6,]<-C
RESULT[7,]<-RESULT[3,]+RESULT[5,]+RESULT[6,]
return(RESULT)
}
han(c(20300,41400,24400,33900),C=c(-40000,0,0,0))