load("C:\\Users\\Daniel1\\Desktop\\Mahjong\\4nin.rdata")
load("C:\\Users\\Daniel1\\Desktop\\Mahjong\\zombie.rdata")

#zombie=matrix(NA,0,9)
#yonin=matrix(NA,0,9)
#colnames(zombie)<-c("id","day","month","year","Daniel","Andreas","Theresa","Tobi","Zombie")
#colnames(yonin)<-c("id","day","month","year","Daniel","Andreas","Theresa","Tobi","NN7")


add=function(typ,d,m,y,scores,test=1,Z=zombie,Y=yonin){
if(typ==3){mat=Z}else{if(typ==4){mat=Y}else{return("ERROR");break()}}
if(is.na(mat[1])){i=1}else{i=max(mat[,1]+1)}
NEW=rbind(mat,c(i,d,m,y,scores))
tsum=sum(NEW[i,5:ncol(NEW)])
if(abs(tsum)>0.01){print("ERRORSUM");break()}else{print(tsum)}
if(test==1){print(NEW);print("THIS WAS A TEST")}else{if(typ==3){zombie<<-NEW}else{yonin<<-NEW};print(NEW)}
}


point=function(mat){
co=ncol(mat)
np=co-4
M=colSums(mat[,5:co])
return(M)
}
point(zombie)

addplayer=function(Mat,Name){
MAT=cbind(Mat,0)
colnames(MAT)=c(colnames(Mat),Name)
return(MAT)
}

resortrelabel=function(M){
M=M[order(M[,1]),]
M[,1]<-1:nrow(M)

return(M)
}
#zombie=resortrelabel(zombie)

 #add(nplay,day,mon,year,c(15.3,28.2,2.7,0,-46.2),0)
add(3,8,10,18,c(0,0,0,0,0),1)

save(yonin,file="C:\\Users\\Daniel1\\Desktop\\Mahjong\\4nin.rdata")
save(zombie,file="C:\\Users\\Daniel1\\Desktop\\Mahjong\\zombie.rdata")