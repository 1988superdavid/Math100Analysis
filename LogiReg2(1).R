#This Rcode, used logistic regression to regress the final exam score
#againts several factors and to predict students final scores.
logidata=read.delim('Fall2012Logi2.txt',header=T)
## Remove NA###
for(i in 2:length(logidata)){
  RowNA=is.na(logidata[,i])
  logidata=logidata[!RowNA,]}

## Time^2 ###
logidata[,ncol(logidata)+1]=logidata$TotalTime^2
names(logidata)[ncol(logidata)]='TTimeSquare'

A=as.numeric(logidata$FinalScores>90)
B=as.numeric(logidata$FinalScores>80)
C=as.numeric(logidata$FinalScores>70)
D=as.numeric(logidata$FinalScores>60)
Fail=as.numeric(logidata$FinalScores<60)

traindata=as.data.frame(cbind(logidata$score,
logidata$SR,logidata$TotalTime,logidata$TTimeSquare))

names(traindata)=c('PlacementS','SR','TotalTime','TotalTimeSquare')
LogiRegA=glm(A~.,data=traindata,
             family=binomial)

LogiRegB=glm(B~.,data=traindata,
 family=binomial) 

LogiRegC=glm(C~.,data=traindata,
 family=binomial) 

LogiRegD=glm(D~.,data=traindata,
 family=binomial) 


PredA=round(fitted(LogiRegA))
PredB=round(fitted(LogiRegB))
PredC=round(fitted(LogiRegC))
PredD=round(fitted(LogiRegD))

Error3=c()
Error3[1]=mean(PredA!=A)
Error3[2]=mean(PredB!=B)
Error3[3]=mean(PredC!=C)
Error3[4]=mean(PredD!=D)

View(Error3)





