#Do regression of FinalExam ALEK raw score against several factors, and use 
#the predicted value to classify students into A,B,C,D
#compare the A,B,C,D with students' actual performance.

#This could be updated by more regorous multi-class classfying techniques #

logidata=read.delim('Fall2012Logi2.txt',header=T)

## Remove NA###
for(i in 2:length(logidata)){
  RowNA=is.na(logidata[,i])
  logidata=logidata[!RowNA,]}

## Time^2 ###
logidata[,ncol(logidata)+1]=logidata$TotalTime^2
names(logidata)[ncol(logidata)]='TTimeSquare'

LinearReg=lm(FinalScores~score+SR+TotalTime+TTimeSquare,data=logidata)

Pred=fitted(LinearReg)
PredTextGrade=as.numeric(Pred>90)
PredTextGrade=cbind(PredTextGrade,as.numeric(Pred>80))
PredTextGrade=cbind(PredTextGrade,as.numeric(Pred>70))
PredTextGrade=cbind(PredTextGrade,as.numeric(Pred>60))

Error3=c()
Error3[1]=mean(TextGrade[,1]!=PredTextGrade[,1])
Error3[2]=mean(TextGrade[,2]!=PredTextGrade[,2])
Error3[3]=mean(TextGrade[,3]!=PredTextGrade[,3])
Error3[4]=mean(TextGrade[,4]!=PredTextGrade[,4])

PredYes=c()
PredYes[1]=sum(PredTextGrade[,1])
PredYes[2]=sum(PredTextGrade[,2])
PredYes[3]=sum(PredTextGrade[,3])
PredYes[4]=sum(PredTextGrade[,4])

Yes=c()
Yes[1]=sum(TextGrade[,1])
Yes[2]=sum(TextGrade[,2])
Yes[3]=sum(TextGrade[,3])
Yes[4]=sum(TextGrade[,4])

Error=c()
Error[1]=abs(PredYes[1]-Yes[1])/nrow(PredTextGrade)
Error[2]=abs(PredYes[2]-Yes[2])/nrow(PredTextGrade)
Error[3]=abs(PredYes[3]-Yes[3])/nrow(PredTextGrade)
Error[4]=abs(PredYes[4]-Yes[4])/nrow(PredTextGrade)

Error3=cbind(Error3,PredYes,Yes,Error)
View(Error3)
