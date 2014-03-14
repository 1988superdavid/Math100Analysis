lateadd=SRdata[,2]
RegStaRaw=SRdata[SRdata[,2]==0,]
RegSta=RegStaRaw[,c(1,4,5,6,7,9)]

####Remove NA from the RegSta Data####

for(i in 2:length(RegSta)){
  RowNA=is.na(RegSta[,i])
  RegSta=RegSta[!RowNA,]}

###Take a look at students whose missed 
###>=3 and Total Time<3.5
MHW3=RegSta[RegSta[,5]>=3,]
Subset=MHW3[MHW3[,6]<=2,]

Pass=as.numeric(RegSta[,2]>=60)


constants=matrix(rep(1,nrow(RegSta),nrow(RegSta),1))
Xs=cbind(constants,RegSta[c(3,4,5,6)])

#### We can use class(Xs) to check its data type###
Xs=data.matrix(Xs) ###Convert from dataframe
                      ### to numeric matrix

#BetaOLS=solve(t(Xs)%*%Xs) %*% t(Xs)%*%Pass

Passdata=cbind(RegSta,Pass)

Passglm <- glm(Pass ~ Pperce+absences+missedHW+TotalTime
    , data = Passdata)
## Because Binary Classification. The cost fun is
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
#CV10=cv.glm(Passdata, Passglm, cost, 10)
(CV10 <- cv.glm(Passdata, Passglm,cost,10)$delta)


reg1=lm(RegSta[,2]~RegSta[,3]+RegSta[,4]+RegSta[,5]
      +RegSta[,6])
Pred=fitted(reg1)
PredFail=Pred[c(Pred<70)]
TFail=sum(as.numeric(c(Pred<70)))
TFail

reg2=lm(RegSta[,2]~RegSta[,4]+RegSta[,5]
        +RegSta[,6])
Pred=fitted(reg2)
PredFail=Pred[c(Pred<70)]
TFail=sum(as.numeric(c(Pred<70)))


### Regresssion with MissedHW, Absences, PA, Total time
Regf=glm(RegSta[, 2] ~ RegSta[, 3] + RegSta[, 4] 
         + RegSta[,5] + RegSta[, 6])
data2013full=read.delim('Testdata2013Full.txt')
data2013full=data2013full[,c(5,6,7,8)]

Pred2013full=predict(Regf,data2013full)
#nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
#(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
#(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)

TtimeExam=read.delim("TtimeExam.txt",header=T)
RegTtimeFin=lm(TtimeExam[,8]~TtimeExam[,15])
qqplot(TtimeExam[,15],TtimeExam[,8])
sub=c(1:300)
plot(TtimeExam[,15],TtimeExam[c(1:100),8])



TwoTtimes=read.delim("TwoTtimes.txt",header=T)

TwoTtimes=na.omit(TwoTtimes) # Eliminate NA
TwoTtimes=TwoTtimes[as.numeric(TwoTtimes[,2])
                    <as.numeric(TwoTtimes[,4]),]
TwoTtimes=na.omit(TwoTtimes) # Eliminate NA
tempT=(as.numeric(TwoTtimes[,4]))^2
RegTtime=lm(as.numeric(TwoTtimes[,3])~tempT+as.numeric(TwoTtimes[,4]))
RegTtime3=lm(as.numeric(TwoTtimes[,3])~as.numeric(TwoTtimes[,2]))
## Convert Final Score as scaled to 100##
FinalScores2=data2013$FinalScores/0.6
data2013=cbind(data2013,FinalScores2)
data2013=data2013[,c(1,2,3,4,6)]
A=as.numeric(data2013$FinalScores>90)
B=as.numeric(data2013$FinalScores>80)
C=as.numeric(data2013$FinalScores>70)
D=as.numeric(data2013$FinalScores>60)
data20132=data2013
data20132$TotalTime=data20132$TotalTime*60
logiA=glm(data20132$A~data20132$absences+data20132$missedHW+data20132$TotalTime,family=binomial)

datareg=data20132[,c(2,3,4)]
logiA2=glm(data20132$A~.,data=datareg,family=binomial)
tempA2=fitted(logiA2)

logiA3=glm(data20132$A~data20132$TotalTime,family=binomial)
tempA3=fitted(logiA3)

logiA4=glm(data20132$A~data20132$absences,family=binomial)
tempA4=fitted(logiA4)

Reg=lm(data20132$FinalScores~data20132$absences+data20132$missedHW
       +data20132$TotalTime)
Regpred=fitted(Reg)
RegpredA=Regpred>90

