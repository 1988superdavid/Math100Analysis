require(e1071)

source('data_setup.R')

response <- 'FE'
#vars <- c('IA','BasePerc','Score')
vars <- c('IA','BasePerc')

sv.frame <- ia[ , vars]

sv.frame <- within(sv.frame, pf <- factor(ifelse(ia$FE >= 70, 'Pass', 'Fail')))

# Linear fitting parameters.  Reasonable defaults:
#   ratio = num.pass/num.fail
#   cost.base = 1
# This sets cost.pass*num.pass + cost.fail*num.fail == cost.base*num
num <- length(sv.frame$pf)
num.fail <- sum(sv.frame$pf == 'Fail')
num.pass <- sum(sv.frame$pf == 'Pass')
cost.base <- 1
ratio <- num.pass / num.fail
cost <- list('Pass'=cost.base * num / (num.pass + ratio*num.fail), 
             'Fail'=cost.base * ratio*num / (num.pass + ratio*num.fail))

# Fit svm model
#fit <- svm(pf ~ ., data=sv.frame, kernel='radial', gamma=0.2, cross=10, class.weights=cost)
fit <- svm(pf ~ ., data=sv.frame, kernel='linear', cross=10, class.weights=cost)

# Make a plot of the model
#plot(fit, data=sv.frame, formula=IA~BasePerc, grid=200, slice=list('Score'=mean(sv.frame$Score)))
plot(fit, data=sv.frame, formula=IA~BasePerc, grid=200)

# Tabulate actual labels vs. fitted labels
pred = predict(fit, sv.frame)
print(table(Actual=sv.frame$pf, Fitted=pred))

#Calculate balanced success rate
fsr <- sum(pred == 'Fail' & sv.frame$pf == 'Fail') / sum(sv.frame$pf == 'Fail')
ffr <- sum(pred == 'Fail' & sv.frame$pf == 'Fail') / sum(pred == 'Fail')
psr <- sum(pred == 'Pass' & sv.frame$pf == 'Pass') / sum(sv.frame$pf == 'Pass')
bsr <-mean(c(fsr, psr))
print(paste('Pass Success Rate:', psr))
print(paste('Fail success rate:', fsr))
print(paste('False postive rate:', 1-ffr))
print(paste('Balanced Success Rate:', bsr))

# Obtain feature weights
w = t(fit$coefs) %*% fit$SV

# =============================================================================
# PREDICTION FOR OTHER SEMESTERS
# =============================================================================

source('multiple_semesters.R')

Prediction <- function(semester)
{
  # Use the previously fit data to make a prediction on pass/fail
  semester.pred <- semester[ , c('NetID',vars, 'GoalCourse','FE')]
  semester.pred <- within(semester.pred, pf <- factor(ifelse(FE >= 70, 'Pass', 'Fail')))
  semester.pred <- semester.pred[complete.cases(semester.pred), ]
  semester.pred <- within(semester.pred, {
    predicted <- predict(fit, semester.pred)
    comparison <- factor(ifelse(pf=='Pass' & predicted=='Pass', 'Correct Pass',
                                ifelse(pf=='Pass' & predicted=='Fail', 'False Fail',
                                       ifelse(pf=='Fail' & predicted=='Pass', 'False Pass',
                                              ifelse(pf=='Fail' & predicted=='Fail', 'Correct Fail','Impossible')))))
  })
  print(table(Actual=semester.pred$pf, Fitted=semester.pred$predicted))
  return(semester.pred)
}

# -------------------------------------
# Spring 2012
# -------------------------------------

print('Spring 2012:')
s12.pred <- Prediction(s12)
p.predict.accuracy.s12 <- qplot(x=IA, y=BasePerc,
                                data=s12.pred[s12.pred$BasePerc <= 100, ],
                                color=comparison, geom='jitter')

# -------------------------------------
# Fall 2012 with more points
# -------------------------------------

print('Fall 2012:')
f12.pred <- Prediction(f12)
p.predict.accuracy.f12 <- qplot(x=IA, y=BasePerc,
                                data=f12.pred[f12.pred$BasePerc <= 100, ],
                                color=comparison, geom='jitter')

# -------------------------------------
# Spring 2013 I
# -------------------------------------

print('Spring 2013 I:')
s13a.pred <- Prediction(s13a)
p.predict.accuracy.s13a <- qplot(x=IA, y=BasePerc,
                                 data=s13a.pred[s13a.pred$BasePerc <= 100, ],
                                 color=comparison, geom='jitter')