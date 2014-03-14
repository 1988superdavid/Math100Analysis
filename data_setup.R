require(plyr)
require(car)
require(reshape2)
require(ggplot2)

ia <- read.delim('SVMBasicTopicsIA',header=T)

# Remove students who didn't take placement exam, have base percentages too high
# or scored too high on the placement exam
ia <- ia[!is.na(ia$Placement) & ia$Placement!=5, ]
ia <- ia[0 <= ia$BasePerc & ia$BasePerc <= 100, ]
ia <- ia[ia$Score < 60, ]

ia <- within(ia,
{
  Placement <- factor(Placement)
  CourseGoal <- factor(CourseGoal)
  CalorAlg <- factor(ifelse(Placement %in% c(1,2), 'Algebra', 'Calculus'))
  TTimeSquare <- STTime^2
})

placement.names <- list('Algebra, proctored'=1,
                        'Algebra, non-proctored'=2,
                        'Calculus, proctored'=3,
                        'Calculus, non-proctored'=4,
                        'Retake Math 100'=5)
levels(ia$Placement) <- placement.names