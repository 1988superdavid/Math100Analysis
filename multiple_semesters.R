# This program uses SVM to seprate the students who passed and those who failed
# This is done for several different semesters. 
# This program can be applied to new semesters' data
# The data could be downloaded from D2L

require(ggplot2)

s13b <- read.csv('d2l_spring_13_b.csv', na.strings='--')
s13a <- read.csv('d2l_spring_13_a.csv', na.strings='--')
f12 <- read.csv('d2l_fall_12.csv', na.strings='--')
s12 <- read.csv('d2l_spring_12.csv', na.strings='--')
f11b <- read.csv('d2l_fall_11_b.csv', na.strings='--')
f11a <- read.csv('d2l_fall_11_a.csv', na.strings='--')

s13b <- within(s13b, 
{
  NetID <- NetID.Text.Grade..Text.
  IA <- SR...Initial.Assessment.Text.Grade..Text.
  BaseTopics <- Base.topics.Text.Grade..Text.
  CourseGoal <- Course.Goal.Points.Grade..Numeric.MaxPoints.10.Weight.0.00001.
  GoalCourse <- Goal.Class..based.on.Course.Goal.Quiz..Text.Grade..Text.
  FE <- FE.ALEKS.score.Text.Grade..Text.
})

s13a <- within(s13a, 
{
  NetID <- NetID.Text.Grade..Text.
  Score <- as.numeric(gsub("[^0-9]", '', Pre.Math100.placement.score.Text.Grade..Text.))
  IA <- SR...Initial.Assessment.in.Beg.Int.Alg.Text.Grade..Text.
  BasePerc <- Base.percentage.Text.Grade..Text.
  CourseGoal <- Course.Goal.Points.Grade..Numeric.MaxPoints.10.Weight.0.00001.
  GoalCourse <- Goal.Class..based.on.Course.Goal.Quiz..Text.Grade..Text.
  FE <- FE.ALEKS.score.Text.Grade..Text.
  Goal.HW.Grade <- factor(BasePerc)
})

f12 <- within(f12, 
{
  NetID <- NetID.Text.Grade..Text.
  Score <- as.numeric(gsub("[^0-9]", '', Pre.Math100.placement.score.Text.Grade..Text.))
  IA <- SR...Initial.Assessment.Text.Grade..Text.
  BasePerc <- as.numeric(as.character(Base.percentage.Text.Grade..Text.))
  CourseGoal <- Course.Goal.Points.Grade..Numeric.MaxPoints.10.Weight.0.00001.
  GoalCourse <- Goal.Class..based.on.Course.Goal.Quiz..Text.Grade..Text.
  FE <- FE.ALEKS.score.Text.Grade..Text.
  Goal.HW.Grade <- factor(BasePerc)
})

s12 <- within(s12, 
{
  NetID <- NetID.Text.Grade..Text.
  IA <- SR...Initial.Assessment.Text.Grade..Text.
  BasePerc <- Base.percentage.Text.Grade..Text.
  CourseGoal <- Course.Goal.Points.Grade..Numeric.MaxPoints.10.Weight.0.00001.
  GoalCourse <- Goal.Class..based.on.Course.Goal.Quiz..Text.Grade..Text.
  FE <- FE.ALEKS.score.Text.Grade..Text.
  Goal.HW.Grade <- factor(BasePerc)
})

f11b <- within(f11b, 
{
  NetID <- tolower(gsub('^#','',Username))
  IA <- SR...Initial.Assessment.Text.Grade..Text.
  #BasePerc <- Base.percentage.Text.Grade..Text.
  #CourseGoal <- Course.Goal.Points.Grade..Numeric.MaxPoints.10.Weight.0.00001.
  #GoalCourse <- Goal.Class..based.on.Course.Goal.Quiz..Text.Grade..Text.
  FE <- FE.Points.Grade..Numeric.MaxPoints.100.Weight.100.Category.Final.Exam.CategoryWeight.40.
  #Goal.HW.Grade <- factor(BasePerc)
})

f11a <- within(f11a, 
{
  NetID <- tolower(gsub('^#','',Username))
  IA <- SR...Initial.Assessment.Text.Grade..Text.
  #BasePerc <- Base.percentage.Text.Grade..Text.
  #CourseGoal <- Course.Goal.Points.Grade..Numeric.MaxPoints.10.Weight.0.00001.
  #GoalCourse <- Goal.Class..based.on.Course.Goal.Quiz..Text.Grade..Text.
  FE <- FE.Points.Grade..Numeric.MaxPoints.100.Weight.100.Category.Final.Exam.CategoryWeight.40.
  #Goal.HW.Grade <- factor(BasePerc)
})

letter.grade <- list('A+'=100,
                     'A'=seq(90,99),
                     'B'=seq(80,89),
                     'C'=seq(70,79),
                     'D'=seq(60,69),
                     'E'=seq(0,59),
                     '>100'=seq(101,200))
spring.13.short.letter <- list('A/A+'=c('A+','A'),
                               'B-C'=c('B','C'),
                               'D'=c('D'),
                               '>100'='>100')

# Rename factor levels
levels(s13a$Goal.HW.Grade) <- letter.grade
levels(s13a$Goal.HW.Grade) <- spring.13.short.letter
levels(f12$Goal.HW.Grade) <- letter.grade
levels(s12$Goal.HW.Grade) <- letter.grade

###############################################################################

# Size fixing options
aspectratio <- 2/(1+sqrt(5))
paper.width <- (8.5-2*1) # Paper width with 1" margins
paper.ratio <- function(facet.width)
{
  # Approximate paper size ratio for a single row of facets
  paper.single.ratio <- 0.75
  paper.multi.ratio <- 0.75
  return(paper.single.ratio/(1 + (facet.width-1)*paper.multi.ratio))
}
#paper.height <- 0.75*paper.width
my.theme <- theme_bw(base_size=1) +
  theme(aspect.ratio = aspectratio )

###############################################################################

p.bp.goal.course.s13a <- qplot(x=GoalCourse, y=BasePerc, data=s13a, geom='jitter')
p.bp.goal.course.f12 <- qplot(x=GoalCourse, y=BasePerc, data=f12, geom='jitter', na.rm=T)
p.bp.goal.course.s12 <- qplot(x=GoalCourse, y=BasePerc, data=s12[s12$GoalCourse!='', ], geom='jitter')

p.fe.goal.hw.s13a <- qplot(x=Goal.HW.Grade, y=FE, data=s13a, geom='boxplot', na.rm=TRUE) +
  geom_hline(y=70)
p.fe.goal.hw.f12 <- qplot(x=Goal.HW.Grade, y=FE, data=f12, geom='boxplot', facets=~GoalCourse, na.rm=TRUE) +
  geom_hline(y=70)
p.fe.goal.hw.s12 <- qplot(x=Goal.HW.Grade, y=FE, data=s12[s12$GoalCourse!='', ], geom='boxplot', facets=~GoalCourse, na.rm=TRUE) +
  geom_hline(y=70)

p.fe.goal.course.f12 <- qplot(x=GoalCourse, y=FE, data=f12, geom='boxplot', na.rm=TRUE)