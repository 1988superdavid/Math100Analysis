Math112=read.delim('Math112S.txt',header=T)

### Put Catalog as factor, otherwise will have###
### problems with Tuckey's methord ###
Math112$Catalog=factor(Math112$Catalog)
Math112$TextGrade=factor(Math112$TextGrade)

## Set up reference group(treatment group)###
Math112$TextGrade=relevel(Math112$TextGrade,ref='N')

Math112100=Math112[Math112$Catalog!=2,]
Math112100B=Math112[Math112$Catalog!=1,]

Difference=aov(final~TextGrade,data=Math112)
summary(Difference)
tk=TukeyHSD(Difference)
plot(tk)

Math112$TextGrade=factor(Math112$TextGrade)
Difference2=aov(Math112$final~Math112$TextGrade)
summary(Difference2)
tk=TukeyHSD(Difference2)
plot(tk)

levels(Math112$TextGrade)

test=glht(Difference,linfct=mcp(TextGrade="Dunnett"))
plot(test)