##  https://www.r-bloggers.com/detailed-guide-to-the-bar-chart-in-r-with-ggplot/

library(tidyverse)
library(reshape)

ggplot(mpg) +
  geom_bar(aes(x = class))

#telling ggplot to map the data in the drv column to the fill aesthetic.
ggplot(mpg) +
  geom_bar(aes(x = class, fill = drv))

# Note we convert the cyl variable to a factor here in order to fill by cylinder
ggplot(mpg) + 
  geom_bar(aes(x = class, fill = factor(cyl)), position = position_dodge(preserve = 'single'))

# Use dplyr to calculate the average hwy_mpg by class
by_hwy_mpg <- mpg %>% group_by(class) %>% summarise(hwy_mpg = mean(hwy))

#Before, we did not specify a y-axis variable and instead let ggplot automatically 
#populate the y-axis with a count of our data. 
#Now, we’re explicityly telling ggplot to use hwy_mpg (computed just above) 
#as our y-axis variable.
#If you don’t specify stat = 'identity', then under the hood, ggplot is automatically passing 
#a default value of stat = 'count', which graphs the counts by group. 
#A y-variable is not compatible with this, so you get the error message.

ggplot(by_hwy_mpg) + 
  geom_bar(aes(x = class, y = hwy_mpg), stat = 'identity')

# Use dplyr to calculate the average hwy_mpg by class
by_hwy_mpg <- mpg %>% group_by(class) %>% summarise(hwy_mpg = mean(hwy))

#geom_col is the same as geom_bar with stat = 'identity'
ggplot(by_hwy_mpg) + 
  geom_col(aes(x = class, y = hwy_mpg))

####

lab=read.csv("Greek_Residence.csv")


ggplot(lab) +
  geom_bar(aes(x = gender))

ggplot(lab) +
  geom_bar(aes(x = where.the.respondent.lives))

ggplot(lab) +
  geom_bar(aes(x = binge.drinking))

ggplot(lab) +
  geom_bar(aes(x = has.a.drinking.problem))

ggplot(lab) +
  geom_bar(aes(x = has.a.drinking.problem), color='black', fill='salmon')

########### THE ONES TO USE #############
#telling ggplot to map the data in the binge column to the fill aesthetic (where someone lives)

####STACKED

ggplot(lab) +
  geom_bar(aes(x = binge.drinking, fill=where.the.respondent.lives))

###SIDE_BY_SIDE
ggplot(lab) + 
  geom_bar(aes(x=binge.drinking, fill=where.the.respondent.lives), position=position_dodge(preserve = 'single'))




GreekLiving <- lab[(lab$where.the.respondent.lives != "Lives Other" &
                    lab$where.the.respondent.lives != "" & 
                    !is.na(lab$where.the.respondent.lives)), ]


NOT.GreekLiving <- lab[(lab$where.the.respondent.lives == "Lives Other" &
                      lab$where.the.respondent.lives != "" & 
                      !is.na(lab$where.the.respondent.lives)), ] 

ggplot(GreekLiving) + 
  geom_bar(aes(x=binge.drinking), position=position_dodge(preserve = 'single'))


ggplot(NOT.GreekLiving) + 
  geom_bar(aes(x=binge.drinking), position=position_dodge(preserve = 'single'))


lab.crosstab.greek.binges = table(GreekLiving$where.the.respondent.lives, GreekLiving$binge.drinking)

lab.crosstab.NOTgreek.binges = table(NOT.GreekLiving$where.the.respondent.lives, NOT.GreekLiving$binge.drinking)


#The dimension for this table are
# 1 = greek house residency (Y/N)
# 2 = drinking binges
lab.crosstab.greek.binges

lab.crosstab.NOTgreek.binges



margin.table(lab.crosstab.greek.binges, 1)

#ask for totals by dimension2, whichis binges
#Should add up to 1400 since nothing is being exluded
margin.table(lab.crosstab.greek.binges, 2)

#Compute the PROPORTION(percentage) for every cell in the lab.crosstab.greek.binges
#In this case, the percentages in all the cells in the entire table will be rounded to 2 decimal places
#and if you add up all the values in all the cells, you'll get 100%
greekBinging = as.data.frame.matrix(round(prop.table(lab.crosstab.greek.binges), 2))

# ask for totals by dimension 1, which is greek house residency;  
#Should add up to 1400 since nothing is being exluded
margin.table(lab.crosstab.NOTgreek.binges, 1)

#ask for totals by dimension2, whichis binges
#Should add up to 1400 since nothing is being exluded
margin.table(lab.crosstab.NOTgreek.binges, 2)

#Compute the PROPORTION(percentage) for every cell in the lab.crosstab.greek.binges
#In this case, the percentages in all the cells in the entire table will be rounded to 2 decimal places
#and if you add up all the values in all the cells, you'll get 100%
round(prop.table(lab.crosstab.NOTgreek.binges), 2)

greekNOTBinging = as.data.frame.matrix(round(prop.table(lab.crosstab.NOTgreek.binges), 2))


data <- as.matrix(data.frame(
  A = c(0.02, 0.01),
B = c(0.27, 0.1),
C = c(0.04, 0.03),
D = c(0.2, 0.62),
E = c(0.2, 0.13),
G = c(0.24, 0.08)))

rownames(data) ＜- c("Group 1", "Group 2")

barplot(data,
col = c("#1b98e0", "#353436"),
beside = TRUE)


XLabels <- c("10+", "6-9", "3-5", "Once", "Twice", "None")
Greeks <- c(0.02,	0.27,	0.04,	0.2,	0.2,	0.24)
NonGreeks <- c(0.01,	0.1,	0.03,	0.62,	0.13,	0.08)
graphable <- data.frame(XLabels, Greeks, NonGreeks)


barplot(as.matrix(graphable))

prop.table(table(graphable$Greek, graphable$NonGreeks), 1)

values <-  c(906, 264, 689, 739, 938)

barplot(values)

r111 <- c("10+", 0.02, 0.01)
graphable <- data.frame(r111)

ggplot(graphable) + 
  geom_bar(aes(x=XLabels, fill=NonGreeks, position=position_dodge(preserve = 'single')))




X <- rbind(X, Y[3,])
#selet 
y<-c(A=5, B=4.25, C=4.5)

#Test that the items in the row match the vector you wanted
x[1,]==y

# testingdf = as.data.frame(round(prop.table(lab.crosstab.greek.binges, margin=1), digits=2))
# 
# 
# round(prop.table(lab.crosstab.greek.binges, margin=1), digits=2)
# round(prop.table(lab.crosstab.greek.binges, margin=2), digits=2)












grp.by.binge <- lab %>% group_by(binge.drinking) %>% summarise(age_groups = mean(age.in.years))

ggplot(grp.by.binge) + 
  geom_bar(aes(x = binge.drinking, y = age_groups), stat = 'identity')


# grp_by_both <- lab %>% group_by(binge.drinking, where.the.respondent.lives ) %>% 
#   summarise(
#     n = n()  ,
#     percent = round(n/sum(n)*100, 2))

grp_by_binge_lives <- group_by(lab, binge.drinking, where.the.respondent.lives )
counts_by_lives <- summarize(grp_by_binge_lives, number_binges = n())

counts_by_lives <- counts_by_lives[-c(1, 2, 3, 6, 11, 14), ]

ggplot(counts_by_lives) + 
  geom_bar(aes(x=binge.drinking, y=number_binges), stat = 'identity')





ggplot(counts_by_lives) + 
  geom_bar(aes(x = binge.drinking), position = position_dodge(preserve = 'single'))

mm2<- melt(counts_by_lives, where.the.respondent.lives='group')

ggplot(data = mm2, aes(x = binge.drinking, y = value, fill = variable)) + 
  geom_col( )

#summarized_count_lives <- summarize(counts_by_lives, sum_binges = sum(number_binges))

ggplot(counts_by_lives) + 
  geom_bar(aes(x = binge.drinking, y = number_binges), stat = 'identity')