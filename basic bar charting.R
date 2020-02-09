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

########### THE ONE TO USE #############
#telling ggplot to map the data in the binge column to the fill aesthetic (where someone lives)
ggplot(lab) +
  geom_bar(aes(x = binge.drinking, fill=where.the.respondent.lives))

ggplot(lab) + 
  geom_bar(aes(x=binge.drinking, fill=where.the.respondent.lives), position=position_dodge(preserve = 'single'))


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