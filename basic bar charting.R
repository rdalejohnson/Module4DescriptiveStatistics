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

unique(lab$has.a.drinking.problem)
table(lab$has.a.drinking.problem)  #gives you frequency counts for the unique values
summary(lab$has.a.drinking.problem)
#unique(lab$has.a.drinking.problem)  #like DISTINCT in sql
#table(lab$has.a.drinking.problem)  #gives you frequency counts for the unique values
sd(lab$has.a.drinking.problem, na.rm=TRUE)
mean(lab$has.a.drinking.problem, na.rm=TRUE)
var(lab$has.a.drinking.problem, na.rm=TRUE)
median(lab$has.a.drinking.problem, na.rm=TRUE)



anyWithScoresAndProblems <- 
  select(filter(lab, where.the.respondent.lives != "" & has.a.drinking.problem > 0), c(where.the.respondent.lives, has.a.drinking.problem))

table(anyWithScoresAndProblems)
summary(anyWithScoresAndProblems)
unique(anyWithScoresAndProblems$has.a.drinking.problem)  #like DISTINCT in sql
sd(anyWithScoresAndProblems$has.a.drinking.problem, na.rm=TRUE)
mean(anyWithScoresAndProblems$has.a.drinking.problem, na.rm=TRUE)
var(anyWithScoresAndProblems$has.a.drinking.problem, na.rm=TRUE)
median(anyWithScoresAndProblems$has.a.drinking.problem, na.rm=TRUE)


nonGreeksWithScores <- 
  select(filter(lab, where.the.respondent.lives == "Lives Other" & has.a.drinking.problem > 0), c(where.the.respondent.lives, has.a.drinking.problem))

table(nonGreeksWithScores)
summary(nonGreeksWithScores)
unique(nonGreeksWithScores$has.a.drinking.problem)  #like DISTINCT in sql
table(nonGreeksWithScores$has.a.drinking.problem)  #gives you frequency counts for the unique values
sd(nonGreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
mean(nonGreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
var(nonGreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
median(nonGreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
IQR(nonGreeksWithScores$has.a.drinking.problem, na.rm=TRUE)



GreeksWithScores <- 
  select(filter(lab, where.the.respondent.lives == "Lives in Fraternity or Sorority" & has.a.drinking.problem != ""), c(where.the.respondent.lives, has.a.drinking.problem))

table(GreeksWithScores)
summary(GreeksWithScores)
unique(GreeksWithScores$has.a.drinking.problem)  #like DISTINCT in sql
table(GreeksWithScores$has.a.drinking.problem)  #gives you frequency counts for the unique values
sd(GreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
mean(GreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
var(GreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
median(GreeksWithScores$has.a.drinking.problem, na.rm=TRUE)
IQR(GreeksWithScores$has.a.drinking.problem, na.rm=TRUE)


#boxplot(GreeksWithScores$has.a.drinking.problem, horizontal=TRUE)

greek_score_plot <-
  boxplot(GreeksWithScores$has.a.drinking.problem,
          main = "Greek-Living Drinking Problem Scores (10 item questionnaire)",
          xlab = "Scores for 46 Students",
          xaxp  = c(10, 30, 4),
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE)

greek_score_plot
greekOutVals = greek_score_plot$out


nongreek_score_plot <-
  boxplot(nonGreeksWithScores$has.a.drinking.problem,
          main = "NONGreek-Living Drinking Problem Scores (10 item questionnaire)",
          xaxp  = c(10, 30, 4),
          
          col = "orange",
          border = "brown",
          horizontal = TRUE,
          notch = TRUE)

nongreek_score_plot
nongreekOutVals = nongreek_score_plot$out
# nonGreeksWithScoresEXC.Outliers <- 
#   select(filter(lab, where.the.respondent.lives == "Lives Other" & has.a.drinking.problem > 0 &
#                   has.a.drinking.problem <= 24.5 ), c(where.the.respondent.lives, has.a.drinking.problem))

summary(nonGreeksWithScoresEXC.Outliers$has.a.drinking.problem)

#c1 <- cut(GreeksWithScores$has.a.drinking.problem, breaks = c(-Inf, 10, 20, 30, 40))
#table(c1)

c2 <- cut(nonGreeksWithScores$has.a.drinking.problem, #breaks = c(-Inf, 10, 20, 30, 40))
            breaks = seq(1, 38, by = 3), na.rm=TRUE)
table(c2)
c1 <- cut(GreeksWithScores$has.a.drinking.problem, #breaks = c(-Inf, 10, 20, 30, 40))
          breaks = seq(1, 38, by = 3), na.rm=TRUE)
table(c1)

c1df = as.matrix(table(c1))
c2df = as.matrix(table(c2))

dfRanges <- data.frame(c1df, c2df)



c22 <- cut(nonGreeksWithScores$has.a.drinking.problem, #breaks = c(-Inf, 10, 20, 30, 40))
          breaks = seq(1, 38, by = 6), na.rm=TRUE)
table(c22)
c12 <- cut(GreeksWithScores$has.a.drinking.problem, #breaks = c(-Inf, 10, 20, 30, 40))
          breaks = seq(1, 38, by = 6), na.rm=TRUE)
table(c12)

c12df = as.matrix(table(c12))
c22df = as.matrix(table(c22))

dfRanges2 <- data.frame(c12df, c22df)



##### PUTTING THE VARIOUS OUTPUTS INTO GROUPS
########################################
unique(lab$has.a.drinking.problem)   #like DISTINCT in sql
unique(nonGreeksWithScores$has.a.drinking.problem)  #like DISTINCT in sql
unique(GreeksWithScores$has.a.drinking.problem)  #like DISTINCT in sql

lab %>% distinct(lab$where.the.respondent.lives, lab$has.a.drinking.problem, .keep_all = TRUE)

#######################################
table(lab$has.a.drinking.problem)  #gives you frequency counts for the unique values
table(nonGreeksWithScores$has.a.drinking.problem)  #gives you frequency counts for the unique values
table(GreeksWithScores$has.a.drinking.problem)  #gives you frequency counts for the unique values
##
lab %>% distinct(where.the.respondent.lives, has.a.drinking.problem, .keep_all = TRUE)
##
nonGreeksWithScores %>% distinct(where.the.respondent.lives, has.a.drinking.problem, .keep_all = TRUE)
##
GreeksWithScores %>% distinct(where.the.respondent.lives, has.a.drinking.problem, .keep_all = TRUE)
##


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

greekBinging
greekNOTBinging

data <- as.matrix(data.frame(
          D = c(20.83, 63.21),  # NONE greek, nogreek
          E = c(20.83, 13.25),  # ONCE greek, nogreek
          G = c(25, 8.4), #TWICE greek, nogreek
          B = c(27.08, 10.37),  #3-5 TIMES greek, nogreek
          C = c(4.17, 3.48), #6-9 TIMES greek, nogreek
          A = c(2.08, 1.29) #10+ TIMES greek, nogreek
))          
          
          

namesArg <- c("None", "Once", "Twice", "3-5", "6-9", "10 or More")

#convert all decimal proportions to percentages:

#data <- data * 100





# X and Y axis limits: http://howtoinr.weebly.com/customize-axis.html
# Dressing up barplot: https://stats.idre.ucla.edu/r/faq/how-can-i-add-features-or-dimensions-to-my-bar-plot/
# Colors; http://www.sthda.com/english/wiki/colors-in-r
# Tick marks and lines across plots: https://stackoverflow.com/questions/3785089/change-the-spacing-of-tick-marks-on-the-axis-of-a-plot
#   Installed package RColorBrewer
library(RColorBrewer)

par(mar=c(6,4,4,4))

mypalette<-RColorBrewer::brewer.pal(7,"Pastel2")


bp <- 
  barplot(
    data,
    #col = mypalette,
    axes = TRUE,
    main = "Percent Drinking Binges for Greek and Non-Greek Living",
    sub = "Sample counts: 48 Greek-Housed and 1321 Non-Greek-Housed Students who answered binge question",
    ylab = "Percentage of Binging Students within each Living Group",
    xlab = "Number of Drinking Binges Over Last Two Weeks",
    names.arg = namesArg,
    ylim = c( 0 , 100 ),
    legend = c("Greek Living", "No Greek Living"),
    col = c("lightcyan1", "lavenderblush"),
    beside = TRUE
  )

text(
    x=bp, 
    y = data+0.4, 
    label=paste(round(data, 1), '%', sep=""),
    cex=1, 
    pos = 3)

axis(side=2, tck=20, at=c(seq(from=0, to=100, by=20)))





# 
# 
# XLabels <- c("10+", "6-9", "3-5", "Once", "Twice", "None")
# Greeks <- c(0.02,	0.27,	0.04,	0.2,	0.2,	0.24)
# NonGreeks <- c(0.01,	0.1,	0.03,	0.62,	0.13,	0.08)
# graphable <- data.frame(XLabels, Greeks, NonGreeks)
# 
# 
# barplot(as.matrix(graphable))
# 
# prop.table(table(graphable$Greek, graphable$NonGreeks), 1)
# 
# values <-  c(906, 264, 689, 739, 938)
# 
# barplot(values)
# 
# r111 <- c("10+", 0.02, 0.01)
# graphable <- data.frame(r111)
# 
# ggplot(graphable) + 
#   geom_bar(aes(x=XLabels, fill=NonGreeks, position=position_dodge(preserve = 'single')))




# X <- rbind(X, Y[3,])
# #selet 
# y<-c(A=5, B=4.25, C=4.5)
# 
# #Test that the items in the row match the vector you wanted
# x[1,]==y
# 








# 
# 
# 
# grp.by.binge <- lab %>% group_by(binge.drinking) %>% summarise(age_groups = mean(age.in.years))
# 
# ggplot(grp.by.binge) + 
#   geom_bar(aes(x = binge.drinking, y = age_groups), stat = 'identity')
# 
# 
# # grp_by_both <- lab %>% group_by(binge.drinking, where.the.respondent.lives ) %>% 
# #   summarise(
# #     n = n()  ,
# #     percent = round(n/sum(n)*100, 2))
# 
# grp_by_binge_lives <- group_by(lab, binge.drinking, where.the.respondent.lives )
# counts_by_lives <- summarize(grp_by_binge_lives, number_binges = n())
# 
# counts_by_lives <- counts_by_lives[-c(1, 2, 3, 6, 11, 14), ]
# 
# ggplot(counts_by_lives) + 
#   geom_bar(aes(x=binge.drinking, y=number_binges), stat = 'identity')
# 
# 
# 
# 
# 
# ggplot(counts_by_lives) + 
#   geom_bar(aes(x = binge.drinking), position = position_dodge(preserve = 'single'))
# 
# mm2<- melt(counts_by_lives, where.the.respondent.lives='group')
# 
# ggplot(data = mm2, aes(x = binge.drinking, y = value, fill = variable)) + 
#   geom_col( )
# 
# #summarized_count_lives <- summarize(counts_by_lives, sum_binges = sum(number_binges))
# 
# ggplot(counts_by_lives) + 
#   geom_bar(aes(x = binge.drinking, y = number_binges), stat = 'identity')