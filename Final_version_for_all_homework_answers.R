library(dplyr)
library(ggplot2)
library(vcd)
library(gmodels)


# read your file
lab=read.csv("Greek_Residence.csv")

#https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement

##################### GENDER SUMMARY ****************************

lab$gender <- as.character(lab$gender)

lab <- lab %>% mutate(gender = ifelse(lab$gender == "", "NA/Blank", gender))

lab$gender <- as.factor(lab$gender)


#Build the starting table of values and counts

labGenderSummary = as.data.frame(table(lab$gender))

colnames(labGenderSummary) <- c("Gender.Code","Frequency.Count")


# labGenderSummary <- rbind(labGenderSummary, 
#   data.frame(
#     "Gender.Code" = "NA" , 
#     "Frequency.Count" = sum(is.na(lab$gender) || length(lab$gender) == 0) )  )
#   )

#Add a percentage-of-total column rounded to 2 decimal places

labGenderSummary <- mutate(labGenderSummary,
                      Gender.Percent = round(Frequency.Count/sum(Frequency.Count)*100, 2)
                    )

labGenderSummary

##################### WHERE LIVES SUMMARY ****************************


lab$where.the.respondent.lives <- as.character(lab$where.the.respondent.lives)

lab <- lab %>% 
      mutate(where.the.respondent.lives = 
                ifelse(lab$where.the.respondent.lives == "", "NA/Blank", where.the.respondent.lives))

lab$where.the.respondent.lives <- as.factor(lab$where.the.respondent.lives)


#Build the starting table of values and counts

labWhereLivesSummary = as.data.frame(table(lab$where.the.respondent.lives))

colnames(labWhereLivesSummary) <- c("Gender.Code","Frequency.Count")


# labGenderSummary <- rbind(labGenderSummary, 
#   data.frame(
#     "Gender.Code" = "NA" , 
#     "Frequency.Count" = sum(is.na(lab$gender) || length(lab$gender) == 0) )  )
#   )

#Add a percentage-of-total column rounded to 2 decimal places

labWhereLivesSummary <- mutate(labWhereLivesSummary,
                           WhereLives.Percent = round(Frequency.Count/sum(Frequency.Count)*100, 2) )

labWhereLivesSummary


##################### BINGE DRINKING SUMMARY ****************************


lab$binge.drinking <- as.character(lab$binge.drinking)

lab <- lab %>% 
  mutate(binge.drinking = 
           ifelse(lab$binge.drinking == "", "NA/Blank", binge.drinking))

lab$binge.drinking <- as.factor(lab$binge.drinking)


#Build the starting table of values and counts

labBingesSummary = as.data.frame(table(lab$binge.drinking))

colnames(labBingesSummary) <- c("Gender.Code","Frequency.Count")


# labGenderSummary <- rbind(labGenderSummary, 
#   data.frame(
#     "Gender.Code" = "NA" , 
#     "Frequency.Count" = sum(is.na(lab$gender) || length(lab$gender) == 0) )  )
#   )

#Add a percentage-of-total column rounded to 2 decimal places

labBingesSummary <- mutate(labBingesSummary,
                               binges.Percent = round(Frequency.Count/sum(Frequency.Count)*100, 2) )

labBingesSummary


#################### AGE IN YEARS SUMMARY **********************************


labAgeInYearsSummary = as.data.frame(table(lab$age.in.years))

colnames(labAgeInYearsSummary) <- c("Age.Value","Frequency.Count")

labAgeInYearsSummary <- rbind(labAgeInYearsSummary,
  data.frame(
     "Age.Value"= "-9999" ,
    "Frequency.Count" = sum(is.na(lab$age.in.years)  )  )
  )



labAgeInYearsSummary <- mutate(labAgeInYearsSummary,
                               ages.Percent = round(Frequency.Count/sum(Frequency.Count)*100, 2) )

sum(labAgeInYearsSummary$Frequency.Count)
sum(labAgeInYearsSummary$ages.Percent)

min(lab$age.in.years, na.rm=TRUE)                                      
max(lab$age.in.years, na.rm=TRUE)                                      
mean(lab$age.in.years, na.rm=TRUE)
median(lab$age.in.years, na.rm=TRUE)
var(lab$age.in.years, na.rm=TRUE)
sd(lab$age.in.years, na.rm=TRUE)



quart <- function(x) {
  x <- sort(x)
  n <- length(x)
  m <- (n+1)/2
  if (floor(m) != m) {
    l <- m-1/2; u <- m+1/2
  } else {
    l <- m-1; u <- m+1
  }
  c(Q1=median(x[1:l]), Q2=median(x[1:n]), Q3=median(x[u:n]) )
}

# y <- matrix(NA, 2, 10)
# rownames(y) <- c("Q1", "Q3")
# colnames(y) <- c(1:9, "Quart")
# for (n in 3:5) {
#   j <- 1
#   for (i in 1:9) {
#     y[, i] <- quantile(1:n, probs=c(1/4, 3/4), type=i)
#   }
#   y[, 10] <- quart(1:n)
#   cat("\n", n, ":\n")
#   print(y, digits=2)
# }

quartilz = as.data.frame( quart(lab$age.in.years))





#################### DRINKING PROBLEMS SCORES SUMMARY **********************************


labDrinkingProblemsScore = as.data.frame(table(lab$has.a.drinking.problem))

colnames(labDrinkingProblemsScore) <- c("Problem.Score.Value","Frequency.Count")

labDrinkingProblemsScore <- rbind(labDrinkingProblemsScore,
                              data.frame(
                                "Problem.Score.Value"= "-9999" ,
                                "Frequency.Count" = sum(is.na(lab$has.a.drinking.problem)  )  )
)



labDrinkingProblemsScore <- mutate(labDrinkingProblemsScore,
                               ages.Percent = round(Frequency.Count/sum(Frequency.Count)*100, 2) )

sum(labDrinkingProblemsScore$Frequency.Count)
sum(labDrinkingProblemsScore$ages.Percent)

min(lab$has.a.drinking.problem, na.rm=TRUE)                                      
max(lab$has.a.drinking.problem, na.rm=TRUE)                                      
mean(lab$has.a.drinking.problem, na.rm=TRUE)
median(lab$has.a.drinking.problem, na.rm=TRUE)
var(lab$has.a.drinking.problem, na.rm=TRUE)
sd(lab$has.a.drinking.problem, na.rm=TRUE)


quartilz = as.data.frame( quart(lab$has.a.drinking.problem))



#******************************** HYPOTHESIS 1


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




##*************** HYPOTHESIS 2 ***************************

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



