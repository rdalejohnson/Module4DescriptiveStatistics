#---
#title: "JOHNSON_BST611_HW2_Dale"
#author: "R. Dale Johnson"
#date: "02/04/2020"
#output: pdf_document
#---

# SOURCES:
#  mappings for columns using PANDAS
#  https://stackoverflow.com/questions/49382207/how-to-map-numeric-data-into-categories-bins-in-pandas-dataframe
#  use of the factor function
#  https://www.datamentor.io/r-programming/factor/
#  converting columns in dataframe into factors
#  https://stackoverflow.com/questions/9251326/convert-data-frame-column-format-from-character-to-factor
#  https://vitalflux.com/learn-r-convert-columns-character-factor/
#  factors in R (EXCELLENT REFERENCE) - labels vs levels, why this is done and reversed, etc.
#  https://www.stat.berkeley.edu/~s133/factors.html
#  frequencies and other tables
#  https://www.statmethods.net/stats/frequencies.html
#  Table() function not as good as count function in the plyr package
#  https://www.r-bloggers.com/how-to-get-the-frequency-table-of-a-categorical-variable-as-a-data-frame-in-r/
#  Using dplyr package to compute frequencies
#  https://stackoverflow.com/questions/24576515/relative-frequencies-proportions-with-dplyr
#  using filter to remove NA when doing a frequency
#  https://forcats.tidyverse.org/
#  using table()
#  https://www.r-bloggers.com/r-function-of-the-day-table/
#  Charts
#  https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
#  mosaics by hand
#  https://www.youtube.com/watch?v=x88vSpqPPjc


#library(tidyverse)

library(dplyr)
library(ggplot2)
library(vcd)
library(gmodels)


# read your file
lab=read.csv("Greek_Residence.csv")

original_lab <- lab
summary(original_lab)

labs.number_of_rows <- nrow(lab)

mean(lab$age.in.years, na.rm=TRUE)

#Add columns for the other descriptive stats here



table2 <- table(lab$gender)

table2
prop.table(table2)

print(table2)
print(as.data.frame(table2))


#factor(lab$gender)
#unique(lab$gender)

######### CONVERT CHARACTER COLUMNS INTO FACTORS ###############

lab <- lab %>% mutate(gender = ifelse(is.na(gender) == TRUE, 99, gender))
lab <- lab %>% mutate(class = ifelse(is.na(class) == TRUE, 99, class))
lab <- lab %>% mutate(fratsoro = ifelse(is.na(fratsoro) == TRUE, 99, fratsoro))
lab <- lab %>% mutate(drinks5 = ifelse(is.na(drinks5) == TRUE, 99, drinks5))
lab <- lab %>% mutate(livewith = ifelse(is.na(livewith) == TRUE, 99, livewith))


lab$gender <- factor(lab$gender,labels=c("Male", "Female", "U"))
lab$class <- factor(lab$class,labels=c("Fr", "So", "Jr", "Sn", "5+", "U"))
#lab$fratsoro <- factor(lab$fratsoro,labels=c("N", "Y", "U"))
lab$drinks5 <- factor(lab$drinks5,labels=c("None", "1", "2", "3-5", 
                                           "6-9", "10+", "U"), ordered=TRUE)
lab$livewith <- factor(lab$livewith,labels=c("N", "Y", "U"), ordered=TRUE)

summary(lab)

head(lab)

CrossTable(lab$gender, lab$class, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

xtabs(~gender + class + Drinking_Binges, data=lab)


################################################################################
#### ANALYSIS OF HYPOTHESIS 1: 
####  Students who live at a frat/sorority house will report higher binge drinking
####  compared to students who reside elsewhere




#labs2 <- subset(x=lab, subset=(gender == "Female" )  )
names(lab)[names(lab) == "drinks5"] <- "Drinking_Binges"
names(lab)[names(lab) == "fratsoro"] <- "Greek_Member"
names(lab)[names(lab) == "drinkprob"] <- "Drinking_Problem"
names(lab)[names(lab) == "livewith"] <- "Greek_House"


lab.crosstab.greek.binges = table(lab$Greek_House, lab$Drinking_Binges)

#The dimension for this table are
# 1 = greek house residency (Y/N)
# 2 = drinking binges
lab.crosstab.greek.binges

# ask for totals by dimension 1, which is greek house residency;  
#Should add up to 1400 since nothing is being exluded
margin.table(lab.crosstab.greek.binges, 1)

#ask for totals by dimension2, whichis binges
#Should add up to 1400 since nothing is being exluded
margin.table(lab.crosstab.greek.binges, 2)

#Compute the PROPORTION(percentage) for every cell in the lab.crosstab.greek.binges
#In this case, the percentages in all the cells in the entire table will be rounded to 2 decimal places
#and if you add up all the values in all the cells, you'll get 100%
round(prop.table(lab.crosstab.greek.binges), 2)


testingdf = as.data.frame(round(prop.table(lab.crosstab.greek.binges, margin=1), digits=2))


round(prop.table(lab.crosstab.greek.binges, margin=1), digits=2)
round(prop.table(lab.crosstab.greek.binges, margin=2), digits=2)


# https://www.r-bloggers.com/how-to-convert-contingency-tables-to-data-frames-with-r/


dataFrameCrosstab <- as.data.frame.matrix(lab.crosstab.greek.binges)

#counts and frequencies (proportions)
########       GENDER     ########################
table(lab$gender)
prop.table(table(lab$gender))

#All in one version using dplyr
labGender <- as.data.frame(
  lab %>%
    group_by(gender) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
  
)

is.num <- sapply(labGender, is.numeric)
labGender[is.num] <- lapply(labGender[is.num], round, 2)

labGender


#########  CLASS/YEAR IN COLLEGE ############
table(lab$class)
prop.table(table(lab$class))

#All in one version using dplyr
labClass <- as.data.frame(
  lab %>%
    group_by(class) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
  
)


is.num <- sapply(labClass, is.numeric)
labClass[is.num] <- lapply(labClass[is.num], round, 2)

labClass

#########  GREEK MEMBERSHIP IN COLLEGE ############
table(lab$where.the.respondent.lives)
prop.table(table(lab$where.the.respondent.lives))

#All in one version using dplyr
labGreekMembers <- as.data.frame(
  lab %>%
    group_by(where.the.respondent.lives) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
  
)


is.num <- sapply(labGreekMembers, is.numeric)
labGreekMembers[is.num] <- lapply(labGreekMembers[is.num], round, 2)

labGreekMembers



#########  BINGE DRINKING  ############
table(lab$binge.drinking)
prop.table(table(lab$binge.drinking))

#All in one version using dplyr
labBinges <- as.data.frame(
  lab %>%
    group_by(binge.drinking) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
  
)


is.num <- sapply(labBinges, is.numeric)
labBinges[is.num] <- lapply(labBinges[is.num], round, 2)

labBinges




#########  LIVES at GREEKHOUSE   ############
table(lab$where.the.respondent.lives)
prop.table(table(lab$where.the.respondent.lives))

#All in one version using dplyr
labLivesAtGreekHouse <- as.data.frame(
  lab %>%
    group_by(where.the.respondent.lives) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
  
)

# labLivesAtGreekHouse <- as.data.frame(
#   original_lab %>%
#     group_by(livewith) %>%
#     summarise(n = n()) %>%
#     mutate(FreqPct = (n/labs.number_of_rows)*100 )
#   
# )


is.num <- sapply(labLivesAtGreekHouse, is.numeric)
labLivesAtGreekHouse[is.num] <- lapply(labLivesAtGreekHouse[is.num], round, 2)

labLivesAtGreekHouse

######## AGE ################


labAge <- as.data.frame(
  lab %>%
    group_by(age.in.years) %>%
    summarise(
      n = n()    ) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100,
           TOTAL = n)
)

summary(labAge$age.in.years)

#standard deviation
sd(lab$age.in.years, na.rm=TRUE)

mean(lab$age.in.years, na.rm=TRUE)


var(lab$age.in.years, na.rm=TRUE)


fivenum(labAge$age.in.years)

##############################

###Drinking_Problem

table(lab$has.a.drinking.problem)
prop.table(table(lab$has.a.drinking.problem))

fivenum(lab$has.a.drinking.problem)

labDrinking_Problem <- as.data.frame(
  lab %>%
    group_by(has.a.drinking.problem) %>%
    summarise(
      n = n()    ) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100)
)
labDrinking_Problem

summary(lab$has.a.drinking.problem)

#standard deviation
sd(lab$has.a.drinking.problem, na.rm=TRUE)


var(lab$has.a.drinking.problem, na.rm=TRUE)

quantile(lab$has.a.drinking.problem, na.rm=TRUE)
IQR(lab$has.a.drinking.problem, na.rm=TRUE)

# https://www.datamentor.io/r-programming/box-plot/

drinking_problem_score_plot <-
      boxplot(lab$has.a.drinking.problem,
      main = "Drinking Problem Scores (10 item questionnaire)",
      xlab = "Summarized Scores for 1120 Students",
      col = "orange",
      border = "brown",
      horizontal = TRUE,
      notch = TRUE)

drinking_problem_score_plot

summary(lab$has.a.drinking.problem)
# https://stackoverflow.com/questions/12866189/calculating-the-outliers-in-r

lab$Drinking_Problem_Cats<-cut(lab$has.a.drinking.problem, 
                               breaks=c(-Inf,4.5, 24.5, +Inf), 
                               labels=c("Lower Outliers", "Withing 25 and 75 percentile", "Upper Outliers"), right=FALSE)
lab$has.a.drinking.problem
summary(lab$has.a.drinking.problem)

###################################

structedlabs <- structable(~ binge.drinking + where.the.respondent.lives, data=lab, 
                           spacing = spacing_increase(start = 1, rate = 2.5), na.rm=TRUE)

structedlabs

dataFrameCrosstab <- as.data.frame.matrix(structedlabs)

dataFrameCrosstab

print(structedlabs)

structedlabs

binging = as.data.frame(table(lab$binge.drinking[!is.na(lab$binge.drinking)]))

binging <- binging %>% mutate(Var1 = ifelse(is.na(Var1) == TRUE, "Missing Data", Var1))

ggplot(binging, aes(x=Var1, y=frequency(), geom_col(  )  ))
ggplot(binging, aes(x=Var1)) + geom_col())

plot_data <- lab %>% 
  group_by(Greek_House, Drinking_Binges) %>% 
  tally %>% 
  mutate(percent = round(n/sum(n)*100, 2) )

plot_data

# ggplot(plot_data, aes(x = Drinking_Binges, y = percent)) +
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = percent(percent)), vjust = -0.5) +
#   labs(title = "Title", y = "percent", x = "Greek House") +
#   scale_y_continuous(labels = percent, limits = c(0,1)) +
#   scale_x_discrete(labels = function(x) str_wrap(x, 10)) +
#   facet_wrap(~Greek_House) 


# labelBinge = c('None', 'Once', 'Twice', '3-5', '6-9', '10+')
# > bingeCountsGreek = c(10, 10, 12, 13, 2, 1)
# > bingeCountsOther = c(835, 175, 111, 137, 46, 17)
# > graphable <- data.frame(labelBinge, bingeCountsOther, bingeCountsGreek)

###################




structable(~Greek_House + Drinking_Binges, data=lab)

xtabs(~Greek_House + Drinking_Binges, data=lab)

library(gmodels)
CrossTable(lab$Greek_House, lab$Drinking_Binges, dnn = c("Lives in Greek House", "# Drinking Binges"),
           prop.chisq = FALSE, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

niceXData = as.data.frame (
  CrossTable(lab$Greek_House, lab$Drinking_Binges, dnn = c("Lives in Greek House", "# Drinking Binges"),
             prop.chisq = FALSE, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
)


niceXData = as.data.frame(  CrossTable(lab$Drinking_Binges, lab$Greek_House, dnn = c("# Drinking Binges", "Lives in Greek House"),
                                       prop.chisq = FALSE, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
)


(fill_colors <- matrix(c("dark cyan","gray","violet","dark magenta"), ncol = 2))


#fontsize idea from https://stat.ethz.ch/pipermail/r-help/2007-September/141170.html

mosaic(~   Greek_House + Drinking_Binges , 
       data=lab, direction=c("h", "v"),
       zero_size=0,
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0),
       labeling_args=list(rot_labels=c(left=0,top=90),gp_labels=(gpar(fontsize=8)),
                          # offset_varnames = c(top = 1), 
                          offset_labels = c(left = 0.3, top=0.3))
)






################# FREQUENCY TABLES ######################
#library(plyr)


labs.number_of_rows <- nrow(lab)
#labs.missing_genders <- sum(lab$gender == "Missing or Unknown")

CrossTable(lab$gender, lab$class, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

#gender.frequency <- plyr::count(lab, "gender")

labGender <- as.data.frame(
  lab %>%
    group_by(gender) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
)

is.num <- sapply(labGender, is.numeric)
labGender[is.num] <- lapply(labGender[is.num], round, 2)

print(labGender)

table(lab$gender, lab$class)

#highlighting="gender", highlighting_fill = c("#CCEEFF", "#BB00AA")

#mosaic(~gender + drinks5+fratsoro, data=lab, 
#       direction=c("h", "v", "h"))

structedlabs <- structable(~fratsoro+ gender + class + drinks5, data=lab, 
                           spacing = spacing_increase(start = 1, rate = 2.5), na.rm=TRUE)

structedlabs

mosaic(structedlabs)

strucplot(structedlabs)

mosaic(~gender +fratsoro+ drinks5 + class  ,
       data=lab, direction=c("h", "v", "h", "h"))


# mosaic(~  gender  ,
#        data=lab, direction=c("h"),
#        spacing = spacing_increase(),
#        gp = gpar(fill = fill_colors, col = 0))


mosaic(~  gender + fratsoro + class + drinks5 ,
       data=lab, direction=c("h", "v", "v", "h"),
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0))



mosaic(~ gender + fratsoro + class , zero_size = 0,
       data=lab, direction=c("h", "v", "v"),
       
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0))

#labeling = labeling_list, margins = c(bottom = 5))

# barplot(table(lab$gender, lab$drinks5))
# 
# barplot(table(lab$gender))
# 
# ggplot(lab, aes(x=gender)) + 
#   geom_bar(aes(fill = class))+ 
#   geom_bar(aes(fill = fratsoro))


#margin.table(as.array  (  labGender$n),1)
################

labClass <- as.data.frame(
  lab %>%
    group_by(class) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
)

is.num <- sapply(labClass, is.numeric)
labClass[is.num] <- lapply(labClass[is.num], round, 2)

print(labClass)


################

labFratSoro <- as.data.frame(
  lab %>%
    group_by(fratsoro) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
)

is.num <- sapply(labFratSoro, is.numeric)
labFratSoro[is.num] <- lapply(labFratSoro[is.num], round, 2)

print(labFratSoro)

################

labdrinks5 <- as.data.frame(
  lab %>%
    group_by(drinks5) %>%
    summarise(n = n()) %>%
    mutate(FreqPct = (n/labs.number_of_rows)*100 )
)

is.num <- sapply(labdrinks5, is.numeric)
labdrinks5[is.num] <- lapply(labdrinks5[is.num], round, 2)

print(labdrinks5)




#############################


#labs2 <- subset(x=lab, subset=(gender == "Female" )  )
names(lab)[names(lab) == "drinks5"] <- "Drinking_Binges"
names(lab)[names(lab) == "fratsoro"] <- "Greek_Member"
names(lab)[names(lab) == "drinkprob"] <- "Drinking_Problem"
names(lab)[names(lab) == "livewith"] <- "Greek_House"


structable(~Greek_House + Drinking_Binges, data=lab)





###################################

femaleOnlyDF <- subset(x=lab, subset=(gender == "Female" )  )
names(femaleOnlyDF)[names(femaleOnlyDF) == "drinks5"] <- "Drinking_Binges"
names(femaleOnlyDF)[names(femaleOnlyDF) == "fratsoro"] <- "Greek_Member"
names(femaleOnlyDF)[names(femaleOnlyDF) == "drinkprob"] <- "Drinking_Problem"
names(femaleOnlyDF)[names(femaleOnlyDF) == "livewith"] <- "Greek_House"






femaleCrossTab <- structable(~Drinking_Binges+Greek_House, data=femaleOnlyDF)



mosaic(~  Drinking_Binges + Greek_House,
       zero_size=0,
       data=femaleCrossTab, direction=c("h", "v"),
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0))


mosaic(~   Greek_House + Drinking_Binges , 
       data=femaleCrossTab, direction=c("h", "v"),
       zero_size=0,
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0))



femaleCrossTab2 <- structable(~Drinking_Problem+Greek_Member+class, data=femaleOnlyDF)



mosaic(~  Drinking_Problem + Greek_Member,
       zero_size=0,
       data=femaleCrossTab2, direction=c("h", "v"),
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0))


mosaic(~   Greek_Member + Drinking_Problem , 
       data=femaleCrossTab2, direction=c("h", "v"),
       zero_size=0,
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0))




