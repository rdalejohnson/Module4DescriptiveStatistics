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

#library(tidyverse)

library(dplyr)
  
# read your file
lab=read.csv("Binge_Drinking.csv")

#factor(lab$gender)
#unique(lab$gender)

######### CONVERT CHARACTER COLUMNS INTO FACTORS ###############

lab <- lab %>% mutate(gender = ifelse(is.na(gender) == TRUE, 99, gender))
lab <- lab %>% mutate(class = ifelse(is.na(class) == TRUE, 99, class))


lab$gender <- factor(lab$gender,labels=c("Male", "Female", "Missing or Unknown"))
lab$class <- factor(lab$class,labels=c("Freshman", "Sophomore", "Junior", "Senior", "5th Year or Higher", "Missing or Unknown"))
lab$fratsoro <- factor(lab$fratsoro,labels=c("Frat/Sorority Member", "Not a Frat/Sorority Member"))
lab$drinks5 <- factor(lab$drinks5,labels=c("Zero binges", "One Binge", "Two Binges", "Three to Five Binges", 
                                           "Six to Nine Binges", "Ten or More Binges"), ordered=TRUE)

################# FREQUENCY TABLES ######################
#library(plyr)


labs.number_of_rows <- nrow(lab)
#labs.missing_genders <- sum(lab$gender == "Missing or Unknown")

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




