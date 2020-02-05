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


lab$gender <- factor(lab$gender,labels=c("Male", "Female", "Missing or Unknown"))
lab$class <- factor(lab$class,labels=c("Freshman", "Sophomore", "Junior", "Senior", "5th Year or Higher"))
lab$fratsoro <- factor(lab$fratsoro,labels=c("Frat/Sorority Member", "Not a Frat/Sorority Member"))
lab$drinks5 <- factor(lab$drinks5,labels=c("Zero binges", "One Binge", "Two Binges", "Three to Five Binges", 
                                           "Six to Nine Binges", "Ten or More Binges"), ordered=TRUE)

################# FREQUENCY TABLES ######################
library(plyr)


labs.number_of_rows <- nrow(lab)

gender.frequency <- plyr::count(lab, "gender")


lab %>%
  filter(!is.na(gender)) %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(freq = (n / sum(n))*100 ) 
#%>%
#  mutate(naFreq = (n/labs.number_of_rows*100))  

lab %>%
  filter(is.na(gender)) %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  mutate(freq = (n / labs.number_of_rows)*100 )


lab %>%
  group_by(gender) %>%
  summarise(nonna = sum(!is.na(gender))) %>%
  mutate(nonaFreq = n / sum(nonna) )         
         

lab %>%
  group_by(gender) %>%
  summarise(n = n()) %>%
  #summarise(nonNAGenderCount = sum(!is.na(gender)) %>%
  mutate(freq = n / sum(n)) 
#%>%
#mutate(noNAfreq = n/sum(nonNAGenderCount))

         

arrange(lab, gender)



# change "1/2" to "Male/Female"
#lab$gender = gsub(1, "Male",   lab$gender)
#lab$gender = gsub(2, "Female", lab$gender)

# change college class numbers to words
lab$class = gsub(1, "Freshman", lab$class)
lab$class = gsub(2, "Sophomore", lab$class)
lab$class = gsub(3, "Junior", lab$class)
lab$class = gsub(4, "Senior", lab$class)
lab$class = gsub(5, "5th year or Higher", lab$class)

lab$fratsoro = gsub(1, "Frat/Sorority Member", lab$fratsoro)
lab$fratsoro = gsub(2, "Not a Frat/Sorority Member", lab$fratsoro)

--# of drinking binges over the last two weeks
lab$drinks5 = ifelse(is.na(lab$drinks5), -1, lab$drinks5)
lab$drinks5 = gsub(1, "Zero binges", lab$drinks5)
lab$drinks5 = gsub(2, "One Binge", lab$drinks5)
lab$drinks5 = gsub(3, "Two Binges", lab$drinks5)
lab$drinks5 = gsub(4, "Three to Five Binges", lab$drinks5)
lab$drinks5 = gsub(5, "Six to Nine Binges", lab$drinks5)
lab$drinks5 = gsub(6, "Ten or More Binges", lab$drinks5)

sapply(lab, class)