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



labAgeSummary <- as.data.frame.matrix(summary(lab$age.in.years)
                                      
                                      
mean(lab$age.in.years, na.rm=TRUE)
                                      
#Add columns for the other descriptive stats here

labAgeInYearsSummary <- mutate(labAgeInYearsSummary,
                               ages.Percent = round(Frequency.Count/sum(Frequency.Count)*100, 2) )






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