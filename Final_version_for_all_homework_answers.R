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
     "Age.Value"= "NA" ,
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

percent
as.data.frame.table (quantile(lab$age.in.years, na.rm=TRUE))

#Add columns for the other descriptive stats here



summary(lab$age.in.years)


