library(dplyr)
library(ggplot2)
library(vcd)
library(gmodels)


# read your file
lab=read.csv("Greek_Residence.csv")

#https://stackoverflow.com/questions/5824173/replace-a-value-in-a-data-frame-based-on-a-conditional-if-statement

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

#########################





tableGender <- table(lab$gender)
prop.table(tableGender)



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