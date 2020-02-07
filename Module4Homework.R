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
  
# read your file
lab=read.csv("Binge_Drinking.csv")

original_lab <- lab
summary(original_lab)

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


################################################################################
#### ANALYSIS OF HYPOTHESIS 1: 
####  Students who live at a frat/sorority house will report higher binge drinking
####  compared to students who reside elsewhere




#labs2 <- subset(x=lab, subset=(gender == "Female" )  )
names(lab)[names(lab) == "drinks5"] <- "Drinking_Binges"
names(lab)[names(lab) == "fratsoro"] <- "Greek_Member"
names(lab)[names(lab) == "drinkprob"] <- "Drinking_Problem"
names(lab)[names(lab) == "livewith"] <- "Greek_House"


structable(~Greek_House + Drinking_Binges, data=lab)

#fontsize idea from https://stat.ethz.ch/pipermail/r-help/2007-September/141170.html

mosaic(~   Greek_House + Drinking_Binges , 
       data=lab, direction=c("h", "v"),
       zero_size=0,
       spacing = spacing_increase(),
       gp = gpar(fill = fill_colors, col = 0),
       labeling_args=list(rot_labels=c(left=0,top=90),gp_labels=(gpar(fontsize=8)),
       offset_varnames = c(top = 1), offset_labels = c(left = 0.3, top=0.3))
      )






################# FREQUENCY TABLES ######################
#library(plyr)
library(ggplot2)
library(vcd)
library(gmodels)


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

(fill_colors <- matrix(c("dark cyan","gray","violet","dark magenta"), ncol = 2))

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




