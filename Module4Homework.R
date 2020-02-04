#---
#title: "JOHNSON_BST611_HW2_Dale"
#author: "R. Dale Johnson"
#date: "02/04/2020"
#output: pdf_document
#---
  
# SOURCES:
#  mappings for columns using PANDAS
#  https://stackoverflow.com/questions/49382207/how-to-map-numeric-data-into-categories-bins-in-pandas-dataframe
  
# read your file
lab=read.csv("Binge_Drinking.csv")


# change "1/2" to "Male/Female"
lab$gender = gsub(1, "Male",   lab$gender)
lab$gender = gsub(2, "Female", lab$gender)

# change college class numbers to words
lab$class = gsub(1, "Freshman", lab$class)
lab$class = gsub(2, "Sophomore", lab$class)
lab$class = gsub(3, "Junior", lab$class)
lab$class = gsub(4, "Senior", lab$class)
lab$class = gsub(5, "5th year or Higher", lab$class)

lab$fratsoro = gsub(1, "Frat/Sorority Member", lab$fratsoro)
lab$fratsoro = gsub(2, "Not a Frat/Sorority Member", lab$fratsoro)

--# of drinking binges over the last two weeks
lab$drinks5 = gsub(1, "No binge", lab$drinks5)
lab$drinks5 = gsub(2, "One Binge", lab$drinks5)
lab$drinks5 = gsub(3, "Two Binges", lab$drinks5)
lab$drinks5 = gsub(4, "Three to Five Binges", lab$drinks5)
lab$drinks5 = gsub(5, "Six to Nine Binges", lab$drinks5)
lab$drinks5 = gsub(6, "Ten or More Binges", lab$drinks5)

