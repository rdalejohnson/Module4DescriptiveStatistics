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
lab$gender = gsub(1, "Male", lab$gender)
lab$gender = gsub(2, "Female",lab$gender)