################################################################################
#############################   CA3   ##########################################
################################################################################
###OPENING CSV FILE

dataset <- read.csv ("C:\\Users\\Silvia\\OneDrive - Dundalk Institute of Technology\\University\\Data Analytics for Immersive Environments\\CA3\\daie_ca3_data_6.csv")
dataset

###ESTABLISHING HYPOTHESIS
#Gender is the explanatory variable and pre trial OR scores are the response variable.

#H0:there is no significant difference in pre trial OR scores among the 2 genders.
#H1: there is a significant difference between the pre trial OR scores among the 2 genders.

###DATA CLEANING

#TEST GROUP: based on the information provided in the CA brief we expect to see 2 possible values in the gender column.
#            These values are: Male and Female.
install.packages("dplyr") #installing dplyr package
library("dplyr") #loading dlpyr package
which (is.na (dataset$gender)) #checking for missing values. NO MISSING VALUES.
categories <- unique (dataset$gender) #finding which categories are actually present in the column
categories #3 categories have been found. Fmale is the extra category.
dataset$gender[dataset$gender == 'Fmale'] <- 'Female' #replacing Fmale with Female
categories_updated <- unique (dataset$gender) #finding which categories are now present in the column
categories_updated 