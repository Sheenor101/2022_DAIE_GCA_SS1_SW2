################################################################################
#############################   CA3   ##########################################
################################################################################
###OPENING CSV FILE

dataset <- read.csv ("D:\\DKIT\Data Analytics\\dataset.csv")
dataset

###ESTABLISHING HYPOTHESIS
#Gender is the explanatory variable and pre trial OR scores are the response variable.

#H0:there is no significant difference in pre trial OR scores among the 2 genders.
#H1: there is a significant difference between the pre trial OR scores among the 2 genders.

###DATA CLEANING

#TEST GROUP: based on the information provided in the CA brief we expect to see 2 possible values in the gender column.
#These values are: Male and Female.
install.packages("dplyr") #installing dplyr package
library("dplyr") #loading dlpyr package
which (is.na (dataset$gender)) #checking for missing values. NO MISSING VALUES.
categories <- unique (dataset$gender) #finding which categories are actually present in the column
categories #3 categories have been found. Fmale is the extra category.
dataset$gender[dataset$gender == 'Fmale'] <- 'Female' #replacing Fmale with Female
categories_updated <- unique (dataset$gender) #finding which categories are now present in the column
categories_updated

#PRE TRIAL OR: we need to verify whether there is any missing values and/or any value below 0 or above 10 in post_trial_or (this is the range of scores for OR)

which (is.na (dataset$pre_trial_or)) #MISSING VALUE LINE 98
dataset$pre_trial_or [is.na (dataset$pre_trial_or)]<-mean (dataset$pre_trial_or,na.rm=TRUE) #replacing missing value with mean
which (is.na (dataset$pre_trial_or)) #checking again for missing values. NO MISSING VALUES
between (dataset$pre_trial_or, 0, 10) #checking if all values are between 0 and 10. ALL VALUES BETWEEN SELECTED RANGE.

###DESCRIPTIVE STATISTICS

#MEAN
mean_pre_or <- mean (dataset$pre_trial_or)
mean_pre_or #MEAN IS 6.12

#MEDIAN
median_pre_or <- median (dataset$pre_trial_or)
median_pre_or #MEDIAN IS 6.04

#MODE
getmode <- function(v) { # creating function to find mode
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_pre_or <- getmode (dataset$pre_trial_or)
mode_pre_or #MODE IS 5.83

#STANDARD DEVIATION
sd_pre_or <- sd (dataset$pre_trial_or)
sd_pre_or #STANDARD DEVIATION IS 1.08