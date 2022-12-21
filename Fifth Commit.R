################################################################################
#############################   CA3   ##########################################
################################################################################
###OPENING CSV FILE

dataset <- read.csv ("D:\\DKIT\\Data Analytics\\dataset.csv")
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

#INTERQUARTILE RANGE (IQR) AND QUARTILES
iqr_pre_or <- IQR (dataset$pre_trial_or) #finding IQR
iqr_pre_or #IQR IS 1.54

quartiles_pre_or <- quantile(dataset$pre_trial_or, prob=c(.25,.5,.75)) #finding quartiles
quartiles_pre_or #QUARTILES: Q1=5.40, Q2=6.04, Q3=6.95

boxplot (dataset$pre_trial_or, data=dataset, main='Box plot of pre trial OR scores', ylab='Pre trial OR scores') #box plot for pre trial OR scores

boxplot (dataset$pre_trial_or ~ dataset$gender, data=dataset, main='Pre trial OR scores by gender', ylab='Pre trial OR scores', xlab='Gender') #box plot for pre trial OR scores for each test group

#MINIMUM VALUE
min_pre_or <- min (dataset$pre_trial_or)
min_pre_or #MINIMUM IS 3.35

#MAXIMUM VALUE
max_pre_or <- max (dataset$pre_trial_or)
max_pre_or #MAXIMUM IS 8.99

###NORMALITY TESTS

#HYSTOGRAM
hist (dataset$pre_trial_or, col='steelBlue', main='Distribution of pre trial OR scores', xlab='Pre trial OR scores')
#the distribution doesn't seem normal. Further tests will be carried out.

#Q-Q PLOT
qqnorm(dataset$pre_trial_or, main='Distribution of pre trial OR scores')
qqline(dataset$pre_trial_or)
#the distribution doesn't seem normal. Further tests will be carried out.

#SHAPIRO-WILK TEST
shapiro.test (dataset$pre_trial_or)
#p = 0.83, which is higher that 0.05. Therefore, THE DISTRIBUTION IS NOT NORMAL.