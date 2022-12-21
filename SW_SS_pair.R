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

###NORMALISATION

log_scale = log(as.data.frame(dataset[6])) #normalising the data using logarithmic normalisation
log_scale
shapiro.test (log_scale$pre_trial_or) #testing if data have been normalised. DATA IS NOW NORMAL (P = 0.01)
dataset['pre_trial_or'] <- log_scale$pre_trial_or ##replacing new normalise data in the original dataset
dataset$pre_trial_or

###TESTING HYPOTHESYS

#INDEPENDENT SAMPLE T-TEST
t_test_results <- t.test (dataset$pre_trial_or ~ dataset$gender, data=dataset, var.equal=FALSE, na.rm=TRUE)
t_test_results #p < 0.01. H0 IS REJECTED AND H1 HAS TO BE ACCEPTED. THERE IS A SIGNIFICAND DIFFERENCE IN THE AVERAGE PRE TRIAL OR SCARES BETWEEN MALES AND FEMALES.
#t score = -3.12
#degrees of freedom (df) = 145.65
#Female mean = 1.75 and Male mean = 1.84. On average, males have a higher pre trial OR score than females.

###CONFIDENCE INTERVALS

#CONFIDENCE INTERVAL FOR pre_trial_or MEAN
n <- 150 #we know by the brief that there are 150 patients in the study
standardised_mean <- mean (dataset$pre_trial_or) #finding mean of pre_trial_or after normalisation
standardised_mean
standardised_sd <- sd (dataset$pre_trial_or) #finding the standard deviation of pre_trial_or after normalisation
standardised_sd
margin_error <- qt(0.95,df=n-1)*standardised_sd/sqrt(n) #calculating margin error
margin_error
lower_pop_interval <- standardised_mean - margin_error
lower_pop_interval #LOWER CONFIDENCE INTERVAL IS 1.77
upper_pop_interval <- standardised_mean + margin_error
upper_pop_interval #UPPER CONFIDENCE INTERVAL IS 1.82

#CONFIDENCE INTERVAL FOR DIFFERENCE IN MEANS BETWEEN FEMALES AND MALES
mean_female <- 1.75 #as seen in the t-test
mean_male <- 1.84 #as seen in t-test
aggregate (dataset$pre_trial_or, list (dataset$gender), FUN=sd) #finding pre_trial_or standard deviation for each gender
sd_female <- 0.17
sd_male <- 0.19
n_f <- 75 #we know from the brief that there are 75 females
n_m <- 75 #we know from the brief that there are 75 males
sp <- ((n_f-1)*sd_female^2+(n_m-1)*sd_male^2)/(n_f+n_m-2) #calculating pooled variance
sp
margin_error_difference <- qt(0.95,df=n_f+n_m-1)*sqrt(sp/n_f + sp/n_m)
margin_error_difference
lower_diff_interval <- (mean_female-mean_male) - margin_error_difference
lower_diff_interval #LOWER CONFIDENCE INTERVAL IS -0.14
upper_diff_interval <- (mean_female-mean_male) + margin_error_difference
upper_diff_interval #UPPER CONFIDENCE INTERVAL IS -0.04

#CONFIDENCE INTERVAL FOR pre_trial_or MEAN FOR FEMALES
margin_error_female <- qt(0.95,df=n_f-1)*sd_female/sqrt(n_f)
margin_error_female
lower_f_interval <- mean_female - margin_error_female
lower_f_interval #LOWER CONFIDENCE INTERVAL IS 1.72
upper_f_interval <- mean_female + margin_error_female
upper_f_interval #UPPER CONFIDENCE INTERVAL IS 1.78

#CONFIDENCE INTERVAL FOR pre_trial_or MEAN FOR MALES
margin_error_male <- qt(0.95,df=n_m-1)*sd_male/sqrt(n_m)
margin_error_male
lower_f_interval <- mean_male - margin_error_male
lower_f_interval #LOWER CONFIDENCE INTERVAL IS 1.80
upper_f_interval <- mean_male + margin_error_male
upper_f_interval #UPPER CONFIDENCE INTERVAL IS 1.88

###CORRELATION
#ESTABLISHING HYPOTHESIS
#H0: there is no correlation between pre and post OR values.
#H1: there is a significant correlation between pre and post OR values.

#DATA CLEANING
which (is.na (dataset$post_trial_or)) #NO MISSING VALUES
between (dataset$post_trial_or, 0, 10) #checking if all values are between 0 and 10. VALUE OUTISE OF RANGE AT LINE 125.
dataset$post_trial_or[126] #wrong value is 12. 
dataset$post_trial_or[126] <- 10 #replacing value with maximum value of the scale
dataset$post_trial_or[126] #checking if the value has changed

#DESCRIPTIVE STATISTICS

mean_post_or <- mean (dataset$post_trial_or)
mean_post_or #MEAN IS 5,43

median_post_or <- median (dataset$pre_trial_or)
median_post_or #MEDIAN IS 1.80 

mode_post_or <- getmode (dataset$post_trial_or)
mode_post_or #MODE IS 5.33

sd_post_or <- sd (dataset$post_trial_or)
sd_post_or #STANDARD DEVIATION IS 1.15

#INTERQUARTILE RANGE (IQR) AND QUARTILES
iqr_post_or <- IQR (dataset$post_trial_or) #finding IQR
iqr_post_or #IQR IS 1.51

quartiles_post_or <- quantile(dataset$post_trial_or, prob=c(.25,.5,.75)) #finding quartiles
quartiles_post_or #QUARTILES: Q1=4.73, Q2=5.33, Q3=6.25

boxplot (dataset$post_trial_or, data=dataset, main='Box plot of post trial OR scores', ylab='Post trial OR scores') #box plot for post trial OR scores

min_post_or <- min (dataset$post_trial_or)
min_post_or #MINIMUM IS 2.17

max_post_or <- max (dataset$post_trial_or)
max_post_or #MAXIMUM IS 10

hist (dataset$post_trial_or, col='steelBlue', main='Distribution of post trial OR scores', xlab='Post trial OR scores')
#the distribution doesn't seem normal. ut seems slightly right skewed. Further tests will be carried out.

#Q-Q PLOT
qqnorm(dataset$post_trial_or, main='Distribution of post trial OR scores')
qqline(dataset$post_trial_or)
#the distribution doesn't seem normal. Further tests will be carried out.

#SHAPIRO-WILK TEST
shapiro.test (dataset$post_trial_or)
#p = 0.047, which is slightly lower that 0.05. Therefore,  we will assume that THE DISTRIBUTION IS NORMAL. 

#CORRELATION
cor.test (dataset$pre_trial_or, dataset$post_trial_or, method = "pearson") #calculating Pearsosn's correlation
#CORRELATION COEFFICIENT IS 0.53 with p < 0.001. H0 REJECT IN FAVOUR OF H1. THERE IS A POSITIVE CORRELATION BEWEEN PRE AND POST TRIAL OR SCORES.

plot(dataset$pre_trial_cpss ~ dataset$post_trial_or,
     data = dataset,
     main = "Pre VS Post trial OR scores)",
     xlab = "Pre trial scores",
     ylab = "Post trial scores")











