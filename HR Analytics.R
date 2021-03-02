## loading the necessary libraries

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(e1071)
library(randomForest)
library(caret)
library(dummies)
library(caTools)
library(car)



# load the datasets
in_time <- read.csv('in_time.csv',header = T,stringsAsFactors = F)
out_time <- read.csv('out_time.csv',header = T,stringsAsFactors = F)
employee_survey <- read.csv('employee_survey_data.csv',stringsAsFactors = F)
manager_survey <- read.csv('manager_survey_data.csv',stringsAsFactors = F)
general <- read.csv('general_data.csv',stringsAsFactors = F)



## Checking if all employee IDs present in the files match 

setdiff(general$EmployeeID,employee_survey$EmployeeID)
setdiff(general$EmployeeID,manager_survey$EmployeeID) 
setdiff(general$EmployeeID,in_time$X) 
setdiff(general$EmployeeID,out_time$X) 


#########################
## FEATURE ENGINEERING ##
#########################

# 1. Counting Leaves taken by each employee from in_time 
#    Punch in time will be considered 0 and NAs is in_time are denoted by 1, considering them leaves 
#    Also, here are some days with all NAs. That could be a public holiday.  

leaves <- ifelse(is.na(in_time[,-1]),1,0)
leaves <- as.data.frame(leaves)

#    Now, the public holidays are removed.... 
#    As NAs are "1" now, so holidays are the one where column sum is equal to the total rows in data. 

leaves <- leaves[,which(colSums(leaves)!=4410)]

#    Now, total leaves of particular employee is the row sum of that row i.e. "X" (here X is emp.ID)
#    So, Creating new DF
employee_leaves <- data.frame(Employee_ID = in_time$X , total_leaves = (rowSums(leaves)))


# 2. Getting total working hours of employees
#    This is out_time - in_time
#    First need to check whether these both datasets are even similar or not.

identical(colnames(in_time$X), colnames(out_time$X)) 

# excluding public holidays from in.time and out.time
in_time <- in_time[, which(colSums(is.na(in_time)) !=4410)]

out_time <- out_time[, which(colSums(is.na(out_time)) !=4410)]


in_time[,-1] <- sapply(in_time[,-1], ymd_hms) 
out_time[,-1] <- sapply(out_time[,-1], ymd_hms)
working_hours <- (out_time[,-1] - in_time[,-1]) # in seconds
working_hours <- working_hours/3600 # in hrs

# Average daily working hours
employee_hours <- data.frame(EmployeeID=in_time$X, 
                      Avg.hrs = round(rowMeans(working_hours, na.rm = T),2))

str(employee_hours)
str(employee_leaves)
colnames(employee_leaves)[1]='EmployeeID'
merged_WorkingHours_leaves=merge(employee_hours,employee_leaves,by='EmployeeID')

######################
## MERGING DATASETS ##
######################

## Merging all the files to form a single final file
merge1 <- merge(x=employee_survey,y= general,by = "EmployeeID")
merge2 <- merge(x= merge1,y= manager_survey,by = "EmployeeID")
employee_final <- merge(x= merge2,y=merged_WorkingHours_leaves, by="EmployeeID")

# Assigning the values given in data dictionary
str(employee_final)

employee_final$EnvironmentSatisfaction=if_else(employee_final$EnvironmentSatisfaction==1,'Low'
                                               ,if_else(employee_final$EnvironmentSatisfaction==2,'Medium'
                                                        ,if_else(employee_final$EnvironmentSatisfaction==3,'High','Very High')))

employee_final$JobSatisfaction=if_else(employee_final$JobSatisfaction==1,'Low'
                                       ,if_else(employee_final$JobSatisfaction==2,'Medium'
                                                ,if_else(employee_final$JobSatisfaction==3,'High','Very High')))

employee_final$WorkLifeBalance=if_else(employee_final$WorkLifeBalance==1,'Bad'
                                       ,if_else(employee_final$WorkLifeBalance==2,'Good'
                                                ,if_else(employee_final$WorkLifeBalance==3,'Better','Best')))

employee_final$Education=if_else(employee_final$Education==1,'Below College'
                                 ,if_else(employee_final$Education==2,'College'
                                          ,if_else(employee_final$Education==3,'Bachelor'
                                                   ,if_else(employee_final$Education==4,'Master','Doctor'))))


employee_final$JobInvolvement=if_else(employee_final$JobInvolvement==1,'low'
                                      ,if_else(employee_final$JobInvolvement==2,'Medium'
                                               ,if_else(employee_final$JobInvolvement==3,'High','Very High')))


###################
## DATA CLEANING ##
###################

str(employee_final)

##checking for NA values
sum(is.na(employee_final))
# 111 NA values
# checking for blank values
colSums(employee_final==""|employee_final==" ",na.rm = T)
# No blank values

# checking for missing values in dataset
colSums(is.na(employee_final))
# checking in percentage
round((colSums(is.na(employee_final)))/nrow(employee_final)*100,2)
# 1. EnvironmentSatisfaction - 0.57% missing values
# 2. JobSatisfaction - 0.45% missing values
# 3. WorkLifeBalance - 0.86% missing values
# 4. NumCompaniesWorked - 0.43% missing values
# 5. TotalWorkingYears - 0.20% missing values

# As the NAs are not even 1 percent, we can ignore/remove them.
employee_final=employee_final[complete.cases(employee_final),]
sum(is.na(employee_final))
str(employee_final)

employee_final <- mutate_if(employee_final,is.character,tolower)

#########################
## UNIVARIATE ANALYSIS ##
#########################

# first for categorical variables

#Creating common function for univariate cat analysis

univariate_categorical <- function(variable,variable_name){
  
  ggplot(employee_final,aes(x = variable, y = ..count.. / sum(..count..))) + 
    geom_bar() + labs(x = variable_name, y = "Percent", title  = variable_name)+
    scale_y_continuous(labels = percent)
  }

table(employee_final$EnvironmentSatisfaction)  
univariate_categorical(employee_final$EnvironmentSatisfaction,"Environment Satisfaction")

table(employee_final$JobSatisfaction)  
univariate_categorical(employee_final$JobSatisfaction,"Job satisfaction")

table(employee_final$WorkLifeBalance)  
univariate_categorical(employee_final$WorkLifeBalance,"Work life Balance")

table(employee_final$Attrition)  
univariate_categorical(employee_final$Attrition,"Attrination")

table(employee_final$BusinessTravel)  
univariate_categorical(employee_final$BusinessTravel,"Buisness Tarvel")

table(employee_final$Department)  
univariate_categorical(employee_final$Department,"Department")

table(employee_final$Education)  
univariate_categorical(employee_final$Education,"Education")

table(employee_final$EducationField)  
univariate_categorical(employee_final$EducationField,"Education Field")

table(employee_final$EmployeeCount)  
#just 1 value, so removing Employee Count
employee_final$EmployeeCount <- NULL

table(employee_final$Gender)  
univariate_categorical(employee_final$Gender,"Gender")

table(employee_final$JobLevel)  
univariate_categorical(employee_final$JobLevel,"Job Level")

table(employee_final$JobRole)  
univariate_categorical(employee_final$JobRole,"Job Role")

table(employee_final$MaritalStatus)  
univariate_categorical(employee_final$MaritalStatus,"Marital")

table(employee_final$NumCompaniesWorked)  
univariate_categorical(employee_final$NumCompaniesWorked,"Number of companies worked")

table(employee_final$Over18)  
#just 1 value, so removing Over 18 column
employee_final$Over18 <- NULL

table(employee_final$StandardHours)  
#just 1 value, so removing Standard hours column
employee_final$StandardHours <- NULL

table(employee_final$StockOptionLevel)  
univariate_categorical(employee_final$StockOptionLevel,"Stock Option Level")

table(employee_final$TrainingTimesLastYear)  
univariate_categorical(employee_final$TrainingTimesLastYear,"Training time last year")

table(employee_final$JobInvolvement)  
univariate_categorical(employee_final$JobInvolvement,"Job Invovlvment")

table(employee_final$PerformanceRating)  
univariate_categorical(employee_final$PerformanceRating,"Performance rating")


# Univariate analysis for countinuous variables

hist(employee_final$Age)
hist(employee_final$DistanceFromHome)
hist(employee_final$MonthlyIncome)
hist(employee_final$PercentSalaryHike)
hist(employee_final$TotalWorkingYears)
hist(employee_final$YearsAtCompany)
hist(employee_final$YearsSinceLastPromotion)
hist(employee_final$YearsWithCurrManager)
hist(employee_final$Avg.hrs)
hist(employee_final$total_leaves)


str(employee_final)

########################
## BIVARIATE ANALYSIS ##
########################

# creating function for Bivariate Categorical data

bivariate_cat_Analysis <-  function(variable,variable_name){
  ggplot(employee_final,aes(x=factor(variable),fill=factor(Attrition)))+geom_bar(position = "fill")+
    ggtitle(variable_name)
  }

bivariate_cat_Analysis(employee_final$EnvironmentSatisfaction, "Environment Satisfaction vs Attrition")
#higher attrition in employees having low environment satisfaction

bivariate_cat_Analysis(employee_final$JobSatisfaction, "Job Satisfaction vs Attrition")
#higher attrition in employees having low job satisfaction

bivariate_cat_Analysis(employee_final$WorkLifeBalance, "Work Life Balance vs Attrition")
#higher attrition in employees having bad work life balance

bivariate_cat_Analysis(employee_final$BusinessTravel, "Business Travel vs Attrition")
# Higher attrition among employees who travel frequently

bivariate_cat_Analysis(employee_final$Department, "Department vs Attrition")
# Higher attrition among employees from HR Department

bivariate_cat_Analysis(employee_final$Education, "Education vs Attrition")
#almost all level of education except college has same attrition

bivariate_cat_Analysis(employee_final$EducationField, "Education Field vs Attrition")
# Education field 'Human resource' has highest attrition

bivariate_cat_Analysis(employee_final$Gender, "Gender vs Attrition")
# Almost same level of attrition

bivariate_cat_Analysis(employee_final$JobLevel, "Job Level vs Attrition")
# Almost all job level has same lavel of attrition

bivariate_cat_Analysis(employee_final$JobRole, "Job Role vs Attrition")
# research scientist and research director has more attrition

bivariate_cat_Analysis(employee_final$MaritalStatus, "Marital Status vs Attrition")
# Singles are more likely to leave

bivariate_cat_Analysis(employee_final$NumCompaniesWorked, "Number of companies worked vs Attrition")
# Persons who has work for 5,6,7,9 has more chance to leave the company while who has worked for 1 company also compared to others

bivariate_cat_Analysis(employee_final$StockOptionLevel, "Stock Option Level vs Attrition")
# Almost same attrition

bivariate_cat_Analysis(employee_final$TrainingTimesLastYear, "Training time last year vs Attrition")
# Ones who have been trained 6 times are very less likely to attrinate

bivariate_cat_Analysis(employee_final$JobInvolvement, "Job Involvement vs Attrition")
# low job involvemnet has high attrition compared to others

bivariate_cat_Analysis(employee_final$PerformanceRating, "Performance Rating vs Attrition")
# almost same attrition

# Now function for bivariate numeric variables

bivariate_numeric_analysis <-  function(variable,variable_name)
{
  ggplot(employee_final,aes(x=variable,fill=factor(Attrition)))+geom_histogram(binwidth = 5,color="black") + ggtitle(variable_name)
  
}

bivariate_numeric_analysis(employee_final$Age, "Age Vs Attrition")
bivariate_numeric_analysis(employee_final$DistanceFromHome, "Distance From Home Vs Attrition")
bivariate_numeric_analysis(employee_final$PercentSalaryHike, "Percent Salary Hike Vs Attrition")
bivariate_numeric_analysis(employee_final$TotalWorkingYears, "Total Working Hours Vs Attrition")
bivariate_numeric_analysis(employee_final$YearsAtCompany, "Years At Company Vs Attrition")
bivariate_numeric_analysis(employee_final$YearsSinceLastPromotion, "Years since last promotion Vs Attrition")
bivariate_numeric_analysis(employee_final$YearsWithCurrManager, "Years with current manager Vs Attrition")
bivariate_numeric_analysis(employee_final$Avg.hrs, "Average Hours Vs Attrition")
bivariate_numeric_analysis(employee_final$total_leaves, "Total leaves Vs Attrition")

# for Monthly Income
ggplot(employee_final,aes(x=employee_final$MonthlyIncome,fill=factor(Attrition)))+geom_histogram(binwidth = 10000,color="black") + ggtitle("Monthly Income Vs Attrition")

str(employee_final)
employee_final <- mutate_if(employee_final,is.character,as.factor)
employee_final$NumCompaniesWorked <- as.factor(employee_final$NumCompaniesWorked)
employee_final$StockOptionLevel <- as.factor(employee_final$StockOptionLevel)
employee_final$JobLevel <- as.factor(employee_final$JobLevel)
employee_final$TrainingTimesLastYear <- as.factor(employee_final$TrainingTimesLastYear)
employee_final$PerformanceRating <- as.factor(employee_final$PerformanceRating)


#######################
## OUTLIER TREATMENT ##
#######################


# Creating function to find outliers 

Outlier <- function(variable){
  boxplot(variable)
  plot(quantile(variable,seq(0,1,0.01),na.rm = T))
quantile(variable,seq(0,1,0.01),na.rm = T)
}

Outlier(employee_final$Age)                      # No outlier
Outlier(employee_final$DistanceFromHome)         # No outlier
Outlier(employee_final$MonthlyIncome)            # No outlier      
Outlier(employee_final$PercentSalaryHike)        # No outlier
Outlier(employee_final$Avg.hrs)                  # No outlier
Outlier(employee_final$total_leaves)             # No outlier
Outlier(employee_final$YearsSinceLastPromotion)  # No outlier


Outlier(employee_final$TotalWorkingYears)
employee_final$TotalWorkingYears[employee_final$TotalWorkingYears>35] <- 35

Outlier(employee_final$YearsAtCompany)
employee_final$YearsAtCompany[employee_final$YearsAtCompany>27] <- 27

Outlier(employee_final$YearsWithCurrManager)
employee_final$YearsWithCurrManager[employee_final$YearsWithCurrManager>15] <- 15


#############
## Scaling ##
#############

str(employee_final)
emp_ID_final <- (employee_final$EmployeeID)

employee_final <- mutate_if(employee_final,is.numeric,scale)
employee_final$EmployeeID <- emp_ID_final

#############################################################################################
##                                  MODEL BUILDING                                         ##     
#############################################################################################


##  have tried 3 Different models
#1. Logistic regression model
#2. Random Forest model
#3. Support Vector Machine(SVM) model



###############################
## Logistic Regression Model ##
###############################

#############################
## Dummy Variable creation ##
#############################

employee_model_log <- dummy.data.frame(employee_final[,-1])
employee_model_log$Attritionno <- NULL
employee_model_log$Attrition <- employee_model_log$Attritionyes
employee_model_log$Attritionyes <- NULL


set.seed(123)
index <- sample.split(employee_model_log$Attrition,SplitRatio = 0.75)
trn_log <- employee_model_log[index,]
val_log <- employee_model_log[!index,]

prop.table(table(employee_model_log$Attrition))*100 # ratios just chcked weather ratio is maintained with origional data
prop.table(table(trn_log$Attrition))*100
prop.table(table(val_log$Attrition))*100




# Model Initiation

log_1 <- glm(Attrition~.,data = trn_log,family = 'binomial')

# Step Wise reduction
log_2 <- step(log_1,direction = 'both')
summary(log_2)


# Based on P value, removed the following variables

# EnvironmentSatisfactionhigh 
# Educationcollege  
# Educationdoctor 
# EducationFieldother 
# Genderfemale 
# JobLevel3
# JobLevel4 
#`JobRolelaboratory technician`
# StockOptionLevel1 
# JobInvolvementlow

log_3 <- glm(formula = Attrition ~ EnvironmentSatisfactionlow + EnvironmentSatisfactionmedium + 
               JobSatisfactionhigh + JobSatisfactionlow + JobSatisfactionmedium + 
               WorkLifeBalancebad + WorkLifeBalancebetter + Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmenthuman resources` + 
               JobLevel1 + JobLevel2 + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked0 + NumCompaniesWorked1 + NumCompaniesWorked2 + 
               NumCompaniesWorked3 + NumCompaniesWorked4 + NumCompaniesWorked6 + 
               NumCompaniesWorked8 + TotalWorkingYears + TrainingTimesLastYear0 + 
               TrainingTimesLastYear1 + TrainingTimesLastYear2 + TrainingTimesLastYear3 + 
               TrainingTimesLastYear4 + TrainingTimesLastYear5 + YearsSinceLastPromotion + 
               YearsWithCurrManager + Avg.hrs, family = "binomial", data = trn_log)

summary(log_3)

#check multicolinearity

sort(vif(log_3))

# Based on Vif and P value, removed the following variables
# TrainingTimesLastYear1
# TrainingTimesLastYear2
# TrainingTimesLastYear4
# EnvironmentSatisfactionmedium 
# JobLevel1 

log_4 <- glm(formula = Attrition ~ EnvironmentSatisfactionlow  + 
               JobSatisfactionhigh + JobSatisfactionlow + JobSatisfactionmedium +
               WorkLifeBalancebad + WorkLifeBalancebetter + Age + `BusinessTravelnon-travel` + 
               BusinessTraveltravel_frequently + `Departmenthuman resources` + 
               JobLevel2 + `JobRolemanufacturing director` + 
               `JobRoleresearch director` + `JobRoleresearch scientist` + 
               `JobRolesales executive` + MaritalStatusdivorced + MaritalStatusmarried + 
               NumCompaniesWorked0 + NumCompaniesWorked1 + NumCompaniesWorked2 + 
               NumCompaniesWorked3 + NumCompaniesWorked4 + NumCompaniesWorked6 + 
               NumCompaniesWorked8 + TotalWorkingYears + TrainingTimesLastYear0 +
               TrainingTimesLastYear2 + TrainingTimesLastYear3 + YearsSinceLastPromotion + 
               YearsWithCurrManager + Avg.hrs, family = "binomial", data = trn_log)


summary(log_4)
sort(vif(log_4))

val_log$pred=predict(log_3,newdata = val_log,type = 'response') #output is logged of survived.. to tackle it is done response

summary(val_log$pred)

pred_sur <- as.factor(ifelse(val_log$pred>=0.155,1,0))



confusionMatrix(pred_sur,as.factor(val_log$Attrition),positive = "1") 

# CONCLUSION from logistic 
# Our Logistic Regression model is able to predict with 72-74% accuracy.
# We get variable importance


#########################
## RANDOM FOREST MODEL ##
#########################



set.seed(100)
index <- sample.split(employee_final$Attrition,SplitRatio = 0.75)
trn_rf <- employee_final[index,]
val_rf <- employee_final[!index,]

prop.table(table(employee_final$Attrition))*100
prop.table(table(trn_rf$Attrition))*100
prop.table(table(val_rf$Attrition))*100

rf_model <- randomForest(Attrition ~ ., data = trn_rf, ntree = 1000, do.trace = T)
pred_hd <- predict(rf_model, newdata = val_rf, type = 'prob') #prob  to assign cut off

summary(pred_hd)

pred_HD <- as.factor(ifelse(pred_hd[,2]>0.29,"yes","no"))

confusionMatrix(pred_HD, as.factor(val_rf$Attrition), positive = "yes")

# Variable importance
varImpPlot(rf_model)

# CONCLUSION from Random Forest 
# Our Random Forest model is able to predict with amazing 98-99% accuracy.
# Far better than Logistic model
# Also, we get variable importance 

###############
## SVM Model ##
###############

employee_model_SVM <- dummy.data.frame(employee_final[,-1])
employee_model_SVM$Attritionno <- NULL
employee_model_SVM$Attrition <- employee_model_SVM$Attritionyes
employee_model_SVM$Attritionyes <- NULL
str(employee_model_SVM)
employee_model_SVM$Attrition <- as.factor(employee_model_SVM$Attrition)

set.seed(100)
index <- sample.split(employee_model_SVM$Attrition,SplitRatio = 0.75)
trn_SVM <- employee_model_SVM[index,]
val_SVM <- employee_model_SVM[!index,]

prop.table(table(employee_model_SVM$Attrition))*100 # ratios just chcked weather ratio is maintained with origional data
prop.table(table(trn_SVM$Attrition))*100
prop.table(table(val_SVM$Attrition))*100


#Linear
str(employee_model_SVM)
svm_model_lin <- svm(Attrition ~ ., data = trn_SVM,
                     kernel = "linear", scale = F, probability = TRUE)
svm_pred_lin <- predict(svm_model_lin, newdata = val_SVM, probability = TRUE)
prob_lin <- attr(svm_pred_lin,"probabilities")
summary(prob_lin)

#setting cut off
svm_lin_set <- as.factor(ifelse(prob_lin[,2] > 0.17, "1","0"))


confusionMatrix(svm_lin_set,(val_SVM$Attrition), positive = "1")


#Radial
str(employee_model_SVM)
svm_model_rad <- svm(Attrition ~ ., data = trn_SVM,
                     kernel = "radial", scale = F, probability = TRUE)
svm_pred_rad <- predict(svm_model_rad, newdata = val_SVM, probability = TRUE)
prob_rad <- attr(svm_pred_rad,"probabilities")
summary(prob_rad)

#setting cut off
svm_rad_set <- as.factor(ifelse(prob_rad[,2] > 0.155, "1","0"))

confusionMatrix(svm_rad_set,(val_SVM$Attrition), positive = "1")

#Polynomial

str(employee_model_SVM)
svm_model_pol <- svm(Attrition ~ ., data = trn_SVM,
                     kernel = "polynomial", scale = F, probability = TRUE)
svm_pred_pol <- predict(svm_model_pol, newdata = val_SVM, probability = TRUE)
prob_pol <- attr(svm_pred_pol,"probabilities")
summary(prob_pol)

#setting cut off
svm_pol_set <- as.factor(ifelse(prob_pol[,2] > 0.145, "1","0"))

confusionMatrix(svm_pol_set,(val_SVM$Attrition), positive = "1")

#Sigmoid

str(employee_model_SVM)
svm_model_sig <- svm(Attrition ~ ., data = trn_SVM,
                     kernel = "sigmoid", scale = F, probability = TRUE)
svm_pred_sig <- predict(svm_model_sig, newdata = val_SVM, probability = TRUE)
prob_sig <- attr(svm_pred_sig,"probabilities")
summary(prob_sig)

#setting cut off
svm_sig_set <- as.factor(ifelse(prob_sig[,2] > 0.17, "1","0"))

confusionMatrix(svm_sig_set,(val_SVM$Attrition), positive = "1")

# CONCLUSION from SVM 
# Our SVM model is able to predict between.
#1. Linear -  around 74%
#2. Radial -  around 82%
#3. Polynomial - around 90%
#4. Sigmoid - around 72%

# Its clearly observed that in "'POLYNOMIAL' - Kernel" in SVM model is giving maximum prediction 
# Accuracy is better from logistic regression, but as we cannot see geometry, finding appropriate classifier is hard
# Cannot check variable importance in SVM


############################################
## OVERALL CONCLUSION and RECOMMENDATIONS ##
############################################

#   Conclusion:
#   After studying all three models, the best model turned out to be the Random Forest model,
#   which gave an amazing 99% accuracy in predicting the Attrition.After this, 
#   SVM model gave an accuracy of 90% with "Polynomial" Kernel, 
#   followed by the logistic regression model (accuracy around 75%).  

#   Recommendations:
#1. Working environment should be improved. 
#2. Company should takes some actions to improve work life balance of employees as Working hrs of a person is playing major role. 
#3. Employees should get regular breaks. 
#4. Managers should not be changed very frequently.
#5. For Frequently traveling employees, company should take some steps to keep them in our company.
#6. HR activities like quizes and indoor games should be promoted.