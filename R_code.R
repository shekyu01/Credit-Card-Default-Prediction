#map working directory
path <- "../1.Projects/PD_EAD_LGD/Credit Scoring"

#set working directory
setwd(path)

#Load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")

head(train)
head(test)

                      #####################################
                      ##### Step1-Univariate Analysis######
                      #####################################

# Let's start by looking at the data type of each column
str(train)

# Here out of 11 var's 2 var's are continuous and 9 var' are 

#Let's separate continuous & categorical var's for univariate analysis

train_cont <- subset(train,select = c(Utilization,DebtRatio,Mon_Income,age))
train_cat <- subset(train,select = c(Deliquent,del_30_59,credit_lines,real_estate,del_60_89,No_Dependents,del_90))

# 1. Continuous Variables:In case of continuous variables, we generally focus on calculating measure of central tendency and spread of data such as Mean, Median, Range, IQR and Standard Deviation.

# Installing "pastecs" package to get summary statistics
install.packages ("pastecs")

#Loading pastecs library
library(pastecs)


options(scipen=100)
options(digits=2)

#Getting summary stat
stat.desc(train_cont)

#2. Categorical Variables:In case of categorical variables, we generally use frequency table to understand distribution of each category. It can be measured using two metrics, Count and Count% against each category.
#Before checking the count of categories, lets check the number of unique values in each categorical variable.

apply(train_cat,2,function(x){length(unique(x))})

#Print the counts of each category

#A.Analysis of Deliquent variable

table(train_cat$Deliquent)
as.matrix((prop.table(table(train_cat$Deliquent))))
# The number of defaulter instances in this variable is 10026 i.e. 6.68% among the all instances.

#B.Analysis of Age variable
table(train_cat$age)
as.matrix((prop.table(table(train_cat$age))))

#Population across diff bins of ages
#20-25(2%), 26-30(5%),..... and so on 

#C.Analysis of del_30_59 variable

table(train_cat$del_30_59)
as.matrix((prop.table(table(train_cat$del_30_59))))
# 85% population is current in our dataset rest 15% pop has been "faced ever 30-59 days past due"
# Similar analysis has been for del_60_89 & del_90


#D.Analysis of credit_lines variable

table(train_cat$credit_lines)
as.matrix((prop.table(table(train_cat$credit_lines))))
# Pop has been equally distributed across all the categories


#E.Analysis of No_Dependents variable

table(train_cat$No_Dependents)
as.matrix((prop.table(table(train_cat$No_Dependents))))
#~60% Population having no dependents & ~ 31% pop has 2-3 dependents

                  #####################################
                  ## Step2- Multivariate Analysis######
                  #####################################


#Multivariate Analysis finds out the relationship between two or more variables. Here, we look for association and disassociation between variables at a pre-defined significance level.
#The type of visualization technique to use depends on the type variable. Thus, there can be 3 combinations of the 2 types of variables:
#categorical - categorical
#continuous - continuous
#categorical - continuous

#1. Both Categorical:
# We need to install "gmodels"
install.packages("gmodels")
library(gmodels)


#Delinq*No_Dependents
#Syntax
#CrossTable(x, y, digits=3, max.width = 5, expected=FALSE, prop.r=TRUE, prop.c=TRUE,prop.t=TRUE, prop.chisq=TRUE, chisq = FALSE, fisher=FALSE, mcnemar=FALSE,resid=FALSE, sresid=FALSE, asresid=FALSE,missing.include=FALSE,format=c("SAS","SPSS"), dnn = NULL, ...)

CrossTable(train$No_Dependents,train$Deliquent,digits=2,prop.r=TRUE, prop.c=FALSE,prop.t=FALSE,format=c("SAS"))
#As u can see in freq table--- %(Default) increases as no. of dep increased

# We can perform same analysis for rest of the categorical variables

#2. Both continuous

#Scatter plot 

plot(train$Mon_Income, train$Utilization,xlab="Utilisation",ylab="Debt_Ratio")  
abline(lm(train$Mon_Income ~ train$Utilization))

#3. Categorical-Continuous Combination
#Create a Boxplot

boxplot(train$Utilization ~ train$Deliquent,xlab='Default status',ylab='Utilization rate')
#We can perform same analysis for rest of the other variables with Deliquent at one side

                  #####################################
                  ### Step3-Missing Value Treatment####
                  #####################################


#1. Checking missing values

#Checking no. of missing in complete dataset
table(is.na(train))

#Check no. of missing columnwise in train data
colSums(is.na(train))


#Check no. of missing columnwise in test data
colSums(is.na(test))

#In both test and train data set, we found missing values in 2 variables:
#Mon_Income (Continuous)
#No_Dependents (categorical)


#2. Imputation

#install "mlr(Multiple Linear Regression)" package
# For more details visit "https://cran.r-project.org/web/packages/mlr/mlr.pdf"
install.packages("mlr")

#Load package
library(mlr)

# Imputing No_Dependents by Mode 
#Syntax
#impute(data, target = character(0L), classes = list(), cols = list(),dummy.classes = character(0L), dummy.cols = character(0L),dummy.type = "factor", force.dummies = FALSE, impute.new.levels = TRUE,       recode.factor.levels = TRUE)

#Impute for train
imputed_data <- impute(train,cols = list(No_Dependents=imputeMode()))
train <-   imputed_data$data
colSums(is.na(train))

#Impute for test
imputed_test_data <- impute(test,cols = list(No_Dependents=imputeMode()))
test <-   imputed_test_data$data
colSums(is.na(test))


# Impute Mon_Income by Mean

#Impute for train
imputed_data <- impute(train,cols = list(Mon_Income=imputeMean()))
train <-   imputed_data$data
colSums(is.na(train))

#Impute for test
imputed_test_data <- impute(test,cols = list(Mon_Income=imputeMean()))
test <-   imputed_test_data$data
colSums(is.na(test))


                #####################################
                ###   Step4-Outlier Treatment    ####
                #####################################

#Install "outlier" package

#Decision Tree algorithm allows to deal with outliers well due to binning of variable. 
# So here I haven't deleted outliers
# To know how to remove outliers visit (http://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/)



              #####################################
              ### Step5-Variable Transformation ###
              #####################################

# Checking the class of all the var's

sapply(train,class)
#using recode function from "car" package

#Determine the %(obs) across Delq category
as.matrix(prop.table(table(train$del_30_59)))
as.matrix(prop.table(table(test$del_30_59)))

as.matrix(prop.table(table(train$del_60_89)))
as.matrix(prop.table(table(test$del_60_89)))

as.matrix(prop.table(table(train$del_90)))
as.matrix(prop.table(table(test$del_90)))


#Depending on the business scenario, we can combine the categories with very few observations. As a thumbrule, lets combine categories with less than 5% of the values.
# For more info plz refer (http://datahack.analyticsvidhya.com/workshop/experiments-with-data/slide?next=IF1LWDJ2778N)

install.packages("car")
library(car)

train$del_30_59 <- recode(train$del_30_59,"c('2',  '3',	'4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'96',	'98')='2'")
test$del_30_59 <- recode(test$del_30_59,"c('2',  '3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'96',	'98')='2'")


train$del_60_89 <- recode(train$del_60_89,"c('2',  '3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'96',	'98')='2'")
test$del_60_89 <- recode(test$del_60_89,"c('2',  '3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'96',	'98')='2'")


train$del_90 <- recode(train$del_90,"c('2',  '3',  '4',  '5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'96',	'98','14','15','17')='2'")
test$del_90 <- recode(test$del_90,"c('2',  '3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'96',	'98','14','15','17')='2'")

#Determine the %(obs) across Dependents category

as.matrix(prop.table(table(train$No_Dependents)))
as.matrix(prop.table(table(test$No_Dependents)))

train$No_Dependents <- recode(train$No_Dependents,"c('3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'13',	'20')='3'")
test$No_Dependents <- recode(test$No_Dependents,"c('3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'13',	'20','43')='3'")


#Determine the %(obs) across real_estate category
as.matrix(prop.table(table(train$real_estate)))
train$real_estate <- recode(train$real_estate,"c('3',	'4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'14',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'23',	'25',	'26',	'29',	'32',	'54')='3'")

as.matrix(prop.table(table(test$real_estate)))
test$real_estate <- recode(test$real_estate,"c('3',  '4',	'5',	'6',	'7',	'8',	'9',	'10',	'11',	'12',	'13',	'14',	'15',	'16',	'18',	'19',	'20',	'21',	'25',	'29',	'37')='3'")



#Determine the %(obs) across credit_lines category

as.matrix(prop.table(table(train$credit_lines)))
train$credit_lines <- recode(train$credit_lines,"c('10', '11',  '12',	'13',	'14',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'22',	'23',	'24',	'25',	'26',	'27',	'28',	'29',	'30',	'31',	'32',	'33',	'34',	'35',	'36',	'37',	'38',	'39',	'40',	'41',	'42',	'43',	'44',	'45',	'46',	'47',	'48',	'49',	'50',	'51',	'52',	'53',	'54',	'56',	'57',	'58')='10'")


as.matrix(prop.table(table(test$credit_lines)))
test$credit_lines <- recode(test$credit_lines,"c('10', '11',  '12',  '13',	'14',	'15',	'16',	'17',	'18',	'19',	'20',	'21',	'22',	'23',	'24',	'25',	'26',	'27',	'28',	'29',	'30',	'31',	'32',	'33',	'34',	'35',	'36',	'37',	'38',	'39',	'40',	'41',	'42',	'43',	'44',	'45',	'46',	'47',	'48',	'49',	'50',	'51',	'52',	'53',	'54',	'56',	'57',	'58','63','85')='10'")




                          #####################################
                          ### Step6-  Predictive Modeling   ###
                          #####################################

# We will use a decision tree algorithm for model building.
# Decision Tree is a powerful algorithm. It is capable of mapping non-linear relationships in the data sets even better than the generalized linear models.


#install the package and load it 

install.packages("rpart")
library(rpart)


#Building the model

set.seed(333)
train.fit1 <- rpart(train$Deliquent ~ ., data= train, method= "class", control= rpart.control(minsplit=20,
                                                                                              minbucket=100,
                                                                                              maxdepth=10,
                                                                                              xval=5))

print(train.fit1)
#Quick summary of these parameters 

#1 minsplit= refers to minimum no. of ob's which must exist in a node to split.
#2 minbucket= refers to minimum no. of ob's which must exist in a terminal node.
#3 maxdepth= refers to depth of the tree
#4 xval= refers to cross validation
#5 For more options visit (http://web.stanford.edu/class/stats315b/minitech.pdf)
# Let's see summary to check variable importance and other imp terms
# Summary function shows var importance, cp-table & the DT

summary(train.fit1)

# Let's plot the tree
library(rpart.plot)
rpart.plot(train.fit1)


#Predictions for train data
predictions_train <- predict(train.fit1,newdata=train, type="class")

#Predictions for test data
predictions_test <- predict(train.fit1,newdata=test, type="class")


#Analyze result

library(caret)
confusionMatrix(predictions_train,train$Deliquent)

# Create dataframe of final prediction
solution_frame <- data.frame(ID=test$ID,Deliquent=predictions_test)


#Writing the solution file
write.csv(solution_frame, file= "final_sol.csv")
