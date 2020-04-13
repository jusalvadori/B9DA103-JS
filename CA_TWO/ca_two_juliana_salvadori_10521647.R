# Data Mining - CA_TWO 
# Two-stage Missing Data Imputation
# Student: Julian Salvadori
# Student No: 10521647
#

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
if (!require("mice")) install.packages("mice")
library(mice)

rm(list=ls())
setwd("C:/Temp/input")

#Read the data and check the content.
Data <- read.csv("CA2Data.csv", header=TRUE, sep= ",")

#Select just some usefull variables
Data <- Data %>% select(c(Income, 
                          samptype,
                          borough,
                          sex,
                          r_age,
                          educ,
                          race,
                          single,
                          college,
                          workmos,
                          workhrs
                          ))
                          
str(Data)

#remove the Income column to allow us to use the mice package to impute missing data to all the 
#remaining variables, otherwise the mice method is going to impute missing values on Income column as well
Data1 <- Data %>% select(-c(Income))
Data2 <- Data

#impute missing data to all the remaining variables  
imputed = complete(mice(Data1,m=5,maxit=50,meth='pmm',seed=500))
#View(imputed)

Data <- imputed
Data$Income <- Data2$Income
#View(Data)

#########################################################################################################
#Step 1: The first step is to determine the missing values of “Income” that are actually equal to 0. 
#To do this, you are required to define a new variable, say “Binary_Income” with 0, 1 and missing values, 
#for the cases which have 0, non-zero and missing values in the “Income” variable, respectively. 
#########################################################################################################

Ind_Function <- function(u)
{
  x <- dim(length(u))
  x[which(u==0)] <- 0
  x[which(!u==0)] <- 1
  return(x)
}

Ind_Function2 <- function(u)
{
  x <- dim(length(u))
  x[which(is.na(u))] <- 0
  x[which(!is.na(u))] <- 1
  return(x)
}

#Generate the column using the function above for the variable Income: 
Data$Binary_Income <- Ind_Function(Data$Income)
#Generate a dummy column to help us to identify the predicted Income columns
Data$Dummy         <- Ind_Function2(Data$Income)
str(Data)

Data$Binary_Income <- as.factor(Data$Binary_Income)
Data$Dummy <- as.factor(Data$Dummy)
str(Data)

#View(Data)


#########################################################################################################
#Step 2: Then use the cases with “Binary_Income” equal to either 0 or 1, to develop a logistic regression
#model.  
#########################################################################################################

#remove column Income and select only non-null Binary_Income rows
Data1 <- Data[!is.na(Data$Binary_Income), ] %>% select(-c(Income, Dummy))
#View(Data1)

#Use a regression model for the Binary_Income using the all remaining variables as independent variables
fitmodel = glm (Binary_Income ~ ., data = Data1, family = 'binomial') # binomial(link=logit)) 
summary(fitmodel) 

pred = predict(fitmodel, type="response")
# confusion matrix
table_matrix = table(Data1$Binary_Income, pred >= 0.5)
table_matrix

# accuracy of the model
accuracy <- sum(diag(table_matrix)) / sum(table_matrix)
accuracy

prediction <- predict(fitmodel, newdata=Data, type = "response")
#View(prediction)


#########################################################################################################
#Step 3: The outcome of this model would allow you to predict the missing values in the “Binary_Income” 
#variable, as 0 or 1.
#########################################################################################################
for(i in 1:nrow(Data))
{
  if ( is.na(Data$Binary_Income[i]) )
    
  { Data$Binary_Income[i] = round(prediction[i], 0)
  }
}
#View(Data[is.na(Data$Income), ])  # 124 rows


#########################################################################################################
#Step 4: For any case which “Binary_Income” is predicted as 0, put the actual “Income” equal to 0 as well. 
#########################################################################################################

for(i in 1:nrow(Data))
{
  if ( Data$Binary_Income[i] == 0 )
  { 
    Data$Income[i] = 0
  }
}
#View(Data[is.na(Data$Income), ])  # 121 rows


#########################################################################################################
#Step 5: Those cases who their “Binary_Income” is predicted as 1, are used along with other cases with  
#nonzero “Income” to develop a linear regression model.
#########################################################################################################

#remove column Binary_Income and Dummy and select only rows where Binary_Income was predict as 1 and 
#Income is non-zero
Data2 <- Data[ Data$Binary_Income == 1, ] %>% select(-c(Binary_Income, Dummy))
#View(Data2)

#Use a regression model for the Income using the all remaining variables as independent variables
fitmodel = lm(Income ~ ., data=Data2)  # linear regression
fitsummary <- summary(fitmodel) 
fitsummary

fitsummary$r.squared 

#########################################################################################################
#Step 6:The outcome of such model would allow you to predict the missing values for the “Income” variable, 
#for the cases which their “Binary_Income” variable is predicted as 1 and therefore are expected to have 
#non-zero “Income”.  
#########################################################################################################

#verify the significant variables
alpha = 0.05
beta_values = NULL
for (i in 1:length(Data2))
{
  if (fitsummary$coefficients[i,4] > alpha )
  { beta_values[i] = 0  }
  else
  { 
    if ( i == 1) # intercept
     { beta_values[i] = 1
     }else {
       beta_values[i] = fitsummary$coefficients[i,1]
     }
  } 
}

#impute missing values
for(i in 1:nrow(Data))
{
  if ( is.na(Data$Income[i]) )
  { 
     Data$Income[i] = beta_values[1]                     +
                      beta_values[2]  * Data$samptype[i] +
                      beta_values[3]  * Data$borough[i]  +
                      beta_values[4]  * Data$sex[i]      +
                      beta_values[5]  * Data$r_age[i]    +
                      beta_values[6]  * Data$educ[i]     +
                      beta_values[7]  * Data$race[i]     +
                      beta_values[8]  * Data$single[i]   +
                      beta_values[9]  * Data$college[i]  +
                      beta_values[10] * Data$workmos[i]  +
                      beta_values[11] * Data$workhrs[i] 
     
    #check for negative Income an set to zero
    if (Data$Income[i] < 0)
    { Data$Income[i] = 0 }
  }
}

#########################################################################################################
#Step 7: The number of cases with predicted “Binary_Income” as either 0 or 1. 
#########################################################################################################

predicted_zero = Data[ Data$Binary_Income == 0 & Data$Dummy == 0, ]
nrow(predicted_zero)
predicted_one  = Data[ Data$Binary_Income == 1 & Data$Dummy == 0, ]
nrow(predicted_one)


#########################################################################################################
#Step 8: The Mean and SD for the non-zero predicted “Income”. 
#########################################################################################################

# all data
mean = mean(Data$Income[Data$Income > 0])
mean
sd   = sd(Data$Income[Data$Income > 0])
sd

# only predicted rows
mean = mean(Data$Income[Data$Income > 0 & Data$Dummy == 0])
mean
sd   = sd(Data$Income[Data$Income > 0 & Data$Dummy == 0])
sd

