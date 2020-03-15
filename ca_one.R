#libraries
if (!require("rpart")) install.packages("rpart")
library(rpart)
if (!require("partykit")) install.packages("partykit")
library(partykit)
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)


#Convert the dataset into csv file, Read.csv tells R to read csv file.
Data <- read.csv('C:/Temp/input/ProjectData.csv', header=TRUE, sep= ';')

#remove the first column (ID):
Data=Data[,-1]

#Check distinct categories of Variables using STR function
str(Data)

#Make sure all the categorical variables are converted into factors. 
#The function rpart will run a regression tree if the response variable is numeric, 
#and a classification tree if it is a factor
cols <- c(1:9)
Data[cols] <- lapply(Data[cols], factor)

str(Data)

##################### Model 1 - Develop the DT model over Xs for all data #####################################

#build the DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data)#,  maxdepth=4) 
                                                           #, control=rpart.control(minsplit=60,
                                                           #   minbucket=30, 
                                                           #   maxdepth=4 ))

#?rpart
#minsplit: the minimum number of observations that must exist in a node for a new split
#minbucket: the minimum number of observations in any terminal <leaf> node
#Maxdepth: Maximum depth for any node, with the root node counted as depth 0.

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)


##################### Model 2 - Develop the DT model over Ys for all data #####################################

#build the DT model
DT_Model <-rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data) #,  maxdepth=4)

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 3 - Develop the DT model over Xs and Ys for all data ##############################

#remove column Group as it's not needed in this test
Data1 <- Data %>% select(-c(Group))

#build the model over all remaining variables
DT_Model <-rpart(Response~., data=Data1) #,  maxdepth=4)

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 4 - Develop the DT model over Xs for Group=0 #####################################

#select only rows where Group=0
Data2 <- Data [Data$Group==0,]

#build the DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data2)#,  maxdepth=4) 

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 5 - Develop the DT model over Ys for Group=0 #####################################

#build the DT model
DT_Model <-rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data2)#,  maxdepth=4)

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 6 - Develop the DT model over Xs and Ys for Group=0 ##############################

#remove column Group as it's not needed in this test
Data2 <- Data2 %>% select(-c(Group))

#build the model over all remaining variables
DT_Model <-rpart(Response~., data=Data2)#,  maxdepth=4)

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 7 - Develop the DT model over Xs for Group=1 #####################################

#select only rows where Group=1
Data3 <- Data [Data$Group==1,]

#build the DT model
DT_Model <-rpart(Response~X1+X2+X3+X4+X5+X6+X7, data=Data3)#,  maxdepth=4) 

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 8 - Develop the DT model over Ys for Group=1 #####################################

#build the DT model
DT_Model <-rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data3)#,  maxdepth=4)

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)

##################### Model 9 - Develop the DT model over Xs and Ys for Group=1 ##############################

#remove column Group as it's not needed in this test
Data3 <- Data3 %>% select(-c(Group))

#build the model over all remaining variables
DT_Model <-rpart(Response~., data=Data3)#,  maxdepth=4)

#Plot the tree
plot(as.party(DT_Model))
print(DT_Model)

#Procedure of Pruning

#The following line fitted tree's CP table (Matrix of Information on optimal pruning given Complexity Parameter). 
#Look where do you see the least error.
print(DT_Model$cptable)

#The line below automatically picks up the least error tree
opt <- which.min(DT_Model$cptable [, "xerror"])

#Pruning the tree to the least xerror
cp <- DT_Model$cptable [opt,"CP"]

#goes until reaching the cp found
DT_Model_pruned <- prune(DT_Model, cp=cp)
plot(as.party(DT_Model_pruned))
print(DT_Model_pruned)
