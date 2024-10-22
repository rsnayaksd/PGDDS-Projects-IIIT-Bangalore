###################################################################
# Submission Description : Support Vector Machines - Assignment   #
# Name                   : Ramashankar Nayak                      #
# Roll Number            : DDA1720008                             #
###################################################################

################## Handwritten Digit Recognitions #################
#                                                                 #
# 1. Business Understanding                                       #
# 2. Data Understanding                                           #
# 3. Data Preparation                                             #
# 4. Model Building                                               #
#    4.1 Linear kernel                                            #
#    4.2 RBF Kernel                                               #
# 5. Hyperparameter tuning and cross validation                   #
# 6. Conclusion                                                   #
#                                                                 #
###################################################################

# 1. Business Understanding: 

# The objective is to develop a model using Support Vector Machine which should correctly 
# classify the handwritten digits based on the pixel values given as features

#####################################################################################

# 2. Data Understanding: 
# MNIST Dataset
# Number of Instances in Training Dataset : 60,000
# Number of Instances in Test Dataset : 20,000
# Number of Attributes : 785 
# The labels values are 0 to 9 and this column has been renamed as number.
# Pixels are organized row-wise. Pixel values are 0 to 255. 0 means background (white), 
# 255 means foreground (black). 

# 3. Data Preparation: 

#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("caTools") 

#Loading Neccessary libraries

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(caTools)

#Loading Data

# setwd("E:/Nayak/Learning/PGDDS/Course4/SVM Assignment/SVM Dataset")

train_Data <- read.csv("mnist_train.csv",header=FALSE)
test_Data <- read.csv("mnist_test.csv",header=FALSE)

#Understanding Dimensions

dim(train_Data)
dim(test_Data)

#Structure of the dataset

str(train_Data)
str(test_Data)

#Changing the column name of the first column for both training and test dataset

colnames(train_Data)[1] <- "number"
colnames(test_Data)[1] <- "number"

#printing first few rows for training dataset

head(train_Data) 

#Exploring the data

summary(train_Data)

#checking missing values for training and test dataset

sapply(train_Data, function(x) sum(is.na(x)))
sapply(test_Data, function(x) sum(is.na(x)))

#Result: It is observeed there are no missing values in training and test datasets

#Making our target class to factor by changing first column i.e.
#number column to factors for both training and test data sets

train_Data$number <- factor(train_Data$number)
test_Data$number <- factor(test_Data$number)

# Checking count of Digits in both datasets using summary
summary(train_Data$number)
summary(test_Data$number)

# Let's use set.seed to get the same training set for building the model further

set.seed(100)

# As we have 60000 datapoints in the training datasets, so to reduce the model evaluation time,
# Split the training dataset by taking 5% i.e. 3000 data points of it and evaluate the final model on complete test dataset

train.indices = sample(1:nrow(train_Data), 0.05*nrow(train_Data))
train = train_Data[train.indices, ] 

# 4. Model Building 
#    4.1 Linear kernel

# Building an standard SVM model using Linear Kernel
Model_linear <- ksvm(number~., data = train, scale = FALSE, kernel = "vanilladot")

#There are some warning because of scaling issue that can be ignored.

#Checking the performance of the above svm model for training dataset
Eval_linear_train <- predict(Model_linear, train_Data)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear_train,train_Data$number)

# Training dataset Accuracy : 0.9087
# Sensitivity           0.97164
# Specificity           0.99329

# Checking the performance of the above svm model for testing dataset
Eval_linear_test <- predict(Model_linear, test_Data)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear_test,test_Data$number)

# Training dataset Accuracy : 0.9071 
# Sensitivity            0.9755
# Specificity            0.9924


# 4.2 RBF Kernel
# Using NON-LINEAR (RBF - Kernel) SVM for building the model

# Checking if using RBF kernel increases the model accuracy

Model_RBF <- ksvm(number~ ., data = train, scale = FALSE, kernel = "rbfdot")

# Note : There are some warning because of scaling issue. But we can ignore them.

# Checking the performance of the above non linear svm model for training dataset
Eval_RBF_train <- predict(Model_RBF, train_Data)

# Confusion Matrix - Linear Kernel
confusionMatrix(Eval_RBF_train,train_Data$number)

# Training dataset Accuracy : 0.939
# Sensitivity           0.97856
# Specificity           0.99636

# Checking the performance of the above non linear svm model for test dataset
Eval_RBF_test <- predict(Model_RBF, test_Data)


#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF_test,test_Data$number)

# Training dataset Accuracy : 0.938
# Sensitivity         0.9827
# Specificity         0.9951

# Conclusion from above two models:
# It seems the RBF model is performing better than Linear kernel svm model
# we can further tune non-linear model through cross validation for better results.

# 5 Hyperparameter tuning and Cross Validation

# We will use the train function from caret package to perform Cross Validation. 

# traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 3 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=3)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Without grid
fit.svm <- train(number~., data=train, method="svmRadial", metric=metric, 
                 trControl=trainControl)

print(fit.svm)

# In order for faster computation, we are keeping limited levels in grids and cross validation

# We are doing cross validation for RBF Model with below:
# Cross Validation folds = 3
# Range of sigma = 0.63e-7, 1.63e-7, 2.63e-7
# Range of C = 1 2 3
# Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(70)

grid <- expand.grid(.sigma = c(0.63e-7,1.63e-7,2.63e-7),.C=c(1,2,3))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
#trcontrol = Our traincontrol method

fit.svm <- train(number~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# In order to get granular analysis, we can consider increasing the level in sigma & C for more fine
# analysis.

print(fit.svm)

#Final Model Result
#Support Vector Machines with Radial Basis Function Kernel 

# 3000 samples
# 784 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
#

       #No pre-processing
#Resampling: Cross-Validated (3 fold) 
#Summary of sample sizes: 2000, 2001, 1999 
#Resampling results across tuning parameters:
#  
# sigma     C  Accuracy   Kappa    
# 6.30e-08  1  0.9100013  0.8999707
# 6.30e-08  2  0.9219993  0.9133104
# 6.30e-08  3  0.9263296  0.9181253
# 1.63e-07  1  0.9323330  0.9247972
# 1.63e-07  2  0.9416673  0.9351702
# 1.63e-07  3  0.9429990  0.9366508
# 2.63e-07  1  0.9410006  0.9344329
# 2.63e-07  2  0.9480013  0.9422111
# 2.63e-07  3  0.9476670  0.9418402
#
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 2.63e-07 and C = 2.

# Plotting the final model for Accuracy(Cross-Validation)

plot(fit.svm)

# We see that accuracy is highest for a model having sigma = 2.63e-07 and C = 2
# Lets build a model with sigma = 2.63e-07 and C = 2

final_model <- ksvm(number~., data=train, scale=FALSE, kernel="rbfdot", C=2, kpar=list(sigma=2.63e-7))

final_model

# Hyperparameter : sigma =  2.63e-07 
# Number of Support Vectors : 1764
# cost C = 2

# Checking training accuracy for final_model

Eval_final_model_train <- predict(final_model,train_Data)
confusionMatrix(Eval_final_model_train, train_Data$number)

# Model Accuracy for training dataset : 0.9514 

Eval_final_model_test <- predict(final_model,test_Data)
confusionMatrix(Eval_final_model_test,test_Data$number)

# Model Accuracy for test dataset : 0.9522

# 6. Conclusion

# There is very less difference between train and test accuracy. So, we can say that over-fitting 
# is not the case, our model is able to predict correct digits using Non-Linear SVM to a large extent.
