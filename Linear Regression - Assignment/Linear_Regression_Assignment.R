# =====================================================================
# Linear Regression Assignment
# A Chinese automobile company Geely Auto - Car Price Prediction Model
# =====================================================================

# Assignment Objective: The ultimate aim is to build a linear model for A Chinese automobile company -
# Geely Auto to predict the price of cars with the available independent variables. 
# It will be used by the Geely Auto management team to understand how exactly the prices vary and 
# accordingly manipulate the design of the cars, the business strategy etc. 

# Loading Required Packages
# To use the stepAIC, loading the MASS package from the library
library(MASS)

# To use the vif function, loading the car package from the library
library(car) 

# For data manipulation and visualization loading below packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Setting the working directory
# setwd("F:/IIIT-B PGDDS/Course3 Predictive Analytics/Module1 Linear Regression/Session2 Multiple Regression")

# ===========================================
#         1.  DATA PREPARATION
# (Data understanding, preparation and EDA)
# ===========================================

# Firstly, importing the "CarPrice_Assignment.csv" and store it in a variable "cars". 
# Using stringsAsFactors = FALSE command to ensure that no attribute is stored as factor by default.

cars <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)


# To display the dataset to get a primary understanding of the various attributes present in the
# cars dataset using View().

# View(cars)

# To see structure of the cars dataset using str()
str(cars)

# 1.1 DATA DICTIONARY/ Data Understanding
# --------------------------------------------------------------------------------------------
# Car_ID			    : Unique id of each observation (Interger)		
# Symboling 		  : Its assigned insurance risk rating, A value of +3 indicates that the auto is risky, -3 that it is probably pretty safe.(Categorical) 		
# carCompany		  : Name of car company (Categorical)		
# fueltype			  : Car fuel type i.e gas or diesel (Categorical)		
# aspiration		  : Aspiration used in a car (Categorical)		
# doornumber		  : Number of doors in a car (Categorical)		
# carbody			    : body of car (Categorical)		
# drivewheel		  : type of drive wheel (Categorical)		
# enginelocation  : Location of car engine (Categorical)		
# wheelbase			  : Weelbase of car (Numeric)		
# carlength			  : Length of car (Numeric)		
# carwidth			  : Width of car (Numeric)		
# carheight			  : height of car (Numeric)		
# carweight		  : The weight of a car without occupants or baggage. (Numeric)		
# enginetype		  : Type of engine. (Categorical)		
# cylindernumber  :	cylinder placed in the car (Categorical)		
# enginesize		  :	Size of car (Numeric)		
# fuelsystem		  :	Fuel system of car (Categorical)		
# boreratio			  : Boreratio of car (Numeric)		
# stroke			    : Stroke or volume inside the engine (Numeric)		
# compressionratio: compression ratio of car (Numeric)		
# horsepower			: Horsepower (Numeric)		
# peakrpm			    : car peak rpm (Numeric)		
# citympg			    : Mileage in city (Numeric)		
# highwaympg			: Mileage on highway (Numeric)		
# price(Dependent variable):  Price of car (Numeric)		
# --------------------------------------------------------------------------------------------
# Otherthan price all are Independent variables


# 1.2 Data Cleaning/ Data Preparation
# --------------------------------------------------------------------------------------------

# Removing car_ID column from the dataset because it is not needed for predicting the model
cars <- cars[,-1]

# To change all the column headings for cars dataset to lowercase
colnames(cars) <- tolower(colnames(cars))

# To change/correct the column "curbweight" heading as "carweight"
colnames(cars)[13] <- "carweight"

# check for duplicated rows
nrow(unique(cars))

# In above result total no. of observations in the dataset is still 205, indicating that there were
# no duplicates in the cars data set.

# To check if there are any missing values in the cars dataset
sum(is.na(cars))    # Fortunately, there are no missing values in the dataset.

# As CarName is comprised of two parts - the first word is the name of 'car company' and 
# the second is the 'car model'. So partitioning carname into company and model
#cars1 <- cars
#cars2 <- cars

#cars$carname <- sapply(strsplit(as.character(cars$carname)," "), "[", 1) 

# Reference: http://tidyr.tidyverse.org/reference/separate.html
# To split carname Using separate() from tidyr package
cars <- separate(cars, carname, c("companyname", "modelname"), sep = " ", extra = "merge", fill = "right")

# To exclude modelelname column from cars dataset
cars <- cars[, -c(3)] 

# To apply lowcase for all the cars companynames for consistency of data
cars$companyname <- tolower(cars$companyname)

# To validate car names for further analysis
unique(cars$companyname)

# Estimating misspelt cars companyname by getting count of each car companyname
table(cars$company) # alfa-romero : 3, maxda : 2, porcshce : 1, toyouta : 1, vokswagen  : 1, vw : 2

# For cars companyname results above, few data issue observed are:
# There is no car company named alfa-romero. So, it need to be corrected as alfa-romeo
# There is no car company named maxda. It's misspelt, need to be corrected as mazda
# There is no car company named porcshce. It's misspelt, need to be corrected as porsche 
# There is no car company named toyouta. It's misspelt, need to be corrected as toyota
# There is no car company named vokswagen. It's misspelt, need to be corrected as volkswagen
# The last one remaining is "vw" which doesnot make any car companyname. Need to confirm from management.
# For time being considering "vw" as volkswagen for analysis in the model

# Below function takes car companyname, and correct it based on above observation, and return it
correct_companyname <- function(car_companyname){
  corrected_name <- car_companyname # initial value assigned as same
  if(car_companyname == "alfa-romero"){
    corrected_name <- "alfa-romeo"
  } else if(car_companyname == "maxda"){
    corrected_name <- "mazda" 
  } else if(car_companyname == "porcshce"){
    corrected_name <- "porsche"
  } else if(car_companyname == "toyouta") {
    corrected_name <- "toyota"
  } else if(car_companyname %in% c("vokswagen", "vw")){
    corrected_name <- "volkswagen"
  } 
  return (corrected_name)
}
 
# Using sapply to correct all car companynames
cars$companyname <- sapply(cars$companyname, correct_companyname)

# Validtaing result after correction. I should get only 27-5 = 22 unique companynames
table(cars$companyname)

str(cars)

# Treating the outliers (if any) for numeric values. 
# To check for outliers, first we find out the quantile values at each 1% interval and see 
# wherever there is a high jump from one quantile to another, we cap/floor those values.

#1.2.1 Checking outliers in the wheelbase variable. 
quantile(cars$wheelbase, seq(0,1,0.01))
# There is a jump between 99% and 100%. So, cap all values above 115.544 (99%) to 115.544. 
cars$wheelbase[which(cars$wheelbase > 115.544 )] <- 115.544
quantile(cars$wheelbase, seq(0,1,0.01))

#1.2.2 Checking outliers in the carlength variable. 
quantile(cars$carlength, seq(0,1,0.01))
# There is a jump between 94% and 95%. So, cap all values above 192.700 (94%) to 192.700. 
cars$carlength[which(cars$carlength > 192.700 )] <- 192.700
quantile(cars$carlength, seq(0,1,0.01))

#1.2.3 Checking outliers in the carwidth variable. 
quantile(cars$carwidth, seq(0,1,0.01))
# There is a jump between 0% and 1%. So, cap all values below 62.536 (1%) to 62.536. 
cars$carwidth[which(cars$carwidth < 62.536 )] <- 62.536
quantile(cars$carwidth, seq(0,1,0.01))

#1.2.4 Checking outliers in the carheight variable. 
quantile(cars$carheight, seq(0,1,0.01))
# There is no significance jump between values. SO no outliers needed.

#1.2.5 Checking outliers in the curbweight variable. 
quantile(cars$carweight, seq(0,1,0.01))
# There is a jump between 0% and 1%. So, cap all values below 1819.72 (1%) to 1819.72. 
cars$carweight[which(cars$carweight < 1819.72 )] <- 1819.72
quantile(cars$carweight, seq(0,1,0.01))

#1.2.6 Checking outliers in the enginesize variable. 
quantile(cars$enginesize, seq(0,1,0.01))
# There is a jump between 3% to 2%, 93% to 94%. So, cap all values below 90.00 (3%) to 90.00 
# and cap all values above 183.00 (93%) to 183.00. 
cars$enginesize[which(cars$enginesize < 90.00 )] <- 90.00
cars$enginesize[which(cars$enginesize > 183.00 )] <- 183.00
quantile(cars$enginesize, seq(0,1,0.01))

#1.2.7 Checking outliers in the boreratio variable. 
quantile(cars$boreratio, seq(0,1,0.01))
# There is no significance jump between values. SO no outliers needed.

#1.2.8 Checking outliers in the stroke variable. 
quantile(cars$stroke, seq(0,1,0.01))
# There is no significance jump between values. SO no outliers needed.

#1.2.9 Checking outliers in the compressionratio variable. 
quantile(cars$compressionratio, seq(0,1,0.01))
# There is a jump between 90% and 91%. So, cap all values above 10.9400 (90%) to 10.9400. 
cars$compressionratio[which(cars$compressionratio > 10.9400 )] <- 10.9400
quantile(cars$compressionratio, seq(0,1,0.01))

#1.2.10 Checking outliers in the horsepower variable. 
quantile(cars$horsepower, seq(0,1,0.01))
# There is a jump between 97% and 98%. So, cap all values above 184.00 (97%) to 184.00. 
cars$horsepower[which(cars$horsepower > 184.00 )] <- 184.00
quantile(cars$horsepower, seq(0,1,0.01))

#1.2.11 Checking outliers in the peakrpm variable. 
quantile(cars$peakrpm, seq(0,1,0.01))
# There is a jump between 99% and 100%. So, cap all values above 6000 (99%) to 6000. 
cars$peakrpm[which(cars$peakrpm > 6000 )] <- 6000
quantile(cars$peakrpm, seq(0,1,0.01))

#1.2.12 Checking outliers in the citympg variable. 
quantile(cars$citympg, seq(0,1,0.01))
# There is a jump between 98% and 99%. So, cap all values above 38.00 (98%) to 38.00. 
cars$citympg[which(cars$citympg > 38.00 )] <- 38.00
quantile(cars$citympg, seq(0,1,0.01))

#1.2.13 Checking outliers in the highwaympg variable. 
quantile(cars$highwaympg, seq(0,1,0.01))
# There is a jump between 99% and 100%. So, cap all values above 49.88 (99%) to 49.88. 
cars$highwaympg[which(cars$highwaympg > 49.88 )] <- 49.88
quantile(cars$highwaympg, seq(0,1,0.01))

#1.2.14 Checking outliers in the price variable. 
quantile(cars$price, seq(0,1,0.01))
# There is a jump between 98% and 99%. So, cap all values above 36809.60 (98%) to 36809.60. 
cars$price[which(cars$price > 36809.60 )] <- 36809.60
quantile(cars$price, seq(0,1,0.01))

# 1.3 Defining dummy variables
# --------------------------------------------------------------------------------------------

# 1.3.1 Starting to create dummy variables from bottom for char type variables
# we see fuelsystem variable having more than 3 levels. 
summary(factor(cars$fuelsystem))

# Converting "fuelsystem" into dummies . 
dummy_01 <- data.frame(model.matrix( ~fuelsystem, data = cars))

# check the dummy_01 data frame.
# View(dummy_01)

# This column should be removed from the newly created dummy_01 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_01 <- dummy_01[,-1]

# Combine the dummy variables to the main cars data set, after removing the original categorical "fuelsystem" column
str(cars)
cars_01 <- cbind(cars[,-17], dummy_01)
# View(cars_01)

# 1.3.2 cylindernumber
# we see cylindernumber variable having more than 3 levels. 
summary(factor(cars$cylindernumber))

# Converting "cylindernumber" into dummies . 
dummy_02 <- data.frame(model.matrix( ~cylindernumber, data = cars))

# check the dummy_02 data frame.
# View(dummy_02)

# This column should be removed from the newly created dummy_02 dataframe containing the dummy values for the variable "cylindernumber". 
dummy_02 <- dummy_02[,-1]

# Combine the dummy variables to the main cars data set, after removing the original categorical "cylindernumber" column
str(cars_01)
cars_02 <- cbind(cars_01[,-15], dummy_02)
# View(cars_02)

# 1.3.3 enginetype
# we see enginetype variable having more than 3 levels. 
summary(factor(cars$enginetype))

# Converting "enginetype" into dummies . 
dummy_03 <- data.frame(model.matrix( ~enginetype, data = cars))

# Check the dummy_03 data frame.
# View(dummy_03)

# This column should be removed from the newly created dummy_02 dataframe containing the dummy values for the variable "enginetype". 
dummy_03 <- dummy_03[,-1]

# Combine the dummy variables to the main cars data set, after removing the original categorical "enginetype" column
str(cars_02)
cars_03 <- cbind(cars_02[,-14], dummy_03)
# View(cars_03)

# 1.3.4 enginelocation
# we see enginelocation variable having only levels. 
summary(factor(cars_03$enginelocation))

# Converting "enginelocation" into dummies . 
cars_03$enginelocation <- as.factor(cars_03$enginelocation)
levels(cars_03$enginelocation) <- c(1,0) 

# Validating after conversion
str(cars_03)
summary(factor(cars_03$enginelocation))

# 1.3.5 drivewheel
# We see drivewheel variable having more than 3 levels. 
summary(factor(cars_03$drivewheel)) 

# Converting "drivewheel" into dummies . 
dummy_05 <- data.frame(model.matrix( ~drivewheel, data = cars))

# Check the dummy_05 data frame.
# View(dummy_05)

# This column should be removed from the newly created dummy_04 dataframe containing the dummy values for the variable "drivewheel". 
dummy_05 <- dummy_05[,-1]

# Combine the dummy variables to the main cars data set, after removing the original categorical "drivewheel" column
str(cars_03)
cars_04 <- cbind(cars_03[,-7], dummy_05)
# View(cars_04) 


# 1.3.6 carbody
# we see carbody variable having more than 3 levels. 
summary(factor(cars_04$carbody)) 

# Converting "carbody" into dummies . 
dummy_06 <- data.frame(model.matrix( ~carbody, data = cars))

# Check the dummy_06 data frame.
# View(dummy_06)

# This column should be removed from the newly created dummy_04 dataframe containing the dummy values for the variable "carbody". 
dummy_06 <- dummy_06[,-1]

# Combine the dummy variables to the main cars data set, after removing the original categorical "carbody" column
str(cars_04)
cars_05 <- cbind(cars_04[,-6], dummy_06)
# View(cars_05) 


# 1.3.7 doornumber
# we see doornumber variable having only one level. 
summary(factor(cars_03$doornumber))

# Converting "doornumber" into dummies . 
cars_05$doornumber <- as.factor(cars_05$doornumber)
levels(cars_05$doornumber) <- c(1,0) 
str(cars_05)

# Verify after conversion into dummy variable
summary(factor(cars_05$doornumber))


# 1.3.8 aspiration
# we see aspiration variable having only one level. 
summary(factor(cars_05$aspiration))

# Converting "aspiration" into dummies . 
cars_05$aspiration <- as.factor(cars_05$aspiration)
levels(cars_05$aspiration) <- c(1,0) 
str(cars_05)

# Verify after conversion into dummy variable
summary(factor(cars_05$aspiration))


# 1.3.9 fueltype
# we see fueltype variable having only one level. 
summary(factor(cars_05$fueltype))

# Converting "fueltype" into dummies . 
cars_05$fueltype <- as.factor(cars_05$fueltype)
levels(cars_05$fueltype) <- c(1,0) 
str(cars_05)

# Verify after conversion into dummy variable
summary(factor(cars_05$fueltype))


# 1.3.10 companyname
# we see companyname variable having more than 3 levels. 
summary(factor(cars_05$companyname))

# Converting "companyname" into dummies . 
dummy_10 <- data.frame(model.matrix( ~companyname, data = cars))

# Check the dummy_10 data frame.
# View(dummy_10)

# This column should be removed from the newly created dummy_10 dataframe containing the dummy values for the variable "companyname". 
dummy_10 <- dummy_10[,-1]

# Combine the dummy variables to the main cars data set, after removing the original categorical "companyname" column
str(cars_05)
cars_06 <- cbind(cars_05[,-2], dummy_10)
# View(cars_06) 

##############################################################################################
# 2.1 DATA ANALYSIS - Modeling uing Multiple Linear Regression
# --------------------------------------------------------------------------------------------

# Divide into training and test data set
# Set the seed to 100, let's run it 
set.seed(100)

# Randomly generate row indices for train dataset
trainindices= sample(1:nrow(cars_06), 0.7*nrow(cars_06))
# Generate the train data set
train = cars_06[trainindices,]

# Similarly store the rest of the observations into an object "test".
test = cars_06[-trainindices,]

# Executing the first model_1 multilinear model in the training set. 
# --------------------------------------------------------------------------------------------

model_1 <-lm(price~.,data=train)

# Check the summary of model. 
summary(model_1)

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 

# We have a total of 64 variables considered into the model  
step <- stepAIC(model_1, direction="both")

# We see many iterations have been done through the stepwise command. 
# Now we need to know our model equation so lets use step command 

step

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
# We can notice that stepAIC removed other variables 

model_2 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                wheelbase + carwidth + carweight + enginesize + boreratio + 
                stroke + horsepower + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Check the summary of model_2
summary(model_2)

## Checking for multicollinearity 
# If the VIF is above 2 or 5 as the business goal says, you would remove 
# the variables if they are statistically insignificant

# Find the VIF of model_2
sort(vif(model_2), decreasing = TRUE)

# enginesize is having high vif value 33.262073, and p-value 0.126644 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model  

# Now, build a new model by removing enginesize variable.
model_3 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                wheelbase + carwidth + carweight + boreratio + 
                stroke + horsepower + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_3
sort(vif(model_3), decreasing = TRUE)

# Check the summary of model_3
summary(model_3)

# cylindernumberfour is having high vif value 32.597918, but p-value 1.42e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 24.420804, but p-value 1.53e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 18.048531, but p-value 0.006787 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 15.814247, but p-value 0.001753 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 15.589655, but p-value 0.000789 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# wheelbase is having high vif value 12.665174, and p-value 0.257006 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing wheelbase variable.
model_4 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + carweight + boreratio + stroke + horsepower + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_4
sort(vif(model_4), decreasing = TRUE)

# Check the summary of model_4
summary(model_4)

# cylindernumberfour is having high vif value 30.232192, and p-value 2.54e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 23.989080, and p-value 7.18e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 17.266047, and p-value 0.011325 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 15.282585, and p-value 0.002890 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 14.441767, and p-value 0.001468 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 11.431053, and p-value 0.000282 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 11.005032, and p-value 8.70e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# horsepower is having high vif value 10.737123, and p-value 0.069733 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing horsepower variable.
model_5 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + carweight + boreratio + stroke + fuelsystem4bbl + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_5
sort(vif(model_5), decreasing = TRUE)

# Check the summary of model_5
summary(model_5)

# cylindernumberfour is having high vif value 26.611627, and p-value 5.99e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 22.385086, and p-value 5.76e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 17.161563, and p-value 0.008097 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 15.271352, and p-value 0.002738 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 14.338539, and p-value 0.000961 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 11.374863, and p-value 0.000200 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 10.991715, and p-value 8.12e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 9.709159, and p-value 9.62e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 9.184833, and p-value 0.000660 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model 

# boreratio is having high vif value 7.416939, and p-value 0.298119 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model 

# Now, build a new model by removing boreratio variable.
model_6 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + carweight + stroke + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + drivewheelfwd + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_6
sort(vif(model_6), decreasing = TRUE)

# Check the summary of model_6
summary(model_6)

# cylindernumberfour is having high vif value 26.544047, and p-value 4.60e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 20.156475, and p-value 3.37e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 17.001933, and p-value 0.005864 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 14.921756, and p-value 0.001484 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 12.541803, and p-value 0.000106 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 11.191857, and p-value 0.000288 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 10.291526, and p-value 1.36e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 8.952495, and p-value 0.000317 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamehonda is having high vif value 7.214999, and p-value 0.029663 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model
 
# companynamemazda is having high vif value 7.157255, and p-value 0.000395 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamenissan is having high vif value 7.134836, and p-value 0.007740 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 6.622191, and p-value 5.68e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# drivewheelfwd is having high vif value 6.334387, and p-value 0.188209 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing drivewheelfwd variable.
model_7 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + carweight + stroke + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge + companynamehonda + 
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_7
sort(vif(model_7), decreasing = TRUE)

# Check the summary of model_7
summary(model_7)

# cylindernumberfour is having high vif value 26.539937, and p-value 4.57e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 16.751678, and p-value 0.003518 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 14.772631, and p-value 0.000946 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 14.289094, and p-value 1.05e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 12.506780, and p-value 8.41e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 11.110491, and p-value 0.000425 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 9.329206, and p-value 7.26e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 8.930748, and p-value 0.000259 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamehonda is having high vif value 6.951827, and p-value 0.050156 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamehonda variable.
model_8 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
                carwidth + carweight + stroke + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge +  
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_8
sort(vif(model_8), decreasing = TRUE)

# Check the summary of model_8
summary(model_8)

# cylindernumberfour is having high vif value 25.643554, and p-value 8.04e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 15.967610, and p-value 0.000800 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 13.995683, and p-value 0.000162 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 13.776883, and p-value 1.47e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 12.028469, and p-value 1.69e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 9.326655, and p-value 8.67e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 8.413244, and p-value 3.37e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfive is having high vif value 5.366995, and p-value 3.12e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 4.539828, and p-value 0.001295 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# fuelsystem4bbl is having high vif value 4.165205, and p-value 0.001389 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 3.746113, and p-value 0.037941 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 3.744306, and p-value 0.000760 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypel is having high vif value 3.634803, and p-value 3.71e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# symboling is having high vif value 3.529029, and p-value 0.858605 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing symboling variable.
model_9 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carweight + 
                stroke + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                enginetypel + enginetypeohcf + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + companynameaudi + 
                companynamebmw + companynamebuick + companynamedodge +  
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_9
sort(vif(model_9), decreasing = TRUE)

# Check the summary of model_9
summary(model_9)

# cylindernumberfour is having high vif value 25.620165, and p-value 7.12e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 13.710325, and p-value 1.08e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 13.332473, and p-value 0.000184 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 13.249485, and p-value 8.67e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 11.954689, and p-value 1.55e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 9.292156, and p-value 7.60e-08 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 6.692882, and p-value 2.55e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfive is having high vif value 5.223806, and p-value 1.51e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 4.539792, and p-value 0.001230 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# fuelsystem4bbl is having high vif value 3.976639, and p-value 0.001168 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 3.746084, and p-value 0.037137 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 3.733924, and p-value 0.000731 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypel is having high vif value 3.634374, and p-value 3.40e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhardtop is having high vif value 3.380890, and p-value 0.000745 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberthree is having high vif value 3.234758, and p-value 0.074857 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing cylindernumberthree variable.
model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carweight + 
                stroke + fuelsystem4bbl + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + enginetypel + enginetypeohcf + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                companynameaudi + companynamebmw + companynamebuick + companynamedodge +  
                companynameisuzu + companynamejaguar + companynamemazda + 
                companynamemercury + companynamemitsubishi + companynamenissan + 
                companynameplymouth + companynamerenault + companynamesaab + 
                companynametoyota + companynamevolkswagen + companynamevolvo, 
              data = train)

# Find the VIF of model_10
sort(vif(model_10), decreasing = TRUE)

# Check the summary of model_10
summary(model_10)

# carbodysedan is having high vif value 13.324078, and p-value 0.000246 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 13.248377, and p-value 0.000106 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 12.517200, and p-value 2.08e-11 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfour is having high vif value 11.617030, and p-value 1.94e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 8.388110, and p-value 1.52e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 8.019456, and p-value 5.01e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 6.690182, and p-value 2.66e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 4.535472, and p-value 0.001136 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfive is having high vif value 3.982266, and p-value 2.50e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 3.475479, and p-value 9.44e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhardtop is having high vif value 3.380570, and p-value 0.000882 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 3.379723, and p-value 0.006286 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 2.760338, and p-value 3.79e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# fuelsystem4bbl is having high vif value 2.589576, and p-value 0.006253 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynameaudi is having high vif value 2.578511, and p-value 0.347057 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynameaudi variable.
model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen + companynamevolvo, 
               data = train)

# Find the VIF of model_11
sort(vif(model_11), decreasing = TRUE)

# Check the summary of model_11
summary(model_11)

# carbodysedan is having high vif value 13.304726, and p-value 0.000212 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhatchback is having high vif value 13.141526, and p-value 7.20e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 12.516467, and p-value 1.89e-11 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfour is having high vif value 11.037144, and p-value 2.81e-10 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 8.261816, and p-value 6.47e-10 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 7.527707, and p-value 1.12e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 6.668287, and p-value 2.00e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfive is having high vif value 3.419511, and p-value 1.28e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhardtop is having high vif value 3.375579, and p-value 0.000768 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 3.365875, and p-value 3.81e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 3.357625, and p-value 0.004870 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 2.969889, and p-value 0.000803 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 2.596463, and p-value 8.58e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# fuelsystem4bbl is having high vif value 2.531779, and p-value 0.003721 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginelocation is having high vif value 2.517742, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamevolvo is having high vif value 2.249237, and p-value 0.239774 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamevolvo variable.
model_12 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)

# Find the VIF of model_12
sort(vif(model_12), decreasing = TRUE)

# Check the summary of model_12
summary(model_12)

# carbodyhatchback is having high vif value 13.019788, and p-value 4.43e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodysedan is having high vif value 12.931146, and p-value 8.46e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carweight is having high vif value 12.204255, and p-value 3.14e-11 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfour is having high vif value 10.687673, and p-value 5.70e-11 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 8.165676, and p-value 1.03e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 7.470601, and p-value 7.04e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodywagon is having high vif value 6.539286, and p-value 8.17e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfive is having high vif value 3.393232, and p-value 1.92e-09 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carbodyhardtop is having high vif value 3.303491, and p-value 0.000379 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 2.728213, and p-value 0.000142 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 2.690909, and p-value 0.010112 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 2.614727, and p-value 4.83e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# fuelsystem4bbl is having high vif value 2.494390, and p-value 0.002264 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginelocation is having high vif value 2.485323, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 2.062539, and p-value 1.00e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# Now we have remaining variables with VIF between 1 and 2, So, lets check 
# corelation between carbodyhatchback, and carbodysedan which is having high VIF value
cor(train$carbodyhatchback, train$carbodysedan)

# The correlation is ~ 64%, indicating that the variables are highly correlated. 
# So, remove the variable with lower significance level out of the two.
# carbodyhatchback p-value 4.43e-05 = 4.43 / 10^5 whis is less than carbodysedan p-value 8.46e-05 = 8.46 / 10^5

# Now, build a new model by removing carbodysedan variable.
model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 cylindernumberfour + cylindernumbersix + enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train) 

# Find the VIF of model_13
sort(vif(model_13), decreasing = TRUE)

# Check the summary of model_13
summary(model_13)

# Adjusted R-squared:  0.9672

# carweight is having high vif value 11.393186, and p-value 4.80e-13 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfour is having high vif value 10.630446, and p-value 1.19e-10 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 7.639486, and p-value 2.89e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 7.353426, and p-value 2.74e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumberfive is having high vif value 3.326876, and p-value 7.26e-10 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 2.686446, and p-value 5.32e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 2.614034, and p-value 0.002119 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 2.568007, and p-value 1.55e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# fuelsystem4bbl is having high vif value 2.484556, and p-value 0.001919 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginelocation is having high vif value 2.312386, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynametoyota is having high vif value 2.026172, and p-value 3.35e-06 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# Now we have remaining variables with VIF between 1 and 2, So, lets check 
# corelation between carweight, and cylindernumberfour which is having high VIF value
cor(train$carweight, train$cylindernumberfour)

# The correlation is ~ 59%, indicating that the variables are highly correlated. 
# cylindernumberfour has high p-value than carweight.
# So, remove the variable cylindernumberfour with lower significance level out of the two.

# Now, build a new model by removing cylindernumberfour variable. 
model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 cylindernumbersix + enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train) 

# Find the VIF of model_14
sort(vif(model_14), decreasing = TRUE)

# Check the summary of model_14
summary(model_14)

# carweight is having high vif value 11.366706, and p-value 9.67e-11 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# carwidth is having high vif value 7.481240, and p-value 2.61e-07 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 2.798138, and p-value 0.383185 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 2.568422, and p-value 0.000798 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 2.534442, and p-value 1.73e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginelocation is having high vif value 2.300717, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# companynamebuick is having high vif value 2.175702, and p-value 5.47e-10 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model 

# Now we have remaining variables with VIF between 1 and 2, So, lets check 
# corelation between carweight, and carwidth which is having high VIF value
cor(train$carweight, train$carwidth)

# The correlation is ~ 88%, indicating that the variables are highly correlated. 
# carwidth has high p-value than carweight.
# So, remove the variable carwidth with lower significance level out of the two.

# Now, build a new model by removing carwidth variable.  
model_15 <- lm(formula = price ~ aspiration + enginelocation + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 cylindernumbersix + enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)  

# Find the VIF of model_15
sort(vif(model_15), decreasing = TRUE)

# Check the summary of model_15
summary(model_15)

# Adjusted R-squared:  0.9414

# carweight is having high vif value 4.071538, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# cylindernumbersix is having high vif value 2.755158, and p-value 0.859402 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model 

# Now, build a new model by removing cylindernumbersix variable. 
model_16 <- lm(formula = price ~ aspiration + enginelocation + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train) 

# Find the VIF of model_16
sort(vif(model_16), decreasing = TRUE)

# Check the summary of model_16
summary(model_16)

# Adjusted R-squared: 0.9418

# carweight is having high vif value 2.674161, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# stroke is having high vif value 2.567467, and p-value 0.002456 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 2.525696, and p-value 5.50e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model 

# Now we have remaining variables with VIF between 1 and 2, So, lets check 
# corelation between carweight, and stroke which is having high VIF value
cor(train$carweight, train$stroke)

# The correlation is ~ 21%, indicating that the variables are not highly correlated. 

# So, lets check corelation between stroke, and enginetypeohcf which is having high VIF value
cor(train$stroke, train$enginetypeohcf)

# The correlation is ~ 49%, indicating that the variables are not highly correlated. 

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that aspiration0 has highest p-value 0.988988 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing aspiration variable.  
model_17 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 enginetypel + enginetypeohcf + 
                 carbodyhardtop + carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)

# Find the VIF of model_17
sort(vif(model_17), decreasing = TRUE)

# Check the summary of model_17
summary(model_17)

# Adjusted R-squared: 0.9423

# stroke is having high vif value 2.464151, and p-value 0.001938 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

# enginetypeohcf is having high vif value 2.448438, and p-value 4.00e-05 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model 

# carweight is having high vif value 2.393431, and p-value 2e-16 < 0.05, which indicates this variable is significant,
# So cannot remove it from the model

cor(train$stroke, train$enginetypeohcf)
cor(train$enginetypeohcf, train$carweight)

# All the 3 variables are not highly correlated
# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that carbodyhardtop has highest p-value 0.653291 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing carbodyhardtop variable. 
model_18 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 enginetypel + enginetypeohcf + 
                 carbodyhatchback + carbodywagon + 
                 companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)

# Find the VIF of model_18
sort(vif(model_18), decreasing = TRUE)

# Check the summary of model_18
summary(model_18)

# Adjusted R-squared: 0.9427

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$stroke)
cor(train$stroke, train$carweight)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that carbodyhatchback has highest p-value 0.58858 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing carbodyhatchback variable.  
model_19 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + fuelsystem4bbl + cylindernumberfive + 
                 enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)

# Find the VIF of model_19
sort(vif(model_19), decreasing = TRUE)
# Check the summary of model_19
summary(model_19)

# Adjusted R-squared: 0.9431 

# All the 3 variables having hgh VIF value are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that cylindernumberfive has highest p-value 0.296674 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing cylindernumberfive variable.  
model_20 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + fuelsystem4bbl + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemercury + companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamerenault + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_20
sort(vif(model_20), decreasing = TRUE)
# Check the summary of model_20
summary(model_20)

# Adjusted R-squared: 0.943

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamerenault has highest p-value 0.297524 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamerenault variable.
model_21 <- lm(formula = price ~ enginelocation + carweight + 
               stroke + fuelsystem4bbl + enginetypel + enginetypeohcf + 
               carbodywagon + companynamebmw + companynamebuick + companynamedodge +  
               companynameisuzu + companynamejaguar + companynamemazda + 
               companynamemercury + companynamemitsubishi + companynamenissan + 
               companynameplymouth + companynamesaab + 
               companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_21
sort(vif(model_21), decreasing = TRUE)
# Check the summary of model_21
summary(model_21)

# Adjusted R-squared: 0.943

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamemercury has highest p-value 0.258532 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamemercury variable.

model_22 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + fuelsystem4bbl + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + companynamenissan + 
                 companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_22
sort(vif(model_22), decreasing = TRUE)
# Check the summary of model_22
summary(model_22)

# Adjusted R-squared: 0.9428

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that fuelsystem4bbl has highest p-value 0.231236 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing fuelsystem4bbl variable.
model_23 <- lm(formula = price ~ enginelocation + carweight + 
               stroke + enginetypel + enginetypeohcf + 
               carbodywagon + companynamebmw + companynamebuick + companynamedodge +  
               companynameisuzu + companynamejaguar + companynamemazda + 
               companynamemitsubishi + companynamenissan + 
               companynameplymouth + companynamesaab + 
               companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_23
sort(vif(model_23), decreasing = TRUE)
# Check the summary of model_23
summary(model_23)

# Adjusted R-squared: 0.9426

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamenissan has highest p-value 0.171707 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamenissan variable.
model_24 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick + companynamedodge +  
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_24
sort(vif(model_24), decreasing = TRUE)
# Check the summary of model_24
summary(model_24)

# Adjusted R-squared: 0.9422

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamedodge has highest p-value 0.113548 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamedodge variable.
model_25 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick +   
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + companynameplymouth + companynamesaab + 
                 companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_25
sort(vif(model_25), decreasing = TRUE)
# Check the summary of model_25
summary(model_25)

# Adjusted R-squared: 0.9415

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamesaab has highest p-value 0.107023 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamesaab variable.
model_26 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick +   
                 companynameisuzu + companynamejaguar + companynamemazda + 
                 companynamemitsubishi + companynameplymouth +  
                 companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_26
sort(vif(model_26), decreasing = TRUE)
# Check the summary of model_26
summary(model_26)

# Adjusted R-squared: 0.9408

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamemazda has highest p-value 0.125890 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamemazda variable.
model_27 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick +   
                 companynameisuzu + companynamejaguar +  
                 companynamemitsubishi + companynameplymouth +  
                 companynametoyota + companynamevolkswagen, data = train)
  
# Find the VIF of model_27
sort(vif(model_27), decreasing = TRUE)
# Check the summary of model_27
summary(model_27)

# Adjusted R-squared: 0.9401

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynamevolkswagen has highest p-value 0.154026 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynamevolkswagen variable.
model_28 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick +   
                 companynameisuzu + companynamejaguar +  
                 companynamemitsubishi + companynameplymouth +  
                 companynametoyota, data = train)
  
# Find the VIF of model_28
sort(vif(model_28), decreasing = TRUE)
# Check the summary of model_28
summary(model_28)

# Adjusted R-squared: 0.9396

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)
cor(train$carweight, train$stroke)

# All the 3 variables are not highly correlated

# So, remove the variable which is having highest p-value i.e. with lower significance level among all the variables.
# I see that companynameplymouth has highest p-value 0.165990 > 0.05, which indicates this variable is NOT significant,
# So remove it from the model

# Now, build a new model by removing companynameplymouth variable.
model_29 <- lm(formula = price ~ enginelocation + carweight + 
                 stroke + enginetypel + enginetypeohcf + 
                 carbodywagon + companynamebmw + companynamebuick +   
                 companynameisuzu + companynamejaguar +  
                 companynamemitsubishi + companynametoyota, data = train)
  
# Find the VIF of model_29
sort(vif(model_29), decreasing = TRUE)
# Check the summary of model_29
summary(model_29)

# Adjusted R-squared: 0.9392

# Checking Correlation amongf variable having high VIF values
cor(train$enginetypeohcf, train$carweight)  # -0.05536289 ~ 5% 

# Both of above variables having high VIF value - are not highly correlated

# Also I have all the variables with p-value < 0.05 that looks significant 
# Now the model has 12 variables all of which are significant. So, model_29 is the final model.

# The value of Adjusted R-squared is ~ 93%. Now, let us move forward to test the model on test data.
 
# Now using model_29 to predict the car price value for test data.
# -------------------------------------------------------------------

Predict_price <- predict(model_29,test[,-which(names(test)=="price")])

# In order to check how accurate are the predictions of the model, we need to find 
# the r-squared value between the predicted and actual prices 

# R-squared is defined as the square of correlation between actual and predicted values of the variable.
rsquared_value <- (cor(test$price,Predict_price))^2
rsquared_value

# We notice that the r-squared value for the model is ~93% while the r-squared value for
# the test dataset is ~87% which is quite possible.

# Generally, a deviation of +(-) 5% in the R-squared value of the model for the test data is
# acceptable. However, if the deviation is significant (>5%) then we need to recheck the model.

# Thus we have the final equation as per Linear Regresion obtained from the final model_29 is as below
# price = mx + c
# price = (20862.4366 * enginelocation) + (10.9389 * carweight) + (-2762.1029 * stroke) + 
#         (-4420.2623 * enginetypel) + (-3232.7043 * enginetypeohcf) + (-2507.2509 * carbodywagon) + 
#         (7460.0005 * companynamebmw) + (6828.4227 * companynamebuick) + (-4385.1449 * companynameisuzu) + 
#         (6412.9037 * companynamejaguar) + (-1850.2018 * companynamemitsubishi) + 
#         (-2196.2495 * companynametoyota) + (-5472.4314)

# All the dependent variables obtained as part of final model_29 are the driving factors

# Plotting using residualPlot for Actual vs Predicted Views for the final model_29
residualPlot(model_29)
