# Brett Waugh
# 12 April 2019
# capstone.R
# Different analytic techniques to predict the
# number of Starbuck's in an area. 
# Data was retrieved from: https://www.kaggle.com/starbucks/store-locations

#########################################################################################################
########################################### Data & Libraries ############################################
#########################################################################################################
# Necessary libraries.
require(readr)
require(ggplot2)
require(dplyr)
require(stats)
require(plyr)
require(cluster)
require(lattice)
require(graphics)
require(grid)
require(gridExtra)
require(mgcv)
require(caret)
require(e1071)
require(boot)
require(ISLR)
require(arm)

# Load in the data. Data was provided by: https://www.kaggle.com/starbucks/store-locations
directory <- read_csv("Documents/IDS4934/capstone/directory.csv")

# States and counts calculated in Splunk. 
starbucksByState <- read_csv("Documents/IDS4934/capstone/starbucksByState.csv")

# Make into a dataframe.
df <- as.data.frame(directory)

# Set seed for study.
set.seed(950)

#########################################################################################################
############################################# Data filtering ############################################
#########################################################################################################

# Only keep stores that are Starbucks. Teavana stores were originally included. 
df1 <- subset(df, Brand=="Starbucks")

# Verify that only Starbucks stores remain. 
nrow(directory[directory$Brand!="Starbucks",])

# Drop rows that do not matter to the analysis. 
df2 <- subset(df1, select = -c(`Store Number`,`Store Number`, `Store Name`, `Ownership Type`, `Street Address`, `Phone Number`, `Timezone`))

# Verify that those fields were dropped.
str(df2)

# Only keep stores that are in the United States. Dataset provided worldwide locations. 
df3 <- subset(df2, Country=="US")

# Verify that only US locations were kept. 
nrow(df3[df3$Country!="US",])

#########################################################################################################
############################################# First Method  #############################################
#########################################################################################################
# Is the mean number of Starbucks in the US a good measure for the number of Starbucks across the US?

# Average number of Starbucks across the US.
meanStarbucks <- nrow(df3)/nrow(starbucksByState)

# Histogram of "Number of Starbucks by State". G1.
ggplot(df3, aes(x=df3$`State/Province`, col=df3$`State/Province`, fill=df3$`State/Province`)) + 
  geom_histogram(stat="count") +  
  theme(axis.text.x = element_text(angle = -90), legend.position = "none") + 
  labs(x = "State", y = "Number of Starbucks", title = "Number of Starbucks by State", caption = "based on data from: https://www.kaggle.com/starbucks/store-locations")

# Histogram of "Number of Starbucks by State" with mean line. G2. 
ggplot(df3, aes(x=df3$`State/Province`, col=df3$`State/Province`, fill=df3$`State/Province`)) + 
  geom_histogram(stat="count") +  
  theme(axis.text.x = element_text(angle = -90), legend.position = "none") + 
  labs(x = "State", y = "Number of Starbucks", title = "Number of Starbucks by State", caption = "based on data from: https://www.kaggle.com/starbucks/store-locations") +
  geom_hline(yintercept=meanStarbucks)

# Accuracy for first method. 
# No states match this mean! Closest results are: MA has 262, MD has 252, MI has 272, NV has 249, and NJ has 249. 
method1Result <- nrow(starbucksByState[starbucksByState$count==meanStarbucks,])/nrow(starbucksByState)
method1Result

#########################################################################################################
############################################# Second Method  ############################################
#########################################################################################################
# Does removing outliers and looking inside one standard devaiation improve our chances at predicting 
# the number of Starbucks in an area?

# Bowplot to determine outliers. G4.
ggplot(starbucksByState, aes(y=starbucksByState$count)) + 
  geom_boxplot(outlier.colour = "red", show.legend = NA, outlier.shape = 8, outlier.size = 5) +  
  theme(legend.position = "none") + 
  labs(y = "Number of Starbucks", title = "Distribution of Starbucks in United States", caption = "based on data from: https://www.kaggle.com/starbucks/store-locations")

# Standard deviation.
allSD <- sd(starbucksByState$count)
allSD

# Data without outliers: CA, TX, WA is 173.17. 
sb1 <- subset(starbucksByState, State_Province!="CA" & State_Province!="TX" & State_Province!="WA")
sb1

# Mean of data without outliers. 
meanSB1 <- mean(sb1$count)
meanSB1

# Standard deviation of data without outliers. 
sdSB1 <- sd(sb1$count)
sdSB1

# Upper and lower limits.
upSB1 <- meanSB1 + sdSB1
upSB1

loSB1 <- meanSB1 - sdSB1
loSB1

# Histogram of "Number of Starbucks by State" within one standard deviation (93.05,439.39). G5.
ggplot(sb1, aes(x=sb1$State_Province, y=sb1$count, col=sb1$State_Province, fill=sb1$State_Province)) + 
  geom_col() +  
  theme(axis.text.x = element_text(angle = -90), legend.position = "none") + 
  labs(x = "State", y = "Number of Starbucks", title = "Number of Starbucks by State", caption = "based on data from: https://www.kaggle.com/starbucks/store-locations") +
  geom_hline(yintercept = upSB1) +
  geom_hline(yintercept = loSB1)

# Boxplot for "Distribution of Starbucks in United States without CA, TX, and WA". G6.
ggplot(sb1, aes(y=sb1$count)) + 
  geom_boxplot(outlier.colour = "red", show.legend = NA, outlier.shape = 8, outlier.size = 5) +  
  theme(legend.position = "none") + 
  labs(y = "Number of Starbucks", title = "Distribution of Starbucks in United States without CA, TX, WA", caption = "based on data from: https://www.kaggle.com/starbucks/store-locations")


# Accuracy for second method. 
method2Result <- nrow(sb1[sb1$count<=upSB1 & sb1$count>=loSB1,])/nrow(starbucksByState)
method2Result

#########################################################################################################
############################################# Third Method  #############################################
#########################################################################################################
# Linear Regression
# Can I use linear regression on lat/long to predict Starbucks locations?

# Map showing "Linear Regression on Starbucks locations". G9.
ggplot(df3, aes(x=Longitude, y=Latitude)) + 
  geom_smooth(method="lm") + 
  geom_point() +  
  labs(x="Longitude", y = "Latitude", title = "Linear Regression on Starbucks locations", caption = "based on data from: https://www.kaggle.com/starbucks/store-locations")

# Correlation between latitude and longitude.
cor(df3$Longitude, df3$Latitude)

# Creating the linear model. 
linearMod <- lm(df3$Longitude ~ df3$Latitude, data=df3)
summary(linearMod)

#########################################################################################################
############################################# More data  ################################################
#########################################################################################################
# By using more data than what is provided, we may be able to more advanced techniques with better
# results. 

# Used Splunk to combine files into a single file. Sources for the file are from several different
# locations. 
addData <- read_csv("Documents/IDS4934/capstone/addData.csv")

#########################################################################################################
############################################# Fourth Method  ############################################
#########################################################################################################
# Multivariate Linear Regression
# Can I use multivariate linear regression on the additional data to predict Starbucks locations?

# First run to see which features are relevant. 
linFit <- lm(addData$starbucks ~ addData$income + addData$numUni + addData$population + addData$crime, data=addData)
summary(linFit)

# Determine the RMSE of the linear model. 
linFitRMSE <- sqrt(mean(linFit$residuals^2))
linFitRMSE

# First run to see which features are relevant. 
linFit2 <- lm(addData$starbucks ~ addData$income + addData$numUni + addData$population, data=addData)
summary(linFit2)

# Determine the RMSE of the linear model. 
linFit2RMSE <- sqrt(mean(linFit2$residuals^2))
linFit2RMSE

#########################################################################################################
############################################# Fifth Method  #############################################
#########################################################################################################
# Logistic Regression
# Can I use logistic regression on the additional data to predict Starbucks locations?

# First logistic model including all fields in additional data. 
logmod1 <- glm(addData$starbucks ~ addData$income + addData$numUni + addData$population + addData$crime, data=addData)
summary(logmod1) # display results

# Calculate RMSE for the first logistic model.
logmod1RMSE <- sqrt(mean(logmod1$residuals^2))
logmod1RMSE

# Second logistic model, excluding crime rate from the additional data. 
logmod2 <- glm(starbucks ~ income + numUni + population, data=addData)
summary(logmod2)

# Calculate RMSE for the second logistic model. 
logmod2RMSE <- sqrt(mean(logmod2$residuals^2))
logmod2RMSE

# Third logistic model (Bayesian Generalized Linear Model), excludes crime rate from additional data.
# Also performs ten fold cross validation on data. 
ControlParameters <-trainControl(method="repeatedcv", 
                                 number=10,
                                 repeats=10)

logmod3 <-train(starbucks ~ income + numUni + population, 
                   data=addData,
                   method='bayesglm',
                   trControl= ControlParameters
)
logmod3
logmod3RMSE <- 201.2931

###########################################################################################################
############################################# Sixth Method  ###############################################
###########################################################################################################
# Support Vector Machines (SVM) Models
# Can I use an SVM model on the new data to predict Starbucks locations?

# Put data into a dataframe. 
df4 <- as.data.frame(addData[2:5])

# Create a training and testing set. 
svm_size <- round(.8 * dim(df4)[1])
svm_train <- df4[1:svm_size,]
svm_test <- df4[-(1:svm_size),]

# First SVM model with C=1, and ten fold cross validation. 
svmMod = svm(df4$starbucks ~ df4$income + df4$numUni + df4$population, data = df4, cost=10, epsilon=1, cross=10, scale=F, kernal='radial')
print(svmMod)

# Results of training set. 
svmModPred <- predict(svmMod, svm_train)
errval <- df4$starbucks - svmModPred
svm_RMSE <- RMSE(errval, obs=df4$starbucks)
print(paste('SVM RMSE: ', svm_RMSE))

# Grid search for best Epsilon and Cost values. Made with help from: https://rpubs.com/richkt/280840
tuneResult1 <- tune(svm, df4$starbucks ~ df4$income + df4$numUni + df4$population,  data = df4,
                    ranges = list(epsilon = seq(0,1,0.01), cost = seq(0.01,5,0.05))
)

# Map tuning results
plot(tuneResult1)

# Continuation of Grid Search.
tuneResult <- tune(svm, df4$starbucks ~ df4$income + df4$numUni + df4$population,  data = df4,
                   ranges = list(epsilon = seq(tuneResult1$best.model$epsilon*1.01,
                                               tuneResult1$best.model$epsilon*1.1,
                                               length.out = 10), 
                                 cost = seq(tuneResult1$best.model$cost-1,
                                            tuneResult1$best.model$cost+1,
                                            length=10)))

plot(tuneResult)
print(tuneResult)

# Final SVM model with values from tuneResult.
svmModTuned = svm(df4$starbucks ~ df4$income + df4$numUni + df4$population, data = df4, cost=tuneResult$best.parameters$cost, epsilon=tuneResult$best.parameters$epsilon, cross=10, scale=F, kernal='radial')
print(svmModTuned)

# Results of training set. 
svmModPredTuned <- predict(svmModTuned, svm_train)
errvalTuned <- df4$starbucks - svmModPredTuned
svm_RMSETuned <- RMSE(errvalTuned, obs=df4$starbucks)
print(paste('Tuned SVM RMSE: ', svm_RMSETuned))

###########################################################################################################
########################################### Results from Models ###########################################
###########################################################################################################

# Create a dataframe for the model results. 
resultsDF <- data.frame(model=c(linFitRMSE, 
                                   linFit2RMSE, 
                                   logmod1RMSE, 
                                   logmod2RMSE, 
                                   logmod3RMSE, 
                                   svm_RMSE, 
                                   svm_RMSETuned), 
                        seq=1:7,
                        names=c("Linear Model (all features)", 
                                "Linear Model (without crime)", 
                                "GLM (all features)", 
                                "GLM (without crime)", 
                                "Bayesian GLM (without crime)",
                                "SVM (without crime)",
                                "SVM (without crime, tuned)"))

# Graph of models and RMSE. 
ggplot(resultsDF, aes(names, model, col=names)) + 
  geom_point(size=6) +
  labs(x="Model Number", y = "RMSE", title = "Model Performance") +
  theme(axis.text.x = element_text(angle = -45))