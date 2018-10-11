~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Decision Trees ##################    
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Loading packages
library(readxl)
library(Hmisc)
library(rpart)
library(rpart.plot)
library(plyr)
library(readxl)


# Loading data in R
organics <- read_excel("C:/Users/Arun/Desktop/BA with R/Homework 3/organics.xlsx")
View(organics)

#Summary statistics of data
summary(organics)
str(organics)
summary(organics$DemGender)


#Converting the NA values in DemGender to 'U' (U <- is unknown, so no point having NA values,hence converting them to 'U')
organics$DemGender[is.na(organics$DemGender)] <- 'U'
describe(organics$DemGender)

#Making ID and targetAmt and DemCluster variable as null
#The variable DemClusterGroup contains collapsed levels of the variable DemCluster. 
#Presume that, based on previous experience, you believe that DemCluster Group is sufficient for this type of modeling effort. 
organics$ID <- NULL
organics$TargetAmt <- NULL
organics$DemCluster <- NULL

# Split the data
library(caTools)
set.seed(42)
spl = sample.split(organics$TargetBuy, SplitRatio = 0.5)
Train = subset(organics, spl==TRUE)
Test = subset(organics, spl==FALSE)

#Proportions indivuduals who purchased organic goods
table(organics$TargetBuy)


table(organics$TargetBuy,organics$DemGender)
  
ggplot(organics, aes(x = TargetBuy, fill = DemGender)) + geom_bar()

#Building the decision tree
targettree <- rpart(TargetBuy ~ ., data = Train, method = "class", parms = list(split="information"))
targettree$cptable


plotcp(targettree)

#Create optimal decision tree
mincp = targettree$cptable[which.min(targettree$cptable[,"xerror"]),"CP"]
#chosen based on plotcp
mincp 

#Pruning the optimal tree selected
targetopttree <- prune(targettree, mincp)
printcp(targetopttree)

## replacing again wiht rpart.plot
rpart.plot(targetopttree)


#Evaluate opt tree performance
targettrain <- testModelPerformance(targetopttree, Train, Train$TargetBuy)
targettest <- testModelPerformance(targetopttree, Test, Test$TargetBuy)


#computing accuracy metrics for decision tree built (Train)
Train$pred <- predict(targetopttree, Train, type = "class")
confMatrix <- table(Actual = Train$TargetBuy, Predicted = Train$pred)
print(confMatrix)

truePos <- confMatrix[2,2]
falseNeg <- confMatrix[2,1]
falsePos <- confMatrix[1,2]
trueNeg <- confMatrix[1,1]

accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
sensitivity <- truePos/(truePos + falseNeg)
specificity <- trueNeg/(falsePos + trueNeg)
falsePosRate <- falsePos/(falsePos + trueNeg)
falseNegRate <- falseNeg/(truePos + falseNeg)
precision <- truePos/(truePos + falsePos)

writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
writeLines(paste("Specificity:", round(specificity, digits = 4)))
writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
writeLines(paste("Precision:", round(precision, digits = 4)))

#computing accuracy metrics for decision tree built (test)
Test$pred <- predict(targetopttree, Test, type = "class")
confMatrix <- table(Actual = Test$TargetBuy, Predicted = Test$pred)
print(confMatrix)

truePos <- confMatrix[2,2]
falseNeg <- confMatrix[2,1]
falsePos <- confMatrix[1,2]
trueNeg <- confMatrix[1,1]

accuracy <- (truePos + trueNeg)/(truePos + falseNeg + falsePos + trueNeg)
sensitivity <- truePos/(truePos + falseNeg)
specificity <- trueNeg/(falsePos + trueNeg)
falsePosRate <- falsePos/(falsePos + trueNeg)
falseNegRate <- falseNeg/(truePos + falseNeg)
precision <- truePos/(truePos + falsePos)

writeLines(paste("Accuracy:", round(accuracy, digits = 4)))
writeLines(paste("Sensitivity:", round(sensitivity, digits = 4)))
writeLines(paste("Specificity:", round(specificity, digits = 4)))
writeLines(paste("False Positive Rate:", round(falsePosRate, digits = 4)))
writeLines(paste("False Negative Rate:", round(falseNegRate, digits = 4)))
writeLines(paste("Precision:", round(precision, digits = 4)))


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Logistic Regression ##################
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Loading data in R
organics <- read_excel("C:/Users/Arun/Desktop/BA with R/Homework 3/organics.xlsx")
View(organics)

#Summary statistics of data
summary(organics)
str(organics)
summary(organics$DemGender)

#Converting the NA values in DemGender to 'U' (U <- is unknown, so no point having NA values,hence converting them to 'U')
organics$DemGender[is.na(organics$DemGender)] <- 'U'
describe(organics$DemGender)


#Making ID and targetAmt and DemCluster variable as null
organics$ID <- NULL
organics$TargetAmt <- NULL
organics$DemCluster <- NULL

#Treating the missing values before splitting them into train and test data
#Delete observations with missing values from DemReg and DemClusterGroup
New_data <- organics[!is.na(organics$DemReg) & !is.na(organics$DemClusterGroup),]

#Imputing DEMAGE variable with mean  
New_data$DemAge <- replace(New_data$DemAge, is.na(New_data$DemAge), mean(na.omit(New_data$DemAge)))

#Imputing DEMAFFL variable with median  
New_data$DemAffl <- replace(New_data$DemAffl, is.na(New_data$DemAffl), mean(na.omit(New_data$DemAffl)))

#Imputing DEMAGE variable with mean  
New_data$PromTime <- replace(New_data$PromTime, is.na(New_data$PromTime), mean(na.omit(New_data$PromTime)))


#Split the data
library(caTools)
set.seed(42)
spl = sample.split(New_data$TargetBuy, SplitRatio = 0.5)
New_Train = subset(New_data, spl==TRUE)
New_Test = subset(New_data, spl==FALSE)


#Running a logistic regression on the organics dataset
Organicslogit <- glm(TargetBuy ~., data = New_Train, family = binomial(link = "logit"))
summary(Organicslogit)

Organicslogit <- glm(TargetBuy ~ DemAffl + DemAge + DemGender + PromTime, data = New_Train, family = binomial(link = "logit"))
summary(Organicslogit)

New_Train$probdonate <- predict(Organicslogit, newdata = New_Train, type = "response")
New_Train$logitpred <- round(New_Train$probdonate)


New_Test$probdonate <- predict(Organicslogit, newdata = New_Test, type = "response")
New_Test$logitpred <- round(New_Test$probdonate)


TrainRes <- testModelPerformance(Organicslogit, New_Train, New_Train$TargetBuy, New_Train$logitpred)
TestRes <- testModelPerformance(Organicslogit, New_Test, New_Test$TargetBuy, New_Test$logitpred)

#Build confidence intervals
confint.default(Organicslogit) 

#Calculate odds ratio
exp(coef(Organicslogit)) 

#Calculate Chi-Square
devdiff <- with(Organicslogit, null.deviance - deviance) #difference in deviance between null and this model
dofdiff <- with(Organicslogit, df.null - df.residual) #difference in degrees of freedom between null and this model
pval <- pchisq(devdiff, dofdiff, lower.tail = FALSE )
paste("Chi-Square: ", devdiff, " df: ", dofdiff, " p-value: ", pval)

#Calculate psuedo R square
New_Train$probsurv <- predict(Organicslogit, newdata = New_Train, type = "response")
resid.dev <- 2 * llh(New_Train$TargetBuy, New_Train$probsurv)
null.dev <- 2 * llh(New_Train$TargetBuy, mean(New_Train$TargetBuy))
pr2 <- 1-(resid.dev/null.dev)
paste("Psuedo R2: ", pr2)



































