# KAGGLE COMPETITION 
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
require(corrplot)
require(Rtsne)
require(stats)
require(knitr)
require(ggplot2)
knitr::opts_chunk$set(cache=TRUE)
library(caTools)
library(randomForest)
library(ROCR)

#as.numeric.factor <- function(x) {seq_along(levels(x))[x]}


# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.
# Let's start by reading the data into R    ,na.strings=c("NA","")
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer

#train = read.csv("/Users/bekterra/Desktop/edX/Kaggle Competition/train2016.csv",stringsAsFactors=FALSE)


                                    ##### Loading dataset #####
colClasses = c("numeric"); names(colClasses) = c("YOB")
train = read.csv("train2016.csv")
test = read.csv("test2016.csv")
check <- read.csv("train2016.csv")
str(train)
summary(train)
str(new_Traindata)

                                    #### DATA PREPARATION ####
# Data coversion with stringsAsFactors = FALSE ####
train <- read.csv("train2016.csv", header=T, na.strings=c("", "NA"), stringsAsFactors = FALSE)
test = read.csv("test2016.csv", header=T, na.strings=c("", "NA"), stringsAsFactors = FALSE)

genderLevels <- c("", "Female", "Male") 
train$Gender <- factor(train$Gender, levels=genderLevels, ordered=TRUE) 
train$Gender <- as.numeric(train$Gender) 
train$Gender[train$Gender==1] <- NA

incomeLevels <- c("", "under $25,000", "$25,001 - $50,000", "$50,000 - $74,999", "$75,000 - $100,000", "$100,001 - $150,000", "over $150,000") 
train$Income <- factor(train$Income, levels=incomeLevels, ordered=TRUE)
train$Income <- as.numeric(train$Income) 
train$Income[train$Income==1] <- NA

householdLevels <- c("", "Domestic Partners (no kids)", "Domestic Partners (w/kids)", "Married (no kids)", "Married (w/kids)", "Single (no kids)", "Single (w/kids)") 
train$HouseholdStatus <- factor(train$HouseholdStatus, levels=householdLevels, ordered=TRUE) 
train$HouseholdStatus <- as.numeric(train$HouseholdStatus) 
train$HouseholdStatus[train$HouseholdStatus==1] <- NA

EducationLevels <- c("", "Current K-12", "High School Diploma", "Current Undergraduate", "Associate's Degree", "Bachelor's Degree", "Master's Degree", "Doctoral Degree") 
train$EducationLevel <- factor(train$EducationLevel, levels=EducationLevels, ordered=TRUE) 
train$EducationLevel <- as.numeric(train$EducationLevel) 
train$EducationLevel[train$EducationLevel==1] <- NA

library(plyr) 
train$Party <- revalue(train$Party, c("Democrat"="0", "Republican"="1")) 
train[,7]<-as.numeric(train[,7])

library(mice)
imputevars <- c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel") 
imputeddata <- train[imputevars] 
imputedtrain <- complete(mice(imputeddata))

train$YOB <- imputedtrain$YOB 
train$Gender <- imputedtrain$Gender 
train$Income <- imputedtrain$Income 
train$EducationLevel <- imputedtrain$EducationLevel 
train$HouseholdStatus <- imputedtrain$HouseholdStatus

# Questions variables conversion
train[,8:108][train[,8:108] == "Yes"] = 1
train[,8:108][train[,8:108] == "No"] = -1
train[,8:108][train[,8:108] == ""] = 0
train[,8:108] = sapply(train[,8:108], as.numeric)
train[,8:108][is.na(train[,8:108])] = 0

string.train <- train

# Data conversion ####
library(readr)
# Converting train dataset Questions

for( cn in colnames(train) ) 
{
  if ( substr(cn, 1, 1) != "Q" ) next
  lvls <- levels( train[,cn] )
  if ( length(lvls) != 3 ) next  # Paranoia
  train[,cn] <- as.integer( factor(train[,cn], levels=c(lvls[2],lvls[1],lvls[3])) ) - 2
}

# Converting test dataset Questions
for( cn in colnames(test) ) {
  if ( substr(cn, 1, 1) != "Q" ) next
  lvls <- levels( test[,cn] )
  if ( length(lvls) != 3 ) next  # Paranoia
  test[,cn] <- as.integer( factor(test[,cn], levels=c(lvls[2], lvls[1], lvls[3])) ) - 2
}


# Factor conversion to numeric
#MyData<-as.data.frame( apply(MyData,2,as.factor))
train[,3:7] <- sapply( train[,3:7], as.numeric )
test[,3:6] <- sapply( test[,3:6], as.numeric )

# Replacing 1 (which is missing value) with NA
train[,3:6][train[,3:6]==1] <- NA
str(test)
test[,3:6][test[,3:6]==1] <- NA

# Predictor conversion from 1 and 2 to 1 and 0
train$Party[train$Party==1] <- 0
train$Party[train$Party==2] <- 1

# Get rid of the ridiculous ages ####
train$YOB[train$YOB < 1910] = NA
train$YOB[train$YOB > 2010] = NA
test$YOB[test$YOB < 1910] = NA
test$YOB[test$YOB > 2010] = NA


# correlations
cor(train)

#Dimensions
dim(train)
dim(test)
names(train)
str(train)
str(train[,2:108])

# MICE imputation on demographic data ####
#(complete(mice(joined_cleansed_set)) 

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train,2,pMiss)
apply(train,1,pMiss)

library("rrcovNA")
vars.for.imputation = c("YOB","Gender","Income","HouseholdStatus","EducationLevel")
#library(VIM)
#aggr_plot <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

library(mice)
set.seed(144)
imputedTrainDemo <- complete(mice(train[,vars.for.imputation]))
train[,2:6] <- imputedTrainDemo
summary(train)
str(train)

set.seed(144)
imputedTestDemog <- complete(mice(test[,vars.for.imputation]))
test[,2:6] <- imputedTestDemog

### CONVERT THE FACTOR TO DUMMIES ####
str(train)
names(train)
str(ImpVar.df)

#For every unique value in the string column, create a new 1/0 column
#This is what Factors do "under-the-hood" automatically when passed to function requiring numeric data
#load library
library(dummies)
#create a dummy data frame
new_Traindata <- dummy.data.frame(train, names = c("Gender","Income","HouseholdStatus","EducationLevel"))
new_Testdata <- dummy.data.frame(test, names = c("Gender","Income","HouseholdStatus","EducationLevel"))

new_Traindata$AgeGroups <- cut(new_Traindata$YOB, breaks=8)
new_Testdata$AgeGroups <- cut(new_Testdata$YOB, breaks=8)

new_Traindata$YOB <- new_Traindata$AgeGroups.train
new_Testdata$YOB <- new_Testdata$AgeGroups.test

summary(new_Traindata)
new_Traindata <- train
new_Testdata<- test
names(new_Traindata)
new_Traindata$YOB[new_Traindata$YOB>=1928 & new_Traindata$YOB<=1937] <- 1
new_Traindata$YOB[new_Traindata$YOB>1937 & new_Traindata$YOB<=1947] <- 2
new_Traindata$YOB[new_Traindata$YOB>1947 & new_Traindata$YOB<=1956] <- 3
new_Traindata$YOB[new_Traindata$YOB>1956 & new_Traindata$YOB<=1966] <- 4
new_Traindata$YOB[new_Traindata$YOB>1966 & new_Traindata$YOB<=1975] <- 5
new_Traindata$YOB[new_Traindata$YOB>1975 & new_Traindata$YOB<=1984] <- 6
new_Traindata$YOB[new_Traindata$YOB>1984 & new_Traindata$YOB<=1994] <- 7
new_Traindata$YOB[new_Traindata$YOB>1994 & new_Traindata$YOB<=2003] <- 8

new_Testdata$YOB[new_Testdata$YOB>=1928 & new_Testdata$YOB<=1937] <- 1
new_Testdata$YOB[new_Testdata$YOB>1937 & new_Testdata$YOB<=1947] <- 2
new_Testdata$YOB[new_Testdata$YOB>1947 & new_Testdata$YOB<=1956] <- 3
new_Testdata$YOB[new_Testdata$YOB>1956 & new_Testdata$YOB<=1966] <- 4
new_Testdata$YOB[new_Testdata$YOB>1966 & new_Testdata$YOB<=1975] <- 5
new_Testdata$YOB[new_Testdata$YOB>1975 & new_Testdata$YOB<=1984] <- 6
new_Testdata$YOB[new_Testdata$YOB>1984 & new_Testdata$YOB<=1994] <- 7
new_Testdata$YOB[new_Testdata$YOB>1994 & new_Testdata$YOB<=2003] <- 8



new_Traindata$YOB <- NULL
new_Testdata$YOB <- NULL

new_Traindata <- dummy.data.frame(new_Traindata, names = c("YOB"))
new_Testdata <- dummy.data.frame(new_Testdata, names = c("YOB"))




# Splitting Train set into training and validation set
spl <- sample.split(new_Traindata$Party,SplitRatio = 0.75)
training <- subset(new_Traindata,spl==TRUE)
testing <- subset(new_Traindata,spl==FALSE)

trainindex <- 1:4176
trainset <- new_Traindata[trainindex, ]
testset <- new_Traindata[-trainindex, ]

# Variable importance ####
# load the library
library(mlbench)
library(party)
library(FSelector)
set.seed(144)
require(randomForest)
new_Traindata$Party <- as.factor(new_Traindata$Party)
att.scores <- random.forest.importance(Party ~ . - USER_ID , new_Traindata)
plot(att.scores)
cutoff.biggest.diff(att.scores)
varImpPlot(att.scores)
cutoff.k(att.scores, k = 20)

##################
library(Hmisc)
library(randomForest)
str(new_Traindata)
x <- new_Traindata[,c(2:23,25:125)]
y <- new_Traindata[,24]

normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)
subsets <- c(1:5, 10, 15, 20, 25)

set.seed(10)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x, y,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile

# Predictors' list
a<- predictors(lmProfile)
a

#plot of chunk rfe_lmprofile
trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))

# Important Variables 
ImpVar <- c("YOB","Gender","Income","HouseholdStatus","EducationLevel","Party","Q109244","Q115611","Q98197","Q98059","Q116881","Q120379","Q101163","Q118232")
str(ImpVar)
ImpVar.df <- data.frame(new_Traindata[,ImpVar])


ImpVar.df$Income <- train$Income


var.df <- data.frame(train[,ImpVar])
str(var.df)

ImpVar_test <- c("YOB","Gender","Income","HouseholdStatus","EducationLevel","Q109244","Q115611","Q98197","Q98059","Q116881","Q120379","Q101163","Q118232")
var.df_test <- data.frame(test[,ImpVar_test])

a.df <- c("Q109244","Q115611","Q113181","Party")

size = 10000
np.random.seed(seed=10)
X_seed = np.random.normal(0, 1, size)



library(rpart)
fit=rpart(factor(Party)~ . -USER_ID, new_Traindata)
plot(fit)
text(fit)
tmp=rownames(fit$splits)
allVars=colnames(attributes(fit$terms)$factors)  
rownames(fit$splits)=1:nrow(fit$splits)
splits=data.frame(fit$splits)
splits$var=tmp
splits$type=""
frame=as.data.frame(fit$frame)
index=0
for(i in 1:nrow(frame)){
    if(frame$var[i] != "<leaf>"){
       index=index + 1
       splits$type[index]="primary"
       if(frame$ncompete[i] > 0){
         for(j in 1:frame$ncompete[i]){
           index=index + 1
           splits$type[index]="competing"}}
       if(frame$nsurrogate[i] > 0){
         for(j in 1:frame$nsurrogate[i]){
              index=index + 1
              splits$type[index]="surrogate"}}}}
splits$var=factor(as.character(splits$var))
splits=subset(splits, type != "surrogate")
out=aggregate(splits$improve,list(Variable = splits$var),sum, na.rm = TRUE)
allVars=colnames(attributes(fit$terms)$factors)
if(!all(allVars %in% out$Variable)){
     missingVars=allVars[!(allVars %in% out$Variable)]
     zeros=data.frame(x = rep(0, length(missingVars)), Variable = missingVars)
     out=rbind(out, zeros)}
out2=data.frame(Overall = out$x)
rownames(out2)=out$Variable
out2

out2[,1:15]





                                    #### MODEL BUILDING ####

# Splitting data into train and validation sets ####
library(caTools)
set.seed(123)
var.df <- train[,var13]
spl <- sample.split(var.df$Party, SplitRatio = 0.75)
TrainSet <- subset(var.df,spl == TRUE)
ValidationSet <- subset(var.df,spl == FALSE)

var13 <- c("YOB","Gender","Party","Q109244","Q115611","Q98197","Q113181","Q101163")
var13_test <- c("USER_ID","YOB","Gender","Q109244","Q115611","Q98197","Q113181","Q101163")


# Logistic Regression ####
# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:
as.factor(predictors(lmProfile))
var.featured <- c("Q109244","Q115611","Q98197","Q98059","Q116881","Q120379","Q113181","Q101163","Q115390","Q119851","Income", 
                  "Q110740", "Q99480","Q98869","Q100689","Q116953","Q106997","Q115899","Q118232","Q120472","Q124742","Q121699",
                  "Q122771","YOB","Q104996")

SelectedVariables<- train[var.featured]

SelectedVariables$Party <- train[,7]

train$Party[train$Party==1] <- 0
train$Party[train$Party==2] <- 1

var13 <- c("YOB","Gender","Party","Q109244","Q115611","Q98197","Q113181","Q101163")
var13_test <- c("YOB","Gender","Q109244","Q115611","Q98197","Q113181","Q101163")

SimpleMod = glm(Party ~ . , data=train[,a.df], family=binomial)
summary(SimpleMod)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

 # Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "/Users/bekterra/Desktop/edX/Kaggle Competition/SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!



# Model Building using H2O ####
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()

# transfering data to h2o cluster
train.h2o <- as.h2o(SelectedVariables)
test.h2o <- as.h2o(test)

#check column index number
colnames(train.h2o)

#dependent variable (Purchase)

y.dep <- 26

#independent variables (dropping ID variables)
x.indep <- c(1:25)

#GBM in H2O
#GBM
system.time(gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o, ntrees = 2000, max_depth = 4, learn_rate = 0.01, seed = 1122))
h2o.performance (gbm.model)

#making prediction and writing submission file
predict.gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
threshold = 0.5
PredTestLabels = as.factor(ifelse(predict.gbm$predict < threshold, "Democrat", "Republican"))


sub_gbm <- data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(sub_gbm, "/Users/bekterra/Desktop/edX/Kaggle Competition/SubmissionGBM.csv", row.names=FALSE)

ROCRpred = prediction(predict.gbm, test)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Deep learning models
system.time(dlearning.model <- h2o.deeplearning(y = y.dep,
                                                x = x.indep,
                                                training_frame = train.h2o,
                                                epoch = 60,
                                                hidden = c(100,100),
                                                activation = "Rectifier",
                                                seed = 1122
)
)

h2o.performance(dlearning.model)

#making predictions
predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))
threshold = 0.5
PredTestLabels = as.factor(ifelse(predict.dl2$predict < threshold, "Democrat", "Republican"))
predict.dl2

#create a data frame and writing submission file
sub_dlearning <- data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(sub_dlearning, "/Users/bekterra/Desktop/edX/Kaggle Competition/Submissiondlearning.csv", row.names=FALSE)

sub_gbm <- data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(sub_gbm, "/Users/bekterra/Desktop/edX/Kaggle Competition/SubmissionGBM.csv", row.names=FALSE)


# GLM Net Model ####
library(caTools)  # for splitting data into train and test set
library(glmnet)  # for ridge regression
library(ROCR)  # for finding the AUC value of our model
showofhands = var.df
summary(showofhands)
set.seed(123)
split = sample.split(showofhands$Party, SplitRatio = 0.75)
train.glm = subset(showofhands, split == TRUE)
test.glm = subset(showofhands, split == FALSE)
dim(train.glm)
dim(test.glm)
x = model.matrix(Party ~ . , data = train.glm)
y = as.numeric(train.glm$Party)
x.test = model.matrix(Party ~ . , data = test.glm)
dim(x)
dim(y)
ridge = glmnet(x, y, alpha = 0)
plot(ridge, xvar = "lambda", label = TRUE)

cv.ridge = cv.glmnet(x, y, alpha = 0)
plot(cv.ridge)

coefi = coef(cv.ridge)
predict = (1/(1 + exp(-(x.test[, ] %*% coefi[2:30, ]))))
summary(predict)

ROCRpredTest = prediction(predict, test.glm$Party)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

testdata = test
testdata$Party = 0
x.submit = model.matrix(Party ~ . - 1 - USER_ID, data = testdata)

predictiontest = (1/(1 + exp(-(x.submit[, ] %*% coefi[2:30, ]))))
summary(predictiontest)
threshold = 0.5
PredTestLabels = as.factor(ifelse(predictiontest < threshold, "Democrat", "Republican"))
table(PredTestLabels)

sub_glm_Ridge <- data.frame(USER_ID = testdata$USER_ID, Predictions = PredTestLabels)
write.csv(sub_glm_Ridge, "/Users/bekterra/Desktop/edX/Kaggle Competition/submissionRidge.csv", row.names=FALSE)

# GBM ####
library(gbm)
library(doMC)
registerDoMC(cores = 2)
predictorsNames <- c(2:6,8:108)
outcomeName <- 7
# CV 
objControl <- trainControl(method='cv', number=5, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel <- train(new_Traindata[,predictorsNames], new_Traindata[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,
                  preProc = c("center", "scale"))


PartyGBM = gbm(Party ~.-USER_ID, data=new_Traindata,
               distribution="bernoulli",
               n.trees=5000,
               shrinkage=0.01,
               verbose=FALSE,
               cv.folds=10,
               n.cores=2,
               interaction.depth=1)

#making prediction and writing submission file
testPred <- predict(PartyGBM, new_Testdata)

test_results <- predict(PartyGBM, new_Testdata, type = "response")
test_results$obs <- new_Testdata$Party
head(test_results)


threshold = 0.5
PredTestLabels = as.factor(ifelse(predGBM < threshold, "Democrat", "Republican"))

sub_gbm <- data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(sub_gbm, "/Users/bekterra/Desktop/edX/Kaggle Competition/SubmissionGBM.csv", row.names=FALSE)

#AUC (area under the curve)
test$Party <- 0
library(ROCR)
ROCRgbm = prediction(predictions,test$Party)
as.numeric(performance(ROCRgbm, "auc")@y.values)


# Ensemble learning ####
# Load libraries
library(mlbench)
library(caret)
library(caretEnsemble)

# Load the dataset
dataset <- train

# Example of Boosting Algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(144)
fit.c50 <- train(Party~., data=train, method="C5.0", trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Party~., data=train, method="gbm", trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

# Example of Bagging algorithms
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
fit.treebag <- train(Party~., data=train, method="treebag",  trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(Party~., data=train, method="rf", trControl=control)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)

# Example of Stacking algorithms
# create submodels
control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(seed)
models <- caretList(Party~., data=train, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)


# correlation between results
modelCor(results)
splom(results)

# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(seed)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)

# stack using random forest
set.seed(seed)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)




                                      # H2O ENSEMBLE #

# H2O Ensemle (Super learning) ####
# An example of binary classification using h2o.ensemble
# This example is also included in the R package documentation for h2o.ensemble
install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")

library(devtools)
library(h2oEnsemble)
library(SuperLearner)  # For metalearner such as "SL.glm"
library(cvAUC)  # Used to calculate test set AUC (requires version >=1.0.1 of cvAUC)

# Start an H2O cluster with nthreads = num cores on your machine
 # Clean slate - just in case the cluster was already running
library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
h2o.removeAll()

# transfering data to h2o cluster
train.h2o <- as.h2o(new_Traindata[,a.df])
valid.h2o <- as.h2o(ValidationSet)
test.h2o <- as.h2o(new_Testdata)

# Convert R data.frames into H2O parsed data objects
y <- "Party"
x <- setdiff(names(train.h2o), y)
family <- "binomial"

# which means that H2O will train a regression model instead
train.h2o[,y] <- as.factor(train.h2o[,y])  #encode the binary repsonse as a factor
test.h2o[,y] <- as.factor(test.h2o[,y])
h2o.levels(train$Party) 

# Partition the data into training, validation and test sets ####
splits <- h2o.splitFrame(data = train.h2o, 
                         ratios = c(0.75),  #partition data into 70%, 15%, 15% chunks
                         seed = 1)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
validation <- splits[[2]]
test <- test.h2o


str(train.h2o[,y])

# Create a custom base learner library & specify the metalearner
#Specify Base Learners & Metalearner
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper", 
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"

# Train Ensemble fit ####
fit <- h2o.ensemble(x = x, y = y, 
                   training_frame = train.h2o, 
                   validation_frame = validation,
                   family = "binomial", 
                   learner = learner, 
                   metalearner = metalearner,
                   cvControl = list(V = 5))

#Generate predictions on the test set.
pred <- predict(fit, test.h2o)

summary(pred)
pred
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(test.h2o[,y])[,1]
labels

threshold = 0.5
pred <- predict(fit, new_Testdata)
predictions. <- as.data.frame(pred$pred)[,3]

PredTestLabels = as.factor(ifelse(predictions < threshold, "Democrat", "Republican"))

h20_Ensemble <- data.frame(USER_ID = new_Testdata$USER_ID, Predictions = PredTestLabels)
write.csv(h20_Ensemble, "/Users/bekterra/Desktop/edX/Kaggle Competition/Submission.h20_Ensemble.csv", row.names=FALSE)


#Model Evaluation ####
#Ensemble test set AUC
library(cvAUC)
cvAUC::AUC(predictions = predictions,labels = labels)

#Base learner test set AUC
L <- length(learner)
auc <- sapply(seq(L), function(l) cvAUC::AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels)) 
data.frame(learner, auc)


# Customized base learner
learner <- c("h2o.glm.wrapper",
             "h2o.randomForest.1", "h2o.randomForest.2",
             "h2o.gbm.1", "h2o.gbm.6", "h2o.gbm.8")
metalearner <- "h2o.glm.wrapper"

#Specifying new learners ####
h2o.glm.1 <- function(..., alpha = 0.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.2 <- function(..., alpha = 0.5) h2o.glm.wrapper(..., alpha = alpha)
h2o.glm.3 <- function(..., alpha = 1.0) h2o.glm.wrapper(..., alpha = alpha)
h2o.randomForest.1 <- function(..., ntrees = 200, nbins = 50, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.randomForest.2 <- function(..., ntrees = 200, sample_rate = 0.75, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.3 <- function(..., ntrees = 200, sample_rate = 0.85, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, sample_rate = sample_rate, seed = seed)
h2o.randomForest.4 <- function(..., ntrees = 200, nbins = 50, balance_classes = TRUE, seed = 1) h2o.randomForest.wrapper(..., ntrees = ntrees, nbins = nbins, balance_classes = balance_classes, seed = seed)
h2o.gbm.1 <- function(..., ntrees = 109, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, seed = seed)
h2o.gbm.2 <- function(..., ntrees = 100, nbins = 50, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, nbins = nbins, seed = seed)
h2o.gbm.3 <- function(..., ntrees = 100, max_depth = 10, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.gbm.4 <- function(..., ntrees = 100, col_sample_rate = 0.8, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.5 <- function(..., ntrees = 100, col_sample_rate = 0.7, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.6 <- function(..., ntrees = 109, col_sample_rate = 0.6, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, col_sample_rate = col_sample_rate, seed = seed)
h2o.gbm.7 <- function(..., ntrees = 100, balance_classes = TRUE, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, balance_classes = balance_classes, seed = seed)
h2o.gbm.8 <- function(..., ntrees = 500, max_depth = 3, seed = 1) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth = max_depth, seed = seed)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.3 <- function(..., hidden = c(500,500), activation = "RectifierWithDropout", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.4 <- function(..., hidden = c(500,500), activation = "Rectifier", epochs = 50, balance_classes = TRUE, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, balance_classes = balance_classes, seed = seed)
h2o.deeplearning.5 <- function(..., hidden = c(100,100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.6 <- function(..., hidden = c(50,50), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.7 <- function(..., hidden = c(100,100), activation = "Rectifier", epochs = 50, seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)


learner <- c("h2o.randomForest.1", "h2o.randomForest.2",
             "h2o.gbm.2", "h2o.gbm.6", "h2o.gbm.8")
metalearner <- "h2o.glm.wrapper"

# Modelling with customized learners ####
fit2 <- h2o.ensemble(x = x, y = y, 
                    training_frame = train.h2o,
                    #validation_frame = validation,
                    family = "binomial", 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 7))

pred <- predict.h2o.ensemble(fit2, test.h2o)
labels <- as.data.frame(test.h2o[,c(y)])[,1]
# Ensemble test AUC
AUC(predictions=as.data.frame(pred$pred)[,3], labels=labels)
pred

# To look at the accuracy of the the single models you used in your ensemble:
L <- length(learner)
sapply(seq(L), function(l) AUC(predictions = as.data.frame(pred$basepred)[,l], labels = labels)) 


#predictions on the test set
pred <- predict(fit2, test.h2o)
cor(ImpVar.df)
typeof(ImpVar.df)
pred


predictions <- as.data.frame(pred$pred)[,3] 
labels <- as.data.frame(test.h2o[,y])[,1]
labels

#Ensemble test set AUC
library(cvAUC)
cvAUC::AUC(predictions = predictions,labels = labels)


predictions <- as.data.frame(pred$pred)[,3]
PredTestLabels = as.factor(ifelse(predictions < threshold, "Democrat", "Republican"))



h20_Ensemble <- data.frame(USER_ID = test[,var13_test]$USER_ID, Predictions = PredTestLabels)
write.csv(h20_Ensemble, "/Users/bekterra/Desktop/edX/Kaggle Competition/Submission.h20_Ensemble.csv", row.names=FALSE)

# Variable importance in H2O models
summary(h2o.gbm.1,n.trees=best.iter)




# Shut down h2o session
h2o.shutdown()
