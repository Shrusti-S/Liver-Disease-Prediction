#data acquistion
liver<-read.csv("C:\\Users\\Admin\\Downloads\\indian_liver_patient.csv")

#dimension
dim(liver)

#view
View(liver)

#structure
str(liver)

#sum of null values
sum(is.na(liver))

#column sum of null values
colSums(is.na(liver))

#Analysis

#ggplot
library(ggplot2)
print(ggplot(liver,aes(x=Dataset))+geom_bar(color="red")+facet_grid(.~Gender)+
  ggtitle("Result by Gender"))

#histogram
print(hist(as.numeric(unlist(liver)),main="liver",col="red"))

#boxplot
boxplot(data$Total_Bilirubin)
boxplot(data$Direct_Bilirubin)

#corrplot
cr <- cor(liver)
library(corrplot)
corrplot(cr,method = "number")

#data preprocessing

liver$Dataset<-factor(ifelse(liver$Dataset>=mean(liver$Dataset,na.rm=TRUE)
                             ,"positive","negative"))

#missforest

library(missForest)
Dataset_imputed<-missForest(liver,maxiter = 2)
liver<-Dataset_imputed$ximp

#check
sum(is.na(liver))

colSums(is.na(liver))

#data splitting
library(caret)
library(klaR)

split=0.70
trainIndex <- createDataPartition(liver$Dataset, p=split, list=FALSE)
l_train <- liver[ trainIndex,]
l_test <- liver[-trainIndex,]
View(l_train)
View(l_test)

#model building

#model1:logistic regression

logit_model=glm(Dataset~.,family='binomial',l_train)

summary(logit_model)

step(logit_model)

#prediction
logit_predict<-predict(logit_model,l_test,type="response")
logit_predict

#threshold value
res<-ifelse(logit_predict>0.6,"positive","negative")
str(l_test$Dataset)

#confusion matrix
library(caret)
logit_metrics<-confusionMatrix(as.factor(res),as.factor(l_test$Dataset))
logit_metrics
logit_metrics$byClass



#ROCR graph
library("ROCR")
ROCRpredict <- predict(logit_model,l_train,type = "response")
ROCRPredict1<-prediction(ROCRpredict,l_train$Dataset)
ROCRgraph <- performance(ROCRPredict1,measure = "tpr",x.measure = "fpr")                         
plot(ROCRgraph, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))


#model2:decision trees
library(C50)
library(gmodels)

#model building
decision_model <- C5.0(Dataset ~.,l_train)
summary(decision_model)

decision_predict<-predict(decision_model,l_test)
decision_predict

library(caret)
decision_metrics<-confusionMatrix(decision_predict,l_test$Dataset)
decision_metrics
decision_metrics$byClass

#model12: optimization of decision tree
liver_boost<-C5.0(l_train[-11],l_train$Dataset)
summary(liver_boost)

#prediction
liver_boostPrediction<-predict(liver_boost,l_test)

#confusion matrix
decision_metrics<-confusionMatrix(liver_boostPrediction,l_test$Dataset)
decision_metrics
decision_metrics$byClass

#
liver_boost<-C5.0(l_train[-11],l_train$Dataset,trials=10)


#model3: random forest
#load the randomForest package
library(randomForest)

bestmtry <- tuneRF(l_train,l_train$Dataset,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)
random_model <- randomForest(Dataset ~ ., l_train,mtry = 3,ntree = 10)

#prediction
random_predict <- predict(random_model,l_test)

#confusion matrix
library(caret)
random_metrics <- confusionMatrix(random_predict,l_test$Dataset)
random_metrics
random_metrics$byClass

#
bestmtry <- tuneRF(l_train,l_train$Dataset,stepFactor = 1.2,improve = 0.01,trace = T,plot = T)
random_model <- randomForest(Dataset ~ ., l_train,mtry = 3,ntree = 100)













