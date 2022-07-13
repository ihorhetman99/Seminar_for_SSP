library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)
library(class)
library(e1071)
library(ROCR)
library(PRROC)

#min-max normalisation
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#z-score 
z_score_norm = function(x){
  (x - mean(x))/sd(x) 
}


#reading data
setwd('D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data')

train = read.csv('train/functionals_train_all.csv')
test = read.csv('test/functionals_test_all.csv')

#preprocessing
train$diagnosis = as.numeric(train$diagnosis)
train$gender = as.numeric(ifelse(train$gender == "male", 1, 0))

test$diagnosis = as.numeric(test$diagnosis)


set.seed(123)
features = c("F2frequency_sma3nz_amean", 
             "HNRdBACF_sma3nz_amean", 
             "F0semitoneFrom27.5Hz_sma3nz_percentile80.0", 
             "F0semitoneFrom27.5Hz_sma3nz_amean",
             "F1frequency_sma3nz_amean",
             "gender", "diagnosis")

train = train[features]
test = test[features]


#logistic regression
model_lr = glm(diagnosis~F0semitoneFrom27.5Hz_sma3nz_percentile80.0 + 
                 F1frequency_sma3nz_amean + 
                 HNRdBACF_sma3nz_amean + 
                 F0semitoneFrom27.5Hz_sma3nz_amean,
               data = train,
               family = binomial)

probabilities <- model_lr %>% predict(test, type = "response")
predicted.classes <- as.factor(ifelse(probabilities < 0.5, "0", "1"))

summary(model_lr)$coef
confusionMatrix(predicted.classes, as.factor(test$diagnosis))

#AUC curve
pred <- prediction(probabilities, train$diagnosis)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = T, main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

#finding best subset 
base.mod = glm(diagnosis~1, data = train, family = binomial)
all.mod = glm(diagnosis~., data = train, family = binomial)

stepMod = step(base.mod, scope = list(lower = base.mod, upper = all.mod),
               direction = 'both', trace = 0, steps = 1000)

shortlistedVars <- names(unlist(stepMod[[1]])) 
shortlistedVars <- shortlistedVars[!shortlistedVars %in% "(Intercept)"] # remove intercept
print(shortlistedVars)

#random forest model
model_rf = randomForest(as.factor(diagnosis)~
                          F0semitoneFrom27.5Hz_sma3nz_percentile80.0+
                          F0semitoneFrom27.5Hz_sma3nz_amean+
                          F1frequency_sma3nz_amean+
                          HNRdBACF_sma3nz_amean
                        ,
                        data = train,
                        importance = TRUE)
model_rf
predTrain <- predict(model_rf, test, type = "class")
confusionMatrix(predTrain, as.factor(test$diagnosis))
importance(model_rf)


#kNN model
train[1:5] = as.data.frame(lapply(train[1:5], min_max_norm))
test[1:5] = as.data.frame(lapply(test[1:5], min_max_norm))

enum.choose <- function(x, k) {
  if(k > length(x)) stop('k > length(x)')
  if(choose(length(x), k)==1){
    list(as.vector(combn(x, k)))
  } else {
    cbn <- combn(x, k)
    lapply(seq(ncol(cbn)), function(i) cbn[,i])
  }
}

subsets = 1:6


resulting_df = data.frame(names = character(0L), 
                          k =  numeric(0L),
                          acc = numeric(0L),
                          spec =  numeric(0L),
                          sens = numeric(0L))

paste(names(train), collapse = ' + ')
for (num in 1:6) {
  indexes = enum.choose(subsets, num)
  for (i in 1:length(indexes)) {
    acc = c()
    sens = c()
    spec = c()
    for (k in 1:10) {
      model_kNN = knn(train[indexes[[i]]], 
                      test[indexes[[i]]], 
                      cl = train$diagnosis, 
                      k = k)
      cm = confusionMatrix(model_kNN, as.factor(test$diagnosis))
      acc = c(acc, cm$overall[1])
      spec = c(spec, cm$byClass[2])
      sens = c(sens, cm$byClass[1])
      
      row = c(paste(indexes[[i]], collapse = ' + '), k, acc, spec, sens)
      resulting_df =  rbind(resulting_df, row)
    }
  }
}
colnames(resulting_df) = c('Names', 'k', 'Acc', 'Spec', 'Sens')
View(resulting_df)
resulting_df%>%filter(resulting_df$Acc == max(resulting_df$Acc ))


model_kNN = knn(train[c(2, 3, 4)], 
                test[c(2, 3, 4)], 
                cl = train$diagnosis, 
                k = 3)
cm = confusionMatrix(model_kNN, as.factor(test$diagnosis))
cm



#SVM
model_svm_radial = svm(formula = as.factor(diagnosis) ~.,
                       data = train[c(1, 3, 4, 7)],
                       type = 'C-classification',
                       kernel = 'radial',
                       gamma = 2,
                       cost = 3)
y_prel_radial = predict(model_svm_radial, newdata = test[c(1, 3, 4)])
confusionMatrix(y_prel_radial, as.factor(test$diagnosis))
