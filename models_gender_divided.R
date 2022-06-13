library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)
library(class)
library(e1071)


setwd('D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data')
train = read.csv('train/extracted_features/audio/training_functionals.csv')
train$diagnosis = as.numeric(train$diagnosis)
train$gender = as.numeric(ifelse(train$gender == " male ", 1, 0))


set.seed(123)
features = c("F2frequency_sma3nz_amean", 
             "HNRdBACF_sma3nz_amean", 
             "F0semitoneFrom27.5Hz_sma3nz_percentile80.0", 
             "F0semitoneFrom27.5Hz_sma3nz_amean",
             "F1frequency_sma3nz_amean",
             "gender", "diagnosis")

train = train[features]

#min-max normalisation
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#z-score 
z_score_norm = function(x){
  (x - mean(x))/sd(x) 
}

summary(train)
train[1:5] = as.data.frame(lapply(train[1:5], min_max_norm))

train[1:5] = as.data.frame(lapply(train[1:5], z_score_norm))


train_male = train[train$gender == 1, ]
train_female = train[train$gender == 0, ]
train_male= train_male[, -6]
train_female= train_female[, -6]

training.samples_male = createDataPartition(y = train_male$diagnosis, 
                                       p = 0.75, list = FALSE)
training.samples_female = createDataPartition(y = train_female$diagnosis, 
                                            p = 0.75, list = FALSE)

train.data_male = train_male[training.samples_male, ]
test.data_male = train_male[-training.samples_male, ]

train.data_female = train_female[training.samples_female, ]
test.data_female = train_female[-training.samples_female, ]


enum.choose <- function(x, k) {
  if(k > length(x)) stop('k > length(x)')
  if(choose(length(x), k)==1){
    list(as.vector(combn(x, k)))
  } else {
    cbn <- combn(x, k)
    lapply(seq(ncol(cbn)), function(i) cbn[,i])
  }
}
subsets = 1:5

#kNN male

indexes = enum.choose(subsets, 5)

for (i in 1:length(indexes)) {
  train.data_knn = train.data_male[, -6]
  test.data_knn = test.data_male[, -6]
  train.data_knn = train.data_male[indexes[[i]]]
  test.data_knn = test.data_male[indexes[[i]]]
  acc = c()
  sens = c()
  spec = c()
  
  print(names(train.data_knn))
  for (k in 1:10) {
    model_kNN = knn(train.data_knn, 
                    test.data_knn, 
                    cl = train.data_male$diagnosis, 
                    k = k)
    cm = confusionMatrix(model_kNN, as.factor(test.data_male$diagnosis))
    acc = c(acc, cm$overall[1])
    spec = c(spec, cm$byClass[2])
    sens = c(sens, cm$byClass[1])
  }
  
  df_res = data.frame(acc, spec, sens)
  print(df_res)
}
#[1] "HNRdBACF_sma3nz_amean"                     
#[2] "F0semitoneFrom27.5Hz_sma3nz_percentile80.0" 

#"F0semitoneFrom27.5Hz_sma3nz_amean" "F1frequency_sma3nz_amean"
#0.8333333 0.8333333 0.8333333, k = 2

#[1] "F0semitoneFrom27.5Hz_sma3nz_percentile80.0" 
#[2] "F0semitoneFrom27.5Hz_sma3nz_amean"         
#[3] "F1frequency_sma3nz_amean" 
#0.8333333 0.8333333 0.8333333, k = 4, 5, 6


#kNN female
indexes = enum.choose(subsets, 5)

for (i in 1:length(indexes)) {
  train.data_knn = train.data_female[, -6]
  test.data_knn = test.data_female[, -6]
  train.data_knn = train.data_female[indexes[[i]]]
  test.data_knn = test.data_female[indexes[[i]]]
  acc = c()
  sens = c()
  spec = c()
  
  print(names(train.data_knn))
  for (k in 1:10) {
    model_kNN = knn(train.data_knn, 
                    test.data_knn, 
                    cl = train.data_female$diagnosis, 
                    k = k)
    cm = confusionMatrix(model_kNN, as.factor(test.data_female$diagnosis))
    acc = c(acc, cm$overall[1])
    spec = c(spec, cm$byClass[2])
    sens = c(sens, cm$byClass[1])
  }
  
  df_res = data.frame(acc, spec, sens)
  print(df_res)
}
#[1] "F2frequency_sma3nz_amean"                  
#[2] "HNRdBACF_sma3nz_amean"                     
#[3] "F0semitoneFrom27.5Hz_sma3nz_percentile80.0"
#[4] "F0semitoneFrom27.5Hz_sma3nz_amean"
# 0.7857143 0.7142857 0.8571429, k = 1

#[1] "F2frequency_sma3nz_amean"                  
#[2] "F0semitoneFrom27.5Hz_sma3nz_percentile80.0"
#[3] "F0semitoneFrom27.5Hz_sma3nz_amean"         
#[4] "F1frequency_sma3nz_amean"   
# 0.7857143 0.7142857 0.8571429, k = 8



#SVM male
model_svm_poly = svm(formula = diagnosis ~.,
                     data = train.data_male,
                     type = 'C-classification',
                     kernel = 'polynomial',
                     degree = 3,
                     gamma = 3,
                     cost = 1
)
y_prel_poly = predict(model_svm_poly, newdata = test.data_male[-6])
confusionMatrix(y_prel_poly, as.factor(test.data_male$diagnosis))

obj <- tune.svm(as.factor(diagnosis)~., data = train.data_male, 
                degree = 1:4,
                gamma = 0:5, 
                cost = 1:5)
summary(obj)
summary(obj)[1]

obj$best.model

model_svm_radial = svm(formula = as.factor(diagnosis) ~.,
                       data = train.data_male,
                       type = 'C-classification',
                       kernel = 'radial',
                       gamma = 1,
                       cost = 3)
y_prel_radial = predict(model_svm_radial, newdata = test.data_male[-6])
confusionMatrix(y_prel_radial, as.factor(test.data_male$diagnosis))


