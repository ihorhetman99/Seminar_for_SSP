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


training.samples = createDataPartition(y = train$diagnosis, 
                                       p = 0.75, list = FALSE)

train.data = train[training.samples, ]
test.data = train[-training.samples, ]


#logistic regression model
model_lr = glm(diagnosis~F2frequency_sma3nz_amean+
              F0semitoneFrom27.5Hz_sma3nz_percentile80.0+
                gender,
            data = train.data,
            family = binomial)

probabilities <- model_lr %>% predict(test.data, type = "response")
predicted.classes <- as.factor(ifelse(probabilities < 0.5, "0", "1"))

summary(model_lr)$coef
confusionMatrix(predicted.classes, as.factor(test.data$diagnosis))

ggplot(data = test.data, aes(y = probabilities, 
                             x = seq(1, length(probabilities)),
                             color = as.factor(diagnosis))
       )+
  geom_point()+
  geom_hline(yintercept = 0.5, linetype="dashed", color = "purple")


#random forest model
model_rf = randomForest(as.factor(diagnosis)~ 
                          F0semitoneFrom27.5Hz_sma3nz_percentile80.0+
                          HNRdBACF_sma3nz_amean+
                          F1frequency_sma3nz_amean
                        ,
                        data = train.data,
                        importance = TRUE)
model_rf
predTrain <- predict(model_rf, test.data, type = "class")
confusionMatrix(predTrain, as.factor(test.data$diagnosis))
importance(model_rf)


cor_m = cor(train)
View(head(round(cor_m, 2)))


summary(train.data)
summary(test.data)



#kNN model
#min-max normalisation applied
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
indexes = enum.choose(subsets, 6)


for (i in 1:length(indexes)) {
  train.data_knn = train.data[indexes[[i]]]
  test.data_knn = test.data[indexes[[i]]]
  acc = c()
  sens = c()
  spec = c()
  
  print(names(train.data_knn))
  for (k in 1:10) {
    model_kNN = knn(train.data_knn, 
                    test.data_knn, 
                    cl = train.data$diagnosis, 
                    k = k)
    cm = confusionMatrix(model_kNN, as.factor(test.data$diagnosis))
    acc = c(acc, cm$overall[1])
    spec = c(spec, cm$byClass[2])
    sens = c(sens, cm$byClass[1])
  }
  
  df_res = data.frame(acc, spec, sens)
  print(df_res)
}

#indexes[[1]], l = 5
#0.8846154 0.9230769 0.8461538, k = 2

#indexes[[1]], l = 6
#0.8846154 0.9230769 0.8461538, k = 1

train.data_knn = train.data[, -7]
test.data_knn = test.data[, -7]

model_kNN = knn(train.data_knn[1:5], 
                test.data_knn[1:5], 
                cl = train.data$diagnosis, 
                k = 2)
cm = confusionMatrix(model_kNN, as.factor(test.data$diagnosis))
cm


#SVM
subsets = 1:6
indexes = enum.choose(subsets, 3)

for (i in 1:length(indexes)) {
  print("----------------------------------------------")
  train.data_svm = train.data[indexes[[i]]]
  train.data_svm$diagnosis = train.data$diagnosis
  test.data_svm = test.data[indexes[[i]]]
  test.data_svm$diagnosis = test.data$diagnosis
  # acc = c()
  # sens = c()
  # spec = c()
  # 
  # model_svm_poly = svm(formula = diagnosis ~.,
  #                      data = train.data_svm,
  #                      type = 'C-classification',
  #                      kernel = 'polynomial',
  #                      degree = 3,
  #                      gamma = 3,
  #                      cost = 1
  # )
  # y_prel_poly = predict(model_svm_poly, newdata = test.data_svm[-ncol(test.data_svm)])
  # cm = confusionMatrix(y_prel_poly, as.factor(test.data_svm$diagnosis))
  # 
  # acc = c(acc, cm$overall[1])
  # spec = c(spec, cm$byClass[2])
  # sens = c(sens, cm$byClass[1])
  # 
  # 
  # df_res = data.frame(acc, spec, sens)
  # print("Polynomial kernel")
  print(paste0("Features: ", names(train.data_svm)[1:ncol(train.data_svm)-1]))
  # print(df_res)
  # 
  # 
  # acc = c()
  # sens = c()
  # spec = c()

  # model_svm_radial = svm(formula = as.factor(diagnosis) ~.,
  #                        data = train.data_svm,
  #                        type = 'C-classification',
  #                        kernel = 'radial',
  #                        gamma = 4,
  #                        cost = 3)
  # y_prel_radial = predict(model_svm_radial, newdata = test.data_svm[-ncol(test.data_svm)])
  # cm = confusionMatrix(y_prel_radial, as.factor(test.data_svm$diagnosis))
  # 
  # acc = c(acc, cm$overall[1])
  # spec = c(spec, cm$byClass[2])
  # sens = c(sens, cm$byClass[1])
  # 
  # df_res = data.frame(acc, spec, sens)
  # print("Radial kernel")
  # print(paste0("Features: ", names(train.data_svm)[1:ncol(train.data_svm)-1]))
  # print(df_res)

  print("Best model:")
  obj <- tune.svm(as.factor(diagnosis)~., data = train.data_svm,
                  degree = 1:4,
                  gamma = 0:5,
                  cost = 1:5)
  print(summary(obj)[1])
  print(summary(obj)[2])

  print(obj$best.model)
}

model_svm_poly = svm(formula = diagnosis ~.,
                data = train.data,
                type = 'C-classification',
                kernel = 'polynomial',
                degree = 3,
                gamma = 3,
                cost = 1
                )
y_prel_poly = predict(model_svm_poly, newdata = test.data[-7])
confusionMatrix(y_prel_poly, as.factor(test.data$diagnosis))

obj <- tune.svm(as.factor(diagnosis)~., data = train.data[c(1, 3, 4, 7)], 
                degree = 1:4,
                gamma = 0:5, 
                cost = 1:5)
summary(obj)[2]
summary(obj)[1]

obj$best.model




model_svm_radial = svm(formula = as.factor(diagnosis) ~.,
                     data = train.data[c(1, 3, 4, 7)],
                     type = 'C-classification',
                     kernel = 'radial',
                     gamma = 2,
                     cost = 3)
y_prel_radial = predict(model_svm_radial, newdata = test.data[c(1, 3, 4)])
confusionMatrix(y_prel_radial, as.factor(test.data$diagnosis))
#0.6538, 0.5385, 0.7692 c(1, 3, 4)
#gamma = 2, cost = 3
names(train.data)
