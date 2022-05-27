library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)


setwd('D:/Uni/SS 2022/Seminar ML for SSP/Debiasing_Clinical_Data/Data')
train = read.csv('train/extracted_features/audio/training_functionals.csv')
train$diagnosis = as.numeric(train$diagnosis)
train$gender = as.numeric(ifelse(train$gender == " male ", 1, -1))

set.seed(123)
features = c("F2frequency_sma3nz_amean", 
             "HNRdBACF_sma3nz_amean", 
             "F0semitoneFrom27.5Hz_sma3nz_percentile80.0", 
             "F0semitoneFrom27.5Hz_sma3nz_amean",
             "F1frequency_sma3nz_amean",
             "gender", "diagnosis")

train = train[features]
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
#table(test.data$gender, test.data$diagnosis)

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
