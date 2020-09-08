install.packages("rJava")
install.packages("RWeka")
install.packages("C50")
install.packages("OneR")
install.packages("gmodels")
install.packages("party")
install.packages("RColorBrewer")

Sys.setenv('JAVA_HOME'="C:/Program Files (x86)/Java/jdk1.8.0_251/bin")

library(rJava)
library(RWeka)
library(C50)
library(OneR)
library(gmodels)
library(party)
library(RColorBrewer)

#step 1:loading the data
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

#step 2:preprocessing
str(mushrooms)
mushrooms$veil_class <- NULL
table(mushrooms$class)

#step 3:training model on data

set.seed(123)
train_sample <- sample(8124, 7000)
str(train_sample)
mushrooms_train <- mushrooms[train_sample, ]
mushrooms_test  <- mushrooms[-train_sample, ]

mushroom_1R <- OneR(class ~ ., data = mushrooms)
mushroom_1R


#Step 4: evaluating model performance
summary(mushroom_1R)

mushroom_pred <- predict(mushroom_1R, mushrooms_test)
CrossTable(mushrooms_test$class, mushroom_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Step 5: Improving model performance
mushroom_JRip <- JRip(class ~ ., data = mushrooms)
mushroom_JRip
summary(mushroom_JRip) 

mushroom_pred <- predict(mushroom_JRip, mushrooms_test)

CrossTable(mushrooms_test$class, mushroom_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))