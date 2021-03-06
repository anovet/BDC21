# GOAL: Cut train/test on 2/6/2020 to predict 10  games from the prior 29

require(BART)
require(coda)
require(randomForest)
require(lubridate)
require(tidyverse)

load("~/Documents/Personal/Hockey/BDC2021/Cleaned Data for Modeling.Rds")
load("~/Documents/Personal/Hockey/BDC2021/Modeling Inputs.Rda")

#####
# Select Variables
##### 

#Get the rows that are from earlier games and should appear in the training set
idx <- clean_data$game_date <= ymd('2020-02-06')

#Split into train and test
x_train <- x_scaled[idx,]
x_test <- x_scaled[!idx,]
x_comp_train <- x_for_comparison[idx,]
x_comp_test <- x_for_comparison[!idx,]
y_for_train <- y_for[idx]
y_for_test <- y_for[!idx]
y_against_train <- y_against[idx]
y_against_test <- y_against[!idx]


#####
# Fit BART Models
#####
print(paste("Starting bart_for1 at", Sys.time()))
cv_bart_for1 <- mc.lbart(x.train = x_train, y.train = y_for_train, x.test = x_test, sparse = TRUE, mc.cores = 7, seed = 8)
print(paste("Ending bart_for1 at", Sys.time()))
head(cv_bart_for1$prob.test.mean)

print(paste("Starting bart_for2 at", Sys.time()))
cv_bart_for2 <- mc.lbart(x.train = x_train, y.train = y_for_train,    x.test = x_test, sparse = TRUE, mc.cores = 7, seed = 76)
print(paste("Ending bart_for2 at", Sys.time()))
head(cv_bart_for2$prob.test.mean)

print(paste("Starting bart_opp1 at", Sys.time()))
cv_bart_opp1 <- mc.lbart(x.train = x_train, y.train = y_against_train, x.test = x_test, sparse = TRUE, mc.cores = 7, seed = 100)
print(paste("Ending bart_opp1 at", Sys.time()))
head(cv_bart_opp1$prob.test.mean)

print(paste("Starting bart_opp2 at", Sys.time()))
cv_bart_opp2 <- mc.lbart(x.train = x_train, y.train = y_against_train, x.test = x_test, sparse = TRUE, mc.cores = 7, seed = 10)
print(paste("Ending bart_opp2 at", Sys.time()))
head(cv_bart_opp2$prob.test.mean)

save(cv_bart_for1, cv_bart_for2, cv_bart_opp1, cv_bart_opp2, 
     file = "~/Documents/Personal/Hockey/BDC2021/BART CV VAEP Models.Rda")

#####
# Fit Comparison Models 
#####
set.seed(23)
cv_lr_for <- glm(y_for ~ ., family="binomial", data = select(x_comp_train, -y_against))
cv_lr_for_test_pred <- predict(cv_lr_for, select(x_comp_test, -y_against), type="response")
cv_lr_opp <- glm(y_against ~ ., family="binomial", data = select(x_comp_train, -y_for))
cv_lr_opp_test_pred <- predict(cv_lr_opp, select(x_comp_test, -y_for), type="response")
cv_rf_for <- randomForest(factor(y_for) ~ ., data = select(x_comp_train, -y_against),
                          xtest = select(x_comp_test, -y_for, -y_against), ytest = factor(y_for_test))
cv_rf_opp <- randomForest(factor(y_against) ~ ., data = select(x_comp_train, -y_for),
                          xtest = select(x_comp_test, -y_for, -y_against), ytest = factor(y_against_test))

save(cv_lr_for, cv_lr_opp, cv_rf_for, cv_rf_opp, 
     cv_lr_for_test_pred, cv_lr_opp_test_pred, 
     file = "~/Documents/Personal/Hockey/BDC2021/Comparison CV VAEP Models.Rda")
