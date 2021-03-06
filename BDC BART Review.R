require(BART)
require(coda)
require(MLmetrics)
require(tidyverse)
require(ggthemes)

load("~/Documents/Personal/Hockey/BDC2021/Cleaned Data for Modeling.Rds")
load("~/Documents/Personal/Hockey/BDC2021/VAEP Models.Rda")

#####
# In Sample Performance
#####

#Organize all of the predictions
predictions <- data.frame(
  actual_for = clean_data$goal_for_soon,
  bart_for = bart_for1$prob.train.mean,
  lr_for = lr_for$fitted.values,
  rf_for = rf_for$votes[,2],
  naive_for = mean(clean_data$goal_for_soon),
  actual_opp = clean_data$goal_against_soon,
  bart_opp = bart_opp1$prob.train.mean,
  lr_opp = lr_opp$fitted.values,
  rf_opp = rf_opp$votes[,2],
  naive_opp = mean(clean_data$goal_against_soon)
)

# Create function to generate key evaluation metrics
get_metrics <- function(preds, actuals, model){
  ll <- LogLoss(preds, actuals)
  auc <- Area_Under_Curve(preds, actuals)
  rmse <- RMSE(preds, actuals)
  r2 <- R2_Score(preds, actuals)
  results <- list(model = model,ll = ll, auc = auc, rmse = rmse, r2 = r2)
}

#Run evaluation metrics and organize in dataframe
rf_for_in_sample_results <- get_metrics(predictions$rf_for, predictions$actual_for, model = "Random Forest For")
lr_for_in_sample_results <- get_metrics(predictions$lr_for, predictions$actual_for, model = "Logistic Regression For")
bart_for_in_sample_results <- get_metrics(predictions$bart_for, predictions$actual_for, model = "BART For")
naive_for_in_sample_results <- get_metrics(predictions$naive_for, predictions$actual_for, model = "Naive Baseline For")
rf_opp_in_sample_results <- get_metrics(predictions$rf_opp, predictions$actual_opp, model = "Random Forest Opp")
lr_opp_in_sample_results <- get_metrics(predictions$lr_opp, predictions$actual_opp, model = "Logistic Regression Opp")
bart_opp_in_sample_results <- get_metrics(predictions$bart_opp, predictions$actual_opp, model = "BART Opp")
naive_opp_in_sample_results <- get_metrics(predictions$naive_opp, predictions$actual_for, model = "Naive Baseline Opp")

all_in_sample_results <- bind_rows(rf_for_in_sample_results, lr_for_in_sample_results, bart_for_in_sample_results, naive_for_in_sample_results,
                                   rf_opp_in_sample_results, lr_opp_in_sample_results, bart_opp_in_sample_results, naive_opp_in_sample_results)


#####
# Out of Sample Performance
#####
load("~/Documents/Personal/Hockey/BDC2021/BART VAEP CV Models.Rda")
load("~/Documents/Personal/Hockey/BDC2021/Baseline CV VAEP Models.Rda")

# Get a copy of the test data
test_clean_data <- filter(clean_data, game_date > ymd('2020-02-06'))

# Organize all of the test predictions
oos_predictions <- data.frame(
  actual_for = select(test_clean_data, goal_for_soon),
  lr_for = cv_lr_for_test_pred,
  rf_for = cv_rf_for$test$votes[,2],
  bart_for = cv_bart_for1$prob.test.mean,
  naive_for = mean(test_clean_data$goal_for_soon),
  actual_opp = filter(ungroup(clean_data), game_date > ymd('2020-02-06')) %>% select(goal_against_soon),
  lr_opp = cv_lr_opp_test_pred,
  rf_opp = cv_rf_opp$test$votes[,2],
  bart_opp = cv_bart_opp1$prob.test.mean,
  naive_opp = mean(test_clean_data$goal_against_soon))

# Run evaluation metrics and organize in dataframe
rf_for_oos_results <-    get_metrics(oos_predictions$rf_for, oos_predictions$goal_for_soon, model = "Random Forest For")
lr_for_oos_results <-    get_metrics(oos_predictions$lr_for, oos_predictions$goal_for_soon, model = "Logistic Regression For")
bart_for_oos_results <-  get_metrics(oos_predictions$bart_for, oos_predictions$goal_for_soon, model = "BART For")
naive_for_oos_results <- get_metrics(oos_predictions$naive_for, oos_predictions$goal_for_soon, model = "Naive Baseline For")
rf_opp_oos_results <-    get_metrics(oos_predictions$rf_opp, oos_predictions$goal_against_soon, model = "Random Forest Opp")
lr_opp_oos_results <-    get_metrics(oos_predictions$lr_opp, oos_predictions$goal_against_soon, model = "Logistic Regression Opp")
bart_opp_oos_results <-  get_metrics(oos_predictions$bart_opp, oos_predictions$goal_against_soon, model = "BART Opp")
naive_opp_oos_results <- get_metrics(oos_predictions$naive_opp, oos_predictions$goal_against_soon, model = "Naive Baseline Opp")

all_oos_results <- bind_rows(rf_for_oos_results, lr_for_oos_results, bart_for_oos_results, naive_for_oos_results,
                              rf_opp_oos_results, lr_opp_oos_results, bart_opp_oos_results, naive_opp_oos_results)



#####
# Callibration
#####

# Group predictions into deciles
oos_predictions <- oos_predictions %>% 
  mutate(pred_for_decile = ntile(bart_for, 10),
         pred_opp_decile = ntile(bart_opp, 10)) 

# Plot callibrations
oos_predictions %>%
  group_by(pred_for_decile) %>%
  summarise(predicted_pre_goal_events = sum(bart_for),
            actual_pre_goal_events = sum(goal_for_soon)) %>%
  ggplot(., aes(x = predicted_pre_goal_events, y = actual_pre_goal_events)) +
  geom_point(color = "lightblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  labs(title = "Test Set Callibration Plot - Predicting Goals For", subtitle = "Bucketed into 10 groups of 1,775 oberservations each",
       x = "Predicted Pre-Goal Events", y = "Actual Pre-Goal Events")+
  theme_tufte()

oos_predictions %>%
  group_by(pred_opp_decile) %>%
  summarise(predicted_pre_goal_events = sum(bart_opp),
            actual_pre_goal_events = sum(goal_against_soon)) %>%
  ggplot(., aes(x = predicted_pre_goal_events, y = actual_pre_goal_events)) +
  geom_point(color = "lightblue", size = 3) +
  geom_abline(slope = 1, intercept = 0, color = "grey") +
  labs(title = "Test Set Callibration Plot - Predicting Goals Against", subtitle = "Bucketed into 10 groups of 1,775 oberservations each",
       x = "Predicted Pre-Goal Events", y = "Actual Pre-Goal Events")+
  theme_tufte()


#####
# Model Diagnostics
#####
#
#head(bart_for1$prob.train.mean) #Review the posterior estimate
#
##Check if markov chains converged
#z <- gewekediag(bart_for1)$z #https://rdrr.io/cran/BART/man/gewekediag.html We want to fail to reject, i.e. Z < 2
#rhat <- gelman.diag(mcmc.list(mcmc(bart_for1$prob.train), mcmc(bart_for1$prob.train)), multivariate = FALSE)
##On above, Approximate convergence is diagnosed when the upper limit is close to 1 and not substantially above. 1.1 is decent but not great
#
##Check if we have enough samples after convergence
#ess <- effectiveSize(mcmc.list(mcmc(bart_for1$prob.train), mcmc(bart_for1$prob.train)))
##Should be at least a few thousand for each parameter
#
#autocorr.diag(mcmc(bart_for1$prob.train))
#autocorr.plot(mcmc(bart_for1$prob.train))#