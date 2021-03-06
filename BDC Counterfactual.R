require(tidyverse)
load("~/Documents/Personal/Hockey/BDC2021/Cleaned Data for Modeling.Rds")
load("~/Documents/Personal/Hockey/BDC2021/Modeling Inputs.Rda")

#####
# Develop Counterfactuals
#####

# Get all shots with traffic and create a version without traffic, then clean up the factor so that predictions will work
traffic_counterfactual <- filter(x_scaled, shot_context %in% c("traffic", "traffic_and_one_timer")) %>%
  mutate(shot_context = case_when(shot_context == "traffic" ~ "neither",
                                  shot_context == "traffic_and_one_timer" ~ "one_timer"))
traffic_counterfactual$shot_context = as.factor(traffic_counterfactual$shot_context)
levels(traffic_counterfactual$shot_context) <- c(levels(traffic_counterfactual$shot_context), "traffic_and_one_timer", "traffic","not_shot") 


# Run the BART model with our counterfactuals as the test data
traffic_bart_for <- mc.lbart(x.train = x_scaled, y.train = y_for,    x.test = traffic_counterfactual, sparse = TRUE, mc.cores = 7, seed = 123)
traffic_bart_opp <- mc.lbart(x.train = x_scaled, y.train = y_against, x.test = traffic_counterfactual, sparse = TRUE, mc.cores = 7, seed = 124)
# save(traffic_bart_for, traffic_bart_opp, file = "Causal Inference Models.Rda")





#####
# Causal Analysis
#####

# load("Causal Inference Models.Rda")
# Extract all the posterior draws for the relevant subset of the training set
idx <- clean_data$shot_context %in% c("traffic", "traffic_and_one_timer")
yhat.train <- traffic_bart_for$yhat.train[,idx]
yhat_diff <- yhat.train - traffic_bart_for$yhat.test
yhat_low <- apply(yhat_diff, 2, function(x)quantile(x,.025))
yhat_high <- apply(yhat_diff, 2, function(x)quantile(x,.975))

 #Organize the resulting predictions
cf_results <- clean_data
cf_results$for_prob <- traffic_bart_for$prob.train.mean
cf_results$opp_prob <- traffic_bart_opp$prob.train.mean
cf_results <- cf_results %>%
  mutate(
    #Get result value per play
    net_prob = for_prob - opp_prob,
    prior_prob_for = ifelse(team == team1, lag(for_prob), lag(opp_prob)),
    prob_for_change = for_prob - prior_prob_for,
    prior_prob_opp = ifelse(team == team1, lag(opp_prob), lag(for_prob)),
    prob_opp_change = opp_prob - prior_prob_opp,
    prior_net = prior_prob_for - prior_prob_opp,
    prob_change = net_prob - prior_net) %>%
  # Limit to the traffic shots
  filter(shot_context %in% c("traffic", "traffic_and_one_timer")) %>%
  bind_cols(cf_prob_for = traffic_bart_for$prob.test.mean,
            cf_prob_opp = traffic_bart_opp$prob.test.mean,
            low_bound = yhat_low,
            high_bound = yhat_high) %>%
  #Calculate key CF stats
  mutate(
    cf_net_prob = cf_prob_for - cf_prob_opp,
    cf_prob_for_change = cf_prob_for - prior_prob_for,
    cf_prob_opp_change = cf_prob_opp - prior_prob_opp,
    cf_prob_change = cf_net_prob - prior_net,
    # Calculate value of traffic: positive values mean traffic helped
    value_diff = prob_change - cf_prob_change,
    for_value_diff = prob_for_change - cf_prob_for_change) %>%
  #Remove plays without a prior calculation
  filter(!is.na(team1))

#ATT:
# Stealing function from Asmae Toumi: https://github.com/asmae-toumi/zone-entries/blob/master/code/analysis.R
Posterior_Summary = function(estimate) {
  # risk difference
  est_mean = mean(estimate)
  est_se = sd(estimate)
  est_lower = quantile(estimate, probs=0.025)
  est_upper = quantile(estimate, probs=0.975)
  res = c(est_mean, est_se, est_lower, est_upper)
  names(res) = c("EST","SE","LOWER","UPPER")
  return(res)
}
att_summary = Posterior_Summary(cf_results$value_diff)

# Plot the value added for each shot
ggplot(cf_results, aes(x = value_diff)) +
  geom_density(fill = "lightblue") +
  labs(title = "Effect of Traffic on Shot BSAVE Value", 
       x = "Value Added by Traffic", y = "Frequency") +
  theme_minimal()



#####
# Conditional Analysis
#####

# Cut the above based on whether or not there was a one timer
cf_results %>% 
  group_by(shot_context) %>% 
  summarise(ave_att = mean(value_diff), 
            se_att = sd(value_diff)/sqrt(n()))

# Look at effect of shot distance
span <- 0.5
  #filter(abs(GPM_60) <= 0.3) %>% 
ggplot(cf_results, aes(x = for_distance, y = value_diff)) + 
  geom_smooth(method = "loess", se = FALSE, span = span, size = 0.8) + 
  geom_smooth(aes(for_distance, low_bound), method = "loess", lty = 3, se = FALSE, span = span, size = 0.75) + 
  geom_smooth(aes(for_distance, high_bound), method = "loess", lty = 3, se = FALSE, span = span, size = 0.75) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  labs(title = "Conditional Impact of Traffic on Shot",
       subtitle = "Based on Shot Distance",
       x = "Distance from Net",
       y = "Estimated impact of traffic",
       caption = "Plot based on previous work by Asmae Toumi") +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks = scales::pretty_breaks())