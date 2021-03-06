require(BART)
require(coda)
require(randomForest)
require(tidyverse)

load("~/Documents/Personal/Hockey/BDC2021/Cleaned Data for Modeling.Rds")

#####
# Select Variables
##### 
y_for <- clean_data$goal_for_soon
y_against <- clean_data$goal_against_soon
x <- ungroup(clean_data) %>%
  select(
  # Context
  is_home, score_state, period, game_time,
  # Key event details
  shot_context, event_detail, event_success, shot_type,
  north_south, x_coordinate, y_coordinate, x_coordinate_2, y_coordinate_2, 
  for_distance, for_angle,
  # Prior events
  event_detail1, event_success1, north_south1, game_time1, prior_x_coordinate1, prior_y_coordinate1, for_distance1, for_angle1,
  event_detail2, event_success2, north_south2, game_time2, prior_x_coordinate2, prior_y_coordinate2, for_distance2, for_angle2,
  event_detail3, event_success3, north_south3, game_time3, prior_x_coordinate3, prior_y_coordinate3, for_distance3, for_angle3,
  # Calculations between events
  time_change1, distance_change1, speed1, north_south_change1, team_change1, 
  time_change2, distance_change2, speed2, north_south_change2, team_change2, 
  time_change3, distance_change3, speed3, north_south_change3, team_change3) %>%
  data.frame() 

### Scale some numeric variables
x_scaled <- x %>%
  select(-score_state, -game_time) %>% #, -period_time
  mutate_if(is.numeric, scale) %>% 
  add_column(score_state = x$score_state) %>%
  add_column(game_time = x$game_time)

### Random forest comparison model can't have nulls, so we'll give a fake value and an indicator column
### Also make responses a factor and include them in the dataset
x_for_comparison <- x_scaled %>%
  mutate(missing_end_coords = is.na(x_coordinate_2),
         x_coordinate_2 = replace_na(x_coordinate_2, -999),
         y_coordinate_2 = replace_na(y_coordinate_2, -999),
         y_for = factor(y_for),
         y_against = factor(y_against))
  
  
save(x_scaled, x_for_comparison, y_for, y_against, file = "~/Documents/Personal/Hockey/BDC2021/Modeling Inputs.Rda")

#####
# Fit BART Models
#####
print(paste("Starting bart_for1 at", Sys.time()))
bart_for1 <- mc.lbart(x.train = x_scaled, y.train = y_for, sparse = TRUE, mc.cores = 7, seed = 86)
print(paste("Ending bart_for1 at", Sys.time()))

print(paste("Starting bart_for2 at", Sys.time()))
bart_for2 <- mc.lbart(x.train = x_scaled, y.train = y_for, sparse = TRUE, mc.cores = 7, seed = 75)
print(paste("Ending bart_for2 at", Sys.time()))

print(paste("Starting bart_opp1 at", Sys.time()))
bart_opp1 <- mc.lbart(x.train = x_scaled, y.train = y_against, sparse = TRUE, mc.cores = 7, seed = 30)
print(paste("Ending bart_opp1 at", Sys.time()))

print(paste("Starting bart_opp2 at", Sys.time()))
bart_opp2 <- mc.lbart(x.train = x_scaled, y.train = y_against, sparse = TRUE, mc.cores = 7, seed = 9)
print(paste("Ending bart_opp2 at", Sys.time()))


#####x
# Fit comparison models
#####
set.seed(22)
lr_for <- glm(y_for ~ ., family="binomial", data = select(x_for_comparison, -y_against))
lr_opp <- glm(y_against ~ ., family="binomial", data = select(x_for_comparison, -y_for))
rf_for <- randomForest(y_for ~ ., data = select(x_for_comparison, -y_against))
rf_opp <- randomForest(y_against ~ ., data = select(x_for_comparison, -y_for))


### Save all outputs
save(bart_for1, bart_for2, bart_opp1, bart_opp2, lr_for, lr_opp, rf_for, rf_opp,
     file = "~/Documents/Personal/Hockey/BDC2021/VAEP Models.Rda")