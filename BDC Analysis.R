# Goal: Use the outputs of the models for player analysis
# Note that this is a bit of a scratch pad
# Not everything in here is used in the final presentation

require(tidyverse)
require(ggridges)
require(ggthemes)
load("~/Documents/Personal/Hockey/BDC2021/BART VAEP Models.Rda")
load("~/Documents/Personal/Hockey/BDC2021/Cleaned Data for Modeling.Rds")
source("~/Documents/Personal/Hockey/BDC2021/BDC Rink.R")

# Get a complete data set with predicted values
results <- clean_data
results$for_prob <- bart_for1$prob.train.mean
results$opp_prob <- bart_opp1$prob.train.mean
results <- results %>%
  mutate(
    #Put coordinates in NHL units so they work in gg_rink plots
    plot_x = x_coordinate - 100,
    plot_y = y_coordinate - 42.5,
    plot_x2 = x_coordinate_2 - 100,
    plot_y2 = y_coordinate_2 - 42.5,
    last_name = word(player,-1),
    #Get result value per play
    net_prob = for_prob - opp_prob,
         prior_prob_for = ifelse(team == team1, lag(for_prob), lag(opp_prob)),
         prob_for_change = for_prob - prior_prob_for,
         prior_prob_opp = ifelse(team == team1, lag(opp_prob), lag(for_prob)),
         prob_opp_change = opp_prob - prior_prob_opp,
         prior_net = prior_prob_for - prior_prob_opp,
         prob_change = net_prob - prior_net) %>%
  #Remove plays without a prior calculation
  filter(!is.na(team1))

# Spot check the results
review <- ungroup(results) %>%
  select(skater_state, game_time, team, player, event, detail_1, x = x_coordinate, y = y_coordinate,
         for_prob, opp_prob, net_prob, goal_for_soon)


####
# Player Rankings
####

# Get a list of the Otter players
otters <- filter(results, team == "Erie Otters") %>% count(player)

# Get aggregate stats by player and player + event type
player_stats <- results %>%
  group_by(player, last_name) %>%
  summarise(total = sum(prob_change, na.rm = TRUE),
            plays = n(),
            avg = mean(prob_change, na.rm = TRUE),
            gp = n_distinct(game_date)) %>%
  mutate(total_per_game = total / gp,
         plays_per_game = plays / gp) %>%
  filter(player %in% otters$player)

player_type_stats <- results %>%
  group_by(player, last_name, event) %>%
  summarise(total = sum(prob_change, na.rm = TRUE),
            plays = n(),
            avg = mean(prob_change, na.rm = TRUE),
            gp = n_distinct(game_date)) %>%
  mutate(total_per_game = total / gp,
         plays_per_game = plays / gp) %>%
  filter(player %in% otters$player)

# Restrict to Otters with 5GP and see playing style by event type
gp5 <- filter(player_stats, gp >= 5)
events <- unique(results$event_group)
for (i in events){
  print(ggplot(filter(player_type_stats, event_group == i, player %in% gp5$player), 
               aes(x = plays_per_game, y = avg)) +
    #geom_point() +
    geom_text(aes(label=last_name), hjust="inward") +
    labs(title = paste0(i," Behavior by Player"), subtitle = "Players with at least 5GP", 
         x = paste0(i," Events per Game"), y = paste0("Avg Value per ", i)) +
    theme_minimal())
}

#####
# Event Analysis
#####

# Check distributions of values by event type
ggplot(results, aes(x = prob_change, y = event_group, height = ..ndensity..)) +
  geom_density_ridges(scale = 0.9, quantile_lines = TRUE, fill = "lightblue",rel_min_height = 0.01) + 
  geom_vline(xintercept = 0, col = "grey", linetype = "dashed") +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  coord_cartesian(xlim = c(-0.1,0.2)) +
  labs(title = "Play Values by Event Type", x = "Net Value Change", y = "Event Type") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)

# Bin values to mess around with viz explooration
results <- mutate(results, grouping = case_when(prob_change < -.1 ~ "Low",
                                                prob_change < 0.1 ~ "Middle",
                                                TRUE ~ "High"))
# Look at value of events by location
ggplot(results, aes(x = plot_x, y = plot_y)) +
  geom_point(alpha = 0.1) +
  gg_rink() + gg_rink(side = "left") +
  facet_grid(event_group ~ grouping) +
  scale_colour_gradient2()

# Look at value of passes by location
ggplot(filter(results, event_group == "Pass"), aes(x = plot_x, y = plot_y)) +
  geom_point(alpha = 0.1) +
  gg_rink() + gg_rink(side = "left") +
  facet_grid(event ~ grouping) +
  scale_colour_gradient2()

#####
# Player Playing Style
#####
# Show a particular player's distribution by event compared to the rest
# Not fully working
drysdale <- mutate(results, dry = last_name == "Drysdale")
ggplot(drysdale, aes(x = prob_for_change, y = event_group, height = ..ndensity.., color = dry, fill = dry)) +
  geom_density_ridges(aes(group = dry),scale = 0.9, quantile_lines = TRUE, fill = "lightblue",rel_min_height = 0.01) + 
  geom_vline(xintercept = 0, col = "grey", linetype = "dashed") +
  scale_y_discrete(expand = c(0, 0)) +     # will generally have to set the `expand` option
  coord_cartesian(xlim = c(-0.1,0.2)) +
  labs(title = "Play Values by Event Type", x = "Net Value Change", y = "Event Type") +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)

ggplot(drysdale, aes(x = prob_change, group = dry, fill = dry)) +
  geom_density(alpha = 0.4) + 
  facet_wrap(event_group ~ .) +
  coord_cartesian(xlim = c(-0.1,0.2))


# Summarize results from Jamie Drysdale
dry_stats <- results %>%
  group_by(player, last_name, event_group) %>%
  summarise(total = sum(for_prob, na.rm = TRUE),
            plays = n(),
            avg = mean(for_prob, na.rm = TRUE),
            gp = n_distinct(game_date)) %>%
  mutate(total_per_game = total / gp,
         plays_per_game = plays / gp) %>%
  filter(player == "Jamie Drysdale")
ggplot(dry_stats, aes(x = reorder(event_group, -total_per_game), y = total_per_game)) +
  geom_col(color = "purple", fill = "purple") +
  labs(title = "Jamie Drysdale Style Summary",
        x = "Event Type", y = "Total BSAVE per Game") +
  theme_minimal()

# Future work: good/bad/fun/full with each payer's change on offense and defense
