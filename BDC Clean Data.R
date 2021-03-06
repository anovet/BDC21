require(janitor)
require(lubridate)
require(tidyverse)
require(ggthemes)

###Download data once and save locally
#raw <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv")
#save(raw, file = "BDC Women Data.Rds")

#raw2 <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_scouting.csv")
#save(raw2, file = "BDC Prospect Data.Rds")

load("~/Documents/Personal/Hockey/BDC2021/BDC Women Data.Rds")
load("~/Documents/Personal/Hockey/BDC2021/BDC Prospect Data.Rds")




###
#Clean Data
###
dat <- raw2 %>%
  # Clean column names
  clean_names() %>%
  # Details 2-4 are null except for shots, I'll rename for clarity
  rename(shot_result = detail_2,
         traffic = detail_3,
         one_timer = detail_4) %>%
  # Don't need penalties for our purposes, as we want "on puck" actions. Future work could add penalties
  filter(event != "Penalty Taken") %>%
  # Change event type to factor
  mutate(shot_result = factor(shot_result))
  




#####
# Add Game Context Variables
#####
dat <- dat %>%
  #Get score state and skater numbers from event team's perspective
  mutate(is_home = team == home_team,
         score_state = ifelse(is_home, 
                              home_team_goals - away_team_goals,
                              away_team_goals - home_team_goals),
         #Get skater number context as factor since each special teams situation is different
         skater_state = factor(paste0(ifelse(is_home, home_team_skaters, away_team_skaters),
                                      "v",
                                      ifelse(is_home, away_team_skaters, home_team_skaters))),
         #Convert clock time to seconds left in period and game
         period_time = as.integer(str_sub(as.character(clock), 1,2)) * 60 +
           as.integer(str_sub(as.character(clock), 4,5)),
         game_time = (3-period) * (20*60) + period_time,
         period = factor(period),
         #Get a unique_id to preserve the order, which sometimes has 2 events listed at the same time
         event_id = row_number()) %>%
  #Remove clock + skater cols since we have them in the format we need; goals stay because they may be their own features
  select(-clock, -home_team_skaters, -away_team_skaters)





#####
#Add Skate events
#####

# Soon, I'll want to know if 2 events in a row have the same player with the puck at the same place. 
# For the first of the 2, we want the pass recipient if applicable, and the actor if not
dat <- dat %>%
  mutate(player_at_end = ifelse(event == "Play", player_2, player),
         x_at_end = ifelse(event == "Play", x_coordinate_2, x_coordinate),
         y_at_end = ifelse(event == "Play", y_coordinate_2, y_coordinate))

# Now we want to identify situations where we should add a "Skate" event. This is complicated.
# We'll start by flagging rows that should have a skate after them.
dat <- dat %>%
# Don't include lag info from other games/periods
group_by(game_date, home_team, period) %>% 
  mutate(follow_with_skate = case_when(
    # No skate after a shot, goal, or missed pass because the actor no longer has possession
    event %in% c("Shot", "Goal", "Incomplete Play") ~ FALSE,
    # No skate if the next event is a faceoff or puck recovery, because something happened where the loost possession
    lead(event) %in% c("Faceoff Win", "Puck Recovery") ~ FALSE,
    # Skate if the same player has the puck for 2 events in a row with different times and locations
    player_at_end == lead(player) & game_time != lead(game_time) &
      (x_at_end != lead(x_coordinate) | y_at_end != lead(y_coordinate)) ~ TRUE,
    # Not a skate if events happen at the same time or location
    x_at_end == lead(x_coordinate) & y_at_end == lead(y_coordinate) ~ FALSE,
    game_time == lead(game_time) ~ FALSE,
    # If next event is a takeaway at a different location, assume a skate
    lead(event) == "Takeaway" & (x_at_end != lead(x_coordinate) | y_at_end != lead(y_coordinate)) ~ TRUE,
    # If next event does not exist (because period ends), no skate
    is.na(lead(event)) ~ FALSE
  ))

# Next, remove zone entries. The dumps and passes are captured by other events, and our skates will replace the carries
dat <- filter(dat, event != "Zone Entry")

# Extract the events which should be followed by skates and use their rows to create skates, with some choices about how to fill the info
skates <- filter(dat, follow_with_skate == TRUE) %>%
  mutate(player = player_at_end, # Should be the same as the player unless its a pass recipient
         x_coordinate = x_at_end,
         y_coordinate = y_at_end, 
         event = "Skate", #Hard code
         detail_1 = NA, # No details needed
         player_2 = NA, # No details needed
         period_time = period_time - 1, # Basically treating this as the "start time" since other events don't have an end
         game_time = game_time - 1, # Basically treating this as the "start time" since other events don't have an end
         event_id = event_id + 0.5, #Increment so we can reorder the completed table
         player_at_end = NA, # No longer needed
         follow_with_skate = NA # No longer needed
         )

#Join skates back into data
full_dat <- bind_rows(dat, skates) %>%
  # Make the end location of the skate the coordinate_2 so that the path is available. 
  # Remove these values for incomplete passes. I suspect it's not helpful since the puck doesn't actually get there
  # This could be done differently if desired
  mutate(
    x_coordinate_2 = case_when(
      event == "Skate" ~  lead(x_coordinate), 
      event == "Incomplete Play" ~ NA_real_,
      TRUE ~ x_coordinate_2),
    y_coordinate_2 = case_when(
      event == "Skate" ~  lead(y_coordinate), 
      event == "Incomplete Play" ~ NA_real_,
      TRUE ~ y_coordinate_2)) %>%
  # Remove columns I only used to create the skates 
  select(-player_at_end, -follow_with_skate, -x_at_end, -y_at_end) %>%
  # Reorder the dataset to include the skates correctly
  arrange(event_id)




#####
# Add Calculated Event Details
#####
full_dat <- full_dat %>%
  mutate(
    #For each event, get distance to each net. "For" is the net that creates a goal for, e.g. the opponent's goalie
    #Goals are are (189, 42.5) and (11,42.5), not the usual 89,0 and -89,0
    for_distance = sqrt((189 - x_coordinate)^2 + (y_coordinate - 42.5)^2),
    away_distance = sqrt((11 - x_coordinate)^2 + (y_coordinate - 42.5)^2),
    # Calculate angle from each net
    # Thanks to Daniel Weinberger for angle calculation help
    for_angle = (pi/2- ifelse(x_coordinate<189,
                              atan2(abs(189-x_coordinate),abs(42.5-y_coordinate)),
                              atan2((189-x_coordinate),abs(42.5-y_coordinate))))*180/pi,
    against_angle = (pi/2- ifelse(x_coordinate<11,
                                  atan2(abs(42.5-y_coordinate),abs((11-x_coordinate))),
                                  atan2(abs(42.5-y_coordinate),11-x_coordinate)
    ))*180/pi + 90,
    #Check which side the goalie is looking
    north_south = y_coordinate >= 42.5)
  
#Check I calculated angles correctly
#ggplot(full_dat, aes(x = x_coordinate, y = y_coordinate)) + geom_point(aes(color = for_angle))
#ggplot(full_dat, aes(x = x_coordinate, y = y_coordinate)) + geom_point(aes(color = against_angle))
#ggplot(filter(full_dat, for_angle <= 90), aes(x = x_coordinate, y = y_coordinate)) + geom_point(aes(color = for_angle))
  




###
# Feature Eng for modeling
###

### Create response variables
full_dat <- full_dat %>%
  #Don't want to include lag/lead info from other games/periods
  group_by(game_date, home_team, period) %>% 
  # Get goals for and against in event itself or next 14 events events
  # 15 events because, as shown later in possessions df, that captures 95% of all possessions
  # Other other options are worth trying, but that's suitable for my purposes
  mutate(goal_for_soon = event == "Goal" |
           (lead(event, 1) == "Goal" & lead(team, 1) == team) |
           (lead(event, 2) == "Goal" & lead(team, 2) == team) |
           (lead(event, 3) == "Goal" & lead(team, 3) == team) |
           (lead(event, 4) == "Goal" & lead(team, 4) == team) |
           (lead(event, 5) == "Goal" & lead(team, 5) == team) |
           (lead(event, 6) == "Goal" & lead(team, 6) == team) |
           (lead(event, 7) == "Goal" & lead(team, 7) == team) |
           (lead(event, 8) == "Goal" & lead(team, 8) == team) |
           (lead(event, 9) == "Goal" & lead(team, 9) == team) |
           (lead(event, 10) == "Goal" & lead(team, 10) == team) |
           (lead(event, 11) == "Goal" & lead(team, 11) == team) |
           (lead(event, 12) == "Goal" & lead(team, 12) == team) |
           (lead(event, 13) == "Goal" & lead(team, 13) == team) |
           (lead(event, 14) == "Goal" & lead(team, 14) == team),
         goal_against_soon = (lead(event, 1) == "Goal" & lead(team, 1) != team) |
           (lead(event, 2) == "Goal" & lead(team, 2) != team) |
           (lead(event, 3) == "Goal" & lead(team, 3) != team) |
           (lead(event, 4) == "Goal" & lead(team, 4) != team) |
           (lead(event, 5) == "Goal" & lead(team, 5) != team) |
           (lead(event, 6) == "Goal" & lead(team, 6) != team) |
           (lead(event, 7) == "Goal" & lead(team, 7) != team) |
           (lead(event, 8) == "Goal" & lead(team, 8) != team) |
           (lead(event, 9) == "Goal" & lead(team, 9) != team) |
           (lead(event, 10) == "Goal" & lead(team, 10) != team) |
           (lead(event, 11) == "Goal" & lead(team, 11) != team) |
           (lead(event, 12) == "Goal" & lead(team, 12) != team) |
           (lead(event, 13) == "Goal" & lead(team, 13) != team) |
           (lead(event, 14) == "Goal" & lead(team, 14) != team),
         #Replace nulls in label (for lack of subsequent event) with FALSE
         goal_for_soon = replace_na(goal_for_soon, FALSE),
         goal_against_soon = replace_na(goal_against_soon, FALSE))


#Check dump outs occur where I think make sense to confirm my understanding of this event type
ggplot(filter(full_dat, event == "Dump In/Out"), aes(x = x_coordinate, y = y_coordinate)) + geom_point()

### Clean / organize event details
full_dat <- full_dat %>%
  mutate(
    # Group the events into more workable action types: event_group has fewer categories and event_detail has more details
    event_group = factor(case_when(
      event %in% c("Play", "Incomplete Play") ~ "Pass",
      event %in% c("Goal", "Shot") ~ "Shot",
      event %in% c("Dump In/Out", "Faceoff Win", "Puck Recovery", "Takeaway", "Skate") ~ event)),
    event_detail = event_group, # Had been experimenting with other cuts of the data, but removing those
    shot_type = factor(case_when(
      event %in% c("Goal", "Shot") ~ detail_1,
      TRUE ~ "not_shot")),
    shot_context = factor(case_when(
      event %in% c("Goal", "Shot") & traffic & one_timer ~ "traffic_and_one_timer",
      event %in% c("Goal", "Shot") & traffic ~ "traffic",
      event %in% c("Goal", "Shot") & one_timer ~ "one_timer",
      event %in% c("Goal", "Shot") ~ "neither",
      TRUE ~ "not_shot")),
    # Note whether event succeeded
    event_success = case_when(
      # Goals
      shot_result == "On Net" ~ TRUE,
      shot_result %in% c("Missed", "Blocked") ~ FALSE,
      # Dump In and Out
      detail_1 == "Retained" ~ TRUE,
      detail_1 =="Lost" ~ FALSE,
      # All other events
      event %in% c("Play", "Faceoff Win", "Puck Recovery", "Takeaway", "Skate") ~ TRUE,
      event %in% c("Incomplete Play") ~ FALSE))

###Get information from past events
get_prior_play <- function(df, n) {
  names <- colnames(df)
  df %>%
    mutate(
      last_team = lag(team, n = n),
      last_event_detail = lag(event_detail, n = n),
      last_event_success = lag(event_success, n = n),
      last_north_south = lag(north_south, n = n),
      last_game_time = lag(game_time, n = n),
      last_for_distance = lag(for_distance, n = n),
      last_away_distance = lag(away_distance, n = n),
      last_for_angle = lag(for_angle, n = n),
      last_against_angle = lag(against_angle, n = n),
      last_x_coordinate = lag(x_coordinate, n = n),
      last_y_coordinate = lag(y_coordinate, n = n)
      ) %>%
    # Convert events into perspective of the event team
    mutate(last_north_south = ifelse(team == last_team, last_north_south, !last_north_south),
           last_distance = ifelse(team == last_team, last_for_distance, last_away_distance),
           last_angle = ifelse(team == last_team, last_for_angle, last_against_angle),
           last_x_coordinate = ifelse(team == last_team, last_x_coordinate, 200 - last_x_coordinate),
           last_y_coordinate = ifelse(team == last_team, last_y_coordinate, 45 - (last_y_coordinate - 42.5))
           ) %>%
    # Remove columns only needed to calculate event perspective
    select(-last_for_distance, -last_away_distance, -last_for_angle, -last_against_angle) %>%
    setNames(c(names, paste0("team",n), paste0("event_detail",n), paste0("event_success",n), paste0("north_south",n),
               paste0("game_time",n), paste0("prior_x_coordinate",n), paste0("prior_y_coordinate",n),
               paste0("for_distance",n), paste0("for_angle",n)))
}

### Get events for the prior 3 plays, so we'll have 4 plays total in the training features for each observation
full_dat <- full_dat %>%
  get_prior_play(., n = 1) %>%
  get_prior_play(., n = 2) %>%
  get_prior_play(., n = 3) %>%
  # No longer need away_distance or against_angle for anything
  select(-away_distance, -against_angle)

###Calculate info between events
full_dat <- full_dat %>%
  mutate(time_change1 = ifelse(game_time1 == game_time, 0.1, game_time1 - game_time),
         distance_change1 = sqrt((x_coordinate - prior_x_coordinate1)^2 + (y_coordinate - prior_x_coordinate1)^2),
         speed1 = ifelse(distance_change1 == 0, 0, (distance_change1 / time_change1)),
         north_south_change1 = north_south == north_south1,
         team_change1 = team == team1,
         time_change2 = ifelse(game_time2 == game_time1, 0.1, game_time2 - game_time1),
         distance_change2 = sqrt((prior_x_coordinate1 - prior_x_coordinate2)^2 + (prior_x_coordinate1 - prior_x_coordinate2)^2),
         speed2 = ifelse(distance_change2 == 0, 0, (distance_change2 / time_change2)),
         north_south_change2 = north_south1 == north_south2,
         #Note team change field always compares to latest event, not the prior one, unlike the other fields
         team_change2 = team == team2,
         time_change3 = ifelse(game_time3 == game_time2, 0.1, game_time3 - game_time2),
         distance_change3 = sqrt((prior_x_coordinate2 - prior_x_coordinate3)^2 + (prior_x_coordinate2 - prior_x_coordinate3)^2),
         speed3 = ifelse(distance_change3 == 0, 0, (distance_change3 / time_change3)),
         north_south_change3 = north_south2 == north_south3,
         team_change3 = team == team3)
  


# Remove the first 3 events in each period since they don't have prior events
# We could be smarter about this in the future, but it's a small enough % of the data that I'd rather just avoid the missing values
# Also restrict to 5v5
clean_data <- filter(full_dat, !is.na(event_detail3), skater_state == "5v5") %>% ungroup()
# Save Results
save(clean_data, file = "~/Documents/Personal/Hockey/BDC2021/Cleaned Data for Modeling.Rds")








#####
# Appendix: Explore Data
#####

###Typical Possession Length
possession <- clean_data %>% ungroup() %>%
  mutate(possession_id = cumsum(coalesce(as.numeric(!team_change1),0))) %>%
  count(possession_id, name = "actions_in_possession") %>%
  count(actions_in_possession, name = "occurrences") %>%
  mutate(perc = occurrences / sum(occurrences),
         cumulative_perc = cumsum(perc))

hist(possession$actions_in_possession)
ggplot(possession, aes(x = actions_in_possession, y = occurrences)) + 
  geom_histogram(stat = "identity", fill = "orange") +
  coord_cartesian(xlim = c(0,30)) +
  labs(title = "Possession Length Distribution", x = "# of Events in Possession", y = "Frequency") +
  theme_minimal()

### Check for "dupes" like we had with zone entries
same_time_events <- get_dupes(clean_data, game_date, game_time)
# Spot checking these and they generally seam reasonable  

