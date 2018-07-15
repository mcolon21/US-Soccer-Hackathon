
new_data <- readr::read_csv("/Users/silasjackmorsink/Downloads/Hackathon Raw Files/Trimmed Columns - Opta/MLS/MLS 2017-2018 trimmed.csv")
team_id <- unique(new_data$team_id)[1]
team <- new_data[which(new_data$team_id == team_id), ]

#====
# adding necessary columns for model

team$x <- as.double(team$x); team$y <- as.double(team$y)

y_part_of_zone <- team$y %/% 10
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 9), 9)
x_part_of_zone <- team$x %/% 10
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 9), 9)
team$attacking_location_zone <- x_part_of_zone * 10 + y_part_of_zone
team$attacking_location_zone <- as.character(team$attacking_location_zone)


team$period_id <- as.double(team$period_id); team$period_min <- as.double(team$period_min); team$period_second <- as.double(team$period_second)
team$game_clock <- (team$period_id - 1) * 45 * 60 + (team$period_min) * 60 + team$period_second

add_prev_columns <- function(variables, df) {
  for(variable in variables) {
    prev_col_name <- paste0("PREV_", as.character(variable))
    df[[prev_col_name]] <- append(NA, df[[as.character(variable)]])[-(nrow(df) + 1)]
  }
  return(df)
}

add_prev_prev_columns <- function(variables, df) {
  for(variable in variables) {
    prev_prev_col_name <- paste0("PREV_PREV_", as.character(variable))
    df[[prev_prev_col_name]] <- append(c(NA, NA), df[[as.character(variable)]])[-c((nrow(df) + 1), (nrow(df) + 2))]
  }
  return(df)
}

team_with_prevs <- add_prev_columns(c("game_clock", "x"), team)
team_with_prevs <- add_prev_prev_columns(c("game_clock", "x"), team_with_prevs)

delta_time <- team_with_prevs$game_clock - team_with_prevs$PREV_PREV_game_clock
delta_x <- team_with_prevs$x - team_with_prevs$PREV_PREV_x
delta_time_one_event <- team_with_prevs$game_clock - team_with_prevs$PREV_game_clock
delta_x_one_event <- team_with_prevs$x - team_with_prevs$PREV_x

# if two events ago was more than 20 seconds ago, use only changes since last event
delta_time <- replace(delta_time, which(delta_time > 20), delta_time_one_event[which(delta_time > 20)])
delta_x <- replace(delta_x, which(delta_time > 20), delta_x_one_event[which(delta_time > 20)])

team_with_prevs$x_velo <- delta_x / delta_time
team_with_prevs$x_velo <- replace(team_with_prevs$x_velo, which(team_with_prevs$x_velo == Inf | team_with_prevs$x_velo == -Inf), 0)

team <- cbind(team_with_prevs)


team$event_type_general <- team$event_type
team$event_type_general <- replace(team$event_type_general, which(team$event_type == "Pass" | team$event_type == "Aerial" | team$event_type == "Offside pass"), "pass")
team$event_type_general <- replace(team$event_type_general, which(team$event_type == "Start" | team$event_type == "Deleted event" | team$event_type == "Start delay" | team$event_type == "End delay" | team$event_type == "Formation change" | team$event_type == "Card" | team$event_type == "Injury time announcement" | team$event_type == "Team set up" | team$event_type == "none" | team$event_type == "Coach setup" | team$event_type == "End" | team$event_type == "Player off" | team$event_type == "Player on"), "non_gameplay")
team$event_type_general <- replace(team$event_type_general, which(team$event_type == "Dispossessed" | team$event_type == "Tackle" | team$event_type == "Blocked pass" | team$event_type == "Interception"), "defensive")
team$event_type_general <- replace(team$event_type_general, which(team$event_type == "Keeper pick-up" | team$event_type == "Save" | team$event_type == "Punch"), "keeper")
team$event_type_general <- replace(team$event_type_general, which(team$event_type == "Ball touched" | team$event_type == "Ball recovery" | team$event_type == "Take on" | team$event_type == "Good skill"), "dribble")
team$event_type_general <- replace(team$event_type_general, which(team$event_type == "Post" | team$event_type == "Miss" | team$event_type == "Goal" |  team$event_type == "Attempt saved"), "shot")
team_event_types <- unique(team$event_type)
data_event_types <- unique(data$event_type)

delete_rows_with_this_event_type <- team_event_types[-which(team_event_types %in% data_event_types)]
team <- team[-which(team$event_type_general %in% delete_rows_with_this_event_type), ]

team <- team[-which(team$event_type_general == "Condition changed"), ]

#### Getting goal probabilities added ####

team$goal_probability <- aod::predict(logit, newdata = team, type = "response")

add_next_columns <- function(variables, df) {
  for(variable in variables) {
    next_col_name <- paste0("NEXT_", as.character(variable))
    df[[next_col_name]] <- append(df[[as.character(variable)]], NA)[-1]
  }
  return(df)
}

team_with_nexts <- add_next_columns("goal_probability", team)
team_with_nexts$gpa <- team_with_nexts$NEXT_goal_probability - team_with_nexts$goal_probability

team <- cbind(team_with_nexts)

# Looking on a player by player basis #
#### getting gpa to a given zone ####

y_part_of_zone <- team$pass_end_y %/% 20
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 4), 4)
x_part_of_zone <- team$pass_end_x %/% 20
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 4), 4)
team$pass_end_zone <- x_part_of_zone * 5 + y_part_of_zone

player_id <- unique(team$player_id)[5]
player <- team[which(team$player_id == player_id), ]

gpa_to_zone <- player %>%
  filter(event_type_general == "pass") %>%
  group_by(pass_end_zone) %>%
  summarize(n_passes = n(), sum_gpa = sum(gpa))
  
gpa_to_zone <- gpa_to_zone[-which(is.na(gpa_to_zone$pass_end_zone)), ]

#### getting team gp at a given zone ####
# Matt is doing this relative to expectation/population gps at each zone
# so you get a sense of how each team differs from the norm

y_part_of_zone <- team$y %/% 20
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 4), 4)
x_part_of_zone <- team$x %/% 20
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 4), 4)
team$new_zone <- x_part_of_zone * 5 + y_part_of_zone

team_for_gp <- add_prev_columns("player_id", team)
team_for_gp <- team[(team_for_gp$PREV_player_id != player_id) & (team_for_gp$event_type_general == "pass" | team_for_gp$event_type_general == "shot"), ]

team_zones_gp <- team_for_gp %>%
  group_by(new_zone) %>%
  summarize(sum(goal_probability))

player_pass_and_shot <- player[which(player$new_zone %in% adequate_data_zones & ((player$event_type_general == "pass") | (player$event_type_general == "shot"))), ]
player_hot_zone_threshold <- as.double(quantile(player$gpa, 0.625, na.rm = T))
player_hot_zones <- player_pass_and_shot %>%
  group_by(new_zone) %>%
  summarize(avg_gpa = mean(gpa)) %>%
  filter(avg_gpa > player_hot_zone_threshold) %>%
  arrange(desc(avg_gpa))




