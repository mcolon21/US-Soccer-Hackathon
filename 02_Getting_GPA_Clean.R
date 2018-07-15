
#### formatting new data ####
new_data <- readr::read_csv("/Users/silasjackmorsink/Downloads/Hackathon Raw Files/Trimmed Columns - Opta/MLS/MLS 2017-2018 trimmed.csv")

new_data$x <- as.double(new_data$x); new_data$y <- as.double(new_data$y)
y_part_of_zone <- new_data$y %/% 10
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 9), 9)
x_part_of_zone <- new_data$x %/% 10
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 9), 9)
new_data$attacking_location_zone <- x_part_of_zone * 10 + y_part_of_zone
new_data$attacking_location_zone <- as.character(new_data$attacking_location_zone)

new_data$period_id <- as.double(new_data$period_id); new_data$period_min <- as.double(new_data$period_min); new_data$period_second <- as.double(new_data$period_second)
new_data$game_clock <- (new_data$period_id - 1) * 45 * 60 + (new_data$period_min) * 60 + new_data$period_second
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
new_data_with_prevs <- add_prev_columns(c("game_clock", "x"), new_data)
new_data_with_prevs <- add_prev_prev_columns(c("game_clock", "x"), new_data_with_prevs)
delta_time <- new_data_with_prevs$game_clock - new_data_with_prevs$PREV_PREV_game_clock
delta_x <- new_data_with_prevs$x - new_data_with_prevs$PREV_PREV_x
delta_time_one_event <- new_data_with_prevs$game_clock - new_data_with_prevs$PREV_game_clock
delta_x_one_event <- new_data_with_prevs$x - new_data_with_prevs$PREV_x
# if two events ago was more than 20 seconds ago, use only changes since last event
delta_time <- replace(delta_time, which(delta_time > 20), delta_time_one_event[which(delta_time > 20)])
delta_x <- replace(delta_x, which(delta_time > 20), delta_x_one_event[which(delta_time > 20)])
new_data_with_prevs$x_velo <- delta_x / delta_time
new_data_with_prevs$x_velo <- replace(new_data_with_prevs$x_velo, which(new_data_with_prevs$x_velo == Inf | new_data_with_prevs$x_velo == -Inf), 0)
new_data <- cbind(new_data_with_prevs)

new_data$event_type_general <- new_data$event_type
new_data$event_type_general <- replace(new_data$event_type_general, which(new_data$event_type == "Pass" | new_data$event_type == "Aerial" | new_data$event_type == "Offside pass"), "pass")
new_data$event_type_general <- replace(new_data$event_type_general, which(new_data$event_type == "Start" | new_data$event_type == "Deleted event" | new_data$event_type == "Start delay" | new_data$event_type == "End delay" | new_data$event_type == "Formation change" | new_data$event_type == "Card" | new_data$event_type == "Injury time announcement" | new_data$event_type == "new_data set up" | new_data$event_type == "none" | new_data$event_type == "Coach setup" | new_data$event_type == "End" | new_data$event_type == "Player off" | new_data$event_type == "Player on"), "non_gameplay")
new_data$event_type_general <- replace(new_data$event_type_general, which(new_data$event_type == "Dispossessed" | new_data$event_type == "Tackle" | new_data$event_type == "Blocked pass" | new_data$event_type == "Interception"), "defensive")
new_data$event_type_general <- replace(new_data$event_type_general, which(new_data$event_type == "Keeper pick-up" | new_data$event_type == "Save" | new_data$event_type == "Punch"), "keeper")
new_data$event_type_general <- replace(new_data$event_type_general, which(new_data$event_type == "Ball touched" | new_data$event_type == "Ball recovery" | new_data$event_type == "Take on" | new_data$event_type == "Good skill"), "dribble")
new_data$event_type_general <- replace(new_data$event_type_general, which(new_data$event_type == "Post" | new_data$event_type == "Miss" | new_data$event_type == "Goal" |  new_data$event_type == "Attempt saved"), "shot")

#### applying goal probability ####
new_data <- new_data[-which(new_data$event_type == "Condition changed"), ]
new_data$attacking_location_zone <- as.double(new_data$attacking_location_zone)
new_data$goal_probability <- aod::predict(logit, newdata = new_data, type = "response")
add_next_columns <- function(variables, df) {
  for(variable in variables) {
    next_col_name <- paste0("NEXT_", as.character(variable))
    df[[next_col_name]] <- append(df[[as.character(variable)]], NA)[-1]
  }
  return(df)
}
new_data_with_nexts <- add_next_columns("goal_probability", new_data)
new_data_with_nexts$gpa <- new_data_with_nexts$NEXT_goal_probability - new_data_with_nexts$goal_probability
new_data <- cbind(new_data_with_nexts)
# readr::read_csv("/Users/silasjackmorsink/Desktop/2017_18_mls.csv")

y_part_of_zone <- new_data$pass_end_y %/% 20
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 4), 4)
x_part_of_zone <- new_data$pass_end_x %/% 20
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 4), 4)
new_data$pass_end_zone <- x_part_of_zone * 5 + y_part_of_zone
new_data$pass_yn <- new_data$event_type_general == "pass"

y_part_of_zone <- new_data$y %/% 20
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 4), 4)
x_part_of_zone <- new_data$x %/% 20
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 4), 4)
new_data$new_zone <- x_part_of_zone * 5 + y_part_of_zone

#### gp for player ####

player_reception_gp <- new_data %>%
  filter(event_type_general == "pass" | event_type_general == "shot") %>%
  group_by(player_id, new_zone) %>%
  summarize(sum_gp = sum(goal_probability), n_pass_or_shot = n())

get_sum_gp_z <- function(zone) {
  zone_specific <- player_reception_gp[player_reception_gp$new_zone == zone, ]
  zone_specific[["sum_gp_z"]] <- as.vector(scale(zone_specific[["sum_gp"]]))
  return(zone_specific)
}
player_reception_gp_z <- purrr::map_df(unique(player_reception_gp$new_zone), get_sum_gp_z)

readr::write_csv(player_reception_gp_z, "Desktop/player_reception_gp_z.csv")

#### gpa for player ####

get_end_zone_gpa_by_player <- function(player_id) {
  player <- new_data[which(new_data$player_id == player_id & new_data$event_type_general == "pass"), ]
  gpa_to_zone <- player %>%
    group_by(pass_end_zone) %>%
    summarize(sum_gpa = sum(gpa), n_passes = sum(pass_yn), team_id = first(team_id))
  if(length(which(is.na(gpa_to_zone$pass_end_zone))) > 0) {
    gpa_to_zone <- gpa_to_zone[-which(is.na(gpa_to_zone$pass_end_zone)), ]
  }
  if(nrow(gpa_to_zone) < 1) {
    return(data.frame(pass_end_zone = 10000, sum_gpa = 10000, pct_gpa = 10000, player_id = 10000))
  }
  gpa_to_zone$pct_gpa <- gpa_to_zone$sum_gpa / sum(gpa_to_zone$sum_gpa)
  gpa_to_zone$player_id <- player_id
  return(gpa_to_zone)
}

pids <- unique(new_data$player_id)[-which(is.na(new_data$player_id))]
player_end_zone_gpas <- purrr::map_df(pids, get_end_zone_gpa_by_player)
player_end_zone_gpas <- player_end_zone_gpas[-which(player_end_zone_gpas$pass_end_zone == 10000), ]
IQR(player_end_zone_gpas$pct_gpa, na.rm = T) * 1.5
upper_cutoff <- IQR(player_end_zone_gpas$pct_gpa, na.rm = T) * 1.5 + as.double(quantile(player_end_zone_gpas$pct_gpa, 0.75, na.rm = T))
lower_cutoff <- as.double(quantile(player_end_zone_gpas$pct_gpa, 0.25, na.rm = T)) - IQR(player_end_zone_gpas$pct_gpa, na.rm = T) * 1.5 
player_end_zone_gpas <- player_end_zone_gpas[which(player_end_zone_gpas$pct_gpa <= upper_cutoff & player_end_zone_gpas$pct_gpa >= lower_cutoff), ]

#### merging with team gps ####

team_gps <- readr::read_csv("/Users/silasjackmorsink/Downloads/team_goal_probabilities.csv")
chem_table <- dplyr::left_join(player_end_zone_gpas, team_gps, by = c("pass_end_zone" = "new_zone", "team_id" = "team"))

player_table <- chem_table %>%
  dplyr::select(player_id, pass_end_zone, sum_gpa, n_passes)
player_table <- player_table[!duplicated(player_table), ]
get_sum_gpa_z <- function(zone) {
  zone_specific <- player_table[player_table$pass_end_zone == zone, ]
  zone_specific[["sum_gpa_z"]] <- as.vector(scale(zone_specific[["sum_gpa"]]))
  return(zone_specific)
}
player_table_updated <- purrr::map_df(unique(player_table$pass_end_zone), get_sum_gpa_z)

team_table <- chem_table %>%
  dplyr::select(team_id, pass_end_zone, sum_gp, removed_player, n)
team_table <- team_table[!duplicated(team_table), ]
get_sum_gp_z <- function(zone) {
  zone_specific <- team_table[team_table$pass_end_zone == zone, ]
  zone_specific[["sum_gp_z"]] <- as.vector(scale(zone_specific[["sum_gp"]]))
  return(zone_specific)
}
team_table_updated <- purrr::map_df(unique(team_table$pass_end_zone), get_sum_gp_z)

combined <- dplyr::left_join(player_table_updated, team_table_updated, by = c("player_id" = "removed_player", "pass_end_zone" = "pass_end_zone"))
combined$z_diff <- abs(combined$sum_gpa_z - combined$sum_gp_z)
combined$adjusted_similarity <- combined$z_diff * combined$n_passes

team_final <- combined %>%
  group_by(team_id) %>%
  summarize(n_passes = sum(n_passes), total_adjusted_similarity = sum(adjusted_similarity, na.rm = T)) %>%
  mutate(final_similarity = total_adjusted_similarity / n_passes) %>%
  arrange(final_similarity)
team_final <- team_final[-which(is.na(team_final$team_id)), ]

###
chem_table$abs_error <- abs(chem_table$pct_gpa - chem_table$prop_gp)
# chem_table$weighted_error <- chem_table$abs_error * chem_table$n_passes

player_table <- chem_table %>%
  dplyr::group_by(player_id) %>%
  dplyr::summarize(mean_error = mean(abs_error), team_id = first(team_id), n_touches = sum(n))

team_table <- chem_table %>%
  group_by(team_id) %>%
  summarize(sum_abs_error = sum(abs_error)) %>%
  mutate(z = as.vector(scale(sum_abs_error))) %>%
  arrange(z) %>%
  left_join(team_id_lookup, by = "team_id")

team_table <- player_table %>%
  group_by(team_id) %>%
  summarize(sum_mean_error = sum(mean_error)) %>%
  mutate(z = as.vector(scale(sum_mean_error))) %>%
  arrange(z) %>%
  left_join(team_id_lookup, by = "team_id")
##

team_id_lookup <- new_data %>%
  group_by(team_id) %>%
  summarize(team_name = first(team))

team_final <- left_join(team_final, team_id_lookup, by = "team_id")
team_relative_performance <- readr::read_csv("/Users/silasjackmorsink/Downloads/2017-18_mls_standings.csv")
team_table_performance <- left_join(team_final, team_relative_performance, by = c("team_name" = "Club"))

ggplot(data = team_table_performance, aes(x = final_similarity, y = pts_above_projection)) +
  geom_point() +
  geom_smooth(method = "lm")
summary(lm(pts_above_projection ~ final_similarity, data = team_table_performance))

#### Player heat map example ####

example <- get_end_zone_gpa_by_player(unique(new_data$player_id[101]))
example$x_coordinate <- example$pass_end_zone %/% 5
example$y_coordinate <- example$pass_end_zone %% 5
library(ggplot2)
ggplot(data = example, aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = pct_gpa))

pids <- unique(chem_table$player_id)

chem_table <- readr::read_csv("Downloads/chem_table.csv")
chem_table$x_coordinate <- chem_table$pass_end_zone %/% 5
chem_table$y_coordinate <- chem_table$pass_end_zone %% 5
library(ggplot2)
ggplot(data = chem_table[(chem_table$player_id == 37996), ], aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = sum_gp_z))

good_fits <- chem_table %>%
  group_by(player_id) %>%
  summarize(sum_adj_sim = sum(adjusted_similarity, na.rm = T), n = sum(n_pass_or_shot, na.rm = T)) %>%
  arrange(sum_adj_sim) %>%
  filter(n > 500)
good_fits <- good_fits$player_id[1:5]

bad_fits <- chem_table %>%
  group_by(player_id) %>%
  summarize(sum_adj_sim = sum(adjusted_similarity, na.rm = T), n = sum(n_pass_or_shot, na.rm = T)) %>%
  arrange(desc(sum_adj_sim)) %>%
  filter(n > 500)
bad_fits <- bad_fits$player_id[1:5]

# player_ids that fit well

player_id <- good_fits[2]
player_team_id <- new_data[which(new_data$player_id == good_fits[2]), 'team_id'][1]
player_name <- new_data[which(new_data$player_id == good_fits[2]), 'player'][1]
player_team <- new_data[which(new_data$player_id == good_fits[2]), 'team'][1]
ggplot(data = chem_table[(chem_table$player_id == good_fits[2]), ], aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = sum_gp_z)) + 
  scale_fill_continuous(limits=c(-1, 1), breaks=seq(-1,1,by=0.5))
ggplot(data = chem_table[(chem_table$team == player_team_id & chem_table$player_id == player_id), ], aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = team_gpa_z))

player_id <- good_fits[3]
player_team_id <- new_data[which(new_data$player_id == player_id), 'team_id'][1]
player_name <- new_data[which(new_data$player_id == player_id), 'player'][1]
player_team <- new_data[which(new_data$player_id == player_id), 'team'][1]
ggplot(data = chem_table[(chem_table$player_id == good_fits[3]), ], aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = sum_gp_z), na.rm = F) + 
  scale_fill_continuous(limits=c(-1, 1), breaks=seq(-1, 1, by=1))
ggplot(data = chem_table[(chem_table$team == player_team_id & chem_table$player_id == player_id), ], aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = team_gpa_z), na.rm = F) +
  scale_fill_continuous(limits=c(-1, 1), breaks=seq(-1, 1, by=1))


# player_ids that don't fit well

### a team

team1 <- chem_table[which(chem_table$team == unique(chem_table$team)[1]), ] %>%
  group_by(player_id) %>%
  summarize(player_sim = sum(adjusted_similarity, na.rm = T))
write.csv(team1, "Desktop/team1.csv")

overall <- chem_table %>%
  group_by(pass_end_zone) %>%
  summarize(sum_gp = sum(sum_gp, na.rm = T), sum_gpa = sum(team_sum_gpa, na.rm = T))
overall$x_coordinate <- overall$pass_end_zone %/% 5
overall$y_coordinate <- overall$pass_end_zone %% 5

ggplot(data = overall, aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = sum_gpa)) +
  ggtitle("Sum of Goal Probability Added by Pass End Zone, MLS 2017-18")

ggplot(data = overall, aes(x_coordinate, y_coordinate)) +
  geom_tile(aes(fill = sum_gp)) +
  ggtitle("Sum of Goal Probabilities by Zone, MLS 2017-18")
