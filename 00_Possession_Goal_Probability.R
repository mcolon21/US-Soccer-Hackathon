
#### Project Concept ####

library(dplyr)

#filenames <- c("MLS 2016-2017 trimmed.csv")
#read_mls_file <- function(filename) {
#  season <- readr::read_csv(paste0("/Users/silasjackmorsink/Downloads/Hackathon Raw Files/Trimmed Columns - Opta/MLS/", filename))
#  for(name in names(season)) {season[[as.character(name)]] <- as.character(season[[as.character(name)]])}
#  return(season)
#}
#data <- purrr::map_df(filenames, read_mls_file)

data <- readr::read_csv("/Users/silasjackmorsink/Downloads/Hackathon Raw Files/Trimmed Columns - Opta/MLS/MLS 2016-2017 trimmed.csv")

# Running a logistic regression
# Predictor Variable 1: location of the ball (zone, maybe 5 by 5)
# Predictor Variable 2: vertical velocity of the ball (units of "x" per second)
# Predictor Variable 3: the event_type
# Outcome Variable: goal on the possession

#### Outcome Variable ####
# Goal on possession
# possession defined as a period of time during which only one team has the ball

add_next_columns <- function(variables, df) {
  for(variable in variables) {
    next_col_name <- paste0("NEXT_", as.character(variable))
    df[[next_col_name]] <- append(df[[as.character(variable)]], NA)[-1]
  }
  return(df)
}

data_with_nexts <- add_next_columns(c("team_id", "period_id"), data)
data_with_nexts$possession_change <- ((data_with_nexts$team_id != data_with_nexts$NEXT_team_id) | (data_with_nexts$period_id != data_with_nexts$NEXT_period_id)) & data_with_nexts$event_type != "Challenge"
data_with_nexts$possession_change <- replace(data_with_nexts$possession_change, which(is.na(data_with_nexts$possession_change)), F)

data_with_nexts$possession_number <- NA
data_with_nexts$goal_on_possession <- 0

data_with_nexts$event_type <- replace(data_with_nexts$event_type, which(is.na(data_with_nexts$event_type)), "none")

possession_num <- 1
possession_change_points <- which(data_with_nexts$possession_change == T)
possession_change_points <- append(possession_change_points, nrow(data_with_nexts))

for(i in 1:(length(possession_change_points) - 1)) {
  from <- possession_change_points[i]
  to <- possession_change_points[i + 1]
  data_with_nexts[from:to, 'possession_number'] <- i
  if(data_with_nexts[to, 'event_type'] == "Goal") {
    data_with_nexts[from:to, 'goal_on_possession'] <- 1
  }
}

data <- cbind(data_with_nexts)

#### Variable 1: location ####
# location_zone
# split the field into 400 zones, each 5 units by 5 units
# for any team, the zones go the same way as you get closer to the goal they're attacking
# zones go 1 to 20 left to right, then 21 to 40, then 41 to 60, etc

# must determine which direction a team is attacking

data$x <- as.double(data$x); data$y <- as.double(data$y)

y_part_of_zone <- data$y %/% 10
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone < 0), 0)
y_part_of_zone <- replace(y_part_of_zone, which(y_part_of_zone > 9), 9)
x_part_of_zone <- data$x %/% 10
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone < 0), 0)
x_part_of_zone <- replace(x_part_of_zone, which(x_part_of_zone > 9), 9)
data$attacking_location_zone <- x_part_of_zone * 10 + y_part_of_zone
data$attacking_location_zone <- as.character(data$attacking_location_zone)

#### Variable 2: vertical ball velocity ####

data$period_id <- as.double(data$period_id); data$period_min <- as.double(data$period_min); data$period_second <- as.double(data$period_second)
data$game_clock <- (data$period_id - 1) * 45 * 60 + (data$period_min) * 60 + data$period_second

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

data_with_prevs <- add_prev_columns(c("game_clock", "x"), data)
data_with_prevs <- add_prev_prev_columns(c("game_clock", "x"), data_with_prevs)

delta_time <- data_with_prevs$game_clock - data_with_prevs$PREV_PREV_game_clock
delta_x <- data_with_prevs$x - data_with_prevs$PREV_PREV_x
delta_time_one_event <- data_with_prevs$game_clock - data_with_prevs$PREV_game_clock
delta_x_one_event <- data_with_prevs$x - data_with_prevs$PREV_x

# if two events ago was more than 20 seconds ago, use only changes since last event
delta_time <- replace(delta_time, which(delta_time > 20), delta_time_one_event[which(delta_time > 20)])
delta_x <- replace(delta_x, which(delta_time > 20), delta_x_one_event[which(delta_time > 20)])

data_with_prevs$x_velo <- delta_x / delta_time
data_with_prevs$x_velo <- replace(data_with_prevs$x_velo, which(data_with_prevs$x_velo == Inf | data_with_prevs$x_velo == -Inf), 0)

data <- cbind(data_with_prevs)

#### Variable 3: Event Type ####
data$event_type_general <- data$event_type
data$event_type_general <- replace(data$event_type_general, which(data$event_type == "Pass" | data$event_type == "Aerial" | data$event_type == "Offside pass"), "pass")
data$event_type_general <- replace(data$event_type_general, which(data$event_type == "Start" | data$event_type == "Deleted event" | data$event_type == "Start delay" | data$event_type == "End delay" | data$event_type == "Formation change" | data$event_type == "Card" | data$event_type == "Injury time announcement" | data$event_type == "Team set up" | data$event_type == "none" | data$event_type == "Coach setup" | data$event_type == "End" | data$event_type == "Player off" | data$event_type == "Player on"), "non_gameplay")
data$event_type_general <- replace(data$event_type_general, which(data$event_type == "Dispossessed" | data$event_type == "Tackle" | data$event_type == "Blocked pass" | data$event_type == "Interception"), "defensive")
data$event_type_general <- replace(data$event_type_general, which(data$event_type == "Keeper pick-up" | data$event_type == "Save" | data$event_type == "Punch"), "keeper")
data$event_type_general <- replace(data$event_type_general, which(data$event_type == "Ball touched" | data$event_type == "Ball recovery" | data$event_type == "Take on" | data$event_type == "Good skill"), "dribble")
data$event_type_general <- replace(data$event_type_general, which(data$event_type == "Post" | data$event_type == "Miss" | data$event_type == "Goal" |  data$event_type == "Attempt saved"), "shot")

#### Logistic Regression ####
data <- readr::read_csv("/Users/silasjackmorsink/Desktop/2016_17_mls.csv")
set.seed(1871)
train_nrow <- round(nrow(data)) * 0.9
train_rows <- sample(1:nrow(data), train_nrow)
test_rows <- seq(1, nrow(data))[-train_rows]
train <- data[train_rows, ]
test <- data[test_rows, ]
test <- test[-which(test$event_type == "Condition changed" | test$event_type == "none"), ]

# readr::write_csv(data, "/Users/silasjackmorsink/Desktop/2016_17_mls.csv")
logit <- glm(goal_on_possession ~ attacking_location_zone + x_velo + event_type, data = train, family = "binomial")

test$goal_probability <- aod::predict(logit, newdata = test, type = "response")
plot(test$goal_probability, test$goal_on_possession)
summary(lm(goal_on_possession ~ goal_probability, data = test))
