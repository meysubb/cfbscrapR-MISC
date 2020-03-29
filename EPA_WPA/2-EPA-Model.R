library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(nnet)
library(mgcv)

source("6-Data-Ingest-Utils.R")
source("7-CV-Utils.R")

pbp_full_df <- readRDS(file = "data/pbp.rds")

## Filter out OT games for now
# Create the EP model dataset that only includes plays with basic seven
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

# Remove  [8] "Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized"
remove_plays <-
  c(
    "Extra Point Missed",
    "Extra Point Good",
    "Timeout",
    "End of Half",
    "End of Game",
    "Uncategorized",
    #"Kickoff",
    "Penalty"
    #"Kickoff Return (Offense)",
    #"Kickoff Return Touchdown"
  )

## need to remove games with 


pbp_no_OT <- pbp_full_df %>% filter(down > 0) %>%
  filter(year>=2014) %>%   
  filter(!play_type %in% remove_plays) %>%
  filter(!is.na(down),!is.na(raw_secs)) %>%
  filter(!is.na(game_id)) %>%
  filter(log_ydstogo != -Inf) %>%
  filter(down>0) %>% 
  rename(TimeSecsRem = raw_secs) %>%
  mutate(
    Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"),
    down = as.factor(down),
    Goal_To_Go = ifelse(
      str_detect(play_type, "Field Goal"),
      distance >= (yards_to_goal - 17),
      distance >= yards_to_goal
    ),
    Under_two = TimeSecsRem <= 120,
    id_play = as.numeric(id_play)
  ) %>% filter(!is.na(game_id))

# fg_contains = str_detect((pbp_no_OT$play_type),"Field Goal")
# fg_no_OT <- pbp_no_OT[fg_contains,]
# 
# fg_model <- mgcv::bam(scoring ~ s(yards_to_goal),
#                       data = fg_no_OT, family = "binomial")
# saveRDS(fg_model,"models/fg_model.rds")
# # Load FG Model
# #fg_model = readRDS("models/fg_model.rds")
# summary(fg_model)
# weight factor, normalized absolute score differential.
# flip the normilization so that larger scores don't matter as much as closer scores
wts = pbp_no_OT %>%
  mutate(
    weights_raw = abs(offense_score - defense_score),
    weights = (max(weights_raw) - weights_raw)/(max(weights_raw)-min(weights_raw))
  ) %>% pull(weights)

# ep_model <-
#   nnet::multinom(
#     Next_Score ~ TimeSecsRem + yards_to_goal + down  + log_ydstogo + Goal_To_Go + Under_two +
#       log_ydstogo * down +
#       yards_to_goal * down + 
#       Goal_To_Go * log_ydstogo,
#     data = pbp_no_OT,
#     maxit = 300,
#     weights = wts
#   )

# Create the LOSO predictions for the selected cfbscrapR model:
ep_model_loso_preds <- calc_ep_multinom_loso_cv(as.formula("Next_Score ~ 
                                                           TimeSecsRem + yards_to_goal + 
                                                           down + log_ydstogo + Goal_To_Go + log_ydstogo*down + 
                                                           yards_to_goal*down + Goal_To_Go*log_ydstogo + 
                                                           Under_two"),ep_model_data = pbp_no_OT)

# Save dataset in data folder as ep_model_loso_preds.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
write.csv(ep_model_loso_preds , "data/ep_model_loso_preds.csv", row.names = FALSE)

# Create the LOSO predictions for the selected cfbscrapR models:
ep_fg_model_loso_preds <- calc_ep_multinom_fg_loso_cv(as.formula("Next_Score ~ 
                                                                 TimeSecsRem + 
                                                                 yards_to_goal + down + 
                                                                 log_ydstogo + Goal_To_Go + 
                                                                 log_ydstogo*down + 
                                                                 yards_to_goal*down + 
                                                                 Goal_To_Go*log_ydstogo + 
                                                                 Under_two"),
                                                      as.formula("scoring ~ s(yards_to_goal)"),
                                                      ep_model_data = pbp_no_OT)

write.csv(ep_fg_model_loso_preds,"ep_fg_model_data_loso.csv",row.names=FALSE)

# # # need
# ep_model <- nnet::multinom(Next_Score ~ TimeSecsRem + yards_to_goal + Under_two +
#                                down + log_ydstogo + log_ydstogo*down +
#                               yards_to_goal*down + Goal_To_Go, data = pbp_no_OT, maxit = 300,
#                            weights = wts)



ep_model
saveRDS(ep_model,"models/ep_model.rds")
# Load EPA Model
ep_model = readRDS("models/ep_model.rds")


all_years_epa = lapply(all_years, function(x) {
  year = unique(x$year)
  print(year)
  val = calculate_epa(x,extra_cols=F)
  return(val)
})


len = length(all_years_epa)

for (i in 1:len) {
  df = all_years_epa[[i]]
  hist(df$EPA)
  Sys.sleep(5)
}



lapply(names(all_years_epa), function(x) {
  write.csv(
    all_years_epa[[x]],
    file = paste0("data/csv/EPA_calcs_", x, ".csv"),
    row.names = F
  )
})

lapply(names(all_years_epa), function(x) {
  saveRDS(
    all_years_epa[[x]],
    file = paste0("data/rds/EPA_calcs_", x, ".RDS"),
    compress = T
  )
})