library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)

pbp_full_df <- readRDS(file = "data/pbp.rds")

## Filter out OT games for now
# Create the EP model dataset that only includes plays with basic seven
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

# Remove  [8] "Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized"
remove_plays <- c("Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized",
                  "Penalty","Kickoff","Kickoff Return (Offense)")

pbp_no_OT <-
  pbp_full_df %>% filter(period <= 4, down > 0) %>%
  filter(!play_type %in% remove_plays) %>%
  filter(!is.na(down),!is.na(raw_secs)) %>%
  filter(log_ydstogo != -Inf) %>%
  rename(TimeSecsRem = raw_secs) %>%
  mutate(Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"))

##+ Under_TwoMinute_Warning
## Create a weighting factor
# need
ep_model <- nnet::multinom(Next_Score ~ TimeSecsRem + adj_yd_line +
                             down + log_ydstogo + log_ydstogo*down +
                             adj_yd_line*down, data = pbp_no_OT, maxit = 300)

test = pbp_no_OT %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo)
ep_start = predict(ep_model,test,type='prob')
test = cbind(test,ep_start)

# New Down = 1
# New Distance = 10



prep_df_epa <- function(dat){
  turnover_play_type = c(
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception'
  )

  dat = dat %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      play_text = tolower(play_text),
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      coef = home_team == defense,
      adj_yd_line = 100 * (1 - coef) + (2 * coef - 1) * yard_line,
      log_ydstogo = log(adj_yd_line),
      half = ifelse(period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0,
      turnover = 0
  ) %>% select(-coef)

  # Turnover Index
  turnover_ind = dat$play_type %in% turnover_play_type
  dat[turnover_ind,"new_down"] = 1
  dat[turnover_ind,"new_distance"] = 10
  # First down
  first_down_ind = str_detect(pbp_no_OT$play_text,'1st')
  dat[first_down_ind,"new_down"] = 1
  dat[first_down_ind,"new_distance"] 10
  # Otherwise What happened?
  dat[(!turnover_ind & !first_down_ind),"new_down"] = dat[(!turnover_ind & !first_down_ind),"down"] + 1
  dat[(!turnover_ind & !first_down_ind),"new_distance"] = dat[(!turnover_ind & !first_down_ind),"distance"] - dat[(!turnover_ind & !first_down_ind),"yards_gained"]


  fifty_ydline = str_detect(pbp_no_OT$play_text,"50 yard line")
  dat[fifty_ydline,"new_yardline"] = 50


}


## pick an SEC game

tamu_18 = pbp_no_OT %>% filter(
  year == 2018,
  offense %in% c("Clemson", "Texas A&M"),
  defense %in% c("Clemson", "Texas A&M")
) %>% select(-X1)
