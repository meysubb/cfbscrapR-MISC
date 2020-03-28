
library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(nnet)
library(mgcv)

source("6-Utils.R")

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


pbp_no_OT <-
  pbp_full_df %>% filter(down > 0) %>%
  filter(!play_type %in% remove_plays) %>%
  filter(!is.na(down),!is.na(raw_secs)) %>%
  filter(!is.na(game_id) %>%
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

fg_contains = str_detect((pbp_no_OT$play_type),"Field Goal")
fg_no_OT <- pbp_no_OT[fg_contains,]

fg_model <- mgcv::bam(scoring ~ s(adj_yd_line),
                       data = fg_no_OT, family = "binomial")
saveRDS(fg_model,"models/fg_model.rds")
# Load FG Model
#fg_model = readRDS("models/fg_model.rds")
summary(fg_model)
# weight factor, normalized absolute score differential.
# flip the normilization so that larger scores don't matter as much as closer scores
wts = pbp_no_OT %>%
  mutate(
    weights_raw = abs(offense_score - defense_score),
    weights = (max(weights_raw) - weights_raw)/(max(weights_raw)-min(weights_raw))
  ) %>% pull(weights)

ep_model <-
  nnet::multinom(
    Next_Score ~ TimeSecsRem + yards_to_goal + down  + log_ydstogo + Goal_To_Go + Under_two +
      log_ydstogo * down +
      yards_to_goal * down + 
      Goal_To_Go * log_ydstogo,
    data = pbp_no_OT,
    maxit = 300,
    weights = wts
  )

# Create the LOSO predictions for the selected cfbscrapR model:
ep_model_loso_preds <- calc_ep_multinom_loso_cv(as.formula("NSH ~ 
                                 TimeSecsRem + adj_yd_line + 
                                 down + log_ydstogo + Goal_To_Go + log_ydstogo*down + 
                                 adj_yd_line*down + Goal_To_Go*log_ydstogo + 
                                 Under_two"),ep_model_data = pbp_no_OT)

# Save dataset in data folder as ep_model_loso_preds.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
write.csv(ep_model_loso_preds , "data/ep_model_loso_preds.csv", row.names = FALSE)

# Create the LOSO predictions for the selected cfbscrapR models:
ep_fg_model_loso_preds <- calc_ep_multinom_fg_loso_cv(as.formula("NSH ~ 
                                                                 TimeSecsRem + 
                                                                 adj_yd_line + down + 
                                                                 log_ydstogo + Goal_To_Go + 
                                                                 log_ydstogo*down + 
                                                                 adj_yd_line*down + 
                                                                 Goal_To_Go*log_ydstogo + 
                                                                 Under_two"),
                                                      as.formula("scoring ~ s(adj_yd_line)"),
                                                      ep_model_data = pbp_no_OT)

write.csv(ep_fg_model_loso_preds,"ep_fg_model_data_loso.csv",row.names=FALSE)

# # # need
# ep_model <- nnet::multinom(Next_Score ~ TimeSecsRem + adj_yd_line + Under_two +
#                                down + log_ydstogo + log_ydstogo*down +
#                               adj_yd_line*down + Goal_To_Go, data = pbp_no_OT, maxit = 300,
#                            weights = wts)



ep_model
saveRDS(ep_model,"models/ep_model.rds")
# Load EPA Model
ep_model = readRDS("models/ep_model.rds")



## At this point, let's create a function to predict EP_before and EP_after, and calculate EPA

calculate_epa_local <- function(clean_pbp_dat, ep_model, fg_model) {
  # constant vectors to be used again
  turnover_play_type = c(
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception',
    'Punt',
    "Field Goal Missed",
    "Blocked Field Goal"
  )

  off_TD = c(
    'Passing Touchdown',
    'Rushing Touchdown',
    "Fumble Recovery (Own) Touchdown",
    "Pass Reception Touchdown"
  )
  def_TD = c(
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Missed Field Goal Return Touchdown',
    "Fumble Return Touchdown Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Punt Return Touchdown",
    "Blocked Punt Touchdown",
    "Sack Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Pass Interception Return Touchdown",
    "Kickoff Touchdown"
  )

  pred_df = clean_pbp_dat %>% arrange(id) %>%  select(
    id,
    drive_id,
    game_id,
    TimeSecsRem,
    down,
    distance,
    adj_yd_line,
    log_ydstogo,
    Under_two,
    Goal_To_Go
  )

  # ep_start
  ep_start = as.data.frame(predict(ep_model, pred_df, type = 'prob'))
  colnames(ep_start) <- ep_model$lev
  ep_start_update = epa_fg_probs(dat = clean_pbp_dat,
                                 current_probs = ep_start,
                                 fg_model = fg_model)
  weights = c(0, 3, -3, -2, -7, 2, 7)
  pred_df$ep_before = apply(ep_start_update, 1, function(row) {
    sum(row * weights)
  })

  ## Prep for EP_after
  prep_df_after = prep_df_epa2(clean_pbp_dat)
  if (length(unique(clean_pbp_dat$game_id)) > 1) {
    # if you are trying to deal with multiple games at once
    # then you have to get the after individually.
    prep_df_after = map_dfr(unique(clean_pbp_dat$game_id),
                            function(x) {
                              clean_pbp_dat %>%
                                filter(game_id == x) %>%
                                prep_df_epa2()
                            })
  }

  ep_end = predict(ep_model, prep_df_after, type = 'prob')
  colnames(ep_end) <- ep_model$lev
  pred_df$ep_after = apply(ep_end, 1, function(row) {
    sum(row * weights)
  })

  colnames(prep_df_after)[4:12] = paste0(colnames(prep_df_after)[4:12], "_end")
  pred_df = clean_pbp_dat %>% left_join(prep_df_after) %>% left_join(pred_df %>% select(id_play, drive_id, game_id, ep_before, ep_after))
  #pred_df$turnover = turnover_col
  ## kickoff plays
  ## calculate EP before at kickoff as what happens if it was a touchback
  ## 25 yard line in 2012 and onwards
  kickoff_ind = (pred_df$play_type =='Kickoff')
  new_kick = pred_df[kickoff_ind,]
  new_kick["adj_yd_line"] = 75
  new_kick["log_ydstogo"] = log(75)
  ep_kickoffs = as.data.frame(predict(ep_model, new_kick, type = 'prob'))
  pred_df[(pred_df$play_type =='Kickoff'),"ep_before"] = apply(ep_kickoffs,1,function(row){
    sum(row*weights)
  })

  turnover_plays = which(pred_df$turnover_end == 1 & !kickoff_ind)
  pred_df[turnover_plays, "ep_after"] = -1 * pred_df[turnover_plays, "ep_after"]

  # game end EP is 0
  pred_df[pred_df$end_half_game_end == 1, "ep_after"] = 0

  ## scoring plays from here on out
  pred_df[(pred_df$play_type %in% off_TD), "ep_after"] = 7
  pred_df[(pred_df$play_type %in% def_TD), "ep_after"] = -7
  pred_df[pred_df$play_type == "Safety", "ep_after"] = -2
  pred_df[pred_df$play_type == "Field Goal Good", "ep_after"] = 3



  pred_df = pred_df %>%
    mutate(EPA = ep_after - ep_before) %>%
    select(-yard_line,
           -coef,
           -log_ydstogo_end,-Goal_To_Go_end) %>% select(
             game_id,
             drive_id,
             id,
             offense,
             offense_conference,
             defense,
             defense_conference,
             home,
             away,
             period,
             half,
             clock.minutes,
             clock.seconds,
             offense_score,
             defense_score,
             play_type,
             play_text,
             scoring,
             TimeSecsRem,
             down,
             distance,
             adj_yd_line,
             yards_gained,
             TimeSecsRem_end,
             down_end,
             distance_end,
             adj_yd_line_end,
             turnover_end,
             Under_two_end,
             everything()
           ) %>%
    mutate(
      rz_play = ifelse((adj_yd_line <= 20), 1, 0),
      scoring_opp = ifelse((adj_yd_line <= 40), 1, 0),
      pass = if_else(
        play_type == "Pass Reception" | play_type == "Passing Touchdown" |
          play_type == "Sack" |
          play_type == "Pass Interception Return" |
          play_type == "Pass Incompletion" |
          play_type == "Sack Touchdown" |
          (play_type == "Safety" &
             str_detect(play_text, "sacked")) |
          (
            play_type == "Fumble Recovery (Own)" &
              str_detect(play_text, "pass")
          ) |
          (
            play_type == "Fumble Recovery (Opponent)" &
              str_detect(play_text, "pass")
          ),
        1,
        0
      ),
      rush = ifelse(
        play_type == "Rush" |
          (play_type == "Safety" &
             str_detect(play_text, "run")) |
          (
            play_type == "Fumble Recovery (Opponent)" &
              str_detect(play_text, "run")
          ) |
          (
            play_type == "Fumble Recovery (Own)" &
              str_detect(play_text, "run")
          ),
        1,
        0
      ),
      stuffed_run = ifelse((rush == 1 &
                              yards_gained <= 0), 1, 0),
      success = ifelse(
        yards_gained >= .5 * distance & down == 1,
        1,
        ifelse(
          yards_gained >= .7 * distance & down == 2,
          1,
          ifelse(
            yards_gained >= distance & down == 3,
            1,
            ifelse(yards_gained >= distance &
                     down == 4, 1, 0)
          )
        )
      ),
      success = ifelse(play_type %in% turnover_play_type, 0, success),
      epa_success = ifelse(EPA > 0, 1, 0)
    )
  return(pred_df)
}

## create a function to ingest the pbp data
## and aid in figuring out probabilities for FG

epa_fg_probs <- function(dat, current_probs, fg_mod) {
  fg_ind = str_detect((dat$play_type), "Field Goal")
  fg_dat = dat[fg_ind, ]

  # we are setting everythign after 0 seconds to have
  # 0 probs.
  end_game_ind = which(dat$TimeSecsRem <= 0)
  current_probs[end_game_ind, ] <- 0

  make_fg_prob <- mgcv::predict.bam(fg_mod, newdata = fg_dat,
                                    type = "response")

  # add in the fg make prob into this
  current_probs2 <- current_probs
  #current_probs2[fg_ind,] <- current_probs[fg_ind,] * (1-make_fg_prob)
  val = (1 - make_fg_prob)
  ind <- dim(current_probs2[fg_ind, ])[1]
  for (i in seq(1, ind)) {
    temp = current_probs2[fg_ind, ]
    temp[i, ] = temp[i, ] * val[i]
  }
  current_probs2[fg_ind, ] =  temp


  # now to flip all the probs,
  current_probs2[fg_ind, "FG"] <-
    make_fg_prob + current_probs[fg_ind, "Opp FG"]
  current_probs2[fg_ind, "Opp FG"] <- current_probs[fg_ind, "FG"]
  current_probs2[fg_ind, "TD"] <- current_probs[fg_ind, "Opp TD"]
  current_probs2[fg_ind, "Opp TD"] <- current_probs[fg_ind, "TD"]
  current_probs2[fg_ind, "Safety"] <-
    current_probs[fg_ind, "Opp Safety"]
  current_probs2[fg_ind, "Opp Safety"] <-
    current_probs[fg_ind, "Safety"]
  return(current_probs2)
}


### Figure out who is who, so you can attribute players to it.
identify_players <- function(pbp_df) {
  pbp_df = pbp_df %>% mutate(
    passer_name = NA,
    receiver_name = NA,
    rusher_name = NA,
    pass_rusher_name_1 = NA,
    pass_rusher_name_2 = NA,
    force_fumble_player = NA,
    sacked_player_name = NA,
    int_player_name = NA,
    deflect_player_name = NA
  )

  pbp_df = pbp_df %>%
    mutate(
      ## Passes - QB
      passer_name = str_split(play_text, "pass") %>% map_chr(., 1),
      passer_name = ifelse(str_detect(play_text, "pass"), passer_name, NA),
      ## all rushes
      rusher_name = str_split(play_text, "run for") %>% map_chr(., 1),
      rusher_name = ifelse(
        is.na(rusher_name),
        str_split(play_text, "rush") %>% map_chr(., 1),
        rusher_name
      ),
      rusher_name = ifelse(
        str_detect(play_text, "rush") |
          str_detect(play_text, "run for"),
        rusher_name,
        NA
      )
    )

  ## Passes - WR
  completed_pass = str_detect(pbp_df$play_text, "pass complete to")
  incomplete_pass = str_detect(pbp_df$play_text, "pass incomplete to")
  pbp_df$receiver_name[completed_pass] = str_split(pbp_df$play_text[completed_pass], "pass complete to") %>%
    map_chr(., 2) %>% str_split(., "for") %>% map_chr(., 1)
  pbp_df$receiver_name[incomplete_pass] = str_split(pbp_df$play_text[incomplete_pass], "pass incomplete to") %>%   map_chr(., 2)

  ## Defensive plays
  fumble = str_detect(pbp_df$play_text, 'forced by')
  pbp_df$force_fumble_player  <-
    str_split(pbp_df$play_text[fumble], 'forced by') %>% map_chr(., 2) %>% str_split(., ",") %>% map_chr(., 1)

  int_td = str_detect(pbp_df$play_text, 'pass intercepted for a TD')
  int = str_detect(pbp_df$play_text, 'pass intercepted') & (!int_td)
}

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
