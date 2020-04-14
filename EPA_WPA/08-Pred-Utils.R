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
    make_fg_prob + current_probs[fg_ind, "Opp_FG"]
  current_probs2[fg_ind, "Opp_FG"] <- current_probs[fg_ind, "FG"]
  current_probs2[fg_ind, "TD"] <- current_probs[fg_ind, "Opp_TD"]
  current_probs2[fg_ind, "Opp_TD"] <- current_probs[fg_ind, "TD"]
  current_probs2[fg_ind, "Safety"] <-
    current_probs[fg_ind, "Opp_Safety"]
  current_probs2[fg_ind, "Opp_Safety"] <-
    current_probs[fg_ind, "Safety"]
  return(current_probs2)
}

prep_df_epa2 <- function(dat) {
  turnover_play_type = c(
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception',
    'Punt'
  )
  
  
  dat = dat %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      new_id = gsub(pattern = unique(game_id), "", x = id_play),
      new_id = as.numeric(new_id),
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      half = ifelse(period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0,
      #log_ydstogo = 0
    )
  
  turnover_ind = dat$play_type %in% turnover_play_type
  dat$turnover = 0
  
  new_offense = !(dat$offense_play == lead(dat$offense_play))
  #fourth_down = dat$down == 4,  & fourth_down
  t_ind = turnover_ind | (new_offense)
  
  dat$turnover[t_ind] <- 1
  
  
  defense_score_vec = c(
    "Blocked Punt Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception Return Touchdown",
    "Safety",
    "Missed Field Goal Return Touchdown",
    "Punt Return Touchdown"
  )
  turnover_vec = c(
    "Blocked Field Goal",
    "Blocked Punt",
    "Field Goal Missed",
    "Fumble Recovery (Opponent)",
    "Missed Field Goal Return",
    "Pass Interception Return",
    "Punt"
  )
  normalplay = c("Rush",
                 "Pass Reception",
                 "Pass Incompletion",
                 "Sack",
                 "Fumble Recovery (Own)")
  score = c("Passing Touchdown", "Rushing Touchdown", "Field Goal Good")
  kickoff = c("Kickoff",
              "Kickoff Return (Offense)",
              "Kickoff Return Touchdown")
  
  dat = dat %>% group_by(game_id, half) %>%
    dplyr::arrange(new_id, .by_group = TRUE) %>%
    mutate(
      turnover_indicator = ifelse(
        play_type %in% defense_score_vec | play_type %in% turnover_vec |
          play_type %in% normalplay &
          yards_gained < distance & down == 4,
        1,
        0
      ),
      down = as.numeric(down),
      down = ifelse(play_type %in% "Kickoff", 5, down),
      new_down = case_when(
        play_type %in% score ~ 1,
        play_type %in% kickoff ~ 1,
        play_type %in% turnover_vec ~ 1,
        play_type %in% defense_score_vec ~ 1,
        play_type %in% normalplay & yards_gained >= distance ~ 1,
        play_type %in% normalplay & yards_gained < distance & down <= 3 ~ down + 1,
        play_type %in% normalplay & yards_gained < distance & down == 4 ~ 1
      ),
      
      yards_gained = as.numeric(yards_gained),
      start_yards_to_goal = as.numeric(start_yards_to_goal),
      new_distance = case_when(
        play_type %in% normalplay &
          yards_gained >= distance &
          (yards_to_goal - yards_gained > 10) ~ 10,
        play_type %in% normalplay &
          yards_gained >= distance &
          (yards_to_goal  - yards_gained < 10) ~ yards_to_goal,
        play_type %in% normalplay &
          yards_gained < distance & down <= 3 ~ distance - yards_gained,
        play_type %in% normalplay &
          yards_gained < distance &
          down == 4 & (100 - (yards_to_goal  - yards_gained) > 10) ~ 10,
        play_type %in% normalplay &
          yards_gained < distance &
          down == 4 &
          (100 - (yards_to_goal  - yards_gained) < 10) ~ 100 - yards_to_goal,
        play_type %in% turnover_vec &
          (100 - (yards_to_goal + yards_gained) > 10) ~ 10,
        play_type %in% turnover_vec &
          (100 - (yards_to_goal + yards_gained) < 10) ~ 100 - (yards_to_goal  + yards_gained),
        play_type %in% defense_score_vec ~ 0,
        play_type %in% score ~ 0,
        play_type %in% kickoff ~ 10
      ),
      
      new_yardline = case_when(
        play_type %in% normalplay ~ yards_to_goal - yards_gained,
        play_type %in% score ~ 0,
        play_type %in% defense_score_vec ~ 0,
        play_type %in% kickoff ~ start_yards_to_goal,
        play_type %in% turnover_vec ~ 100 - yards_to_goal + yards_gained
      ),
      
      new_TimeSecsRem = lead(TimeSecsRem),
      new_log_ydstogo = log(new_distance),
      new_Goal_To_Go = ifelse(new_yardline <= new_distance, 1, 0),
      # new under two minute warnings
      new_Under_two = new_TimeSecsRem <= 120,
      end_half_game = 0
    ) %>%
    mutate_at(vars(new_TimeSecsRem), ~ replace_na(., 0)) %>% ungroup()
  
  punt_plays = dat$play_type == "Punt"
  touchback_punt = (stringr::str_detect(dat$play_text,"touchback")) & (punt_plays)
  yds_gained_more_0 = (dat$yards_gained > 0 ) & punt_plays
  dat[punt_plays,"new_down"] = 1
  dat[punt_plays,"new_distance"] = 10
  dat[punt_plays,"new_log_ydstogo"] = log(10)
  dat[punt_plays,"new_Goal_To_Go"] = 0
  dat[touchback_punt,"new_yardline"] = 80
  dat[yds_gained_more_0,"new_yardline"] = 100 - (with(dat[yds_gained_more_0,],yards_to_goal-yards_gained))
  punt_ind = (dat$yards_gained == 0) & punt_plays & !touchback_punt
  if(any(punt_ind)){
    punt_play = dat[punt_ind,] %>% pull(play_text)
    double_try = stringi::stri_extract_last_regex(punt_play,'(?<=the )[^,]+')
    q = as.numeric(stringi::stri_extract_last_regex(double_try,"\\d+"))
    # ball always chances hands 
    dat[punt_ind,"new_yardline"] = 100 - q
    
  }
  
  
  
  end_of_half_plays = is.na(dat$new_yardline) &
    (dat$new_TimeSecsRem == 0)
  if (any(end_of_half_plays)) {
    dat$new_yardline[end_of_half_plays] <- 99
    dat$new_down[end_of_half_plays] <- 4
    dat$new_distance[end_of_half_plays] <- 99
    dat$end_half_game[end_of_half_plays] <- 1
    dat$new_log_ydstogo[end_of_half_plays] <- log(99)
    dat$new_Under_two[end_of_half_plays] <-
      dat$new_TimeSecsRem[end_of_half_plays] <= 120
  }
  
  missing_yd_line = dat$new_yardline == 0
  dat$new_yardline[missing_yd_line] = 99
  dat$new_log_ydstogo[missing_yd_line] = log(99)
  
  dat = dat %>% select(
    id_play,
    new_id,
    game_id,
    drive_id,
    new_TimeSecsRem,
    new_down,
    new_distance,
    new_yardline,
    new_log_ydstogo,
    new_Goal_To_Go,
    new_Under_two,
    end_half_game,
    turnover
  ) %>% arrange(new_id)
  colnames(dat) = gsub("new_", "", colnames(dat))
  colnames(dat)[8] <- "yards_to_goal"
  colnames(dat)[2] <- "new_id"
  
  return(dat)
}

