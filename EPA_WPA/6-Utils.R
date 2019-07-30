find_game_next_score_half <- function(drive_df){
  score_plays <- which(drive_df$scoring == TRUE & !str_detect(drive_df$drive_result,"END OF"))

  final_df = lapply(1:nrow(drive_df),find_next_score,
                    score_plays_i = score_plays,dat_drive=drive_df) %>%
    bind_rows()

  final_df2 = cbind(drive_df,final_df)
  return(final_df2)
}

find_next_score <- function(play_i,score_plays_i,dat_drive){
  next_score_i <- score_plays_i[which(score_plays_i >= play_i)[1]]

  if( is.na(next_score_i) |
      dat_drive$start_period[play_i] <= 2 & dat_drive$start_period[next_score_i] %in% c(3,4) |
      dat_drive$start_period[play_i] %in% c(3,4) & dat_drive$start_period[next_score_i] > 4){

    score_drive <- dat_drive$drive_id[play_i]
    next_score <- 0
    return(data.frame(NSH = next_score,
                      DSH = score_drive))
  } else{

    score_drive <- dat_drive$drive_id[next_score_i]
    # Identify current and next score teams
    # if they are the same then you are good
    # if it is different then flip the negative sign
    current_team <- dat_drive$offense[play_i]
    next_score_team <- dat_drive$offense[next_score_i]

    if(str_detect(dat_drive$drive_result[next_score_i],"RETURN TD")){
      if( identical(current_team,next_score_team)){
        next_score <- -1 * dat_drive$pts_drive[next_score_i]
      } else {
        next_score <- dat_drive$pts_drive[next_score_i]
      }
    } else{
      if( identical(current_team,next_score_team)){
        # if same then you score
        next_score <- dat_drive$pts_drive[next_score_i]
      }else{
        # if different, then other team scored
        next_score <- -1 * dat_drive$pts_drive[next_score_i]
      }
    }
    return(data.frame(NSH = next_score,
                      DSH = score_drive))
  }
}

prep_df_epa <- function(dat) {
  # This function is used to calculate the EP - after the play
  # Then EPA = EP_After - EP_Before 
  # Provide, this function so people can calcualte 
  # EPA on new games in 2019 szn
  # after which they can feed that to the WPA model
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
  dat[turnover_ind, "new_down"] = 1
  dat[turnover_ind, "new_distance"] = 10
  # First down
  first_down_ind = str_detect(dat$play_text, '1st')
  dat[first_down_ind, "new_down"] = 1
  dat[first_down_ind, "new_distance"] = 10
  dat[first_down_ind, "new_yardline"] = dat[first_down_ind,"adj_yd_line"] - dat[first_down_ind,"yards_gained"]
  # Otherwise What happened?
  dat[(!turnover_ind &
         !first_down_ind), "new_down"] = dat[(!turnover_ind &
                                                !first_down_ind), "down"] + 1
  dat[(!turnover_ind &
         !first_down_ind), "new_distance"] = dat[(!turnover_ind &
                                                    !first_down_ind), "distance"] - dat[(!turnover_ind &
                                                                                           !first_down_ind), "yards_gained"]
  dat[(!turnover_ind & !first_down_ind),"new_yardline"] =  dat[(!turnover_ind &
                                                                  !first_down_ind), "adj_yd_line"] - dat[(!turnover_ind &
                                                                                                            !first_down_ind), "yards_gained"]
  
  
  fifty_ydline = str_detect(dat$play_text, "50 yard line")
  dat[fifty_ydline, "new_yardline"] = 50
  
  opp_fumb_rec = str_detect(dat$play_text, "Fumble Recovery (Opponent)")
  dat[opp_fumb_rec, "new_yardline"] = 100 - (dat[opp_fumb_rec, "yard_line"] +
                                               dat[opp_fumb_rec, "yards_gained"])
  dat[opp_fumb_rec, "new_down"] = 1
  dat[opp_fumb_rec, "new_distance"] = 10
  
  sack = str_detect(dat$play_type, "Sack")
  dat[sack, "new_yardline"] = 100 - (dat[sack, "yard_line"] - dat[sack, "yards_gained"])
  dat[sack, "new_down"] = dat[sack, "down"] + 1
  dat[sack, "new_distance"] = dat[sack, "distance"] - dat[sack, "yards_gained"]
  return(dat)
}