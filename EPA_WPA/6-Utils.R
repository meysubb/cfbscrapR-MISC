team_abbrs_df <- read_csv('https://raw.githubusercontent.com/903124/CFB_EPA_data/master/cfb_teams_list.csv')
team_abbrs_df$abbreviation <- team_abbrs_df$abbreviation 
write.csv(team_abbrs_df,"team_abrs.csv",row.names = F)

team_abbrs_list  = paste(team_abbrs_df$abbreviation, collapse="|") 

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
  
  off_TD = c('Passing Touchdown','Rushing Touchdown')
  def_TD = c('Interception Return Touchdown','Fumble Return Touchdown')
  
  dat = dat %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
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
    ) %>%  
    left_join(team_abbrs_df,by=c("offense"="full_name")) %>% 
    left_join(team_abbrs_df,by=c("defense"="full_name"),suffix=c("_offense","_defense"))
  
  # these mess up when you have to regex the yardline, so remove em
  dat$play_text = gsub("1st down","temp",dat$play_text)
  dat$play_text = gsub("2nd down","temp",dat$play_text)
  dat$play_text = gsub("3rd down","temp",dat$play_text)
  dat$play_text = gsub("4th down","temp",dat$play_text)
  
  
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
  
  opp_fumb_rec = dat$play_type=="Fumble Recovery (Opponent)"
  dat[opp_fumb_rec, "new_down"] = 1
  dat[opp_fumb_rec, "new_distance"] = 10
  dat[opp_fumb_rec, "new_yardline"] = 100 - (dat[opp_fumb_rec, "yard_line"] +
                                               dat[opp_fumb_rec, "yards_gained"])
  sack = str_detect(dat$play_type, "Sack")
  if(any(sack)){
    dat[sack, "new_yardline"] = (dat[sack, "adj_yd_line"] - dat[sack, "yards_gained"])
    dat[sack, "new_down"] = dat[sack, "down"] + 1
    dat[sack, "new_distance"] = dat[sack, "distance"] - dat[sack, "yards_gained"]
    
    ## sack and fumble, this seems to be weirdly tricky
    sack_fumble = sack & (str_detect(dat$play_text,"fumbled"))
    q = str_extract_all(dat$play_text[sack_fumble],"\\d+")
    q = unlist(sapply(q,function(x){
      ind = length(x)
      val = as.numeric(x[ind])
      if(val==0){
        val = as.numeric(x[ind-1])
      }
      if(length(val)==0){
        return(NA)
      }
      return(val)
    },simplify = T))
    # now identify, if you need to subtract 100?
    
    receovered_string = str_extract(dat$play_text[sack_fumble], '(?<=, recovered)[^,]+(?=,)')
    temp_team = str_extract_all(receovered_string, team_abbrs_list)
    team_team = unlist(sapply(temp_team,function(x){
      ind = length(x)
      val = x[ind]
      if(length(val)==0){
        return(NA)
      }
      return(val)
    }))
    dat[sack_fumble,"coef"] = team_team
    dat[sack_fumble,"new_yardline"] = abs(((1-!(dat[sack_fumble,"coef"] == dat[sack_fumble,"abbreviation_defense"])) * 100) - q)
  }
  
  
  safety = dat$play_type == 'Safety'
  if(any(safety)){
    dat[safety,"new_yardline"] = 99 
  }
  
  
  off_td_ind = dat$play_type %in% off_TD
  if(any(off_td_ind)){
    dat[off_td_ind,"new_down"] = 1
    dat[off_td_ind,"new_distance"] = 10
  }
  
  #Fake yardline for Offensive tocuhdown play
  temp_inds = (off_td_ind | (dat$play_type %in% def_TD))
  if(any(temp_inds)){
    dat[temp_inds,"new_yardline"] = 99
  }
  
  
  # Turnovers on Down 
  tod_ind  = (!off_td_ind) & (dat$new_down>4)
  if(any(tod_ind)){
    dat[tod_ind,"turnover"] = 1
    dat[tod_ind,"new_down"] = 1
    dat[tod_ind,"new_distance"] = 10
    dat[tod_ind,"new_yardline"] = 100-dat[tod_ind,"new_yardline"]
  }
  
  
  # missed field goal, what happens
  miss_fg <- "Field Goal Missed"
  miss_fg_ind = dat$play_type == miss_fg
  if(any(miss_fg_ind)){
    dat[miss_fg_ind,"new_down"] = 1 
    dat[miss_fg_ind,"new_distance"] = 10
    # if FG is within 20, team gets it at the 20
    # otherwise team gets it at the LOS (add the 17 yards back)
    adj_yds = dat[miss_fg_ind,] %>% pull(adj_yd_line)
    dat[miss_fg_ind,"new_yardline"] = ifelse(adj_yds<=20,80,100 - (adj_yds-17))
  }
  
  
  # missed field goal return
  block_return <- c("Missed Field Goal Return")
  block_inds = dat$play_type %in% block_return
  if(any(block_inds)){
    dat[block_inds,"new_down"] = 1
    dat[block_inds,"new_distance"] = 10
    dat[block_inds,"new_yardline"] = (100 - dat[block_inds,"adj_yd_line"])  - dat[block_inds,"yards_gained"]
  }
  

  # interception return 
  int_ret <- c("Pass Interception Return","Blocked Field Goal")
  int_inds = dat$play_type %in% int_ret
  if(any(int_inds)){
    dat[int_inds,"new_down"] = 1
    dat[int_inds,"new_distance"] = 10 
    # extract the yardline via regex
    # this sucks but do it
    q = str_extract_all(dat$play_text[int_inds],"\\d+")
    q = unlist(sapply(q,function(x){
      ind = length(x)
      val = as.numeric(x[ind])
      if(length(val)==0){
        return(NA)
      }
      return(val)
    },simplify = T))
    # now identify, if you need to subtract 100?
  
    temp_team = str_extract_all(dat$play_text[int_inds], team_abbrs_list)
    team_team = unlist(sapply(temp_team,function(x){
      ind = length(x)
      val = x[ind]
      if(length(val)==0){
        return(NA)
      }
      return(val)
    }))
    dat[int_inds,"coef"] = team_team
    dat[int_inds,"new_yardline"] = abs(((1-!(dat[int_inds,"coef"] == dat[int_inds,"abbreviation_defense"])) * 100) - q)
    
    
  }
  
  touchback = str_detect(dat$play_text,"touchback")
  dat[touchback,"new_yardline"] = 80
  dat[touchback,"new_down"] = 1
   
  missing_inds = dat$new_distance <= 0 
  dat[missing_inds,"new_down"] = 1
  dat[missing_inds,"new_distance"] = 10
  dat[missing_inds,"new_yardline"] = dat[missing_inds,"adj_yd_line"] - dat[missing_inds,"yards_gained"]
  
  ## If 0, reset to 25
  #zero_yd_line = dat$new_yardline == 0
  #dat[zero_yd_line,"new_yardline"] = 25
  
  dat = dat %>% select(TimeSecsRem,new_down,new_distance,new_yardline,turnover) %>%
    mutate(
      new_TimeSecsRem = lead(TimeSecsRem),
      new_log_ydstogo = log(new_yardline)) %>%
    mutate_at(vars(new_TimeSecsRem), ~ replace_na(., 0)) %>%  select(-TimeSecsRem) %>%
    rename(adj_yd_line=new_yardline) 
  colnames(dat) = gsub("new_","",colnames(dat))
  
  adj_to = (dat$adj_yd_line == 0) & (turnover_ind)
  dat[adj_to,"log_ydstogo"] = log(75)
  dat[adj_to,"adj_yd_line"] = 75
  
  dat$Under_two = dat$TimeSecsRem <= 120
  
  return(dat)
}

