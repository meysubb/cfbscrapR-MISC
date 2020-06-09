team_abbrs_df <- read_csv('https://raw.githubusercontent.com/903124/CFB_EPA_data/master/cfb_teams_list.csv')
team_abbrs_df$full_name <- team_abbrs_df$full_name
team_abbrs_df$abbreviation <- team_abbrs_df$abbreviation
write.csv(team_abbrs_df,"team_abrs.csv",row.names = F)
library(snakecase)
team_abbrs_list  = paste(team_abbrs_df$abbreviation, collapse="|")

find_game_next_score_half <- function(drive_df){
  drive_df$drive_id <- as.numeric(drive_df$drive_id)
  drive_df = drive_df %>% arrange(drive_id)
  score_plays <- which(drive_df$scoring == TRUE & !str_detect(drive_df$drive_result,"END OF"))

  final_df = lapply(1:nrow(drive_df),find_next_score,
                    score_plays_i = score_plays,dat_drive=drive_df) %>% bind_rows()

  final_df2 = cbind(drive_df,final_df)
  return(final_df2)
}

find_next_score <- function(play_i,score_plays_i,dat_drive){
  defense_tds <- c("PUNT RETURN TD","FUMBLE TD")
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

    ## If the defense scores
    ## we need to make sure the next_score_team is correct
    next_score_team <- dat_drive$offense[next_score_i]
    if(dat_drive$drive_result[next_score_i] %in% defense_tds){
      next_score_team <- dat_drive$defense[next_score_i]
    }

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

add_penalty_cols <- function(raw_df){
  #-- penalty in play text----
  pen_text = str_detect(raw_df$play_text, "Penalty|penalty|PENALTY")
  #-- declined in play text----
  pen_declined_text = str_detect(raw_df$play_text,"declined|Declined|DECLINED")
  #--no play in play text----
  pen_no_play_text = str_detect(raw_df$play_text,"no play|No Play|NO PLAY")
  #--off-setting in play text----
  pen_offset_text = str_detect(raw_df$play_text,"off-setting")|
    str_detect(raw_df$play_text,"Off-Setting")|
    str_detect(raw_df$play_text,"OFF-SETTING")
  #--off-setting in play text----
  pen_1st_down_text = str_detect(raw_df$play_text,"1st down")|
    str_detect(raw_df$play_text,"1st Down")|
    str_detect(raw_df$play_text,"1st DOWN")|
    str_detect(raw_df$play_text,"1ST Down")|
    str_detect(raw_df$play_text,"1ST down")|
    str_detect(raw_df$play_text,"1ST DOWN")
  
  #-- penalty play_types
  pen_type = raw_df$play_type == "Penalty"|raw_df$play_type == "penalty"
  
  #-- penalty_flag T/F flag conditions
  raw_df$penalty_flag = F
  raw_df$penalty_flag[pen_type] <- T
  raw_df$penalty_flag[pen_text] <- T
  
  #-- penalty_declined T/F flag conditions
  raw_df$penalty_declined = F
  raw_df$penalty_declined[pen_text & pen_declined_text] <- T
  raw_df$penalty_declined[pen_type & pen_declined_text] <- T
  
  #-- penalty_no_play T/F flag conditions
  raw_df$penalty_no_play = F
  raw_df$penalty_no_play[pen_text & pen_no_play_text] <- T
  raw_df$penalty_no_play[pen_type & pen_no_play_text] <- T
  
  #-- penalty_offset T/F flag conditions
  raw_df$penalty_offset = F
  raw_df$penalty_offset[pen_text & pen_offset_text] <- T
  raw_df$penalty_offset[pen_type & pen_offset_text] <- T
  #-- penalty_1st_conv T/F flag conditions
  raw_df$penalty_1st_conv = F
  raw_df$penalty_1st_conv[pen_text & pen_1st_down_text] <- T
  raw_df$penalty_1st_conv[pen_type & pen_1st_down_text] <- T
  
  
  return(raw_df)
}

add_play_type_cols <-function(play_df){
  ##--Play type vectors------
  punt = c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  field_goal = c(
    "Field Goal Good",
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown"
  )
  play_df = play_df %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      yards_gained = as.numeric(yards_gained),
      clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
      raw_secs = clock.minutes * 60 + clock.seconds,
      TimeSecsRem = raw_secs,
      half = ifelse(period <= 2, 1, 2),
      new_yardline = 0,
      new_down = 0,
      new_distance = 0,
      new_offense = 0,
      turnover = 0,
      turnover_play_check = 0,
      downs_turnover = 0,
      offense_scoring_play = 0,
      defense_scoring_play = 0,
      end_of_half_play = 0 
    ) %>% group_by(game_id, half) %>%
    dplyr::arrange(id_play, .by_group = TRUE) %>%
    ungroup()
  
  ## add play counts for passes, rushes, punts, kicks, kickoffs, 
  ## for home offense/defense/special teams
  play_df <- play_df %>% 
    mutate(
      home_offense = ifelse(offense_play == home,TRUE,FALSE),
      pass = if_else(
        play_type == "Pass Reception" |
          play_type == "Pass Completion" |
          play_type == "Passing Touchdown" |
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
          play_type == "Rushing Touchdown" |
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
      fg_play = ifelse(play_type %in% field_goal,1,0),
      punt_play = ifelse(play_type %in% punt,1,0),
      kickoff_play = ifelse(play_type %in% kickoff,1,0))
  return(play_df)
}

add_play_type_counts <- function(play_df){
  ##--Play type vectors------
  punt = c(
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  field_goal = c(
    "Field Goal Good",
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown"
  )
  ## add play counts for passes, rushes, punts, kicks, kickoffs, 
  ## for home offense/defense/special teams
  play_df <- play_df %>% 
    mutate(
      home_offense_p = ifelse(home_offense,1,0),
      home_offense_pass_p = ifelse(home_offense & pass == 1,1,0),
      home_offense_rush_p = ifelse(home_offense & rush == 1,1,0),
      away_offense_p = ifelse(!home_offense,1,0),
      away_offense_pass_p = ifelse(!home_offense & pass == 1,1,0),
      away_offense_rush_p = ifelse(!home_offense & rush == 1,1,0),
      home_defense_p = away_offense_p,
      home_defense_pass_p = away_offense_pass_p,
      home_defense_rush_p = away_offense_rush_p,
      away_defense_p = home_offense_p,
      away_defense_pass_p = home_offense_pass_p,
      away_defense_rush_p = home_offense_rush_p,
      home_offense_1st_p = ifelse(home_offense & down == 1 & !(play_type %in% kickoff),1,0),
      home_offense_1st_pass_p = ifelse(home_offense & down == 1 & !(play_type %in% kickoff) & pass,1,0),
      home_offense_1st_rush_p = ifelse(home_offense & down == 1 & !(play_type %in% kickoff) & rush,1,0),
      home_offense_2nd_p = ifelse(home_offense & down == 2 & !(play_type %in% kickoff),1,0),
      home_offense_2nd_pass_p = ifelse(home_offense & down == 2 & !(play_type %in% kickoff) & pass,1,0),
      home_offense_2nd_rush_p = ifelse(home_offense & down == 2 & !(play_type %in% kickoff) & rush,1,0),
      home_offense_3rd_p = ifelse(home_offense & down == 3 & !(play_type %in% kickoff),1,0),
      home_offense_3rd_pass_p = ifelse(home_offense & down == 3 & !(play_type %in% kickoff) & pass,1,0),
      home_offense_3rd_rush_p = ifelse(home_offense & down == 3 & !(play_type %in% kickoff) & rush,1,0),
      home_offense_4th_p = ifelse(home_offense & down == 4 & !(play_type %in% kickoff),1,0),
      home_offense_4th_pass_p = ifelse(home_offense & down == 4 & !(play_type %in% kickoff) & pass,1,0),
      home_offense_4th_rush_p = ifelse(home_offense & down == 4 & !(play_type %in% kickoff) & rush,1,0),
      away_offense_1st_p = ifelse(!home_offense & down == 1 & !(play_type %in% kickoff),1,0),
      away_offense_1st_pass_p = ifelse(!home_offense & down == 1 & !(play_type %in% kickoff) & pass,1,0),
      away_offense_1st_rush_p = ifelse(!home_offense & down == 1 & !(play_type %in% kickoff) & rush,1,0),
      away_offense_2nd_p = ifelse(!home_offense & down == 2 & !(play_type %in% kickoff),1,0),
      away_offense_2nd_pass_p = ifelse(!home_offense & down == 2 & !(play_type %in% kickoff) & pass,1,0),
      away_offense_2nd_rush_p = ifelse(!home_offense & down == 2 & !(play_type %in% kickoff) & rush,1,0),
      away_offense_3rd_p = ifelse(!home_offense & down == 3 & !(play_type %in% kickoff),1,0),
      away_offense_3rd_pass_p = ifelse(!home_offense & down == 3 & !(play_type %in% kickoff) & pass,1,0),
      away_offense_3rd_rush_p = ifelse(!home_offense & down == 3 & !(play_type %in% kickoff) & rush,1,0),
      away_offense_4th_p = ifelse(!home_offense & down == 4 & !(play_type %in% kickoff),1,0),
      away_offense_4th_pass_p = ifelse(!home_offense & down == 4 & !(play_type %in% kickoff) & pass,1,0),
      away_offense_4th_rush_p = ifelse(!home_offense & down == 4 & !(play_type %in% kickoff) & rush,1,0),
      home_defense_1st_p = away_offense_1st_p,
      home_defense_1st_pass_p = away_offense_1st_pass_p,
      home_defense_1st_rush_p = away_offense_1st_rush_p,
      home_defense_2nd_p = away_offense_2nd_p,
      home_defense_2nd_pass_p = away_offense_2nd_pass_p,
      home_defense_2nd_rush_p = away_offense_2nd_rush_p,
      home_defense_3rd_p = away_offense_3rd_p,
      home_defense_3rd_pass_p = away_offense_3rd_pass_p,
      home_defense_3rd_rush_p = away_offense_3rd_rush_p,
      home_defense_4th_p = away_offense_4th_p,
      home_defense_4th_pass_p = away_offense_4th_pass_p,
      home_defense_4th_rush_p = away_offense_4th_rush_p,
      away_defense_1st_p = home_offense_1st_p,
      away_defense_1st_pass_p = home_offense_1st_pass_p,
      away_defense_1st_rush_p = home_offense_1st_rush_p,
      away_defense_2nd_p = home_offense_2nd_p,
      away_defense_2nd_pass_p = home_offense_2nd_pass_p,
      away_defense_2nd_rush_p = home_offense_2nd_rush_p,
      away_defense_3rd_p = home_offense_3rd_p,
      away_defense_3rd_pass_p = home_offense_3rd_pass_p,
      away_defense_3rd_rush_p = home_offense_3rd_rush_p,
      away_defense_4th_p = home_offense_4th_p,
      away_defense_4th_pass_p = home_offense_4th_pass_p,
      away_defense_4th_rush_p = home_offense_4th_rush_p,
      home_SP_punt_off_p = ifelse(home_offense & (play_type %in% punt),1,0),
      away_SP_punt_off_p = ifelse(!home_offense & (play_type %in% punt),1,0),
      home_SP_fg_off_p = ifelse(home_offense & (play_type %in% field_goal),1,0),
      away_SP_fg_off_p = ifelse(!home_offense & (play_type %in% field_goal),1,0), 
      home_SP_ko_off_p = ifelse(home_offense & (play_type %in% kickoff),1,0),
      away_SP_ko_off_p = ifelse(!home_offense & (play_type %in% kickoff),1,0),
      home_SP_punt_def_p = away_SP_punt_off_p,
      away_SP_punt_def_p = home_SP_punt_off_p,
      home_SP_fg_def_p = away_SP_fg_off_p,
      away_SP_fg_def_p = home_SP_fg_off_p, 
      home_SP_ko_def_p = away_SP_ko_off_p,
      away_SP_ko_def_p = away_SP_ko_off_p 
    ) %>% group_by(game_id) %>%
    mutate(
      home_offense_plays = cumsum(home_offense_p),
      home_offense_pass_plays = cumsum(home_offense_pass_p),
      home_offense_rush_plays = cumsum(home_offense_rush_p),
      home_offense_1st_plays = cumsum(home_offense_1st_p),
      home_offense_1st_pass_plays = cumsum(home_offense_1st_pass_p),
      home_offense_1st_rush_plays = cumsum(home_offense_1st_rush_p),
      home_offense_2nd_plays = cumsum(home_offense_2nd_p),
      home_offense_2nd_pass_plays = cumsum(home_offense_2nd_pass_p),
      home_offense_2nd_rush_plays = cumsum(home_offense_2nd_rush_p),
      home_offense_3rd_plays = cumsum(home_offense_3rd_p),
      home_offense_3rd_pass_plays = cumsum(home_offense_3rd_pass_p),
      home_offense_3rd_rush_plays = cumsum(home_offense_3rd_rush_p),
      home_offense_4th_plays = cumsum(home_offense_4th_p),
      home_offense_4th_pass_plays = cumsum(home_offense_4th_pass_p),
      home_offense_4th_rush_plays = cumsum(home_offense_4th_rush_p),
      away_offense_plays = cumsum(away_offense_p),
      away_offense_pass_plays = cumsum(away_offense_pass_p),
      away_offense_rush_plays = cumsum(away_offense_rush_p),
      away_offense_1st_plays = cumsum(away_offense_1st_p),
      away_offense_1st_pass_plays = cumsum(away_offense_1st_pass_p),
      away_offense_1st_rush_plays = cumsum(away_offense_1st_rush_p),
      away_offense_2nd_plays = cumsum(away_offense_2nd_p),
      away_offense_2nd_pass_plays = cumsum(away_offense_2nd_pass_p),
      away_offense_2nd_rush_plays = cumsum(away_offense_2nd_rush_p),
      away_offense_3rd_plays = cumsum(away_offense_3rd_p),
      away_offense_3rd_pass_plays = cumsum(away_offense_3rd_pass_p),
      away_offense_3rd_rush_plays = cumsum(away_offense_3rd_rush_p),
      away_offense_4th_plays = cumsum(away_offense_4th_p),
      away_offense_4th_pass_plays = cumsum(away_offense_4th_pass_p),
      away_offense_4th_rush_plays = cumsum(away_offense_4th_rush_p),
      home_defense_plays = cumsum(home_defense_p),
      home_defense_pass_plays = cumsum(home_defense_pass_p),
      home_defense_rush_plays = cumsum(home_defense_rush_p),
      home_defense_1st_plays = cumsum(home_defense_1st_p),
      home_defense_1st_pass_plays = cumsum(home_defense_1st_pass_p),
      home_defense_1st_rush_plays = cumsum(home_defense_1st_rush_p),
      home_defense_2nd_plays = cumsum(home_defense_2nd_p),
      home_defense_2nd_pass_plays = cumsum(home_defense_2nd_pass_p),
      home_defense_2nd_rush_plays = cumsum(home_defense_2nd_rush_p),
      home_defense_3rd_plays = cumsum(home_defense_3rd_p),
      home_defense_3rd_pass_plays = cumsum(home_defense_3rd_pass_p),
      home_defense_3rd_rush_plays = cumsum(home_defense_3rd_rush_p),
      home_defense_4th_plays = cumsum(home_defense_4th_p),
      home_defense_4th_pass_plays = cumsum(home_defense_4th_pass_p),
      home_defense_4th_rush_plays = cumsum(home_defense_4th_rush_p),
      away_defense_plays = cumsum(away_defense_p),
      away_defense_pass_plays = cumsum(away_defense_pass_p),
      away_defense_rush_plays = cumsum(away_defense_rush_p),
      away_defense_1st_plays = cumsum(away_defense_1st_p),
      away_defense_1st_pass_plays = cumsum(away_defense_1st_pass_p),
      away_defense_1st_rush_plays = cumsum(away_defense_1st_rush_p),
      away_defense_2nd_plays = cumsum(away_defense_2nd_p),
      away_defense_2nd_pass_plays = cumsum(away_defense_2nd_pass_p),
      away_defense_2nd_rush_plays = cumsum(away_defense_2nd_rush_p),
      away_defense_3rd_plays = cumsum(away_defense_3rd_p),
      away_defense_3rd_pass_plays = cumsum(away_defense_3rd_pass_p),
      away_defense_3rd_rush_plays = cumsum(away_defense_3rd_rush_p),
      away_defense_4th_plays = cumsum(away_defense_4th_p),
      away_defense_4th_pass_plays = cumsum(away_defense_4th_pass_p),
      away_defense_4th_rush_plays = cumsum(away_defense_4th_rush_p),
      home_SP_punt_off_plays = cumsum(home_SP_punt_off_p),
      away_SP_punt_off_plays = cumsum(away_SP_punt_off_p),
      home_SP_fg_off_p = cumsum(home_SP_fg_off_p),
      away_SP_fg_off_p = cumsum(away_SP_fg_off_p), 
      home_SP_ko_off_p = cumsum(home_SP_ko_off_p),
      away_SP_ko_off_p = cumsum(away_SP_ko_off_p),
      home_SP_punt_def_plays = cumsum(home_SP_punt_def_p),
      away_SP_punt_def_plays = cumsum(away_SP_punt_def_p),
      home_SP_fg_def_p = cumsum(home_SP_fg_def_p),
      away_SP_fg_def_p = cumsum(away_SP_fg_def_p), 
      home_SP_ko_def_p = cumsum(home_SP_ko_def_p),
      away_SP_ko_def_p = cumsum(away_SP_ko_def_p)    
    ) %>% group_by(game_id, drive_id) %>%
    mutate(
      home_offense_drive_plays = cumsum(home_offense_p),
      home_offense_drive_pass_plays = cumsum(home_offense_pass_p),
      home_offense_drive_rush_plays = cumsum(home_offense_rush_p),
      home_offense_drive_1st_plays = cumsum(home_offense_1st_p),
      home_offense_drive_1st_pass_plays = cumsum(home_offense_1st_pass_p),
      home_offense_drive_1st_rush_plays = cumsum(home_offense_1st_rush_p),
      home_offense_drive_2nd_plays = cumsum(home_offense_2nd_p),
      home_offense_drive_2nd_pass_plays = cumsum(home_offense_2nd_pass_p),
      home_offense_drive_2nd_rush_plays = cumsum(home_offense_2nd_rush_p),
      home_offense_drive_3rd_plays = cumsum(home_offense_3rd_p),
      home_offense_drive_3rd_pass_plays = cumsum(home_offense_3rd_pass_p),
      home_offense_drive_3rd_rush_plays = cumsum(home_offense_3rd_rush_p),
      home_offense_drive_4th_plays = cumsum(home_offense_4th_p),
      home_offense_drive_4th_pass_plays = cumsum(home_offense_4th_pass_p),
      home_offense_drive_4th_rush_plays = cumsum(home_offense_4th_rush_p),
      away_offense_drive_plays = cumsum(away_offense_p),
      away_offense_drive_pass_plays = cumsum(away_offense_pass_p),
      away_offense_drive_rush_plays = cumsum(away_offense_rush_p),
      away_offense_drive_1st_plays = cumsum(away_offense_1st_p),
      away_offense_drive_1st_pass_plays = cumsum(away_offense_1st_pass_p),
      away_offense_drive_1st_rush_plays = cumsum(away_offense_1st_rush_p),
      away_offense_drive_2nd_plays = cumsum(away_offense_2nd_p),
      away_offense_drive_2nd_pass_plays = cumsum(away_offense_2nd_pass_p),
      away_offense_drive_2nd_rush_plays = cumsum(away_offense_2nd_rush_p),
      away_offense_drive_3rd_plays = cumsum(away_offense_3rd_p),
      away_offense_drive_3rd_pass_plays = cumsum(away_offense_3rd_pass_p),
      away_offense_drive_3rd_rush_plays = cumsum(away_offense_3rd_rush_p),
      away_offense_drive_4th_plays = cumsum(away_offense_4th_p),
      away_offense_drive_4th_pass_plays = cumsum(away_offense_4th_pass_p),
      away_offense_drive_4th_rush_plays = cumsum(away_offense_4th_rush_p),
      home_defense_drive_plays = cumsum(home_defense_p),
      home_defense_drive_pass_plays = cumsum(home_defense_pass_p),
      home_defense_drive_rush_plays = cumsum(home_defense_rush_p),
      home_defense_drive_1st_plays = cumsum(home_defense_1st_p),
      home_defense_drive_1st_pass_plays = cumsum(home_defense_1st_pass_p),
      home_defense_drive_1st_rush_plays = cumsum(home_defense_1st_rush_p),
      home_defense_drive_2nd_plays = cumsum(home_defense_2nd_p),
      home_defense_drive_2nd_pass_plays = cumsum(home_defense_2nd_pass_p),
      home_defense_drive_2nd_rush_plays = cumsum(home_defense_2nd_rush_p),
      home_defense_drive_3rd_plays = cumsum(home_defense_3rd_p),
      home_defense_drive_3rd_pass_plays = cumsum(home_defense_3rd_pass_p),
      home_defense_drive_3rd_rush_plays = cumsum(home_defense_3rd_rush_p),
      home_defense_drive_4th_plays = cumsum(home_defense_4th_p),
      home_defense_drive_4th_pass_plays = cumsum(home_defense_4th_pass_p),
      home_defense_drive_4th_rush_plays = cumsum(home_defense_4th_rush_p),
      away_defense_drive_plays = cumsum(away_defense_p),
      away_defense_drive_pass_plays = cumsum(away_defense_pass_p),
      away_defense_drive_rush_plays = cumsum(away_defense_rush_p),
      away_defense_drive_1st_plays = cumsum(away_defense_1st_p),
      away_defense_drive_1st_pass_plays = cumsum(away_defense_1st_pass_p),
      away_defense_drive_1st_rush_plays = cumsum(away_defense_1st_rush_p),
      away_defense_drive_2nd_plays = cumsum(away_defense_2nd_p),
      away_defense_drive_2nd_pass_plays = cumsum(away_defense_2nd_pass_p),
      away_defense_drive_2nd_rush_plays = cumsum(away_defense_2nd_rush_p),
      away_defense_drive_3rd_plays = cumsum(away_defense_3rd_p),
      away_defense_drive_3rd_pass_plays = cumsum(away_defense_3rd_pass_p),
      away_defense_drive_3rd_rush_plays = cumsum(away_defense_3rd_rush_p),
      away_defense_drive_4th_plays = cumsum(away_defense_4th_p),
      away_defense_drive_4th_pass_plays = cumsum(away_defense_4th_pass_p),
      away_defense_drive_4th_rush_plays = cumsum(away_defense_4th_rush_p)
    ) %>% ungroup() 
  
  return(play_df)
}

prep_play_end_vars <- function(dat) {
  ##--Play type vectors------
  turnover_play_type = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Field Goal Missed",
    "Missed Field Goal Return",
    "Missed Field Goal Return Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Punt",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown",
    "Uncategorized Touchdown Touchdown"
  )
  defense_score_vec = c(
    "Blocked Punt Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Punt Touchdown",
    "Punt Return Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Safety",
    "Sack Touchdown",
    "Interception Return Touchdown",
    "Pass Interception Return Touchdown",
    "Uncategorized Touchdown",
    "Uncategorized Touchdown Touchdown"
  )
  turnover_vec = c(
    "Blocked Field Goal",
    "Blocked Field Goal Touchdown",
    "Blocked Punt",
    "Blocked Punt Touchdown",
    "Fumble Recovery (Opponent)",
    "Fumble Recovery (Opponent) Touchdown",
    "Fumble Return Touchdown",
    "Defensive 2pt Conversion",
    "Interception",
    "Interception Return Touchdown",
    "Pass Interception Return",
    "Pass Interception Return Touchdown",
    "Sack Touchdown",
    "Uncategorized Touchdown",
    "Uncategorized Touchdown Touchdown"
  )
  normalplay = c(
    "Rush",
    "Pass",
    "Pass Reception",
    "Pass Incompletion",
    "Pass Completion",
    "Sack",
    "Fumble Recovery (Own)"
  )
  penalty = c('Penalty')
  offense_score_vec = c(
    "Passing Touchdown",
    "Rushing Touchdown",
    "Field Goal Good",
    "Pass Reception Touchdown",
    "Fumble Recovery (Own) Touchdown",
    "Uncategorized Touchdown",
    "Uncategorized Touchdown Touchdown"
  )
  kickoff = c(
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown",
    "Kickoff Touchdown"
  )
  
  dat = dat %>%
    mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
    mutate(
      new_yardline = 0,
      new_down = 0,
      new_distance = 0,
      new_offense = 0,
      turnover = 0,
      turnover_play_check = 0,
      downs_turnover = 0,
      offense_scoring_play = 0,
      defense_scoring_play = 0,
      end_of_half_play = 0 
    ) %>% group_by(game_id, half) %>%
    dplyr::arrange(id_play, .by_group = TRUE)
  
  # define turnover flag 
  turnover_ind = dat$play_type %in% turnover_play_type
  
  # data is ordered
  new_offense = !(dat$offense_play == lead(dat$offense_play, order_by = dat$id_play))
  offense_scoring_plays = dat$play_type %in% offense_score_vec
  defense_scoring_plays = dat$play_type %in% defense_score_vec
  
  # end of half check as well
  end_of_half_plays = is.na(lead(dat$half,order_by = dat$id_play)) | 
    !(dat$half == lead(dat$half,order_by = dat$id_play))
  
  # define turnover on downs
  downs_turnover = (dat$play_type %in% normalplay & 
                      dat$down == 4 & new_offense & 
                      !offense_scoring_plays & !end_of_half_plays)
  
  
  # is specifically defined as a turnover
  turnover_play_check = dat$play_type %in% turnover_vec
  
  # turnovers only occur on actual change of offense
  # but not scoring plays
  # and not at the end of half.
  # Turnovers now capture downs, when there is a change of offense after a fourth down normal play.
  t_ind = (turnover_ind | new_offense | turnover_play_check | downs_turnover) & !offense_scoring_plays & !end_of_half_plays
  
  dat$new_offense[new_offense] <- 1
  dat$offense_scoring_play[offense_scoring_plays] <- 1
  dat$defense_scoring_play[defense_scoring_plays] <- 1
  dat$end_of_half_play[end_of_half_plays] <- 1
  dat$downs_turnover[downs_turnover] <- 1
  dat$turnover_play_check[turnover_play_check] <- 1
  dat$turnover[t_ind] <- 1
  
  dat = dat %>% ungroup() %>% group_by(game_id, half) %>%
    dplyr::arrange(id_play, .by_group = TRUE) %>%
    mutate(
      turnover_indicator = ifelse(
        play_type %in% defense_score_vec | play_type %in% turnover_vec |
          play_type %in% normalplay &
          yards_gained < distance & down == 4,
        1,
        0
      ),
      down = as.numeric(down),
      #--New Down-----
      new_down = as.numeric(case_when(
        ##--Penalty Cases (new_down)-----
        # 8 cases with three T/F penalty flags
        # 4 cases in 1
        play_type %in% penalty & penalty_1st_conv ~ 1,
        # offsetting penalties, no penalties declined, no 1st down by penalty (1 case)
        play_type %in% penalty & !penalty_declined &
          penalty_offset & !penalty_1st_conv ~ down,
        # offsetting penalties, penalty declined true, no 1st down by penalty
        # seems like it would be a regular play at that point (1 case, split in three)
        play_type %in% penalty & penalty_declined &
          penalty_offset & !penalty_1st_conv &
          yards_gained < distance & down <= 3 ~ down+1,
        play_type %in% penalty & penalty_declined &
          penalty_offset & !penalty_1st_conv &
          yards_gained < distance & down == 4 ~ 1,
        play_type %in% penalty & penalty_declined &
          penalty_offset & !penalty_1st_conv &
          yards_gained > distance ~ 1,
        # only penalty declined true, same logic as prior (1 case, split in three)
        play_type %in% penalty & penalty_declined &
          !penalty_offset & !penalty_1st_conv &
          yards_gained < distance & down <= 3 ~ down+1,
        play_type %in% penalty & penalty_declined &
          !penalty_offset & !penalty_1st_conv &
          yards_gained < distance & down == 4 ~ 1,
        play_type %in% penalty & penalty_declined &
          !penalty_offset & !penalty_1st_conv &
          yards_gained >= distance ~ 1,
        # no other penalty flags true, lead on down (1 case)
        play_type %in% penalty & !penalty_declined &
          !penalty_offset & !penalty_1st_conv ~ lead(down, order_by=id_play),
        ##--Scores, kickoffs, turnovers, defensive scores----
        play_type %in% offense_score_vec ~ 1,
        play_type %in% kickoff ~ 1,
        play_type %in% turnover_vec ~ 1,
        play_type %in% defense_score_vec ~ 1,
        ##--Regular Plays----
        ##--regular play 1st down ----
        play_type %in% normalplay & yards_gained >= distance ~ 1,
        # iterate to next down due to not meeting the yards to gain
        play_type %in% normalplay & yards_gained < distance & down <= 3 ~ as.integer(down) + 1,
        # turnover on downs
        play_type %in% normalplay & yards_gained < distance & down == 4 ~ 1,
        TimeSecsRem == 1800 ~ 1
      )),
      yards_gained = as.numeric(ifelse(yards_gained == 11131.000, 13, yards_gained)),
      drive_start_yards_to_goal = as.numeric(drive_start_yards_to_goal),
      #--New Distance-----
      new_distance = as.numeric(case_when(
        ##--Penalty cases (new_distance)
        play_type %in% penalty & penalty_declined
        & !penalty_offset & !penalty_1st_conv ~ as.numeric(distance - yards_gained),
        play_type %in% penalty & penalty_declined
        & penalty_offset & !penalty_1st_conv ~ as.numeric(distance - yards_gained),        
        #--offsetting penalties, keep same distance
        play_type %in% penalty &
          penalty_offset ~ as.numeric(distance),
        #--penalty first down conversions, 10 or to goal
        play_type %in% penalty &
          penalty_1st_conv ~ as.numeric(ifelse(yards_to_goal  - yards_gained <= 10,
                                               as.numeric(yards_to_goal),10)),
        #--penalty without first down conversion
        play_type %in% penalty & !penalty_declined &
          !penalty_1st_conv &
          !penalty_offset ~ as.numeric(ifelse((yards_gained >= distance) &
                                                (yards_to_goal - yards_gained <= 10),
                                              as.numeric(yards_to_goal),10)),
        ##--normal plays
        play_type %in% normalplay &
          yards_gained >= distance &
          (yards_to_goal - yards_gained >= 10) ~ 10,
        play_type %in% normalplay &
          yards_gained >= distance &
          (yards_to_goal  - yards_gained <= 10) ~ as.numeric(yards_to_goal),
        play_type %in% normalplay &
          yards_gained < distance & down <= 3 ~ as.numeric(distance - yards_gained),
        play_type %in% normalplay &
          yards_gained < distance &
          down == 4 & (100 - (yards_to_goal  - yards_gained) >= 10) ~ 10,
        play_type %in% normalplay &
          yards_gained < distance &
          down == 4 &
          (100 - (yards_to_goal  - yards_gained) <= 10) ~ as.numeric(100 - yards_to_goal),
        ##--turnovers, defensive scores, scores, kickoffs
        play_type %in% turnover_vec ~ 10,
        # play_type %in% turnover_vec &
        #   (100 - (yards_to_goal + yards_gained) >= 10) ~ 10,
        # play_type %in% turnover_vec &
        #   (100 - (yards_to_goal + yards_gained) <= 10) ~ 100 - (yards_to_goal  + yards_gained),
        play_type %in% defense_score_vec ~ 0,
        play_type %in% offense_score_vec ~ 0,
        play_type %in% kickoff ~ 10      
      )),
      #--New Yardline----
      new_yardline = case_when(
        play_type %in% penalty & penalty_offset ~ as.integer(yards_to_goal),
        play_type %in% penalty & !penalty_offset ~ as.integer(yards_to_goal - yards_gained),
        play_type %in% normalplay ~ as.integer(yards_to_goal - yards_gained),
        play_type %in% offense_score_vec ~ as.integer(0),
        play_type %in% defense_score_vec ~ as.integer(0),
        play_type %in% kickoff ~ as.integer(drive_start_yards_to_goal),
        play_type %in% turnover_vec ~ as.integer(100 - yards_to_goal + yards_gained)
      ),
      new_TimeSecsRem = ifelse(!is.na(lead(TimeSecsRem,order_by=id_play)),lead(TimeSecsRem,order_by=id_play),0),
      new_log_ydstogo = ifelse(new_distance == 0|is.na(new_distance), log(1),log(new_distance)),
      new_Goal_To_Go = ifelse(new_yardline <= new_distance, TRUE, FALSE),
      # new under two minute warnings
      new_Under_two = new_TimeSecsRem <= 120,
      end_half_game = 0
    ) %>%
    mutate_at(vars(new_TimeSecsRem), ~ replace_na(., 0)) %>% ungroup()
  
  #--Punt Plays--------------------------
  punt_plays = dat$punt_play == 1
  touchback_punt = ifelse(!is.na(stringr::str_detect(dat$play_text,"touchback") & (punt_plays)),
                          stringr::str_detect(dat$play_text,"touchback") & (punt_plays),FALSE)
  dat$touchback_punt <- ifelse(touchback_punt,1,0)
  
  dat[punt_plays,"new_down"] = 1
  dat[punt_plays,"new_distance"] = 10
  dat[punt_plays,"new_log_ydstogo"] = log(10)
  dat[punt_plays,"new_Goal_To_Go"] = FALSE
  dat[touchback_punt,"new_yardline"] = 80
  punt_play = dat[punt_plays,] %>% pull(play_text)
  yds_punted = as.numeric(stringr::str_extract(
    stringi::stri_extract_first_regex(punt_play, '(?<= punt for)[^,]+'),
    "\\d+"
  ))
  
  # ball always changes hands
  punt_yd_line = dat[punt_plays,] %>% pull(yards_to_goal)
  punt_yds_gained = dat[punt_plays,] %>% pull(yards_gained)
  dat$punt_yd_line = NA
  dat$punt_yds_gained = NA
  dat$yards_punted = NA
  dat$punt_yd_line[punt_plays] = dat$yards_to_goal[punt_plays]
  dat$punt_yds_gained[punt_plays] =dat$yards_gained[punt_plays] 
  dat$new_yardline[punt_plays] = (100 - dat$punt_yd_line[punt_plays]) + dat$yards_punted[punt_plays] - dat$punt_yds_gained[punt_plays]
  dat$yards_punted[punt_plays] = yds_punted
  #--End of Half Plays--------------------------
  end_of_half_plays = (dat$new_TimeSecsRem == 0)
  if (any(end_of_half_plays)) {
    dat$new_yardline[end_of_half_plays] <- 99
    dat$new_down[end_of_half_plays] <- 4
    dat$new_distance[end_of_half_plays] <- 99
    dat$end_half_game[end_of_half_plays] <- 1
    dat$new_log_ydstogo[end_of_half_plays] <- log(99)
    dat$new_Under_two[end_of_half_plays] <-
      dat$new_TimeSecsRem[end_of_half_plays] <= 120
  }
  
  # missed field goal needs to be here
  # needs to go before the na check to set to 99
  dat = dat %>% mutate(new_yardline = if_else(
    is.na(new_yardline) &
      play_type %in% c("Field Goal Missed", "Blocked Field Goal"),
    100 - (yards_to_goal - 9),
    new_yardline
  ))
  
  #--General weird plays that don't have an easy fix----
  na_yd_line = which(is.na(dat$new_yardline) | dat$new_yardline >= 100)
  dat$new_yardline[na_yd_line] = dat$yards_to_goal[na_yd_line+1]
  
  neg_distance = which(dat$new_distance < 0)
  dat$new_distance[neg_distance] = dat$distance[neg_distance+1]
  dat$new_log_ydstogo[neg_distance] = log(dat$new_distance[neg_distance])
  
  #--Missing yd_line Plays--------------------------
  missing_yd_line = dat$new_yardline == 0
  dat$new_yardline[missing_yd_line] = 99
  dat$new_log_ydstogo[missing_yd_line] = log(99)
  
  dat = dat %>%
    mutate(
      down = as.factor(down),
      new_down = as.factor(new_down),
      new_home_offense = ifelse(turnover == 1, !home_offense, home_offense)) %>% 
    arrange(id_play) 
  # colnames(dat)[3] <- "new_id"
  dat = dat %>% 
    rename("new_yards_to_goal"="new_yardline")
  
  return(dat)
}

add_timeout_cols <- function(play_df) {
  pbp_df <- play_df %>%
    group_by(game_id, half) %>%
    arrange(desc(year),id_play) %>%
    mutate(
      timeout_called = ifelse(play_type %in% c("Timeout"),1,0),
      timeout_team = ifelse(play_type %in% c("Timeout"),
                            ifelse(!is.na(str_extract(play_text, "timeout (.+)")),
                                   str_extract(play_text, "timeout (.+)"),
                                   str_extract(play_text, "Timeout (.+)")), NA)
    ) %>%
    mutate(timeout_team = str_remove(timeout_team, ",(.+)")) %>%
    mutate(
      timeout_team = str_to_lower(str_remove(timeout_team, "Timeout ")),
      timeout_team = case_when(
        timeout_team == "af" ~ "air force",
        timeout_team == "air force academy" ~ "air force",
        timeout_team == "air force falcons" ~ "air force",
        timeout_team == "arkansas st." ~ "arkansas state",
        timeout_team == "asu" ~ "arkansas state",
        timeout_team == "ball state cardinals" ~ "ball state",
        timeout_team == "bgsu" ~ "bowling green",
        timeout_team == "brigham young" ~ "byu",
        timeout_team == "byu cougars" ~ "byu",
        timeout_team == "centrl michigan" ~ "central michigan",
        timeout_team == "cmu" ~ "central michigan",
        timeout_team == "coastl carolina" ~ "coastal carolina",
        timeout_team == "cs" ~ "colorado state",
        timeout_team == "eastrn michigan" ~ "eastern michigan",
        timeout_team == "ecu" ~ "east carolina",
        timeout_team == "emu" ~ "eastern michigan",
        timeout_team == "fau" ~ "florida atlantic",
        timeout_team == "fiu" ~ "florida international",
        timeout_team == "fla atlantic" ~ "florida atlantic",
        timeout_team == "florida intl" ~ "florida international",
        timeout_team == "floridainternational" ~ "florida international",
        timeout_team == "fresno st." ~ "fresno state",
        timeout_team == "ga southern" ~ "georgia southern",
        timeout_team == "gsu" ~ "georgia state",
        timeout_team == "hawai`i" ~ "hawai'i",
        timeout_team == "hawaii" ~ "hawai'i",
        timeout_team == "iowa hawkeyes" ~ "iowa",
        timeout_team == "las vegas" ~ "unlv",
        timeout_team == "latech" ~ "louisiana tech",
        timeout_team == "louisiana lafayette" ~ "louisiana",
        timeout_team == "louisiana state" ~ "lsu",
        timeout_team == "louisiana-lafayette" ~ "louisiana",
        timeout_team == "massachusetts" ~ "umass",
        timeout_team == "miami (fla.)" ~ "miami",
        timeout_team == "miami (ohio)" ~ "miami (oh)",
        timeout_team == "miami fl" ~ "miami",
        timeout_team == "miami florida" ~ "miami",
        timeout_team == "miami oh" ~ "miami (oh)",
        timeout_team == "miami ohio" ~ "miami (oh)",
        timeout_team == "middle tenn st" ~ "middle tennessee",
        timeout_team == "minnesota gophers" ~ "minnesota",
        timeout_team == "mississippi" ~ "ole miss",
        timeout_team == "mt" ~ "middle tennessee",
        timeout_team == "n.c. state" ~ "nc state",
        timeout_team == "NA" ~ "",
        timeout_team == "niu" ~ "northern illinois",
        timeout_team == "nm state" ~ "new mexico state",
        timeout_team == "nmsu" ~ "new mexico state",
        timeout_team == "north carolina st" ~ "nc state",
        timeout_team == "northernil" ~ "northern illinois",
        timeout_team == "ohio bobcats" ~ "ohio",
        timeout_team == "ohio university" ~ "ohio",
        timeout_team == "olddominion" ~ "old dominion",
        timeout_team == "ole ole miss" ~ "ole miss",
        timeout_team == "oregon st." ~ "oregon state",
        timeout_team == "rice owls" ~ "rice",
        timeout_team == "san jose st" ~ "san josé state",
        timeout_team == "san jose state" ~ "san josé state",
        timeout_team == "sj" ~ "san josé state",
        timeout_team == "sjsu" ~ "san josé state",
        timeout_team == "smu mustangs" ~ "smu",
        timeout_team == "southern  miss" ~ "southern mississippi",
        timeout_team == "southern cal" ~ "usc",
        timeout_team == "southern methodist university" ~ "smu",
        timeout_team == "temple owls" ~ "temple",
        timeout_team == "temple university" ~ "temple",
        timeout_team == "texas el paso" ~ "utep",
        timeout_team == "texas state university" ~ "texas state",
        timeout_team == "texassan" ~ "ut san antonio",
        timeout_team == "texas-san antonio" ~ "ut san antonio",
        timeout_team == "tls" ~ "tulsa",
        timeout_team == "troy university" ~ "troy",
        timeout_team == "tulane green wave" ~ "tulane",
        timeout_team == "uh" ~ "hawai'i",
        timeout_team == "ui" ~ "idaho",
        timeout_team == "ul" ~ "louisiana",
        timeout_team == "ul lafayette" ~ "louisiana",
        timeout_team == "ul monroe" ~ "louisiana monroe",
        timeout_team == "ull" ~ "louisiana",
        timeout_team == "ulm" ~ "louisiana monroe",
        timeout_team == "university of idaho" ~ "idaho",
        timeout_team == "usa" ~ "south alabama",
        timeout_team == "usf" ~ "south florida",
        timeout_team == "usm" ~ "southern mississippi",
        timeout_team == "usu" ~ "utah state",
        timeout_team == "utsa" ~ "ut san antonio",
        timeout_team == "washington st." ~ "washington state",
        timeout_team == "west virginia university" ~ "west virginia",
        timeout_team == "westrn kentucky" ~ "western kentucky",
        timeout_team == "westrn michigan" ~ "western michigan",
        timeout_team == "wfu" ~ "wake forest",
        timeout_team == "wku" ~ "western kentucky",
        timeout_team == "wmu" ~ "western michigan",
        timeout_team == "wsu" ~ "washington state",
        timeout_team == "wyoming cowboys" ~ "wyoming",
        TRUE~timeout_team),
      home_timeout = ifelse(is.na(timeout_team),0,
                            ifelse(str_detect(str_to_lower(home),fixed(timeout_team))==TRUE,1,0)), 
      away_timeout = ifelse(is.na(timeout_team),0,
                            ifelse(str_detect(str_to_lower(away),fixed(timeout_team))==TRUE,1,0)),
      off_timeouts_rem_before = NA,
      def_timeouts_rem_before = NA,
      off_timeouts_rem_after = NA,
      def_timeouts_rem_after = NA,
      home_timeouts_rem_before = NA,
      away_timeouts_rem_before = NA,
      home_timeouts_rem_after = NA,
      away_timeouts_rem_after = NA
    ) %>%
    mutate(
      home_timeout = 
        case_when(timeout_called ==1 & home_timeout==1 & away_timeout==1 ~
                    ifelse(is.na(timeout_team),0,
                           ifelse(str_detect(str_to_lower(home),
                                             paste0("^",timeout_team,"$"))==TRUE,1,0)),TRUE~ home_timeout),
      away_timeout = 
        case_when(timeout_called ==1 & home_timeout==1 & away_timeout==1 ~
                    ifelse(is.na(timeout_team),0,
                           ifelse(str_detect(str_to_lower(away),
                                             paste0("^",timeout_team,"$"))==TRUE,1,0)),TRUE~ away_timeout)
    ) %>%
    mutate(
      home_timeouts_rem_after = 3 - cumsum(home_timeout),
      away_timeouts_rem_after = 3 - cumsum(away_timeout),
      home_timeouts_rem_before = ifelse(!is.na(lag(home_timeouts_rem_after,order_by = id_play)),
                                        lag(home_timeouts_rem_after,order_by = id_play),3),
      away_timeouts_rem_before = ifelse(!is.na(lag(away_timeouts_rem_after,order_by = id_play)),
                                        lag(away_timeouts_rem_after,order_by = id_play),3),
      off_timeouts_rem_after = ifelse(offense_play==home,home_timeouts_rem_after,away_timeouts_rem_after),
      def_timeouts_rem_after = ifelse(defense_play==home,home_timeouts_rem_after,away_timeouts_rem_after),
      off_timeouts_rem_before = ifelse(offense_play==home,home_timeouts_rem_before,away_timeouts_rem_before),
      def_timeouts_rem_before = ifelse(defense_play==home,home_timeouts_rem_before,away_timeouts_rem_before)
    ) %>% ungroup()
  



  
  return(pbp_df)
}


#-----------------------------------    
    # str_detect(str_to_lower(pbp_full_df_double$home),paste0("^",pbp_full_df_double$timeout_team,"$"))
    
    # pbp_full_df <- pbp_full_df %>%
    #       mutate(home_timeout = 
    #                case_when(timeout_called ==1 & home_timeout==1 & away_timeout==1 ~
    #                            ifelse(is.na(timeout_team),0,
    #                              ifelse(str_detect(str_to_lower(home),
    #                                     paste0("^",timeout_team,"$"))==TRUE,1,0)),
    #                          TRUE~ home_timeout),
    #              away_timeout = 
    #                case_when(timeout_called ==1 & home_timeout==1 & away_timeout==1 ~
    #                             ifelse(is.na(timeout_team),0,
    #                               ifelse(str_detect(str_to_lower(away),
    #                                      paste0("^",timeout_team,"$"))==TRUE,1,0)),
    #                           TRUE~ away_timeout)
    #              )
    
    
    
    
    # pbp_full_df <- pbp_full_df %>%
    #               left_join(pbp_mismatched_crosswalk, by=c('timeout_team'),suffix=c("",".y")) %>%
    #               mutate(
    #                 timeout_team = ifelse(timeout_called==1&away_timeout==0&home_timeout==0,
    #                                       str_to_lower(team_name),timeout_team),
    #                 home_timeout = ifelse(is.na(timeout_team),0,ifelse(str_detect(str_to_lower(home),fixed(timeout_team))==TRUE,1,0)), 
    #                 away_timeout = ifelse(is.na(timeout_team),0,ifelse(str_detect(str_to_lower(away),fixed(timeout_team))==TRUE,1,0))
    #                 
    #               )
#-------    
