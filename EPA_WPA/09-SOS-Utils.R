pbp_team_summary_stats <- function(plays) {
  # Create Garbage time filter, eliminate FCS games
  pbp <- plays 
  
  ## Cleaning the data and feature engineering
  pbp <- pbp %>%
    rename(offense = offense_play,
           defense = defense_play) %>%
    mutate(rz_play = ifelse((adj_yd_line <= 20), 1, 0), 
           so_play = ifelse((adj_yd_line <= 40 | play_type == "(Passing Touchdown) | (Rushing Touchdown"), 1, 0),
           pass = if_else(play_type == "Pass Reception" | play_type == "Passing Touchdown" |
                            play_type == "Sack" | play_type == "Pass Interception Return" |
                            play_type == "Pass Incompletion" | play_type == "Sack Touchdown" |
                            (play_type == "Safety" & str_detect(play_text, "sacked")) |
                            (play_type == "Fumble Recovery (Own)" & str_detect(play_text, "pass")) |
                            (play_type == "Fumble Recovery (Opponent)" & str_detect(play_text, "pass")), 1, 0),
           rush = ifelse(play_type == "Rush" | play_type == "Rushing Touchdown" | (play_type == "Safety" & str_detect(play_text, "run")) |
                           (play_type == "Fumble Recovery (Opponent)" & str_detect(play_text, "run")) | 
                           (play_type == "Fumble Recovery (Own)" & str_detect(play_text, "run")), 1, 0),
           std.down = ifelse(down == 1, 1,
                             ifelse(down == 2 & distance < 8, 1, 
                                    ifelse(down == 3 & distance < 5, 1,
                                           ifelse(down == 4 & distance < 5, 1, 0)))),
           pass.down = ifelse(down == 2 & distance > 8, 1, 
                              ifelse(down == 3 & distance > 5, 1, 
                                     ifelse(down == 4 & distance > 5, 1, 0))),
           rush_pass = if_else(rush == 1, "rush", 
                               if_else(pass == 1, "pass", "NA")),
           stuffed_run = ifelse((rush == 1 & yards_gained <=0), 1, 0),
           stopped_run = ifelse((rush == 1 & yards_gained <=2), 1, 0),
           opp_rate_run = ifelse((rush == 1 & yards_gained >= 4), 1, 0),
           epa_success = ifelse((rush == 1 | pass == 1) & EPA >= 0, 1, 0),
           epa_explosive = if_else((rush == 1 & EPA >= 1.7917221), 1, 
                                   if_else((pass == 1 & EPA >= 2.4486338), 1, 0)),
           epa_success_pass = ifelse((pass == 1) & EPA >= 0, 1, 0),
           epa_success_rush = ifelse((rush == 1) & EPA >= 0, 1, 0),         
           epa_explosive_pass = if_else((pass == 1 & EPA >= 2.4486338), 1, 0),
           epa_explosive_rush = if_else((rush == 1 & EPA >= 1.7917221), 1, 0),
           short_rush_attempt = ifelse(distance <= 2 & rush == 1, 1, 0),
           short_rush_success = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance, 1, 0),
           short_pass_attempt = ifelse(distance <= 2 & pass == 1, 1, 0),
           short_pass_success = ifelse(distance <= 2 & pass == 1 & yards_gained >= distance, 1, 0),
           power_rush_OL_conversion = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance, 1, 
                                             ifelse(distance <= 2 & rush == 1 & yards_gained < distance,0,NA)),
           power_rush_OL_yards = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance, distance, 
                                        ifelse(distance <= 2 & rush == 1 & yards_gained < distance, yards_gained,NA)),
           power_rush_OL_epa = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance, EPA, 
                                      ifelse(distance <= 2 & rush == 1 & yards_gained < distance, EPA,NA)),
           power_rush_OL_conversion_3rd4th = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance & down %in% c(3,4), 1, 
                                                    ifelse(distance <= 2 & rush == 1 & yards_gained < distance & down %in% c(3,4),0,NA)),
           power_rush_OL_yards_3rd4th = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance & down %in% c(3,4), distance, 
                                               ifelse(distance <= 2 & rush == 1 & yards_gained < distance & down %in% c(3,4), yards_gained,NA)),
           power_rush_OL_epa_3rd4th = ifelse(distance <= 2 & rush == 1 & yards_gained >= distance & down %in% c(3,4), EPA, 
                                             ifelse(distance <= 2 & rush == 1 & yards_gained < distance & down %in% c(3,4), EPA,NA)),
           sack = ifelse(((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds = ifelse(((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa = ifelse(((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           sack_1st = ifelse(down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds_1st = ifelse(down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa_1st = ifelse(down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           sack_2nd = ifelse(down==2 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds_2nd = ifelse(down==2 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa_2nd = ifelse(down==2 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           sack_3rd = ifelse(down==3 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds_3rd = ifelse(down==3 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa_3rd = ifelse(down==3 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           sack_4th = ifelse(down==4 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds_4th = ifelse(down==4 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa_4th = ifelse(down==4 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           sack_std_down = ifelse(std.down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds_std_down = ifelse(std.down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa_std_down = ifelse(std.down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           sack_pass_down = ifelse(pass.down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),1,0),
           sack_yds_pass_down = ifelse(pass.down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),yards_gained,0),
           sack_epa_pass_down = ifelse(pass.down==1 & ((play_type %in% c("Sack","Sack Touchdown"))|(play_type == "Safety" & str_detect(play_text, "sacked"))),EPA,0),
           tfl_rush = ifelse(rush==1 & yards_gained <0,1,0),
           tfl_pass = ifelse(pass==1 & yards_gained <0,1,0),
           tfl_rush_yds = ifelse(rush==1 & yards_gained <0,yards_gained,0),
           tfl_pass_yds = ifelse(pass==1 & yards_gained <0,yards_gained,0),
           tfl_rush_epa = ifelse(rush==1 & yards_gained <0,EPA,0),
           tfl_pass_epa = ifelse(pass==1 & yards_gained <0,EPA,0),
           penalty_yds = ifelse(play_type=="Penalty",yards_gained,NA),
           penalty_yds_1st_down = ifelse(play_type=="Penalty"& down == 1,yards_gained,NA),
           penalty_yds_2nd_down = ifelse(play_type=="Penalty"& down == 2,yards_gained,NA),
           penalty_yds_3rd_down = ifelse(play_type=="Penalty"& down == 3,yards_gained,NA),
           penalty_yds_4th_down = ifelse(play_type=="Penalty"& down == 4,yards_gained,NA),
           penalty_yds_std_down = ifelse(play_type=="Penalty"& std.down == 1,yards_gained,NA),
           penalty_yds_pass_down = ifelse(play_type=="Penalty"& pass.down == 1,yards_gained,NA),
           penalty_epa = ifelse(play_type=="Penalty",EPA,NA),
           penalty_epa_1st_down = ifelse(play_type=="Penalty"& down == 1,EPA,NA),
           penalty_epa_2nd_down = ifelse(play_type=="Penalty"& down == 2,EPA,NA),
           penalty_epa_3rd_down = ifelse(play_type=="Penalty"& down == 3,EPA,NA),
           penalty_epa_4th_down = ifelse(play_type=="Penalty"& down == 4,EPA,NA),
           penalty_epa_std_down = ifelse(play_type=="Penalty"& std.down == 1,EPA,NA),
           penalty_epa_pass_down = ifelse(play_type=="Penalty"& pass.down == 1,EPA,NA),
           PlayTimeSecElapsed = TimeSecsRem_end - TimeSecsRem,
           year = j
    )
 return(pbp) 
}

extract_player_names <- function(play_df){
  ## Extract player names
  # RB names 
  pbp <- play_df %>%
    mutate(rush_player = ifelse(rush == 1, str_extract(play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA)) %>%
    mutate(rush_player = str_remove(rush_player, " run | \\d+ Yd Run"))
  
  # QB names 
  pbp <- pbp %>%
    mutate(pass_player = ifelse(pass==1, str_extract(play_text, "pass from (.*?) \\(|(.{0,30} )pass |(.{0,30} )sacked|(.{0,30} )incomplete "), NA)) %>%
    mutate(pass_player = str_remove(pass_player, "pass | sacked| incomplete")) %>%
    mutate(pass_player = if_else(play_type == "Passing Touchdown", str_extract(play_text, "from(.+)"), pass_player),
           pass_player = str_remove(pass_player, "from "), 
           pass_player = str_remove(pass_player, "\\(.+\\)"),
           pass_player = str_remove(pass_player, " \\,"))
  
  ## Receiver names
  pbp <- pbp %>%
    mutate(receiver_player = ifelse(pass==1, str_extract(play_text, "to (.+)"), NA)) %>%
    mutate(receiver_player = if_else(str_detect(play_text, "Yd pass"), str_extract(play_text, "(.+)\\d"), receiver_player)) %>%
    mutate(receiver_player = ifelse(play_type == "Sack", NA, receiver_player)) %>%
    mutate(receiver_player = str_remove(receiver_player, "to "),
           receiver_player = str_remove(receiver_player, "\\,.+"),
           receiver_player = str_remove(receiver_player, "for (.+)"),
           receiver_player = str_remove(receiver_player, "( \\d{1,2})"))
  
  ## Extract player names
  ## sack player names
  pbp <- pbp %>%
    mutate(sack_players = ifelse(pass==1 & play_type == "Sack", 
                                 str_extract(play_text, "sacked by (.+)"), NA)) %>%
    mutate(sack_players = str_remove(sack_players, "for (.+)")) %>%
    mutate(sack_players = str_remove(sack_players, "(.+) by")) %>%
    mutate(sack_player1 = str_remove(sack_players, "and (.+)")) %>%
    mutate(sack_player2 = if_else(str_detect(sack_players, "and (.+)"), 
                                  str_remove(sack_players, " (.+) and"), NULL)) 
  
  ## Interception player names
  pbp <- pbp %>%
    mutate(interception_player = ifelse(pass==1 & play_type == "Pass Interception Return", str_extract(play_text, "intercepted (.+)"), NA)) %>%
    mutate(interception_player = str_remove(interception_player, "return (.+)")) %>%
    mutate(interception_player = str_remove(interception_player, "(.+) intercepted ")) %>%
    mutate(interception_player = str_remove(interception_player, "intercepted"))
  
  return(pbp)
}
