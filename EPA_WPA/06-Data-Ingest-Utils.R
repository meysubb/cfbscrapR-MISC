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
