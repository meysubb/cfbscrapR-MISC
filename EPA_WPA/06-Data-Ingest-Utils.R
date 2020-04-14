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

add_timeout_cols <- function(play_df){
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
}
