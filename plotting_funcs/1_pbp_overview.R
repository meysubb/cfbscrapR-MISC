library(ggrepel)
prep_df_pbp_overview <- function(df){
  clean_df = df %>%  
    mutate(
      event = case_when(
        str_detect(play_text,"fumble") ~ "Fumble",
        str_detect(play_text,"interception") ~ "INT",
        str_detect(play_text,"sack") ~ "Sack",
        TRUE ~ "nothing"
      ),
      event = ifelse(event=="nothing",NA,event),
      clean_play_type = case_when(
        str_detect(play_type,"Pass") ~ "Pass",
        str_detect(play_type,"Rush") ~ "Rush",
        str_detect(play_type,"Field Goal") ~ "Kick",
        str_detect(play_type,"Kickoff") ~ "Kick",
        str_detect(play_type,"Punt") ~ "Punt",
        str_detect(play_type,"Penalty") ~ "Penalty",
        TRUE ~ "Other"
      ),
      new_drive_id = as.numeric(gsub("401110804","",drive_id)),
      score_text = paste0(offense_score,"-",defense_score)
    ) %>% group_by(drive_id) %>% 
    mutate(
      play_num = row_number()
    )  %>% ungroup()
  
}

plot_pbp_overview <- function(df,gameid,s_title){
  game_df = df %>% filter(game_id == gameid)
  clean_game_df = prep_df_pbp_overview(game_df)
  clean_game_df$new_drive_id = as.numeric(gsub(gameid,"",clean_game_df$drive_id))
  
  clean_drive_info = clean_game_df %>% group_by(new_drive_id) %>% filter(row_number()==n()) %>% ungroup() %>% 
    mutate(y_max = max(play_num) + 5,
           score_text = ifelse(scoring==TRUE,score_text,NA)) 
  
  
  nd_id = clean_drive_info$new_drive_id
  off_team = clean_drive_info$offense
  
  ggplot() + 
    geom_tile(data=clean_game_df,aes(x=new_drive_id,y=play_num,fill=clean_play_type,alpha=yards_gained,width=0.8),size=1.2) + 
    geom_text_repel(data=clean_game_df,aes(x=new_drive_id,y=play_num,label=event),point.padding = NA) + 
    geom_label_repel(data=clean_drive_info,aes(x=new_drive_id,y=y_max,label=score_text),point.padding = NA) + 
    coord_flip() + 
    scale_alpha(breaks=c(-20,-10,0,10,20)) + 
    scale_x_reverse(labels = off_team, breaks = nd_id,expand = expand_scale(add = 1.2)) +
    labs(
      x = "",
      y = "",
      fill = "Play type",
      alpha = "Yards gained",
      title = "Play-by-Play Overview",
      subtitle = s_title,
      caption = "@CFB_Data | @msubbaiah1"
    ) + 
    theme_classic(base_size=16)
}


plot_pbp_overview(pbp_full_df,401110731,"Texas A&M vs. Texas State")
plot_pbp_overview(pbp_full_df,401110784,"Texas A&M vs. Clemson")
plot_pbp_overview(pbp_full_df,401110804,"Texas A&M vs. Auburn")




  




