library(cfbscrapR)
library(tidyverse)
library(dplyr)
library(stringr)
source("6-Utils.R")

## Pull Schedule data

df <- data.frame(years = 2014:2018)
schedule_df <- df %>% mutate(
  game_dat = purrr::map(years,cfb_game_info))
schedule_df <- schedule_df %>% tidyr::unnest(game_dat)
colnames(schedule_df)[2] <- "game_id"

## clean schedule data, remove FCS games
schedule_df_clean <- schedule_df %>% tidyr::drop_na(home_conference) %>% tidyr::drop_na(away_conference)
saveRDS(schedule_df_clean,"data/game_schedule.RDS")

## Drive data
drive_df <- df %>% mutate(
  drive_dat =purrr::map(years,cfb_pbp_data,week=NULL,drive=TRUE))
drive_dat = drive_df %>% tidyr::unnest(drive_dat)


# dat_merge <- drive_dat %>% merge(schedule_df_clean)
# colnames(dat_merge)[7] <- "drive_id"
# dat_merge <- dat_merge %>% select(-home_line_scores,-away_line_scores)
#write.csv(dat_merge,"data/clean_drives_data.csv",row.names = FALSE)

dat_merge <- read_csv("data/clean_drives_data.csv")

week_vector = 1:15
year_vector = 2014:2018
weekly_year_df = expand.grid(year=year_vector,week=week_vector)
### scrape yearly
year_split = split(weekly_year_df,weekly_year_df$year)


for(i in 1:length(year_split)){
  print(paste0("Working on ",i))
  year_split[[i]] = year_split[[i]] %>% mutate(
    pbp = purrr::map2(.x = year,.y=week,cfb_pbp_data)
  )
  Sys.sleep(5)
}

year_split = lapply(year_split,function(x){
  x %>% tidyr::unnest(pbp)
})


all_years = bind_rows(year_split) #%>% inner_join(drive)

# some of these were mis-labelled
# fix
td_e = str_detect(all_years$play_text,"TD") | str_detect(all_years$play_text,"Touchdown") | str_detect(all_years$play_text,"TOUCHDOWN")
all_years$play_type[grepl("KICK",all_years$play_text) & str_detect(all_years$play_text,"fumble") & td_e] <- paste0(all_years$play_type[grepl("KICK",all_years$play_text) & str_detect(all_years$play_text,"fumble") & td_e]," Touchdown")
write.csv(all_years,"data/raw_all_years.csv",row.names = FALSE)
#all_years <- read_csv("data/raw_all_years.csv")

drive_join_df = dat_merge %>% select(home_team,drive_id)
# Figure out the adjusted yard-line, since the API has it in terms of home team
# Need to remove OT data, since the clock is just binary.
clean_all_years = all_years %>% inner_join(drive_join_df,by=c('drive_id')) %>%
  arrange(id) %>%
  mutate_at(vars(clock.minutes, clock.seconds), ~replace_na(., 0)) %>%
  mutate(
    clock.minutes = ifelse(period %in% c(1,3),15+clock.minutes,clock.minutes),
    raw_secs = clock.minutes * 60 + clock.seconds,
    coef = home_team == defense,
    adj_yd_line = 100 * (1-coef) + (2*coef-1)*yard_line,
    log_ydstogo = log(adj_yd_line),
    half = ifelse(period<=2,1,2)
  ) %>% select(-coef)

# Adjust Field Goal by 17 yards
fg_inds = str_detect(clean_all_years$play_type,"Field Goal")
clean_all_years[fg_inds,"adj_yd_line"] = clean_all_years[fg_inds,"adj_yd_line"] + 17

## Figure out the next score now
clean_drive = dat_merge %>% mutate(
  pts_drive = case_when(
    str_detect(drive_result,"TD") ~ 7,
    #str_detect(drive_result,"FG") ~ 3,
    str_detect(drive_result,"SF") ~ -2,
    drive_result == 'FG GOOD' ~ 3,
    drive_result == 'FG' ~ 3,
    drive_result == 'MISSED FG TD' ~ -7,
    drive_result == 'KICKOFF RETURN TD' ~ -7,
    drive_result == 'END OF HALF TD' ~ 7,
    drive_result == "END OF GAME TD" ~ 7,
    drive_result == 'PUNT RETURN TD' ~ -7,
    drive_result == 'PUNT TD' ~ -7,
    drive_result == 'INT TD' ~ -7,
    drive_result == 'INT RETURN TOUCH' ~ -7,
    drive_result == 'FUMBLE RETURN TD' ~ -7,
    drive_result == 'FUMBLE TD' ~ -7,
    drive_result == 'DOWNS TD' ~ -7,
    TRUE ~ 0),
  scoring = ifelse(pts_drive!=0,TRUE,scoring)
) %>% arrange(game_id,drive_id)

## Find next scoring drive.
## try to find next score for just one game, then apply throughout
# g_ids = clean_drive$game_id %>% sample(5)
# sample_test = clean_drive %>% filter(game_id %in% g_ids) %>%
#   select(game_id,drive_id,offense,defense,scoring,start_yardline,end_yardline,plays,drive_result,pts_drive,start_period) %>%
#   mutate(
#     half = ifelse(start_period<=2,1,2)
#   ) %>% group_by(half) %>% arrange(game_id,drive_id)

## get the next score half
## with the drive_details
library(purrr)
clean_next_score_drive <- map_dfr(unique(clean_drive$game_id),
                               function(x) {
                                 clean_drive %>%
                                   filter(game_id == x) %>%
                                   find_game_next_score_half()
                               })

# drive, and the next drives score details
# join this back to the pbp
clean_next_select <- clean_next_score_drive %>% select(game_id,drive_id,offense,defense,neutral_site,NSH,scoring) %>%
  mutate(
    Next_Score = case_when(
      NSH == 7 ~ "TD",
      NSH == 3 ~ "FG",
      NSH == 2 ~ "Safety",
      NSH == -2 ~ "Opp Safety",
      NSH == -3  ~ "Opp FG",
      NSH == -7 ~ "Opp TD",
      TRUE ~ "No_Score"
    )
  )
#%>%mutate(drive_id = as.numeric(drive_id))
pbp_full_df <- clean_all_years %>% left_join(clean_next_select)
#write.csv(pbp_full_df,"data/full_pbp_df.csv")

## ESPN doesn't report full games in some instances, and that really throws things off. 
## get rid of these. Thanks
check_for_full_game = pbp_full_df %>%  filter(period==4) %>% group_by(game_id,clock.minutes) %>% 
  summarize(val=n()) %>% filter(clock.minutes == min(clock.minutes))
keep_full_games = check_for_full_game %>% filter(clock.minutes==0) %>% pull(game_id)

pbp_full_df = pbp_full_df %>% filter(game_id %in% keep_full_games)

saveRDS(pbp_full_df,"data/pbp.rds")
