library(cfbscrapR)
library(tidyverse)
library(dplyr)
library(stringr)
source("06-Data-Ingest-Utils.R")

## Pull Schedule data

df <- data.frame(years = 2009:2019)
schedule_df <-
  df %>% mutate(game_dat = purrr::map(years, cfb_game_info))
schedule_df <- schedule_df %>% tidyr::unnest(game_dat)
colnames(schedule_df)[2] <- "game_id"

## clean schedule data, remove FCS games
schedule_df_clean <-
  schedule_df %>% tidyr::drop_na(home_conference) %>% tidyr::drop_na(away_conference)
saveRDS(schedule_df_clean, "data/game_schedule.RDS")

# schedule_df_clean <- readRDS("data/game_schedule.RDS")

## Drive data
drive_df <- df %>% mutate(drive_dat = purrr::map(
  years,
  cfb_drives,
  season_type = 'both',
  week = NULL
))
drive_dat = drive_df %>% tidyr::unnest(drive_dat)


dat_merge <- drive_dat %>% merge(schedule_df_clean)
colnames(dat_merge)[7] <- "drive_id"
saveRDS(dat_merge,"data/clean_drives_data.RDS")
#write.csv(dat_merge, "data/clean_drives_data.csv", row.names = FALSE)

# dat_merge <- readRDS("data/clean_drives_data.RDS")

week_vector = 1:15
year_vector = 2009:2019
weekly_year_df = expand.grid(year = year_vector, week = week_vector)
### scrape yearly
year_split = split(weekly_year_df, weekly_year_df$year)


for (i in 1:length(year_split)) {
  print(paste0("Working on ", i))
  year_split[[i]] = year_split[[i]] %>% mutate(pbp = purrr::map2(
    .x = year,
    .y = week,
    cfb_pbp_data,
    season_type = 'both'
  ))
  Sys.sleep(1)
}

year_split = lapply(year_split, function(x) {
  x %>% tidyr::unnest(pbp)
})


all_years = bind_rows(year_split) #%>% inner_join(drive)

# check which special teams plays had touchdowns in the text
# but not in the description and add touchdown to them
all_years <- cfbscrapR::clean_pbp_dat(all_years)

saveRDS(all_years, "data/raw_all_years.rds")
# all_years <- readRDS("data/raw_all_years.rds")

drive_join_df = dat_merge %>% select(home_team, drive_id) %>%
  mutate(drive_id = as.numeric(drive_id))

# Figure out the adjusted yard-line, since the API has it in terms of home team
# Need to remove OT data, since the clock is just binary.
clean_all_years = all_years %>%
  mutate(drive_id = as.numeric(drive_id)) %>%
  inner_join(drive_join_df, by = c('drive_id')) %>%
  arrange(drive_id) %>%
  mutate_at(vars(clock.minutes, clock.seconds), ~ replace_na(., 0)) %>%
  mutate(
    clock.minutes = ifelse(period %in% c(1, 3), 15 + clock.minutes, clock.minutes),
    raw_secs = clock.minutes * 60 + clock.seconds,
    half = ifelse(period <= 2, 1, 2),
    # make sure all kick off downs are -1
    down = ifelse(down == 5 &
                    str_detect(play_type, "Kickoff"),-1, down)
  ) %>% filter(down < 5)

## Figure out the next score now
clean_drive <- cfbscrapR::clean_drive_info(dat_merge %>% rename("id"="drive_id"))

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
clean_next_select <-
  clean_next_score_drive %>% select(game_id,
                                    drive_id,
                                    offense,
                                    defense,
                                    neutral_site,
                                    NSH,
                                    DSH,
                                    scoring) %>%
  mutate(
    Next_Score = case_when(
      NSH == 7 ~ "TD",
      NSH == 3 ~ "FG",
      NSH == 2 ~ "Safety",
      NSH == -2 ~ "Opp_Safety",
      NSH == -3  ~ "Opp_FG",
      NSH == -7 ~ "Opp_TD",
      TRUE ~ "No_Score"
    )
  )
clean_next_select = clean_next_select %>% mutate(drive_id = as.numeric(drive_id)) %>% rename("scoring_drive" = "scoring")
pbp_full_df <-
  clean_all_years %>% left_join(clean_next_select,
                                by=c("game_id","drive_id"),
                                suffix=c("_play","_drive")) %>% 
  mutate(log_ydstogo = log(distance))

## TO-DO Need to identify other Extra Point lines

# Adjust Field Goal by 17 yards
fg_inds = str_detect(pbp_full_df$play_type, "Field Goal")
ep_inds = str_detect(pbp_full_df$play_type, "Extra Point")
kicker_inds = fg_inds | ep_inds
pbp_full_df[kicker_inds, "yards_to_goal"] = pbp_full_df[kicker_inds, "yards_to_goal"] + 17
pbp_full_df[kicker_inds, "log_ydstogo"] = log(pbp_full_df[kicker_inds, "distance"])

## Remove OT games
OT_games = pbp_full_df %>% group_by(game_id) %>%
  summarize(max_per = max(period))

OT_games_lst = OT_games %>% filter(max_per > 4) %>% pull(game_id)

pbp_full_df = pbp_full_df %>%  filter(!(game_id %in% OT_games_lst))


## ESPN doesn't report full games in some instances, and that really throws things off.
## get rid of these. Thanks
check_for_full_game = pbp_full_df %>%  filter(period == 4) %>% group_by(game_id, clock.minutes) %>%
  summarize(val = n()) %>% filter(clock.minutes == min(clock.minutes))
keep_full_games = check_for_full_game %>% filter(clock.minutes == 0) %>% pull(game_id)

pbp_full_df = pbp_full_df %>%
  filter(game_id %in% keep_full_games) %>%
  mutate(
    # calculate absolute score difference
    abs_diff = abs(offense_score - defense_score),
    # Calculate the drive difference between the next score drive and the
    # current play drive:
    Drive_Score_Dist = DSH - as.numeric(drive_id),
    # Create a weight column based on difference in drives between play
    # and next score:
    Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
      (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
    # Create a weight column based on score differential:
    ScoreDiff_W = (max(abs_diff) - abs_diff) /
      (max(abs_diff) - min(abs_diff)),
    # Add these weights together and scale again:
    Total_W = Drive_Score_Dist_W + ScoreDiff_W,
    Total_W_Scaled = (Total_W - min(Total_W)) /
      (max(Total_W) - min(Total_W))
  )

saveRDS(pbp_full_df, "data/pbp.RDS")
