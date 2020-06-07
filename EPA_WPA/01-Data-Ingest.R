library(cfbscrapR)
library(tidyverse)
library(dplyr)
library(stringr)
source("06-Data-Ingest-Utils.R")
week_vector = 1:15
year_vector = 2013:2019
years_to_train = 2013:2019

## Pull Schedule data

df <- data.frame(years = years_to_train)
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
  cfb_pbp_data,
  season_type = 'both',
  week = NULL,
  drive = TRUE
))
drive_dat = drive_df %>% tidyr::unnest(drive_dat)


dat_merge <- drive_dat %>% merge(schedule_df_clean)
colnames(dat_merge)[7] <- "drive_id"
dat_merge <-
  dat_merge %>% select(-home_line_scores, -away_line_scores)
write.csv(dat_merge, "data/clean_drives_data.csv", row.names = FALSE)

# dat_merge <- read_csv("data/clean_drives_data.csv")

### scrape weekly
weekly_year_df = expand.grid(year = year_vector, week = week_vector)
### scrape yearly
year_split = split(weekly_year_df, weekly_year_df$year)


for (i in 1:length(year_split)) {
  print(paste0("Working on ", i))
  year_split[[i]] = year_split[[i]] %>% mutate(pbp = purrr::map2(
    .x = year,
    .y = week,
    cfb_pbp_data,
    season_type = 'both',
    epa_wpa = FALSE
  ))
  Sys.sleep(5)
}

year_split = lapply(year_split, function(x) {
  x %>% tidyr::unnest(pbp)
})


all_years = bind_rows(year_split) #%>% inner_join(drive)

# check which special teams plays had touchdowns in the text
# but not in the description and add touchdown to them
#-- touchdowns----
td_vec = str_detect(all_years$play_text, "TD|Touchdown|TOUCHDOWN|touchdown")

#-- kicks/punts----
kick_vec = str_detect(all_years$play_text, "KICK") &
  !is.na(all_years$play_text)
punt_vec = str_detect(all_years$play_text, "Punt|punt") &
  !is.na(all_years$play_text)

#-- fumbles----
fumble_vec = str_detect(all_years$play_text, "fumble")
#-- pass/rush split----
rush_vec = all_years$play_type == "Rush"
pass_vec = all_years$play_type == "Pass Reception" | all_years$play_type == "Pass Completion" | all_years$play_type == "Pass"

#-- sacks----
#-- non-safety sacks are the only ones we want, otherwise would be an additional condition----
sack_vec = all_years$play_type == "Sack" |
  all_years$play_type == "Sack Touchdown"

#-- change of possession
poss_change_vec = all_years$change_of_poss == 1

## Fix strip-sacks to fumbles----
all_years$play_type[fumble_vec &
                      sack_vec & poss_change_vec & !td_vec] <-
  "Fumble Recovery (Opponent)"
all_years$play_type[fumble_vec & sack_vec & td_vec] <-
  "Fumble Recovery (Opponent) Touchdown"

## touchdown check, want where touchdowns aren't in the play_type----
td_check = !str_detect(all_years$play_type, "Touchdown")

#-- fix kickoff fumble return TDs----
all_years$play_type[kick_vec & fumble_vec & td_vec & td_check] <-
  paste0(all_years$play_type[kick_vec &
                               fumble_vec &
                               td_vec & td_check], " Touchdown")

#-- fix punt return TDs----
all_years$play_type[punt_vec & td_vec & td_check] <-
  paste0(all_years$play_type[punt_vec &
                               td_vec & td_check], " Touchdown")

#-- fix rush/pass tds that aren't explicit----
all_years$play_type[td_vec & rush_vec] = "Rushing Touchdown"
all_years$play_type[td_vec & pass_vec] = "Passing Touchdown"

#-- fix duplicated TD play_type labels----
pun_td_sq = (all_years$play_type == "Punt Touchdown Touchdown")
all_years$play_type[pun_td_sq] <- "Punt Touchdown"
fum_td_sq = (all_years$play_type == "Fumble Return Touchdown Touchdown")
all_years$play_type[fum_td_sq] == "Fumble Return Touchdown"
rush_td_sq = (all_years$play_type == "Rushing Touchdown Touchdown")
all_years$play_type[rush_td_sq] == "Rushing Touchdown"

## penalty detection-----
all_years <- add_penalty_cols(all_years)

## kickoff down adjustment
all_years = all_years %>%
  mutate(down = ifelse(down == 5 & str_detect(play_type, "Kickoff"), 1, down),
         down = ifelse(down == 5 & str_detect(play_type, "Penalty"),1 , down),
         half = ifelse(period <= 2, 1, 2))

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
clean_drive = dat_merge %>% mutate(
  pts_drive = case_when(
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
    str_detect(drive_result, "SF") ~ -2,
    str_detect(drive_result, "TD") ~ 7,
    TRUE ~ 0
  ),
  scoring = ifelse(pts_drive != 0, TRUE, scoring)
) %>% arrange(game_id, drive_id)

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
clean_next_select = clean_next_select %>% mutate(drive_id = as.numeric(drive_id))
pbp_full_df <-
  clean_all_years %>% left_join(clean_next_select) %>% mutate(log_ydstogo = log(distance))

# TO-DO Need to identify other Extra Point lines

## Adjust Field Goal by 17 yards
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
check_for_full_game = pbp_full_df %>%  
  filter(period == 4) %>% 
  group_by(game_id, clock.minutes) %>%
  summarize(val = n()) %>% 
  filter(clock.minutes == min(clock.minutes))

keep_full_games = check_for_full_game %>% 
  filter(clock.minutes == 0) %>% 
  pull(game_id)

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

# add timeout columns
pbp_full_TO_df <- add_timeout_cols(pbp_full_df)

pbp_full_counts_df <- add_play_counts(pbp_full_TO_df)

saveRDS(pbp_full_counts_df, "data/pbp.RDS")
