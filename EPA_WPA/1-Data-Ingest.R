library(cfbscrapR)
library(dplyr)


## Pull Schedule data

df <- data.frame(years = 2010:2018)
schedule_df <- df %>% mutate(
  game_dat = purrr::map(years,cfb_game_info))
schedule_df <- schedule_df %>% tidyr::unnest(game_dat)
colnames(schedule_df)[2] <- "game_id"

## clean schedule data, remove FCS games 

## Drive data
drive_df <- df %>% mutate(
  drive_dat =purrr::map(years,cfb_pbp_data,week=NULL,drive=TRUE))
drive_dat = drive_df %>% tidyr::unnest(drive_dat)



week_vector = 1:15

