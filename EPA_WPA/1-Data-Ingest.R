library(cfbscrapR)
library(dplyr)
library(stringr)


## Pull Schedule data

df <- data.frame(years = 2010:2018)
schedule_df <- df %>% mutate(
  game_dat = purrr::map(years,cfb_game_info))
schedule_df <- schedule_df %>% tidyr::unnest(game_dat)
colnames(schedule_df)[2] <- "game_id"

## clean schedule data, remove FCS games 
schedule_df_clean <- schedule_df %>% tidyr::drop_na(home_conference) %>% tidyr::drop_na(away_conference)

## Drive data
drive_df <- df %>% mutate(
  drive_dat =purrr::map(years,cfb_pbp_data,week=NULL,drive=TRUE))
drive_dat = drive_df %>% tidyr::unnest(drive_dat)


dat_merge <- drive_dat %>% merge(schedule_df_clean)
colnames(dat_merge)[7] <- "drive_id"
write.csv(dat_merge,"data/clean_drives_data.csv")

week_vector = 1:15
year_vector = 2010:2018
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


all_years = bind_rows(year_split) %>% inner_join(drive)

drive_join_df = dat_merge %>% select(home_team,drive_id)
# Figure out the adjusted yard-line, since the API has it in terms of home team
clean_all_years = all_years %>% inner_join(drive_join_df,by=c('drive_id')) %>% 
  mutate(
    coef = home_team == defense,
    adj_yd_line = 100 * (1-coef) + (2*coef-1)*yard_line
  )

## Figure out the next score now
clean_drive = dat_merge %>% mutate(
  pts_drive = case_when(
    str_detect(drive_result,"TD") ~ 7,
    str_detect(drive_result,"FG") ~ 3,
    str_detect(drive_result,"SF") ~ -2,
    drive_result == 'MISSED FG TD' ~ -7,
    drive_result == 'KICKOFF RETURN TD' ~ -7,
    drive_result == 'PUNT RETURN TD' ~ -7,
    drive_result == 'PUNT TD' ~ -7,
    drive_result == 'INT TD' ~ -7,
    drive_result == 'INT RETURN TOUCH' ~ -7,
    drive_result == 'FUMBLE RETURN TD' ~ -7,
    drive_result == 'FUMBLE TD' ~ -7,
    drive_result == 'DOWNS TD' ~ -7,
    TRUE ~ 0)
) %>% arrange(game_id,drive_id) %>% 
  mutate(
    next_drive_point = pmax(lead(-pts_drive),-2)
  )
                                           
clean_drive[which(clean_drive$pts_drive == 0),"pts_drive"] = clean_drive$next_drive_point
