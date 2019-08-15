library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
source("6-Utils.R")

epa_16 <- readRDS("data/EPA_calcs_2016.RDS")
epa_17 <- readRDS("data/EPA_calcs_2017.RDS")
epa_18 <- readRDS("data/EPA_calcs_2018.RDS")

epa <- bind_rows(epa_16,epa_17,epa_18)

## Game ID 
## to see who won?

game_sched <- readRDS("data/game_schedule.RDS")

win_df <- game_sched %>% mutate(
  winner = ifelse(home_points > away_points, home_team,
                   ifelse(home_points < away_points, away_team, "TIE"))
) %>% select(game_id,winner)

epa_w = epa %>% left_join(win_df) %>% 
  mutate(
    score_diff = offense_score - defense_score,
    ExpScoreDiff = score_diff + EPA,
    Win_Indicator = as.factor(ifelse(offense==winner,1,0)),
    half = as.factor(half),
    ExpScoreDiff_Time_Ratio = ExpScoreDiff/ (TimeSecsRem + 1)
  )

library(parallel)
cl <- makeCluster(detectCores()-1)


wp_model <- mgcv::bam(Win_Indicator ~ s(ExpScoreDiff) + 
                        s(TimeSecsRem, by = half) + 
                        s(ExpScoreDiff_Time_Ratio) + 
                        Under_two*half + 
                        Under_two*half,
                      data = epa_w, family = "binomial", cluster=cl)

save(wp_model, file="wp_model.RData")


tamu_18 = epa_w %>% filter(
    year == 2018,
    offense %in% c("Clemson", "Texas A&M"),
   defense %in% c("Clemson", "Texas A&M")
)

tamu_18$wp = predict(wp_model,newdata = tamu_18,type="response")

tamu_18_s = tamu_18 %>% mutate(
  wpa = lead(wp) - wp,
  home_team_wpa = if_else(offense == home_team,
                                 wpa, -wpa),
  away_team_wpa = if_else(defense == home_team,
                          -wpa,wpa)
)
