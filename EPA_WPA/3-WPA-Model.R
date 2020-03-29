library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
source("6-Utils.R")

epa_15 <- readRDS("data/rds/EPA_calcs_2015.RDS")
epa_16 <- readRDS("data/rds/EPA_calcs_2016.RDS")
epa_17 <- readRDS("data/rds/EPA_calcs_2017.RDS")
epa_18 <- readRDS("data/rds/EPA_calcs_2018.RDS")

epa <- bind_rows(epa_16,epa_17,epa_18)

## Game ID
## to see who won?

game_sched <- readRDS("data/game_schedule.RDS")

win_df <- game_sched %>% mutate(
  winner = ifelse(home_points > away_points, home_team,
                   ifelse(home_points < away_points, away_team, "TIE"))
) %>% select(game_id,winner)


epa[is.na(epa$ep_after),"ep_after"] = 0
epa$EPA <- epa$ep_after - epa$ep_before

epa_w = epa %>% left_join(win_df) %>%
  mutate(
    score_diff = offense_score - defense_score,
    home_EPA = ifelse(offense_play==home,EPA,-EPA),
    away_EPA = -home_EPA,
    ExpScoreDiff = score_diff + ep_before,
    Win_Indicator = as.factor(ifelse(offense_play==winner,1,0)),
    half = as.factor(half),
    ExpScoreDiff_Time_Ratio = ExpScoreDiff/ (TimeSecsRem + 1)
  )

# library(parallel)
# cl <- makeCluster(detectCores()-1)
#
#
# wp_model <- mgcv::bam(Win_Indicator ~ s(ExpScoreDiff) +
#                         s(TimeSecsRem, by = half) +
#                         s(ExpScoreDiff_Time_Ratio) +
#                         Under_two*half,
#                       data = epa_w, family = "binomial", cluster=cl)
#
# stopCluster(cl)

save(wp_model, file="data/wp_model.RData")

load("wp_model.RData")
library(mgcv)
create_wpa <- function(df,wp_mod){
  Off_Win_Prob = as.vector(predict(wp_mod,newdata=df,type="response"))
  df2 = df %>% mutate(
    wp = Off_Win_Prob,
    def_wp = 1-wp,
    home_wp = if_else(offense_play == home,
                      wp,def_wp),
    away_wp = if_else(offense_play != home,
                      wp,def_wp)) %>% group_by(half) %>%
    mutate(
      # ball changes hand
      change_of_poss = ifelse(offense_play == lead(offense_play), 0, 1),
      change_of_poss = ifelse(is.na(change_of_poss), 0, change_of_poss)) %>% ungroup() %>%
    mutate(
      # base wpa
      end_of_half = ifelse(half == lead(half),0,1),
      lead_wp = lead(wp),
      wpa_base = lead_wp - wp,
      # account for turnover
      wpa_change = ifelse(change_of_poss == 1, (1 - lead_wp) - wp, wpa_base),
      wpa = ifelse(end_of_half==1,0,wpa_change),
      home_wp_post = ifelse(offense_play == home,
                            home_wp + wpa,
                            home_wp - wpa),
      away_wp_post = ifelse(offense_play != home,
                            away_wp + wpa,
                            away_wp - wpa),
      adj_TimeSecsRem = ifelse(half == 1, 1800 + TimeSecsRem, TimeSecsRem)
  )
  return(df2)
}




epa_19 <- readRDS("data/rds/EPA_calcs_2019.RDS")


epa <- bind_rows(epa_16,epa_17,epa_18,epa_19)

epa[is.na(epa$ep_after),"ep_after"] = 0
epa$EPA <- epa$ep_after - epa$ep_before

epa_w = epa %>% left_join(win_df) %>%
  mutate(
    score_diff = offense_score - defense_score,
    home_EPA = ifelse(offense_play==home,EPA,-EPA),
    away_EPA = -home_EPA,
    ExpScoreDiff = score_diff + ep_before,
    Win_Indicator = as.factor(ifelse(offense_play==winner,1,0)),
    half = as.factor(half),
    ExpScoreDiff_Time_Ratio = ExpScoreDiff/ (TimeSecsRem + 1)
  )


