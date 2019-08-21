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


epa[is.na(epa$ep_after),"ep_after"] = 0
epa$EPA <- epa$ep_after - epa$ep_before

epa_w = epa %>% left_join(win_df) %>% 
  mutate(
    score_diff = offense_score - defense_score,
    home_EPA = ifelse(offense==home_team,EPA,-EPA),
    away_EPA = -home_EPA,
    ExpScoreDiff = score_diff + ep_before,
    Win_Indicator = as.factor(ifelse(offense==winner,1,0)),
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
#                         Under_two*half +
#                         Under_two*half,
#                       data = epa_w, family = "binomial", cluster=cl)
# 
# save(wp_model, file="data/wp_model.RData")
# stopCluster(cl)

load("wp_model.RData")
library(mgcv)
create_wpa <- function(df,wp_mod){
  Off_Win_Prob = predict(wp_mod,newdata=df,type="response")
  df2 = df %>% mutate(
    wp = Off_Win_Prob,
    def_wp = 1-wp,
    home_wp = if_else(offense == home_team,
                      wp,def_wp),
    away_wp = if_else(offense != home_team,
                      wp,def_wp),
    # ball changes hand
    change_of_poss = ifelse(offense == lead(offense),0,1),
    change_of_poss = ifelse(is.na(change_of_poss),0,change_of_poss),
    # base wpa
    lead_wp = lead(wp),
    wpa_base = lead_wp - wp,
    # account for turnover
    wpa_change = ifelse(change_of_poss==1,(1-lead_wp)-wp,wpa_base),
    wpa = wpa_change,
    home_team_wpa = if_else(offense == home_team,
                            wpa, -wpa),
    # try a different way of calculating away team
    away_team_wpa = if_else(offense != home_team,
                            wpa,-wpa),
    cum_home_wpa = cumsum(home_team_wpa),
    cum_away_wpa = cumsum(away_team_wpa),
    final_home_wpa =  0.5 + cum_home_wpa,
    final_away_wpa =  0.5 + cum_away_wpa,
    adj_TimeSecsRem = ifelse(half==1,1800+TimeSecsRem,TimeSecsRem)
  )
  return(df2)
}


plot_func <- function(dat,away_color,home_color,year){
  away_team <- names(away_color)
  home_team <- names(home_color)
  names(away_color) <- NULL
  names(home_color) <- NULL
  plot_df <- dat %>% select(final_home_wpa,final_away_wpa,adj_TimeSecsRem) %>% gather(team,wpa,-adj_TimeSecsRem)
  
  p1 = ggplot(plot_df,aes(x=adj_TimeSecsRem,y=wpa,color=team)) + 
    geom_line(size=2) + 
    geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") + 
    scale_x_reverse(breaks = seq(0, 3600, 300)) + 
    scale_color_manual(labels = c(away_team,home_team),
                      values = c(away_color,home_color),
                      guide = FALSE)  +
    annotate("text", x = 3000, y = .75, label = away_team, color = away_color, size = 8) +
    annotate("text", x = 3000, y = .25, label = home_team, color = home_color, size = 8) +
    scale_y_continuous(limits=c(0,1)) +
    geom_vline(xintercept = 900, linetype = "dashed", black) +
    geom_vline(xintercept = 1800, linetype = "dashed", black) +
    geom_vline(xintercept = 2700, linetype = "dashed", black) +
    geom_vline(xintercept = 0, linetype = "dashed", black) +
    labs(
      x = "Time Remaining (seconds)",
      y = "Win Probability",
      title = paste("Win Probability Chart -", year),
      subtitle = paste(home_team,"vs",away_team),
      caption = "Data from collegefootballdataAPI, Plot by @msubbaiah1"
    ) + theme_bw(base_size = 16)
  return(p1)
}

## Test plot of TAMU vs Clemson WPA
tamu_18 = epa_w %>% filter(
  year == 2018,
  offense %in% c("Clemson", "Texas A&M"),
  defense %in% c("Clemson", "Texas A&M")
)
tamu_wpa = tamu_18 %>% create_wpa(wp_mod=wp_model)

## Need to automate this last part
tamu_wpa[is.na(tamu_wpa$final_away_wpa),"final_away_wpa"] <- 1
tamu_wpa[is.na(tamu_wpa$final_home_wpa),"final_home_wpa"] <- 0
away_color <- c(CLE="#F56600")
home_color <- c(TAMU="#500000")

plot_func(tamu_wpa,away_color,home_color,year=2018)



## Test Plot UT vs USC
ut_ou = epa_w %>% filter(
  year==2018,
  week == 6,
  offense %in% c("Oklahoma","Texas"),
  defense %in% c("Oklahoma","Texas")
)
ut_wpa = ut_ou %>% create_wpa(wp_mod=wp_model)
ut_wpa[is.na(ut_wpa$final_home_wpa),"final_home_wpa"] <- 0
ut_wpa[is.na(ut_wpa$final_away_wpa),"final_away_wpa"] <- 1
away_color <- c(UT="#BF5700")
home_color <- c(OU="#841617")
plot_func(ut_wpa,away_color,home_color,year="2018 - Week 6")


## Test plot UM vs OSU - 2016
um_osu = epa_w %>% filter(
  year == 2017,
  offense %in% c("Michigan","Ohio State"),
  defense %in% c("Michigan","Ohio State")
)
um_wpa = um_osu %>% create_wpa(wp_mod=wp_model)
um_wpa[is.na(um_wpa$final_home_wpa),"final_home_wpa"] <- 0
um_wpa[is.na(um_wpa$final_away_wpa),"final_away_wpa"] <- 1
away_color <- c(OSU="#BB0000")
home_color <- c(UM="#00274C")
plot_func(um_wpa,away_color,home_color,year=2017)


tamu_ucla = epa_w %>% filter(
  year ==2017,
  offense %in% c("UCLA", "Texas A&M"),
  defense %in% c("UCLA", "Texas A&M")
)
tamu_wpa = tamu_ucla %>% create_wpa(wp_mod=wp_model)
tamu_wpa_17 <- tamu_wpa %>% filter(year==2017)

tamu_wpa_17[is.na(tamu_wpa_17$final_away_wpa),"final_away_wpa"] <- 0
tamu_wpa_17[is.na(tamu_wpa_17$final_home_wpa),"final_home_wpa"] <- 1
away_color <- c(TAMU="#500000")
home_color <- c(UCLA="#2D68C4")


plot_func(tamu_wpa_17,away_color,home_color,year=2017)

uva_vtech = epa_w %>% filter(
  year == 2018,
  offense %in% c("Virginia","Virginia Tech"),
  defense %in% c("Virginia","Virginia Tech"  )
) %>% create_wpa(wp_mod = wp_model)

home_color <- c(VT="#500000")
away_color <- c(UVA="#BF5700")

plot_func(uva_vtech,away_color,home_color,year=2018)
