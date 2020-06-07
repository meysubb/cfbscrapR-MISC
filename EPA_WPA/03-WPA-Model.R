library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
library(parallel)
library(mgcv)
library(purrr)
source("07-CV-Utils.R")
source("08-Pred-Utils.R")

epa_14 <- readRDS("data/rds/EPA_FG_Calcs_2014.RDS")
epa_15 <- readRDS("data/rds/EPA_FG_Calcs_2015.RDS")
epa_16 <- readRDS("data/rds/EPA_FG_Calcs_2016.RDS")
epa_17 <- readRDS("data/rds/EPA_FG_Calcs_2017.RDS")
epa_18 <- readRDS("data/rds/EPA_FG_Calcs_2018.RDS")
epa_19 <- readRDS("data/rds/EPA_FG_Calcs_2019.RDS")

epa <- bind_rows(epa_14,epa_15,epa_16,epa_17,epa_18,epa_19)

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
  ) %>% group_by(game_id) %>%
  mutate(
    home_EPA_rtotal = cumsum(home_EPA),
    away_EPA_rtotal = cumsum(away_EPA),
    offense_EPA_rtotal = ifelse(home_offense, home_EPA_rtotal, away_EPA_rtotal),
    offense_EPA_ratio = ifelse(home_offense,
                               home_EPA_rtotal/away_EPA_rtotal,
                               away_EPA_rtotal/home_EPA_rtotal)
  ) %>% ungroup()


# Create the LOSO predictions for the selected nflscrapR model:
wp_model_loso_preds <- calc_wp_gam_loso_cv(as.formula("
                                      Win_Indicator ~ 
                                        half + s(ExpScoreDiff) + 
                                        s(TimeSecsRem, by = half) + 
                                        s(ExpScoreDiff_Time_Ratio) + 
                                        off_timeouts_rem_before*half*Under_two +
                                        def_timeouts_rem_before*half*Under_two +
                                        home_offense +
                                        offense_EPA_ratio"),
                                        wp_model_data = epa_w)

# Save dataset in data folder as wp_model_loso_preds.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
write_csv(wp_model_loso_preds, "data/wp_model_loso_preds.csv")
saveRDS(wp_model_loso_preds,"data/rds/wp_model_data_loso.rds")


# Use the following pipeline to create a dataset used for charting the
# cross-validation calibration results:

wp_cv_loso_calibration_results <- wp_model_loso_preds %>%
  # Create binned probability column:
  mutate(bin_pred_prob = round(win_prob / 0.05) * .05) %>%
  # Group by both the qtr and bin_pred_prob:
  group_by(period, bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_plays = n(), 
            n_wins = length(which(win_ind == 1)),
            bin_actual_prob = n_wins / n_plays) %>%
  ungroup() 


# Create a label data frame for the chart:
ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25), 
                       lab = c("More times\nthan expected", "Fewer times\nthan expected"),
                       period = factor("1st Quarter"))

# Create the calibration chart:
wp_cv_loso_calibration_results <- wp_cv_loso_calibration_results %>%
  mutate(qtr = fct_recode(factor(period), 
                      "1st Quarter" = "1", 
                      "2nd Quarter" = "2",
                      "3rd Quarter" = "3", 
                      "4th Quarter" = "4")) 

# Calculate the calibration error values:  
wp_cv_cal_error <- wp_cv_loso_calibration_results %>% 
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(period) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_plays = n(),
            n_wins = sum(n_wins, na.rm = TRUE))

# Overall weighted calibration error:
print("Weighted calibration error:")
with(wp_cv_cal_error, weighted.mean(weight_cal_error, n_wins))
cal_error <- round(with(wp_cv_cal_error, weighted.mean(weight_cal_error, n_wins)),5)
print(cal_error)
# new_hfa: 0.02411699, old: 0.02468438
plot_caption <- glue::glue("Overall Weighted Calibration Error: {cal_error}")

ggplot(data = wp_cv_loso_calibration_results) +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays))+
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +   geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(title = "Calibration plots for Win Probability Model",
       subtitle = "Leave-One-Season-Out Cross-Validation, WP Model",
       caption = plot_caption,
       size = "Number of plays",
       x = "Estimated Win Probability",
       y = "Observed Win Probability") + 
  geom_text(data = ann_text, aes(x = x, y = y, label = lab)) +
  theme(
    legend.title = element_text(size = 9, margin=margin(t=0.2,r=0,b=0.2,l=0.2,unit=c("mm")), family = "serif",face = "bold"),
    legend.text = element_text(size = 9, margin=margin(t=0.2,r=0,b=0.2,l=0.2,unit=c("mm")), family = "serif"),
    legend.background = element_rect(fill = "grey85"),
    legend.key = element_rect(fill = "grey85"),
    legend.key.width = unit(1.5,"mm"),
    legend.key.size = unit(2.0,"mm"),
    legend.margin=margin(t = 0.4,b = 0.4,l=0.4,r=0.4,unit=c('mm')),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box.background = element_rect(colour = "#500f1b"),
    axis.title.x = element_text(size = 12, margin=margin(0,0,0,0, unit=c("mm")), family = "serif", face = "bold"),
    axis.text.x = element_text(size = 12, margin=margin(0,0,-0.1,0, unit=c("mm")), family = "serif"),
    axis.title.y = element_text(size = 12, margin=margin(0,0,0,0, unit=c("mm")), family = "serif", face = "bold"),
    axis.text.y = element_text(size = 12, margin=margin(0,0.1,0,0, unit=c("mm")), family = "serif"),
    plot.title = element_text(size = 12, margin=margin(t=0, r=0, b=0.2,l=0, unit=c("mm")), 
                              lineheight=0.7, family = "serif", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, margin=margin(t=0, r=0, b=0.2,l=0, unit=c("mm")), 
                                 lineheight=0.7, family = "serif", hjust = 0.5),
    plot.caption = element_text(size = 10, margin=margin(t=0.3, r=0, b=0,l=0, unit=c("mm")), 
                                lineheight=0.7, family = "serif", face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey65",color="black"),
    plot.background = element_rect(fill = "grey85",color="black"),
    plot.margin=unit(c(1,2,1,3),"mm"))+      
  facet_wrap(~ qtr, ncol = 4)+ 
ggsave("figures/wp_cv_loso_calibration_results.png", height = 9/1.2, width = 16/1.2)




## final calculations
cl <- makeCluster(detectCores()-1)


wp_model <- mgcv::bam(Win_Indicator ~ 
                        half + s(ExpScoreDiff) + 
                        s(TimeSecsRem, by = half) + 
                        s(ExpScoreDiff_Time_Ratio) + 
                        off_timeouts_rem_before*half*Under_two +
                        def_timeouts_rem_before*half*Under_two +
                        home_offense +
                        offense_EPA_ratio ,
                      data = epa_w, family = "binomial", cluster=cl)

stopCluster(cl)

save(wp_model, file="data/final_wp_model.RData")




