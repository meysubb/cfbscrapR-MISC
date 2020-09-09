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
  group_by(game_id) %>% 
  mutate(
    game_play = 1,
    game_play_number = cumsum(game_play),adj_TimeSecsRem = ifelse(.data$half == 1, 1800 + .data$TimeSecsRem, .data$TimeSecsRem),
    turnover_vec_lag = dplyr::lag(.data$turnover_vec, 1),
    def_td_play_lag = dplyr::lag(.data$def_td_play, 1),
    play_after_turnover = ifelse(.data$turnover_vec_lag == 1 & .data$def_td_play_lag != 1, 1, 0),
    score_diff = .data$offense_score - .data$defense_score,
    lag_score_diff = lag(.data$score_diff, 1),
    lag_score_diff = ifelse(.data$game_play_number == 1, 0, .data$lag_score_diff),
    offense_play_lag = dplyr::lag(.data$offense_play, 1),
    offense_play_lag = ifelse(.data$game_play_number == 1, .data$offense_play, .data$offense_play_lag),
    offense_play_lead = dplyr::lead(.data$offense_play, 1),
    offense_play_lead2 = dplyr::lead(.data$offense_play, 2),
    score_pts = ifelse(.data$offense_play_lag == .data$offense_play,
                       (.data$score_diff - .data$lag_score_diff),
                       (.data$score_diff + .data$lag_score_diff)),
    score_diff_start = ifelse(.data$offense_play_lag == .data$offense_play,
                              .data$lag_score_diff,
                              -1*.data$lag_score_diff),
    EPA = .data$ep_after - .data$ep_before,
    def_EPA = -1*.data$EPA,
    home_EPA = ifelse(.data$offense_play == .data$home, .data$EPA, -1*.data$EPA),
    away_EPA = -1*.data$home_EPA,
    ExpScoreDiff = .data$score_diff_start + .data$ep_before,
    half = as.factor(.data$half),
    ExpScoreDiff_Time_Ratio = .data$ExpScoreDiff/(.data$adj_TimeSecsRem + 1)) %>% 
      ungroup()  
  
epa_w = epa_w %>% mutate(Win_Indicator = as.factor(ifelse(offense_play==winner,1,0)))


# Create the LOSO predictions for the selected nflscrapR model:
wp_model_loso_preds <- calc_wp_gam_loso_cv(as.formula("Win_Indicator ~ 
                                                      s(ExpScoreDiff) + 
                                                      s(TimeSecsRem, by = half) + 
                                                      s(ExpScoreDiff_Time_Ratio) + 
                                                      Under_two*off_timeouts_rem_before*half + 
                                                      Under_two*def_timeouts_rem_before*half"),
                                           wp_model_data = epa_w)

# Save dataset in data folder as wp_model_loso_preds.csv
# (NOTE: this dataset is not pushed due to its size exceeding
# the github limit but will be referenced in other files)
write_csv(wp_model_loso_preds, "data/wp_model_loso_preds.csv")
saveRDS(wp_model_loso_preds,"data/wp_model_data_loso.rds")


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
            bin_actual_prob = n_wins / n_plays)


# Create a label data frame for the chart:
ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25), 
                       lab = c("More times\nthan expected", "Fewer times\nthan expected"),
                       period = factor("1st Quarter"))

# Create the calibration chart:
wp_cv_loso_calibration_results %>%
  ungroup() %>%
  mutate(qtr = fct_recode(factor(period), "1st Quarter" = "1", "2nd Quarter" = "2",
                          "3rd Quarter" = "3", "4th Quarter" = "4")) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() + geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(size = "Number of plays",
       x = "Estimated win probability",
       y = "Observed win probability") + 
  geom_text(data = ann_text, aes(x = x, y = y, label = lab)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        legend.position = "bottom") +
  facet_wrap(~ qtr, ncol = 4)
ggsave("wp_cv_loso_calibration_results.png", height = 9/1.2, width = 16/1.2)

# Calculate the calibration error values:  
wp_cv_cal_error <- wp_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(period) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_wins = sum(n_wins, na.rm = TRUE))

# Overall weighted calibration error:
with(wp_cv_cal_error, weighted.mean(weight_cal_error, n_wins))
# 0.008924582


## final calculations
cl <- makeCluster(detectCores()-1)


wp_model <- mgcv::bam(Win_Indicator ~
                          s(ExpScoreDiff) +
                            s(TimeSecsRem, by = half) +
                            s(ExpScoreDiff_Time_Ratio) +
                            Under_two*off_timeouts_rem_before*half +
                            Under_two*def_timeouts_rem_before*half,
                      data = epa_w, family = "binomial", cluster=cl,
                      discrete=TRUE)

stopCluster(cl)

save(wp_model, file="data/final_wp_model.RData")




