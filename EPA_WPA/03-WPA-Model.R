library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
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
  )


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
            n_wins = length(which(Win_Indicator == 1)),
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

# Calculate the calibration error values:  
wp_cv_cal_error <- wp_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(qtr) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_wins = sum(n_wins, na.rm = TRUE))

# Overall weighted calibration error:
with(wp_cv_cal_error, weighted.mean(weight_cal_error, n_wins))
# 0.01309723


## final calculations

# library(parallel)
# cl <- makeCluster(detectCores()-1)
# 
# 
# wp_model <- mgcv::bam(Win_Indicator ~
#                           s(ExpScoreDiff) +
#                             s(TimeSecsRem, by = half) +
#                             s(ExpScoreDiff_Time_Ratio) +
#                             Under_two*off_timeouts_rem_before*half +
#                             Under_two*def_timeouts_rem_before*half
#                       data = epa_w, family = "binomial", cluster=cl)
# 
# stopCluster(cl)

save(wp_model, file="data/wp_model.RData")

load("wp_model.RData")
library(mgcv)

