# library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(nnet)
library(mgcv)

source("06-Data-Ingest-Utils.R")
source("07-CV-Utils.R")

pbp_full_df <- readRDS(file = "data/pbp.rds")

## Filter out OT games for now
# Create the EP model dataset that only includes plays with basic seven
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

# Remove  [8] "Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized"
remove_plays <-
  c(
    "Extra Point Missed",
    "Extra Point Good",
    "Timeout",
    "End of Half",
    "End of Game",
    "Uncategorized",
    "placeholder",
    "Penalty"	
    #"Kickoff",
    #"Kickoff Return (Offense)",
    #"Kickoff Return Touchdown"
  )

## need to remove games with 
pbp_no_OT <- pbp_full_df %>% 
  filter(down > 0) %>%
  filter(year>=2014) %>%   
  filter(!play_type %in% remove_plays) %>%
  filter(!is.na(down),!is.na(raw_secs)) %>%
  filter(!is.na(game_id)) %>%
  filter(log_ydstogo != -Inf) %>%
  filter(game_id != 400603838) %>%
  rename(TimeSecsRem = raw_secs) %>%
  mutate(
    Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"),
    down = as.factor(down),
    Goal_To_Go = ifelse(
      str_detect(play_type, "Field Goal"),
      distance >= (yards_to_goal - 17),
      distance >= yards_to_goal
    ),
    Under_two = TimeSecsRem <= 120
  ) %>% filter(!is.na(game_id))

#------Create the LOSO predictions for the selected cfbscrapR model:------
#  Create the LOSO predictions for the selected cfbscrapR model:
# ep_model_loso_preds <- calc_ep_multinom_loso_cv(as.formula("Next_Score ~
#                                                            TimeSecsRem + yards_to_goal +
#                                                            down + log_ydstogo + Goal_To_Go + log_ydstogo*down +
#                                                            yards_to_goal*down + Goal_To_Go*log_ydstogo +
#                                                            Under_two"),ep_model_data = pbp_no_OT)
# 
# # Save dataset in data folder as ep_model_loso_preds.csv
# # (NOTE: this dataset is not pushed due to its size exceeding
# # the github limit but will be referenced in other files)
# write.csv(ep_model_loso_preds , "data/ep_model_loso_preds.csv", row.names = FALSE)
#
# # Use the following pipeline to create a dataset used for charting the
# # cross-validation calibration results:
# ep_model_preds <-
#   cbind(Next_Score = ep_model_loso_preds[,c("Next_Score")],
#         ep_model_loso_preds[,(ncol(ep_model_loso_preds)-6):ncol(ep_model_loso_preds)])
# 
# ep_cv_loso_calibration_results <-
#   ep_model_preds %>%
#   # Create a row index column:
#   mutate(play_index = 1:n()) %>%
#   # Gather the columns for the different scoring event probabilities:
#   gather(next_score_type, pred_prob, -Next_Score, -play_index) %>%
#   # Create binned probability column:
#   mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
#   # Group by both the next_score_type and bin_pred_prob:
#   group_by(next_score_type, bin_pred_prob) %>%
#   # Calculate the calibration results:
#   summarize(n_plays = n(),
#             n_scoring_event = length(which(Next_Score == next_score_type)),
#             bin_actual_prob = n_scoring_event / n_plays)
# 

# # Create a label data frame for the chart:
# ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25),
#                        lab = c("More times\nthan expected", "Fewer times\nthan expected"),
#                        next_score_type = factor("No Score (0)"))
# 
# # Create the calibration chart:
# ep_cv_loso_calibration_results %>%
#   ungroup() %>%
#   mutate(next_score_type = fct_relevel(next_score_type,
#                                        "Opp_Safety", "Opp_FG",
#                                        "Opp_TD", "No_Score", "Safety",
#                                        "FG", "TD"
#   ),
#   next_score_type = fct_recode(next_score_type,
#                                "-Field Goal (-3)" = "Opp_FG",
#                                "-Safety (-2)" = "Opp_Safety",
#                                "-Touchdown (-7)" = "Opp_TD",
#                                "Field Goal (3)" = "FG",
#                                "No Score (0)" = "No_Score",
#                                "Touchdown (7)" = "TD",
#                                "Safety (2)" = "Safety")) %>%
#   ggplot() +
#   geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
#   geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
#   geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
#   coord_equal() +   geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
#   scale_x_continuous(limits = c(0,1)) +
#   scale_y_continuous(limits = c(0,1)) +
#   labs(size = "Number of plays",
#        x = "Estimated next score probability",
#        y = "Observed next score probability") +
#   geom_text(data = ann_text, aes(x = x, y = y, label = lab)) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         strip.background = element_blank(),
#         strip.text = element_text(size = 18),
#         axis.title = element_text(size = 18),
#         axis.text.y = element_text(size = 12),
#         axis.text.x = element_text(size = 10, angle = 90),
#         legend.title = element_text(size = 18),
#         legend.text = element_text(size = 16),
#         legend.position = c(1, .05), legend.justification = c(1, 0)) +
#   facet_wrap(~ next_score_type, ncol = 4)+
#   ggsave("figures/ep_cv_loso_calibration_results.png", height = 9/1.2, width = 16/1.2)
# 
# # Calculate the calibration error values:
# cv_cal_error <- ep_cv_loso_calibration_results %>%
#   ungroup() %>%
#   mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
#   group_by(next_score_type) %>%
#   summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
#             n_scoring_event = sum(n_scoring_event, na.rm = TRUE))
# 
# Overall weighted calibration error:
# with(cv_cal_error, weighted.mean(weight_cal_error, n_scoring_event))
# 0.01123694
# 
#--- Create the LOSO and FG predictions for the selected cfbscrapR models:-----
# Create the LOSO predictions for the selected cfbscrapR models:
ep_fg_model_loso_preds <- calc_ep_multinom_fg_loso_cv(as.formula("Next_Score ~ 
                                                                 TimeSecsRem + 
                                                                 yards_to_goal + down + 
                                                                 log_ydstogo + Goal_To_Go + 
                                                                 log_ydstogo*down + 
                                                                 yards_to_goal*down + 
                                                                 Goal_To_Go*log_ydstogo + 
                                                                 Under_two"),
                                                      as.formula("scoring ~ s(yards_to_goal)"),
                                                      ep_model_data = pbp_no_OT)
write.csv(ep_fg_model_loso_preds,"data/ep_fg_model_data_loso.csv",row.names=FALSE)
# ep_fg_model_loso_preds <- read.csv("data/ep_fg_model_data_loso.csv")

# Use the following pipeline to create a dataset used for charting the
# cross-validation calibration results:
ep_fg_model_preds <- 
  cbind(Next_Score = ep_fg_model_loso_preds[,c("Next_Score")],
        ep_fg_model_loso_preds[,(ncol(ep_fg_model_loso_preds)-6):ncol(ep_fg_model_loso_preds)])
ep_fg_cv_loso_calibration_results <- ep_fg_model_preds %>%
  # Create a row index column:
  mutate(play_index = 1:n()) %>%
  # Gather the columns for the different scoring event probabilities:
  gather(next_score_type, pred_prob, -Next_Score, -play_index) %>%
  # Create binned probability column:
  mutate(bin_pred_prob = round(pred_prob / 0.05) * .05) %>%
  # Group by both the next_score_type and bin_pred_prob:
  group_by(next_score_type, bin_pred_prob) %>%
  # Calculate the calibration results:
  summarize(n_plays = n(), 
            n_scoring_event = length(which(Next_Score == next_score_type)),
            bin_actual_prob = n_scoring_event / n_plays)


# Create a label data frame for the chart:
ann_text <- data.frame(x = c(.25, 0.75), y = c(0.75, 0.25), 
                       lab = c("More times\nthan expected", "Fewer times\nthan expected"),
                       next_score_type = factor("No Score (0)"))

# Create the calibration chart:
ep_fg_cv_loso_calibration_results %>%
  ungroup() %>%
  mutate(next_score_type = fct_relevel(next_score_type,
                                       "Opp_FG",
                                       "Opp_Safety","Opp_TD",
                                       "FG","Safety","TD",
                                       "No_Score"),
         next_score_type = fct_recode(next_score_type,
                                      "-Field Goal (-3)" = "Opp_FG",
                                      "-Safety (-2)" = "Opp_Safety",
                                      "-Touchdown (-7)" = "Opp_TD",
                                      "Field Goal (3)" = "FG",
                                      "Touchdown (7)" = 'TD',
                                      "No Score (0)" = "No_Score")) %>%
  ggplot() +
  geom_point(aes(x = bin_pred_prob, y = bin_actual_prob, size = n_plays)) +
  geom_smooth(aes(x = bin_pred_prob, y = bin_actual_prob), method = "loess") +
  geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
  coord_equal() +   geom_text(data = ann_text,aes(x = x, y = y, label = lab)) +
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(title = "Leave-One-Season-Out Cross Validation Calibration for\nExpected Points Model by Scoring Event",
       size = "Number of plays",
       x = "Estimated Next Score Probability",
       y = "Observed Next Score Probability") + 
  #scale_color_brewer(palette = "Spectral", name = NULL, guide = FALSE) +
  geom_text(data = ann_text, aes(x = x, y = y, label = lab)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ next_score_type, ncol = 3)+  
  ggsave("figures/ep_fg_cv_loso_calibration_results.png", height = 9/1.2, width = 16/1.2)


# Calculate the calibration error values:  
cv_fg_cal_error <- ep_fg_cv_loso_calibration_results %>% 
  ungroup() %>%
  mutate(cal_diff = abs(bin_pred_prob - bin_actual_prob)) %>%
  group_by(next_score_type) %>% 
  summarize(weight_cal_error = weighted.mean(cal_diff, n_plays, na.rm = TRUE),
            n_scoring_event = sum(n_scoring_event, na.rm = TRUE))

# Overall weighted calibration error:
with(cv_fg_cal_error, weighted.mean(weight_cal_error, n_scoring_event))
# 0.01314949

### Create Final Models 
final_pbp = pbp_no_OT %>% mutate(
  # 1 - drive difference
  Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) /
    (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
  # 2 - score differential
  ScoreDiff_W = (max(abs_diff) - abs_diff) /
    (max(abs_diff) - min(abs_diff)),
  # 3 - combo of 1 and 2
  Total_W = Drive_Score_Dist_W + ScoreDiff_W,
  Total_W_Scaled = (Total_W - min(Total_W)) /
    (max(Total_W) - min(Total_W))
)

ep_model <-
  nnet::multinom(
    Next_Score ~ 
      TimeSecsRem + yards_to_goal + down + log_ydstogo + Goal_To_Go + Under_two +  
      log_ydstogo*down + yards_to_goal*down + Goal_To_Go*log_ydstogo, 
    data = final_pbp,
    maxit = 300,
    weights = Total_W_Scaled
  )

save(ep_model, file="models/final_ep_model.RData")
saveRDS(ep_model,file="models/final_ep_model.RDS")

# note looks like no extra points after 2014
# need to separate them
pbp_fg_df <- pbp_full_df %>% 
  filter(year>=2014) %>%   
  filter(grepl("Field Goal",play_type) | grepl("Extra Point",play_type)) %>%
  filter(!is.na(down),!is.na(raw_secs)) %>%
  filter(!is.na(game_id)) %>%
  filter(log_ydstogo != -Inf) %>%
  rename(TimeSecsRem = raw_secs) %>%
  mutate(
    Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"),
    down = as.factor(down),
    Goal_To_Go = ifelse(
      str_detect(play_type, "Field Goal"),
      distance >= (yards_to_goal - 17),
      distance >= yards_to_goal
    ),
    Under_two = TimeSecsRem <= 120,
    id_play = as.numeric(id_play)
  ) %>% filter(!is.na(game_id))

fg_model <- mgcv::bam(scoring ~ s(yards_to_goal),
                      data = pbp_fg_df, family = "binomial")
saveRDS(fg_model,"models/final_fg_model.rds")
save(fg_model,file="models/final_fg_model.RData")

all_years = pbp_no_OT %>% split(pbp_no_OT$year)


source("08-Pred-Utils.R")
all_years_epa = lapply(all_years, function(x) {
  year = unique(x$year)
  print(year)
  val = calculate_epa_local(x,ep_model = ep_model,fg_model = fg_model)
  return(val)
})

len = length(all_years)

for (i in 1:len) {
  df = all_years_epa[[i]]
  year = df$year[1]
  filename = glue::glue("figures/histogram_epa_season_{year}.png")
  plot_title = glue::glue("Histogram of EPA - {year} season")
  ggplot(df, aes(x=EPA))+
    geom_histogram(color="darkblue", fill="lightblue", binwidth=1)+
    labs(title=plot_title,subtitle="Data: @CFB_data with #cfbscrapR",
         x = "Expected Points Added", y="Number of plays") +
    theme(
      axis.title.x = element_text(size = 10, margin=margin(0,0,0,0, unit=c("mm")), family = "Gill Sans MT", face = "bold"),
      axis.text.x = element_text(size = 10, margin=margin(0,0,-0.1,0, unit=c("mm")), family = "Gill Sans MT"),
      axis.title.y = element_text(size = 10, margin=margin(0,0,0,0, unit=c("mm")), family = "Gill Sans MT", face = "bold"),
      axis.text.y = element_text(size = 10, margin=margin(0,0.1,0,0, unit=c("mm")), family = "Gill Sans MT"),
      plot.title = element_text(size = 12, margin=margin(t=0, r=0, b=0.2,l=0, unit=c("mm")), lineheight=0.7, family = "Gill Sans MT", face = "bold"),
      plot.subtitle = element_text(size = 10, margin=margin(t=0, r=0, b=0.2,l=0, unit=c("mm")), lineheight=0.7, family = "Gill Sans MT"),
      plot.caption = element_text(size = 10, margin=margin(t=0, r=0, b=0,l=0, unit=c("mm")), lineheight=0.7, family = "Gill Sans MT"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "grey35",color="black"),
      plot.background = element_rect(fill = "grey85",color="black"),
      plot.margin=unit(c(1,2,1,3),"mm"))+
    ggsave(filename, height = 101.6, width = 152.4, units=c('mm'), type="cairo")
  Sys.sleep(5)
}


lapply(all_years_epa,function(x){
  year = unique(x$year)
  name = glue::glue("data/rds/EPA_FG_Calcs_{year}.RDS")
  print(name)
  saveRDS(x,name)
})
