library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
source("6-Utils.R")

pbp_full_df <- readRDS(file = "data/pbp.rds")

## Filter out OT games for now
# Create the EP model dataset that only includes plays with basic seven
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

# Remove  [8] "Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized"
remove_plays <- c("Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized",
                  "Penalty","Kickoff","Kickoff Return (Offense)")

pbp_no_OT <-
  pbp_full_df %>% filter(period <= 4, down > 0) %>%
  filter(!play_type %in% remove_plays) %>%
  filter(!is.na(down),!is.na(raw_secs)) %>%
  filter(log_ydstogo != -Inf) %>%
  rename(TimeSecsRem = raw_secs) %>%
  mutate(Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"))

##+ Under_TwoMinute_Warning
## Create a weighting factor
# need
ep_model <- nnet::multinom(Next_Score ~ TimeSecsRem + adj_yd_line +
                             down + log_ydstogo + log_ydstogo*down +
                             adj_yd_line*down, data = pbp_no_OT, maxit = 300)
saveRDS(ep_model,"ep_model.rds")

test = pbp_no_OT %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo) %>% 
  filter()

tamu_18 = pbp_no_OT %>% filter(
  year == 2018,
  offense %in% c("Clemson", "Texas A&M"),
  defense %in% c("Clemson", "Texas A&M")
) 

tamu_18_pred = tamu_18 %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo)

ep_start = predict(ep_model,tamu_18_pred,type='prob')
weights = c(0,3,-3,-2,-7,2,7)
tamu_18_pred$ep_before = apply(ep_start, 1, function(row){
  sum(row * weights)
})
#test = cbind(tamu_18,ep_start)

# New Down = 1
# New Distance = 10

tamu_18_dat = prep_df_epa(tamu_18) 

ep_end = predict(ep_model,tamu_18_dat,type='prob')
tamu_18_pred$ep_after = apply(ep_end, 1, function(row){
  sum(row * weights)
})

tamu_18_pred = tamu_18_pred %>% 
  mutate(
    EPA = ep_after - ep_before
  )

## Review EPA calcs
tamu_18_pred = cbind(tamu_18$play_text,tamu_18_pred)
