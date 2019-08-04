library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
source("6-Utils.R")

pbp_full_df <- readRDS(file = "data/pbp.rds") %>% select(-X1)

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
  mutate(Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"),
         Under_two = TimeSecsRem <= 120)

fg_contains = str_detect((pbp_no_OT$play_type),"Field Goal")
fg_no_OT <- pbp_no_OT[fg_contains,]

fg_model <- mgcv::bam(scoring ~ s(adj_yd_line), 
                      data = fg_no_OT, family = "binomial")
saveRDS(fg_model,"fg_model.rds")

##+ Under_TwoMinute_Warning
## Create a weighting factor
# need
ep_model <- nnet::multinom(Next_Score ~ TimeSecsRem + adj_yd_line + Under_two + 
                              down + log_ydstogo + log_ydstogo*down +
                              adj_yd_line*down, data = pbp_no_OT, maxit = 300)
#saveRDS(ep_model,"ep_model.rds")
ep_model = readRDS("ep_model.rds")


## At this point, let's create a function to predict EP_before and EP_after, and calculate EPA

calculate_epa <- function(clean_pbp_dat,ep_mod,fg_mod){
  # constant vectors to be used again
  turnover_play_type = c(
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception'
  )
  
  off_TD = c('Passing Touchdown','Rushing Touchdown')
  def_TD = c('Interception Return Touchdown','Fumble Return Touchdown')
  
  
  pred_df = clean_pbp_dat %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo,Under_two)
  
  # ep_start
  ep_start = predict(ep_mod,pred_df,type='prob')
  weights = c(0,3,-3,-2,-7,2,7)
  pred_df$ep_before = apply(ep_start, 1, function(row){
    sum(row * weights)
  })
  
  ## Prep for EP_after 
  prep_df_after = prep_df_epa(prep_df) 
  turnover_col = prep_df_after %>% pull(turnover)
  prep_df_after = prep_df_after %>% select(-turnover)
  
  ep_end = predict(ep_model,prep_df_after,type='prob')
  pred_df$ep_after = apply(ep_end, 1, function(row){
    sum(row * weights)
  })
  
  pred_df = cbind(clean_pbp_dat[,c("play_type","play_text")],pred_df)
  pred_df$turnover = turnover_col
  
  pred_df[(pred_df$play_type %in% off_TD) | (pred_df$play_type %in% def_TD),"ep_after"] = 7
  pred_df[(pred_df$play_type %in% turnover_play_type) |
            (pred_df$turnover == 1), "ep_after"] = -1 * pred_df[(pred_df$play_type %in% turnover_play_type) |
                                                                  (pred_df$turnover == 1), "ep_after"]
  
  pred_df[pred_df$play_type=="Safety","ep_after"] = -2
  
  ## need to do something about the FG related plays using the field goal model here. s
  
  
  pred_df = pred_df %>% 
    mutate(
      EPA = ep_after - ep_before
    )
  return(pred_df)
}

test = pbp_no_OT %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo) %>% 
  filter()

tamu_18 = pbp_no_OT %>% filter(
  year == 2018,
  offense %in% c("Clemson", "Texas A&M"),
  defense %in% c("Clemson", "Texas A&M")
) 

tamu_18_pred = tamu_18 %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo)

ep_start = predict(ep_model,tamu_18_pred,type='prob')

tamu_18_pred$ep_before = apply(ep_start, 1, function(row){
  sum(row * weights)
})
#test = cbind(tamu_18,ep_start)

# New Down = 1
# New Distance = 10

tamu_18_dat = prep_df_epa(tamu_18) 
tamu_18_dat[tamu_18_dat$adj_yd_line == 0,"log_ydstogo"] = log(25)
tamu_18_dat[tamu_18_dat$adj_yd_line == 0,"adj_yd_line"] = 25

turnover_col = tamu_18_dat %>% pull(turnover)
tamu_18_dat = tamu_18_dat %>% select(-turnover)

ep_end = predict(ep_model,tamu_18_dat,type='prob')
tamu_18_pred$ep_after = apply(ep_end, 1, function(row){
  sum(row * weights)
})

tamu_18_pred = cbind(tamu_18[,c("play_type","play_text")],tamu_18_pred)

turnover_play_type = c(
  'Fumble Recovery (Opponent)',
  'Pass Interception Return',
  'Interception Return Touchdown',
  'Fumble Return Touchdown',
  'Safety',
  'Interception',
  'Pass Interception'
)

off_TD = c('Passing Touchdown','Rushing Touchdown')
def_TD = c('Interception Return Touchdown','Fumble Return Touchdown')

tamu_18_pred$turnover = turnover_col

tamu_18_pred[(tamu_18_pred$play_type %in% off_TD) | (tamu_18_pred$play_type %in% def_TD),"ep_after"] = 7
tamu_18_pred[(tamu_18_pred$play_type %in% turnover_play_type) |
               (tamu_18_pred$turnover == 1), "ep_after"] = -1 * tamu_18_pred[(tamu_18_pred$play_type %in% turnover_play_type) |
                                                                               (tamu_18_pred$turnover == 1), "ep_after"]

tamu_18_pred[tamu_18_pred$play_type=="Safety","ep_after"] = -2

tamu_18_pred = tamu_18_pred %>% 
  mutate(
    EPA = ep_after - ep_before
  )

## Review EPA calcs
tamu_18_pred = cbind(tamu_18$play_text,tamu_18_pred)
